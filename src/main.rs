pub mod binary;
pub mod class;
pub mod external;
pub mod function;
pub mod memory;
pub mod vm;
pub mod strstrip;
pub mod control;
pub mod llist;
pub mod signal;

use crate::vm::{Instruction, Value, VirtualMachine};
use crate::binary::asm::read_instructions;
use crate::binary::asm_writer::write_instructions;
use crate::binary::asmparser::Parser;
use crate::binary::asmtokens::tokenize;
use crate::control::IOControllerW;
use ctrlc;

use std::env::args;
use std::fs::{File, canonicalize};
use std::io::{Read, stderr, Stdout, Stderr, stdout};
use std::panic;
use std::process::ExitCode;
use std::sync::Arc;
use std::time::Instant;
use chrono::prelude::*;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};

use ansi_term::Color;
use parking_lot::Mutex;
use async_std::task;

struct ManifestLayout {
    package: String,
    author: Option<String>,
    version: String,
    input: String,
    output: String,
}

macro_rules! error_println {
    ($($arg:tt)*) => {
        eprintln!("{} {}", Color::Red.bold().paint("Error:"), format_args!($($arg)*));
    };
}

macro_rules! note_println {
    ($($arg:tt)*) => {
        eprintln!("{} {}", Color::Black.on(Color::White).paint("Note:"), format_args!($($arg)*));
    }
}

fn get_current_time() -> String {
    let local: DateTime<Local> = Local::now();
    local.format("%H:%M:%S").to_string()
}

fn usage(program_name: &str) {
    eprintln!(
        "Usage: {} <OPTION> [ARGS]
OPTIONS:
    run                                         Runs the program stored at the filepath specified
    build                                       Builds the assembly code stored at the filepath specified
                                                  and stores its equivalent binary at the output filepath specified

ARGS:
    -d | --debug                                Runs the program in debug mode
    -i | --input <filepath>                     Specifies the input file path
    --stack-backtrace-lim                       Specifies the stack backtrace limit
    -e | --extern <libpath> [...FUNCTIONS]      Imports the specified functions from the library
                                                  specified for use in the VM during runtime
    -o | --output <filepath>                    Specifies the output file path",
        program_name
    );
}

fn main() -> ExitCode {
    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
        let bind = stdout();
        let mut lock = bind.lock();
        let errbind = stderr();
        let errlock = errbind.lock();
        let _ = write!(lock, "\n{} Ctrl+C signal", Color::Red.bold().paint("Aborted"));
        std::process::exit(130);
    })
    .expect("Error setting Ctrl+C handler");

    let panicked = panic::catch_unwind(|| {
        let mut option = String::new();
        let mut input = String::new();
        let mut debug = false;
        let mut output = String::new();
        let mut stack_backtrace_limit: i64 = 7;
        let mut default_recursion_depth = 1000;
        let mut depth_is_active = true;
        let mut is_silent = false;
        let mut hide_return = false;
        let mut timeout = None;
    
        let stdout_binding: &'static mut Stdout = Box::leak(Box::new(std::io::stdout()));
        let stderr_binding: &'static mut Stderr = Box::leak(Box::new(stderr()));
    
        let stdout = IOControllerW::new(stdout_binding);
        let stderr = IOControllerW::new(stderr_binding);
    
        let stdout_static_ref: &'static mut IOControllerW<'static, std::io::Stdout> = 
            Box::leak(Box::new(stdout));
        let stderr_static_ref: &'static mut IOControllerW<'static, std::io::Stderr> =
            Box::leak(Box::new(stderr));
    
        let stdout_arc: &'static mut Arc<Mutex<&mut IOControllerW<'_, Stdout>>> =
            Box::leak(Box::new(Arc::new(Mutex::new(stdout_static_ref))));
        let stderr_arc: &'static mut Arc<Mutex<&mut IOControllerW<'_, Stderr>>> =
            Box::leak(Box::new(Arc::new(Mutex::new(stderr_static_ref))));
    
        let program_name = "qlang";
    
        let mut program_args: Vec<Value> = Vec::new();
    
        let mut args = args().skip(1);
    
        if args.len() < 1 {
            error_println!("Minimum number of arguments is 1");
            usage(program_name);
            return ExitCode::FAILURE;
        }
        loop {
            let next_arg = args.next();
            match next_arg {
                Some(arg) => match arg.as_str() {
                    "-d" | "--debug" => {
                        if debug == true {
                            error_println!("-d | --debug can only be used once");
                            note_println!("Debug mode already enabled");
                            return ExitCode::FAILURE;
                        }
                        debug = true;
                    }
                    "-i" | "--input" => match args.next() {
                        Some(arg) => {
                            if input.as_str() != "" {
                                error_println!("-i | --input can only be used once");
                                note_println!("Input file was already set to `{}`", input);
                                return ExitCode::FAILURE;
                            }
                            input = arg;
                        }
                        None => {
                            error_println!("-i | --input requires an argument");
                            note_println!("Please provide an argument like `--input /path/to/file");
                            return ExitCode::FAILURE;
                        }
                    },
                    "-o" | "--output" => match args.next() {
                        Some(arg) => {
                            if output.as_str() != "" {
                                error_println!("-o | --output can only be used once");
                                note_println!("Output file was already set to `{}`", output);
                                return ExitCode::FAILURE;
                            }
                            output = arg;
                        }
                        None => {
                            error_println!("-o | --output requires an argument");
                            note_println!("Please provide an argument like `--output /path/to/out.q`");
                            return ExitCode::FAILURE;
                        }
                    },
                    "-q" | "--quiet" => {
                        if is_silent {
                            error_println!("-q | --quiet can only be used once");
                            note_println!("Quiet mode already enabled");
                            return ExitCode::FAILURE;
                        }
                        is_silent = true;
                    }
                    "--hide-return" | "--noreturn" => {
                        if hide_return {
                            error_println!("--hide-return | --noreturn can only be used once");
                            note_println!("Hide return already enabled");
                            return ExitCode::FAILURE;
                        }
                        hide_return = true;
                    }
                    "--timeout" => match args.next() {
                        Some(arg) => {
                            if timeout != None {
                                error_println!("--timeout can only be used once");
                                note_println!("Timeout already set to `{}`", timeout.unwrap());
                                return ExitCode::FAILURE;
                            }
                            let res = arg.parse::<usize>();
                            if let Ok(res) = res {
                                timeout = Some(res);
                            } else if let Err(err) = res {
                                error_println!("Could not parse content obtained as argument to --timeout as an unsigned integer: {}", err);
                                note_println!("`{}` is possibly not a valid unsigned integer", arg);
                                return ExitCode::FAILURE;
                            }
                        }
                        None => {
                            error_println!("--timeout requires an argument");
                            note_println!("Please provide an argument like `--timeout 30000`");
                            return ExitCode::FAILURE;
                        }
                    }
                    "run" => {
                        if option != String::new() {
                            error_println!("The main option can only be used once");
                            note_println!("\"Main\" option already set to `{}`", option);
                            return ExitCode::FAILURE;
                        }
                        option = arg;
                    }
                    "build" => {
                        if option != String::new() {
                            error_println!("The main option can only be used once");
                            note_println!("\"Main\" option already set to `{}`", option);
                            return ExitCode::FAILURE;
                        }
                        option = arg;
                    }
                    "manifest" => {
                        if option != String::new() {
                            error_println!("The main option can only be used once");
                            note_println!("\"Main\" option already set to `{}`", option);
                            return ExitCode::FAILURE;
                        }
                        option = arg;
                    }
                    "cbuild" => {
                        if option != String::new() {
                            error_println!("The main option can only be used once");
                            note_println!("\"Main\" option already set to `{}`", option);
                            return ExitCode::FAILURE;
                        }
                        option = arg;
                    }
                    "--stack-backtrace-lim" => {
                        if stack_backtrace_limit != 7 {
                            error_println!("The --stack-backtrace-lim option can only be used once");
                            note_println!("Stack backtrace limit already manually set to `{}`", stack_backtrace_limit);
                            return ExitCode::FAILURE;
                        } else {
                            match args.next() {
                                Some(arg) => {
                                    let parsed_u32 = arg.parse::<u32>();
                                    // if parsed_u32.is_err() {
                                    //     error_println!("--stack-backtrace-lim requires a positive numeric argument");
                                    //     return ExitCode::FAILURE;
                                    // } else {
                                    //     stack_backtrace_limit = parsed_u32.unwrap() as i64;
                                    // }
                                    if let Ok(parsed_u32) = parsed_u32 {
                                        stack_backtrace_limit = parsed_u32 as i64;
                                    } else if let Err(_) = parsed_u32 {
                                        error_println!("The argument for --stack-backtrace-lim must be a valid unsigned integer");
                                        note_println!("`{}` is possibly not a valid unsigned integer", arg);
                                        return ExitCode::FAILURE;
                                    }
                                }
                                None => {
                                    error_println!("--stack-backtrace-lim requires an argument");
                                    note_println!("Please provide an argument like `--stack-backtrace-lim 17`");
                                    return ExitCode::FAILURE;
                                }
                            }
                        }
                    }
                    "--max-recursion-depth" => {
                        if default_recursion_depth != 1000 {
                            error_println!("The --max-recursion-depth option can only be used once");
                            note_println!("Max recursion depth already set to `{}`", default_recursion_depth);
                            return ExitCode::FAILURE;
                        } else {
                            match args.next() {
                                Some(arg) => {
                                    let parsed_usize = arg.parse::<u32>();
                                    if let Ok(parsed_usize) = parsed_usize {
                                        default_recursion_depth = parsed_usize;
                                    } else if let Err(_) = parsed_usize {
                                        error_println!("The argument for --max-recursion-depth must be a valid unsigned integer");
                                        note_println!("`{}` is possibly not a valid unsigned integer", arg);
                                        return ExitCode::FAILURE;
                                    }
                                }
                                None => {
                                    error_println!("--max-recursion-depth requires an argument");
                                    note_println!("Please provide an argument like `--max-recursion-depth 10000`");
                                    return ExitCode::FAILURE;
                                }
                            }
                        }
                    }
                    "--ignore-recursion-depth" => {
                        if depth_is_active != true {
                            error_println!("The --ignore-recursion-depth option can only be used once");
                            note_println!("Ignore recursion depth already enabled");
                            return ExitCode::FAILURE;
                        } else {
                            match args.next() {
                                Some(arg) => {
                                    let parsed_bool = arg.parse::<bool>();
                                    if parsed_bool.is_err() {
                                        error_println!("--ignore-recursion-depth requires a boolean argument");
                                        note_println!("`{}` is possibly not a valid boolean argument (true | false)", arg);
                                        return ExitCode::FAILURE;
                                    } else {
                                        depth_is_active = parsed_bool.unwrap();
                                    }
                                }
                                None => {
                                    eprintln!("--ignore-recursion-depth requires an argument");
                                    note_println!("Please provide an argument like `--ignore-recursion-depth true`");
                                    return ExitCode::FAILURE;
                                }
                            }
                        }
                    }
                    "--" => {
                        while let Some(arg) = args.next() {
                            program_args.push(Value::String(arg));
                        }
                        break;
                    }
                    _ => {
                        if !input.is_empty() {
                            error_println!("Input already defined (assumed {} to be an input file as it is not a recognized argument)", arg);
                            return ExitCode::FAILURE;
                        }
                        input = arg;
                    }
                },
                None => break,
            }
        }
        if option == String::from("run") {
            let start = Instant::now();
            if &input == "" {
                error_println!("No input file provided");
                note_println!("Provide a path argument at the end like `/path/to/input_file`");
                return ExitCode::FAILURE;
            }
            if input.ends_with("/") || input.ends_with("\\") {
                error_println!("`{}` is a directory", input);
                return ExitCode::FAILURE;
            }
            if !std::path::Path::new(&input).exists() {
                error_println!("The file `{}` does not exist.", input);
                note_println!("Please make sure the file path is correct and exists on your system");
                return ExitCode::FAILURE;
            }
            if !is_silent {
                eprintln!("{} tasks", Color::Cyan.bold().paint(" Starting"));
            }
            let file = std::fs::File::open(&input);
            if let Err(error) = file {
                error_println!("Could not open file: {}", error);
                return ExitCode::FAILURE;
            }
            let mut file = file.unwrap();
            let instructions = read_instructions(&mut file);
            if let Ok(instructions) = instructions {
                let mut runtime = VirtualMachine::new(instructions.clone(), &input)
                    .expect("Couldn't create a virtual machine instance correctly.");
                runtime.check_labels();
                runtime.set_max_recursiveness_level(default_recursion_depth as usize);
                runtime.allocate_variable_in_root(String::from("args"), Value::List(program_args));
    
                runtime.check_labels();
                if !is_silent {
                    eprintln!("{} labels", Color::Cyan.bold().paint(" Checking"));
                }
                runtime.link_return();
                if !is_silent {
                    eprintln!("{} returns", Color::Cyan.bold().paint("  Linking"));
                }
                if !is_silent {
                    let duration = start.elapsed();
                    let seconds = duration.as_secs();
                    let nanoseconds = duration.subsec_micros();
                    eprintln!("{}tasks in {}.{:09} seconds", Color::Green.bold().paint(" Finished "), seconds, nanoseconds);
                    eprintln!("{}`{}`", Color::Green.bold().paint("  Running "), &canonicalize(input).unwrap().to_str().unwrap()[4..]);
                }
                let arced_instant = Arc::new(start);
                let arced_instant_staticref: &'static mut Arc<Instant> = Box::leak(Box::new(arced_instant));
                let result = runtime.run(Arc::new(String::from("__main__")), arced_instant_staticref, timeout, stdout_arc, stderr_arc);
                match task::block_on(result) {
                    Ok(result) => {
                        if is_silent || hide_return {
                            return ExitCode::SUCCESS;
                        }
                        let mut lock = stdout_arc.lock();
                        let _ = lock.write_pending();
                        lock.lock();
                        let mut stderrlock = stderr_arc.lock();
                        let _ = stderrlock.write_pending();
                        eprintln!("{}", vm::value_to_string(&result));
                        let duration = start.elapsed();
                        let seconds = duration.as_secs();
                        let nanoseconds = duration.subsec_micros();
                        eprintln!("{}ended program in {}.{:09} seconds", Color::Green.bold().paint(" Success "), seconds, nanoseconds);
                        return ExitCode::SUCCESS;
                    }
                    Err(err) => {
                        let mut stdout_lock = stdout_arc.lock();
                        let mut stderr_lock = stderr_arc.lock();
                        stderr_lock.clear();
                        stdout_lock.clear();
                        stderr_lock.unlock();
                        let info = runtime.generate_error_info(err);
                        {
                            let _ = writeln!(
                                stderr_lock,
                                "{}: {}",
                                Color::Red.bold().paint("Error"),
                                Color::White.bold().paint(info.message)
                            );
                            let _ = stderr_lock.flush();
                        }
                        eprintln!("Backtrace:");
                        {
                            let _ = writeln!(
                                stderr_lock,
                                "{}",
                                runtime.get_opstack_backtrace(stack_backtrace_limit as usize)
                            );
                            let _ = stderr_lock.flush();
                        }
                        return ExitCode::FAILURE;
                    }
                }
            } else if let Err(err) = instructions {
                error_println!("Could not read binary file: {}", err);
                return ExitCode::FAILURE;
            } else {
                unreachable!()
            }
        } else if option == String::from("build") {
            if &input == "" {
                error_println!("Input file not specified");
            }
            if !std::path::Path::new(&input).exists() {
                error_println!("The file `{}` does not exist.", input);
                return ExitCode::FAILURE;
            }
            let file = std::fs::File::open(&input);
            if let Err(error) = file {
                error_println!("Could not open file: {}", error);
                return ExitCode::FAILURE;
            }
            let mut file = file.unwrap();
            let mut buffer = String::new();
            if let Err(err) = file.read_to_string(&mut buffer) {
                error_println!("Could not read file: {}", err);
                return ExitCode::FAILURE;
            }
            let tokens = tokenize(&buffer, &input);
            if let Err(err) = tokens {
                eprintln!("{}:{}", input, err);
                return ExitCode::FAILURE;
            }
            let tokens = tokens.unwrap();
            let mut parser = Parser::new(tokens);
            let instructions = parser.parse();
            if let Err(err) = instructions {
                eprintln!("{}:{}", input, err);
                return ExitCode::FAILURE;
            }
            let instructions = instructions.unwrap();
            if output == String::new() {
                error_println!("Output file not specified");
                usage(program_name);
                return ExitCode::FAILURE;
            }
            let res = File::create(output);
            if let Ok(mut file) = res {
                let result = write_instructions(&mut file, instructions);
                if let Err(err) = result {
                    error_println!(
                        "Could not write instructions to output file: {}",
                        err
                    );
                    return ExitCode::FAILURE;
                } else {
                    return ExitCode::SUCCESS;
                }
            } else if let Err(err) = res {
                error_println!("Could not create output file: {}", err);
                return ExitCode::FAILURE;
            } else {
                return ExitCode::SUCCESS;
            }
        } else if option == "cbuild" {
            if &input == "" {
                error_println!("Input file not specified");
                return ExitCode::FAILURE;
            }
            let start = Instant::now();
            if !std::path::Path::new(&input).exists() {
                error_println!("The file `{}` does not exist.", input);
                return ExitCode::FAILURE;
            }
            let file = std::fs::File::open(&input);
            if let Err(error) = file {
                error_println!("Could not open file: {}", error);
                return ExitCode::FAILURE;
            }
            let mut file = file.unwrap();
            let mut buffer = String::new();
            if let Err(err) = file.read_to_string(&mut buffer) {
                error_println!("Could not read file: {}", err);
                return ExitCode::FAILURE;
            }
            let tokens = tokenize(&buffer, &input);
            if let Err(err) = tokens {
                eprintln!("{}:{}", input, err);
                return ExitCode::FAILURE;
            }
            let tokens = tokens.unwrap();
            let mut parser = Parser::new(tokens);
            let instructions = parser.parse();
            if let Err(err) = instructions {
                eprintln!("{}:{}", input, err);
                return ExitCode::FAILURE;
            }
            let instructions = instructions.unwrap();
            let mut runtime = VirtualMachine::new(instructions.clone(), &input)
                    .expect("Couldn't create a virtual machine instance correctly.");
            runtime.check_labels();
            runtime.set_max_recursiveness_level(default_recursion_depth as usize);
            runtime.allocate_variable_in_root(String::from("args"), Value::List(program_args));
    
            runtime.check_labels();
            eprintln!("{} labels", Color::Cyan.bold().paint(" Checking"));
            runtime.link_return();
            eprintln!("{} returns", Color::Cyan.bold().paint("  Linking"));
            let duration = start.elapsed();
            let seconds = duration.as_secs();
            let nanoseconds = duration.subsec_micros();
            eprintln!("{}tasks in {}.{:09} seconds", Color::Green.bold().paint(" Finished "), seconds, nanoseconds);
            eprintln!("{}`{}`", Color::Green.bold().paint("  Running "), &canonicalize(input).unwrap().to_str().unwrap()[4..]);
            let arced_instant = Arc::new(start);
            let arced_instant_staticref: &'static mut Arc<Instant> = Box::leak(Box::new(arced_instant));
            let result = runtime.run(Arc::new(String::from("__main__")), arced_instant_staticref, timeout, stdout_arc, stderr_arc);
            match task::block_on(result) {
                Ok(result) => {
                    eprintln!("{}", vm::value_to_string(&result));
                    return ExitCode::SUCCESS;
                }
                Err(err) => {
                    error_println!(
                        "Uncaught Runtime {}: {}",
                        Color::Red.paint("Error"),
                        Color::White.bold().paint(err)
                    );
                    eprintln!(
                        "{}",
                        runtime.get_opstack_backtrace(stack_backtrace_limit as usize)
                    );
                    return ExitCode::FAILURE;
                }
            }
        } else if &option == "manifest" {


            return ExitCode::FAILURE;
        } else {
            eprintln!("Unsupported command-line option: {}", option);
            return ExitCode::FAILURE;
        }
    });
    match panicked {
        Ok(exitcode) => exitcode,
        Err(panicinfo) => {
            eprintln!("{} {} {:?}", Color::Red.bold().paint("Error"), Color::White.bold().paint("(program panicked, caught unwind)"), panicinfo);
            note_println!("To know more about `{}`, please visit `{}`", Color::White.blink().paint("panic"), Color::White.underline().paint("https://doc.rust-lang.org/std/macro.panic.html"));
            return ExitCode::FAILURE;
        }
    };
    ExitCode::SUCCESS
}