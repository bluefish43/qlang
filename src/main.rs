use ansi_term::Color;
use binary::asm::read_instructions;
use binary::asm_writer::write_instructions;
use binary::asmparser::Parser;
use binary::asmtokens::tokenize;
use external::get_functions;
use vm::{RawValue, VirtualMachine};

use crate::vm::{Instruction, Value};

pub mod binary;
pub mod class;
pub mod external;
pub mod function;
pub mod gcwrapper;
pub mod memory;
pub mod vm;

use std::env::args;
use std::fs::{File, canonicalize};
use std::io::Read;
use std::process::{ExitCode};
use std::sync::Arc;
use std::time::Instant;
use chrono::prelude::*;

fn get_current_time() -> String {
    let local: DateTime<Local> = Local::now();
    local.format("%H:%M:%S").to_string()
}

fn usage(program_name: &str) {
    println!(
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

pub fn main() -> ExitCode {
    let mut option = String::new();
    let mut input = String::new();
    let mut debug = false;
    let mut output = String::new();
    let mut libraries_to_load_with_fns: Vec<(String, Vec<String>)> = Vec::new();
    let mut stack_backtrace_limit = 7;
    let mut default_recursion_depth = 1000;
    let mut depth_is_active = true;
    let mut is_silent = false;
    let mut hide_return = false;

    let program_name = "qlang";

    let mut program_args: Vec<Value> = Vec::new();

    let mut args = args().skip(1);

    if args.len() < 1 {
        println!("Error: Minimum number of arguments is 1");
        usage(program_name);
        return ExitCode::FAILURE;
    }
    loop {
        let next_arg = args.next();
        match next_arg {
            Some(arg) => match arg.as_str() {
                "-d" | "--debug" => {
                    if debug == true {
                        println!("Error: -d | --debug can only be used once");
                        return ExitCode::FAILURE;
                    }
                    debug = true;
                }
                "-i" | "--input" => match args.next() {
                    Some(arg) => {
                        if input.as_str() != "" {
                            println!("Error: -i | --input can only be used once");
                            return ExitCode::FAILURE;
                        }
                        input = arg;
                    }
                    None => {
                        println!("Error: -i | --input requires an argument");
                        return ExitCode::FAILURE;
                    }
                },
                "-o" | "--output" => match args.next() {
                    Some(arg) => {
                        if output.as_str() != "" {
                            println!("Error: -o | --output can only be used once");
                            return ExitCode::FAILURE;
                        }
                        output = arg;
                    }
                    None => {
                        println!("Error: -o | --output requires an argument");
                        return ExitCode::FAILURE;
                    }
                },
                "-q" | "--quiet" => {
                    if is_silent {
                        println!("Error: -q | --quiet can only be used once");
                        return ExitCode::FAILURE;
                    }
                    is_silent = true;
                }
                "--hide-return" | "--noreturn" => {
                    if hide_return {
                        println!("Error: --hide-return | --noreturn can only be used once");
                        return ExitCode::FAILURE;
                    }
                    hide_return = true;
                }
                "run" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        return ExitCode::FAILURE;
                    }
                    option = arg;
                }
                "build" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        return ExitCode::FAILURE;
                    }
                    option = arg;
                }
                "cbuild" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        return ExitCode::FAILURE;
                    }
                    option = arg;
                }
                "--stack-backtrace-lim" => {
                    if stack_backtrace_limit != 7 {
                        println!("Error: The --stack-backtrace-lim option can only be used once");
                        return ExitCode::FAILURE;
                    } else {
                        match args.next() {
                            Some(arg) => {
                                let parsed_u32 = arg.parse::<u32>();
                                if parsed_u32.is_err() {
                                    println!("Error: --stack-backtrace-lim requires a positive numeric argument");
                                    return ExitCode::FAILURE;
                                } else {
                                    stack_backtrace_limit = parsed_u32.unwrap();
                                }
                            }
                            None => {
                                println!("Error: --stack-backtrace-lim requires an argument");
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                }
                "--max-recursion-depth" => {
                    if stack_backtrace_limit != 3000 {
                        println!("Error: The --max-recursion-depth option can only be used once");
                        return ExitCode::FAILURE;
                    } else {
                        match args.next() {
                            Some(arg) => {
                                let parsed_usize = arg.parse::<usize>();
                                if parsed_usize.is_err() {
                                    println!("Error: --max-recursion-depth requires a positive numeric argument");
                                    return ExitCode::FAILURE;
                                } else {
                                    default_recursion_depth = parsed_usize.unwrap();
                                }
                            }
                            None => {
                                println!("Error: --max-recursion-depth requires an argument");
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                }
                "--ignore-recursion-depth" => {
                    if depth_is_active != true {
                        println!("Error: The --ignore-recursion-depth option can only be used once");
                        return ExitCode::FAILURE;
                    } else {
                        match args.next() {
                            Some(arg) => {
                                let parsed_bool = arg.parse::<bool>();
                                if parsed_bool.is_err() {
                                    println!("Error: --ignore-recursion-depth requires a boolean argument");
                                    return ExitCode::FAILURE;
                                } else {
                                    depth_is_active = parsed_bool.unwrap();
                                }
                            }
                            None => {
                                println!("Error: --ignore-recursion-depth requires an argument");
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                }
                "-e" | "--extern" => {
                    let library_name = args.next();
                    if let None = library_name {
                        println!("Error: -e | --extern requires a library to load");
                        return ExitCode::FAILURE;
                    }
                    let library_name = library_name.unwrap();
                    let mut funcnames: Vec<String> = Vec::new();
                    loop {
                        match args.next() {
                            Some(arg) => match arg.as_str() {
                                "--end" => {
                                    libraries_to_load_with_fns.push((library_name, funcnames));
                                    break;
                                }
                                _ => {
                                    funcnames.push(arg);
                                }
                            },
                            None => {
                                println!("Error: Unclosed -e | --extern parameters");
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
                    println!("Error: Unknown command line option: {}", arg);
                    return ExitCode::FAILURE;
                }
            },
            None => break,
        }
    }
    if option == String::from("run") {
        let start = Instant::now();
        if !std::path::Path::new(&input).exists() {
            println!("Error: The file '{}' does not exist.", input);
            return ExitCode::FAILURE;
        }
        if !is_silent {
            eprintln!("{} tasks", Color::Cyan.bold().paint(" Starting"));
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            println!("Error: Could not open file: {}", error);
            return ExitCode::FAILURE;
        }
        let mut extern_fns = vec![];
        for (libname, functions) in libraries_to_load_with_fns {
            let library = get_functions(
                &libname,
                &*functions.iter().map(|v| v.as_str()).collect::<Vec<&str>>(),
            );
            if let Ok(library) = library {
                extern_fns.extend(library.into_iter());
            } else if let Err(err) = library {
                println!("Error: Could not get extern library {}: {}", libname, err);
                return ExitCode::FAILURE;
            }
        }
        let mut file = file.unwrap();
        let instructions = read_instructions(&mut file);
        if let Ok(instructions) = instructions {
            let mut runtime = VirtualMachine::new(instructions.clone(), &input)
                .expect("Error: Couldn't create a virtual machine instance correctly.");
            runtime.check_labels();
            let fns_static_ref: &'static Vec<(
                String,
                std::sync::Arc<dyn Fn(&[Value]) -> Value + 'static>,
            )> = Box::leak(Box::new(
                extern_fns
                    .into_iter()
                    .map(|(name, f)| {
                        (
                            name,
                            Arc::new(move |args: &[Value]| unsafe {
                                f(args
                                    .to_vec()
                                    .iter()
                                    .map(|value| vm::value_to_raw(value.clone()))
                                    .collect::<Vec<RawValue>>()
                                    .as_slice()
                                    .as_ptr())
                            })
                                as Arc<dyn Fn(&[Value]) -> Value + 'static>,
                        )
                    })
                    .collect::<Vec<_>>(),
            ));

            runtime.extend_functions_wextern(fns_static_ref);
            runtime.set_max_recursiveness_level(default_recursion_depth);
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
            let result = runtime.run(String::from("__main__"));
            match result {
                Ok(result) => {
                    if is_silent || hide_return {
                        return ExitCode::SUCCESS;
                    }
                    println!("{}", vm::value_to_string(&result));
                    return ExitCode::SUCCESS;
                }
                Err(err) => {
                    println!(
                        "Uncaught Runtime {}: {}",
                        Color::Red.paint("Error"),
                        Color::White.bold().paint(err)
                    );
                    println!(
                        "{}",
                        runtime.get_opstack_backtrace(stack_backtrace_limit as usize)
                    );
                    return ExitCode::FAILURE;
                }
            }
        } else if let Err(err) = instructions {
            println!("Error: Could not read binary file: {}", err);
            return ExitCode::FAILURE;
        } else {
            unreachable!()
        }
    } else if option == String::from("build") {
        if &input == "" {
            println!("Error: Input file not specified");
        }
        if !std::path::Path::new(&input).exists() {
            println!("Error: The file '{}' does not exist.", input);
            return ExitCode::FAILURE;
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            println!("Error: Could not open file: {}", error);
            return ExitCode::FAILURE;
        }
        let mut file = file.unwrap();
        let mut buffer = String::new();
        if let Err(err) = file.read_to_string(&mut buffer) {
            println!("Error: Could not read file: {}", err);
            return ExitCode::FAILURE;
        }
        let tokens = tokenize(&buffer, &input);
        if let Err(err) = tokens {
            println!("{}:{}", input, err);
            return ExitCode::FAILURE;
        }
        let tokens = tokens.unwrap();
        let mut parser = Parser::new(tokens);
        let instructions = parser.parse();
        if let Err(err) = instructions {
            println!("{}:{}", input, err);
            return ExitCode::FAILURE;
        }
        let instructions = instructions.unwrap();
        if output == String::new() {
            println!("Error: Output file not specified");
            usage(program_name);
            return ExitCode::FAILURE;
        }
        let res = File::create(output);
        if let Ok(mut file) = res {
            let result = write_instructions(&mut file, instructions);
            if let Err(err) = result {
                println!(
                    "Error: Could not write instructions to output file: {}",
                    err
                );
                return ExitCode::FAILURE;
            } else {
                return ExitCode::SUCCESS;
            }
        } else if let Err(err) = res {
            println!("Error: Could not create output file: {}", err);
            return ExitCode::FAILURE;
        } else {
            return ExitCode::SUCCESS;
        }
    } else if option == "cbuild" {
        if &input == "" {
            println!("Error: Input file not specified");
            return ExitCode::FAILURE;
        }
        let start = Instant::now();
        if !std::path::Path::new(&input).exists() {
            println!("The file '{}' does not exist.", input);
            return ExitCode::FAILURE;
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            println!("Error: Could not open file: {}", error);
            return ExitCode::FAILURE;
        }
        let mut file = file.unwrap();
        let mut buffer = String::new();
        if let Err(err) = file.read_to_string(&mut buffer) {
            println!("Error: Could not read file: {}", err);
            return ExitCode::FAILURE;
        }
        let tokens = tokenize(&buffer, &input);
        if let Err(err) = tokens {
            println!("{}:{}", input, err);
            return ExitCode::FAILURE;
        }
        let tokens = tokens.unwrap();
        let mut parser = Parser::new(tokens);
        let instructions = parser.parse();
        if let Err(err) = instructions {
            println!("{}:{}", input, err);
            return ExitCode::FAILURE;
        }
        let instructions = instructions.unwrap();
        let mut runtime = VirtualMachine::new(instructions.clone(), &input)
                .expect("Error: Couldn't create a virtual machine instance correctly.");
        runtime.check_labels();
        runtime.set_max_recursiveness_level(default_recursion_depth);
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
        let result = runtime.run(String::from("__main__"));
        match result {
            Ok(result) => {
                println!("{}", vm::value_to_string(&result));
                return ExitCode::SUCCESS;
            }
            Err(err) => {
                println!(
                    "Uncaught Runtime {}: {}",
                    Color::Red.paint("Error"),
                    Color::White.bold().paint(err)
                );
                println!(
                    "{}",
                    runtime.get_opstack_backtrace(stack_backtrace_limit as usize)
                );
                return ExitCode::FAILURE;
            }
        }
    } else {
        println!("Unsupported command-line option: {}", option);
        return ExitCode::FAILURE;
    }
}
