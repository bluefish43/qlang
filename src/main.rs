//pub mod global;

use binary::asm::read_instructions;
use binary::asm_writer::write_instructions;
use binary::asmparser::Parser;
use binary::asmtokens::tokenize;
use vm::{VirtualMachine};
use ansi_term::Color;

use crate::vm::{Instruction, Value};

pub mod class;
pub mod external;
pub mod function;
pub mod gcwrapper;
pub mod vm;
pub mod binary;
pub mod memory;
pub mod manifest;

use std::collections::VecDeque;
use std::env::args;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use std::sync::Arc;

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

pub fn main() {
    let mut option = String::new();
    let mut input = String::new();
    let mut debug = false;
    let mut output = String::new();
    let mut libraries_to_load_with_fns: Vec<(String, Vec<String>)> = Vec::new();
    let mut stack_backtrace_limit = 7;
    let program_name = "qlang";

    let mut args = args().skip(1);

    if args.len() < 1 {
        println!("Error: Minimum number of arguments is 1");
        usage(program_name);
        exit(1);
    }
    loop {
        let next_arg = args.next();
        match next_arg {
            Some(arg) => match arg.as_str() {
                "-d" | "--debug" => {
                    if debug == true {
                        println!("Error: -d | --debug can only be used once");
                        exit(1)
                    }
                    debug = true;
                }
                "-i" | "--input" => match args.next() {
                    Some(arg) => {
                        if input.as_str() != "" {
                            println!("Error: -i | --input can only be used once");
                            exit(1)
                        }
                        input = arg;
                    }
                    None => {
                        println!("Error: -i | --input requires an argument");
                        exit(1)
                    }
                },
                "-o" | "--output" => match args.next() {
                    Some(arg) => {
                        if output.as_str() != "" {
                            println!("Error: -o | --output can only be used once");
                            exit(1)
                        }
                        output = arg;
                    }
                    None => {
                        println!("Error: -o | --output requires an argument");
                        exit(1)
                    }
                }
                "run" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "build" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "writetest" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "runtest" => {
                    if option != String::new() {
                        println!("Error: The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "--stack-backtrace-lim" => {
                    if stack_backtrace_limit != 7 {
                        println!("Error: The --stack-backtrace-lim option can only be used once");
                        exit(1)
                    } else {
                        match args.next() {
                            Some(arg) => {
                                let parsed_u32 = arg.parse::<u32>();
                                if parsed_u32.is_err() {
                                    println!("Error: --stack-backtrace-lim requires a positive numeric argument");
                                    exit(1)
                                } else {
                                    stack_backtrace_limit = parsed_u32.unwrap();
                                }
                            }
                            None => {
                                println!("Error: --stack-backtrace-lim requires an argument");
                                exit(1)
                            }
                        }
                    }
                }
                "-e" | "--extern" => {
                    let library_name = args.next();
                    if let None = library_name {
                        println!("Error: -e | --extern requires a library to load");
                        exit(1)
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
                                exit(1)
                            }
                        }
                    }
                }
                _ => {
                    println!("Error: Unknown command line option: {}", arg);
                    exit(1)
                }
            },
            None => break,
        }
    }
    if option == String::from("run") {
        if !std::path::Path::new(&input).exists() {
            println!("The file '{}' does not exist.", input);
            exit(1)
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            println!("Error: Could not open file: {}", error);
            exit(1)
        }
        let mut file = file.unwrap();
        let instructions = read_instructions(&mut file);
        if let Ok(instructions) = instructions {
            let mut runtime = VirtualMachine::new(instructions.clone(), &input, 10000, true).expect("Couldn't create a virtual machine instance correctly.");
            runtime.check_labels();
            let result = runtime.run();
            match result {
                Ok(result) => {
                    println!("{}", vm::value_to_string(&result));
                    exit(0)
                }
                Err(err) => {
                    println!("{}: {}", Color::Red.bold().paint("Error"), Color::White.bold().paint(err));
                    println!(
                        "{}",
                        runtime.get_opstack_backtrace(stack_backtrace_limit as usize)
                    );
                    exit(1)
                }
            }
        } else if let Err(err) = instructions {
            println!("Error: Could not read binary file: {}", err);
            exit(1)
        }
    } else if option == String::from("build") {
        if !std::path::Path::new(&input).exists() {
            println!("The file '{}' does not exist.", input);
            exit(1)
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            println!("Error: Could not open file: {}", error);
            exit(1)
        }
        let mut file = file.unwrap();
        let mut buffer = String::new();
        if let Err(err) = file.read_to_string(&mut buffer) {
            println!("Error: Could not read file: {}", err);
            exit(1);
        }
        let tokens = tokenize(&buffer, &input);
        if let Err(err) = tokens {
            println!("{}:{}", input, err);
            exit(1);
        }
        let tokens = tokens.unwrap();
        let mut parser = Parser::new(tokens);
        let instructions = parser.parse();
        if let Err(err) = instructions {
            println!("{}:{}", input, err);
            exit(1);
        }
        let instructions = instructions.unwrap();
        if output == String::new() {
            println!("Error: Output file not specified");
            usage(program_name);
            exit(1);
        }
        let res = File::create(output);
        if let Ok(mut file) = res {
            let result = write_instructions(&mut file, instructions);
            if let Err(err) = result {
                println!("Error: Could not write instructions to output file: {}", err);
                exit(1);
            }
        } else if let Err(err) = res {
            println!("Error: Could not create output file: {}", err);
            exit(1);
        }
    }
}

fn factorize(number: i32) -> Vec<i32> {
    let mut factors = vec![];
    let mut current = number;
    let mut i = 0;
    loop {
        if current == 1 {
            return factors;
        }
        if (current as f64 / i as f64).fract() == 0.0 {
            factors.push(i);
            current /= i;
            i = 2;
        } else {
            i += 1;
            if i as f64 > (number as f64 / 2.0) {
                factors.push(current);
                return factors;
            } else {
                continue;
            }
        }
    }
}