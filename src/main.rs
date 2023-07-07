//pub mod global;

use binary::asm::read_instructions;
use binary::asm_writer::write_instructions;
use binary::asmparser::Parser;
use binary::asmtokens::tokenize;
use qo::parser::Parser as QOParser;
use qo::tokenizer::Tokenizer;
use qo::tokens::Token as QOToken;
use qo::tokens::TokenKind as QOTokenKind;
use vm::{VirtualMachine};
use ansi_term::Color;

use crate::qo::converter::convert;
use crate::qo::parser::format_error;
use crate::vm::{Instruction, Value};

pub mod class;
pub mod external;
pub mod function;
pub mod gcwrapper;
pub mod vm;
pub mod binary;
pub mod memory;
pub mod manifest;
pub mod qo;

use core::panic;
use std::collections::VecDeque;
use std::env::args;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use std::sync::Arc;
use std::time::Instant;

macro_rules! error_println {
    ($($args:expr),*) => {
        println!("{} {}", Color::Red.bold().paint("Error:"), format_args!($($args),*))
    };
}

macro_rules! note_println {
    ($($args:expr),*) => {
        println!("{} {}", Color::White.bold().paint("Note:"), format_args!($($args),*))
    };
}

macro_rules! example_println {
    ($($args:expr),*) => {
        let input = format!($($args),*);
        let lines: Vec<String> = input.lines().map(|line| format!("+ {}", ansi_term::Color::Green.paint(line))).collect();
        let output = lines.join("\n");
        println!("{}", output);
    };
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

pub fn main() {
    let mut instant = Instant::now();
    let mut option = String::new();
    let mut input = String::new();
    let mut debug = false;
    let mut output = String::new();
    let mut libraries_to_load_with_fns: Vec<(String, Vec<String>)> = Vec::new();
    let mut stack_backtrace_limit = 7;
    let program_name = "qvm";
    let mut timeout: Option<u128> = None;
    let mut timed = false;
    let mut nanos = false;
    let mut millis = false;

    let mut args = args().skip(1);

    if args.len() < 1 {
        error_println!("Minimum number of arguments is 1");
        usage(program_name);
        exit(1);
    }
    loop {
        let next_arg = args.next();
        match next_arg {
            Some(arg) => match arg.as_str() {
                "-d" => {
                    if debug == true {
                        error_println!("-d can only be used once");
                        note_println!("each option can only be used once");
                        exit(1)
                    }
                    debug = true;
                }
                "-i" => match args.next() {
                    Some(arg) => {
                        if input.as_str() != "" {
                            error_println!("-i can only be used once");
                            note_println!("each option can only be used once");
                            exit(1)
                        }
                        input = arg;
                    }
                    None => {
                        error_println!("-i requires an argument");
                        note_println!("provide an argument like -i /path/to/file");
                        exit(1)
                    }
                },
                "-o" => match args.next() {
                    Some(arg) => {
                        if output.as_str() != "" {
                            error_println!("-o can only be used once");
                            note_println!("each option can only be used once");
                            exit(1)
                        }
                        output = arg;
                    }
                    None => {
                        error_println!("-o requires an argument");
                        note_println!("provide an argument like -i /path/to/file");
                        exit(1)
                    }
                }
                "run" => {
                    if option != String::new() {
                        error_println!("The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "build" => {
                    if option != String::new() {
                        error_println!("The main option can only be used once");
                        exit(1)
                    }
                    option = arg;
                }
                "--stack-backtrace-lim" => {
                    if stack_backtrace_limit != 7 {
                        error_println!("The --stack-backtrace-lim option can only be used once");
                        exit(1)
                    } else {
                        match args.next() {
                            Some(arg) => {
                                let parsed_u32 = arg.parse::<u32>();
                                if parsed_u32.is_err() {
                                    error_println!("--stack-backtrace-lim requires a positive numeric argument");
                                    note_println!("provide something like --stack-backtrace-lim 60000");
                                    exit(1)
                                } else {
                                    stack_backtrace_limit = parsed_u32.unwrap();
                                }
                            }
                            None => {
                                error_println!("--stack-backtrace-lim requires an argument");
                                note_println!("provide something like --stack-backtrace-lim 60000");
                                exit(1)
                            }
                        }
                    }
                }
                "--timed" => {
                    if timed {
                        error_println!("--timed already defined");
                        note_println!("each option can only be used once");
                        exit(1)
                    }
                    timed = true;
                }
                "--nanos" => {
                    if millis || nanos {
                        error_println!("a {} is already defined", if millis { "--millis" } else { "--nanos" });
                        exit(1)
                    }
                    nanos = true;
                }
                "--millis" => {
                    if millis || nanos {
                        error_println!("a {} is already defined", if millis { "--millis" } else { "--nanos" });
                        exit(1)
                    }
                    millis = true;
                }
                "--timeout" => {
                    match args.next() {
                        Some(arg) => {
                            let parsed_u32 = arg.parse::<u128>();
                            if parsed_u32.is_err() {
                                error_println!("--timeout requires a positive numeric argument");

                                exit(1)
                            } else {
                                timeout = Some(parsed_u32.unwrap());
                            }
                        }
                        None => {
                            error_println!("--timeout requires an argument");
                            exit(1)
                        }
                    }
                }
                "qotest" => match args.next() {
                    Some(arg) => {
                        if option.as_str() != "" {
                            error_println!("main option can only be used once");
                            note_println!("each option can only be used once");
                            exit(1)
                        }
                        option = String::from("qotest");
                        input = arg;
                    }
                    None => {
                        panic!()
                    }
                }
                _ => {
                    if &input != "" {
                        error_println!("input is already defined: assumed `{}` to be an input file as its not a recognized argument", input);
                        usage(program_name);
                    } else {
                        input = arg;
                    }
                }
            },
            None => break,
        }
    }
    if option == String::from("run") {
        if !std::path::Path::new(&input).exists() {
            error_println!("The file '{}' does not exist.", input);
            note_println!("provide a file that exists in your machine");
            exit(1)
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            error_println!("Could not open file: {}", error);
            exit(1)
        }
        let mut file = file.unwrap();
        let instructions = read_instructions(&mut file);
        if let Ok(instructions) = instructions {
            let mut runtime = VirtualMachine::new(instructions.clone(), &input, 10000, true).expect("Couldn't create a virtual machine instance correctly.");
            runtime.check_labels();
            let result = runtime.run(timeout);
            match result {
                Ok(result) => {
                    println!("\n{}", vm::value_to_string(&result));
                    if timed && nanos {
                        println!("{}ns elapsed", instant.elapsed().as_nanos());
                    } else if timed {
                        println!("{}ms elapsed", instant.elapsed().as_millis());
                    }
                    exit(0)
                }
                Err(err) => {
                    error_println!("{}", Color::White.bold().paint(err));
                    println!(
                        "{}",
                        runtime.get_opstack_backtrace(stack_backtrace_limit as u32)
                    );
                    if timed {
                        if nanos {
                            println!("\n{}ns elapsed", instant.elapsed().as_nanos());
                        } else {
                            println!("\n{}ms elapsed", instant.elapsed().as_millis());
                        }
                    }
                    exit(1)
                }
            }
        } else if let Err(err) = instructions {
            error_println!("Could not read binary file: {}", err);
            note_println!("make sure the file provided is a valid executable");
            exit(1)
        }
    } else if option == String::from("build") {
        if !std::path::Path::new(&input).exists() {
            error_println!("The file '{}' does not exist.", input);
            note_println!("provide a file that exists in your machine");
            exit(1)
        }
        let file = std::fs::File::open(&input);
        if let Err(error) = file {
            error_println!("Could not open file: {}", error);
            exit(1)
        }
        let mut file = file.unwrap();
        let mut buffer = String::new();
        if let Err(err) = file.read_to_string(&mut buffer) {
            error_println!("Could not read file: {}", err);
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
            error_println!("Output file not specified");
            note_println!("provide an argument like -o /path/to/file");
            usage(program_name);
            exit(1);
        }
        let res = File::create(output);
        if let Ok(mut file) = res {
            let result = write_instructions(&mut file, instructions);
            if let Err(err) = result {
                error_println!("Could not write instructions to output file: {}", err);
                exit(1);
            }
        } else if let Err(err) = res {
            error_println!("Could not create output file: {}", err);
            exit(1);
        }
    } else if option == String::from("qotest") {
        eprintln!("getting here");
        let mut file = File::open(input).unwrap();
        let mut buffer = String::new();
        file.read_to_string(&mut buffer).unwrap();
        let mut tokenizer = Tokenizer::new(buffer.clone());
        let tokens: Vec<QOToken> = tokenizer.tokenize().unwrap();
        eprintln!("{:?}", tokens);
        let mut parser = QOParser::new(&tokens);
        let parsed = parser.parse();
        if let Err((msg, tok)) = parsed {
            if let Some(token) = tok {
                let fmt = format_error(&buffer, Color::White.bold().paint(&msg[0]).to_string().as_str(), token);
                error_println!("{}", fmt);
                if msg.len() > 1 {
                    note_println!("{}", msg[1]);
                }
                if msg.len() >= 2 {
                    example_println!("{}", msg[2]);
                }
            } else {
                error_println!("{}", msg[0]);
                if msg.len() > 1 {
                    note_println!("{}", msg[1]);
                }
                if msg.len() >= 2 {
                    example_println!("{}", msg[2]);
                }
            }
            exit(1)
        } else if let Ok(parsed) = parsed {
            eprintln!("{:?}", parsed);
            let converted = convert(parsed);
            eprintln!("Converted:\n{}", converted);
        }
    } else {
        eprintln!("Unknown option: {}", option);
        exit(1)
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