# WACC Compiler - Group 33
The WACC compiler project for group 33 composed of Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane.

This compiler was developed from the 24th of january to the 4th of march 2022.

## WACC Language
WACC is a basic language supporting procedural programming. It supports basic if 
statements, while loops, functions, modules, standard IO and a simple static type system.

```
begin
    bool collatz(int n) is
        println n;
        if n == 1 then
            return true
        else 
            bool x = false;
            if (n % 2) == 0 then
                x = call collatz(n / 2) # tail call optimised
            else
                x = call collatz(3 * n + 1) # tail call optimised
            fi;
            return x
        fi
    end

    int num = 0;
    print "Enter the value you want to check: ";
    read num;
    println "Checking...\n";
    bool _ = call collatz(num);
    print num;
    println " does eventually go to 1!"
end
```

Imperial provides access to the [WACC reference compiler](https://teaching.doc.ic.ac.uk/wacc_compiler/). While no source code access is provided, it is useful to check the expected behaviour of WACC Programs.

## Compiler Design
![compiler map](docs/image/Compiler%20Map.png)
### Why Rust?
We chose Rust for several reasons:
- High Performance - this enabled us to perform intensive optimisation passes, generate complex error messages and run large tests quickly.
- Memory Safety - enabled helped us work quickly with complex graph data structures without high risk of significant memory/concurrency bugs.
- Wonderful Syntax - simple, low-boilerplate, and highly expressive - making it a joy to work with.
- Supporting Infrastructure - cargo made managing dependencies a breeze, as well as running our extensive tests. Rustdoc allowed for extensive documentation (updated and hosted on gitlab pages by our CI).
- To improve our Rust through a large project!

We decided to use nightly rust in order to take advantage of features such as 

### Design philosophy
We designed our compiler to be as extendable as possible. By separating it into multiple intermediate representations we allow for new backends, optimisations passes, and potentially entire language frontends to be implemented and connected.

This modularity also allowed us to test large compiler components in isolation, with printouts for the various representations useful for debugging.

### Optimisations
The main two components of our backend are the `ThreeCode` (general three-address code) and `ArmRepr` (arm based representation) both of which are control flow graphs.

Architecture non-specific optimisations such as inlining, constant propagation, dead code elimination and tail-call optimisation are done no the `ThreeCode` representation. This enabled us to inspect optimisations on the threecode independent of the backend arm translation.

Instruction selection (including optimisations for constants - e.g reorganising comparisons), register allocation (using our own, fast `O(n)` allocation algorithm) and stack organisation are all done by the `ArmRepr`.


## Get Started
## Prerequisites
[Cargo](https://doc.rust-lang.org/cargo/).

### Build
The provided makefile can be used (used by Imperial's Testing environment).
```Bash
make         # make the debug version - unoptimised (cargo build)
make release # make release (optimised) (cargo build --release)
```
Or alternatively cargo:
```Bash
cargo build
cargo build --release
```

### Documentation
```Bash
cargo doc
sensible-browser target/doc/compile/index.html # open in browser
```

### Test
```Bash
cargo test
```
### Using the compiler
A command line interface built using [clap](https://docs.rs/clap/latest/clap/) is provided.
```
wacc_33 0.6.9
Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane
WACC compiler

USAGE:
    compile [OPTIONS] <FILE>

ARGS:
    <FILE>    
  
OPTIONS:
    -a, --arm-temp                  Print the backend representations (arm with temporaries)
        --const-branch              Enable constant branch optimization
        --const-prop                Enable constant propagation
        --dead-code                 Enable dead code elimination
    -f, --final-three-code          Print the three code optimised threecode of the program
    -h, --help                      Print help information
    -i, --ir-print                  Print the intermediate representation generated
        --inlining <MODE>           Set the function inlining mode [default: off] [possible values:
                                    off, low, medium, high]
    -o, --outputpath <FILE>         The name of the output file
    -O                              Enable all optimizations
        --tail-call                 run tail call optimisation
    -u, --unoptimised-three-code    Print the three code representation of the program
    -V, --version                   Print version information
```
