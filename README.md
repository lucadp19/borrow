# borrow

Borrow is a small language written in Haskell, born to explain some of the
semantics of the [Rust Language](https://github.com/rust-lang/rust) such as
ownership/affine semantics, references and lifetimes.

## Installation

In order to use Borrow you need to install the 
[Haskell Tool Stack](https://github.com/commercialhaskell/stack). 

You may then clone this repository in a directory of your choice and
locally build Borrow: 

```console
$ git clone https://github.com/lucadp19/borrow
$ stack update
$ stack build
```

You are now ready to use Borrow!

## Usage

You can use Borrow through `Stack`: 
```console
$ stack exec borrow -- [<options>]
```

The given options are
- `-f`|`--file FILE`: the file containing the Borrow program to parse/type/execute;
    if this option isn't set then the program is read through `stdin`
- `-p`|`--parse`: parses the program and returns the parsed AST
- `-t`|`--type`: parses and type-checks the program and returns the resulting type
- `-e`|`--exec`: parses, type-checks and finally evaluates the program.

Only one of `-p`, `-t` and `-e` can be set. If none are set, `-e` is the default.

Examples of Borrow programs can be found in the `/examples` directory.

## Language Reference

The type-system and interpreter for the language are explained in detail in my
[undergraduate thesis](https://github.com/lucadp19/undergrad-thesis).
