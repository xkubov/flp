# BKG-2-CNF

This repository continains implementation of algorithm that takes definition of Context 
free Grammar and transforms it into Chomsky normal form.

This project was created for subject FLP on Faculty of Information Technology VUT in Brno
in academic year 2019/2020.

More about Chomsky normal form can be found in: https://en.wikipedia.org/wiki/Chomsky_normal_form

## Usage

Result of the implementation is command line application that has following interface.

```bash
Usage: bkg-2-cnf <options> [input]
  -i    Prints loaded CFG on stdout.
  -1    Pritns CFG without trivial rules on stdout.
  -2    Prints converted CFG to CNF on stdout.
```

If no `[input]` file is provided grammar is read from the STDIN until `EOF` is read.
The grammar on input must be in valid form. See https://en.wikipedia.org/wiki/Context-free_grammar
for mor information about CFG.

## Included implementation

In included implementation I have included all the functionality that was
requested from the bkg-2-cnf.

## Build and Installation

To build bkg-2-cnf program use command `make` form root of this repository.
After that the `bkg-2-cnf` will be located in the root of the repository ready
to be used.
