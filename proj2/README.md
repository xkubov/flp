# FLP: project 2

Goal of the project is to implement simulator of non-deterministic Turing machine.
Implementation takes definition of a Turing machine on standard input.

Assignment specified following format for input:

```
<rule-0><nl>
<rule-1><nl>
...
<rule-n><nl>
<tape>
```

Where each rule has following format:

```
<state> <symbol-on-tape> <new-state> <new-symbol-or-R-L>
```

And tape:

```
<symbol-0><symbol-1>...<symbol-n>
```

Each state must satisfy regex `[A-Z]` and each symbol must comfort regex `[a-z]`.

# Usage

When run `flp20-log -h` from command line you will be provided with following
help screen:

```
usage: flp20-log <option>
<option>:
    -t prints execution time in miliseconds at the end of output.
    -h prints this help.

```

# About Implementation

Simulation of Turing machine is implemented using BFS algorithm so that if final configuration exists
it is find. Example of possible cycling configuration is located in `tests/in/test08`.

This project implements all functionality specified by the assignment. Apart from that
I have implemented option to provide comments on input so that I could provide
documentation for each test file. Each comment must start with `#` character.

For example, this is valid input:

```
#
# Turing machine that accepts
# only input containing substring 'aba'.
#
S b S R
S c S R
S a A R
A a A R
A c S R
A b B R
# Turing machine should not fail when moved far right.
B a F R
B b S R
B c S R
# This must end with success.
aba
```

# Build and Installation

To build the project just use `make` from the root of the repository:
After that you will be provided with executable `flp20-log`.

# Testing

With project comes automated testing script that can be invoked by running from command line:

```
$ make tests
```

This will result in compilation and run of `flp20-log` with included tests set. Each test is defined
in `tests/in/test*` and should have output to compare located in `tests/out/test*` (Same name).
After invocation each run will be measured and output of tests will be provided on `stdout`.
Apart from testing of output measurement of time is provided.

Example of running `make tests`:

```
Testing Turing Machine
===========================
Measuring times
	in/test10: 2.000000ms
	in/test04: 0.000000ms
	in/test03: 1.000000ms
	in/test02: 1.000000ms
	in/test05: 0.000000ms
	in/test09: 1.000000ms
	in/test00: 0.000000ms
	in/test07: 2.000000ms
	in/test06: 0.000000ms
	in/test01: 1.000000ms
	in/test08: 1.000000ms
Passed  11/11 tests
===========================
```
