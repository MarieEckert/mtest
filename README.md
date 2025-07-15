# mtest

simple testing utility with pretty output inspired by tsoding's btest.

hacked together out of boredom.

## usage

tests are specified in a file which is included into the mtest binary at compile
time. this file should be called `tests.inc` and be located in the current
working directory at compile time.

tests are simply pascal programs and are compiled at test time, all build
artifacts will be written to the same directory as the test source.

### adding tests

the `example_tests.inc` file in the root of this repository can be used as a template.
in here you can add more targets to the `TTarget` enum and append tests into the
`tests` const array.

each test has a few settings:

* name
* source – a freepascal source file
* compileArgs – array of arguments for compilation
* run – should the resulting executable be run?
* args – args for running the executable
* ignoredTargets – set of TTarget for which the test should not be executed

the location of the freepascal binary can be specified via the `FPC_BIN`
constant.

you probably do not want to modify the types `TTest` and `TTestSet`.
