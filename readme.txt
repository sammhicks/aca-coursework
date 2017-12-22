Requirements

The Makefile should download and install the necessary npm packages

If tsc fails, install the TypeScript compiler using:

  npm install -g typescript

This shouldn't be required, as the output of the TypeScript compiler (the .js files) are included




To run a script, run:

  nodejs index.js <path to script.json>

Examples can be found in the benchmarks and feature-tests folders




If you want to edit the scripts or write your own, the compiler, written in Prolog, is found in the compiler folder

To compile scripts:

  cd compiler

  swipl compiler.pl

Inside the terminal:

  compile_all.

This should compile all scripts in the benchmarks and feature-tests folders