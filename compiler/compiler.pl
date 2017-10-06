
:- module(compiler, [
			  compile/2
		  ]).

:- use_module(parser/parser).
:- use_module(compiler/compile).
:- use_module(optimiser/optimiser).

compile(Src, Out) :-
	phrase_from_file(script(S), Src),
	compile_script(S, Intermediate),
	optimise(Intermediate, Out).
