
:- module(compile, [
			  compile_script/2
		  ]).

:- use_module(state).

compile_script(Script, Out) :-
	compile_script(Script, Out, []).

compile_script(script(Instructions, Functions)) -->
	compile_instructions(Instructions),
	compile_functions(Functions).


compile_instructions([]) -->
	[].

compile_instructions([I|Is]) -->
	compile_instruction(I),
	compile_instructions(Is).

compile_instruction(noop) -->
	[noop].

compile_instruction(var(V)) -->
	add_variables([V]).

compile_instruction(array(Name, Length)) -->
	add_variables([Name]),
	[
		add(Name, [sr]),
		add(sr, [sr], Length)
	].

compile_instruction(var(V, Init)) -->
	compile_instruction(var(V)),
	compile_instruction(assignment(var(V), Init)).

compile_instruction(assignment(var(V0), v(V1))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[add(R0, [R1], 0)].

compile_instruction(assignment(var(V0), n(N1))) -->
	lookup_variables([V0], [R0]),
	[add(R0, [], N1)].

compile_instruction(assignment(var(V0), array(V1, V2 + N3))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	[ld(R0, [R1, R2], N3)].

compile_instruction(assignment(var(V0), array(V1, v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	[ld(R0, [R1, R2], 0)].

compile_instruction(assignment(var(V0), array(V1, n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[ld(R0, [R1], N2)].

compile_instruction(assignment(var(V0), add(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	[add(R0, [R1, R2], 0)].

compile_instruction(assignment(var(V0), add(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[add(R0, [R1], N2)].

compile_instruction(assignment(var(V0), add(n(N1), v(V2)))) -->
	lookup_variables([V0, V2], [R0, R2]),
	[add(R0, [R2], N1)].

compile_instruction(assignment(var(V0), add(n(N1), n(N2)))) -->
	lookup_variables([V0], [R0]),
	N3 is N1 + N2,
	[add(R0, [], N3)].

compile_instruction(assignment(var(V0), sub(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	[sub(R0, [R1, R2], 0)].

compile_instruction(assignment(var(V0), sub(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[sub(R0, [R1], N2)].

compile_instruction(assignment(var(V0), sub(n(N1), v(V0)))) -->
	temp_variable(Temp),
	compile_instruction(assignment(var(Temp), n(N1))),
	compile_instruction(assignment(var(V0), sub(v(Temp), v(V0)))).

compile_instruction(assignment(var(V0), sub(n(N1), v(V2)))) -->
	compile_instruction(assignment(var(V0), n(N1))),
	compile_instruction(assignment(var(V0), sub(v(V0), v(V2)))).

compile_instruction(assignment(var(V0), mul(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	[mul(R0, [R1, R2], 0)].

compile_instruction(assignment(var(V0), mul(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[mul(R0, [R1], N2)].

compile_instruction(assignment(var(V0), mul(n(N1), v(V2)))) -->
	lookup_variables([V0, V2], [R0, R2]),
	[mul(R0, [R2], N1)].

compile_instruction(assignment(var(V0), mul(n(N1), n(N2)))) -->
	lookup_variables([V0], [R0]),
	N3 is N1 * N2,
	[mul(R0, [], N3)].

compile_instruction(assignment(var(V0), cmp(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1,	R2]),
	[cmp(R0, R1, R2)].

compile_instruction(assignment(var(V0), cmp(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	[cmpi(R0, R1, N2)].

compile_instruction(assignment(var(V0), rand(Min, Max))) -->
	lookup_variables([V0], [R0]),
	[rand(R0, Min, Max)].


compile_functions([]) -->
	[].

% compile_functions([F|Fs]) -->
%	compile_function(F),
%	compile_functions(Fs).
