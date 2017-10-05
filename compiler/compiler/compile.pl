
:- module(compile, [
			  compile_script/2
		  ]).

:- use_module(state).

compile_script(Script, Out) :-
	compile_script(Script, Out, []).

compile_script(script(Instructions, Functions)) -->
	init_state,
	compile_instructions(Instructions),
	compile_functions(Functions),
	finish.


compile_instructions([]) -->
	[].

compile_instructions([I|Is]) -->
	compile_instruction(I),
	!,
	compile_instructions(Is).


compile_instruction(noop) -->
	add_instructions([noop]).

compile_instruction(var(V)) -->
	add_variables([V]).

compile_instruction(var(V, Init)) -->
	compile_instruction(var(V)),
	compile_instruction(assignment(var(V), Init)).

compile_instruction(array(Name, Length)) -->
	add_variables([Name]),
	lookup_variables([Name], [Array]),
	get_frame_size(Array_Start),
	increment_frame_size(Length),
	add_instructions([add(Array, [sr], Array_Start)]).

compile_instruction(assignment(var(V0), v(V1))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([add(R0, [R1], 0)]).

compile_instruction(assignment(var(V0), n(N1))) -->
	lookup_variables([V0], [R0]),
	add_instructions([add(R0, [], N1)]).

compile_instruction(assignment(var(V0), array(V1, V2 + N3))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([ld(R0, [R1, R2], N3)]).

compile_instruction(assignment(var(V0), array(V1, v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([ld(R0, [R1, R2], 0)]).

compile_instruction(assignment(var(V0), array(V1, n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([ld(R0, [R1], N2)]).

compile_instruction(assignment(var(V0), add(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([add(R0, [R1, R2], 0)]).

compile_instruction(assignment(var(V0), add(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([add(R0, [R1], N2)]).

compile_instruction(assignment(var(V0), add(n(N1), v(V2)))) -->
	lookup_variables([V0, V2], [R0, R2]),
	add_instructions([add(R0, [R2], N1)]).

compile_instruction(assignment(var(V0), add(n(N1), n(N2)))) -->
	lookup_variables([V0], [R0]),
	N3 is N1 + N2,
	add_instructions([add(R0, [], N3)]).

compile_instruction(assignment(var(V0), sub(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([sub(R0, [R1, R2], 0)]).

compile_instruction(assignment(var(V0), sub(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([sub(R0, [R1], N2)]).

compile_instruction(assignment(var(V0), sub(n(N1), v(V0)))) -->
	temp_variable(Temp),
	compile_instruction(assignment(var(Temp), n(N1))),
	compile_instruction(assignment(var(V0), sub(v(Temp), v(V0)))).

compile_instruction(assignment(var(V0), sub(n(N1), v(V2)))) -->
	compile_instruction(assignment(var(V0), n(N1))),
	compile_instruction(assignment(var(V0), sub(v(V0), v(V2)))).

compile_instruction(assignment(var(V0), mul(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([mul(R0, [R1, R2], 0)]).

compile_instruction(assignment(var(V0), mul(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([mul(R0, [R1], N2)]).

compile_instruction(assignment(var(V0), mul(n(N1), v(V2)))) -->
	lookup_variables([V0, V2], [R0, R2]),
	add_instructions([mul(R0, [R2], N1)]).

compile_instruction(assignment(var(V0), mul(n(N1), n(N2)))) -->
	lookup_variables([V0], [R0]),
	N3 is N1 * N2,
	add_instructions([mul(R0, [], N3)]).

compile_instruction(assignment(var(V0), cmp(v(V1), v(V2)))) -->
	lookup_variables([V0, V1, V2], [R0, R1,	R2]),
	add_instructions([cmp(R0, R1, R2)]).

compile_instruction(assignment(var(V0), cmp(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([cmpi(R0, R1, N2)]).

compile_instruction(assignment(var(V0), rand(Min, Max))) -->
	lookup_variables([V0], [R0]),
	add_instructions([rand(R0, Min, Max)]).


compile_instruction(assignment(array(V1, V2 + N3), v(V0))) -->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([st(R0, [R1, R2], N3)]).

compile_instruction(assignment(array(V1, v(V2)), v(V0)))-->
	lookup_variables([V0, V1, V2], [R0, R1, R2]),
	add_instructions([st(R0, [R1, R2], 0)]).

compile_instruction(assignment(array(V1, n(N2)), v(V0))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([st(R0, [R1], N2)]).

compile_instruction(assignment(array(A, I), RHS)) -->
	temp_variable(Temp),
	compile_instruction(assignment(var(Temp), RHS)),
	compile_instruction(assignment(array(A, I), v(Temp))).


compile_functions([]), [State] -->
	[State].

% compile_functions([F|Fs]) -->
%	compile_function(F),
%	compile_functions(Fs).
