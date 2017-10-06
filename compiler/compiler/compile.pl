
:- module(compile, [
			  compile_script/2
		  ]).

:- use_module(state).

compile_script(Script, Out) :-
	compile_script(Script, Out, []).

compile_script(script(Instructions, Functions)) -->
	init_state,
	compile_instructions(Instructions),
	%compile_functions(Functions),
	finish.

compile_instructions(Instructions, Block_Length) -->
	lookup_state(State),
	{
		compile_instructions(Instructions, [State|Operations], [End_State]),
		length(Operations, Block_Length)
	},
	restore_state(End_State),
	add_instructions(Operations).


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

compile_instruction(outs(String)) -->
	add_instructions([log(String)]).

compile_instruction(outv(V)) -->
	lookup_variables([V], [R]),
	add_instructions([out(V, R)]).

compile_instruction(if(cond(Precondition, Variable, Comparison), Then, noop)) -->
	lookup_state(Saved_State),
	compile_instructions(Precondition),
	lookup_variables([Variable], [Register]),
	compile_comparison(Comparison, true, InvFlag, CondFlag),
	add_instructions([j(Body_Length, InvFlag, CondFlag, Register)]),
	compile_instructions(Then, Body_Length),
	restore_state(Saved_State).

compile_instruction(if(cond(Precondition, Variable, Comparison), Then, Else)) -->
	lookup_state(Saved_State),
	compile_instructions(Precondition),
	lookup_variables([Variable], [Register]),
	compile_comparison(Comparison, false, InvFlag, CondFlag),
	add_instructions([j(Else_Jump_Size, InvFlag, CondFlag, Register)]),
	lookup_state(After_Comparison_State),
	compile_instructions(Else, Else_Size),
	add_instructions([j(Then_Jump_Size, false, "1", -1)]),
	Else_Jump_Size is Else_Size + 2,
	restore_state(After_Comparison_State),
	compile_instructions(Then, Then_Size),
	Then_Jump_Size is Then_Size + 1,
	restore_state(Saved_State).

compile_instruction(for(Setup, Condition, Increment, Do)) -->
	lookup_state(Saved_State),
	compile_instructions(Setup),
	{
		append(Do, Increment, Do_Increment)
	},
	compile_instruction(for(Condition, Do_Increment)),
	restore_state(Saved_State).

compile_instruction(for(cond(Precondition, Variable, Comparison), Do)) -->
	lookup_state(Saved_State),
	compile_instructions(Precondition, Precondition_Size),
	lookup_variables([Variable], [Register]),
	compile_comparison(Comparison, true, InvFlag, CondFlag),
	add_instructions([j(Do_Jump_Size, InvFlag, CondFlag, Register)]),
	compile_instructions(Do, Do_Size),
	add_instructions([j(Loop_Jump_Size, false, "1", -1)]),
	Do_Jump_Size is Do_Size + 2,
	Loop_Jump_Size is -(Precondition_Size + Do_Size + 1),
	restore_state(Saved_State).


compile_comparison(Comparison, Invert, InvFlag, CondFlag, State, State) :-
	comparison_flags(Comparison, RawInvFlag, CondFlag),
	xor(RawInvFlag, Invert, InvFlag).


comparison_flags(<,  false,	"<").
comparison_flags(=,  false,	"0").
comparison_flags(>,  false,	">").
comparison_flags(<=, true,  ">").
comparison_flags(<>, true,	"0").
comparison_flags(>=, true,	"<").


compile_functions([]), [State] -->
	[State].

% compile_functions([F|Fs]) -->
%	compile_function(F),
%	compile_functions(Fs).
