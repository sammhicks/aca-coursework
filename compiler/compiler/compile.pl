
:- module(compile, [
			  compile_script/2
		  ]).

:- use_module(state).
:- use_module(util).

compile_script(Script, Out) :-
	compile_script(Script, Out, []).


compile_script(script(Instructions, Functions)) -->
	start(Functions),
	init_state(Functions, none),
	compile_instructions(Instructions, PC),
	compile_functions(Functions, PC),
	!,
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

compile_instruction(halt) -->
	add_instructions([halt]).

compile_instruction(var(V)) -->
	add_variables([V]).

compile_instruction(var(V, Init)) -->
	compile_instruction(var(V)),
	compile_instruction(assignment(var(V), Init)).

compile_instruction(array(Name, Length)) -->
	add_variables([Name]),
	get_frame_size(Array_Start),
	increment_frame_size(Length),
	compile_instructions([assignment(var(Name), add(v(sr), n(Array_Start)))]).

compile_instruction(assignment(var(V0), v(V1))) -->
	lookup_variables([V0, V1], [R, R]).

compile_instruction(assignment(var(V0), add(v(V0), n(0)))) -->
	[].

compile_instruction(assignment(var(V0), add(n(0), v(V0)))) -->
	[].

compile_instruction(assignment(var(V0), sub(v(V0), n(0)))) -->
	[].

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
	add_instructions([sub(R0, R1, R2, 0)]).

compile_instruction(assignment(var(V0), sub(v(V1), n(N2)))) -->
	lookup_variables([V0, V1], [R0, R1]),
	add_instructions([sub(R0, R1, null, N2)]).

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
	add_instructions([cj(Body_Length, InvFlag, CondFlag, Register)]),
	compile_instructions(Then, Body_Length),
	restore_state(Saved_State).

compile_instruction(if(cond(Precondition, Variable, Comparison), Then, Else)) -->
	lookup_state(Saved_State),
	compile_instructions(Precondition),
	lookup_variables([Variable], [Register]),
	compile_comparison(Comparison, false, InvFlag, CondFlag),
	add_instructions([cj(Else_Jump_Size, InvFlag, CondFlag, Register)]),
	lookup_state(After_Comparison_State),
	compile_instructions(Else, Else_Size),
	add_instructions([j(Then_Jump_Size)]),
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
	add_instructions([cj(Do_Jump_Size, InvFlag, CondFlag, Register)]),
	compile_instructions(Do, Do_Size),
	add_instructions([j(Loop_Jump_Size)]),
	Do_Jump_Size is Do_Size + 2,
	Loop_Jump_Size is -(Precondition_Size + Do_Size + 1),
	restore_state(Saved_State).

compile_instruction(call(Function, Returns, Arguments)) -->
	compile_move_arguments(Arguments, 0),
	lookup_function(Function, Target),
	get_frame_size(Frame_Size),
	compile_instruction(assignment(var(sr), add(v(sr), n(Frame_Size)))),
	add_instructions([b(Target)]),
	compile_instruction(assignment(var(sr), sub(v(sr), n(Frame_Size)))),
	compile_move_returns(Returns, 0).

compile_instruction(I, State, State) :-
	format("Cannot compile instruction: ~q\n", [I]),
	!,
	fail.


compile_comparison(Comparison, Invert, InvFlag, CondFlag, State, State) :-
	comparison_flags(Comparison, RawInvFlag, CondFlag),
	xor(RawInvFlag, Invert, InvFlag).


comparison_flags(<,  false,	"<").
comparison_flags(=,  false,	"0").
comparison_flags(>,  false,	">").
comparison_flags(<=, true,  ">").
comparison_flags(<>, true,	"0").
comparison_flags(>=, true,	"<").


compile_move_arguments([], _Index) -->
	[].

compile_move_arguments([Arg|Args], Index) -->
	compile_move_argument(Arg, Index),
	{
		Next_Index is Index + 1
	},
	compile_move_arguments(Args, Next_Index).


compile_move_argument(Arg, Index) -->
	compile_instruction(assignment(var(r(Index)), Arg)).


compile_move_returns([], _Index) -->
	[].

compile_move_returns([Return|Returns], Index) -->
	compile_move_return(Return, Index),
	{
		Next_Index is Index + 1
	},
	compile_move_returns(Returns, Next_Index).


compile_move_return(Arg, Index) -->
	compile_instruction(assignment(var(Arg), v(r(Index)))).


compile_functions(Functions, PC) -->
	compile_functions(Functions, Functions, PC).


compile_functions(_Functions, [], _PC) -->
	[].

compile_functions(All_Functions, [F|Fs], PC) -->
	compile_function(All_Functions, F, PC, New_PC),
	!,
	compile_functions(All_Functions, Fs, New_PC).


compile_function(Functions, function(Name, Returns, Arguments, Body), PC, New_PC) -->
	lookup_function(Name, PC),
	init_state(Functions, function(Name, Returns, Arguments, Body)),
	lookup_state(State),
	{
		compile_instructions(Body, [State|Body_Operations], [_]),
		register_writes(Body_Operations, [], Writes),
		lookup_variables(Writes, Register_Writes, [State], [State]),
		sort(Register_Writes, Sorted_Register_Writes),
		save_registers(Sorted_Register_Writes, [State|Save_Operations], [New_State]),
		get_frame_size(Frame_Size, [New_State], [New_State]),
		compile_instruction(assignment(var(sr), add(v(sr), n(Frame_Size))), [New_State|Push_Operations], [New_State]),
		compile_instruction(assignment(var(sr), sub(v(sr), n(Frame_Size))), [New_State|Pop_Operations], [New_State]),
		restore_registers(Sorted_Register_Writes, [New_State|Restore_Operations], [_]),
		append([
			Save_Operations,
			Push_Operations,
			Body_Operations,
			Pop_Operations,
			Restore_Operations,
			[ret]
		], Operations),
		length(Operations, Operations_Count),
		New_PC is PC + Operations_Count
	},
	add_instructions(Operations).


register_writes([], Writes, Writes).

register_writes([Op|Ops], Current, Final) :-
	register_writes(Op, Op_Writes),
	union(Op_Writes, Current, New),
	register_writes(Ops, New, Final).


register_writes(add(R, _, _), [r(R)]).
register_writes(sub(R, _, _, _), [r(R)]).
register_writes(mult(R, _, _), [r(R)]).
register_writes(cmp(R, _, _), [r(R)]).
register_writes(cmpi(R, _, _), [r(R)]).
register_writes(ld(R, _, _), [r(R)]).
register_writes(st(_, _, _), []).
register_writes(b(_), [lr]).
register_writes(j(_), []).
register_writes(cj(_, _, _, _), []).
register_writes(ret, []).
register_writes(rand(R, _, _), [r(R)]).
register_writes(log(_), []).
register_writes(out(_, _), []).


save_registers([]) -->
	[].

save_registers([R|Rs]) -->
	save_register(R),
	!,
	save_registers(Rs).


save_register(R) -->
	lookup_variables([V], [R]),
	{
		volatile_variable(V)
	}.

save_register(R) -->
	get_frame_size(Index),
	compile_instruction(assignment(array(sr, n(Index)), v(r(R)))),
	increment_frame_size(1).


restore_registers([]) -->
	[].

restore_registers([R|Rs]) -->
	restore_register(R),
	!,
	restore_registers(Rs).


restore_register(R) -->
	lookup_variables([V], [R]),
	{
		volatile_variable(V)
	}.

restore_register(R) -->
	get_frame_size(Index),
	compile_instruction(assignment(var(r(R)), array(sr, n(Index)))),
	increment_frame_size(-1).


volatile_variable(sr).
volatile_variable(return(_)).
