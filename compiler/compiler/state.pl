
:- module(state, [
			  init_state//0,
			  lookup_state//1,
			  restore_state//1,
			  add_variables//1,
			  lookup_variables//2,
			  temp_variable//1,
			  lookup_functions//2,
			  update_stack_size//1,
			  is//2
		  ]).

state(state(Variables, Functions, Stack_Size),
	  Variables, Functions, Stack_Size).


init_state(Tail, [State|Tail]) :-
	init_state(State).


init_state(State) :-
	state(State, [sr, lr], [], 0).


lookup_state(State, [State|Tail], [State|Tail]).


restore_state(State, [_|Tail], [State|Tail]).


add_variables(Variables, [Old_State|Tail], [New_State|Tail]) :-
	state(Old_State, Old_Variables, Functions, Stack_Size),
	append(Old_Variables, Variables, New_Variables),
	state(New_State, New_Variables, Functions, Stack_Size).



lookup_variables(Variables, Registers, [State|Tail], [State|Tail]) :-
	state(State, All_Variables, _, _),
	lookup_variables(Variables, Registers, All_Variables).


lookup_variables([], [], _).

lookup_variables([V|Vs], [R|Rs], All_Variables) :-
	lookup_variable(V, R, All_Variables),
	lookup_variables(Vs, Rs, All_Variables).


lookup_variable(r(N), N, _) :-
	!.

lookup_variable(Variable, Register, Variables) :-
	nth0(Register, Variables, Variable).


temp_variable(r(Register), [State|Tail], [State|Tail]) :-
	state(State, Variables, _, _),
	length(Variables, Register).


lookup_functions(Functions, Addresses, [State|Tail], [State|Tail]) :-
	state(State, All_Functions, _, _),
	lookup_functions(Functions, Addresses, All_Functions).


lookup_functions([], [], _).

lookup_functions([F|Fs], [A|As], All_Functions) :-
	lookup_function(F, A, All_Functions),
	lookup_functions(Fs, As, All_Functions).


lookup_function(Function, Address, Functions) :-
	member(Function-Address, Functions).


is(A, B, State, State) :-
	A is B.
