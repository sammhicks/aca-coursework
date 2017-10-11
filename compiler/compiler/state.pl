
:- module(state, [
			  start//1,
			  init_state//2,
			  finish//0,
			  lookup_state//1,
			  restore_state//1,
			  add_variables//1,
			  lookup_variables//2,
			  temp_variable//1,
			  lookup_function//2,
			  get_frame_size//1,
			  set_frame_size//1,
			  increment_frame_size//1,
			  add_instructions//1
		  ]).

state(state(Variables, Functions, Frame_Size),
	  Variables, Functions, Frame_Size).


start(Functions, Tail, [State|Tail]) :-
	maplist(function_label_map, Functions, Function_Labels),
	state(State, [], Function_Labels, 0).


init_state(Functions, Function, [Old_State|Tail], [State|Tail]) :-
	init_state(Old_State, State, Functions, Function).


init_state(Old_State, State, Functions, Function) :-
	state(Old_State, _, Function_Labels, _),
	state_variables(Functions, Function, Variables),
	state(State, Variables, Function_Labels, 0).


function_label_map(function(Name, _Returns, _Arguments, _Body), Name-_).


state_variables(Functions, Function, Variables) :-
	allocate_functions_returns(Functions, Returns_Variables),
	allocate_functions_arguments(Functions, Arguments_Variables),
	function_variables(Function, Returns_Variables, Arguments_Variables),
	append([[sr, lr], Returns_Variables, Arguments_Variables], Variables).


allocate_functions_returns(Functions, Variables) :-
	maplist(function_returns_count, Functions, Counts),
	max_list(Counts, Count),
	length(Variables, Count).


function_returns_count(function(_Name, Returns, _Arguments, _Body), Count) :-
	length(Returns, Count).


allocate_functions_arguments(Functions, Variables) :-
	maplist(function_arguments_count, Functions, Counts),
	max_list(Counts, Count),
	length(Variables, Count).


function_arguments_count(function(_Name, _Returns, Arguments, _Body), Count) :-
	length(Arguments, Count).


function_variables(function(_Name, Returns, Arguments, _Body), Return_Variables, Argument_Variables) :-
	return_variables(Returns, Return_Variables, 0),
	argument_variables(Arguments, Argument_Variables, 0).

function_variables(none, Return_Variables, Argument_Variables) :-
	function_variables(function(_Name, [], [], _Body), Return_Variables, Argument_Variables).


return_variables([], [], _ID).

return_variables([], [return(ID)|Function_Variables], ID) :-
	Next_ID is ID + 1,
	return_variables([], Function_Variables, Next_ID).

return_variables([Name|Names], [[Name, return(ID)]|Function_Variables], ID) :-
	Next_ID is ID + 1,
	return_variables(Names, Function_Variables, Next_ID).

argument_variables([], [], _ID).

argument_variables([], [argument(ID)|Function_Variables], ID) :-
	Next_ID is ID + 1,
	argument_variables([], Function_Variables, Next_ID).

argument_variables([Name|Names], [[Name, argument(ID)]|Function_Variables], ID) :-
	Next_ID is ID + 1,
	argument_variables(Names, Function_Variables, Next_ID).


finish([_], []).


lookup_state(State, [State|Tail], [State|Tail]).


restore_state(State, [_|Tail], [State|Tail]).


add_variables(Variables, [Old_State|Tail], [New_State|Tail]) :-
	state(Old_State, Old_Variables, Functions, Frame_Size),
	append(Old_Variables, Variables, New_Variables),
	state(New_State, New_Variables, Functions, Frame_Size).



lookup_variables(Variables, Registers, [State|Tail], [State|Tail]) :-
	state(State, All_Variables, _, _),
	lookup_variables(Variables, Registers, All_Variables).


lookup_variables([], [], _).

lookup_variables([V|Vs], [R|Rs], All_Variables) :-
	lookup_variable(V, R, All_Variables),
	!,
	lookup_variables(Vs, Rs, All_Variables).


lookup_variable(r(N), N, _).

lookup_variable(Variable, Register, Variables) :-
	nth0(Register, Variables, Variable_Names),
	is_list(Variable_Names),
	member(Variable, Variable_Names).

lookup_variable(Variable, Register, Variables) :-
	nth0(Register, Variables, Variable).


temp_variable(r(Register), [State|Tail], [State|Tail]) :-
	state(State, Variables, _, _),
	length(Variables, Register).


lookup_function(Function, Address, [State|Tail], [State|Tail]) :-
	state(State, Functions, _, _),
	lookup_function(Function, Address, Functions).


lookup_function(Function, Address, Functions) :-
	member(Function-Address, Functions).


get_frame_size(Size, [State|Tail], [State|Tail]) :-
	state(State, _, _, Size).

set_frame_size(Size, [Old_State|Tail], [New_State|Tail]) :-
	state(Old_State, Variables, Functions, _),
	state(New_State, Variables, Functions, Size).

increment_frame_size(Increment, [Old_State|Tail], [New_State|Tail]) :-
	state(Old_State, Variables, Functions, Old_Size),
	New_Size is Old_Size + Increment,
	state(New_State, Variables, Functions, New_Size).


add_instructions(Instructions), [State] -->
	[State],
	Instructions.
