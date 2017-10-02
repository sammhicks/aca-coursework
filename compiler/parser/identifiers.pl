
:- module(identifiers, [
			  array_with_index//2,
			  array_index//1,
			  variable_or_number_list//1,
			  variable_list//1,
			  variable_or_number//1,
			  variable//1,
			  var_keyword//0,
			  for_keyword//0
		  ]).

:- use_module(library(dcg/basics)).

:- use_module(basics).

array_with_index(Array, Index) -->
	variable(Array),
	whites_string_whites("["),
	array_index(Index),
	whites,
	"]".


array_index(V + N) -->
	variable(V),
	whites_string_whites("+"),
	number(N).

array_index(V) -->
	variable_or_number(V).


variable_or_number_list([Item|Items]) -->
	variable_or_number(Item),
	variable_or_number_tail(Items).

variable_or_number_list([]) -->
	[].


variable_or_number_tail([Item|Items]) -->
	whites_string_whites(","),
	!,
	variable_or_number(Item),
	variable_or_number_tail(Items).

variable_or_number_tail([]) -->
	[].


variable_list([V|Vs]) -->
	variable(V),
	variable_list_tail(Vs).

variable_list([]) -->
	[].


variable_list_tail([]) -->
	[].

variable_list_tail([V|Vs]) -->
	whites_string_whites(","),
	variable(V),
	variable_list_tail(Vs).


variable_or_number(v(V)) -->
	variable(V).

variable_or_number(n(N)) -->
	number(N).


variable(Variable) -->
	code_type(First, alpha),
	codes_type(Other, alnum),
	{
	    string_codes(Variable, [First|Other])
	}.

var_keyword -->
	"var",
	white.

for_keyword -->
	"for",
	white.
