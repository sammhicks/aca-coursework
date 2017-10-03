
:- module(assignments, [
			  assignment//1,
			  lhs//1,
			  rhs//1
		  ]).

:- use_module(basics).
:- use_module(identifiers).

:- use_module(library(dcg/basics)).

assignment(var(V, Init)) -->
	var_keyword,
	whites,
	variable(V),
	whites_string_whites("="),
	rhs(Init).

assignment(assignment(LHS, RHS)) -->
	lhs(LHS),
	whites_string_whites("="),
	rhs(RHS).


lhs(var(V)) -->
	variable(V).

lhs(array(A, I)) -->
	array_with_index(A, I).


rhs(add(A, B)) -->
	binop("+", A, B).

rhs(sub(A, B)) -->
	binop("-", A, B).

rhs(mult(A, B)) -->
	binop("*", A, B).

rhs(cmp(A, B)) -->
	binop("<>", A, B).

rhs(rand(A, B)) -->
	"[",
	whites,
	number(A),
	whites_string_whites(","),
	number(B),
	whites,
	")".

rhs(array(A, I)) -->
	array_with_index(A, I).

rhs(R) -->
	variable_or_number(R).


binop(Op, A, B) -->
	variable_or_number(A),
	whites_string_whites(Op),
	variable_or_number(B).
