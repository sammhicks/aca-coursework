
:- module(util, [
	      is//2, % -A, +B. is/2 within DCG. Does not affect the list
	      xor/3  % ?A, ?B, ?C. C == A xor B
	  ]).


is(A, B, State, State) :-
	A is B.

xor(false, false, false).
xor( true, false,  true).
xor(false,  true,  true).
xor( true,  true, false).
