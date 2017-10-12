
:- module(util, [
			 is//2,
			 xor/3
		 ]).


is(A, B, State, State) :-
	A is B.

xor(false, false, false).
xor( true, false,  true).
xor(false,  true,  true).
xor( true,  true, false).
