
:- module(optimiser, [
			  optimise/2
		  ]).

optimise(Old, New) :-
	optimise_once(Old, Intermediate),
	!,
	optimise(Intermediate, New).

optimise(A, A).


optimise_once(Old, New) :-
	nth0(PC, Old, j(Offset, InvFlag, CondFlag, Register), Rest),
	Target is PC + Offset,
	nth0(Target, Old, j(Target_Offset, false, "1", -1)),
	New_Offset is Offset + Target_Offset,
	nth0(PC, New, j(New_Offset, InvFlag, CondFlag, Register), Rest).
