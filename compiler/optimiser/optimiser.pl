
:- module(optimiser, [
	      optimise/2
	  ]).

optimise(Old, New) :-
	optimise_once(Old, Intermediate),
	!,
	optimise(Intermediate, New).

optimise(A, A).


optimise_once(Old, New) :-
	nth0(PC, Old, j(Offset), Rest),
	Target is PC + 1 + Offset,
	nth0(Target, Old, j(Target_Offset)),
	New_Offset is Offset + Target_Offset + 1,
	nth0(PC, New, j(New_Offset), Rest).

optimise_once(Old, New) :-
	nth0(PC, Old, cj(Offset, Invert, Cond, R1), Rest),
	Target is PC + 1 + Offset,
	nth0(Target, Old, j(Target_Offset)),
	New_Offset is Offset + Target_Offset + 1,
	nth0(PC, New, cj(New_Offset, Invert, Cond, R1), Rest).

