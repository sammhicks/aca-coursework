
:- module(counter, [
              increment_counter/2,
              finalize_counter/1,
              finalize_counter/2
          ]).

increment_counter(Counter, Increment) :-
    (   var(Counter)
    ->  Counter = _ + Increment
    ;   Counter = LHS + _,
        increment_counter(LHS, Increment)).

finalize_counter(Counter) :-
    (   var(Counter)
    ->  Counter = 0
    ;   Counter = LHS + _,
        finalize_counter(LHS)).

finalize_counter(Counter, Copy) :-
    (   var(Counter)
    ->  Copy = 0
    ;   Counter = LHS + RHS,
        Copy = LHS_Copy + RHS,
        finalize_counter(LHS, LHS_Copy)).
