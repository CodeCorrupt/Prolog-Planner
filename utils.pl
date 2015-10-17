:- module(utils,[
	      member/2,
	      writelist/1,
	      reverse_print_stack/1,
	      empty_stack/1,
	      stack/3,
	      member_stack/2,
	      empty_queue/1,
	      enqueue/3,
	      dequeue/3,
	      member_queue/2,
	      empty_set/1,
	      member_set/2,
	      add_to_set/3,
	      remove_from_set/3,
	      union/3,
	      intersection/3,
	      set_diff/3,
	      subset/2,
	      equal_set/2
	  ]).

%%% List %%%

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

writelist([ ]) :- nl.
writelist([H|T]) :- write(' '), write(H), writelist(T).

%%% Stack %%%

reverse_print_stack(S) :- empty_stack(S).
reverse_print_stack(S) :-
  stack(E, Rest, S),
  reverse_print_stack(Rest),
  write(E), nl.

empty_stack([ ]).

stack(Top, Stack, [Top|Stack]).

member_stack(Element, Stack) :- member(Element, Stack).

%%% Queue %%%

empty_queue( [ ] ).

enqueue(E,[],[E]).
enqueue(E,[H|T],[H|W]) :- enqueue(E,T,W).

dequeue(E,[E|T],T).
dequeue(E,[E|_],_).

member_queue(E,Q) :- member(E,Q).

%%% Set %%%

empty_set( [ ] ).

member_set(E,S) :- member(E,S).

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T) :- !.
remove_from_set(E, [H|T], [H|T_new]) :-
	remove_from_set(E, T, T_new), !.

union([], S, S).
union([H|T], S, S_new) :-
	union(T, S, S2),
	add_to_set(H, S2, S_new).

intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
	member_set(H, S),
	intersection(T, S, S_new),!.
intersection([_|T], S, S_new) :-
	intersection(T, S, S_new),!.

set_diff([], _, []).
set_diff([H|T], S, T_new) :-
	member_set(H, S),
	set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :-
	set_diff(T, S, T_new), !.

subset([], _).
subset([H|T], S) :-
	member_set(H, S),
	subset(T, S).

equal_set(S1, S2) :-
	subset(S1, S2),
	subset(S2, S1).

