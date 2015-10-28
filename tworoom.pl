%%%%%%%%% Two Room Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Based on one of the sample programs in:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
	       move/4,go/2,test/0
	   ]).

/* Load the utilities provided */
:- [utils].

/* If the current state and goal are the same... */
/*      Print the moves                          */
plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :-
                /* Blindly make a move */
				move(Name, Preconditions, Actions, _),
                /* Check if the preconditions for that move are true */
				conditions_met(Preconditions, State),
                /* If you made it this far then the move is possible */
				change_state(State, Actions, Child_state),
                /* Make sure the new state is not one we already did */
				not(member_state(Child_state, Been_list)),
                /* Add new state to the list of been states */
				stack(Child_state, Been_list, New_been_list),
                /* Add the new move to the list */
				stack(Name, Moves, New_moves),
            /* Plan your next move */
			plan(Child_state, Goal, New_been_list, New_moves),!.

/* Change the state from a given list of moves */
/* If the list is empty then don't do anything */
change_state(S, [], S).
/* Tail recursively apply the moves to the state */
/*      NOTE: The add and del are simply strings from the move predicate */
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
/* Check if the preconditions P are in the current state S */
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */
/* move(Name, 
*       [Preconditions], 
*       [Moves]). */
move(pickup(X), 
        [handempty, clear(X), on(X, Y, Z), handroom(Z)],
		[del(handempty), del(clear(X)), del(on(X, Y, Z)), add(clear(Y)),	add(holding(X))],
        true).

move(pickup(X), 
        [handempty, clear(X), ontable(X, Z), handroom(Z)],
		[del(handempty), del(clear(X)), del(ontable(X, Z)), add(holding(X))],
        true).

move(putdown(X), 
        [holding(X), handroom(Z)],
		[del(holding(X)), add(ontable(X, Z)), add(clear(X)), add(handempty)],
        true).

move(stack(X, Y), 
        [holding(X), clear(Y), handroom(Z)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y, Z)), add(clear(X))],
        true).

move(goroom(X), 
        [handroom(Y)],
        [del(handroom(X)), add(handroom(Y))],
        (X \= Y)).

/* run commands */
go(S, G) :- plan(S, G, [S], []).

test :- go([handempty, ontable(b,1), on(a, b, 1), clear(a), handroom(1)],
            [handempty, ontable(b,2), on(a, b, 2), clear(a), handroom(1)]).
