/** FLP Excercise num. 5
 *
 *  This module contains answers to the questions in slides
 *  of the 5th excercise of FLP.
 *
 *  @author Peter Kubov (xkubov06)
 *  @date 31. 03. 2020
 */

/**
 * Homework #1
 */
subbags([], [[]]).
subbags([X|XS], P) :- subbags(XS, P0), addOneToAll(X, P0, P1), append(P0, P1, P).

addOneToAll(_, [], []).
addOneToAll(E, [L|LS], [[E|L]|T]) :- addOneToAll(E, LS, T).


:- dynamic robot/2, dira/1.

/**
 * Homework #2: robots
 */
obsazeno(P) :- robot(_, P); dira(P).

vytvor(I, P) :- obsazeno(P) -> false ; (
    robot(I, PE) -> (
        retract(robot(I,PE)),
        assert(robot(I,P))
    ) ; (
        assert(robot(I,P))
    )
).

vytvor(P) :- obsazeno(P) -> false ; (
    assert(dira(P))
).

odstran(P) :- obsazeno(P) -> (
    robot(I, P) -> (
        retract(robot(I,P))
    ) ; (
        retract(dira(P))
    )
); true.

obsazene_pozice(X) :- bagof(P, obsazeno(P), X).
obsazene_roboty(X) :- bagof(ID, P^robot(ID, P), X).

inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.
doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).
pohni(I, Operace) :- (
    robot(I, Pos) -> (
        Op =.. [Operace, Pos, NPos], call(Op), (
        dira(NPos) -> 
            odstran(Pos)
        ;
            vytvor(I, NPos)
        )
    )
).

armageddon :- forall(robot(_, P), vybuch(P)).
vybuch(P) :- odstran(P), vytvor(P).

g_size(3).

g_test(X:Y) :- g_size(S), X >= 0, Y >= 0, X =< S, Y =< S.

g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).

g_one(X:Y, Len, L, R) :- member(X:Y, L) -> false ; (
    length([X:Y|L], SolLen),
    SolLen == Len,
    reverse([X:Y|L], R), !
).
g_one(X:Y, Len, L, R) :- member(X:Y, L) -> false ; (
    g_move(X:Y, XN:YN),
    g_one(XN:YN, Len, [X:Y|L], R)
).

g_all(R, Len) :- g_size(S), between(0,S,X), between(0,S,Y), g_one(X:Y, Len, [], R).

g_allLength(R) :- g_size(S), Max is S * S, between(1, Max, Len), g_all(R, Len).
