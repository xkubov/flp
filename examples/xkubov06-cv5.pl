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
 *
 * Returns true if a robot occupies the position P
 * or if hole is present on the postion P.
 */
obsazeno(P) :- robot(_, P); dira(P).

/**
 * Creates new robot with id I on position P.
 *
 * Returns false if a robot or hole is already present on the position.
 * If robot already exists on different position retracts it
 * from that position and moves to the new.  Otherwise creates
 * robot and returns true.
 */
vytvor(I, P) :- obsazeno(P) -> false ; (
    robot(I, PE) -> (
        retract(robot(I,PE)),
        assert(robot(I,P))
    ) ; (
        assert(robot(I,P))
    )
).

/**
 * Creates new hole. If a hole or robot is positioned on the position P
 * returns false.
 */
vytvor(P) :- obsazeno(P) -> false ; (
    assert(dira(P))
).

/**
 * Removes either robot or hole from the position P.
 */
odstran(P) :- obsazeno(P) -> (
    robot(I, P) -> (
        retract(robot(I,P))
    ) ; (
        retract(dira(P))
    )
); true.

/**
 * Returns all positions that are occupied by either robots
 * or holes.
 */
obsazene_pozice(X) :- bagof(P, obsazeno(P), X).

/**
 * Returns all IDs of robots.
 */
obsazene_roboty(X) :- bagof(ID, P^robot(ID, P), X).

inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.
doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).

/**
 * Moves robot. If hole is on a position where robot is
 * going to be moved then robot is destroyed.
 */
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

/**
 * Destroys all the robots.
 */
armageddon :- forall(robot(_, P), vybuch(P)).
vybuch(P) :- odstran(P), vytvor(P).

/**
 * Homework #3: gestures brute force.
 */

g_size(3).

g_test(X:Y) :- g_size(S), X >= 0, Y >= 0, X < S, Y < S.

g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).

/**
 * Finds all possible solutions starting from X:Y
 */
g_one(X:Y, Len, L, R) :- member(X:Y, L) -> false ; (
    length([X:Y|L], Len), % cut finding if we have solution of wanted length
    reverse([X:Y|L], R), ! % path will be in reverse.
).
g_one(X:Y, Len, L, R) :- member(X:Y, L) -> false ; (
    g_move(X:Y, XN:YN), % move to all directions
    g_one(XN:YN, Len, [X:Y|L], R) % check solution in all directions
).

/**
 * Finds all the soltuion of the lenght Len.
 */
g_all(R, Len) :- g_size(S), UB is S-1, between(0,UB,X), between(0,UB,Y), g_one(X:Y, Len, [], R).

/**
 * Find all possible solutions. There are no solutions above size of g_size*g_size.
 * (g_size*g_size) solution covers all the connected points.
 */
g_allLength(R) :- g_size(S), Max is S * S, between(1, Max, Len), g_all(R, Len).
