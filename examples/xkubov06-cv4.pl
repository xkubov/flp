/** FLP Excercise num. 4
 *
 *  This module contains answers to the questions in slides
 *  of the 4th excercise of FLP.
 *
 *  @author Peter Kubov (xkubov06)
 *  @date 21. 03. 2020
 */

/**
 * Homework #1
 */
parent(R,X) :- mother(R,X).
parent(R,X) :- father(R,X).

sibling(X,Y) :- parent(Z,X), parent(Z,Y).
sister(X,Y) :- woman(X), sibling(X,Y).
grandpa(X,Y) :- father(X,Z), parent(Z,Y).
is_mother(X) :- mother(X,_).
aunt(X,Y) :- woman(X), sibling(X,Z), parent(Z,Y).

/**
 * Homework #2
 *
 * Create join/3 functor that will join two input lists into third one.
 * 
 * Usage:
 * ```
 * join([1,2], [3,4], X).
 * X = [1,2,3,4]
 *
 * join([1,2], X, [1,2,3,4]).
 * X = [3,4]
 * ```
 */
join([], L, L).
join([H|T], L, [H|Z]) :- join(T, L, Z).

/**
 * Homework #3
 *
 * Create reverse with usage of `join` defined before.
 * Usage:
 * ```
 * reverse([1,2,3], Z).
 * Z = [3,2,1]
 * ```
 */
reverse([], []).
reverse([H|T], Res) :- reverse(T, Z), join(Z, [H], Res).

/**
 * Homework #4
 *
 * Create merge that will join two sorted lists into one
 * sorted list.
 *
 * Usage:
 * ```
 * merge([1,3,5], [2,4], Z).
 * Z = [1,2,3,4,5]
 *
 * merge([[],[2,3]], [[1], [4,5,6]], Z).
 * Z = [[], [1], [2,3], [4,5,6]]
 * ```
 */
merge(L, [], L).
merge([], L, L).
merge([X|XS], [Y|YS], [X|T]) :- X @=< Y, merge(XS, [Y|YS], T).
merge([X|XS], [Y|YS], [Y|T]) :- X @> Y, merge([X|XS], YS, T).

/**
 * Homework #5
 *
 * Create nsort functor that will sort list on input.
 *
 * Usage:
 * ```
 * nsort([8,7,1,3], Z).
 * Z = [1,3,7,8]
 *
 * nsort([[1,1,1], [1], [], [1, 1]], Z).
 * Z = [[], [1], [1,1], [1,1,1]]
 * ```
 */
nsort([], []).
nsort([H|T], SL) :- nsort(T, Z), merge([H], Z, SL).

/**
 * Homework #6
 *
 * Create split functor that will split one list into
 * two on delimeter ' '.
 *
 * Usage:
 * ```
 * split([h,e,l,l,o,' ',w,o,r,l,d], Z).
 * Z = [[h,e,l,l,o], [w,o,r,l,d]]
 * ```
 */
split([], [[]]).
split([H|T], [[]|Z]) :- H == ' ', split(T, Z).
split([H|T], [[H|F]|Z]) :- split(T, [F|Z]).

% Bonus: any delim.
% Usage:
% ```
% split2([h,e,l,l,o,#,w,o,r,l,d], #, Z).
% Z = [[h,e,l,l,o], [w,o,r,l,d]]
% ```
split2([], _, [[],[]]).
split2([H|T], D, [[], T]) :- H == D.
split2([H|T], D, [[H|F], S]) :- H \= D, split2(T, D, [F, S]).

/**
 * Homework #7
 *
 * Create zipWith functor that takes functor and two lists.
 * For i = 0 to N applies functor on OP(L1[i], L2[i]) where N
 * is length of shorter list.
 *
 * Usage:
 * ```
 * zipWith(plus, [1, 2, 3], [10, 20, 30, 40], Z).
 * Z = [11, 22, 33]
 * ```
 */
zipWith(_, [], _, []).
zipWith(_, _, [], []).
zipWith(OP, [X|XT], [Y|YT], [XY|XYT]) :-
		call(OP, X, Y, XY),
		zipWith(OP, XT, YT, XYT).

% Bonus: map functor.
% Usage:
%  ```
%  map(inc, [1, 2, 3], X).
%  X = [2, 3, 4]
%  ```
map(_, [], []).
map(OP, [H|T], [X|Z]) :-
		call(OP, H, X),
		map(OP, T, Z).

% Bonus: filter functor.
% Usage:
%  ```
%  filter([>, 3], [1, 2, 3, 4, 5], Z).
%  Z = [4, 5]
%
%  filter([mother, petr], [eva, jana, linda], Z).
%  Z = [eva]
%
%  filter([is_mother], [marie, eva, tomas, jan], Z).
%  Z = [marie, eva]
%  ```
filter(_, [], []).
filter([Pred|Ops], [H|L], [H|Z]) :-
	Cond =.. [Pred|[H|Ops]], call(Cond),
	filter([Pred|Ops], L, Z).
filter(PA, [_|L], Z) :-
	filter(PA, L, Z).

/**
 * Below are helping data used to debug/test above functors.
 */

% Example functor for HW 7.
plus(X, Y, Z) :- Z is X + Y.
inc(X, Y) :- Y is X + 1.

% Example database for HW 1.

man(jan).
man(pavel).
man(robert).
man(tomas).
man(petr).

woman(marie).
woman(jana).
woman(linda).
woman(eva).

father(tomas,jan).
father(jan,robert).
father(jan,jana).
father(pavel,linda).
father(pavel,eva).

mother(marie,robert).
mother(linda,jana).
mother(eva,petr).
