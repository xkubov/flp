/**
 * FLP Proeject 2 Turing Machine
 *
 * This program simulates actions of Nondeterministic Turing Machine
 * that are provided on STDIN.
 *
 * @author Peter Kubov (xkubov06@stud.fit.vutbr.cz)
 * @date 21. 03. 2020
 */

/**
 * Declatation of dynamic predicate transition.
 */
:- dynamic transition/4.

main([]) :- main_control, !.
main(['-t']) :-
    statistics(runtime,[Start|_]),
    main_control,
    statistics(runtime,[End|_]),
    Runtime is End - Start,
    format('~f\n', [Runtime]), !.
main(['-h']) :- usage, !.
main(Args) :- format('error: invalid arguments combination: ~w\n', [Args]), usage, halt(1).

main_control :-
    catch(
        catch((
            read_lines(X), parse_input(X, Tape),
            simulate_machine('S', Tape)
        ), error(MSG), (
            format('error: ~w\n', [MSG]), halt(1)
        )
    ), abnormal_termination(_), true
).

/**
 * Simulates machine on tape from starting nonterminal StartN.
 */
simulate_machine(StartN,Tape) :-
    format_configuration(StartN, Tape, 0, Cfg),
    get_head(0, Tape, Head),
    % start simulation of tape on input from nonterminal S
    findall(
	action_path(transition(StartN, Head, NQ, Action), 0, Tape, [Cfg]),
	transition(StartN, Head, NQ, Action),
	ActionPaths
    ),
    try_action_paths(ActionPaths).

/**
 * Implements BFS for finding the most suitable action path.
 */
try_action_paths([]) :- true.
try_action_paths([action_path(transition(_, _, Q, Action), Pos, Tape, CFG)| AP]) :-
     catch((
		do_action(Tape, Pos, Action, NewTape, NewPos),
		format_configuration(Q, NewTape, NewPos, NCFG),
		% If final state is reached halt computation.
		(Q == 'F' -> (
			print_lines([NCFG|CFG]), !)
		; (
			get_head(NewPos, NewTape, Head),
			findall(
				action_path(transition(Q, Head, NQ, NAction), NewPos, NewTape, [NCFG|CFG]),
				transition(Q, Head, NQ, NAction),
				ActionPaths
			),
			append(AP, ActionPaths, NextAP),
			try_action_paths(NextAP)
		))
	), abnormal_termination(_), (
		% Try next action path
		try_action_paths(AP)
	)).

/**
 * Substitutes symbol on Tape on specified posistion.
 * Provides new tape as last parameter.
 */
substitute([_|Tape], 0, C, [C|Tape]).
substitute([H|Tape], Pos, C, [H|NTape]) :- dec(Pos, NPos), substitute(Tape, NPos, C, NTape).

/**
 * Moves head on tape/alters position of head. If position of head is moved too far
 * right or left throws abnormal_termination.
 */
move_head(P, D, Tape, NP) :-
    (D == 'L' -> dec(P, NP) ; inc(P, NP)),
    (can_move(NP, Tape) -> (
        % Move head to new position.
        true
    ) ; (
        throw(abnormal_termination('cannot move head'))
    )
).

/**
 * Checks whether position stays on tape.
 *
 * Position must be pozitive integer.
 */
can_move(0, []).
can_move(0, [_|_]).
can_move(Pos, [_|Tape]) :- dec(Pos, NPos), can_move(NPos, Tape).

/**
 * Increments argument.
 */
inc(Pos, NPos) :- NPos is Pos + 1.

/**
 * Decrements argument.
 */
dec(Pos, NPos) :- NPos is Pos - 1.

/**
 * Returns symbol under head of turing machine.
 */
get_head(0, [H|_], H) :- !.
get_head(_, [], _) :- throw(abnormal_termination('head too far')).
get_head(Pos, [_|T], H) :- dec(Pos, NPos), get_head(NPos, T, H).

/**
 * Creates printable format string of the TM configuration.
 */
format_configuration(Q, [], 0, FMT) :- format(atom(FMT), '~w\n', [Q]), !.
format_configuration(Q, Tape, 0, FMT) :- format_configuration(Q, Tape, -1, RST), format(atom(FMT), '~w~w', [Q, RST]), !.
format_configuration(_, [H|T], -1, FMT) :- format_configuration(_, T, -1, RST), format(atom(FMT), '~w~w', [H, RST]), !.
format_configuration(Q, [H|T], Pos, FMT) :- dec(Pos, NP), format_configuration(Q, T, NP, RST), format(atom(FMT), '~w~w', [H, RST]), !.
format_configuration(_, [], _, '\n') :- !.

/**
 * Preforms action on tape specified by parameter.
 */
do_action(Tape, Head, Action, NewTape, NewHead) :- (
    is_tape(Action) -> ( % substitue
        substitute(Tape, Head, Action, NewTape), NewHead = Head
    ) ; ( % move
        move_head(Head, Action, Tape, NewHead), NewTape = Tape
    )
).

/**
 * Prints lines provided in list on input.
 */
print_lines([]).
print_lines([H|T]) :- print_lines(T), write(H).

/**
 * Parses input lines.
 */
parse_input([], _) :- throw(error('no input provided')).
parse_input([H], Tape) :- parse_tape(H, Tape), !.
parse_input([H|T], Tape) :- parse_rule(H), parse_input(T, Tape).

/**
 * Parses line as it is a tape. Throws exception on error.
 */
parse_tape([C], [C]) :- is_tape(C), !.
parse_tape([C|T], [C|R]) :- is_tape(C), parse_tape(T, R), !.
parse_tape([C|_], _) :- format(MSG, 'malformed tape: invalid symbol: "~w"', [C]), throw(error(MSG)).
% empty tape cannot happen.

/**
 * Returns true if character on parameter has syntax of state.
 */
is_state(Q) :- char_type(Q, alpha), char_type(Q, upper).

/**
 * Returns true if character on parameter has syntax of type alphabet.
 */
is_tape(C) :- char_type(C, alpha), char_type(C, lower).

/**
 * Returns true if character has syntax of tape alphabet or R/L.
 */
is_tape_or_LR(C) :- (is_tape(C); C == 'L'; C == 'R').

/**
 * Parses line as it is a rule of a turing machine.
 */
parse_rule([H|_]) :- H == '#', !.
parse_rule([Q,_,T,_,NQ,_,NT]) :- is_state(Q), is_state(NQ), is_tape(T), is_tape_or_LR(NT), assert(transition(Q, T, NQ, NT)), !.
parse_rule(_) :- throw(error('Invalid rule')).

/**
 * Prints array.
 */
read_line(L, C) :- get_char(C), (
    is_eof_eol(C) -> (
        L = [], !
    ) ; (
        read_line(LL,_), [C|LL] = L
    )
).

/**
 * Reads lines from STDIN until EOF is reached.
 */
read_lines(Ls) :- prompt(_, ''), read_line(L,C), (
    C == end_of_file -> (
        Ls=[]
    ) ; (
        read_lines(LLs), [L|LLs] = Ls
    )
).

is_eof_eol(C) :- C == end_of_file.
is_eof_eol(C) :- char_code(C, Code), Code == 10.

/**
 * Helper append predicate.
 *
 * Takes two lists and append them into single list:
 *
 * append([1,2,3], [4,5], Res) -> Res = [1,2,3,4,5]
 */
append([], X, X).
append([X | Y], Z, [X | W]) :- append(Y, Z, W).

usage :-
	write('usage: flp20-log <option>\n'),
	write('<option>:\n'),
	write('    -t prints execution time in miliseconds at the end of output.\n'),
	write('    -h prints this help.\n').
