/**
 * FLP Proeject 2 Turing Machine
 * 
 * This program simulates actions of Turing Machine
 * that are provided on STDIN.
 *
 * @author Peter Kubov (xkubov06@stud.fit.vutbr.cz)
 * @date 21. 03. 2020
 */

main :- 
    catch(
        catch((
            read_lines(X), parse_input(X, Tape),
            % initialize databaze. Set position of head to 0 and empty end configuration.
            assert(tm_head_pos(0)), assert(tm_end_configuration([])),
            % start simulation of tape on input from nonterminal S
            simulate_machine('S', Tape)
        ), error(MSG), (
            format('error: ~w\n', [MSG]), halt(1)
        )
    ), abnormal_termination(MSG), (write('abnormal termination\n'), halt(0))
).

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
move_head(D, Tape) :- tm_head_pos(P),
    (D == 'L' -> dec(P, NP) ; inc(P, NP)),
    (can_move(NP, Tape) -> (
        retract(tm_head_pos(P)), assert(tm_head_pos(NP))
    ) ; (
        throw(abnormal_termination('cannot move head'))
    )
).

/**
 * Checks wheter position stays on tape.
 */
can_move(-1, _) :- false.
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
get_head(Tape, Head) :- tm_head_pos(X), do_get_head(X, Tape, Head).
do_get_head(0, [H|_], H) :- !.
do_get_head(_, [], _) :- throw(abnormal_termination('head too far')).
do_get_head(Pos, [_|T], H) :- dec(Pos, NPos), do_get_head(NPos, T, H).

/**
 * Creates printable format string of the TM configuration.
 */
format_configuration(Q, Tape, FMT) :- tm_head_pos(X), do_format(Q, Tape, X, FMT).
do_format(Q, [], 0, FMT) :- format(atom(FMT), '~w\n', [Q]), !.
do_format(Q, Tape, 0, FMT) :- do_format(Q, Tape, -1, RST), format(atom(FMT), '~w~w', [Q, RST]), !.
do_format(_, [H|T], -1, FMT) :- do_format(_, T, -1, RST), format(atom(FMT), '~w~w', [H, RST]), !.
do_format(Q, [H|T], Pos, FMT) :- dec(Pos, NP), do_format(Q, T, NP, RST), format(atom(FMT), '~w~w', [H, RST]), !.
do_format(_, [], _, '\n') :- !.

/**
 * Resets position of head of TM to position specified by parameter.
 */
reset_position(ResetPos) :- tm_head_pos(X), retract(tm_head_pos(X)), assert(tm_head_pos(ResetPos)).

/**
 * Resets end configuration of TM to end configuration specified by parameter.
 */
reset_tm_end_configuration(NewConfig) :- tm_end_configuration(X), retract(tm_end_configuration(X)), assert(tm_end_configuration(NewConfig)).

/**
 * Adds configuration to the end configuration of TM.
 */
add_to_tm_end_configuration(X) :- tm_end_configuration(EndConfig), reset_tm_end_configuration([X|EndConfig]).

/**
 * Preforms action on tape specified by parameter.
 */
do_action(Tape, Action, NewTape) :- (
    is_tape(Action) -> ( % substitue
        tm_head_pos(X), substitute(Tape, X, Action, NewTape)
    ) ; ( % move
        move_head(Action, Tape), NewTape = Tape
    )
).

/**
 * Prints lines provided in list on input.
 */
print_lines([]).
print_lines([H|T]) :- print_lines(T), write(H). 

/**
 * Tries path of performation with specified action and
 * possible states. If success is not possible returns false.
 */
try_state_paths(_, _, []) :- false.
try_state_paths(Tape, Action, [State|_]) :-
    tm_head_pos(SavePos),
    tm_end_configuration(Save),
    catch((
        do_action(Tape, Action, NewTape),
        simulate_machine(State, NewTape)
    ), abnormal_termination(_), (reset_position(SavePos), reset_tm_end_configuration(Save), false)), !.
try_state_paths(Tape, Action, [_|OtherPaths]) :- try_state_paths(Tape, Action, OtherPaths).

/**
 * Tries more paths of actions specified on parameter. For each action tries
 * more possible states. If no action is possible throws exception -> abnormal termination.
 */
try_action_paths(_, _, []) :- throw(abnormal_termination('no action to do')).
try_action_paths(Q, Tape, [Action|_]) :-
    get_head(Tape, Head),
    bagof(NQ, transition(Q, Head, NQ, Action), NQs),
    try_state_paths(Tape, Action, NQs), !.
try_action_paths(Q, Tape, [_|OtherPaths]) :- try_action_paths(Q, Tape, OtherPaths).

/**
 * Simulates TM on tape specified as parameter. Simulation ends if state F is reached.
 */
simulate_machine('F', Tape) :- 
    format_configuration('F',Tape,Cfg), add_to_tm_end_configuration(Cfg),
    tm_end_configuration(X), print_lines(X), halt(0).
simulate_machine(Q,Tape) :- get_head(Tape, Head), format_configuration(Q,Tape,Cfg), add_to_tm_end_configuration(Cfg),
    bagof(Ac, NQ^transition(Q, Head, NQ, Ac), Actions),
    try_action_paths(Q, Tape, Actions).

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
parse_rule([Q,_,T,_,NQ,_,NT]) :- is_state(Q), is_state(NQ), is_tape(T), is_tape_or_LR(NT), assert(transition(Q, T, NQ, NT)), !.
parse_rule(_) :- throw(error('Invalud rule')).

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
