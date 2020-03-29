main :-
    prompt(_, ''),
    catch(
        catch((
            read_lines(X), parse_input(X, Tape), assert(pos(0)), assert(end_configuration([])), simulate_machine('S', Tape)
        ), error(MSG), (
            format('error: ~w\n', [MSG]), halt(1)
        )
    ), abnormal_termination(MSG), (write(MSG), halt(0))
).

substitute([_|Tape], 0, C, [C|Tape]).
substitute([H|Tape], Pos, C, [H|NTape]) :- dec(Pos, NPos), substitute(Tape, NPos, C, NTape).

can_move(-1, _) :- false.
can_move(0, [_|_]).
can_move(Pos, [_|Tape]) :- dec(Pos, NPos), can_move(NPos, Tape).

inc(Pos, NPos) :- NPos is Pos + 1.
dec(Pos, NPos) :- NPos is Pos - 1.

get_head(Tape, Head) :- pos(X), do_get_head(X, Tape, Head).
do_get_head(0, [H|_], H) :- !.
do_get_head(_, [], _) :- throw(abnormal_termination('head too far')).
do_get_head(Pos, [_|T], H) :- dec(Pos, NPos), do_get_head(NPos, T, H).

move_head(D, Tape) :- pos(P),
    (D == 'L' -> dec(P, NP) ; inc(P, NP)),
    (can_move(NP, Tape) -> (
        retract(pos(P)), assert(pos(NP))
    ) ; (
        throw(abnormal_termination('cannot move head'))
    )
).

format_configuration(Q, Tape, FMT) :- pos(X), do_format(Q, Tape, X, FMT).
do_format(Q, [], 0, FMT) :- format(atom(FMT), '~w\n', [Q]), !.
do_format(Q, Tape, 0, FMT) :- do_format(Q, Tape, -1, RST), format(atom(FMT), '~w~w', [Q, RST]), !.
do_format(_, [H|T], -1, FMT) :- do_format(_, T, -1, RST), format(atom(FMT), '~w~w', [H, RST]), !.
do_format(Q, [H|T], Pos, FMT) :- dec(Pos, NP), do_format(Q, T, NP, RST), format(atom(FMT), '~w~w', [H, RST]), !.
do_format(_, [], _, '\n') :- !.

reset_position(ResetPos) :- pos(X), retract(pos(X)), assert(pos(ResetPos)).

reset_end_configuration(NewConfig) :- end_configuration(X), retract(end_configuration(X)), assert(end_configuration(NewConfig)).

add_to_end_configuration(X) :- end_configuration(EndConfig), reset_end_configuration([X|EndConfig]).

do_action(Tape, Action, NewTape) :- (
    is_tape(Action) -> ( % substitue
        pos(X), substitute(Tape, X, Action, NewTape)
    ) ; ( % move
        move_head(Action, Tape), NewTape = Tape
    )
).

assert_one([H], H).
assert_one(_, _) :- throw(error('more than one action can be done')).

try_state_paths(_, _, []) :- false.
try_state_paths(Tape, Action, [State|_]) :-
    pos(SavePos),
    end_configuration(Save),
    catch((
        do_action(Tape, Action, NewTape),
        simulate_machine(State, NewTape)
    ), abnormal_termination(_), (reset_position(SavePos), reset_end_configuration(Save), false)), !.
try_state_paths(Tape, Action, [_|OtherPaths]) :- try_state_paths(Tape, Action, OtherPaths).

print_lines([]).
print_lines([H|T]) :- print_lines(T), write(H). 

try_action_paths(_, _, []) :- throw(abnormal_termination('no action to do')).
try_action_paths(Q, Tape, [Action|_]) :-
    get_head(Tape, Head),
    bagof(NQ, transition(Q, Head, NQ, Action), NQs),
    try_state_paths(Tape, Action, NQs), !.
try_action_paths(Q, Tape, [_|OtherPaths]) :- try_action_paths(Q, Tape, OtherPaths).

simulate_machine('F', Tape) :- 
    format_configuration('F',Tape,Cfg), add_to_end_configuration(Cfg),
    end_configuration(X), print_lines(X), halt(0).
simulate_machine(Q,Tape) :- get_head(Tape, Head), format_configuration(Q,Tape,Cfg), add_to_end_configuration(Cfg),
    bagof(Ac, NQ^transition(Q, Head, NQ, Ac), Actions),
    try_action_paths(Q, Tape, Actions).

parse_input([], _) :- throw(error('no input provided')).
parse_input([H], Tape) :- parse_tape(H, Tape), !.
parse_input([H|T], Tape) :- parse_rule(H), parse_input(T, Tape).

parse_tape([C], [C]) :- is_tape(C), !.
parse_tape([C|T], [C|R]) :- is_tape(C), parse_tape(T, R), !.
parse_tape([C|_], _) :- format(MSG, 'malformed tape: invalid symbol: "~w"', [C]), throw(error(MSG)).
% empty tape cannot happen.

is_state(Q) :- char_type(Q, alpha), char_type(Q, upper).
is_tape(C) :- char_type(C, alpha), char_type(C, lower).
is_tape_or_LR(C) :- (is_tape(C); C == 'L'; C == 'R').

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

read_lines(Ls) :- read_line(L,C), (
    C == end_of_file -> ( 
        Ls=[]
    ) ; (
        read_lines(LLs), [L|LLs] = Ls
    )
).

is_eof_eol(C) :- C == end_of_file. 
is_eof_eol(C) :- char_code(C, Code), Code == 10.
