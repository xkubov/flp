:- initialization(main, main).

main(_) :- catch((
        read_lines(X), parse_input(X, Tape), simulate_machine('S', Tape)
    ), error(MSG), (
        format('error: ~w\n', [MSG]), halt(1)
    )
).

substitute([_|Tape], 0, C, [C|Tape]).
substitute([H|Tape], Pos, C, [H|NTape]) :- substitute(Tape, Pos-1, C, NTape).

can_move(-1, _) :- false.
can_move(0, [_|_]).
can_move(Pos, [_|Tape]) :- can_move(Pos-1, Tape).

move_head(D, Tape) :- pos(P),
    (D == 'L' -> NP = P-1 ; NP = P+1),
    (can_move(NP, Tape) -> (
        retract(pos(P)), assert(pos(NP))
    ) ; (
        throw(abnormal_termination)
    )
).

simulate_machine(_,_) :- transition('S', 'a', 'B', 'a').

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
