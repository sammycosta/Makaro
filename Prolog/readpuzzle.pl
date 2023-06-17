:- module(readpuzzle, [read_file/2, get_size/2, get_string_matrix/3, get_first/2, get_tail/2, get_int_matrix/3,
                       sublist_from_index/3]).

% Primeira leitura do arquivo -------------------------------------------------

read_file(File, Puzzle) :-
    open(File, read, Stream),
    read_file_lines(Stream, Puzzle),
    close(Stream).

read_file_lines(Stream, Lines) :-
    at_end_of_stream(Stream),
    !,
    Lines = [].
read_file_lines(Stream, [Line|Rest]) :-
    read_line(Stream, Line),
    read_file_lines(Stream, Rest).

read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    atom_codes(LineAtom, LineCodes),
    atomic_list_concat(LineList, ' ', LineAtom),
    maplist(atom_string, LineList, Line).

% Funções de parsing das estruturas adquiridas a partir do arquivo -------------

sublist_from_index(Index, List, Sublist) :- 
    length(List, Length),
    Index =< Length,
    drop(List, Index, Sublist).

drop(List, 0, List).
drop([_|T], N, Sublist) :-
    N > 0,
    N1 is N - 1,
    drop(T, N1, Sublist).

get_size([H|T], X) :-
    get_first(H, V),
    atom_number(V, X).

get_first([H|_], H).

get_tail([_|T], T).

get_string_matrix(1, [H|_], [H]).
get_string_matrix(N, [H|T], [H|L2]) :- 
    N > 1,
    N2 is N-1,
    get_string_matrix(N2, T, L2).

string_list_to_int_list([], []).
string_list_to_int_list([String|Rest], [Int|Result]) :-
    atom_string(Atom, String),
    (   Atom = '0'
    ->  Int = _
    ;   number_string(Int, String)
    ),
    string_list_to_int_list(Rest, Result).

get_int_matrix(1, [H|_], [H2]) :- 
    string_list_to_int_list(H, H2).
get_int_matrix(N, [H|T], [H2|L2]) :- 
    N > 1,
    N2 is N-1,
    string_list_to_int_list(H, H2),
    get_int_matrix(N2, T, L2).