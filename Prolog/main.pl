load_modules :-
    use_module('readpuzzle.pl'),
    use_module('solve.pl').

sublist_from_index(Index, List, Sublist) :- 
    length(List, Length),
    Index =< Length,
    drop(List, Index, Sublist).

drop(List, 0, List).
drop([_|T], N, Sublist) :-
    N > 0,
    N1 is N - 1,
    drop(T, N1, Sublist).

ocr(_, [], 0) :- !.
ocr(X, [Element|Rs], V) :-
    atom_string(AtomX, X),
    atom_string(AtomElement, Element),
    AtomX = AtomElement,
    ocr(X, Rs, Ocr),
    V is 1 + Ocr.
ocr(X, [_|Rs], V) :-
    ocr(X, Rs, V).

% Quantidade de elementos em uma região
region_size(_, [], 0) :- !.
region_size(Region_number, [Head|Tail], Total_size) :-
    ocr(Region_number, Head, Line_ocurr),
    region_size(Region_number, Tail, Total_size2),
    Total_size is Line_ocurr + Total_size2.

main :-
    load_modules,
    read_file('./puzzle/puzzle_01.txt', File_Result), writeln(File_Result),
    get_size(File_Result, N), writeln(N),
    get_tail(File_Result, Tail),
    get_string_matrix(N, Tail, Regions_matrix),
    sublist_from_index(N, Tail, Tail2),
    get_int_matrix(N, Tail2, Certainties_matrix), writeln(Certainties_matrix),
    writeln(Regions_matrix),
    region_size(7, Regions_matrix, Total_size), writeln(Total_size),
    makaro(Certainties_matrix).    % halt.