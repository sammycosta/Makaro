load_modules :-
    use_module('readpuzzle.pl'),
    use_module('solve.pl').

% Retorna uma sublista a partir de um determinado index
sublist_from_index(Index, List, Sublist) :- 
    length(List, Length),
    Index =< Length,
    drop(List, Index, Sublist).

drop(List, 0, List).
drop([_|T], N, Sublist) :-
    N > 0,
    N1 is N - 1,
    drop(T, N1, Sublist).


main :-
    load_modules,
    read_file('./puzzle/puzzle_14 _1.txt', File_Result),
    get_size(File_Result, N),
    get_tail(File_Result, Tail),
    get_string_matrix(N, Tail, Regions_matrix),
    sublist_from_index(N, Tail, Tail2),
    get_int_matrix(N, Tail2, Certainties_matrix),
    max_regions(Regions_matrix, Max),
    get_regions_sizes(Regions_matrix, Max, 1, Regions_sizes),
    makaro(Certainties_matrix, Regions_matrix, Regions_sizes, Result),
    maplist(portray_clause, Result),
    halt.