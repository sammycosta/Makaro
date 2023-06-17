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
    style_check(-singleton),
    load_modules,
    read_file('../puzzle_17.txt', FileResult),
    get_size(FileResult, N),
    get_tail(FileResult, Tail),
    get_string_matrix(N, Tail, RegionsMatrix),
    sublist_from_index(N, Tail, Tail2),
    get_int_matrix(N, Tail2, CertaintiesMatrix),
    max_regions(RegionsMatrix, Max),
    get_regions_sizes(RegionsMatrix, Max, 1, RegionsSizes),
    makaro(CertaintiesMatrix, RegionsMatrix, RegionsSizes, Result),
    maplist(portray_clause, Result),
    halt.