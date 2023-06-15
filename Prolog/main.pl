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


main :-
    load_modules,
    read_file('./puzzle/puzzle_01.txt', File_Result), writeln(File_Result),
    get_size(File_Result, N), writeln(N),
    get_tail(File_Result, Tail),
    get_string_matrix(N, Tail, Regions_matrix),
    sublist_from_index(N, Tail, Tail2),
    get_int_matrix(N, Tail2, Certainties_matrix), writeln(Certainties_matrix),
    % regions_matrix(Regions_matrix),
    % writeln(Regions_matrix),
    % problem(Certainties_matrix),
    % writeln(Certainties_matrix),
    region_size(7, Regions_matrix, Total_size), writeln(Total_size),
    max_regions(Regions_matrix, Max),
    get_all_regions(Regions_matrix, Max, 1, Regions_max),
    % writeln(Max),
    writeln(Regions_max),
    makaro(Certainties_matrix, Regions_matrix, Regions_max, Result),
    writeln(Result).    % halt.