load_modules :-
    use_module('readpuzzle.pl'),
    use_module('solve.pl').

main :-
    style_check(-singleton), % Desativa warnings de singleton
    load_modules,
    read_file('../puzzle_17.txt', FileResult),
    get_size(FileResult, N),
    get_tail(FileResult, Tail),
    get_string_matrix(N, Tail, RegionsMatrix),
    sublist_from_index(N, Tail, Tail2),
    get_int_matrix(N, Tail2, CertaintiesMatrix),
    max_regions(RegionsMatrix, Max),
    get_regions_sizes(RegionsMatrix, Max, 1, RegionsSizes),
    solve_makaro(CertaintiesMatrix, RegionsMatrix, RegionsSizes, Result),
    maplist(portray_clause, Result),
    halt.