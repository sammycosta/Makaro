:- module(solve, [solve_makaro/4, region_size/3, max_regions/2, get_regions_sizes/4]).
:- use_module(library(clpfd)).

% Quantidade de ocorrências de um elemento em uma lista
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
region_size(RegionNumber, [Head|Tail], TotalSize) :-
    ocr(RegionNumber, Head, LineOcurr),
    region_size(RegionNumber, Tail, TotalSize2),
    TotalSize is LineOcurr + TotalSize2.

% Transforma uma matriz de string em uma matriz de int
string_matrix_to_int_matrix([], []).
string_matrix_to_int_matrix([Row|Rest], [NumberRow|NumberRest]) :-
  maplist(string_to_int, Row, NumberRow),
  string_matrix_to_int_matrix(Rest, NumberRest).

% Transforma uma string em um inteiro, caso não seja possível, retorna 0
string_to_int(String, Number) :-
  (catch(number_string(Number, String), _, fail) -> true ; Number = 0).

% Retorna o domínio de uma região baseado no seu tamanho
get_domain(RegionsSizes, Value, N) :-
    string_to_int(Value, IntValue),
    IntValue > 0,
    nth1(IntValue, RegionsSizes, Lim),
    N in 1..Lim.
get_domain(_, Value, 0) :-
    string_to_int(Value, IntValue),
    IntValue =:= 0.

% Retorna o número total de regiões
max_regions(Regions, MaxNumberRegions) :-
    string_matrix_to_int_matrix(Regions, IntReg),
    append(IntReg, FlatRegions),
    max_list(FlatRegions, MaxNumberRegions).

% Retorna uma lista com o tamanho de cada região, em ordem crescente
get_regions_sizes(_, MaxNumber, Number, []) :-
    MaxNumber < Number, !.
get_regions_sizes(RegionsMatrix, MaxNumber, Number, [RegionValue]) :-
    MaxNumber =:= Number,
    region_size(Number, RegionsMatrix, RegionValue).
get_regions_sizes(RegionsMatrix, MaxNumber, Number, [RegionValue|ListOfRegions]) :-
    MaxNumber > Number,
    region_size(Number, RegionsMatrix, RegionValue),
    Num2 is Number + 1,
    get_regions_sizes(RegionsMatrix, MaxNumber, Num2, ListOfRegions).

% Retorna todos os índices nos quais um elemnto é encontrado em uma lista
indexes(List, E, Is) :-
    findall(N, nth0(N, List, E), Is).

% Extrai o elemento de uma determinada posição de uma lista
nth([H|_], H, 0) :- !.
nth([_|T], X, I) :- I >= 0, I2 is I - 1, nth(T,X,I2), !.

% Condição para que os elementos de uma mesma região sejam distintos
apply_region_rule(DomainsList, Regions, Reg) :- 
    indexes(Regions, Reg, PositionsReg),                                   
    maplist(nth(DomainsList), ElemReg, PositionsReg),
    all_distinct(ElemReg).

% Tranforma uma lista em uma matriz, dado o tamanho 
list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|MatrixRest]) :-
    length(Row, Size),
    append(Row, Rest, List),
    list_to_matrix(Rest, Size, MatrixRest).

% Condição que os elementos adjacentes de uma lista sejam distintos sem contar com o 0
check_adjacent_distinct([]).
check_adjacent_distinct([_]).
check_adjacent_distinct([X, Y | Rest]) :-
    (X #= 0 ; Y #= 0),
    check_adjacent_distinct([Y | Rest]).
check_adjacent_distinct([X, Y | Rest]) :-
    X #\= Y,
    check_adjacent_distinct([Y | Rest]).

% Retorna posições que possuem seta
arrow_positions(RegionsMatrix, ArrowPositions) :-
    Arrows = ['D', 'U', 'L', 'R'],
    findall((Row, Col), (
        nth0(Row, RegionsMatrix, RowList),
        nth0(Col, RowList, Value),
        member(Value, Arrows)
    ), ArrowPositions).

% Verifica se uma posição está dentro dos limites da matriz
check_bounds(N, Row, Col) :-
    Row >= 0,
    Col >= 0,
    Row < N,
    Col < N.

% Retorna o elemento na posição da matriz caso esteja dentro dos limites, 0 caso contrário
element_within_bounds(N, Row, Col, Matrix, Value) :-
    check_bounds(N, Row, Col),
    !,
    nth0(Row, Matrix, RowList),
    nth0(Col, RowList, Value).
element_within_bounds(_, _, _, _, 0).

% Aplica regra onde a maior posição deve ser a apontada pela seta
apply_arrow_rule(N, MatResult, StrRegions, (Row, Col)) :-
    nth0(Row, StrRegions, RowList),
    nth0(Col, RowList, Arrow),
    RowAbove is Row - 1,
    RowBelow is Row + 1,
    ColLeft is Col - 1,
    ColRight is Col + 1,
    element_within_bounds(N, RowAbove, Col, MatResult, ValueAbove),
    element_within_bounds(N, RowBelow, Col, MatResult, ValueBelow),
    element_within_bounds(N, Row, ColLeft, MatResult, ValueLeft),
    element_within_bounds(N, Row, ColRight, MatResult, ValueRight),
    (
        Arrow = 'D' ->
            ValueBelow #> ValueAbove, ValueBelow #> ValueLeft, ValueBelow #> ValueRight
        ;
        Arrow = 'U' ->
            ValueAbove #> ValueBelow, ValueAbove #> ValueLeft, ValueAbove #> ValueRight
        ;
        Arrow = 'L' ->
            ValueLeft #> ValueAbove, ValueLeft #> ValueBelow, ValueLeft #> ValueRight
        ;
        Arrow = 'R' ->
            ValueRight #> ValueAbove, ValueRight #> ValueBelow, ValueRight #> ValueLeft
        ;
        true
    ).

% Resolve o puzzle
solve_makaro(Board, RegionsMatrix, RegionsSizes, MatResult) :-
    length(Board, L),
    append(Board, FlatBoard),
    append(RegionsMatrix, FlatRegions),

    maplist(get_domain(RegionsSizes), FlatRegions, DomainsList),
    
    length(RegionsSizes, Max), % Max = número de regiões
    numlist(1, Max, Regs), % Lista de 1 a Max
    maplist(number_string, Regs, RegsStr),
    maplist(apply_region_rule(DomainsList, FlatRegions), RegsStr),

    DomainsList = FlatBoard,

    % Aplica regra das adjacências linha por linha da matriz, e então transpõe e aplica nas colunas
    list_to_matrix(DomainsList, L, MatResult),
    maplist(check_adjacent_distinct, MatResult),
    transpose(MatResult, TransposedBoard),
    maplist(check_adjacent_distinct, TransposedBoard),

    % Posições das setas e aplicação da regra
    maplist(atom_string, StrFlatRegions, FlatRegions), % Transforma em string pois não encontrava 'D', etc
    list_to_matrix(StrFlatRegions, L, StrRegions),
    arrow_positions(StrRegions, PositionsArrows),
    maplist(apply_arrow_rule(L, MatResult, StrRegions), PositionsArrows).
    
    maplist(label, MatResult).