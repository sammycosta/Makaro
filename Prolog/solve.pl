:- module(solve, [makaro/4, region_size/3, max_regions/2, get_regions_sizes/4]).
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
region_size(Region_number, [Head|Tail], Total_size) :-
    ocr(Region_number, Head, Line_ocurr),
    region_size(Region_number, Tail, Total_size2),
    Total_size is Line_ocurr + Total_size2.

% Transforma uma matriz de string em uma matriz de int
string_matrix_to_int_matrix([], []).
string_matrix_to_int_matrix([Row|Rest], [NumberRow|NumberRest]) :-
  maplist(string_to_int, Row, NumberRow),
  string_matrix_to_int_matrix(Rest, NumberRest).

% Transforma uma string em um inteiro, caso não seja possível, retorna 0
string_to_int(String, Number) :-
  (catch(number_string(Number, String), _, fail) -> true ; Number = 0).

% Retorna o domínio de uma região baseado no seu tamanho
get_domain(Regions_sizes, Value, N) :-
    string_to_int(Value, Int_value),
    Int_value > 0,
    nth1(Int_value, Regions_sizes, Lim),
    N in 1..Lim.
get_domain(Regions_sizes, Value, 0) :-
    string_to_int(Value, Int_value),
    Int_value =:= 0.

% Precisa ter essa função? Não seria só usar a get_domain?
domain(Regions_sizes, Value, N) :-
    get_domain(Regions_sizes, Value, N).

% Retorna o número total de regiões
max_regions(Regions, Max_number_regions) :-
    string_matrix_to_int_matrix(Regions, Int_reg),
    append(Int_reg, Flat_regions),
    max_list(Flat_regions, Max_number_regions).

% Retorna uma lista com o tamanho de cada região, em ordem crescente
get_regions_sizes(_, Max_number, Number, []) :- !
    Max_number < Number.
get_regions_sizes(Regions_matrix, Max_number, Number, [Region_value]) :-
    Max_number =:= Number,
    region_size(Number, Regions_matrix, Region_value).
get_regions_sizes(Regions_matrix, Max_number, Number, [Region_value|List_of_regions]) :-
    Max_number > Number,
    region_size(Number, Regions_matrix, Region_value),
    Num2 is Number + 1,
    get_regions_sizes(Regions_matrix, Max_number, Num2, List_of_regions).

% Retorna todos os índices nos quais um elemnto é encontrado em uma lista
indices(List, E, Is) :-
    findall(N, nth0(N, List, E), Is).

% Extrai o elemento de uma determinada posição de uma lista? entender isso aqui...
enesimo([H|_], H, 0) :- !.
enesimo([_|T], X, I) :- I >= 0, I2 is I - 1, enesimo(T,X,I2), !.

% Condição para que os elementos de uma mesma região sejam distintos
at_region(Domains_list, Regions, Reg) :- 
    indices(Regions, Reg, Positions_reg),   % Listas com as posições de cada região 
    % writeln(Positions_reg),
    % Aplica domínio da região... Ver melhor...                                                 
    maplist(enesimo(Domains_list), Elem_reg, Positions_reg),
    all_distinct(Elem_reg).

% Tranforma uma lista em uma matriz, dado o tamanho 
list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|MatrixRest]) :-
    length(Row, Size),
    append(Row, Rest, List),
    list_to_matrix(Rest, Size, MatrixRest).

% Aplica a regra em uma lista de que um elemento não deve ser igual ao do lado
check_adjacent_distinct([]).
check_adjacent_distinct([_]).
check_adjacent_distinct([X, Y | Rest]) :-
    (X #= 0 ; Y #= 0), % Não deve contar os 0 nisso
    check_adjacent_distinct([Y | Rest]).
check_adjacent_distinct([X, Y | Rest]) :-
    X #\= Y,
    check_adjacent_distinct([Y | Rest]).

arrow_positions(Regions_matrix, Arrow_positions) :-
    Arrows = ['D', 'U', 'L', 'R'],
    findall((Row, Col), (
        nth0(Row, Regions_matrix, Row_list),
        nth0(Col, Row_list, Value),
        member(Value, Arrows)
    ), Arrow_positions).


check_bounds(N, Row, Col) :-
    Row >= 0,
    Col >= 0,
    Row < N,
    Col < N.

element_within_bounds(N, Row, Col, Matrix, Value) :-
    check_bounds(N, Row, Col),
    nth0(Row, Matrix, RowList),
    nth0(Col, RowList, Value).
element_within_bounds(_, _, _, _, 0).


apply_arrow_rule(N, Mat_result, Str_regions, (Row, Col)) :-
    nth0(Row, Str_regions, RowList),
    nth0(Col, RowList, Arrow),
    RowAbove is Row - 1,
    RowBelow is Row + 1,
    ColLeft is Col - 1,
    ColRight is Col + 1,

    element_within_bounds(N, RowAbove, Col, Mat_result, ValueAbove),
    element_within_bounds(N, RowBelow, Col, Mat_result, ValueBelow),
    element_within_bounds(N, Row, ColLeft, Mat_result, ValueLeft),
    element_within_bounds(N, Row, ColRight, Mat_result, ValueRight),
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
        true % Caso padrão quando a seta não é reconhecida
    ).


% Resolve o puzzle
makaro(Board, Regions_matrix, Regions_sizes, Mat_result) :-
    length(Board, L),
    append(Board, Flat_board),
    append(Regions_matrix, Flat_regions),

    % % writeln(Flat_regions),
    % % writeln(Flat_board),
    maplist(domain(Regions_sizes), Flat_regions, Domains_list),
    
    length(Regions_sizes, Max), % Max = número de regiões

    numlist(1, Max, Regs), % Lista de 1 a Max
    maplist(number_string, Regs, Regs_str),

    maplist(at_region(Domains_list, Flat_regions), Regs_str),

    Domains_list = Flat_board, %  Como ela é atualizada?

    % Aplica regra das adjacências linha por linha da matriz, e então transpõe e aplica nas colunas
    list_to_matrix(Domains_list, L, Initial_result),
    maplist(check_adjacent_distinct, Initial_result),
    transpose(Initial_result, Transposed_board),
    maplist(check_adjacent_distinct, Transposed_board),
    transpose(Transposed_board, Mat_result),

    % Posições das setas
    maplist(atom_string, Str_flat_regions, Flat_regions), % Transforma em string pois não encontrava 'D', etc
    list_to_matrix(Str_flat_regions, L, Str_regions),
    arrow_positions(Str_regions, Positions_arrows),
    % writeln(Positions_arrows),

    % apply_arrow_rule(L, Mat_result, Str_regions, (3,6)),
    maplist(apply_arrow_rule(L, Mat_result, Str_regions), Positions_arrows).
    
    maplist(label, Mat_result).

    % string_matrix_to_number_matrix(Regions_matrix, Number_regions),
    % writeln(Number_regions),
    % replace_numbers_with_occurrences(Number_regions, Max_matrix).
    % writeln(Max_matrix).
    % append(Max_matrix, Max_flat),
    % writeln(Max_flat).
    % nth0(0, Max_flat, First),
    % writeln(First).
    % number_string(Num, First),
    % number(Num).