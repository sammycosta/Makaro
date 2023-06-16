:- module(solve, [makaro/4, region_size/3, max_regions/2, get_regions_sizes/4]).
:- use_module(library(clpfd)).

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

% Retorna uma lista com o tamanho de cada região
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

% 
indices(List, E, Is) :-
    findall(N, nth0(N, List, E), Is).

% Extrai o elemento de uma determinada posição de uma lista? entender isso aqui...
enesimo([H|_], H, 0) :- !.
enesimo([_|T], X, I) :- I >= 0, I2 is I - 1, enesimo(T,X,I2), !.

at_region(Domains_list, Regions, Reg) :- 
    indices(Regions, Reg, Positions_reg),   % Listas com as posições de cada região 
    writeln(Positions_reg),
    % Aplica domínio da região... Ver melhor...                                                 
    maplist(enesimo(Domains_list), Elem_reg, Positions_reg),
    all_distinct(Elem_reg).

list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|MatrixRest]) :-
    length(Row, Size),
    append(Row, Rest, List),
    list_to_matrix(Rest, Size, MatrixRest).

% Resolve o puzzle
makaro(Board, Regions_matrix, Regions_sizes, Mat_result) :-
    length(Board, L),
    append(Board, Flat_board),
    append(Regions_matrix, Flat_regions),
    % writeln(Flat_regions),
    % writeln(Flat_board),
    maplist(domain(Regions_sizes), Flat_regions, Domains_list),
    
    length(Regions_sizes, Max), % Max = número de regiões

    numlist(1, Max, Regs), % Lista de 1 a Max
    maplist(number_string, Regs, Regs_str),

    maplist(at_region(Domains_list, Flat_regions), Regs_str),

    Domains_list = Flat_board, %  Como ela é atualizada?
    list_to_matrix(Domains_list, L, Mat_result),
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