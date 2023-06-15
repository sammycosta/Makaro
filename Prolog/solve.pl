% Regras:
% Em uma região os números vão de 1 até N (tamanho da região)
% Os números não devem se repetir na mesma região
% O número não deve ser igual a nenhum dos números adjacentes


% Talvez para as regras basicamente vão ter regras simples e daí vai juntando regras

:- module(solve, [makaro/4, region_size/3, max_regions/2, get_all_regions/4]).
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

% % Função para substituir os números pela quantidade de ocorrências na matriz
% replace_numbers_with_occurrences([], []).
% replace_numbers_with_occurrences([Row|Matrix], [NewRow|NewMatrix]) :-
%     replace_row_numbers_with_occurrences(Row, NewRow),
%     replace_numbers_with_occurrences(Matrix, NewMatrix).

% replace_row_numbers_with_occurrences([], []).
% replace_row_numbers_with_occurrences([Number|Row], [Occurrences|NewRow]) :-
%     var(Number), % Verifica se o elemento é uma variável não instanciada
%     replace_row_numbers_with_occurrences(Row, NewRow).
% replace_row_numbers_with_occurrences([Number|Row], [Occurrences2|NewRow]) :-
%     number(Number), % Verifica se o elemento é um número
%     region_size(Number, [Number|Row], Occurrences),
%     replace_row_numbers_with_occurrences(Row, NewRow).
% replace_row_numbers_with_occurrences([Element|Row], [Element|NewRow]) :-
%     \+ number(Element), % Verifica se o elemento não é um número
%     replace_row_numbers_with_occurrences(Row, NewRow).

% string_matrix_to_number_matrix([], []).
% string_matrix_to_number_matrix([StringRow|StringMatrix], [NumberRow|NumberMatrix]) :-
%     convert_row_to_number(StringRow, NumberRow),
%     string_matrix_to_number_matrix(StringMatrix, NumberMatrix).

% convert_row_to_number([], []).
% convert_row_to_number([StringElement|StringRow], [NumberElement|NumberRow]) :-
%     (number_string(NumberElement, StringElement) ; NumberElement = StringElement),
%     convert_row_to_number(StringRow, NumberRow).

string_matrix_to_int_matrix([], []).
string_matrix_to_int_matrix([Row|Rest], [NumberRow|NumberRest]) :-
  maplist(string_to_int, Row, NumberRow),
  string_matrix_to_int_matrix(Rest, NumberRest).

string_to_int(String, Number) :-
  (catch(number_string(Number, String), _, fail) -> true ; Number = 0).

% get_domain(Lim, N) :-
%     N in 1..Lim.


get_domain(List_of_regions, Value, N) :-
    string_to_int(Value, Int_value),
    % Position is Int_value - 1,
    % nth0(Position, List_of_regions, Lim),
    writeln(Int_value),
    Int_value > 0,
    nth1(Int_value, List_of_regions, Lim),
    writeln(Lim),
    % N #>= 1,
    % N #=< Lim.
    N in 1..Lim.

get_domain(List_of_regions, Value, 0) :-
    string_to_int(Value, Int_value),
    % Position is Int_value - 1,
    % nth0(Position, List_of_regions, Lim),
    writeln(Int_value),
    Int_value =:= 0.

domain(List_of_regions, Value, N) :-
    get_domain(List_of_regions, Value, N).

get_all_regions(_, Max_number, Number, []) :- !
    Max_number < Number.

get_all_regions(Regions_matrix, Max_number, Number, [Region_value]) :-
    Max_number =:= Number,
    region_size(Number, Regions_matrix, Region_value).

get_all_regions(Regions_matrix, Max_number, Number, [Region_value|List_of_regions]) :-
    Max_number > Number,
    region_size(Number, Regions_matrix, Region_value),
    Num2 is Number + 1,
    get_all_regions(Regions_matrix, Max_number, Num2, List_of_regions).

max_regions(Regions, Max_number_regions) :-
    string_matrix_to_int_matrix(Regions, Int_reg),
    writeln(Int_reg),
    append(Int_reg, Flat_regions),
    max_list(Flat_regions, Max_number_regions).

indices(List, E, Is) :-
    % number_string(E1, E),
    findall(N, nth0(N, List, E), Is).

enesimo([H|_], H, 0) :- !.
enesimo([_|T], X, I) :- I >= 0, I2 is I - 1, enesimo(T,X,I2), !.

atRegion(ListMatrix, Regions, Reg) :- 
    indices(Regions, Reg, Positions_reg),   
    writeln(Positions_reg),                                                     
    maplist(enesimo(ListMatrix), Elem_reg, Positions_reg),
    all_distinct(Elem_reg).

list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|MatrixRest]) :-
    length(Row, Size),
    append(Row, Rest, List),
    list_to_matrix(Rest, Size, MatrixRest).

makaro(Board, Regions_matrix, List_of_regions, Mat_result) :-
    length(Board, L),
    append(Board, Flat_Board),
    append(Regions_matrix, Flat_regions),
    % writeln(Flat_regions),
    % writeln(Flat_Board),

    length(List_of_regions, Max),
    numlist(1, Max, Groups),
    maplist(domain(List_of_regions), Flat_regions, Result),
    
    writeln(Result),
    length(List_of_regions, Max),
    numlist(1, Max, Regs),
    maplist(number_string, Regs, Regs_str),
    writeln(Flat_regions),

    % string_matrix_to_int_matrix(Flat_regions, Int_regions),
        
    maplist(atRegion(Result, Flat_regions), Regs_str),

    Result = Flat_Board,
    list_to_matrix(Result, L, Mat_result),
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

    
    

    
    % writeln(Flat_Board),
    % Flat_Board ins 1..9,
    % Board = Flat_Board
    
    % flatten(Board, Flat_Board),
    % length(Flat_Board, N),
    % findall(Var, (member(Var, Flat_Board), var(Var)), Vars),
    % Vars ins 1..N, 
    % labeling([], Vars),
    % writeln(Vars).
