% Regras:
% Em uma região os números vão de 1 até N (tamanho da região)
% Os números não devem se repetir na mesma região
% O número não deve ser igual a nenhum dos números adjacentes


% Talvez para as regras basicamente vão ter regras simples e daí vai juntando regras

:- module(solve, [makaro/1]).
:- use_module(library(clpfd)).

% makaro(Board) :-
%     flatten(Board, Flat_Board),
%     length(Flat_Board, N),
%     findall(Var, (member(Var, Flat_Board), var(Var)), Vars),
%     Vars ins 1..N, 
%     labeling([], Vars),
%     writeln(Vars).
