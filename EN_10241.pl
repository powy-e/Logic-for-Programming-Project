% IDENTIFICACAO E COISA E TAL

% Extrai_ilhas_Linhas(Numero da Linha, Linha, Lista de Ilhas existentes)
extrai_ilhas_linha_aux(_, _, Ilhas, Index, L, Ilhas) :- Index > L.
extrai_ilhas_linha_aux(N_L, Linha, Ilhas, Index, L, F_Ilhas) :- 
    nth1(Index, Linha, P),
     P=\= 0,
     append(Ilhas, [ilha(P,(N_L,Index))], Novas_ilhas),
     N_index is Index + 1, 
     extrai_ilhas_linha_aux(N_L, Linha, Novas_ilhas, N_index, L, F_Ilhas);

     N_index is Index + 1, 
     extrai_ilhas_linha_aux(N_L, Linha, Ilhas, N_index, L, F_Ilhas).




extrai_ilhas_linha(_, [], []). %REDUNDANTE???? WHO KNOWS NOT SHY NOT ME ITZY
extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    length(Linha, L), 
     extrai_ilhas_linha_aux(N_L, Linha, [], 1, L, Ilhas).



% Predicado ilhas(Puz, Ilhas) premite a partir de um Puzzle obter uma lista
% com Ilhas ordenada da esquerda p/a a direita e debaixo p/a cima
ilhas_aux(_, Ilhas_Acc, N_Linha, Ilhas_Acc, L):- N_Linha > L.
ilhas_aux(Puz, Ilhas_prev, N_Linha, Ilhas_Final, L):- 
    nth1(N_Linha, Puz, Linha), extrai_ilhas_linha(N_Linha, Linha, Ilhas),
     NovoN_Linha is N_Linha +1,
     append(Ilhas_prev, Ilhas, Ilhas_Acc),
     ilhas_aux(Puz, Ilhas_Acc, NovoN_Linha, Ilhas_Final, L).

ilhas(Puz, Ilhas) :-
    length(Puz, L), ilhas_aux(Puz, [], 1, Ilhas, L).


% BIZINHAS

mesma_linha(L,ilha(_,(X,_))) :- X==L.
mesma_coluna(C, ilha(_,(_,Y))) :- Y==C.
mesma_ilha(L, C, ilha(_,(X,Y))) :- Y==C, X==L.



ilha_lados_aux(_,[], Acc, Acc).
ilha_lados_aux(C_ilha, [ilha(N,(L,C))|_], Lados, Acc) :- C_ilha < C, (Acc = []->[ilha(N,(L,C))]= Lados;append([Acc], [ilha(N,(L,C))] , Lados)).
%ilha_lados_aux(C_ilha, [ilha(N1,(L1,C1))|ilha(N2,(L2,C2))], [ilha(N1,(L1,C1)) | ilha(N2,(L2,C2))], _) :- C1< C_ilha, C_ilha < C2.
ilha_lados_aux(C_ilha, [ilha(N,(L,C))|R], Filtradas, _) :-
    C< C_ilha, ilha_lados_aux(C_ilha, R, Filtradas, ilha(N,(L,C))).
ilha_lados(ilha(_,(_,C_ilha)),Ilhas, Filtradas) :-
    ilha_lados_aux(C_ilha, Ilhas, Filtradas, []).



ilha_alturas_aux(_,[], Acc, Acc).
ilha_alturas_aux(L_ilha, [ilha(N,(L,C))|_], Alturas, Acc) :- L_ilha < L, (Acc = []->[ilha(N,(L,C))]= Alturas;append([Acc], [ilha(N,(L,C))] , Alturas)).
%ilha_alturas_aux(L_ilha, [ilha(N1,(L1,C1))|ilha(N2,(L2,C2))], [ilha(N1,(L1,C1)) | ilha(N2,(L2,C2))], _) :- L1< L_ilha, L_ilha < L2.
ilha_alturas_aux(L_ilha, [ilha(N,(L,C))|R], Filtradas, _) :-
    L< L_ilha, ilha_alturas_aux(L_ilha, R, Filtradas, ilha(N,(L,C))).
ilha_alturas(ilha(_,(L_ilha,_)),Ilhas, Filtradas) :-
    ilha_alturas_aux(L_ilha, Ilhas, Filtradas, []).


vizinhas_ordenar_helper(_, [], Ilhas_Linha, Ilhas_Linha):- writeln("TSAFJDKshfKJSDHFJKDSH").
vizinhas_ordenar_helper(L, [ilha(N1,(L1,C1))], Ilhas_Linha, Vizinhas) :-
    L1<L,
    %writeln("AKDHSADHJSAKD"),
    %writeln("h").
    %C1<C -> append(ilha(N1,(L1,C1)), Ilhas_Linha, Vizinhas);append(ilha(N1,(L1,C1)), Ilhas_Coluna, Vizinhas).
    append(ilha(N1,(L1,C1)), Ilhas_Linha, Vizinhas).
vizinhas_ordenar_helper(_, [ilha(N1,(L1,C1))], Ilhas_Linha, Vizinhas) :-
    %writeln("AKDHSADHJSAKD"),
    %writeln("h").
    %C1<C -> append(ilha(N1,(L1,C1)), Ilhas_Linha, Vizinhas);append(ilha(N1,(L1,C1)), Ilhas_Coluna, Vizinhas).
    append(Ilhas_Linha, ilha(N1,(L1,C1)), Vizinhas).
vizinhas_ordenar_helper(_, [P|R], Ilhas_Linha, Vizinhas) :-
    writeln("HEYYYYY"),
    writeln(P),
    writeln(R),
    append([P], Ilhas_Linha, Pre_Vizinhas),append(Pre_Vizinhas, R, Vizinhas).

   

vizinhas(Ilhas, ilha(N,(L,C)), Vizinhas) :- 
    exclude(mesma_ilha(L,C), Ilhas, F_Ilhas),
    include(mesma_linha(L), F_Ilhas, Ilhas_linha),
    include(mesma_coluna(C), F_Ilhas, Ilhas_coluna),
    ilha_lados(ilha(N,(L,C)),Ilhas_linha, Filtradas_Linha),
    ilha_alturas(ilha(N,(L,C)),Ilhas_coluna, Filtradas_Coluna),writeln(" "),
    vizinhas_ordenar_helper(C, Filtradas_Coluna, Filtradas_Linha, Vizinhas).
    
%:- Ilhas = [ilha(2,(1,3)),ilha(1,(3,1)),ilha(6,(3,3)),ilha(1,(3,5))], vizinhas(Ilhas, ilha(6, (3, 3)), Vizinhas), writeln(Vizinhas); writeln(false).
% output: [ilha(2,(1,3)),ilha(1,(3,1)),ilha(1,(3,5)),ilha(2,(5,3))]
