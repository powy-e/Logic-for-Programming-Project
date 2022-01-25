% IDENTIFICACAO E COISA E TAL

% Extrai_ilhas_Linhas(Numero da Linha, Linha, Lista de Ilhas existentes)/3
% Permite extrair as ilhas de uma linha
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




% Predicado ilhas(Puz, Ilhas)/2 premite a partir de um Puzzle obter uma lista
% com Ilhas ordenada da esquerda p/a a direita e debaixo p/a cima
ilhas_aux(_, Ilhas_Acc, N_Linha, Ilhas_Acc, L):- N_Linha > L.
ilhas_aux(Puz, Ilhas_prev, N_Linha, Ilhas_Final, L):- 
    nth1(N_Linha, Puz, Linha), extrai_ilhas_linha(N_Linha, Linha, Ilhas),
     NovoN_Linha is N_Linha +1,
     append(Ilhas_prev, Ilhas, Ilhas_Acc),
     ilhas_aux(Puz, Ilhas_Acc, NovoN_Linha, Ilhas_Final, L).

ilhas(Puz, Ilhas) :-
    length(Puz, L), ilhas_aux(Puz, [], 1, Ilhas, L).


% VIZINHAS

mesma_ilha(L, C, ilha(_,(L,C))).
mesma_linha(L,ilha(_,(L,_))).
mesma_coluna(C, ilha(_,(_,C))).


%%%%%%% Exportar tudo para uma usando ..[] e uma Flag
escolhe_elementos_prox_horizontais(_, [], []).
escolhe_elementos_prox_horizontais(C, Ilhas, El_horizontais) :-
    escolhe_elementos_prox_horizontais(C, Ilhas, El_horizontais, []).
escolhe_elementos_prox_horizontais(_, [], El_Acc, El_Acc).
escolhe_elementos_prox_horizontais(C, [ilha(N,(L,CI))|R_Ilhas], El_horizontais, El_Acc) :-
    C < CI, append(El_Acc, [ilha(N,(L,CI))], El_horizontais);
    C > CI, escolhe_elementos_prox_horizontais(C, R_Ilhas, El_horizontais, [ilha(N,(L,CI))]).

escolhe_elementos_prox_verticais(_, [], []).
escolhe_elementos_prox_verticais(L, Ilhas, V_Linhas) :-
    escolhe_elementos_prox_verticais(L, Ilhas, V_Linhas, []).
escolhe_elementos_prox_verticais(_, [], El_Acc, El_Acc).
escolhe_elementos_prox_verticais(L, [ilha(N,(LI,C))|R_Ilhas], V_Linhas, Linhas_Acc) :-
    L < LI,  append(Linhas_Acc, [ilha(N,(LI,C))], V_Linhas);
    L > LI,  escolhe_elementos_prox_verticais(L, R_Ilhas, V_Linhas, [ilha(N,(LI,C))]).
 
junta_linha_coluna(Horizontais, Verticais, L, Vizinhas) :-
    length(Verticais, 0), Vizinhas = Horizontais;
    length(Verticais, 1),
     Verticais = [ilha(_,(L1,_))],
     L>L1,
     append(Verticais, Horizontais, Vizinhas);
    length(Verticais, 1),
     append(Horizontais, Verticais, Vizinhas);
    Verticais = [P|R], append([P], Horizontais, Pre), append(Pre, R, Vizinhas).
    
    
vizinhas([], _,[]).
vizinhas(Ilhas, ilha(_,(L,C)), Vizinhas) :-
    exclude(mesma_ilha(L,C), Ilhas, F_Ilhas), %Remove o proprio elemento
    include(mesma_linha(L), F_Ilhas, Ilhas_horizontais), 
    include(mesma_coluna(C), F_Ilhas, Ilhas_verticais),
    escolhe_elementos_prox_horizontais(C, Ilhas_horizontais, V_horizontais),
    escolhe_elementos_prox_verticais(L, Ilhas_verticais, V_verticais),
    junta_linha_coluna(V_horizontais, V_verticais, L, Vizinhas).



%% Estado


estado_aux(_, Estado_Acumulado, Len_Ilhas, Estado_Acumulado, Len_Ilhas).
estado_aux(Ilhas, Estado, Index, Estado_Acumulado, Len_Ilhas):-
    nth0(Index, Ilhas, Ilha_Selecionada),
    vizinhas(Ilhas, Ilha_Selecionada, Vizinhas),
    Linha = [Ilha_Selecionada, Vizinhas, []],
    N_index is Index+1,
    append(Estado_Acumulado, [Linha], N_Acumulado),
    estado_aux(Ilhas, Estado, N_index, N_Acumulado, Len_Ilhas).


estado(Ilhas, Estado) :-  
    length(Ilhas, L), 
    estado_aux(Ilhas, Estado, 0, [], L).



% posicoes_entre



posicoes_entre((L,C1), (L,C2), Posicoes) :-
    (C1 < C2 -> CI = C1, CF = C2;CI = C2, CF = C1),
    C_I is CI + 1,
    C_F is CF -1,
    findall((L,C3), between(C_I, C_F, C3), Posicoes).
    
posicoes_entre((L1,C), (L2,C), Posicoes) :-
    (L1 < L2 -> LI = L1, LF = L2;LI = L2, LF = L1),
    L_I is LI + 1,
    L_F is LF -1,
    findall((L3,C), between(L_I, L_F, L3), Posicoes).



%cria_ponte

cria_ponte((L1,C1), (L2,C2), ponte(PosF1, PosF2)):- 
    L1=< L2, C1=< C2, PosF1 = (L1,C1), PosF2 = (L2,C2);
    PosF1 = (L2,C2), PosF2 = (L1,C1).


% caminho_livre/5

caminho_livre(Pos1, Pos2, _, ilha(_,(L1,C1)), ilha(_,(L2,C2))):-
    member((L1,C1),[Pos1, Pos2]), member((L2,C2),[Pos1,Pos2]).
caminho_livre(_, _, Posicoes, ilha(_,(L1,C1)), ilha(_,(L2,C2))):-
    posicoes_entre((L1,C1),(L2,C2), Pos_N),
    findall(X, (member(X, Pos_N), member(X, Posicoes)), Ocupado),
    Ocupado = [].



% atualiza_vizinhas_entrada/5

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I,Vz, Pt], [I, N_Vz, Pt]):-
    findall(Y, (member(Y,Vz), caminho_livre(Pos1, Pos2, Posicoes, I, Y)), N_Vz).
    


% actualiza_vizinhas_apos_pontes/4

actualiza_vizinhas_apos_pontes([], _, _, []).
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, [[I,Vz, Pt]]):-
    length(Estado, 1),
    posicoes_entre(Pos1,Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Estado, [I,Vz, Pt]).
actualiza_vizinhas_apos_pontes([P|R], Pos1, Pos2, NovoEstado):-
    posicoes_entre(Pos1,Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, P, [I,Vz, Pt]),
    actualiza_vizinhas_apos_pontes(R, Pos1, Pos2, Estado_seguinte),
    append([[I,Vz, Pt]], Estado_seguinte, NovoEstado).
    

% ilhas_terminadas/2
ilhas_terminadas(Estado, Ilhas_term):-
    findall(
        ilha(N, (L,C)), 
        (
            member([ilha(N, (L,C)), _, Pontes], Estado), N\='X', length(Pontes, N)
            ),
         Ilhas_term).


% 2.11 tira_ilhas_terminadas_entrada/3

membro_ao_contrario(Lista, El):-
    member(El, Lista).
    
tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vz, Pt], [I, N_Vz, Pt]) :-
    exclude(membro_ao_contrario(Ilhas_term), Vz, N_Vz).

% 2.12 tira_ilhas_terminadas/3
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


%2.13 Predicado marca_ilhas_terminadas_entrada/3

marca_ilhas_terminadas_entrada(Ilhas_term,[ilha(N,Pos), Vz, Pt], [ilha(X,Pos), Vz, Pt]):-
    member(ilha(N,Pos), Ilhas_term) -> X='X'; X=N.
    
%2.14 Predicado marca_ilhas_terminadas/3
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


%2.15 trata_ilhas_terminadas/2

trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Meio),
    marca_ilhas_terminadas(Meio, Ilhas_term, Novo_estado).

% 2.16 junta_pontes/5
adiciona_multiplas(Coisa, Vezes, List):-
    length(List, Vezes), maplist(=(Coisa), List).

adiciona_pontes(_, _, _, [], []).
adiciona_pontes(Ilha1, Ilha2, Pontes, [[Ilha3,Vz, Pt]|R], Estado_Modificado):-
    Ilha3 \= Ilha1, Ilha3 \= Ilha2,
        adiciona_pontes(Ilha1, Ilha2, Pontes, R, Resto_Estado),
        Estado_Modificado = [[Ilha3,Vz, Pt]|Resto_Estado];
    append(Pt, Pontes, N_Pt),
        adiciona_pontes(Ilha1, Ilha2, Pontes, R, Resto_Estado),
        Estado_Modificado = [[Ilha3,Vz, N_Pt]|Resto_Estado].
    
    
junta_pontes(Estado, Num_Pontes, ilha(N1,(L1,C1)), ilha(N2,(L2,C2)), Novo_Estado) :-
    cria_ponte((L1,C1), (L2,C2), Ponte),
    adiciona_multiplas(Ponte, Num_Pontes, Pontes),
    adiciona_pontes(ilha(N1,(L1,C1)), ilha(N2,(L2,C2)), Pontes, Estado, Estado_Pontes),
    actualiza_vizinhas_apos_pontes(Estado_Pontes, (L1,C1), (L2,C2), Estado_Atualizado),
    trata_ilhas_terminadas(Estado_Atualizado, Novo_Estado).

    


