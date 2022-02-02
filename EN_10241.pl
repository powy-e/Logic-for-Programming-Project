%102415 - Eduardo Nazario


%//////////////////////////////////////////////////////////////////////////////%
%//                                                                          //%
%//                         Logica para Programacao                          //%
%//                 Projeto - Solucionador de Puzzles Hashi                  //%
%//                                                                          //%
%//                         Eduardo Nazario - 102415                         //%
%//                    eduardo.nazario@tecnico.ulisboa.pt                    //%
%//                                                                          //%
%//////////////////////////////////////////////////////////////////////////////%

%------------------------------------------------------------------------------%

% 2.1
% Predicado extrai_ilhas_Linhas/3
% Permite obter dada uma Linha e o seu numero, as ilhas incluidas nessa linha,

%------------------------------------------------------------------------------%

extrai_ilhas_linha_aux(_, _, Ilhas, Index, L, Ilhas) :- Index > L.
extrai_ilhas_linha_aux(N_L, Linha, Ilhas, Index, L, F_Ilhas) :- 
    nth1(Index, Linha, P),
    P=\= 0,
    append(Ilhas, [ilha(P,(N_L,Index))], Novas_ilhas),
    N_index is Index + 1, 
    extrai_ilhas_linha_aux(N_L, Linha, Novas_ilhas, N_index, L, F_Ilhas);
    N_index is Index + 1, 
    extrai_ilhas_linha_aux(N_L, Linha, Ilhas, N_index, L, F_Ilhas).


extrai_ilhas_linha(_, [], []).
extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    length(Linha, L), 
    extrai_ilhas_linha_aux(N_L, Linha, [], 1, L, Ilhas).




%------------------------------------------------------------------------------%

% 2.2
% Predicado ilhas/2
% Permite com um Puzzle obter uma lista de Ilhas ordenadas,
% isto e da esquerda para a direita e debaixo para cima

%------------------------------------------------------------------------------%

ilhas_aux(_, Ilhas_Acc, N_Linha, Ilhas_Acc, L):- N_Linha > L.
ilhas_aux(Puz, Ilhas_prev, N_Linha, Ilhas_Final, L):- 
    nth1(N_Linha, Puz, Linha), extrai_ilhas_linha(N_Linha, Linha, Ilhas),
    NovoN_Linha is N_Linha +1,
    append(Ilhas_prev, Ilhas, Ilhas_Acc),
    ilhas_aux(Puz, Ilhas_Acc, NovoN_Linha, Ilhas_Final, L).

ilhas(Puz, Ilhas) :-
    length(Puz, L), ilhas_aux(Puz, [], 1, Ilhas, L).


%------------------------------------------------------------------------------%

% 2.3
% Predicado vizinhas/3
% Permite obter quais as Ilhas que sao Vizinhas de uma dada ilha

%------------------------------------------------------------------------------%

mesma_ilha(L, C, ilha(_,(L,C))).
mesma_linha(L,ilha(_,(L,_))).
mesma_coluna(C, ilha(_,(_,C))).


% Seleciona apenas as ilhas Vizinhas no eixo horizontal
escolhe_elementos_prox_horizontais(_, [], []).
escolhe_elementos_prox_horizontais(C, Ilhas, El_horizontais) :-
    escolhe_elementos_prox_horizontais(C, Ilhas, El_horizontais, []).
escolhe_elementos_prox_horizontais(_, [], El_Acc, El_Acc).
escolhe_elementos_prox_horizontais(C, [ilha(N,(L,CI))|R_Ilhas], El_horizontais, El_Acc) :-
    C < CI, append(El_Acc, [ilha(N,(L,CI))], El_horizontais);
    C > CI, 
    escolhe_elementos_prox_horizontais(C, R_Ilhas, El_horizontais, [ilha(N,(L,CI))]).


% Seleciona apenas as ilhas Vizinhas no eixo Vertical
escolhe_elementos_prox_verticais(_, [], []).
escolhe_elementos_prox_verticais(L, Ilhas, V_Linhas) :-
    escolhe_elementos_prox_verticais(L, Ilhas, V_Linhas, []).
escolhe_elementos_prox_verticais(_, [], El_Acc, El_Acc).
escolhe_elementos_prox_verticais(L, [ilha(N,(LI,C))|R_Ilhas], V_Linhas, Linhas_Acc) :-
    L < LI,  append(Linhas_Acc, [ilha(N,(LI,C))], V_Linhas);
    L > LI,  escolhe_elementos_prox_verticais(L, R_Ilhas, V_Linhas, [ilha(N,(LI,C))]).

% Cria uma lista com as Ilhas Vizinhas Ordenadas
junta_linha_coluna(Horizontais, Verticais, L, Vizinhas) :-
    length(Verticais, 0), Vizinhas = Horizontais; 
    
    length(Verticais, 1),           
     Verticais = [ilha(_,(L1,_))],              % se existir uma ilha no eixo vertical,
     L>L1,                                      % se esta se encontrar em cima
     append(Verticais, Horizontais, Vizinhas);  % adiciona-a em Primeiro Lugar
    
    length(Verticais, 1),
     append(Horizontais, Verticais, Vizinhas); %Caso contrario adiciona em Ultimo Lugar

    Verticais = [P|R],                      % Caso existam duas ilhas no eixo vertical,
     append([P], Horizontais, Pre),         % Entao a Primeira sera o Primeiro elemento
     append(Pre, R, Vizinhas).              % E a segunda o Ultimo elemento


vizinhas([], _,[]).
vizinhas(Ilhas, ilha(_,(L,C)), Vizinhas) :-
    exclude(mesma_ilha(L,C), Ilhas, F_Ilhas),
    include(mesma_linha(L), F_Ilhas, Ilhas_horizontais), % Seleciona as ilhas Horizontais
    include(mesma_coluna(C), F_Ilhas, Ilhas_verticais),  % Seleciona as ilhas Verticais
    escolhe_elementos_prox_horizontais(C, Ilhas_horizontais, V_horizontais),
    escolhe_elementos_prox_verticais(L, Ilhas_verticais, V_verticais),
    junta_linha_coluna(V_horizontais, V_verticais, L, Vizinhas).


%------------------------------------------------------------------------------%

% 2.4
% Predicado estado/2
% Permite originar uma lista com 3 elementos, uma Ilha, uma Lista com as
% Vizinhas dessa Ilha e uma Lista com pontes da ilha ([] no inicio)

%------------------------------------------------------------------------------%

estado_aux(_, Estado_Acumulado, Len_Ilhas, Estado_Acumulado, Len_Ilhas).
estado_aux(Ilhas, Estado, Index, Estado_Acumulado, Len_Ilhas):-
    nth0(Index, Ilhas, Ilha_Selecionada),
    vizinhas(Ilhas, Ilha_Selecionada, Vizinhas),
    Linha = [Ilha_Selecionada, Vizinhas, []],
    N_index is Index + 1,
    append(Estado_Acumulado, [Linha], N_Acumulado),
    estado_aux(Ilhas, Estado, N_index, N_Acumulado, Len_Ilhas).


estado(Ilhas, Estado) :-  
    length(Ilhas, L), 
    estado_aux(Ilhas, Estado, 0, [], L).


%------------------------------------------------------------------------------%

% 2.5
% Predicado posicoes_entre/3
% Permite Obter uma Lista com as posicoes localizadas entre duas posicoes

%------------------------------------------------------------------------------%


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


%------------------------------------------------------------------------------%

% 2.6
% Predicado cria_ponte/3
% Permite criar uma Ponte entre duas Posicoes (ponte(POS1, POS2)) 

%------------------------------------------------------------------------------%

cria_ponte((L1,C1), (L2,C2), ponte(PosF1, PosF2)):- 
    L1=< L2, C1=< C2, PosF1 = (L1,C1), PosF2 = (L2,C2);
    PosF1 = (L2,C2), PosF2 = (L1,C1).


%------------------------------------------------------------------------------%

% 2.7
% Predicado caminho_livre/5
% Permite verificar se uma ponte nao obstroi duas Ilhas Vizinhas

%------------------------------------------------------------------------------%

caminho_livre(Pos1, Pos2, _, ilha(_,(L1,C1)), ilha(_,(L2,C2))):-
    member((L1,C1),[Pos1, Pos2]), member((L2,C2),[Pos1,Pos2]).
caminho_livre(_, _, Posicoes, ilha(_,(L1,C1)), ilha(_,(L2,C2))):-
    posicoes_entre((L1,C1),(L2,C2), Pos_N),
    findall(X, (member(X, Pos_N), member(X, Posicoes)), Ocupado),
    Ocupado = [].


%------------------------------------------------------------------------------%

% 2.8
% Predicado actualiza_vizinhas_entrada/5
% Permite actualizar uma entrada para que ilhas deixem de ser Vizinhas
% tiverem o caminho obstruido por uma ponte

%------------------------------------------------------------------------------%

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I,Vz, Pt], [I, N_Vz, Pt]):-
    findall(Y, (member(Y,Vz), caminho_livre(Pos1, Pos2, Posicoes, I, Y)), N_Vz).


%------------------------------------------------------------------------------%

% 2.9
% Predicado actualiza_vizinhas_apos_pontes/4
% Permite obter o Estado apos a atualizacao das ilhas que deixam de ser Vizinhas
% com a construcao de uma ponte

%------------------------------------------------------------------------------%

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

%------------------------------------------------------------------------------%

% 2.10
% Predicado ilhas_terminadas/2
% Permite obter uma lista com todas as ilhas que ja tem associadas a elas 
% todas as pontes possiveis de um estado

%------------------------------------------------------------------------------%

ilhas_terminadas(Estado, Ilhas_term):-
    findall(
        ilha(N, (L,C)), 
        (
            member([ilha(N, (L,C)), _, Pontes], Estado),
            N\='X', 
            length(Pontes, N)
         ),
        Ilhas_term).
    
    
%------------------------------------------------------------------------------%

% 2.11 
% Predicado tira_ilhas_terminadas_entrada/3
% Permite obter uma entrada com as ilhas terminadas removidas da lista de
% vizinhas de outra entrada

%------------------------------------------------------------------------------%


membro_ao_contrario(Lista, El):-
    member(El, Lista).

tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vz, Pt], [I, N_Vz, Pt]) :-
    exclude(membro_ao_contrario(Ilhas_term), Vz, N_Vz).

%------------------------------------------------------------------------------%

% 2.12 
% Predicado tira_ilhas_terminadas/3
% Permite obter um estado em que todas as ilhas terminadas foram removidas das
% suas entradas

%------------------------------------------------------------------------------%

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%------------------------------------------------------------------------------%

% 2.13 
% Predicado marca_ilhas_terminadas_entrada/3
% Permite marcar a ilha de uma entrada se esta pertencer a uma lista de ilhas
% terminadas, isto e substituir o seu numero de pontes por 'X'

%------------------------------------------------------------------------------%

marca_ilhas_terminadas_entrada(Ilhas_term,[ilha(N,Pos), Vz, Pt], [ilha(X,Pos), Vz, Pt]):-
    member(ilha(N,Pos), Ilhas_term) -> X='X'; X=N.

%------------------------------------------------------------------------------%

% 2.14 
% Predicado marca_ilhas_terminadas/3
% Permite obter um estado em que todas as ilhas terminadas foram marcadas nas
% suas entradas


%------------------------------------------------------------------------------%

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%------------------------------------------------------------------------------%

% 2.15 
% Predicado trata_ilhas_terminadas/2
% Permite obter um estado apos a aplicacao de tira_ilhas_terminadas e
% marca_ilhas_terminadas a um outro estado

%------------------------------------------------------------------------------%

trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Meio),
    marca_ilhas_terminadas(Meio, Ilhas_term, Novo_estado).

%------------------------------------------------------------------------------%

% 2.16 
% Predicado junta_pontes/5
% Permite obter o estado resultante da adicao de pontes entre duas ilhas

%------------------------------------------------------------------------------%

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


junta_pontes(Estado, Num_Pontes, ilha(N1,POS1), ilha(N2,POS2), Novo_Estado) :-
    cria_ponte(POS1, POS2, Ponte),
    adiciona_multiplas(Ponte, Num_Pontes, Pontes),
    adiciona_pontes(ilha(N1,POS1), ilha(N2,POS2), Pontes, Estado, Estado_Pontes),
    actualiza_vizinhas_apos_pontes(Estado_Pontes, POS1, POS2, Estado_Atualizado),
    trata_ilhas_terminadas(Estado_Atualizado, Novo_Estado).


%//////////////////////////        END OF FILE       ////////////////////////////%

