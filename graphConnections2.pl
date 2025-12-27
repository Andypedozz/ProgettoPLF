% ##############################################################
% # Corso di Programmazione Logica e Funzionale                #
% # Progetto per la sessione autunnale A.A. 2024/2025          #
% # Versione Prolog (GProlog)                                  #
% # di Andrea Pedini                                          #
% ##############################################################

% -------------------------
% DEFINIZIONE DEL GRAFO
% -------------------------

vertice(0).
vertice(1).
vertice(2).
vertice(3).
vertice(4).
vertice(5).
vertice(6).
vertice(7).

arco(0,1).
arco(1,2).
arco(2,0).
arco(2,3).
arco(3,4).
arco(4,5).
arco(4,6).
arco(5,6).
arco(6,7).

% -------------------------
% UTILITY LISTE
% -------------------------

membro(X, [X|_]).
membro(X, [_|T]) :- membro(X, T).

aggiungi_unico(X, L, L) :- membro(X, L), !.
aggiungi_unico(X, L, [X|L]).

unione([], L, L).
unione([H|T], L, R) :-
    aggiungi_unico(H, L, L1),
    unione(T, L1, R).

% -------------------------
% ADIACENZA
% -------------------------

adiacente(X, Y) :- arco(X, Y).

adiacenti(X, Lista) :-
    findall(Y, adiacente(X, Y), Lista).

% -------------------------
% DFS CON ORDINE DI FINE
% -------------------------

dfs([], Visitati, Visitati).
dfs([V|Vs], Visitati, Ris) :-
    membro(V, Visitati), !,
    dfs(Vs, Visitati, Ris).
dfs([V|Vs], Visitati, Ris) :-
    adiacenti(V, Vicini),
    dfs(Vicini, [V|Visitati], Visitati1),
    dfs(Vs, Visitati1, Ris).

ordine_completamento(Ordine) :-
    findall(V, vertice(V), Vertici),
    dfs(Vertici, [], Ordine).

% -------------------------
% GRAFO INVERTITO
% -------------------------

arco_invertito(X, Y) :- arco(Y, X).

adiacenti_inv(X, Lista) :-
    findall(Y, arco_invertito(X, Y), Lista).

% -------------------------
% DFS PER UNA SCC
% -------------------------

dfs_scc([], _, Visitati, Visitati).
dfs_scc([V|Vs], VisitatiGlobali, VisitatiLocali, Ris) :-
    membro(V, VisitatiLocali), !,
    dfs_scc(Vs, VisitatiGlobali, VisitatiLocali, Ris).
dfs_scc([V|Vs], VisitatiGlobali, VisitatiLocali, Ris) :-
    membro(V, VisitatiGlobali), !,
    dfs_scc(Vs, VisitatiGlobali, VisitatiLocali, Ris).
dfs_scc([V|Vs], VisitatiGlobali, VisitatiLocali, Ris) :-
    adiacenti_inv(V, Vicini),
    dfs_scc(Vicini, VisitatiGlobali, [V|VisitatiLocali], Visitati1),
    dfs_scc(Vs, VisitatiGlobali, Visitati1, Ris).

% -------------------------
% CALCOLO SCC (KOSARAJU)
% -------------------------

calcola_scc([], _, []).
calcola_scc([V|Vs], Visitati, Sccs) :-
    membro(V, Visitati), !,
    calcola_scc(Vs, Visitati, Sccs).
calcola_scc([V|Vs], Visitati, [Scc|Rest]) :-
    dfs_scc([V], Visitati, [], Scc),
    unione(Scc, Visitati, Visitati1),
    calcola_scc(Vs, Visitati1, Rest).

kosaraju(Sccs) :-
    ordine_completamento(Ord),
    reverse(Ord, OrdRev),
    calcola_scc(OrdRev, [], Sccs).

% -------------------------
% INDICE DELLA SCC
% -------------------------

indice_scc(V, [Scc|_], 0) :- membro(V, Scc), !.
indice_scc(V, [_|Rest], I) :-
    indice_scc(V, Rest, I1),
    I is I1 + 1.

% -------------------------
% GRAFO COMPRESSO
% -------------------------

arco_compresso(Sccs, I, J) :-
    arco(X, Y),
    indice_scc(X, Sccs, I),
    indice_scc(Y, Sccs, J),
    I \= J.

archi_compressi(Sccs, Archi) :-
    findall((I,J), arco_compresso(Sccs, I, J), A),
    sort(A, Archi).

% -------------------------
% INDEGREE
% -------------------------

indegree(_, [], 0).
indegree(N, [(_,N)|T], R) :-
    indegree(N, T, R1),
    R is R1 + 1.
indegree(N, [_|T], R) :-
    indegree(N, T, R).

% -------------------------
% CONTEGGIO SCC CON INDEGREE 0
% -------------------------

conta_scc_zero(Start, Risultato) :-
    kosaraju(Sccs),
    archi_compressi(Sccs, Archi),
    indice_scc(Start, Sccs, SStart),
    length(Sccs, N),
    conta(0, N, SStart, Archi, Risultato).

conta(I, N, _, _, 0) :- I >= N, !.
conta(I, N, SStart, Archi, R) :-
    I < N,
    indegree(I, Archi, D),
    conta(I+1, N, SStart, Archi, R1),
    ( I \= SStart, D =:= 0 -> R is R1 + 1 ; R = R1 ).

% -------------------------
% UTILITY PER STAMPARE SCC
% -------------------------

stampa_scc(Sccs) :- stampa_scc(Sccs, 0).

stampa_scc([], _).
stampa_scc([Scc|Rest], I) :-
    write(I), write(' -> '), write(Scc), nl,
    I1 is I + 1,
    stampa_scc(Rest, I1).

% -------------------------
% ESEMPIO DI ESECUZIONE
% -------------------------

main :-
    Start = 7,  % nodo di partenza da considerare
    write('Original Graph:'), nl,

    % --- stampa vertici ---
    findall(V, vertice(V), Vertici),
    write('Vertici: '), write(Vertici), nl,

    % --- stampa archi ---
    findall((X,Y), arco(X,Y), Archi),
    write('Archi: '), write(Archi), nl, nl,

    % --- calcolo SCC ---
    kosaraju(Sccs),
    write('Strongly Connected Components:'), nl,
    stampa_scc(Sccs), nl,

    % --- archi compressi ---
    archi_compressi(Sccs, ArchiComp),
    write('Compressed Graph Edges: '), write(ArchiComp), nl, nl,

    % --- stampa nodo di partenza ---
    write('Start Node: '), write(Start), nl,

    % --- conteggio SCC con indegree 0 (escludendo start) ---
    conta_scc_zero(Start, Count),
    write('Number of SCCs with indegree 0 (excluding start): '), write(Count), nl.