% ##############################################################
% # Corso di Programmazione Logica e Funzionale                #
% # Progetto per la sessione autunnale A.A. 2024/2025          #
% # Versione Prolog (GProlog)                                  #
% # di Andrea Pedini                                           #
% ##############################################################

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA GRAFO DA FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leggi_termine(Stream, Termine) :-
    read_line(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    read_from_atom(LineConPunto, Termine).

leggi_grafo_da_file(File, grafo(Nodi,Archi)) :-
    open(File, read, Stream),
    leggi_termine(Stream, Nodi),
    leggi_termine(Stream, Archi),
    close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATI DI BASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

membro(X,[X|_]).
membro(X,[_|T]) :- membro(X,T).

nodi(grafo(N,_), N).
archi(grafo(_,A), A).

adiacente(grafo(_,A), X, Y) :-
    membro((X,Y), A).

adiacenti(G, X, L) :-
    findall(Y, adiacente(G,X,Y), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DFS CON ORDINE DI COMPLETAMENTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dfs_visit(_, N, V, V, []) :-
    membro(N,V), !.

dfs_visit(G, N, V, V2, Ordine) :-
    \+ membro(N,V),
    adiacenti(G, N, Vicini),
    dfs_lista(G, Vicini, [N|V], V1, OrdVicini),
    append(OrdVicini, [N], Ordine),
    V2 = V1.

dfs_lista(_, [], V, V, []).
dfs_lista(G, [H|T], V, V2, Ordine) :-
    dfs_visit(G, H, V, V1, O1),
    dfs_lista(G, T, V1, V2, O2),
    append(O1, O2, Ordine).

dfs_grafo(G, Ordine) :-
    nodi(G, Nodi),
    dfs_lista(G, Nodi, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRASPOSTO DEL GRAFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trasposto(grafo(N,A), grafo(N,AT)) :-
    trasponi_archi(A,AT).

trasponi_archi([], []).
trasponi_archi([(X,Y)|T], [(Y,X)|R]) :-
    trasponi_archi(T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DFS PER UNA SCC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dfs_scc(_, N, V, V, []) :-
    membro(N,V), !.

dfs_scc(G, N, V, V2, [N|Comp]) :-
    \+ membro(N,V),
    adiacenti(G, N, Vicini),
    dfs_scc_lista(G, Vicini, [N|V], V2, Comp).

dfs_scc_lista(_, [], V, V, []).
dfs_scc_lista(G, [H|T], V, V2, Comp) :-
    dfs_scc(G, H, V, V1, C1),
    dfs_scc_lista(G, T, V1, V2, C2),
    append(C1, C2, Comp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALGORITMO DI KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kosaraju(G, SCCs) :-
    dfs_grafo(G, Ordine),
    reverse(Ordine, OrdInv),
    trasposto(G, GT),
    kosaraju_visita(GT, OrdInv, [], SCCs).

kosaraju_visita(_, [], _, []).
kosaraju_visita(G, [N|T], V, SCCs) :-
    membro(N,V), !,
    kosaraju_visita(G, T, V, SCCs).

kosaraju_visita(G, [N|T], V, [SCC|R]) :-
    dfs_scc(G, N, V, V1, SCC),
    kosaraju_visita(G, T, V1, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO DELLE SCC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scc_di_nodo(N, [S|_], S) :-
    membro(N,S), !.
scc_di_nodo(N, [_|T], S) :-
    scc_di_nodo(N,T,S).

arco_scc(G, SCCs, S1, S2) :-
    archi(G,A),
    membro((X,Y),A),
    scc_di_nodo(X,SCCs,S1),
    scc_di_nodo(Y,SCCs,S2),
    S1 \= S2.

grado_entrante(G, SCCs, S, Grado) :-
    findall(1, arco_scc(G,SCCs,_,S), L),
    length(L, Grado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA RISULTATI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stampa_scc([]).
stampa_scc([S|T]) :-
    write('SCC: '), write(S), nl,
    stampa_scc(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
    leggi_grafo_da_file('input.txt', G),
    nodi(G, Nodi),
    kosaraju(G, SCCs),

    nl,
    write('Componenti fortemente connesse:'), nl,
    stampa_scc(SCCs),

    write('Inserisci nodo di partenza: '),
    read(Nodo),
    scc_di_nodo(Nodo, SCCs, SCCpartenza),

    findall(
        S,
        ( membro(S,SCCs),
          S \= SCCpartenza,
          grado_entrante(G,SCCs,S,0)
        ),
        ZeroIn
    ),
    length(ZeroIn, Conteggio),

    nl,
    write('Numero di SCC con grado entrante 0 (esclusa quella di partenza): '),
    write(Conteggio), nl.
