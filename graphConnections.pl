% ##################################################################
% #         Corso di Programmazione Logica e Funzionale            #
% #        Progetto per la sessione autunnale A.A 2024/2025        #
% #                        Andrea Pedini & Matteo Fraternali      #
% ##################################################################

:- initialization(main).

% ============================================================
% DFS e funzioni ausiliarie
% ============================================================

% Vertici adiacenti
adiac(V, [], []).
adiac(V, [(V,U)|As], [U|Adj]) :-
    adiac(V, As, Adj).
adiac(V, [(X,Y)|As], Adj) :-
    V \= X,
    adiac(V, As, Adj).

% DFS per ordine di completamento
dfs([], _, Visited, Visited).
dfs([U|Us], Edges, Visited, Stack) :-
    member(U, Visited), !,
    dfs(Us, Edges, Visited, Stack).
dfs([U|Us], Edges, Visited, Stack) :-
    \+ member(U, Visited),
    adiac(U, Edges, Vicini),
    dfs(Vicini, Edges, [U|Visited], NewVisited),
    dfs(Us, Edges, NewVisited, Stack).

% Inverti archi
inverti_archi([], []).
inverti_archi([(U,V)|Es], [(V,U)|EsInv]) :-
    inverti_archi(Es, EsInv).

% DFS per raccogliere una SCC
dfs_scc([], _, _, VisitedLocal, VisitedLocal).
dfs_scc([U|Us], Edges, VisitedGlobal, VisitedLocal, Result) :-
    ( member(U, VisitedLocal) ->
        dfs_scc(Us, Edges, VisitedGlobal, VisitedLocal, Result)
    ; member(U, VisitedGlobal) ->
        dfs_scc(Us, Edges, VisitedGlobal, VisitedLocal, Result)
    ; adiac(U, Edges, Vicini),
      append(Vicini, Us, NewStack),
      append(VisitedLocal, [U], NewVisitedLocal),
      dfs_scc(NewStack, Edges, VisitedGlobal, NewVisitedLocal, Result)
    ).

% Calcolo tutte le SCC
get_sccs([], _, _, []).
get_sccs([U|Us], Edges, VisitedGlobal, [SCC|SCCs]) :-
    member(U, VisitedGlobal), !,
    get_sccs(Us, Edges, VisitedGlobal, SCCs).
get_sccs([U|Us], Edges, VisitedGlobal, [SCC|SCCs]) :-
    dfs_scc([U], Edges, VisitedGlobal, [], SCC),
    append(VisitedGlobal, SCC, NewVisitedGlobal),
    get_sccs(Us, Edges, NewVisitedGlobal, SCCs).

% Trova indice della SCC contenente un vertice
find_scc_index(V, SCCs, Index) :-
    nth0(Index, SCCs, Comp),
    member(V, Comp), !.

% Costruzione grafo compresso
compress_graph(SCCs, Edges, Compressed) :-
    findall((I,J),
        ( member((U,V), Edges),
          find_scc_index(U, SCCs, I),
          find_scc_index(V, SCCs, J),
          I \= J
        ),
        Pairs),
    sort(Pairs, Compressed).

% Calcola indegree di una componente
indegree(Comp, Edges, Count) :-
    include([(_,J)]>>(J=Comp), Edges, L),
    length(L, Count).

% Conta SCC con indegree 0 diverse da quella di partenza
count_indegree_zero(Start, SCCs, Compressed, Count) :-
    find_scc_index(Start, SCCs, StartIndex),
    findall(I,
        (nth0(I, SCCs, _),
         I \= StartIndex,
         indegree(I, Compressed, 0)),
        List),
    length(List, Count).

% Algoritmo di Kosaraju
kosaraju(Vs, Edges, SCCs) :-
    dfs(Vs, Edges, [], Ordine),
    inverti_archi(Edges, RevEdges),
    reverse(Ordine, RevOrdine),
    get_sccs(RevOrdine, RevEdges, [], SCCs).

% ============================================================
% Main con dati di esempio
% ============================================================

main :-
    % Dati di esempio
    Vs = [1,2,3,4,5,6],
    Edges = [(1,2),(2,3),(3,1),(4,5),(5,6),(6,4),(3,4)],
    writeln('Grafo iniziale:'),
    writeln(Edges),
    kosaraju(Vs, Edges, SCCs),
    writeln('Componenti fortemente connesse:'),
    writeln(SCCs),
    compress_graph(SCCs, Edges, Compressed),
    writeln('Grafo delle componenti fortemente connesse:'),
    writeln(Compressed),
    % Vertice di partenza di esempio
    Start = 1,
    format('Vertice di partenza: ~w~n', [Start]),
    count_indegree_zero(Start, SCCs, Compressed, Count),
    format('Numero di SCC con indegree = 0 diverse da quella contenente ~w: ~w~n', [Start, Count]),
    halt.
