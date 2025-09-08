% Esegui (SWI-Prolog):  swipl -q -s main.pl -g main -t halt -- input.pldata

:- initialization(main, main).

% ======= Lettura file =======
read_graph(File, Vs, Es) :-
    open(File, read, S),
    read(S, Vs),       % prima riga: lista di vertici [..].
    read(S, Es),       % seconda riga: lista di archi [(U,V),..].
    close(S).

print_list(L) :- write(L), nl.

% ======= Adiacenza e trasposto =======
adj(V, Es, Ns) :- findall(X, member((V,X), Es), Ns).

transpose_edges(Es, Ts) :- findall((B,A), member((A,B), Es), Ts).

% ======= DFS: ordine di fine (post-ordine) =======
dfs_post_all([], _, Vis, Order, Vis, Order).
dfs_post_all([V|Vs], Es, Vis0, Order0, VisF, OrderF) :-
    (   ord_memberchk(V, Vis0) ->
        dfs_post_all(Vs, Es, Vis0, Order0, VisF, OrderF)
    ;   dfs_post(V, Es, Vis0, Vis1, Order1),
        append(Order1, Order0, Order2),
        dfs_post_all(Vs, Es, Vis1, Order2, VisF, OrderF)
    ).

dfs_post(V, Es, Vis0, VisF, OrderF) :-
    ord_add_element(Vis0, V, Vis1),
    adj(V, Es, Ns),
    dfs_post_list(Ns, Es, Vis1, Vis2, Order0),
    append(Order0, [V], OrderF),
    VisF = Vis2.

dfs_post_list([], _, Vis, Vis, []).
dfs_post_list([U|Us], Es, Vis0, VisF, OrderF) :-
    (   ord_memberchk(U, Vis0) ->
        dfs_post_list(Us, Es, Vis0, VisF, OrderF)
    ;   dfs_post(U, Es, Vis0, Vis1, OrderU),
        dfs_post_list(Us, Es, Vis1, VisF, OrderUs),
        append(OrderU, OrderUs, OrderF)
    ).

% ======= DFS su trasposto per raccogliere SCC =======
dfs_collect(V, EsT, Vis0, VisF, CompF) :-
    ord_add_element(Vis0, V, Vis1),
    adj(V, EsT, Ns),
    dfs_collect_list(Ns, EsT, Vis1, VisF1, CompNs),
    CompF = [V|CompNs],
    VisF = VisF1.

dfs_collect_list([], _, Vis, Vis, []).
dfs_collect_list([U|Us], EsT, Vis0, VisF, CompF) :-
    (   ord_memberchk(U, Vis0) ->
        dfs_collect_list(Us, EsT, Vis0, VisF, CompF)
    ;   dfs_collect(U, EsT, Vis0, Vis1, CompU),
        dfs_collect_list(Us, EsT, Vis1, VisF, CompUs),
        append(CompU, CompUs, CompF)
    ).

kosaraju(Vs, Es, SCCs) :-
    sort(Vs, VsOrd),
    empty_ord_set(Vis0),
    dfs_post_all(VsOrd, Es, Vis0, [], _, Order),
    transpose_edges(Es, EsT),
    kosaraju_collect(Order, EsT, [], SCCs).

kosaraju_collect([], _, _, []).
kosaraju_collect([V|Vs], EsT, Vis0, SCCs) :-
    (   ord_memberchk(V, Vis0) ->
        kosaraju_collect(Vs, EsT, Vis0, SCCs)
    ;   dfs_collect(V, EsT, Vis0, Vis1, Comp),
        kosaraju_collect(Vs, EsT, Vis1, Rest),
        SCCs = [Comp|Rest]
    ).

% ======= Condensazione e conteggi =======
rep_of_component([R|_], R).

build_rep_map([], _, []).
build_rep_map([Comp|Cs], RepPairsAcc, RepPairs) :-
    rep_of_component(Comp, R),
    findall((V,R), member(V, Comp), Pairs),
    append(RepPairsAcc, Pairs, RepPairsMid),
    build_rep_map(Cs, RepPairsMid, RepPairs).
rep_map(SCCs, RepPairs) :- build_rep_map(SCCs, [], RepPairs).

condensation_edges(Es, RepPairs, CEset) :-
    findall((RU,RV),
            ( member((U,V), Es),
              member((U,RU), RepPairs),
              member((V,RV), RepPairs),
              RU \= RV ),
            Raw),
    sort(Raw, CEset).

indegree_zero_count_except(Start, Vs, Es, SCCs, K) :-
    rep_map(SCCs, RepPairs),
    member((Start,RStart), RepPairs),        % errore se Start non presente
    findall(R, (member(Comp,SCCs), rep_of_component(Comp,R)), Reps0),
    sort(Reps0, Reps),
    condensation_edges(Es, RepPairs, CEs),
    % indegree per ciascun R:
    findall(R, member(R, Reps), AllZero0),
    exclude({CEs}/[R]>>member((_,R), CEs), AllZero0, ZeroReps0),
    % togli la componente di partenza
    exclude(=(RStart), ZeroReps0, ZeroReps),
    length(ZeroReps, K).

% ======= Utilità insiemi ordinati =======
empty_ord_set([]).
ord_memberchk(E, [X|_]) :- E == X, !.
ord_memberchk(E, [X|Xs]) :- E @> X, !, ord_memberchk(E, Xs).
ord_memberchk(_, _) :- fail.

ord_add_element([], E, [E]).
ord_add_element([X|Xs], E, [E,X|Xs]) :- E @< X, !.
ord_add_element([X|Xs], E, [X|Ys]) :- E @> X, !, ord_add_element(Xs, E, Ys).
ord_add_element([X|Xs], E, [X|Xs]) :- E == X, !.

% ======= Main =======
main(Argv) :-
    (   Argv = [File|_] -> true
    ;   writeln('Uso: swipl -q -s main.pl -g main -t halt -- <fileInput>'), halt(1)
    ),
    read_graph(File, Vs, Es),
    writeln('Vertici:'), print_list(Vs),
    writeln('Archi (orientati):'), print_list(Es),
    (   \+ valid_graph(Vs, Es) ->
        writeln('Errore: grafo non valido (duplicati o vertici mancanti in archi).'), halt(1)
    ;   true ),
    write('Inserisci il vertice di partenza (numero, seguito da punto): '), flush_output,
    read(Start),
    (   \+ member(Start, Vs) ->
        writeln('Errore: vertice di partenza non presente.'), halt(1)
    ;   true ),
    kosaraju(Vs, Es, SCCs),
    write('SCC trovate: '), writeln(SCCs),
    indegree_zero_count_except(Start, Vs, Es, SCCs, K),
    format('Numero di componenti (diverse da quella di partenza) con grado entrante 0: ~w~n', [K]).

% Validazione (analoga a grafoDir/1 di Haskell)
valid_graph(Vs, Es) :-
    sort(Vs, VsS), length(VsS, N1), length(Vs, N1),          % no duplicati nei vertici
    forall(member((U,V), Es), (member(U, Vs), member(V, Vs))),
    sort(Es, EsS), length(EsS, N2), length(Es, N2).          % no duplicati negli archi
