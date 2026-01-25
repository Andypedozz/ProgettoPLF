/* ##############################################################
# Corso di Programmazione Logica e Funzionale                  #
# Progetto per la sessione autunnale A.A. 2024/2025            #
# Versione GNU Prolog                                         #
############################################################## */

/*
Specifica : Scrivere un programma Prolog che legga un grafo orientato
da file e calcoli le sue Componenti Fortemente Connesse (SCC)
utilizzando l’algoritmo di Kosaraju.
Successivamente il programma costruisce il grafo compresso
e determina il numero di SCC con grado entrante zero,
escludendo la SCC contenente un nodo scelto dall’utente.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato principale del programma .
- Legge il grafo da file
- Calcola le componenti fortemente connesse
- Stampa i risultati ottenuti */
main :-
    leggi_grafo_da_file('input.txt', G),
    nodi(G, Nodi),
    archi(G, Archi),
    kosaraju(G, SCCs),

    stampa_separatore,
    write('            GRAFO LETTO DA FILE       '), nl,
    stampa_riga,
    write('Vertici: '), write(Nodi), nl,
    write('Archi:   '), write(Archi), nl,

    nl,
    stampa_separatore,
    write('       COMPONENTI FORTEMENTE CONNESSE '), nl,
    stampa_riga,
    stampa_scc_numerate(SCCs, 0),

    nl,
    stampa_separatore,
    write('           GRAFO COMPRESSO'), nl,
    stampa_riga,
    write('Inserisci il vertice di partenza (tra '),
    write(Nodi), write('):'), nl,

    leggi_numero(Nodo),

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
    stampa_separatore,
    write('Numero di SCC con indegree 0 (esclusa partenza): '),
    write(Conteggio), nl,
    stampa_separatore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che stampa una linea separatrice */
stampa_separatore :-
    write('======================================'), nl.

/* Predicato che stampa una riga separatrice */
stampa_riga :-
    write('--------------------------------------'), nl.

/* Predicato che stampa le SCC numerate .
- Il primo parametro la lista delle SCC
- Il secondo parametro l’indice corrente */
stampa_scc_numerate([], _).
stampa_scc_numerate([S|T], N) :-
    write('SCC '), write(N), write(': '),
    write(S), nl,
    N1 is N + 1,
    stampa_scc_numerate(T, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA GRAFO DA FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che legge una linea da uno stream .
- Il primo parametro lo stream di input
- Il secondo parametro la linea letta */
leggi_linea(Stream, Line) :-
    get_char(Stream, Char),
    leggi_linea_aux(Stream, Char, Chars),
    atom_chars(Line, Chars).

/* Predicato ausiliario per la lettura delle linee .
- Il primo parametro lo stream
- Il secondo parametro il carattere corrente
- Il terzo parametro la lista dei caratteri letti */
leggi_linea_aux(_, end_of_file, []) :- !.
leggi_linea_aux(_, '\n', []) :- !.
leggi_linea_aux(Stream, Char, [Char|Chars]) :-
    get_char(Stream, NextChar),
    leggi_linea_aux(Stream, NextChar, Chars).

/* Predicato che legge un termine Prolog da file .
- Il primo parametro lo stream
- Il secondo parametro il termine letto */
leggi_termine(Stream, Termine) :-
    leggi_linea(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    read_from_atom(LineConPunto, Termine).

/* Predicato che legge il grafo da file .
- Il primo parametro il nome del file
- Il secondo parametro il grafo letto */
leggi_grafo_da_file(File, grafo(Nodi, Archi)) :-
    open(File, read, Stream),
    leggi_termine(Stream, Nodi),
    leggi_termine(Stream, Archi),
    close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INPUT NUMERICO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che legge un numero da input standard .
- Il parametro il numero letto */
leggi_numero(N) :-
    leggi_linea(user_input, Line),
    atom_codes(Line, Codes),
    number_codes(N, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATI BASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che verifica l’appartenenza di un elemento a una lista .
- Il primo parametro l’elemento
- Il secondo parametro la lista */
membro(X,[X|_]).
membro(X,[_|T]) :- membro(X,T).

/* Predicato che restituisce i nodi del grafo .
- Il primo parametro il grafo
- Il secondo parametro la lista dei nodi */
nodi(grafo(N,_), N).

/* Predicato che restituisce gli archi del grafo .
- Il primo parametro il grafo
- Il secondo parametro la lista degli archi */
archi(grafo(_,A), A).

/* Predicato che verifica l’adiacenza tra due nodi .
- Il primo parametro il grafo
- Il secondo parametro il nodo sorgente
- Il terzo parametro il nodo destinazione */
adiacente(grafo(_,A), X, Y) :- membro((X,Y), A).

/* Predicato che restituisce i nodi adiacenti a un nodo .
- Il primo parametro il grafo
- Il secondo parametro il nodo
- Il terzo parametro la lista dei nodi adiacenti */
adiacenti(G, X, L) :- findall(Y, adiacente(G,X,Y), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRATEGIE DI COMBINAZIONE PER LA VISITA IN PROFONDITA'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che inserisce un nodo in coda alla lista .
Usato per l’ordine di completamento */
combina_fine(N, Lista, Risultato) :-
    append(Lista, [N], Risultato).

/* Predicato che inserisce un nodo in testa alla lista .
Usato per la costruzione delle SCC */
combina_testa(N, Lista, [N|Lista]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITA' GENERICA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato di visita in profondità generico .
- Il primo parametro la strategia di combinazione
- Il secondo parametro il grafo
- Il terzo parametro il nodo corrente
- Il quarto parametro i nodi visitati
- Il quinto parametro i nodi visitati aggiornati
- Il sesto parametro il risultato della visita */
visitaInProfondita(_, _, N, V, V, []) :-
    membro(N, V), !.

visitaInProfondita(Combina, G, N, V, V2, Risultato) :-
    \+ membro(N, V),
    adiacenti(G, N, Vicini),
    visitaInProfondita_lista(Combina, G, Vicini, [N|V], V1, RisFigli),
    call(Combina, N, RisFigli, Risultato),
    V2 = V1.

/* Predicato ausiliario per visitare una lista di nodi .
- Parametri analoghi alla visita in profondità principale */
visitaInProfondita_lista(_, _, [], V, V, []).
visitaInProfondita_lista(Combina, G, [H|T], V, V2, Risultato) :-
    visitaInProfondita(Combina, G, H, V, V1, R1),
    visitaInProfondita_lista(Combina, G, T, V1, V2, R2),
    append(R1, R2, Risultato).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITA' SPECIALIZZATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Visita in profondità per il calcolo dell’ordine di completamento */
visitaInProfondita_ordine(G, N, V, V2, Ordine) :-
    visitaInProfondita(combina_fine, G, N, V, V2, Ordine).

/* Visita in profondità per la costruzione di una componente fortemente connessa */
visitaInProfondita_scc(G, N, V, V2, Comp) :-
    visitaInProfondita(combina_testa, G, N, V, V2, Comp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITA' ORDINE DI COMPLETAMENTO (GLOBALE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che calcola l’ordine di completamento globale del grafo */
visitaInProfondita_grafo(G, Ordine) :-
    nodi(G, Nodi),
    visitaInProfondita_lista(combina_fine, G, Nodi, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO TRASPOSTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che costruisce il grafo trasposto */
trasposto(grafo(N,A), grafo(N,AT)) :-
    trasponi_archi(A, AT).

/* Predicato che inverte tutti gli archi del grafo */
trasponi_archi([], []).
trasponi_archi([(X,Y)|T], [(Y,X)|R]) :-
    trasponi_archi(T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che implementa l’algoritmo di Kosaraju */
kosaraju(G, SCCs) :-
    visitaInProfondita_grafo(G, Ordine),
    reverse(Ordine, OrdInv),
    trasposto(G, GT),
    kosaraju_visita(GT, OrdInv, [], SCCs).

/* Predicato ausiliario di visita per Kosaraju */
kosaraju_visita(_, [], _, []).
kosaraju_visita(G, [N|T], V, SCCs) :-
    membro(N, V), !,
    kosaraju_visita(G, T, V, SCCs).
kosaraju_visita(G, [N|T], V, [SCC|R]) :-
    visitaInProfondita_scc(G, N, V, V1, SCC),
    kosaraju_visita(G, T, V1, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO DELLE SCC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che individua la SCC di un nodo */
scc_di_nodo(N, [S|_], S) :- membro(N,S), !.
scc_di_nodo(N, [_|T], S) :- scc_di_nodo(N,T,S).

/* Predicato che verifica l’esistenza di un arco tra SCC */
arco_scc(G, SCCs, S1, S2) :-
    archi(G,A),
    membro((X,Y),A),
    scc_di_nodo(X,SCCs,S1),
    scc_di_nodo(Y,SCCs,S2),
    S1 \= S2.

/* Predicato che calcola il grado entrante di una SCC */
grado_entrante(G, SCCs, S, Grado) :-
    findall(1, arco_scc(G,SCCs,_,S), L),
    length(L, Grado).
