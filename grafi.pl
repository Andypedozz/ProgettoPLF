% ##################################################################
% #         Corso di Programmazione Logica e Funzionale            #
% #        Progetto per la sessione invernale A.A 2025/2026        #
% #                        di Andrea Pedini                        #
% #                       Matricola: 322918                        #
% #                       e Matteo Fraternali                      #
% #                       Matricola: 316637                        #
% ##################################################################

/*
    Specifica:
    Scrivere un programma Prolog che legga un grafo orientato da file
    e calcoli le Componenti Fortemente Connesse (SCC) utilizzando
    l’algoritmo di Kosaraju.
    Successivamente costruisce il grafo compresso e determina
    il numero di SCC con grado entrante zero, escludendo la SCC
    contenente un nodo scelto dall’utente.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Predicato principale del programma.
    - legge il grafo da file in modo sicuro
    - gestisce eventuali errori
    - avvia l’esecuzione principale
*/
main :-
    leggi_grafo_sicuro('input.txt', Risultato),
    gestisci_risultato(Risultato).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GESTIONE RISULTATO LETTURA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    - Risultato = errore → stampa messaggio e termina
    - Risultato = ok(Grafo) → esegue il programma
*/
gestisci_risultato(errore) :-
    nl,
    stampa_separatore,
    write('Errore nel file di input.'), nl,
    write('Controllare formato e contenuto del grafo.'), nl,
    stampa_separatore,
    nl,
    !, fail.

gestisci_risultato(ok(G)) :-
    esegui_programma(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ESECUZIONE PRINCIPALE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    esegui_programma/1
    - G: grafo valido
    Esegue:
    - stampa del grafo
    - calcolo SCC
    - acquisizione nodo di partenza
    - conteggio SCC con grado entrante zero
*/
esegui_programma(G) :-
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

    leggi_vertice_valido(Nodi, Nodo),

    scc_di_nodo(Nodo, SCCs, SCCpartenza),

    findall(
        S,
        ( membro(S,SCCs),
          S \= SCCpartenza,
          grado_entrante(G, SCCs, S, 0)
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
%% LETTURA E VALIDAZIONE GRAFO DA FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    leggi_grafo_sicuro/2
    - File: nome del file
    - Risultato:
        ok(grafo(Nodi,Archi)) se il parsing è valido
        errore               altrimenti
*/
leggi_grafo_sicuro(File, ok(grafo(Nodi, Archi))) :-
    open(File, read, Stream),

    leggi_termine(Stream, Nodi),
    is_list(Nodi),
    Nodi \= [],

    leggi_termine(Stream, Archi),
    is_list(Archi),

    close(Stream),

    archi_validi(Archi, Nodi), !.

leggi_grafo_sicuro(_, errore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALIDAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    archi_validi/2
    - Archi: lista di archi (X,Y)
    - Nodi: lista dei nodi del grafo
    Verifica che ogni arco usi solo nodi esistenti
*/
archi_validi([], _).
archi_validi([(X,Y)|T], Nodi) :-
    membro(X, Nodi),
    membro(Y, Nodi),
    archi_validi(T, Nodi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA LINEE E TERMINI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    leggi_linea/2
    - Stream: stream di input
    - Line: linea letta come atomo
*/
leggi_linea(Stream, Line) :-
    get_char(Stream, Char),
    leggi_linea_aux(Stream, Char, Chars),
    atom_chars(Line, Chars).

/*
    leggi_linea_aux/3
    - Stream: stream di input
    - Char: carattere corrente
    - Chars: lista dei caratteri letti
*/
leggi_linea_aux(_, end_of_file, []) :- !.
leggi_linea_aux(_, '\n', []) :- !.
leggi_linea_aux(Stream, Char, [Char|Chars]) :-
    get_char(Stream, NextChar),
    leggi_linea_aux(Stream, NextChar, Chars).

/*
    leggi_termine/2
    - Stream: stream di input
    - Termine: termine Prolog letto
*/
leggi_termine(Stream, Termine) :-
    leggi_linea(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    read_from_atom(LineConPunto, Termine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INPUT NUMERICO E VALIDAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    leggi_vertice_valido/2
    - Nodi: lista dei nodi del grafo
    - Nodo: nodo valido scelto dall’utente
*/
leggi_vertice_valido(Nodi, Nodo) :-
    write('Inserisci il vertice di partenza (tra '),
    write(Nodi), write('):'), nl,
    leggi_numero(N),
    (   membro(N, Nodi)
    ->  Nodo = N
    ;   write('Vertice non valido! Riprova.'), nl,
        leggi_vertice_valido(Nodi, Nodo)
    ).

/*
    leggi_numero/1
    - N: numero letto da input standard
*/
leggi_numero(N) :-
    leggi_linea(user_input, Line),
    atom_codes(Line, Codes),
    number_codes(N, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    stampa_separatore/0
    Stampa una linea di separazione
*/
stampa_separatore :-
    write('======================================'), nl.

/*
    stampa_riga/0
    Stampa una riga separatrice
*/
stampa_riga :-
    write('--------------------------------------'), nl.

/*
    stampa_scc_numerate/2
    - ListaSCC: lista delle componenti
    - N: indice corrente
*/
stampa_scc_numerate([], _).
stampa_scc_numerate([S|T], N) :-
    write('SCC '), write(N), write(': '),
    write(S), nl,
    N1 is N + 1,
    stampa_scc_numerate(T, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATI BASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    membro/2
    Verifica appartenenza di un elemento a una lista
*/
membro(X,[X|_]).
membro(X,[_|T]) :- membro(X,T).

/*
    nodi/2
    - Grafo
    - Lista dei nodi
*/
nodi(grafo(N,_), N).

/*
    archi/2
    - Grafo
    - Lista degli archi
*/
archi(grafo(_,A), A).

/*
    adiacente/3
    - Grafo
    - Nodo sorgente
    - Nodo destinazione
*/
adiacente(grafo(_,A), X, Y) :-
    membro((X,Y), A).

/*
    adiacenti/3
    - Grafo
    - Nodo
    - Lista dei nodi adiacenti
*/
adiacenti(G, X, L) :-
    findall(Y, adiacente(G,X,Y), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COMBINAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    combina_fine/3
    Inserisce un nodo in coda alla lista
*/
combina_fine(N, Lista, Risultato) :-
    append(Lista, [N], Risultato).

/*
    combina_testa/3
    Inserisce un nodo in testa alla lista
*/
combina_testa(N, Lista, [N|Lista]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITA'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    visitaInProfondita/6
    - Combina: strategia di combinazione
    - G: grafo
    - N: nodo corrente
    - V: nodi visitati
    - V2: nodi visitati aggiornati
    - Risultato: risultato accumulato
*/
visitaInProfondita(_, _, N, V, V, []) :-
    membro(N, V), !.

visitaInProfondita(Combina, G, N, V, V2, Risultato) :-
    \+ membro(N, V),
    adiacenti(G, N, Vicini),
    visitaInProfondita_lista(Combina, G, Vicini, [N|V], V1, RisFigli),
    call(Combina, N, RisFigli, Risultato),
    V2 = V1.

/*
    visitaInProfondita_lista/6
    Visita una lista di nodi
*/
visitaInProfondita_lista(_, _, [], V, V, []).
visitaInProfondita_lista(Combina, G, [H|T], V, V2, Risultato) :-
    visitaInProfondita(Combina, G, H, V, V1, R1),
    visitaInProfondita_lista(Combina, G, T, V1, V2, R2),
    append(R1, R2, Risultato).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITE SPECIALIZZATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    visitaInProfondita_ordine/5
    Calcola l’ordine di completamento
*/
visitaInProfondita_ordine(G, N, V, V2, Ordine) :-
    visitaInProfondita(combina_fine, G, N, V, V2, Ordine).

/*
    visitaInProfondita_scc/5
    Costruisce una SCC
*/
visitaInProfondita_scc(G, N, V, V2, Comp) :-
    visitaInProfondita(combina_testa, G, N, V, V2, Comp).

/*
    visitaInProfondita_grafo/2
    Calcola l’ordine di completamento globale
*/
visitaInProfondita_grafo(G, Ordine) :-
    nodi(G, Nodi),
    visitaInProfondita_lista(combina_fine, G, Nodi, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO TRASPOSTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    trasposto/2
    Costruisce il grafo trasposto
*/
trasposto(grafo(N,A), grafo(N,AT)) :-
    trasponi_archi(A, AT).

/*
    trasponi_archi/2
    Inverte tutti gli archi
*/
trasponi_archi([], []).
trasponi_archi([(X,Y)|T], [(Y,X)|R]) :-
    trasponi_archi(T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    kosaraju/2
    Calcola le componenti fortemente connesse
*/
kosaraju(G, SCCs) :-
    visitaInProfondita_grafo(G, Ordine),
    reverse(Ordine, OrdInv),
    trasposto(G, GT),
    kosaraju_visita(GT, OrdInv, [], SCCs).

/*
    kosaraju_visita/4
    Visita ausiliaria per Kosaraju
*/
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

/*
    scc_di_nodo/3
    Individua la SCC contenente un nodo
*/
scc_di_nodo(N, [S|_], S) :-
    membro(N,S), !.
scc_di_nodo(N, [_|T], S) :-
    scc_di_nodo(N,T,S).

/*
    arco_scc/4
    Verifica esistenza di un arco tra due SCC
*/
arco_scc(G, SCCs, S1, S2) :-
    archi(G,A),
    membro((X,Y),A),
    scc_di_nodo(X,SCCs,S1),
    scc_di_nodo(Y,SCCs,S2),
    S1 \= S2.

/*
    grado_entrante/4
    Calcola il grado entrante di una SCC
*/
grado_entrante(G, SCCs, S, Grado) :-
    findall(1, arco_scc(G,SCCs,_,S), L),
    length(L, Grado).
