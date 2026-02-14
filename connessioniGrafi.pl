% ##################################################################
% #         Corso di Programmazione Logica e Funzionale            #
% #        Progetto per la sessione invernale A.A 2025/2026        #
% #                        di Andrea Pedini                        #
% #                       Matricola: 322918                        #
% #                       e Matteo Fraternali                      #
% #                       Matricola: 316637                        #
% ##################################################################

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Predicato di avvio del programma.
    Legge il grafo dal file 'input.txt', ne valida il contenuto
    e avvia l'esecuzione del programma principale.
*/
main :-
    leggi_grafo_sicuro('input.txt', Risultato),
    gestisci_risultato(Risultato).

/* Predicato che gestisce il risultato della lettura e validazione del file. */
gestisci_risultato(errore) :-
    nl, stampa_separatore,
    write('Errore nel file di input.'), nl,
    write('Controllare formato, duplicati e contenuto del grafo.'), nl,
    stampa_separatore, nl, !, fail.

gestisci_risultato(ok(Grafo)) :-
    esegui_programma(Grafo).

/* Predicato principale che coordina l'esecuzione. */
esegui_programma(Grafo) :-
    vertici(Grafo, Vertici),
    archi(Grafo, Archi),
    kosaraju(Grafo, SCCs),

    stampa_separatore,
    write('            GRAFO LETTO DA FILE       '), nl,
    stampa_riga,
    write('Vertici: '), write(Vertici), nl,
    write('Archi:   '), write(Archi), nl,

    nl, stampa_separatore,
    write('       COMPONENTI FORTEMENTE CONNESSE '), nl,
    stampa_riga,
    stampa_scc_numerate(SCCs, 0),

    nl, stampa_separatore,
    write('           GRAFO COMPRESSO'), nl,
    stampa_riga,

    leggi_vertice_valido(Vertici, VerticeScelto),
    scc_di_vertice(VerticeScelto, SCCs, SCCPartenza),

    findall(S, (membro(S, SCCs), S \= SCCPartenza,
                grado_entrante(Grafo, SCCs, S, 0)), SCCZeroIn),
    length(SCCZeroIn, Conteggio),

    nl, stampa_separatore,
    write('Numero di SCC con indegree 0 (esclusa partenza): '),
    write(Conteggio), nl,
    stampa_separatore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA E VALIDAZIONE DEL GRAFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Predicato che legge il grafo da file in modo sicuro.
    Effettua controlli su formato, duplicati e coerenza archi.
*/
leggi_grafo_sicuro(File, ok(grafo(Vertici, Archi))) :-
    catch(open(File, read, Stream), _, fail),

    leggi_termine_sicuro(Stream, Vertici),
    is_list(Vertici),
    Vertici \= [],
    lista_vertici_valida(Vertici),
    vertici_senza_duplicati(Vertici),

    leggi_termine_sicuro(Stream, Archi),
    is_list(Archi),
    lista_archi_valida(Archi),

    close(Stream),
    archi_validi(Archi, Vertici), !.

leggi_grafo_sicuro(_, errore).

/* Predicato che verifica che la lista dei vertici non contenga duplicati. */
vertici_senza_duplicati([]).
vertici_senza_duplicati([H|T]) :-
    \+ membro(H, T),
    vertici_senza_duplicati(T).

/* Predicato che legge un termine Prolog da stream intercettando eccezioni. */
leggi_termine_sicuro(Stream, Termine) :-
    leggi_linea(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail).

/* Predicato che verifica che una lista contenga solo interi. */
lista_vertici_valida([]).
lista_vertici_valida([H|T]) :-
    integer(H),
    lista_vertici_valida(T).

/* Predicato che verifica che una lista contenga solo coppie (X,Y) di interi. */
lista_archi_valida([]).
lista_archi_valida([(X,Y)|T]) :-
    integer(X),
    integer(Y),
    lista_archi_valida(T).

/* Predicato che verifica che tutti gli archi usino solo vertici esistenti. */
archi_validi([], _).
archi_validi([(X,Y)|Resto], Vertici) :-
    membro(X, Vertici),
    membro(Y, Vertici),
    archi_validi(Resto, Vertici).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA DA STREAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Predicato che legge una riga da uno stream.
    Parametri:
    - Stream: stream di input
    - Linea: atomo contenente la riga letta
*/
leggi_linea(Stream, Linea) :-
    get_char(Stream, Char),
    leggi_linea_ausiliario(Stream, Char, Caratteri),
    atom_chars(Linea, Caratteri).

/*
    Predicato ausiliario che legge carattere per carattere
    fino a newline o fine file.
*/
leggi_linea_ausiliario(_, end_of_file, []) :- !.
leggi_linea_ausiliario(_, '\n', []) :- !.
leggi_linea_ausiliario(Stream, Char, [Char|Resto]) :-
    get_char(Stream, Next),
    leggi_linea_ausiliario(Stream, Next, Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INPUT UTENTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Predicato che richiede all'utente un vertice valido.
    Garantisce che sia intero e presente nel grafo.
*/
leggi_vertice_valido(Vertici, Vertice) :-
    write('Inserisci il vertice di partenza (tra '),
    write(Vertici), write('):'), nl,
    leggi_numero_valido(N),
    (membro(N, Vertici) -> Vertice = N
    ; write('Vertice non valido! Riprova.'), nl,
      leggi_vertice_valido(Vertici, Vertice)).

/* Predicato che legge un numero intero valido da input standard. */
leggi_numero_valido(N) :-
    leggi_linea(user_input, Line),
    (leggi_e_valida_numero(Line, N) -> true
    ; write('Input non valido! Inserisci un numero intero.'), nl,
      leggi_numero_valido(N)).

/* Predicato che verifica che una stringa rappresenti esattamente un intero. */
leggi_e_valida_numero(Line, N) :-
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail),
    numero_valido(Termine, N).

/* Predicato che controlla che il termine sia un numero intero puro. */
numero_valido(N, N) :- integer(N).
numero_valido(Termine, _) :-
    \+ integer(Termine),
    write('Errore: "'), write(Termine), write('" non e'' un numero intero.'), nl,
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che stampa una linea di separazione grafica. */
stampa_separatore :- write('======================================'), nl.

/* Predicato che stampa una riga separatrice corta. */
stampa_riga :- write('--------------------------------------'), nl.

/* Predicato che stampa tutte le SCC numerandole. */
stampa_scc_numerate([], _).
stampa_scc_numerate([S|Resto], Indice) :-
    write('SCC '), write(Indice), write(': '), write(S), nl,
    Next is Indice + 1,
    stampa_scc_numerate(Resto, Next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATI DI BASE SUL GRAFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che verifica l'appartenenza di un elemento a una lista. */
membro(X, [X|_]).
membro(X, [_|Resto]) :- membro(X, Resto).

/* Predicato che estrae i vertici dal termine grafo. */
vertici(grafo(N,_), N).

/* Predicato che estrae gli archi dal termine grafo. */
archi(grafo(_,A), A).

/* Predicato che verifica se esiste arco orientato X -> Y. */
adiacente(grafo(_,A), X, Y) :-
    membro((X,Y), A).

/* Predicato che restituisce tutti i vertici raggiungibili con un arco. */
adiacenti(Grafo, Vertice, Adiacenti) :-
    findall(Y, adiacente(Grafo, Vertice, Y), Adiacenti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITÀ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che implementa la visita in profondità generica parametrizzata. */
visitaInProfondita(_, _, Vertice, Visitati, Visitati, []) :-
    membro(Vertice, Visitati), !.

visitaInProfondita(Combina, Grafo, Vertice, Visitati, VisitatiFinali, Risultato) :-
    \+ membro(Vertice, Visitati),
    adiacenti(Grafo, Vertice, Vicini),
    visitaInProfondita_lista(Combina, Grafo, Vicini,
                            [Vertice|Visitati], VisitatiParziali, RisultatiFigli),
    call(Combina, Vertice, RisultatiFigli, Risultato),
    VisitatiFinali = VisitatiParziali.

/* Predicato che implementa la visita in profondità su lista di vertici. */
visitaInProfondita_lista(_, _, [], Visitati, Visitati, []).
visitaInProfondita_lista(Combina, Grafo, [H|T], Visitati, VisitatiFinali, Risultato) :-
    visitaInProfondita(Combina, Grafo, H, Visitati, Visitati1, R1),
    visitaInProfondita_lista(Combina, Grafo, T, Visitati1, VisitatiFinali, R2),
    append(R1, R2, Risultato).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITE SPECIALIZZATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che implementa la visita in profondità che costruisce ordine di completamento. */
visitaInProfondita_ordine(Grafo, Vertice, Visitati, VisitatiFinali, Ordine) :-
    visitaInProfondita(combina_fine, Grafo, Vertice, Visitati, VisitatiFinali, Ordine).

/* Predicato che implementa la visita in profondità che costruisce una SCC. */
visitaInProfondita_scc(Grafo, Vertice, Visitati, VisitatiFinali, Componente) :-
    visitaInProfondita(combina_testa, Grafo, Vertice, Visitati, VisitatiFinali, Componente).

/* Predicato che implementa la visita in profondità globale su tutto il grafo. */
visitaInProfondita_grafo(Grafo, Ordine) :-
    vertici(Grafo, Vertici),
    visitaInProfondita_lista(combina_fine, Grafo, Vertici, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRATEGIE DI COMBINAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che inserisce un vertice in coda alla lista. */
combina_fine(N, Lista, Risultato) :-
    append(Lista, [N], Risultato).

/* Predicato che inserisce un vertice in testa alla lista. */
combina_testa(N, Lista, [N|Lista]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO TRASPOSTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che costruisce il grafo trasposto invertendo tutti gli archi. */
trasposto(grafo(Vertici, Archi), grafo(Vertici, ArchiTrasposti)) :-
    trasponi_archi(Archi, ArchiTrasposti).

/* Predicato che inverte direzione di ogni arco. */
trasponi_archi([], []).
trasponi_archi([(X,Y)|Resto], [(Y,X)|Trasposti]) :-
    trasponi_archi(Resto, Trasposti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALGORITMO DI KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che implementa l'algoritmo di Kosaraju. */
kosaraju(Grafo, SCCs) :-
    visitaInProfondita_grafo(Grafo, Ordine),
    reverse(Ordine, OrdineInverso),
    trasposto(Grafo, GrafoTrasposto),
    kosaraju_visita(GrafoTrasposto, OrdineInverso, [], SCCs).

/* Predicato che che implementa la costruzione progressiva delle SCC. */
kosaraju_visita(_, [], _, []).
kosaraju_visita(Grafo, [Vertice|Resto], Visitati, SCCs) :-
    membro(Vertice, Visitati), !,
    kosaraju_visita(Grafo, Resto, Visitati, SCCs).

kosaraju_visita(Grafo, [Vertice|Resto], Visitati, [SCC|Altre]) :-
    visitaInProfondita_scc(Grafo, Vertice, Visitati, Visitati1, SCC),
    kosaraju_visita(Grafo, Resto, Visitati1, Altre).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO COMPRESSO DELLE SCC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicato che trova la SCC che contiene un vertice. */
scc_di_vertice(Vertice, [S|_], S) :-
    membro(Vertice, S), !.
scc_di_vertice(Vertice, [_|Resto], S) :-
    scc_di_vertice(Vertice, Resto, S).

/* Predicato che verifica l'esistenza di un arco tra SCC diverse. */
arco_scc(Grafo, SCCs, S1, S2) :-
    archi(Grafo, Archi),
    membro((X,Y), Archi),
    scc_di_vertice(X, SCCs, S1),
    scc_di_vertice(Y, SCCs, S2),
    S1 \= S2.

/* Predicato che calcola il grado entrante di una SCC.*/
grado_entrante(Grafo, SCCs, SCC, Grado) :-
    findall(1, arco_scc(Grafo, SCCs, _, SCC), Lista),
    length(Lista, Grado).