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
    Legge il grafo da file 'input.txt' e gestisce il risultato.
    Non riceve parametri, il file di input è fisso.
*/
main :-
    leggi_grafo_sicuro('input.txt', Risultato),
    gestisci_risultato(Risultato).

/*
    Gestisce il risultato della lettura del file.
    Parametri:
    - Risultato: errore oppure ok(Grafo)
    In caso di errore stampa messaggio e fallisce.
    In caso di successo passa il grafo a esegui_programma.
*/
gestisci_risultato(errore) :-
    nl, stampa_separatore,
    write('Errore nel file di input.'), nl,
    write('Controllare formato e contenuto del grafo.'), nl,
    stampa_separatore, nl, !, fail.

gestisci_risultato(ok(Grafo)) :-
    esegui_programma(Grafo).

/*
    Predicato principale che coordina l'esecuzione del programma.
    Parametri:
    - Grafo: termine grafo(Nodi, Archi)
    Calcola le SCC, stampa informazioni, chiede input utente
    e calcola le SCC con grado entrante zero.
*/
esegui_programma(Grafo) :-
    nodi(Grafo, Nodi),
    archi(Grafo, Archi),
    kosaraju(Grafo, SCCs),

    stampa_separatore,
    write('            GRAFO LETTO DA FILE       '), nl,
    stampa_riga,
    write('Vertici: '), write(Nodi), nl,
    write('Archi:   '), write(Archi), nl,

    nl, stampa_separatore,
    write('       COMPONENTI FORTEMENTE CONNESSE '), nl,
    stampa_riga,
    stampa_scc_numerate(SCCs, 0),

    nl, stampa_separatore,
    write('           GRAFO COMPRESSO'), nl,
    stampa_riga,

    leggi_vertice_valido(Nodi, NodoScelto),
    scc_di_nodo(NodoScelto, SCCs, SCCPartenza),

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
    Legge il grafo da file in modo sicuro.
    Parametri:
    - File: nome del file di input
    - Risultato: ok(grafo(Nodi,Archi)) se successo, errore altrimenti
    Valida formato e contenuto del grafo letto.
*/
leggi_grafo_sicuro(File, ok(grafo(Nodi, Archi))) :-
    catch(open(File, read, Stream), _, fail),
    leggi_termine_sicuro(Stream, Nodi),
    is_list(Nodi), Nodi \= [], lista_nodi_valida(Nodi),
    leggi_termine_sicuro(Stream, Archi),
    is_list(Archi), lista_archi_valida(Archi),
    close(Stream),
    archi_validi(Archi, Nodi), !.

leggi_grafo_sicuro(_, errore).

/*
    Legge un termine Prolog da stream intercettando eccezioni.
    Parametri:
    - Stream: stream di input aperto
    - Termine: termine Prolog letto dalla riga
*/
leggi_termine_sicuro(Stream, Termine) :-
    leggi_linea(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail).

/*
    Verifica che una lista contenga solo interi.
    Parametri:
    - Lista: lista da validare come lista di nodi
*/
lista_nodi_valida([]).
lista_nodi_valida([H|T]) :- integer(H), lista_nodi_valida(T).

/*
    Verifica che una lista contenga solo coppie (X,Y) di interi.
    Parametri:
    - Lista: lista da validare come lista di archi
*/
lista_archi_valida([]).
lista_archi_valida([(X,Y)|T]) :- integer(X), integer(Y), lista_archi_valida(T).

/*
    Verifica che tutti gli archi usino nodi esistenti.
    Parametri:
    - Archi: lista di archi (X,Y)
    - Nodi: lista di nodi validi
*/
archi_validi([], _).
archi_validi([(X,Y)|Resto], Nodi) :-
    membro(X, Nodi), membro(Y, Nodi), archi_validi(Resto, Nodi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA DA STREAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Legge una riga da uno stream.
    Parametri:
    - Stream: stream di input
    - Linea: atomo contenente il testo della riga
*/
leggi_linea(Stream, Linea) :-
    get_char(Stream, Char),
    leggi_linea_aux(Stream, Char, Caratteri),
    atom_chars(Linea, Caratteri).

/*
    Predicato ausiliario per leggere carattere per carattere.
    Parametri:
    - Stream: stream di input
    - Char: carattere corrente
    - Caratteri: lista dei caratteri accumulati
*/
leggi_linea_aux(_, end_of_file, []) :- !.
leggi_linea_aux(_, '\n', []) :- !.
leggi_linea_aux(Stream, Char, [Char|Resto]) :-
    get_char(Stream, Next),
    leggi_linea_aux(Stream, Next, Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INPUT UTENTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Richiede all'utente un nodo valido.
    Parametri:
    - Nodi: lista dei nodi validi del grafo
    - Nodo: nodo scelto dall'utente (validato)
    Garantisce che l'input sia esattamente un numero intero
    e che appartenga alla lista dei nodi del grafo.
*/
leggi_vertice_valido(Nodi, Nodo) :-
    write('Inserisci il vertice di partenza (tra '),
    write(Nodi), write('):'), nl,
    leggi_numero_valido(N),
    (membro(N, Nodi) -> Nodo = N
    ; write('Vertice non valido! Riprova.'), nl,
      leggi_vertice_valido(Nodi, Nodo)).

/*
    Legge un numero intero valido da input standard.
    Parametri:
    - N: numero intero letto e validato
    Gestisce input non validi come stringhe, numeri con spazi,
    o altri caratteri non numerici.
*/
leggi_numero_valido(N) :-
    leggi_linea(user_input, Line),
    (leggi_e_valida_numero(Line, N) -> true
    ; write('Input non valido! Inserisci un numero intero.'), nl,
      leggi_numero_valido(N)).

/*
    Legge e valida che una stringa rappresenti esattamente un numero intero.
    Parametri:
    - Line: atomo contenente l'input
    - N: numero intero validato
    Utilizza read_from_atom per parsing sicuro e verifica che non ci siano
    termini aggiuntivi dopo il numero.
*/
leggi_e_valida_numero(Line, N) :-
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail),
    numero_valido(Termine, N).

/*
    Verifica che un termine rappresenti un numero intero valido.
    Parametri:
    - Termine: termine letto dall'input
    - N: numero intero validato
    Controlla che il termine sia un intero e che non contenga
    strutture complesse o altri elementi.
*/
numero_valido(N, N) :- integer(N).
numero_valido(Termine, _) :- 
    \+ integer(Termine), 
    write('Errore: "'), write(Termine), write('" non è un numero intero.'), nl,
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Stampa una linea di separazione grafica.
*/
stampa_separatore :- write('======================================'), nl.

/*
    Stampa una riga separatrice più corta.
*/
stampa_riga :- write('--------------------------------------'), nl.

/*
    Stampa tutte le SCC numerandole.
    Parametri:
    - Lista delle SCC da stampare
    - Indice: numero progressivo per la numerazione
*/
stampa_scc_numerate([], _).
stampa_scc_numerate([S|Resto], Indice) :-
    write('SCC '), write(Indice), write(': '), write(S), nl,
    Next is Indice + 1,
    stampa_scc_numerate(Resto, Next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATI DI BASE SUL GRAFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Verifica l'appartenenza di un elemento a una lista.
    Parametri:
    - X: elemento da cercare
    - Lista: lista in cui cercare
*/
membro(X, [X|_]).
membro(X, [_|Resto]) :- membro(X, Resto).

/*
    Estrae la lista dei nodi dal grafo.
    Parametri:
    - Grafo: termine grafo(Nodi, Archi)
    - Nodi: lista dei vertici
*/
nodi(grafo(N,_), N).

/*
    Estrae la lista degli archi dal grafo.
    Parametri:
    - Grafo: termine grafo(Nodi, Archi)
    - Archi: lista delle coppie (X,Y)
*/
archi(grafo(_,A), A).

/*
    Verifica se esiste un arco orientato tra due nodi.
    Parametri:
    - Grafo: grafo di riferimento
    - X: nodo sorgente
    - Y: nodo destinazione
*/
adiacente(grafo(_,A), X, Y) :- membro((X,Y), A).

/*
    Restituisce tutti i nodi raggiungibili da un nodo.
    Parametri:
    - Grafo: grafo di riferimento
    - Nodo: nodo di partenza
    - Adiacenti: lista dei nodi raggiungibili con un arco
*/
adiacenti(Grafo, Nodo, Adiacenti) :-
    findall(Y, adiacente(Grafo, Nodo, Y), Adiacenti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITÀ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Implementazione generica della visita in profondità.
    Parametri:
    - Combina: predicato che definisce come accumulare risultato
    - Grafo: grafo su cui eseguire la visita
    - Nodo: nodo corrente
    - Visitati: lista nodi già visitati
    - VisitatiFinali: lista nodi visitati al termine
    - Risultato: risultato prodotto dalla visita
*/
visitaInProfondita(_, _, Nodo, Visitati, Visitati, []) :-
    membro(Nodo, Visitati), !.

visitaInProfondita(Combina, Grafo, Nodo, Visitati, VisitatiFinali, Risultato) :-
    \+ membro(Nodo, Visitati),
    adiacenti(Grafo, Nodo, Vicini),
    visitaInProfondita_lista(Combina, Grafo, Vicini,
                            [Nodo|Visitati], VisitatiParziali, RisultatiFigli),
    call(Combina, Nodo, RisultatiFigli, Risultato),
    VisitatiFinali = VisitatiParziali.

/*
    Versione della visita che opera su lista di nodi.
    Parametri:
    - Combina: strategia di accumulo
    - Grafo: grafo di riferimento
    - Lista: nodi da visitare
    - Visitati: nodi già visitati
    - VisitatiFinali: nodi visitati al termine
    - Risultato: risultato complessivo
*/
visitaInProfondita_lista(_, _, [], Visitati, Visitati, []).
visitaInProfondita_lista(Combina, Grafo, [H|T], Visitati, VisitatiFinali, Risultato) :-
    visitaInProfondita(Combina, Grafo, H, Visitati, Visitati1, R1),
    visitaInProfondita_lista(Combina, Grafo, T, Visitati1, VisitatiFinali, R2),
    append(R1, R2, Risultato).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITE SPECIALIZZATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Calcola l'ordine di completamento dei nodi.
    Parametri:
    - Grafo: grafo di riferimento
    - Nodo: nodo di partenza
    - Visitati: nodi già visitati
    - VisitatiFinali: nodi visitati al termine
    - Ordine: lista nodi in ordine di completamento
*/
visitaInProfondita_ordine(Grafo, Nodo, Visitati, VisitatiFinali, Ordine) :-
    visitaInProfondita(combina_fine, Grafo, Nodo, Visitati, VisitatiFinali, Ordine).

/*
    Costruisce una singola SCC.
    Parametri:
    - Grafo: grafo di riferimento
    - Nodo: nodo di partenza
    - Visitati: nodi già visitati
    - VisitatiFinali: nodi visitati al termine
    - Componente: SCC costruita
*/
visitaInProfondita_scc(Grafo, Nodo, Visitati, VisitatiFinali, Componente) :-
    visitaInProfondita(combina_testa, Grafo, Nodo, Visitati, VisitatiFinali, Componente).

/*
    Calcola ordine di completamento per tutti i nodi.
    Parametri:
    - Grafo: grafo di riferimento
    - Ordine: lista nodi in ordine di completamento
*/
visitaInProfondita_grafo(Grafo, Ordine) :-
    nodi(Grafo, Nodi),
    visitaInProfondita_lista(combina_fine, Grafo, Nodi, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRATEGIE DI COMBINAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Inserisce nodo in coda alla lista.
    Parametri:
    - N: nodo da inserire
    - Lista: lista corrente
    - Risultato: lista risultante
*/
combina_fine(N, Lista, Risultato) :- append(Lista, [N], Risultato).

/*
    Inserisce nodo in testa alla lista.
    Parametri:
    - N: nodo da inserire
    - Lista: lista corrente
    - Risultato: lista risultante
*/
combina_testa(N, Lista, [N|Lista]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO TRASPOSTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Costruisce il grafo trasposto.
    Parametri:
    - Grafo originale
    - GrafoTrasposto: grafo con archi invertiti
*/
trasposto(grafo(Nodi, Archi), grafo(Nodi, ArchiTrasposti)) :-
    trasponi_archi(Archi, ArchiTrasposti).

/*
    Inverte la direzione di tutti gli archi.
    Parametri:
    - Archi: lista archi originali
    - ArchiTrasposti: lista archi invertiti
*/
trasponi_archi([], []).
trasponi_archi([(X,Y)|Resto], [(Y,X)|Trasposti]) :-
    trasponi_archi(Resto, Trasposti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALGORITMO DI KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Implementazione algoritmo di Kosaraju.
    Parametri:
    - Grafo: grafo originale
    - SCCs: lista delle componenti fortemente connesse
*/
kosaraju(Grafo, SCCs) :-
    visitaInProfondita_grafo(Grafo, Ordine),
    reverse(Ordine, OrdineInverso),
    trasposto(Grafo, GrafoTrasposto),
    kosaraju_visita(GrafoTrasposto, OrdineInverso, [], SCCs).

/*
    Visita ausiliaria che costruisce progressivamente le SCC.
    Parametri:
    - Grafo: grafo trasposto
    - Ordine: lista nodi in ordine di visita
    - Visitati: nodi già visitati
    - SCCs: lista delle SCC costruite
*/
kosaraju_visita(_, [], _, []).
kosaraju_visita(Grafo, [Nodo|Resto], Visitati, SCCs) :-
    membro(Nodo, Visitati), !,
    kosaraju_visita(Grafo, Resto, Visitati, SCCs).

kosaraju_visita(Grafo, [Nodo|Resto], Visitati, [SCC|Altre]) :-
    visitaInProfondita_scc(Grafo, Nodo, Visitati, Visitati1, SCC),
    kosaraju_visita(Grafo, Resto, Visitati1, Altre).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO COMPRESSO DELLE SCC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Trova la SCC che contiene un nodo.
    Parametri:
    - Nodo: nodo da cercare
    - SCCs: lista delle SCC
    - SCC: SCC che contiene il nodo
*/
scc_di_nodo(Nodo, [S|_], S) :- membro(Nodo, S), !.
scc_di_nodo(Nodo, [_|Resto], S) :- scc_di_nodo(Nodo, Resto, S).

/*
    Verifica se esiste arco tra due SCC diverse.
    Parametri:
    - Grafo: grafo originale
    - SCCs: lista delle SCC
    - S1: SCC sorgente
    - S2: SCC destinazione
*/
arco_scc(Grafo, SCCs, S1, S2) :-
    archi(Grafo, Archi),
    membro((X,Y), Archi),
    scc_di_nodo(X, SCCs, S1),
    scc_di_nodo(Y, SCCs, S2),
    S1 \= S2.

/*
    Calcola il grado entrante di una SCC.
    Parametri:
    - Grafo: grafo originale
    - SCCs: lista delle SCC
    - SCC: SCC di cui calcolare grado
    - Grado: numero archi entranti
*/
grado_entrante(Grafo, SCCs, SCC, Grado) :-
    findall(1, arco_scc(Grafo, SCCs, _, SCC), Lista),
    length(Lista, Grado).