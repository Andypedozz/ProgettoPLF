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
Predicato che avvia il programma leggendo il grafo dal file e gestendo il risultato della lettura.
Parametri:
 - Nessuno
*/
main :-
    leggi_grafo_sicuro('input.txt', Risultato),
    gestisci_risultato(Risultato).

/*
Predicato che gestisce il risultato della lettura e validazione del file di input.
Parametri:
 - Risultato: può assumere il valore errore oppure ok(Grafo)
*/
gestisci_risultato(errore) :-
    nl, stampa_separatore,
    write('Errore nel file di input.'), nl,
    write('Controllare formato, duplicati e contenuto del grafo.'), nl,
    stampa_separatore, nl, !, fail.

gestisci_risultato(ok(Grafo)) :-
    esegui_programma(Grafo).

/*
Predicato che coordina l’esecuzione del programma stampando il grafo, calcolando le SCC e mostrando i risultati finali.
Parametri:
 - Grafo: struttura grafo(Vertici, Archi) rappresentante il grafo orientato
*/
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
Predicato che legge un grafo da file effettuando controlli su formato, duplicati e coerenza degli archi.
Parametri:
 - File: nome del file da cui leggere il grafo
 - Risultato: ok(grafo(Vertici,Archi)) se valido, altrimenti errore
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

/*
Predicato che verifica che una lista di vertici non contenga elementi duplicati.
Parametri:
 - ListaVertici: lista di vertici da controllare
*/
vertici_senza_duplicati([]).
vertici_senza_duplicati([H|T]) :-
    \+ membro(H, T),
    vertici_senza_duplicati(T).

/*
Predicato che legge un termine Prolog da uno stream intercettando eventuali eccezioni.
Parametri:
 - Stream: stream di input
 - Termine: termine Prolog letto dallo stream
*/
leggi_termine_sicuro(Stream, Termine) :-
    leggi_linea(Stream, Line),
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail).

/*
Predicato che verifica che una lista contenga esclusivamente numeri interi.
Parametri:
 - ListaVertici: lista da validare
*/
lista_vertici_valida([]).
lista_vertici_valida([H|T]) :-
    integer(H),
    lista_vertici_valida(T).

/*
Predicato che verifica che una lista contenga solo coppie di interi rappresentanti archi.
Parametri:
 - ListaArchi: lista di coppie (X,Y)
*/
lista_archi_valida([]).
lista_archi_valida([(X,Y)|T]) :-
    integer(X),
    integer(Y),
    lista_archi_valida(T).

/*
Predicato che verifica che ogni arco utilizzi solo vertici presenti nella lista dei vertici.
Parametri:
 - Archi: lista degli archi
 - Vertici: lista dei vertici del grafo
*/
archi_validi([], _).
archi_validi([(X,Y)|Resto], Vertici) :-
    membro(X, Vertici),
    membro(Y, Vertici),
    archi_validi(Resto, Vertici).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LETTURA DA STREAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che legge una riga da uno stream e la restituisce come atomo.
Parametri:
 - Stream: stream di input
 - Linea: atomo contenente la riga letta
*/
leggi_linea(Stream, Linea) :-
    get_char(Stream, Char),
    leggi_linea_ausiliario(Stream, Char, Caratteri),
    atom_chars(Linea, Caratteri).

/*
Predicato che legge carattere per carattere fino a newline o fine file.
Parametri:
 - Stream: stream di input
 - Char: carattere corrente letto
 - ListaCaratteri: lista dei caratteri letti
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
Predicato che richiede all’utente un vertice valido presente nel grafo.
Parametri:
 - Vertici: lista dei vertici del grafo
 - Vertice: vertice scelto e validato
*/
leggi_vertice_valido(Vertici, Vertice) :-
    write('Inserisci il vertice di partenza (tra '),
    write(Vertici), write('): '),
    leggi_numero_valido(N),
    (membro(N, Vertici) -> Vertice = N
    ; write('Vertice non valido! Riprova.'), nl,
      leggi_vertice_valido(Vertici, Vertice)).

/*
Predicato che legge da input standard un numero intero valido.
Parametri:
 - N: numero intero letto e validato
*/
leggi_numero_valido(N) :-
    leggi_linea(user_input, Line),
    (leggi_e_valida_numero(Line, N) -> true
    ; write('Input non valido! Inserisci un numero intero.'), nl,
      leggi_numero_valido(N)).

/*
Predicato che verifica che una stringa rappresenti esattamente un numero intero.
Parametri:
 - Line: atomo contenente l’input dell’utente
 - N: numero intero risultante
*/
leggi_e_valida_numero(Line, N) :-
    atom_concat(Line, '.', LineConPunto),
    catch(read_from_atom(LineConPunto, Termine), _, fail),
    numero_valido(Termine, N).

/*
Predicato che controlla che un termine sia un numero intero puro.
Parametri:
 - Termine: termine da verificare
 - N: numero intero valido risultante
*/
numero_valido(N, N) :- integer(N).
numero_valido(Termine, _) :-
    \+ integer(Termine),
    write('Errore: "'), write(Termine), write('" non e'' un numero intero.'), nl,
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STAMPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che stampa una linea grafica di separazione lunga.
Parametri:
 - Nessuno
*/
stampa_separatore :- write('======================================'), nl.

/*
Predicato che stampa una linea grafica di separazione corta.
Parametri:
 - Nessuno
*/
stampa_riga :- write('--------------------------------------'), nl.

/*
Predicato che stampa le componenti fortemente connesse numerandole progressivamente.
Parametri:
 - ListaSCC: lista delle componenti fortemente connesse
 - Indice: numero progressivo della componente
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
Predicato che verifica l’appartenenza di un elemento a una lista.
Parametri:
 - X: elemento da cercare
 - Lista: lista in cui cercare
*/
membro(X, [X|_]).
membro(X, [_|Resto]) :- membro(X, Resto).

/*
Predicato che estrae la lista dei vertici da una struttura grafo.
Parametri:
 - Grafo: termine grafo(Vertici,Archi)
 - Vertici: lista dei vertici estratta
*/
vertici(grafo(N,_), N).

/*
Predicato che estrae la lista degli archi da una struttura grafo.
Parametri:
 - Grafo: termine grafo(Vertici,Archi)
 - Archi: lista degli archi estratta
*/
archi(grafo(_,A), A).

/*
Predicato che verifica l’esistenza di un arco orientato tra due vertici.
Parametri:
 - Grafo: struttura del grafo
 - X: vertice di partenza
 - Y: vertice di arrivo
*/
adiacente(grafo(_,A), X, Y) :-
    membro((X,Y), A).

/*
Predicato che restituisce la lista dei vertici adiacenti a un vertice dato.
Parametri:
 - Grafo: struttura del grafo
 - Vertice: vertice di partenza
 - Adiacenti: lista dei vertici raggiungibili
*/
adiacenti(Grafo, Vertice, Adiacenti) :-
    findall(Y, adiacente(Grafo, Vertice, Y), Adiacenti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITÀ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che implementa la visita in profondità parametrizzata con una strategia di combinazione.
Parametri:
 - Combina: predicato di combinazione dei risultati
 - Grafo: struttura del grafo
 - Vertice: vertice corrente
 - Visitati: lista dei vertici già visitati
 - VisitatiFinali: lista finale dei vertici visitati
 - Risultato: risultato aggregato della visita
*/
visitaInProfondita(_, _, Vertice, Visitati, Visitati, []) :-
    membro(Vertice, Visitati), !.

visitaInProfondita(Combina, Grafo, Vertice, Visitati, VisitatiFinali, Risultato) :-
    \+ membro(Vertice, Visitati),
    adiacenti(Grafo, Vertice, Vicini),
    visitaInProfondita_lista(Combina, Grafo, Vicini,
                            [Vertice|Visitati], VisitatiParziali, RisultatiFigli),
    call(Combina, Vertice, RisultatiFigli, Risultato),
    VisitatiFinali = VisitatiParziali.

/*
Predicato che applica la visita in profondità a una lista di vertici.
Parametri:
 - Combina: predicato di combinazione
 - Grafo: struttura del grafo
 - ListaVertici: lista di vertici da visitare
 - Visitati: lista dei vertici già visitati
 - VisitatiFinali: lista finale dei vertici visitati
 - Risultato: risultato aggregato complessivo
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
Predicato che esegue una visita in profondità costruendo l’ordine di completamento.
Parametri:
 - Grafo: struttura del grafo
 - Vertice: vertice di partenza
 - Visitati: lista dei vertici già visitati
 - VisitatiFinali: lista finale dei vertici visitati
 - Ordine: lista dei vertici in ordine di completamento
*/
visitaInProfondita_ordine(Grafo, Vertice, Visitati, VisitatiFinali, Ordine) :-
    visitaInProfondita(combina_fine, Grafo, Vertice, Visitati, VisitatiFinali, Ordine).

/*
Predicato che esegue una visita in profondità per costruire una componente fortemente connessa.
Parametri:
 - Grafo: struttura del grafo
 - Vertice: vertice di partenza
 - Visitati: lista dei vertici già visitati
 - VisitatiFinali: lista finale dei vertici visitati
 - Componente: lista dei vertici appartenenti alla SCC
*/
visitaInProfondita_scc(Grafo, Vertice, Visitati, VisitatiFinali, Componente) :-
    visitaInProfondita(combina_testa, Grafo, Vertice, Visitati, VisitatiFinali, Componente).

/*
Predicato che esegue la visita in profondità sull’intero grafo.
Parametri:
 - Grafo: struttura del grafo
 - Ordine: lista dei vertici in ordine di completamento globale
*/
visitaInProfondita_grafo(Grafo, Ordine) :-
    vertici(Grafo, Vertici),
    visitaInProfondita_lista(combina_fine, Grafo, Vertici, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRATEGIE DI COMBINAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che inserisce un vertice in coda a una lista.
Parametri:
 - N: vertice da inserire
 - Lista: lista esistente
 - Risultato: lista con N aggiunto in coda
*/
combina_fine(N, Lista, Risultato) :-
    append(Lista, [N], Risultato).

/*
Predicato che inserisce un vertice in testa a una lista.
Parametri:
 - N: vertice da inserire
 - Lista: lista esistente
 - Risultato: lista con N aggiunto in testa
*/
combina_testa(N, Lista, [N|Lista]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAFO TRASPOSTO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che costruisce il grafo trasposto invertendo tutti gli archi.
Parametri:
 - Grafo: grafo originale
 - GrafoTrasposto: grafo con archi invertiti
*/
trasposto(grafo(Vertici, Archi), grafo(Vertici, ArchiTrasposti)) :-
    trasponi_archi(Archi, ArchiTrasposti).

/*
Predicato che inverte la direzione di ogni arco.
Parametri:
 - Archi: lista originale di archi
 - ArchiTrasposti: lista di archi invertiti
*/
trasponi_archi([], []).
trasponi_archi([(X,Y)|Resto], [(Y,X)|Trasposti]) :-
    trasponi_archi(Resto, Trasposti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALGORITMO DI KOSARAJU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Predicato che implementa l’algoritmo di Kosaraju per il calcolo delle componenti fortemente connesse.
Parametri:
 - Grafo: struttura del grafo
 - SCCs: lista delle componenti fortemente connesse trovate
*/
kosaraju(Grafo, SCCs) :-
    visitaInProfondita_grafo(Grafo, Ordine),
    reverse(Ordine, OrdineInverso),
    trasposto(Grafo, GrafoTrasposto),
    kosaraju_visita(GrafoTrasposto, OrdineInverso, [], SCCs).

/*
Predicato che costruisce progressivamente l’elenco delle componenti fortemente connesse.
Parametri:
 - Grafo: grafo trasposto
 - ListaVertici: vertici ordinati per visita
 - Visitati: lista dei vertici già assegnati a una SCC
 - SCCs: lista finale delle componenti fortemente connesse
*/
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

/*
Predicato che individua la componente fortemente connessa contenente un dato vertice.
Parametri:
 - Vertice: vertice da cercare
 - ListaSCC: lista delle componenti
 - SCC: componente che contiene il vertice
*/
scc_di_vertice(Vertice, [S|_], S) :-
    membro(Vertice, S), !.
scc_di_vertice(Vertice, [_|Resto], S) :-
    scc_di_vertice(Vertice, Resto, S).

/*
Predicato che verifica l’esistenza di un arco tra due componenti fortemente connesse distinte.
Parametri:
 - Grafo: struttura del grafo originale
 - SCCs: lista delle componenti fortemente connesse
 - S1: componente sorgente
 - S2: componente destinazione
*/
arco_scc(Grafo, SCCs, S1, S2) :-
    archi(Grafo, Archi),
    membro((X,Y), Archi),
    scc_di_vertice(X, SCCs, S1),
    scc_di_vertice(Y, SCCs, S2),
    S1 \= S2.

/*
Predicato che calcola il grado entrante di una componente fortemente connessa nel grafo compresso.
Parametri:
 - Grafo: struttura del grafo originale
 - SCCs: lista delle componenti fortemente connesse
 - SCC: componente di cui calcolare il grado entrante
 - Grado: numero di archi entranti nella componente
*/
grado_entrante(Grafo, SCCs, SCC, Grado) :-
    findall(1, arco_scc(Grafo, SCCs, _, SCC), Lista),
    length(Lista, Grado).
