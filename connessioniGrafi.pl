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
    - Grafo: termine grafo(Vertici, Archi)
    Calcola le SCC, stampa informazioni, chiede input utente
    e calcola le SCC con grado entrante zero.
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
    Legge il grafo da file in modo sicuro.
    Parametri:
    - File: nome del file di input
    - Risultato: ok(grafo(Vertici,Archi)) se successo, errore altrimenti
    Valida formato e contenuto del grafo letto.
*/
leggi_grafo_sicuro(File, ok(grafo(Vertici, Archi))) :-
    catch(open(File, read, Stream), _, fail),
    leggi_termine_sicuro(Stream, Vertici),
    is_list(Vertici), Vertici \= [], lista_vertici_valida(Vertici),
    leggi_termine_sicuro(Stream, Archi),
    is_list(Archi), lista_archi_valida(Archi),
    close(Stream),
    archi_validi(Archi, Vertici), !.

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
    - Lista: lista da validare come lista di vertici
*/
lista_vertici_valida([]).
lista_vertici_valida([H|T]) :- integer(H), lista_vertici_valida(T).

/*
    Verifica che una lista contenga solo coppie (X,Y) di interi.
    Parametri:
    - Lista: lista da validare come lista di archi
*/
lista_archi_valida([]).
lista_archi_valida([(X,Y)|T]) :- integer(X), integer(Y), lista_archi_valida(T).

/*
    Verifica che tutti gli archi usino vertici esistenti.
    Parametri:
    - Archi: lista di archi (X,Y)
    - Vertici: lista di vertici validi
*/
archi_validi([], _).
archi_validi([(X,Y)|Resto], Vertici) :-
    membro(X, Vertici), membro(Y, Vertici), archi_validi(Resto, Vertici).

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
    Richiede all'utente un vertice valido.
    Parametri:
    - Vertici: lista dei vertici validi del grafo
    - Vertice: vertice scelto dall'utente (validato)
    Garantisce che l'input sia esattamente un numero intero
    e che appartenga alla lista dei vertici del grafo.
*/
leggi_vertice_valido(Vertici, Vertice) :-
    write('Inserisci il vertice di partenza (tra '),
    write(Vertici), write('):'), nl,
    leggi_numero_valido(N),
    (membro(N, Vertici) -> Vertice = N
    ; write('Vertice non valido! Riprova.'), nl,
      leggi_vertice_valido(Vertici, Vertice)).

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
    Estrae la lista dei vertici dal grafo.
    Parametri:
    - Grafo: termine grafo(Vertici, Archi)
    - Vertici: lista dei vertici
*/
vertici(grafo(N,_), N).

/*
    Estrae la lista degli archi dal grafo.
    Parametri:
    - Grafo: termine grafo(Vertici, Archi)
    - Archi: lista delle coppie (X,Y)
*/
archi(grafo(_,A), A).

/*
    Verifica se esiste un arco orientato tra due vertici.
    Parametri:
    - Grafo: grafo di riferimento
    - X: vertice sorgente
    - Y: vertice destinazione
*/
adiacente(grafo(_,A), X, Y) :- membro((X,Y), A).

/*
    Restituisce tutti i vertici raggiungibili da un vertice.
    Parametri:
    - Grafo: grafo di riferimento
    - Vertice: vertice di partenza
    - Adiacenti: lista dei vertici raggiungibili con un arco
*/
adiacenti(Grafo, Vertice, Adiacenti) :-
    findall(Y, adiacente(Grafo, Vertice, Y), Adiacenti).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISITA IN PROFONDITÀ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Implementazione generica della visita in profondità.
    Parametri:
    - Combina: predicato che definisce come accumulare risultato
    - Grafo: grafo su cui eseguire la visita
    - Vertice: vertice corrente
    - Visitati: lista vertici già visitati
    - VisitatiFinali: lista vertici visitati al termine
    - Risultato: risultato prodotto dalla visita
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
    Versione della visita che opera su lista di vertici.
    Parametri:
    - Combina: strategia di accumulo
    - Grafo: grafo di riferimento
    - Lista: vertici da visitare
    - Visitati: vertici già visitati
    - VisitatiFinali: vertici visitati al termine
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
    Calcola l'ordine di completamento dei vertici.
    Parametri:
    - Grafo: grafo di riferimento
    - Vertice: vertice di partenza
    - Visitati: vertici già visitati
    - VisitatiFinali: vertici visitati al termine
    - Ordine: lista vertici in ordine di completamento
*/
visitaInProfondita_ordine(Grafo, Vertice, Visitati, VisitatiFinali, Ordine) :-
    visitaInProfondita(combina_fine, Grafo, Vertice, Visitati, VisitatiFinali, Ordine).

/*
    Costruisce una singola SCC.
    Parametri:
    - Grafo: grafo di riferimento
    - Vertice: vertice di partenza
    - Visitati: vertici già visitati
    - VisitatiFinali: vertici visitati al termine
    - Componente: SCC costruita
*/
visitaInProfondita_scc(Grafo, Vertice, Visitati, VisitatiFinali, Componente) :-
    visitaInProfondita(combina_testa, Grafo, Vertice, Visitati, VisitatiFinali, Componente).

/*
    Calcola ordine di completamento per tutti i vertici.
    Parametri:
    - Grafo: grafo di riferimento
    - Ordine: lista vertici in ordine di completamento
*/
visitaInProfondita_grafo(Grafo, Ordine) :-
    vertici(Grafo, Vertici),
    visitaInProfondita_lista(combina_fine, Grafo, Vertici, [], _, Ordine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRATEGIE DI COMBINAZIONE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Inserisce vertice in coda alla lista.
    Parametri:
    - N: vertice da inserire
    - Lista: lista corrente
    - Risultato: lista risultante
*/
combina_fine(N, Lista, Risultato) :- append(Lista, [N], Risultato).

/*
    Inserisce vertice in testa alla lista.
    Parametri:
    - N: vertice da inserire
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
trasposto(grafo(Vertici, Archi), grafo(Vertici, ArchiTrasposti)) :-
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
    - Ordine: lista vertici in ordine di visita
    - Visitati: vertici già visitati
    - SCCs: lista delle SCC costruite
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
    Trova la SCC che contiene un vertice.
    Parametri:
    - Vertice: vertice da cercare
    - SCCs: lista delle SCC
    - SCC: SCC che contiene il vertice
*/
scc_di_vertice(Vertice, [S|_], S) :- membro(Vertice, S), !.
scc_di_vertice(Vertice, [_|Resto], S) :- scc_di_vertice(Vertice, Resto, S).

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
    scc_di_vertice(X, SCCs, S1),
    scc_di_vertice(Y, SCCs, S2),
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