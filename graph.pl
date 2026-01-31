% ##################################################################
% #         Corso di Programmazione Logica e Funzionale            #
% #        Progetto per la sessione invernale A.A 2025/2026        #
% #                        di Andrea Pedini                        #
% #                       Matricola: 322918                        #
% #                       e Matteo Fraternali                      #
% #                       Matricola: 316637                        #
% ##################################################################

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

main :-
    leggi_grafo_da_file('input.txt', G, Valido),
