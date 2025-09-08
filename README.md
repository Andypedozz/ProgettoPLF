# Progetto di Programmazione Funzionale e Logica
Questo progetto implementa la soluzione a un problema sui grafi, enunciato come segue:
Dato un insieme di vertici, un insieme di archi e un vertice di partenza denominato S, calcolare il numero minimo di archi da aggiungere per far sì che ogni nodo del grafo sia raggiungibile a partire da S.

Il problema viene risolto tramite l'individuazione delle componenti fortemente connesse (SCC - Strongly Connected Components) del grafo iniziale, aggiungendo tante connessioni quante sono le SCC con grado entrante uguale a 0.

Specifica:
Scrivere un programma Haskell e un programma Prolog che legga da file e stampi a schermo i seguenti dati di input:
- Una lista di numeri interi (vertici grafo)
- Una lista di coppie di numeri interi (archi grafo)
Successivamente il programma dovrà:
- costruire un grafo orientato
- acquisire da tastiera un vertice di partenza
- costruire un nuovo grafo orientato in cui i vertici sono le componenti fortemente connesse rappresentate da un singolo vertice
  scelto arbitrariamente tra gli vertici di una componente fortemente connessa e gli archi
  rappresentano i collegamenti tra le componenti fortemente connesse distinte
- stampare a schermo il numero di componenti fortemente connesse diverse dalla
  componente contenente l'aeroporto di partenza che hanno grado entrante uguale a 0 (ossia il numero di archi da aggiungere per rendere tutto il grafo raggiungibile).
