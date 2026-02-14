-- ##################################################################
-- #         Corso di Programmazione Logica e Funzionale            #
-- #        Progetto per la sessione invernale A.A 2025/2026        #
-- #                        di Andrea Pedini                        #
-- #                       Matricola: 322918                        #
-- #                       e Matteo Fraternali                      #
-- #                       Matricola: 316637                        #
-- ##################################################################

{-
    Scrivere un programma Haskell e un programma Prolog che leggano da file e stampino a schermo i seguenti dati di input:
    * Un insieme di numeri interi
    * Una insieme di coppie di numeri interi
    Successivamente il programma dovrà:
    * Costruire un grafo orientato i cui vertici sono costituiti dalla lista di interi letta
      da file e gli archi sono rappresentati dalle coppie di interi letti da file.
    * Costruire un nuovo grafo orientato in cui i vertici sono le componenti
      fortemente connesse del grafo iniziale rappresentate da un singolo vertice
      scelto arbitrariamente tra i vertici di una componente fortemente connessa e
      gli archi sono i collegamenti tra le componenti fortemente connesse distinte.
    * Acquisire un numero intero tra i vertici del grafo a rappresentare il vertice di
      partenza e stampare a schermo il numero di componenti fortemente
      connesse diverse dalla componente contenente il vertice di partenza che
      hanno grado entrante uguale a 0.
-}

module Main where

-- Import della funzione nub per eliminare duplicati da una lista
import Data.List ( nub )

--------------------------------------------------
-- FUNZIONE PRINCIPALE
--------------------------------------------------

{- 
    Azione principale del programma.

    1) Legge un grafo orientato dal file "input.txt".
    2) Verifica che il formato e il contenuto del grafo siano validi.
    3) Calcola le Componenti Fortemente Connesse (SCC) con Kosaraju.
    4) Costruisce il grafo compresso (ogni SCC diventa un nodo).
    5) Richiede all'utente un vertice valido di partenza.
    6) Conta quante SCC hanno grado entrante zero,
       escludendo la SCC contenente il vertice scelto.

    Se il file contiene errori il programma termina in modo controllato.
-}
main :: IO ()
main = do
    risultato <- leggiGrafoDaFile "input.txt"

    case risultato of
        Nothing -> do
            putStrLn "\nIl programma è stato terminato a causa di errori nel file di input.\n"

        Just (vertici, archi) -> do

            putStrLn "\n======================================"
            putStrLn "            GRAFO LETTO DA FILE       "
            putStrLn "--------------------------------------"
            putStrLn $ "Vertici: " ++ show vertici
            putStrLn $ "Archi:   " ++ show archi

            let componenti = kosaraju vertici archi

            putStrLn "\n======================================"
            putStrLn "       COMPONENTI FORTEMENTE CONNESSE "
            putStrLn "--------------------------------------"
            mapM_
                (\(i, c) -> putStrLn $ "SCC " ++ show i ++ ": " ++ show c)
                (zip [0 ..] componenti)

            let grafoCompresso = comprimiGrafo componenti archi

            putStrLn "\n======================================"
            putStrLn "           GRAFO COMPRESSO             "
            putStrLn "--------------------------------------"
            mapM_
                (\(i, j) -> putStrLn $ "SCC_" ++ show i ++ " -> SCC_" ++ show j)
                grafoCompresso

            verticePartenza <- acquisisciVertice vertici

            let numeroZero =
                    contaSCCConGradoZero verticePartenza componenti grafoCompresso

            putStrLn "\n======================================"
            putStrLn $
                "Numero di SCC con grado entrante 0 (esclusa la partenza): "
                ++ show numeroZero
            putStrLn "======================================\n"

--------------------------------------------------
-- LETTURA E VALIDAZIONE DEL GRAFO DA FILE
--------------------------------------------------

{- 
    Funzione che legge e valida un grafo orientato da file.

    Parametri:
    - nomeFile : percorso del file da leggere

    Il file deve contenere almeno due righe:
    1) Lista dei vertici  (es: [1,2,3])
    2) Lista degli archi  (es: [(1,2),(2,3)])

    Restituisce:
    - Just (vertici, archi) se parsing e validazione hanno successo
    - Nothing se si verifica un errore
-}
leggiGrafoDaFile :: FilePath -> IO (Maybe ([Int], [(Int, Int)]))
leggiGrafoDaFile nomeFile = do
    contenuto <- readFile nomeFile
    case lines contenuto of
        vLine:aLine:_ -> parseGrafo vLine aLine
        _ -> errore "Errore: il file deve contenere almeno due righe (lista vertici e lista archi)."

{- 
    Funzione che esegue il parsing testuale delle due righe lette dal file.

    Parametri:
    - rVertici : stringa contenente la lista dei vertici
    - rArchi   : stringa contenente la lista degli archi

    Usa 'reads' per convertire le stringhe nei tipi Haskell corretti.
-}
parseGrafo :: String -> String -> IO (Maybe ([Int], [(Int, Int)]))
parseGrafo rVertici rArchi =
    case (reads rVertici, reads rArchi) of
        ([(vertici, "")], [(archi, "")]) ->
            validaGrafo vertici archi
        _ ->
            errore "Errore: formato non valido. Verificare parentesi, virgole e struttura delle liste."

{- 
    Funzione che verifica la validità semantica del grafo.

    Parametri:
    - vertici : lista dei vertici del grafo
    - archi   : lista degli archi orientati del grafo

    Controlli effettuati:
    - lista vertici non vuota
    - assenza di vertici duplicati
    - archi che usano solo vertici esistenti
-}
validaGrafo :: [Int] -> [(Int, Int)] -> IO (Maybe ([Int], [(Int, Int)]))
validaGrafo vertici archi
    | null vertici =
        errore "Errore: la lista dei vertici è vuota. Il grafo deve contenere almeno un vertice."
    | not (verticiSenzaDuplicati vertici) =
        errore "Errore: la lista dei vertici contiene duplicati. Ogni vertice deve comparire una sola volta."
    | not (archiValidi vertici archi) =
        errore "Errore: esistono archi che utilizzano vertici non presenti nella lista dei vertici."
    | otherwise =
        return (Just (vertici, archi))

{- 
    Funzione che verifica l'assenza di vertici duplicati.

    Parametri:
    - vertici : lista dei vertici del grafo

    Restituisce:
    - True se tutti i vertici sono distinti
    - False se esiste almeno un duplicato
-}
verticiSenzaDuplicati :: [Int] -> Bool
verticiSenzaDuplicati vertici =
    length vertici == length (nub vertici)

{- 
    Funzione che controlla che ogni arco utilizzi solo vertici esistenti.

    Parametri:
    - vertici : lista dei vertici ammessi
    - archi   : lista degli archi del grafo
-}
archiValidi :: [Int] -> [(Int, Int)] -> Bool
archiValidi vertici =
    all (\(x, y) -> x `elem` vertici && y `elem` vertici)

{- 
    Funzione che stampa un messaggio di errore e restituisce Nothing.
-}
errore :: String -> IO (Maybe a)
errore msg = putStrLn msg >> return Nothing

--------------------------------------------------
-- FUNZIONE DI ACQUISIZIONE DEL VERTICE
--------------------------------------------------

{- 
    Funzione che acquisisce da tastiera un vertice valido.

    Parametri:
    - vertici : lista dei vertici del grafo

    Continua a chiedere input finché l'utente non inserisce
    un intero presente nella lista dei vertici.
-}
acquisisciVertice :: [Int] -> IO Int
acquisisciVertice vertici = do
    putStrLn $
        "Inserisci il vertice di partenza (tra " ++ show vertici ++ "):"
    input <- getLine
    case reads input :: [(Int, String)] of
        [(v, _)] | v `elem` vertici -> return v
        _ -> do
            putStrLn "Vertice non valido: inserire un numero presente nella lista dei vertici."
            acquisisciVertice vertici

--------------------------------------------------
-- FUNZIONI DI BASE SUI GRAFI
--------------------------------------------------

{- 
    Funzione che restituisce tutti i vertici adiacenti ad un vertice dato.
    Parametri:
    - v     : vertice di partenza
    - archi : lista degli archi del grafo
-}
verticiAdiacenti :: Int -> [(Int, Int)] -> [Int]
verticiAdiacenti v archi = [y | (x, y) <- archi, x == v]

{- 
    Funzione che inverte la direzione di tutti gli archi del grafo.
    Parametri:
    - archi : lista degli archi del grafo

    Restituisce:
    - archi invertiti
-}
invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi [] = []
invertiArchi ((x, y) : xs) = (y, x) : invertiArchi xs

--------------------------------------------------------
-- FUNZIONI DI COMBINAZIONE PER LA VISITA IN PROFONDITA'
--------------------------------------------------

{-
    Funzione che inserisce il vertice in coda (post-order).
    Parametri:
    - vertice
    - risultato

    Restituisce:
    - risultato aggiornato
-}
aggiungiInCoda :: Int -> [Int] -> [Int]
aggiungiInCoda v res = res ++ [v]

{-
    Funzione che inserisce il vertice in test (pre-order).
    Parametri:
    - vertice
    - risultato

    Restituisce:
    - risultato aggiornato
-}
aggiungiInTesta :: Int -> [Int] -> [Int]
aggiungiInTesta v res = v : res

--------------------------------------------------
-- VISITA IN PROFONDITA' GENERICA
--------------------------------------------------

{- 
    Funzione che implementa una visita in profondità generica parametrizzata.

    Parametri:
    - combina  : funzione di combinazione dei risultati
    - v        : vertice corrente
    - archi    : lista archi
    - visitati : lista vertici già visitati
-}
visitaInProfondita :: (Int -> [Int] -> [Int]) -> Int -> [(Int, Int)] -> [Int] -> ([Int], [Int])
visitaInProfondita combina v archi visitati
    | v `elem` visitati = (visitati , [])
    | otherwise =
        let visitati' = v : visitati
            (visitatiFinali, risultatiFigli) =
                foldl visita (visitati', []) (verticiAdiacenti v archi)
        in (visitatiFinali, combina v risultatiFigli)
  where
    visita (vis, res) u =
        let (vis', res') = visitaInProfondita combina u archi vis
        in (vis', res ++ res')

--------------------------------------------------
-- VISITA SPECIALIZZATE
--------------------------------------------------

{- 
    Funzione che implementa la visita in profondità
    per il calcolo dell'ordine di fine.
    Parametri:
    - vertice di partenza
    - archi
    - visitati

    Restituisce:
    - visitati
    - ordine di fine
-}
visitaInProfonditaOrdineFine :: Int -> [(Int, Int)] -> [Int] -> ([Int], [Int])
visitaInProfonditaOrdineFine = visitaInProfondita aggiungiInCoda

{-
    Funzione che implementa la visita in profondità per
    la costruzione delle componenti fortemente connesse.
    Parametri:
    - vertice di partenza
    - archi
    - visitati

    Restituisce:
    - visitati
    - scc
-}
visitaInProfonditaComponente :: Int -> [(Int, Int)] -> [Int] -> ([Int], [Int])
visitaInProfonditaComponente = visitaInProfondita aggiungiInTesta

--------------------------------------------------
-- ORDINE DI FINE
--------------------------------------------------

{- 
    Funzione che calcola l'ordine di fine globale del grafo.
    Parametri:
    - vertici
    - archi

    Restituisce:
    - ordine
-}
ordineDiFine :: [Int] -> [(Int, Int)] -> [Int]
ordineDiFine vertici archi =
    snd $
        foldl visitaGlobale ([], []) vertici
  where
    visitaGlobale (vis, ord) v =
        let (vis', ord') = visitaInProfonditaOrdineFine v archi vis
        in (vis', ord ++ ord')

--------------------------------------------------
-- KOSARAJU
--------------------------------------------------

{- 
    Funzione che implementa l'algoritmo di Kosaraju
    per componenti fortemente connesse.
    Parametri:
    - vertici
    - archi

    Restituisce:
    - componenti
-}
kosaraju :: [Int] -> [(Int, Int)] -> [[Int]]
kosaraju vertici archi =
    componenti
  where
    ordine         = reverse (ordineDiFine vertici archi)
    archiInvertiti = invertiArchi archi
    componenti =
        snd $
            foldl costruisci ([], []) ordine

    costruisci (vis, comps) v
        | v `elem` vis = (vis, comps)
        | otherwise =
            let (vis', comp) = visitaInProfonditaComponente v archiInvertiti vis
            in (vis', comps ++ [comp])

--------------------------------------------------
-- GRAFO COMPRESSO
--------------------------------------------------

{-
    Funzione che restituisce l'indice della componente
    fortemente connessa contenente un vertice.
    Parametri:
    - vertice
    - componenti

    Restituisce:
    - indice
-}
indiceSCC :: Int -> [[Int]] -> Int
indiceSCC v sccs =
    case [i | (i, comp) <- zip [0 ..] sccs , v `elem` comp] of
        (i:_) -> i
        []        -> error ("Errore interno: vertice non trovato in alcuna SCC: " ++ show v)

{-
    Funzione che costruisce il grafo compresso
    delle componenti fortemente connesse.
    Parametri:
    - componenti
    - archi

    Restituisce:
    - archi
-}
comprimiGrafo :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
comprimiGrafo sccs archi =
    nub
        [ (i, j)
        | (x, y) <- archi
        , let i = indiceSCC x sccs
        , let j = indiceSCC y sccs
        , i /= j
        ]

{-
    Funzioen che calcola il grado entrante di
    una componente fortemente connessa.
    Parametri:
    - indice
    - archi

    Restituisce:
    - grado
-}
gradoEntrante :: Int -> [(Int, Int)] -> Int
gradoEntrante c archi =
    length [() | (_, y) <- archi , y == c]

{-
    Funzione che conta le componenti fortemente connesse
    con grado entrante zero (esclusa quella di partenza).
    Parametri:
    - vertice di partenza
    - componenti
    - archi

    Restituisce:
    - numero
-}
contaSCCConGradoZero :: Int -> [[Int]] -> [(Int, Int)] -> Int
contaSCCConGradoZero v sccs archi =
    length
        [ i
        | i <- [0 .. length sccs - 1]
        , i /= indiceSCC v sccs
        , gradoEntrante i archi == 0
        ]