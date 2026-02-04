-- ##################################################################
-- #         Corso di Programmazione Logica e Funzionale            #
-- #        Progetto per la sessione invernale A.A 2025/2026        #
-- #                        di Andrea Pedini                        #
-- #                       Matricola: 322918                        #
-- #                       e Matteo Fraternali                      #
-- #                       Matricola: 316637                        #
-- ##################################################################

{-
    Specifica : Scrivere un programma Haskell che legga un grafo orientato
    da file e calcoli le sue Componenti Fortemente Connesse (SCC)
    utilizzando l'algoritmo di Kosaraju.
    Successivamente il programma costruisce il grafo compresso
    e determina il numero di SCC con grado entrante zero,
    escludendo la SCC contenente un vertice scelto dall'utente.
-}

module Main where

-- Caricamento della funzione per eliminare duplicati da una lista
import Data.List ( nub )

--------------------------------------------------
-- FUNZIONE PRINCIPALE
--------------------------------------------------

{- 
    Funzione principale del programma.
    - legge un grafo da file
    - calcola le componenti fortemente connesse
    - costruisce il grafo compresso
    - calcola quante SCC hanno grado entrante zero
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
    Legge e valida un grafo da file.
    Restituisce:
    - Just (vertici, archi) se il parsing e la validazione hanno successo
    - Nothing in caso di errore
-}

leggiGrafoDaFile :: FilePath -> IO (Maybe ([Int], [(Int, Int)]))
leggiGrafoDaFile nomeFile = do
    contenuto <- readFile nomeFile
    case lines contenuto of
        vLine:aLine:_ -> parseGrafo vLine aLine
        _ -> errore "Errore: il file deve contenere almeno due righe."

-- Parsing delle due righe
parseGrafo :: String -> String -> IO (Maybe ([Int], [(Int, Int)]))
parseGrafo rVertici rArchi =
    case (reads rVertici, reads rArchi) of
        ([(vertici, "")], [(archi, "")]) ->
            validaGrafo vertici archi
        _ ->
            errore "Errore: formato non valido delle liste (parentesi o virgole errate)."

-- Validazione semantica
validaGrafo :: [Int] -> [(Int, Int)] -> IO (Maybe ([Int], [(Int, Int)]))
validaGrafo vertici archi
    | null vertici =
        errore "Errore: la lista dei vertici è vuota."
    | not (archiValidi vertici archi) =
        errore "Errore: alcuni archi contengono vertici non presenti nella lista."
    | otherwise =
        return (Just (vertici, archi))

-- Controllo archi
archiValidi :: [Int] -> [(Int, Int)] -> Bool
archiValidi vertici =
    all (\(x, y) -> x `elem` vertici && y `elem` vertici)

-- Utility per errori
errore :: String -> IO (Maybe a)
errore msg = putStrLn msg >> return Nothing

--------------------------------------------------
-- FUNZIONE DI ACQUISIZIONE DEL VERTICE
--------------------------------------------------

{- 
    Funzione che acquisisce da tastiera un vertice valido.
    - l'argomento è la lista dei vertici del grafo
-}
acquisisciVertice :: [Int] -> IO Int
acquisisciVertice vertici = do
    putStrLn $
        "Inserisci il vertice di partenza (tra " ++ show vertici ++ "):"
    input <- getLine
    case reads input :: [(Int, String)] of
        [(v, _)] | v `elem` vertici -> return v
        _ -> do
            putStrLn "Vertice non valido! Riprova."
            acquisisciVertice vertici

--------------------------------------------------
-- FUNZIONI DI BASE SUI GRAFI
--------------------------------------------------

{- 
    Funzione che restituisce tutti i vertici adiacenti a un vertice.
    - il primo argomento è il vertice di partenza
    - il secondo argomento è la lista degli archi
-}
verticiAdiacenti :: Int -> [(Int, Int)] -> [Int]
verticiAdiacenti v archi = [y | (x, y) <- archi, x == v]

{- 
    Funzione che inverte la direzione di tutti gli archi del grafo.
    - l'argomento è la lista degli archi
-}
invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi [] = []
invertiArchi ((x, y) : xs) = (y, x) : invertiArchi xs

--------------------------------------------------------
-- FUNZIONI DI COMBINAZIONE PER LA VISITA IN PROFONDITA'
--------------------------------------------------------

{- 
    Funzione di combinazione che inserisce il vertice
    alla fine della lista dei risultati (post-order).
-}
aggiungiInCoda :: Int -> [Int] -> [Int]
aggiungiInCoda v res = res ++ [v]

{- 
    Funzione di combinazione che inserisce il vertice
    all'inizio della lista dei risultati (pre-order).
-}
aggiungiInTesta :: Int -> [Int] -> [Int]
aggiungiInTesta v res = v : res

--------------------------------------------------
-- VISITA IN PROFONDITA' GENERICA
--------------------------------------------------

{- 
    Visita in profondità generica parametrizzata da una funzione di combinazione.
    - il primo argomento decide come aggiungere il vertice corrente
    - il secondo argomento è il vertice di partenza
    - il terzo argomento è la lista degli archi
    - il quarto argomento è la lista dei vertici visitati
    
    Restituisce:
    - la lista aggiornata dei vertici visitati
    - la lista dei risultati accumulati secondo la strategia scelta
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
-- VISITA IN PROFONDITA' SPECIALIZZATE
--------------------------------------------------

{- 
    Visita in profondità che calcola l'ordine di fine dei vertici.
-}
visitaInProfonditaOrdineFine :: Int -> [(Int, Int)] -> [Int] -> ([Int], [Int])
visitaInProfonditaOrdineFine = visitaInProfondita aggiungiInCoda

{- 
    Visita in profondità che costruisce una singola componente fortemente connessa.
-}
visitaInProfonditaComponente :: Int -> [(Int, Int)] -> [Int] -> ([Int], [Int])
visitaInProfonditaComponente = visitaInProfondita aggiungiInTesta

--------------------------------------------------
-- ORDINE DI FINE GLOBALE
--------------------------------------------------

{- 
    Funzione che calcola l'ordine di fine globale del grafo.
    - il primo argomento è la lista dei vertici
    - il secondo argomento è la lista degli archi
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
-- ALGORITMO DI KOSARAJU
--------------------------------------------------

{- 
    Funzione che calcola tutte le componenti fortemente connesse del grafo
    utilizzando l'algoritmo di Kosaraju.
    - il primo argomento è la lista dei vertici
    - il secondo argomento è la lista degli archi
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
    Funzione che restituisce l'indice della SCC contenente un vertice.
    - il primo argomento è il vertice
    - il secondo argomento è la lista delle SCC
-}
indiceSCC :: Int -> [[Int]] -> Int
indiceSCC v sccs =
    case [i | (i, comp) <- zip [0 ..] sccs , v `elem` comp] of
        (i:_) -> i
        []        -> error ("Vertice non trovato: " ++ show v)

{- 
    Funzione che costruisce il grafo compresso.
    - il primo argomento sono le SCC
    - il secondo argomento è la lista degli archi originali
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
    Funzione che calcola il grado entrante di una SCC.
    - il primo argomento è l'indice della SCC
    - il secondo argomento è la lista degli archi del grafo compresso
-}
gradoEntrante :: Int -> [(Int, Int)] -> Int
gradoEntrante c archi =
    length [() | (_, y) <- archi , y == c]

{- 
    Funzione che conta quante SCC hanno grado entrante zero,
    escludendo quella contenente il vertice di partenza.
    - il primo argomento è il vertice di partenza
    - il secondo argomento sono le SCC
    - il terzo argomento è il grafo compresso
-}
contaSCCConGradoZero :: Int -> [[Int]] -> [(Int, Int)] -> Int
contaSCCConGradoZero v sccs archi =
    length
        [ i
        | i <- [0 .. length sccs - 1]
        , i /= indiceSCC v sccs
        , gradoEntrante i archi == 0
        ]