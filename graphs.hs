-- ##############################################################
-- # Corso di Programmazione Logica e Funzionale                #
-- # Progetto per la sessione autunnale A.A. 2024/2025          #
-- ##############################################################

import Data.List (nub)

--------------------------------------------------
-- TIPI
--------------------------------------------------

type Grafo = ([Int], [(Int, Int)])

--------------------------------------------------
-- MAIN
--------------------------------------------------

main :: IO ()
main = do
    -- Lettura del grafo da file
    contenuto <- readFile "input.txt"
    let righe = lines contenuto
        vertici = read (head righe) :: [Int]
        archi   = read (righe !! 1) :: [(Int,Int)]

    -- Acquisizione del vertice di partenza
    vPartenza <- acquisisciVertice vertici

    putStrLn "======================================"
    putStrLn "Grafo letto da file"
    putStrLn "SCC:"
    let sccs = kosaraju vertici archi
    mapM_ print sccs

    let gc = comprimiGrafo sccs archi
    putStrLn "Grafo compresso:"
    mapM_ print gc

    putStrLn $
        "SCC con indegree 0 (esclusa partenza): "
        ++ show (contaSCCZero vPartenza sccs gc)

--------------------------------------------------
-- FUNZIONE DI ACQUISIZIONE VERTICE
--------------------------------------------------

acquisisciVertice :: [Int] -> IO Int
acquisisciVertice vertici = do
    putStrLn $ "Inserisci il vertice di partenza (tra " ++ show vertici ++ "):"
    input <- getLine
    case reads input :: [(Int, String)] of
        [(v,_)] | v `elem` vertici -> return v
        _ -> do
            putStrLn "Vertice non valido! Riprova."
            acquisisciVertice vertici

--------------------------------------------------
-- FUNZIONI DI GRAFO
--------------------------------------------------

adiacenti :: Int -> [(Int, Int)] -> [Int]
adiacenti v archi = [ y | (x,y) <- archi, x == v ]

invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi [] = []
invertiArchi ((x, y) : xs) = (y, x) : invertiArchi xs

--------------------------------------------------
-- DFS PER ORDINE DI FINE (KOSARAJU 1)
--------------------------------------------------

dfsOrdine :: Int -> [(Int,Int)] -> [Int] -> ([Int], [Int])
dfsOrdine v archi visitati
    | v `elem` visitati = (visitati, [])
    | otherwise =
        let visitati' = v : visitati
            (visitatiFinali, ordineFigli) =
                foldl visita (visitati', []) (adiacenti v archi)
        in (visitatiFinali, ordineFigli ++ [v])
  where
    visita (vis, ord) u =
        let (vis', ord') = dfsOrdine u archi vis
        in (vis', ord ++ ord')

ordineDiFine :: [Int] -> [(Int,Int)] -> [Int]
ordineDiFine vertici archi =
    snd $
      foldl visitaGlobale ([], []) vertici
  where
    visitaGlobale (vis, ord) v =
        let (vis', ord') = dfsOrdine v archi vis
        in (vis', ord ++ ord')

--------------------------------------------------
-- DFS COMPONENTE (KOSARAJU 2)
--------------------------------------------------

dfsComponente :: Int -> [(Int,Int)] -> [Int] -> ([Int], [Int])
dfsComponente v archi visitati
    | v `elem` visitati = (visitati, [])
    | otherwise =
        let visitati' = v : visitati
            (visitatiFinali, compFigli) =
                foldl visita (visitati', []) (adiacenti v archi)
        in (visitatiFinali, v : compFigli)
  where
    visita (vis, comp) u =
        let (vis', comp') = dfsComponente u archi vis
        in (vis', comp ++ comp')

--------------------------------------------------
-- KOSARAJU COMPLETO
--------------------------------------------------

kosaraju :: [Int] -> [(Int,Int)] -> [[Int]]
kosaraju vertici archi =
    scc
  where
    ordine         = reverse (ordineDiFine vertici archi)
    archiInvertiti = invertiArchi archi
    scc            = snd $
        foldl costruisci ([], []) ordine

    costruisci (vis, comps) v
        | v `elem` vis = (vis, comps)
        | otherwise =
            let (vis', comp) = dfsComponente v archiInvertiti vis
            in (vis', comps ++ [comp])

--------------------------------------------------
-- GRAFO COMPRESSO
--------------------------------------------------

indiceSCC :: Int -> [[Int]] -> Int
indiceSCC v sccs =
    case [ i | (i,comp) <- zip [0..] sccs, v `elem` comp ] of
        (i:_) -> i
        []    -> error ("Vertice non trovato: " ++ show v)

comprimiGrafo :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
comprimiGrafo sccs archi =
    nub
      [ (i,j)
      | (x,y) <- archi
      , let i = indiceSCC x sccs
      , let j = indiceSCC y sccs
      , i /= j
      ]

gradoEntrante :: Int -> [(Int,Int)] -> Int
gradoEntrante c archi =
    length [ () | (_,y) <- archi, y == c ]

contaSCCZero :: Int -> [[Int]] -> [(Int,Int)] -> Int
contaSCCZero v sccs archi =
    length
      [ i
      | i <- [0..length sccs - 1]
      , i /= indiceSCC v sccs
      , gradoEntrante i archi == 0
      ]
