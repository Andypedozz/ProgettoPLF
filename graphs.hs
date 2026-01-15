-- ##############################################################
-- # Corso di Programmazione Logica e Funzionale                #
-- # Progetto per la sessione autunnale A.A. 2024/2025          #
-- # di Andrea Pedini                                          #
-- # Matricola: 322918                                         #
-- # e Matteo Fraternali                                       #
-- # Anno di corso: terzo                                      #
-- ##############################################################

{- Specifica:
 - acquisire da file una lista di vertici e una lista di archi
 - costruire un grafo orientato
 - calcolare le componenti fortemente connesse
 - costruire il grafo compresso
 - contare le componenti con indegree 0
   diverse da quella contenente un vertice scelto
-}

import Data.List (nub)

--------------------------------------------------
-- TIPI
--------------------------------------------------

type Grafo = ([Int], [(Int, Int)])
type Caso  = ([Int], [(Int, Int)], Int)

--------------------------------------------------
-- DATI
--------------------------------------------------

casi :: [Caso]
casi =
  [ ([1,2,3,4,5],
     [(1,2),(2,3),(3,4),(4,5),(5,1)],
     4)

  , ([1,2,3,4,5,6],
     [(1,2),(2,3),(3,1),(4,5)],
     4)

  , ([1,2,3,4,5,6],
     [(1,2),(1,3),(2,4),(3,4),(4,5),(4,6)],
     2)

  , ([1,2,3,4,5,6,7],
     [(1,2),(2,3),(3,1),(3,4),(4,5),(5,3),(5,6),(6,7)],
     5)

  , ([1,2,3,4,5,6],
     [(1,2),(2,3),(3,3),(4,5)],
     6)

  , ([1,2,3,4,5,6],
     [(1,2),(1,3),(1,4),(1,5),(1,6)],
     3)

  , ([1,2,3,4,5,6,7,8],
     [(1,2),(2,3),(3,1),(4,5),(5,6),(6,4),(6,7),(7,8)],
     8)

  , ([1,2,3,4,5,6,7],
     [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,4)],
     7)

  , ([1,2,3,4,5,6,7,8],
     [(1,2),(2,3),(3,1),(3,4),(4,5),(5,3),(5,6),(6,7),(7,6),(7,8)],
     8)

  , ([1,2,3,4,5,6,7],
     [(1,2),(2,3),(3,1),(2,4),(4,5),(5,6),(6,4),(6,7)],
     6)
  ]

--------------------------------------------------
-- MAIN
--------------------------------------------------

main :: IO ()
main = mapM_ eseguiCaso (zip [1..] casi)

eseguiCaso :: (Int, Caso) -> IO ()
eseguiCaso (i, (vertici, archi, vPartenza)) = do
    putStrLn "======================================"
    putStrLn ("Caso #" ++ show i)
    putStrLn "SCC:"
    let sccs = kosaraju vertici archi
    mapM_ print sccs
    let gc = comprimiGrafo sccs archi
    putStrLn "Grafo compresso:"
    mapM_ print gc
    putStrLn $
        "SCC con indegree 0 (esclusa partenza): "
        ++ show (contaSCCZero vPartenza sccs gc)
    putStrLn ""

--------------------------------------------------
-- FUNZIONI DI GRAFO
--------------------------------------------------

adiacenti :: Int -> [(Int, Int)] -> [Int]
adiacenti v archi = [ y | (x,y) <- archi, x == v ]

invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi = map (\(x,y) -> (y,x))

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
        []    -> error ("Vertice non trovato in nessuna SCC: " ++ show v)

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


