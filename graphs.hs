-- ##############################################################
-- # Corso di Programmazione Logica e Funzionale                #
-- # Progetto per la sessione autunnale A.A. 2024/2025          #
-- # di Andrea Pedini                                          #
-- # Matricola: 322918                                         #
-- # e Matteo Fraternali                                       #
-- # Anno di corso: terzo                                      #
-- ##############################################################

import Data.List (nub)

--------------------------------------------------
-- TIPI
--------------------------------------------------

type Grafo = ([Int], [(Int, Int)])
type Caso  = ([Int], [(Int, Int)], Int)
-- (vertici, archi, vertice di partenza)

--------------------------------------------------
-- DATI HARDCODED
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
main = mapM_ (uncurry eseguiCaso) (zip [1..] casi)

--------------------------------------------------
-- ESECUZIONE DI UN CASO
--------------------------------------------------

eseguiCaso :: Int -> Caso -> IO ()
eseguiCaso idx (vertici, archi, vPartenza) = do
    putStrLn "======================================"
    putStrLn ("Caso #" ++ show idx)

    stampaLista "Grafo iniziale (archi):" archi

    let sccs = kosaraju vertici archi
    stampaSCC sccs

    let grafoCompresso = comprimiGrafo sccs archi
    stampaGrafoCompresso grafoCompresso

    stampaIndegree (length sccs) grafoCompresso

    let risultato = contaSCCZero vPartenza sccs grafoCompresso

    putStrLn $
        "Numero di SCC con indegree 0 (esclusa la componente contenente "
        ++ show vPartenza ++ "): " ++ show risultato
    putStrLn ""

--------------------------------------------------
-- FUNZIONI GRAFO
--------------------------------------------------

adiacenti :: Int -> [(Int, Int)] -> [Int]
adiacenti _ [] = []
adiacenti v ((x,y):as)
    | v == x    = y : adiacenti v as
    | otherwise = adiacenti v as

dfs :: [Int] -> [(Int, Int)] -> [Int] -> [Int] -> [Int]
dfs [] _ _ pila = pila
dfs (v:vs) archi visitati pila
    | v `elem` visitati = dfs vs archi visitati pila
    | otherwise =
        dfs vicini archi visitati' pila ++ [v]
  where
    visitati' = v : visitati
    vicini    = adiacenti v archi

invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi = map (\(x,y) -> (y,x))

dfsComponente :: [Int] -> [(Int, Int)] -> [Int] -> [Int] -> [Int]
dfsComponente [] _ _ visitatiLocali = visitatiLocali
dfsComponente (v:vs) archi visitatiGlobali visitatiLocali
    | v `elem` visitatiLocali = dfsComponente vs archi visitatiGlobali visitatiLocali
    | v `elem` visitatiGlobali = dfsComponente vs archi visitatiGlobali visitatiLocali
    | otherwise =
        dfsComponente (vicini ++ vs) archi visitatiGlobali (visitatiLocali ++ [v])
  where
    vicini = adiacenti v archi

calcolaSCC :: [Int] -> [(Int, Int)] -> [Int] -> [[Int]]
calcolaSCC [] _ _ = []
calcolaSCC (v:vs) archi visitati
    | v `elem` visitati = calcolaSCC vs archi visitati
    | otherwise =
        componente : calcolaSCC vs archi visitati'
  where
    componente = dfsComponente [v] archi visitati []
    visitati'  = visitati ++ componente

kosaraju :: [Int] -> [(Int, Int)] -> [[Int]]
kosaraju vertici archi =
    sccs ++ map (:[]) verticiNonUsati
  where
    ordine         = dfs vertici archi [] []
    archiInvertiti = invertiArchi archi
    sccs           = calcolaSCC (reverse ordine) archiInvertiti []
    verticiUsati   = concat sccs
    verticiNonUsati = [ v | v <- vertici, v `notElem` verticiUsati ]


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
    nub [ (i,j)
        | (x,y) <- archi
        , let i = indiceSCC x sccs
        , let j = indiceSCC y sccs
        , i /= j
        ]

gradoEntrante :: Int -> [(Int, Int)] -> Int
gradoEntrante c archi =
    length [ () | (_,y) <- archi, y == c ]

contaSCCZero :: Int -> [[Int]] -> [(Int, Int)] -> Int
contaSCCZero v sccs archi =
    length [ i
           | (i,_) <- zip [0..] sccs
           , i /= sccPartenza
           , gradoEntrante i archi == 0
           ]
  where
    sccPartenza = indiceSCC v sccs

--------------------------------------------------
-- FUNZIONI DI STAMPA
--------------------------------------------------

stampaLista :: Show a => String -> [a] -> IO ()
stampaLista titolo xs = do
    putStrLn titolo
    mapM_ (\x -> putStrLn ("  - " ++ show x)) xs
    putStrLn ""

stampaSCC :: [[Int]] -> IO ()
stampaSCC sccs = do
    putStrLn "Componenti fortemente connesse:"
    mapM_ stampa (zip [0..] sccs)
    putStrLn ""
  where
    stampa (i,comp) =
        putStrLn ("  C" ++ show i ++ " = " ++ show comp)

stampaGrafoCompresso :: [(Int,Int)] -> IO ()
stampaGrafoCompresso archi = do
    putStrLn "Grafo compresso (tra componenti):"
    mapM_ stampaArco archi
    putStrLn ""
  where
    stampaArco (i,j) =
        putStrLn ("  C" ++ show i ++ " -> C" ++ show j)

stampaIndegree :: Int -> [(Int,Int)] -> IO ()
stampaIndegree n archi = do
    putStrLn "Indegree delle componenti:"
    mapM_ stampa [0..n-1]
    putStrLn ""
  where
    stampa i =
        putStrLn ("  C" ++ show i ++ " : " ++ show (gradoEntrante i archi))
