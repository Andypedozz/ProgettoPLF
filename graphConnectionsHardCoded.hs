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

import Data.Char (isSpace)
import Data.List (nub)

{- Tipo per rappresentare un grafo diretto -}
type Grafo = ([Int], [(Int, Int)])

{- Elimina spazi bianchi iniziali e finali -}
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

{- Lettura dei dati da file (non usata nel main di esempio) -}
leggiDatiDaFile :: FilePath -> IO Grafo
leggiDatiDaFile path = do
    contenuto <- readFile path
    let righe = filter (not . null) $
                map (trim . takeWhile (/= '\r')) (lines contenuto)
    processaRighe righe
  where
    processaRighe (l1:l2:_) = return (read l1, read l2)
    processaRighe _ = error "Errore: file malformato"

{- Vertici adiacenti -}
adiacenti :: Int -> [(Int, Int)] -> [Int]
adiacenti _ [] = []
adiacenti v ((x,y):as)
    | v == x    = y : adiacenti v as
    | otherwise = adiacenti v as

{- DFS per ordine di completamento -}
dfs :: [Int] -> [(Int, Int)] -> [Int] -> [Int] -> [Int]
dfs [] _ _ pila = pila
dfs (v:vs) archi visitati pila
    | v `elem` visitati = dfs vs archi visitati pila
    | otherwise =
        dfs vicini archi visitati' pila ++ [v]
  where
    visitati' = v : visitati
    vicini    = adiacenti v archi

{- Inversione archi -}
invertiArchi :: [(Int, Int)] -> [(Int, Int)]
invertiArchi [] = []
invertiArchi ((x,y):as) = (y,x) : invertiArchi as

{- DFS per una singola SCC -}
dfsComponente :: [Int] -> [(Int, Int)] -> [Int] -> [Int] -> [Int]
dfsComponente [] _ _ visitatiLocali = visitatiLocali
dfsComponente (v:vs) archi visitatiGlobali visitatiLocali
    | v `elem` visitatiLocali = dfsComponente vs archi visitatiGlobali visitatiLocali
    | v `elem` visitatiGlobali = dfsComponente vs archi visitatiGlobali visitatiLocali
    | otherwise =
        dfsComponente (vicini ++ vs) archi visitatiGlobali (visitatiLocali ++ [v])
  where
    vicini = adiacenti v archi

{- Calcolo SCC -}
calcolaSCC :: [Int] -> [(Int, Int)] -> [Int] -> [[Int]]
calcolaSCC [] _ _ = []
calcolaSCC (v:vs) archi visitati
    | v `elem` visitati = calcolaSCC vs archi visitati
    | otherwise =
        componente : calcolaSCC vs archi visitati'
  where
    componente = dfsComponente [v] archi visitati []
    visitati'  = visitati ++ componente

{- Algoritmo di Kosaraju -}
kosaraju :: [Int] -> [(Int, Int)] -> [[Int]]
kosaraju vertici archi =
    calcolaSCC (reverse ordine) archiInvertiti []
  where
    ordine         = dfs vertici archi [] []
    archiInvertiti = invertiArchi archi

{- Indice SCC che contiene un vertice -}
indiceSCC :: Int -> [[Int]] -> Int
indiceSCC v sccs =
    head [ i | (i,comp) <- zip [0..] sccs, v `elem` comp ]

{- Costruzione grafo compresso -}
comprimiGrafo :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
comprimiGrafo sccs archi =
    nub [ (i,j)
        | (x,y) <- archi
        , let i = indiceSCC x sccs
        , let j = indiceSCC y sccs
        , i /= j ]

{- Indegree di una componente -}
indegree :: Int -> [(Int, Int)] -> Int
indegree c archi =
    length [ () | (_,y) <- archi, y == c ]

{- Conteggio SCC con indegree 0 -}
contaSCCZero :: Int -> [[Int]] -> [(Int, Int)] -> Int
contaSCCZero v sccs archi =
    length [ i
           | (i,_) <- zip [0..] sccs
           , i /= sccPartenza
           , indegree i archi == 0
           ]
  where
    sccPartenza = indiceSCC v sccs

{- Lettura vertice di partenza -}
leggiVertice :: IO Int
leggiVertice = do
    putStrLn "Inserisci il vertice di partenza:"
    read <$> getLine

--------------------------------------------------
-- FUNZIONI DI STAMPA (visualizzazione migliorata)
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
        putStrLn ("  C" ++ show i ++ " : " ++ show (indegree i archi))

--------------------------------------------------
-- MAIN
--------------------------------------------------

main :: IO ()
main = do
    (vertici, archi) <- leggiDatiDaFile "input.txt"

    stampaLista "Grafo iniziale (archi):" archi

    let sccs = kosaraju vertici archi
    stampaSCC sccs

    let grafoCompresso = comprimiGrafo sccs archi
    stampaGrafoCompresso grafoCompresso

    stampaIndegree (length sccs) grafoCompresso

    v <- leggiVertice
    let risultato = contaSCCZero v sccs grafoCompresso

    putStrLn $
        "Numero di SCC con indegree 0 (esclusa la componente contenente "
        ++ show v ++ "): " ++ show risultato
