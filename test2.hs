-- ##################################################################
-- #         Corso di Programmazione Logica e Funzionale            #
-- #        Progetto per la sessione autunnale A.A 2024/2025        #
-- #                        di Andrea Pedini                        #
-- #                       Matricola: 322918                        #
-- #                       e Matteo Fraternali                      #
-- #                      Matricola: en m arcord                    #
-- #                       Anno di corso: terzo                     #
-- ##################################################################

{-
    Specifica: Scrivere un programma Haskell e un programma Prolog che:
    - acquisiscano da file una lista di numeri interi e una lista di coppie di numeri interi
    - generino un grafo orientato con i dati acquisiti da file
    - acquisiscano un numero intero tra i vertici del grafo
    - generino un nuovo grafo i cui vertici sono le componenti fortemente connesse del grafo di partenza
    - calcolino e stampino a schermo il numero di componenti fortemente connesse con grado entrante uguale a 0
      diverse dalla componente contenente il vertice di partenza
-}

module Main where
import Data.Char (isSpace)
import Data.List (nub)

type GrafoDir a = ([a], [(a, a)])
 
grafoDir :: (Eq a) => GrafoDir a -> Bool
grafoDir (vs, as) = controlla_v vs && controlla_a as vs
    where
        controlla_v [] = True
        controlla_v (v : vs) = notElem v vs && controlla_v vs
        controlla_a [] _ = True
        controlla_a ((v1, v2) : as) vs = elem v1 vs && elem v2 vs &&
                                         notElem (v1, v2) as && controlla_a as vs

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

leggiDatiDaFile :: FilePath -> IO ([Int], [(Int,Int)])
leggiDatiDaFile path = do
  content <- readFile path
  let ls = filter (not . null) . map (trim . takeWhile (/= '\r')) $ lines content
  case ls of
    (l1:l2:_) -> do
      let vs = read l1 :: [Int]
      let as = read l2 :: [(Int,Int)]
      pure (vs, as)
    _ -> error "File malformato: servono due righe ([Int] e [(Int,Int)])."

-- Funzione per ottenere i vertici adiacenti a un vertice
adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as

-- params: 
dfs :: (Eq a) => [a] -> [(a,a)] -> [a] -> [a] -> [a]
dfs [] _ _ stack = stack
dfs (u:us) as visited stack
    | u `elem` visited = dfs us as visited stack
    | otherwise = dfs vicini as visited' stack ++ [u]
        where
            visited' = u : visited
            vicini = adiac u as

-- params: edges => reversedEdges
invertiArchi :: (Eq a) => [(a, a)] -> [(a, a)]
invertiArchi [] = []
invertiArchi ((u,v) : es) = (v,u) : invertiArchi es

-- DFS che raccoglie un'intera componente
-- params: stack, edges, visitedGlobal, visitedLocal => visitedLocal aggiornato
dfsSCC :: (Eq a) => [a] -> [(a,a)] -> [a] -> [a] -> [a]
dfsSCC [] _ _ visitedLocal = visitedLocal
dfsSCC (u:us) as visitedGlobal visitedLocal
    | u `elem` visitedLocal = dfsSCC us as visitedGlobal visitedLocal
    | u `elem` visitedGlobal = dfsSCC us as visitedGlobal visitedLocal
    | otherwise =
        dfsSCC (vicini ++ us) as visitedGlobal (visitedLocal ++ [u])
  where
    vicini = adiac u as

-- Calcolo delle SCCs
getSCCs :: (Eq a) => [a] -> [(a,a)] -> [a] -> [[a]]
getSCCs [] _ _ = []
getSCCs (u:us) as visitedGlobal
    | u `elem` visitedGlobal = getSCCs us as visitedGlobal
    | otherwise =
        scc : getSCCs us as visitedGlobal'
  where
    scc         = dfsSCC [u] as visitedGlobal []
    visitedGlobal' = visitedGlobal ++ scc

-- Dato un vertice, restituisce l'indice della SCC che lo contiene
findSCCIndex :: (Eq a) => a -> [[a]] -> Int
findSCCIndex v sccs = head [i | (i,comp) <- zip [0..] sccs, v `elem` comp]

-- Costruzione del grafo compresso
compressGraph :: (Eq a) => [[a]] -> [(a,a)] -> [(Int,Int)]
compressGraph sccs edges =
    nub [(i,j) |
        (u,v) <- edges,
        let i = findSCCIndex u sccs,
        let j = findSCCIndex v sccs,
        i /= j]

-- Legge un vertice da tastiera
readStartVertex :: IO Int
readStartVertex = do
    putStrLn "Inserisci il vertice di partenza:"
    input <- getLine
    let v = read input :: Int
    return v

main :: IO ()
main = do
    let vs = [0,1,2,3,4,5,6,7]
    let as = [(0,1),(1,2),(2,0),(2,3),(3,4),(4,5),(4,6),(4,7),(5,6),(6,7)]
    let startingNode = 0
    putStrLn "Grafo iniziale:"
    putStrLn $ "Nodo di partenza: " ++ show startingNode
    let ordineDiCompletamento  = dfs [startingNode] as [] []
    putStrLn "Ordine di completamento della DFS:"
    print ordineDiCompletamento
    let archiInvertiti = invertiArchi as
    putStrLn "Grafo con archi invertiti:"
    print archiInvertiti
    let sccs = getSCCs (reverse ordineDiCompletamento) archiInvertiti []
    putStrLn "Componenti fortemente connesse:"
    print sccs
    let sccsGraph = compressGraph sccs as
    putStrLn "Grafo delle componenti fortemente connesse:"
    print sccsGraph

-- acquisisciVerticePartenza

-- calcolaNumeroComponentiNonRaggiungibili