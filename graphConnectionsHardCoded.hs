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

-- Caricamento delle funzioni necessarie per le operazioni con caratteri e liste
import Data.Char (isSpace)
import Data.List (nub)

{- Dato per rappresentare un Grafo Diretto -} 
type GrafoDir a = ([a], [(a, a)])
 
{- Funzione che verifica se un grafo è ben strutturato -}
grafoDir :: (Eq a) => GrafoDir a -> Bool
grafoDir (vs, as) = controlla_v vs && controlla_a as vs
    where
        controlla_v [] = True
        controlla_v (v : vs) = notElem v vs && controlla_v vs
        controlla_a [] _ = True
        controlla_a ((v1, v2) : as) vs = elem v1 vs && elem v2 vs &&
                                         notElem (v1, v2) as && controlla_a as vs
{- Funzione -}
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

{- Funzione che legge i dati del grafo da un file di testo -}
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

{- Funzione per ottenere i vertici adiacenti a un vertice -}
adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as

{- Funzione che effettua l'attraversamento in profondità del grafo
   e restituisce l'ordine di completamento -}
dfs :: (Eq a) => [a] -> [(a,a)] -> [a] -> [a] -> [a]
dfs [] _ _ stack = stack
dfs (u:us) as visited stack
    | u `elem` visited = dfs us as visited stack
    | otherwise = dfs vicini as visited' stack ++ [u]
        where
            visited' = u : visited
            vicini = adiac u as

{- Funzione che dato un grafo restituisce il grafo trasposto (archi invertiti) -}
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
acquisisciVerticePartenza :: IO Int
acquisisciVerticePartenza = do
    putStrLn "Inserisci il vertice di partenza:"
    input <- getLine
    let v = read input :: Int
    return v

kosaraju :: (Eq a) => [a] -> [(a, a)] -> [[a]]
kosaraju vs as = sccs
  where
    -- Passo 1: DFS per ottenere l'ordine di completamento
    ordineDiCompletamento = dfs vs as [] []
    -- Passo 2: Invertire il grafo
    archiInvertiti = invertiArchi as
    -- Passo 3: DFS sulle componenti fortemente connesse
    sccs = getSCCs (reverse ordineDiCompletamento) archiInvertiti []

-- Calcola l'indegree di ogni componente
calcolaIndegree :: (Eq a) => a -> [(a, a)] -> Int
calcolaIndegree comp archi =
    length [() | (_,j) <- archi, j == comp]

-- Conta le SCC con indegree = 0 diverse da quella che contiene il nodo di partenza
calcolaComponentiConIndegreeZero :: (Eq a) => a -> [[a]] -> [(Int,Int)] -> Int
calcolaComponentiConIndegreeZero start sccs compressedEdges =
    length [ comp
           | (i, comp) <- zip [0..] sccs
           , i /= startSCC
           , calcolaIndegree i compressedEdges == 0
           ]
  where
    startSCC = findSCCIndex start sccs

main :: IO ()
main = do
    let vs = [0,1,2,3,4,5,6,7]
    let as = [(0,1),(1,2),(2,0),(2,3),(3,4),(4,5),(4,6),(5,6),(6,7)]
    putStrLn "Grafo iniziale:"
    print as
    let sccs = kosaraju vs as
    putStrLn "Componenti fortemente connesse:"
    print sccs
    let sccsGraph = compressGraph sccs as
    putStrLn "Grafo delle componenti fortemente connesse:"
    print sccsGraph
    start <- acquisisciVerticePartenza
    let count = calcolaComponentiConIndegreeZero start sccs sccsGraph
    putStrLn $ "Numero di SCC con indegree = 0 diverse da quella contenente " ++ show start ++ ": " ++ show count
