-- ##################################################################
-- #         Corso di Programmazione Logica e Funzionale            #
-- #        Progetto per la sessione autunnale A.A 2024/2025        #
-- #                        di Andrea Pedini                        #
-- #                       Matricola: 322918                        #
-- #                       e Matteo Fraternali                      #
-- #                       Matricola: en m arcord                   #
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

type GrafoDir a = ([a], [(a, a)])

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- Funzione per leggere i dati del grafo da file
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

-- Attraversamento in profondità (ritorna lista in ordine di completamento)
visitaInProfondita :: (Eq a) => GrafoDir a -> a -> [a]
visitaInProfondita (vs, as) i =
    if i `elem` vs
        then dfs [i] as []
        else []

-- DFS che costruisce lista di completamento
dfs :: (Eq a) => [a] -> [(a, a)] -> [a] -> [a]
dfs [] _ vis = vis
dfs (u:us) as vis
    | u `elem` vis = dfs us as vis
    | otherwise =
        let vicini = adiac u as
            visConVicini = dfs vicini as vis
            visFinale = dfs us as visConVicini
        in visFinale ++ [u]


-- Funzione per ottenere il grafo compresso delle componenti fortemente connesse
kosaraju :: GrafoDir a -> GrafoDir a
kosaraju grafo = ([], [])

-- Funzione per calcolare il numero minimo di archi da aggiungere
calcolaArchiDaAggiungere :: GrafoDir a -> Int
calcolaArchiDaAggiungere grafo = 0

main :: IO()
main = do
  (vs, as) <- leggiDatiDaFile "input.txt"
  let vicini = adiac 4 as
  print vicini


