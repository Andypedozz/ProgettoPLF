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
{-# OPTIONS_GHC -Wno-tabs #-}

module Main where
import Data.Char (isSpace)
import Text.Read (readMaybe)

{- Dato per l-}
type GrafoDir a = ([a], [(a, a)])

{- Funzione principale del programma -}

main :: IO ()
main = do
    let vs = [0,1,2,3,4,5,6,7]
    let as = [(0,1),(1,2),(2,0),(2,3),(3,4),(4,5),(4,7),(5,6),(6,4),(6,7)]
    let valid = grafoDir (vs, as)
    print valid

    putStrLn "Inserisci un numero intero:"
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just n -> do
            putStrLn ("Hai inserito: " ++ show n)
            let order = visitaInProfondita (vs, as) n
            print order
        Nothing ->
            putStrLn "Errore: non hai inserito un intero valido."


-- Funzione per eliminare spazi dalle stringhe lette
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

-- Funzione per validare il grafo
grafoDir :: (Eq a) => GrafoDir a -> Bool
grafoDir (vs, as) = controlla_v vs && controlla_a as vs
  where
      controlla_v [] = True
      controlla_v (v : vs) = notElem v vs && controlla_v vs
      controlla_a [] _ = True
      controlla_a ((v1, v2) : as) vs = elem v1 vs && elem v2 vs &&
                                       notElem (v1, v2) as && controlla_a as vs

-- Funzione per ottenere i vertici adiacenti a un vertice
adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as

-- Attraversamento in profondità (ritorna lista in ordine di completamento)
visitaInProfondita :: (Eq a) => GrafoDir a -> a -> [a]
visitaInProfondita (vs, as) i
    | i `elem` vs = dfs [i] as [] []
    | otherwise   = []

-- dfs: stack, archi, visitati, completati -> lista completati (post-order)
dfs :: (Eq a) => [a] -> [(a,a)] -> [a] -> [a] -> [a]
dfs [] _ _ finished = finished
dfs (u:us) as visited finished
    | u `elem` visited = dfs us as visited finished
    | otherwise =
        let visited' = u : visited
            vicini = adiac u as
            finished' = dfs vicini as visited' finished
        in dfs us as visited' (finished' ++ [u])
                     -- aggiungo u in post-order

-- Funzione per ottenere il grafo compresso delle componenti fortemente connesse
kosaraju :: GrafoDir a -> GrafoDir a
kosaraju (_, _) = ([], [])

-- Funzione per calcolare il numero minimo di archi da aggiungere
calcolaArchiDaAggiungere :: GrafoDir a -> Int
calcolaArchiDaAggiungere grafo = 0



