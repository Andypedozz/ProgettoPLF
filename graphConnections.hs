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

type GrafoDir a = ([a], [(a, a)])

grafoDir :: (Eq a) => GrafoDir a -> Bool
grafoDir (vs, as) = controlla_v vs && controlla_a as vs
    where
        controlla_v [] = True
        controlla_v (v : vs) = notElem v vs && controlla_v vs
        controlla_a [] _ = True
        controlla_a ((v1, v2) : as) vs = elem v1 vs && elem v2 vs &&
                                         notElem (v1, v2) as && controlla_a as vs

adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as

cercaGrafoDirAmp :: (Eq a) => a -> GrafoDir a -> a -> Bool
cercaGrafoDirAmp v (vs, as) i = elem i vs && cerca v [i] as [] 'a'

cercaGrafoDirProf :: (Eq a) => a -> GrafoDir a -> a -> Bool
cercaGrafoDirProf v (vs, as) i = elem i vs && cerca v [i] as [] 'p'

cerca :: (Eq a) => a -> [a] -> [(a, a)] -> [a] -> Char -> Bool
cerca _ [] _ _ _ = False
cerca v (u : us) as vis t | v == u = True
                          | v /= u = cerca' v (u : us) as vis t
    where
        cerca' v (u : us) as vis t | elem u vis = cerca v us as vis t
                                   | otherwise  = cerca'' v u us as vis t
        cerca'' v u us as vis t | t == 'a' = cerca v (us ++ adiac u as) as (u : vis) t
                                | t == 'p' = cerca v (adiac u as ++ us) as (u : vis) t

trasposto :: (Eq a) => GrafoDir a -> GrafoDir a
trasposto (vs, as) = (vs, [(v2, v1) | (v1, v2) <- as])

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

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

generaGrafoOrientato

acquisisciVerticePartenza

generaGrafoComponentiFortementeConnesse

calcolaNumeroComponentiNonRaggiungibili


main :: IO()
main = do

