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

type Grafo a = (Eq a) => (a, [(a, a)])

--------------------------------------------------
-- DATI
--------------------------------------------------

grafo :: ([Integer], [(Integer, Integer)])
grafo = (
    [1, 2, 3, 4, 5],   -- Nodi
    [(1, 2), (2, 3), (3, 4), (4, 5), (5, 1)], -- Archi
    1)  -- Nodo di partenza

--------------------------------------------------
-- MAIN
--------------------------------------------------

main :: IO ()
main = do

--------------------------------------------------
-- FUNZIONI DI GRAFO
--------------------------------------------------

adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as



