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

-- Compila con:   ghc -O2 Main.hs
-- Esegui con:    ./Main input.txt
-- Formato file:  riga1 = [Int], riga2 = [(Int,Int)]
-- Usa i datatype/funzioni dati nel testo (copiati qui sotto).

import System.Environment (getArgs)
import System.IO
import Data.Char (isSpace)
import Data.List (nub, sort, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ======= Datatype e funzioni pre-esistenti =======
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

-- ======= Utilità =======
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

readTwoLines :: FilePath -> IO ([Int], [(Int,Int)])
readTwoLines path = do
  content <- readFile path
  let ls = filter (not . null) . map (trim . takeWhile (/= '\r')) $ lines content
  case ls of
    (l1:l2:_) -> do
      let vs = read l1 :: [Int]
      let as = read l2 :: [(Int,Int)]
      pure (vs, as)
    _ -> error "File malformato: servono due righe ([Int] e [(Int,Int)])."

-- ======= Kosaraju: SCC =======
-- Passo 1: ordine di fine visita (post-ordine) su G
postOrder :: (Ord a, Eq a) => GrafoDir a -> [a]
postOrder (vs, as) = reverse (goAll vs S.empty [])
  where
    goAll [] _ acc = acc
    goAll (v:rest) vis acc
      | v `S.member` vis = goAll rest vis acc
      | otherwise        = let (vis', stack) = dfs v vis
                           in goAll rest vis' (stack ++ acc)

    dfs v vis0 =
      let (vis1, stackChildren) =
            foldl' (\(vis, acc) u ->
                       if u `S.member` vis then (vis, acc)
                       else let (vis', st') = dfs u vis in (vis', st' ++ acc))
                   (S.insert v vis0, [])
                   (adiac v as)
      in (vis1, stackChildren ++ [v])

-- Passo 2: DFS su G^T seguendo l'ordine del passo 1
sccKosaraju :: (Ord a, Eq a) => GrafoDir a -> [[a]]
sccKosaraju g@(vs, as) =
  let order = postOrder g
      (_, asT) = trasposto g
  in collect order S.empty []
  where
    (_, asT) = trasposto g
    collect [] _ comps = comps
    collect (v:rest) vis comps
      | v `S.member` vis = collect rest vis comps
      | otherwise        =
          let (vis', comp) = dfsT v vis []
          in collect rest vis' (comp:comps)

    dfsT v vis acc =
      let vis' = S.insert v vis
          nexts = adiac v asT
          (visFinal, accFinal) =
            foldl' (\(vi, ac) u ->
                      if u `S.member` vi then (vi, ac)
                      else dfsT u vi ac)
                   (vis', acc) nexts
      in (visFinal, v:acc)

-- ======= Condensazione e conteggi =======
-- Rappresentante di una SCC: il primo elemento della lista
repMap :: (Ord a) => [[a]] -> M.Map a a
repMap sccs = M.fromList [(v, head comp) | comp <- sccs, v <- comp]

condensationEdges :: (Ord a, Eq a) => GrafoDir a -> [[a]] -> S.Set (a,a)
condensationEdges (vs, as) sccs =
  let rmap = repMap sccs
      toRep x = rmap M.! x
      edges = [ (toRep u, toRep v) | (u,v) <- as, toRep u /= toRep v ]
  in S.fromList edges

indegreeZeroCountExcept :: (Ord a, Eq a) => a -> GrafoDir a -> [[a]] -> Int
indegreeZeroCountExcept start g@(vs, as) sccs =
  let reps = map head sccs
      rset = S.fromList reps
      rmap = repMap sccs
      rStart = case M.lookup start rmap of
                 Nothing -> error "Il vertice di partenza non è nei vertici del grafo."
                 Just r  -> r
      ced = condensationEdges g sccs
      indeg = M.fromListWith (+) [ (v, 0) | v <- reps ] `M.union`
              M.fromListWith (+) [ (v, 1) | (_, v) <- S.toList ced ]
      zeroIn = [ r | r <- reps, M.findWithDefault 0 r indeg == 0, r /= rStart ]
  in length zeroIn

-- ======= Main =======
main :: IO ()
main = do
  args <- getArgs
  whenErr (null args) "Uso: ./Main <fileInput>"
  let path = head args
  (vs, as) <- readTwoLines path
  putStrLn "Vertici:"
  print vs
  putStrLn "Archi (orientati):"
  print as
  let g = (vs, as) :: GrafoDir Int
  whenErr (not (grafoDir g)) "Input non valido: vertici/archi duplicati o archi con vertici inesistenti."
  putStr "Inserisci il vertice di partenza: "
  hFlush stdout
  ln <- getLine
  let start = read (trim ln) :: Int
  whenErr (start `notElem` vs) "Il vertice di partenza non è nei vertici del grafo."
  let sccs = sccKosaraju g
  putStrLn $ "SCC trovate: " ++ show sccs
  let k = indegreeZeroCountExcept start g sccs
  putStrLn $ "Numero di componenti (diverse da quella di partenza) con grado entrante 0: " ++ show k

-- piccola utilità
whenErr :: Bool -> String -> IO ()
whenErr cond msg = if cond then ioError (userError msg) else pure ()
