type GrafoDir a = ([a], [(a, a)])

grafo_dir :: (Eq a) => GrafoDir a -> Bool
grafo_dir (vs, as) = controlla_v vs && controlla_a as vs
    where
        controlla_v [] = True
        controlla_v (v : vs) = not (L.membro v vs) && controlla_v vs
        controlla_a [] _ = True
        controlla_a ((v1, v2) : as) vs = L.membro v1 vs && L.membro v2 vs &&
                                            not (L.membro (v1, v2) as) && controlla_a as vs

adiac :: (Eq a) => a -> [(a, a)] -> [a]
adiac _ [] = []
adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                        | v /= v1 = adiac v as

-- Attraversamento in ampiezza
cerca_grafo_dir_amp :: (Eq a) => a -> GrafoDir a -> a -> Bool
cerca_grafo_dir_amp v (vs, as) i = L.membro i vs && cerca v [i] as [] ’a’

-- Attraversamento in profondità
cerca_grafo_dir_prof :: (Eq a) => a -> GrafoDir a -> a -> Bool
cerca_grafo_dir_prof v (vs, as) i = L.membro i vs && cerca v [i] as [] ’p’

-- Ricerca vertice
cerca :: (Eq a) => a -> [a] -> [(a, a)] -> [a] -> Char -> Bool
cerca _ [] _ _ _ = False
cerca v (u : us) as vis t | v == u = True
                            | v /= u = cerca’ v (u : us) as vis t
    where
        cerca’ v (u : us) as vis t | L.membro u vis = cerca v us as vis t
                                    | otherwise = cerca’’ v u us as vis t
        cerca’’ v u us as vis t | t == ’a’ = cerca v (us ++ adiac u as) as (u : vis) t
                                | t == ’p’ = cerca v (adiac u as ++ us) as (u : vis) t

-- Genera trasposto di un Grafo dato
trasposto :: (Eq a) => GrafoDir a -> GrafoDir a
trasposto (vs, as) = (vs, [ (v2, v1) | (v1, v2) <- as ])

dfsPostOrder :: (Eq a) => GrafoDir a -> [a] -> [a] -> [a]
dfsPostOrder _ [] vis = vis
dfsPostOrder g@(vs, as) (x:xs) vis
    | L.membro x vis = dfsPostOrder g xs vis
    | otherwise = let vis' = dfsPostOrder g (adiac x as) vis
                  in dfsPostOrder g xs (x : vis')

dfsComponenti :: (Eq a) => GrafoDir a -> [a] -> [a] -> ([a], [a])
dfsComponenti _ [] vis = ([], vis)
dfsComponenti g@(vs, as) (x:xs) vis
    | L.membro x vis = dfsComponenti g xs vis
    | otherwise = let comp = esplora x as vis
                      vis' = comp ++ vis
                  in (comp, vis')
    where
        esplora x as vis
            | L.membro x vis = []
            | otherwise = x : concat [ esplora y as (x : vis) | y <- adiac x as ]

scc :: (Eq a) => GrafoDir a -> [[a]]
scc g@(vs, as) =
    let ordine = reverse (dfsPostOrder g vs [])
        gT = trasposto g
    in sccAux gT ordine [] []

sccAux :: (Eq a) => GrafoDir a -> [a] -> [a] -> [[a]] -> [[a]]
sccAux _ [] _ acc = acc
sccAux g@(vs, as) (x:xs) vis acc
    | L.membro x vis = sccAux g xs vis acc
    | otherwise =
        let (comp, vis') = dfsComponenti g [x] vis
        in sccAux g xs vis' (comp : acc)

type SCCGrafo a = ([[a]], [(Int, Int)])  -- nodi: liste di nodi originali, archi: tra indici

buildSCCGraph :: (Eq a) => GrafoDir a -> [[a]] -> SCCGrafo a
buildSCCGraph (vs, as) comps =
    let compMap = [ (v, i) | (i, comp) <- zip [0..] comps, v <- comp ]
        archiSCC = [ (i, j) |
                     (v1, v2) <- as,
                     Just i <- [lookup v1 compMap],
                     Just j <- [lookup v2 compMap],
                     i /= j ]
        archiSCCUnici = rimuoviDuplicati archiSCC
    in (comps, archiSCCUnici)


rimuoviDuplicati :: (Eq a) => [a] -> [a]
rimuoviDuplicati [] = []
rimuoviDuplicati (x:xs)
    | x `elem` xs = rimuoviDuplicati xs
    | otherwise = x : rimuoviDuplicati xs

-- Funzione per contare le SCC con grado entrante 0 diverse da Start
sccZeroInDegreeExceptStart :: (Eq a) => GrafoDir a -> a -> Int
sccZeroInDegreeExceptStart g@(vs, as) start =
    let comps = scc g
        sccG@(nodiSCC, archiSCC) = buildSCCGraph g comps
        compMap = [ (v, i) | (i, comp) <- zip [0..] comps, v <- comp ]
        Just startComp = lookup start compMap
        tutti = [0..(length comps - 1)]
        entranti = [ j | (_, j) <- archiSCC ]
        senzaEntranti = filter (`notElem` entranti) tutti
        soloAltri = filter (/= startComp) senzaEntranti
    in length soloAltri

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = lines contents
        nodi = read (head ls) :: [Int]
        archi = read (ls !! 1) :: [(Int, Int)]
        start = read (ls !! 2) :: Int
        g = (nodi, archi)
    print $ sccZeroInDegreeExceptStart g start

