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

trovaSCC :: (Eq a) => GrafoDir a -> [[a]]
