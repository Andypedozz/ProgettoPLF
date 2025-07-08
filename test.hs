{- Definizioni globali -}
{- dall'alto verso il basso -}
circonferenza :: Float
circonferenza = pi * diametro

diametro :: Float
diametro = 2 * raggio

raggio :: Float
raggio = sqrt 15.196

{- dal basso verso l'alto -}
raggio :: Float
raggio = sqrt 15.196

diametro :: Float
diametro = 2 * raggio

circonferenza :: Float
circonferenza = pi * diametro

-- Definizioni globali con definizioni locali
circonferenza :: Float
circonferenza = pi * diametro
    where
        diametro = 2 * raggio
        raggio = sqrt 15.196

-- oppure
circonferenza :: Float
circonferenza = let
                    raggio = sqrt 15.196
                    diametro = 2 * raggio
                in
                    pi * diametro

signum x = if (x > 0)
            then 1
            else
                if (x == 0)
                    then 0
                    else - 1

signum x | x > 0    = 1
         | x == 0   = 0
         | x < 0    = -1

signum x | x > 0    = 1
         | x == 0   = 0
         | otherwise    = -1

fattoriale :: Int -> Int
fattoriale 0    = 1
fattoriale n | n > 0 = n * fattoriale (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)

disg :: Bool -> Bool -> Bool
disg True _ = True
disg False x = x

cong :: Bool -> Bool -> Bool
cong False _ = False
cong True x = x

cond :: Bool -> a -> a -> a
cond True e _ = e
cond False _ e = e

confronta :: Char -> Char -> Int
confronta c c = 0
confronta c1 c2 | c1 < c2 = -1
                | c1 > c2 = 1

confronta :: Char -> Char -> Int
confronta c1 c2 | c1 == c2 = 0
                | c1 > c2 = 1
                | c1 < c2 = -1

-- Liste
lunghezza :: [a] -> Int
lunghezza [] = 0
lunghezza (_ : xs) = 1 + lunghezza xs

testa :: [a] -> a
testa (x : _) = x

coda :: [a] -> [a]
coda (_ : xs) = xs

membro :: (Eq a) => a -> [a] -> Bool
membro _ [] = False
membro x (y : ys) | x == y = True
                  | otherwise = membro x ys

prefisso :: (Eq a) => [a] -> [a] -> Bool
prefisso [] _ = True
prefisso (_ : _) [] = False
prefisso (x : xs) (y : ys) | x == y = prefisso xs ys
                           | otherwise = False
