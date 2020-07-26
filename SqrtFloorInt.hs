module Main where

import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)
import Data.Bits (unsafeShiftR, finiteBitSize, countLeadingZeros, bit)

main :: IO ()
main = do
    e <- fmap getAnInt getArgs
    case e of
        Right n -> print (sqrtFloor n)
        Left err -> do
            progName <- getProgName
            putStrLn (progName ++ ": " ++ err)
            putStrLn ("usage: " ++ progName ++ ": <positive Int>")

-- on suppose que les Int sont des Int32 ou des Int64
-- On utilise le fait que la (partie entière de la ) racine carrée est 
-- presque deux fois moins  large que le nombre original, par exemple en 
-- décimal la racine carrée d'un nombre à deux chiffres 10 <= n < 100
-- est comprise entre 1 et 9. Pour un nombre à trois chiffres :
-- 100 <= n < 1000 la racine comporte 2 chiffre (10 <= sqrtFloor(n) < 31)
-- sqrtFloor va faire deux chiffres pour 100 <= n < 10000
-- En résumé quand la largeur de n est paire, la largeur de sqrtFloor(n)
-- est égale à n/2 ; lorsque la la largeur de n est impaire
-- la largeur de sqrtFloor (n) est égale à ((la largeur de n) + 1) / 2
-- Donc il suffit d'initialiser la borne inférieure sur le nombre
-- le plus petit qui a la largeur de sqrtFloor(n). C'est facile
-- on met juste le bit qui correspond à cette largeur minimale
-- à 1, tous les autres bits sont à zéros.
-- Pour la borne supérieure on prend le minimum entre 
-- 2 * (borne inférieure) et la limite au delà de laquelle 
-- (limit+1)^2 > (maxBound::Int), c'est à dire :
--      limit = sqrtFloor (maxBound :: Int)
-- Sauf que l'on ne peut pas la calculer juste en utilisant des Int…
-- donc je l'ai précalculé en utilisant des Integer
width :: Int
width = finiteBitSize (0::Int)     -- nombre de bits d'un Int : 32 ou 64… 

limit :: Int
limit | width == 32 = 46340        -- limite au delà de laquelle          
      | otherwise   = 3037000499   -- (limit+1)^2 > (maxBound :: Int)    


-- L'initialisation est très délicate, car j'essaye d'encadrer au plus
-- près la partie entière de la racine carrée du nombre tout en tenant
-- compte des limites des Int…
initialize :: Int -> (Int, Int)
initialize n = (inf, sup)
    where
        nWidth = width - countLeadingZeros n  -- largeur du nombre n

        sqrtWidth | even nWidth = nWidth `unsafeShiftR` 1       -- sqrtWidth >= 1
                  | otherwise   = (nWidth + 1) `unsafeShiftR` 1

        inf = bit (sqrtWidth - 1)        -- les bits sont indexés à partir de 0

        sup = min limit (bit sqrtWidth)  -- il faut garantir que sup <= limit


sqrtFloor :: Int -> Int
sqrtFloor n
    | n == 0 || n == 1        = n
    | n < 4                   = 1                     --  n >= 4 ==> width (n) >= 2
    | n >= limit ^ (2::Int)   = limit                 -- (limit+1)^2 > (maxBound::Int)
    | otherwise               = bisection n inf sup   -- 1 < n < limit ^ 2
        where
          (inf, sup) = initialize n                      


mean ::  Int -> Int -> Int
{-# INLINE mean #-}
mean x y = (x+y) `unsafeShiftR` 1 -- `moyenne' de deux nombres 

-- la bonne méthode pour faire la recherche dichotomique.
bisection :: Int -> Int -> Int -> Int
bisection n inf sup
  | inf == sup-1 = inf
  | mid*mid > n  = bisection n inf mid
  | otherwise    = bisection n mid sup
    where mid = mean inf sup
    
getAnInt :: [String] -> Either String Int
getAnInt [s] = case readMaybe s :: Maybe Int of
                    (Just n) | s /= show n  -> Left ("`" ++ s ++ "' is too large to be an Int") 
                             | n < 0        -> Left ("`" ++ s ++ "' isn't a positive Int")
                             | otherwise    -> Right n 
                    Nothing                 -> Left ("`" ++ s ++ "' isn't an Int")
getAnInt [] = Left "Not enough arguments"
getAnInt _  = Left "Too many arguments"

