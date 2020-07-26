-- Le 31 mai 2018, calcul de racine cubique par la méthode des sécantes.
-- Plutôt que d'utiliser la formule d'itération :
-- x(n+1) = x(n) - f(x(n)) * (x(n) - x(n-1))/(f(x(n)) - f(x(n-1)))
-- où f(x) = x^3 - val, val étant le nombre dont on cherche une
-- approximation de la racine cubique.
-- Je construis une suite de « droites » et je calcul les intersections
-- comme si je le dessinais.
{-# OPTIONS_GHC -O2 #-}
module Main where

-- import           Data.Either        (lefts, rights)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           Text.Read          (readMaybe)

-- Pour revenir à des Double il suffit de commenter la ligne
-- suivante et de mettre : type MyFloat = Double
import           Data.Number.BigFloat

-- C'était une bonne idée, mais ça ne marche pas car dans
-- le module Data.Number.Fixed la fonction eps n'est pas
-- exportée : peut-être à recompiler en exportant cette
-- fonction et aussi prec500 (qui est défini mais non exporté).
-- Pour avoir 100 décimales ajouter :
--      {-# LANGUAGE EmptyDataDecls #-}
-- avant le module Main en haut. Ajouter :
--      import Data.Number.Fixed
-- Puis définir un nouveau type :
--      data Prec100
-- le déclarer instance de Epsilon :
--      instance Epsilon Prec100 where
--          eps _ = 1e-100

type MyFloat = BigFloat Prec50

toMyFloat :: Real a => a -> MyFloat
toMyFloat = realToFrac


main :: IO ()
main = do
      e <- fmap getValues getArgs
      case e of
          Right (val, x1, x2, iter) -> printResult $ cubeRoot val x1 x2 iter
          Left s                    -> usage s


printResult :: MyFloat -> IO ()
printResult r =
    putStrLn ("Root: "
              <> show r
              <> "\n"
              <> "Cube: "
              <> show (cube r)
             )


usage :: String -> IO ()
usage s = do
    progName <- getProgName
    putStrLn (progName <> ": " <> s)
    putStrLn ("Usage: " <> progName <> ": value xA xB iter")
    putStrLn ("       " <> progName <> " compute the cube root of `value'")
    putStrLn  "       [xA, xB] define the interval where to iterate"
    putStrLn  "       and must satisfy: xA^3 < value < xB^3"
    putStrLn  "       iter is the number of iterations"
    exitFailure


cubeRoot :: Double -> Double -> Double -> Int -> MyFloat
cubeRoot val x1 x2 iter =
    let x2'  = toMyFloat x2
        val' = toMyFloat val
        x1'  = toMyFloat x1
        d0   = droite x1' x2'
        d    = droites val' x2' d0 !! (iter-1)
    in
        xInter d val'


cube :: RealFrac a =>  a -> a
cube x = x ^ (3 :: Int)

-- droite sous la forme y = a*x + b
-- dir représente a et ord représente b
data Droite = Droite { dir::MyFloat , ord::MyFloat }

instance Show Droite where
    showsPrec _ (Droite d y) = showString ("Droite " <> show d <> " " <> show y)

-- Construit une droite qui passe par les points :
--    (x1, x1^3) et (x2, x2^3)
droite ::  MyFloat -> MyFloat -> Droite
droite x1 x2 = Droite k yd
    where k = (cube x2 - cube x1) / (x2 - x1)
          yd = cube x1 - k * x1


{--- fournit f(x0) pour une droite (soit l'intersection d'une droite avec-}
{--- la droite d'équation x = x0)-}
{-yInter :: Droite -> MyFloat -> MyFloat-}
{-yInter (Droite k yd) x0 =  k*x0 + yd-}

-- fournit l'abcisse de l'intersection d'une droite quelconque
-- avec une droite // à l'axe des x de coordonnée y = y0
xInter :: Droite -> MyFloat -> MyFloat
xInter (Droite k yd) y0 = (y0 - yd) / k

-- Création d'une liste de droites. Pour chaque itération on utilise la
-- valeur de l'intersection de la droite précédente avec la droite y=val
-- val correspond au cube de la racine cubique que l'on cherche à approcher.
-- L'ensemble des droites calculées partage le point supérieur initiale (x2, x2^3)
-- la conséquence est que plus la racine est proche de x2, plus la convergence est
-- rapide.
-- Par exemple pour rechercher la racine cubique de 5 qui est comprise entre 1 et 2
--      ghci> let d0 = droite 1 2
--      ghci> let l = droites 5 2 d0
--      ghci> let d = l !! 10
--      ghci> xInter d 5
--      1.7099759457140111
--      ghci> cube $ xInter d 5
--      4.999999991555268
--      ghci> let d = l !! 20
--      ghci> xInter d 5
--      1.709975946676697
--      ghci> cube $ xInter d 5
--      5.000000000000001
--      …
-- Cette méthode est très sensible à l'initialisation.
-- mais converge vite quand l'initialisation est optimisée.
droites :: MyFloat -> MyFloat -> Droite -> [Droite]
droites val upper  = iterate f
    where
        f d = droite (xInter d val) upper


-- Fonctions auxiliaires pour obtenir et vérifier les arguments de la cli
getValues :: [String] -> Either String (Double, Double, Double, Int)
getValues args
  | lg == 4          = either Left r (getValues' args)
  | lg >=0 && lg < 4 = Left "Not enough arguments"
  | otherwise        = Left "Too many arguments"
  where
    lg = length args
    r e@(v, x1, x2, _) =
      if cube x1 < v && v < cube x2
        then Right e
        else Left ("Condition: `"
                     <> show (cube x1)
                     <> " < "
                     <> show v
                     <> " < "
                     <> show (cube x2)
                     <> "' is false")

getValues' :: [String] -> Either String (Double, Double, Double, Int) 
getValues' [sv, sx1, sx2, siter] = do
  v <- tryParseDouble sv
  x1 <- tryParseDouble sx1
  x2 <- tryParseDouble sx2
  iter <- tryParseInt siter
  return (v, x1, x2, iter)

tryParseDouble :: String -> Either String Double
tryParseDouble s = case readMaybe s :: Maybe Double of
        Just d  -> Right d
        Nothing -> Left "Argument must be a Double"

tryParseInt :: String -> Either String Int
tryParseInt s = case readMaybe s :: Maybe Int of
        Just n  | s /= show n   -> Left ("Parameter for iter: `" <> s <> "' is too large to be an Int")
                | n <= 0        -> Left "The number of iteration must be a positive Int"
                | otherwise     -> Right n
        _                       -> Left ("Parameter for iter `" <> s <> "' isn't an Int")

