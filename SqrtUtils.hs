{-# OPTIONS_GHC -O2 #-}
module SqrtUtils (
  doMain
  ,ComputeDecimals
  ,newton
  ,sqrtFloor
  ,iterations
  ,uStart
  ,vStart
  ,nextU
  ,nextV
  ,Env(..)
)
where

import Text.Read (readMaybe)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)

data Env = Env {expBase :: Int
               ,baseSqrt :: Integer
               ,squareBase :: Integer
               }

type ComputeDecimals = Env -> Int -> Integer -> [Integer]

doMain :: ComputeDecimals -> IO ()
doMain rac = do
    e <- fmap processArguments getArgs
    case e of
        Right (nb, p, base) -> putStrLn (racine nb p base rac)
        Left s       -> usage s

usage :: String -> IO ()
usage message = do
    progName <- getProgName
    putStrLn (progName ++ ": " ++ message)
    putStrLn ("usage: " ++ progName ++ " n [p [base]]")
    putStrLn  "     where n, p and base are integers"
    putStrLn  "     n is the number of which we compute the square root"
    putStrLn  "     p is the number of decimals, default to 30"
    putStrLn  "     base is the granularity, default to 10"
    exitFailure


racine ::  Integer -> Int -> Int -> ComputeDecimals -> String
racine nb p base rac = toS (rac env p nb)
   where
     toS = toString base
     env = Env {expBase = base
               ,baseSqrt = (10::Integer) ^ toInteger base
               ,squareBase = baseSqrt env ^ (2::Integer)
               }

-- fonction qui calcule la partie entière de la racine carrée d'un nombre
-- entier
sqrtFloor :: Integer -> Integer
sqrtFloor nb
    | nb < 2    = nb
    | otherwise = bisection 1 nb 
    where
        bisection inf sup 
            | inf == sup-1   = inf
            | mid * mid > nb = bisection inf mid
            | otherwise      = bisection mid sup
              where mid = (inf + sup) `div` 2

iterations :: Int -> Int -> Int
iterations e p
 | p `mod` e == 0 = q
 | otherwise      = 1 + q
   where q = p `div` e

newton :: Integer -> Integer -> Integer
newton u v = go k0 (nextK k0)
  where
    k0 = 1 + v `div` u
    nextK k = (k*k + v) `div` (2*k + u)
    go k kplus
      | k == kplus = k
      | otherwise  = go kplus (nextK kplus)

uStart :: Integer -> Integer -> Integer
uStart b f = 2 * f * b

vStart :: Integer -> Integer -> Integer -> Integer
vStart sq nb f = (nb - f * f) * sq

nextU :: Integer -> Integer -> Integer -> Integer
nextU b k u = (2*k + u) * b

nextV :: Integer -> Integer -> Integer -> Integer -> Integer
nextV sq k u v = (v - k * (k + u)) * sq

-- pour convertir la liste resultat de racine en une chaine
toString :: Int -> [Integer] -> String
toString _    []      = ""
toString _    [k]     = show k
toString base (k0:ks) = show k0 ++ "." ++ concatMap format ks
    where
     format k = let s = show k
                    delta = base - length s
                in replicate delta '0' ++ s

-- fonctions auxiliaires pour le traitement de la CLI
tryParseAnIntegral :: (Show a, Read a, Integral a) => String -> Either String a
tryParseAnIntegral s =
    case readMaybe s of
        Just n
          | show n /=  s -> Left ("Argument `" ++ s ++ "' is too large")
          | n < 0        -> Left ("Argument `" ++ s ++ "' should not be negative")
          | otherwise    -> Right n
        Nothing          -> Left ("Argument `" ++ s ++ "' is not a valid Integral")

-- j'utilise l'aspect monadique de (Either String a) 
processArguments :: [String] -> Either String (Integer, Int, Int)
processArguments [s] = do
   n <- tryParseAnIntegral s :: Either String Integer
   return (n, 30, 10)

processArguments [sn, sp] = do
     n <- tryParseAnIntegral sn :: Either String Integer
     p <- tryParseAnIntegral sp :: Either String Int
     return (n, p, 10)

processArguments [sn, sp, sbase] = do
  n <- tryParseAnIntegral sn :: Either String Integer
  p <- tryParseAnIntegral sp :: Either String Int
  base <- either
            Left
            notZero
            (tryParseAnIntegral sbase :: Either String Int)
  return (n, p, base)
    where notZero b
            | b == 0    = Left "The granularity must be positive"
            | otherwise = Right b
  
processArguments [] = Left "Not enough arguments"
processArguments _  = Left "Too many arguments"
