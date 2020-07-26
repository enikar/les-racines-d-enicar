-- Le lundi 17 décembre 2018.
-- Toujours la racine mais en utilisant un module pour
-- factoriser les fonctions utilitaires entre nsqrt.hs et lsqrt.hs
-- Pour compiler : ghc -O2 nsqrt.hs
-- Revue le 20 juillet 2020
{-# OPTIONS_GHC -O2 #-}

module Main where

import SqrtUtils (ComputeDecimals
                 ,doMain
                 ,newton
                 ,iterations
                 ,sqrtFloor
                 ,uStart
                 ,vStart
                 ,nextU
                 ,nextV
                 ,Env(..)
                 )

main :: IO ()
main = doMain rac

rac ::  ComputeDecimals
rac env p0 nb
 | p1 == 0 || f * f == nb = [f]
 | otherwise             = f : go p1 u0 v0
  where
    b = baseSqrt env
    sq = squareBase env
    p1 = iterations (expBase env) p0
    f = sqrtFloor nb 

    u0 = uStart b f
    v0 = vStart sq nb f

    uCal = nextU b
    vCal = nextV sq

    go p u v
     | p == 0   = []
     | otherwise = k : go (p - 1) (uCal k u) (vCal k u v)
     where
       k = newton u v
