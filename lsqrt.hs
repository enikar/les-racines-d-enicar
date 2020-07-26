-- le lundi 17 décembre 2018.
-- Comme nsqrt.hs mais en utilisant des listes.
-- Pour compiler : ghc -O2 lsqrt.hs
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

-- La fonction qui calcule les décimales
rac :: ComputeDecimals
rac env p0 nb
  | p == 0 || f * f == nb = [f]
  | otherwise             = f : take p lk
    where
      b = baseSqrt env
      sq = squareBase env
      p = iterations (expBase env) p0
      f = sqrtFloor nb 

      u0 = uStart b f
      v0 = vStart sq nb f

      lk = zipWith newton lu lv
      lu = u0 : zipWith  luCal lk lu
      lv = v0 : zipWith3 lvCal lk lu lv

      luCal = nextU b
      lvCal = nextV sq
