{-# LANGUAGE OverloadedStrings #-}
module Measures where

import Control.DeepSeq
import Criterion.Main

import Formatting
import Formatting.Clock
import System.Clock

import Winograd
import WinogradMatrix


fastTest8 :: IO ()
fastTest8 = do
  let a = matrixCtor 2 (-1) 500
  let b = matrixCtor 2 (-3) 500
  start <- a `deepseq` b `deepseq` getTime Realtime
  let
    c = winograd8 a b
  end <- c `deepseq` getTime Realtime
  writeFile "8" (show c)
  fprint (timeSpecs % "\n") start end


fastTest9 :: IO ()
fastTest9 = do
  let a = matrixCtor 2 (-1) 500
  let b = matrixCtor 2 (-3) 500
  start <- a `deepseq` b `deepseq` getTime Realtime
  let
    c = winograd9 a b
  end <- c `deepseq` getTime Realtime
  writeFile "9" (show c)
  fprint (timeSpecs % "\n") start end



allBench :: IO ()
allBench = do
  let
    a  = list2Ctor  2 1
    b  = list2Ctor  2 3
    am = matrixCtor 2 1
    bm = matrixCtor 2 3
    evenl = 1000
    oddl  = 1001
  defaultMain [
    bgroup "mult" [
         bgroup "wgd1even"  [ bench (show x) $ nf (uncurry winograd1) (a x, b x)   | x <- [100,200..evenl] ]
       , bgroup "wgd2even"  [ bench (show x) $ nf (uncurry winograd2) (a x, b x)   | x <- [100,200..evenl] ]
       , bgroup "wgd8even"  [ bench (show x) $ nf (uncurry winograd8) (am x, bm x) | x <- [100,200..evenl] ]
       , bgroup "wgd9even"  [ bench (show x) $ nf (uncurry winograd9) (am x, bm x) | x <- [100,200..evenl] ]
       --, bgroup "wgd10even" [ bench (show x) $ nf (uncurry winograd10) (am x, bm x) | x <- [100,200..evenl] ]
                    ]
                ]


partBench :: IO ()
partBench = do
  let
    am = matrixCtor 2 1
    bm = matrixCtor 2 3
  defaultMain [
    bgroup "mult" [
         bgroup "wgd8even"  [ bench (show x) $ nf (uncurry winograd8) (am x, bm x) | x <- [100, 500] ]
       , bgroup "wgd9even"  [ bench (show x) $ nf (uncurry winograd9) (am x, bm x) | x <- [100, 500] ]
                    ]
                ]
