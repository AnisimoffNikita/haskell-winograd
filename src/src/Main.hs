module Main where

import System.Environment

import qualified Data.Matrix as M

import Control.DeepSeq
import Control.Exception
import System.Clock
import System.Mem
import Data.Time
import Criterion.Main

import WinogradMatrix

test f = do
  let a = matrixCtor 2 (-3) 5 6
  let b = matrixCtor 4 (-1) 6 7
  putStrLn "a:5.5"
  putStrLn $ M.prettyMatrix a
  putStrLn "b:5.5"
  putStrLn $ M.prettyMatrix b
  let c0 = M.multStrassenMixed a b
  putStrLn "0:5.5"
  putStrLn $ M.prettyMatrix c0
  let c = f a b
  putStrLn "1:5.5"
  putStrLn $ M.prettyMatrix c

  let a = matrixCtor 2 (-3) 6 7
  let b = matrixCtor 4 (-1) 7 8
  putStrLn "a:4.4"
  putStrLn $ M.prettyMatrix a
  putStrLn "b:4.4"
  putStrLn $ M.prettyMatrix b
  let c0 = M.multStrassenMixed a b
  putStrLn "0:4.4"
  putStrLn $ M.prettyMatrix c0
  let c = f a b
  putStrLn "1:4.4"
  putStrLn $ M.prettyMatrix c

  let a = matrixCtor 2 (-3) 100 201
  let b = matrixCtor 4 (-1) 201 402
  let c0 = M.multStrassenMixed a b
  let c = f a b
  putStrLn $ "equal: " ++ show (c == c0)



test' = do
  let a = matrixCtor 2 (-3)
  let b = matrixCtor 4 (-1)
  -- defaultMain [
  --   bgroup "mult" [ bench "1" $ nf (uncurry winograd1) (a,b)
  --                 , bench "2" $ nf (uncurry winograd2) (a,b)
  --                 , bench "3" $ nf (uncurry winograd3) (a,b)
  --                 , bench "4" $ nf (uncurry winograd4) (a,b)
  --                 , bench "5" $ nf (uncurry winograd5) (a,b)
  --                 , bench "6" $ nf (uncurry winograd6) (a,b)
  --                 , bench "StrassenMixed" $ nf (uncurry M.multStrassenMixed) (a,b)
  --                 ]
  --             ]

  defaultMain [
    bgroup "mult" [
                    -- bgroup "1" [bench (show x) $ nf (uncurry winograd1) (a x x, b x x) | x <- [100,200..1000] ],
                    -- bgroup "2" [bench (show x) $ nf (uncurry winograd2) (a x x, b x x) | x <- [100,200..1000] ],
                    -- bgroup "3" [bench (show x) $ nf (uncurry winograd3) (a x x, b x x) | x <- [100,200..1000] ],
                    bgroup "4" [bench (show x) $ nf (uncurry winograd4) (a x x, b x x) | x <- [100,200..1000] ],
                    bgroup "5" [bench (show x) $ nf (uncurry winograd5) (a x x, b x x) | x <- [100,200..1000] ],
                    bgroup "5'" [bench (show x) $ nf (uncurry winograd5) (a x x, b x x) | x <- [101,201..1001] ],
                    bgroup "6" [bench (show x) $ nf (uncurry winograd6) (a x x, b x x) | x <- [101,201..1001] ],
                    bgroup "s" [bench (show x) $ nf (uncurry M.multStrassenMixed) (a x x, b x x) | x <- [100,200..1000] ],
                    bgroup "s'" [bench (show x) $ nf (uncurry M.multStrassenMixed) (a x x, b x x) | x <- [101,201..1001] ]
                  ]
              ]

main = test'
