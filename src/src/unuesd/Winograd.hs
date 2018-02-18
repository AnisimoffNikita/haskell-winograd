module Winograd where

import Data.Array
import Debug.Trace
import Control.DeepSeq

type Vector = [Int]
type Matrix = [Vector]

transpose :: Matrix  -> Matrix
transpose [] = []
transpose ([]:_) = []
transpose x = (map head x):transpose (map tail x)

winograd1 :: Matrix -> Matrix -> Matrix
winograd1 a b = c
  where
    rows = winogradRows a
    cols = winogradCols b
    c = winogradMain rows cols a b

winogradRows :: Matrix -> Vector
winogradRows [] = []
winogradRows (x:xs) = (winogradGroup x) : winogradRows xs

winogradCols :: Matrix -> Vector
winogradCols [] = []
winogradCols x = (winogradGroup (map head x)) : winogradCols ( map tail x)

winogradGroup :: Vector -> Int
winogradGroup [] = 0
winogradGroup [_] = 0
winogradGroup (x1:x2:xs) = x1*x2 + winogradGroup xs

winogradMain :: Vector -> Vector -> Matrix -> Matrix -> Matrix
winogradMain rows cols a b = c
  where
    l = (length b `div` 2) * 2
    b' = transpose b
    c' = [ [ f x y | y <- zip (transpose b) cols ] | x <- zip a rows ]
    f (x, col) (y, row) = - row - col + g (take l x) (take l y)
    g [] [] = 0
    g (x1:x2:xs) (y1:y2:ys) = (x1 + y2)*(x2 + y1) + g xs ys

    col = map last a
    row = last b
    c'' = [ [ x*y | y <- row ] | x <- col ]

    c = if odd $ length b then
           zipWith (zipWith (+)) c'' c'
         else
           c'

winograd2wa :: (Matrix, Matrix) -> Matrix
winograd2wa (a,b) = winograd2 a b

winograd2 :: Matrix -> Matrix -> Matrix
winograd2 a b = c
 where
   b' = transpose b
   rows = winogradRows2 a
   cols = winogradRows2 b'
   c =  winogradMain2 rows cols a b' (length b)

winogradRows2 :: Matrix -> Vector
winogradRows2 x = foldr (\x acc -> (winogradGroup x):acc) [] x

winogradMain2 :: Vector -> Vector -> Matrix -> Matrix -> Int -> Matrix
winogradMain2 rows cols a b' lb = c
  where
    l = lb - (lb `mod` 1)
    rc = [ [ x+y | y <- rows ] | x <- cols ]
    c' = [ [ f x y 0 | y <- b' ] | x <- a ]
    f [] _ acc = acc
    f [_] _ acc = acc
    f (x1:x2:xs) (y1:y2:ys) acc = f xs ys ((x1 + y2)*(x2 + y1) + acc)

    col = map last a
    row = map last b'
    lst = [ [ x*y | y <- row ] | x <- col ]

    c = if odd $ lb then
          zipWith3 (zipWith3 (\x y z -> x + y - z)) lst c' rc
        else
          zipWith (zipWith (+)) c' rc
