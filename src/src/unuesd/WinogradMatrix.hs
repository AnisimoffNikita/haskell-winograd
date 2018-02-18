module WinogradMatrix where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Control.Loop (forLoopFold)
import Control.DeepSeq
import Control.Parallel
import Debug.Trace

winograd6wa :: (M.Matrix Int, M.Matrix Int) -> M.Matrix Int
winograd6wa (x, y) = winograd6 x y

winograd6 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int
winograd6 x y =  M.fromLists list
  where
    rx = M.nrows x
    cx = M.ncols x
    ry = M.nrows y
    cy = M.ncols y

    ev = even cx

    end = if ev then 0 else 1

    rows = V.fromList [ sum [M.unsafeGet i j x * M.unsafeGet i (j+1) x | j <- [1, 3 .. cx - end ] ] | i <- [1 .. rx ] ]
    cols = V.fromList [ sum [M.unsafeGet i j y * M.unsafeGet (i+1) j y | i <- [1, 3 .. ry - end ] ] | j <- [1 .. cy ] ]
    list = if ev then
             [ [sum [ (M.unsafeGet i (k+1) x + M.unsafeGet k j y) * (M.unsafeGet i k x + M.unsafeGet (k+1) j y) | k <- [1, 3 .. cx] ] - (V.unsafeIndex rows (i-1)) - (V.unsafeIndex cols (j-1))
               | j <- [1 .. cy] ]
                 |i <- [1 .. rx] ]
           else
             [ [sum [ (M.unsafeGet i (k+1) x + M.unsafeGet k j y) * (M.unsafeGet i k x + M.unsafeGet (k+1) j y) | k <- [1, 3 .. cx-1] ] - (V.unsafeIndex rows (i-1)) - (V.unsafeIndex cols (j-1) )
              + x M.! (i, cx) * y M.! (ry, j)
               | j <- [1 .. cy] ]
                 |i <- [1 .. rx] ]


winograd8wa :: (M.Matrix Int, M.Matrix Int) -> M.Matrix Int
winograd8wa (x, y) = winograd8 x y

winograd8 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int
winograd8 x y = z
  where
    rx = M.nrows x
    cx = M.ncols x
    ry = M.nrows y
    cy = M.ncols y

    ev = even cx

    end = if ev then 0 else 1

    rows = V.generate rx $ \i' -> let i = i'+1 in sum [M.unsafeGet i j x * M.unsafeGet i (j+1) x | j <- [1, 3 .. cx - end ] ]
    cols = V.generate cy $ \j' -> let j = j'+1 in sum [M.unsafeGet i j y * M.unsafeGet (i+1) j y | i <- [1, 3 .. ry - end ] ]

    z = if ev then
          M.matrix rx cy $ \(i,j) ->
             sum [ (M.unsafeGet i (k+1) x + M.unsafeGet k j y) * (M.unsafeGet i k x + M.unsafeGet (k+1) j y) | k <- [1, 3 .. cx] ] - V.unsafeIndex rows (i-1) - V.unsafeIndex cols (j-1)

        else
          M.matrix rx cy $ \(i,j) ->
             sum [ (M.unsafeGet i (k+1) x + M.unsafeGet k j y) * (M.unsafeGet i k x + M.unsafeGet (k+1) j y) | k <- [1, 3 .. cx-1] ] - V.unsafeIndex rows (i-1) - V.unsafeIndex cols (j-1)
              + M.unsafeGet i cx x * M.unsafeGet ry j y

winograd9wa :: (M.Matrix Int, M.Matrix Int) -> M.Matrix Int
winograd9wa (x, y) = winograd9 x y

winograd9 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int
winograd9 a b = c
  where
    ra = M.nrows a
    ca = M.ncols a
    rb = M.nrows b
    cb = M.ncols b

    isEven = even cx

    avs = V.generate ra $ \i -> M.getRow (i+1) x
    bvs = V.generate cb $ \j -> M.getCol (j+1) y

    rows = V.generate ra $ \i -> wgdGroup $ V.unsafeIndex avs i
    cols = V.generate cb $ \j -> wgdGroup $ V.unsafeIndex bvs j

    wgdGroup x = let
                   finish = (V.length x - 1)
                 in
                   forLoopFold 0 (<finish) (+2) 0 $
                   \acc i -> acc - V.unsafeIndex x i *  V.unsafeIndex x (i+1)

    c = avs `deepseq` bvs `deepseq` if isEven then
          M.matrix rx cy $
          \(i,j) -> V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          g (V.unsafeIndex xvs (i-1)) (V.unsafeIndex yvs (j-1))
        else
          M.matrix rx cy $
          \(i,j) -> V.unsafeIndex rows (i-1) + 
          V.unsafeIndex cols (j-1) +
          g (V.unsafeIndex xvs (i-1)) (V.unsafeIndex yvs (j-1)) +



    g :: V.Vector Int -> V.Vector Int -> Int
    g r c = forLoopFold 0 (<(cx-1)) (+2) 0 $ \acc i ->
              let
                x1 = V.unsafeIndex r i
                x2 = V.unsafeIndex r (i+1)
                y1 = V.unsafeIndex c i
                y2 = V.unsafeIndex c (i+1)
              in
              acc + (x1+x2)*(y1+y2)
