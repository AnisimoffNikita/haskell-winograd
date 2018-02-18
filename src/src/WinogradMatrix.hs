module WinogradMatrix where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Control.DeepSeq

winograd1 :: Num a => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd1 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd1 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd1 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd1 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    rows = V.generate m $ \i -> group $ M.getRow (i + 1) a
    cols = V.generate p $ \j -> group $ M.getCol (j + 1) b

    group v = foldl (group' v) 0 [0, 2 .. V.length v  - 2]
    group' v acc i =
      acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = M.matrix m p $ \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (M.getRow i a) (M.getCol j b) +
          if odd n then
            M.unsafeGet i n a * M.unsafeGet n j b
          else
            0

    helper r c =
      foldl (helper' r c) 0 [0, 2 .. V.length r - 2]
    helper' r c acc i = let
                      y1 = V.unsafeIndex c (i)
                      y2 = V.unsafeIndex c (i+1)
                      x1 = V.unsafeIndex r (i)
                      x2 = V.unsafeIndex r (i+1)
                    in
                      acc +(x1+y2)*(x2+y1)


winograd2 :: Num a => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd2 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd2 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd2 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd2 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    a' = V.generate m $ \i -> M.getRow (i+1) a
    b' = V.generate p $ \j -> M.getCol (j+1) b

    rows = V.generate m $ \i -> group $ V.unsafeIndex a' i
    cols = V.generate p $ \j -> group $ V.unsafeIndex b' j

    group v =foldl (group' v) 0 [0, 2 .. V.length v - 2]
    group' v acc i = acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1)) +
          if odd n then
            M.unsafeGet i n a * M.unsafeGet n j b
          else
            0

    helper r c =foldl (helper' r c) 0 [0, 2 .. V.length r - 2]
    helper' r c acc i = let
                    y1 = V.unsafeIndex c (i)
                    y2 = V.unsafeIndex c (i+1)
                    x1 = V.unsafeIndex r (i)
                    x2 = V.unsafeIndex r (i+1)
                  in
                    acc +(x1+y2)*(x2+y1)


winograd3 :: Num a => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd3 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd3 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd3 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd3 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    a' = V.generate m $ \i -> M.getRow (i+1) a
    b' = V.generate p $ \j -> M.getCol (j+1) b

    rows = V.generate m $ \i -> group $ V.unsafeIndex a' i
    cols = V.generate p $ \j -> group $ V.unsafeIndex b' j

    group v =foldl (group' v) 0 [0, 2 .. n - 2]
    group' v acc i = acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1)) +
          if odd n then
            M.unsafeGet i n a * M.unsafeGet n j b
          else
            0

    helper r c =foldl (helper' r c) 0 [0, 2 .. n - 2]
    helper' r c acc i = let
                    y1 = V.unsafeIndex c (i)
                    y2 = V.unsafeIndex c (i+1)
                    x1 = V.unsafeIndex r (i)
                    x2 = V.unsafeIndex r (i+1)
                  in
                    acc +(x1+y2)*(x2+y1)


winograd4 :: Num a => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd4 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd4 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd4 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd4 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    a' = V.generate m $ \i -> M.getRow (i+1) a
    b' = V.generate p $ \j -> M.getCol (j+1) b

    rows = V.generate m $ \i -> group $ V.unsafeIndex a' i
    cols = V.generate p $ \j -> group $ V.unsafeIndex b' j

    group v =foldl (group' v) 0 [0, 2 .. n - 2]
    group' v acc i = acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = if odd n then
          M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1)) +
          M.unsafeGet i n a * M.unsafeGet n j b
        else
          M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1))

    helper r c =foldl (helper' r c) 0 [0, 2 .. n - 2]
    helper' r c acc i = let
                    y1 = V.unsafeIndex c (i)
                    y2 = V.unsafeIndex c (i+1)
                    x1 = V.unsafeIndex r (i)
                    x2 = V.unsafeIndex r (i+1)
                  in
                    acc +(x1+y2)*(x2+y1)

winograd5 :: (Num a, NFData a) => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd5 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd5 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd5 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd5 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    a' = V.generate m $ \i -> M.getRow (i+1) a
    b' = V.generate p $ \j -> M.getCol (j+1) b

    rows = V.generate m $ \i -> group $ V.unsafeIndex a' i
    cols = V.generate p $ \j -> group $ V.unsafeIndex b' j

    group v = foldl (group' v) 0 [0, 2 .. n - 2]
    group' v acc i = acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = a' `deepseq` b' `deepseq` if odd n then
          M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1)) +
          M.unsafeGet i n a * M.unsafeGet n j b
        else
          M.matrix m p $
          \(i,j) ->
          V.unsafeIndex rows (i-1) +
          V.unsafeIndex cols (j-1) +
          helper (V.unsafeIndex a' (i-1)) (V.unsafeIndex b' (j-1))

    helper r c = foldl (helper' r c) 0 [0, 2 .. n - 2]
    helper' r c acc i = let
                    y1 = V.unsafeIndex c (i)
                    y2 = V.unsafeIndex c (i+1)
                    x1 = V.unsafeIndex r (i)
                    x2 = V.unsafeIndex r (i+1)
                  in
                    acc +(x1+y2)*(x2+y1)


winograd6 :: (Num a, NFData a) => M.Matrix a -> M.Matrix a -> M.Matrix a
{-# SPECIALIZE winograd5 :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double #-}
{-# SPECIALIZE winograd5 :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int #-}
{-# SPECIALIZE winograd5 :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational #-}
winograd6 a b = if n' == n then c else error "error"
  where
    m = M.nrows a
    n = M.ncols a
    n' = M.nrows b
    p = M.ncols b

    a' = V.generate m $ \i -> M.getRow (i+1) a
    b' = V.generate p $ \j -> M.getCol (j+1) b

    rows = V.generate m $ \i -> group $ V.unsafeIndex a' i
    cols = V.generate p $ \j -> group $ V.unsafeIndex b' j

    group v = foldl (group' v) 0 [0, 2 .. n - 2]
    group' v acc i = acc - V.unsafeIndex v i * V.unsafeIndex v (i+1)

    c = a' `deepseq` b' `deepseq` if odd n then
          M.matrix m p $
          \(i,j) ->
          let
            t1 = V.unsafeIndex a' (i-1)
            t2 = V.unsafeIndex b' (j-1)
          in
            V.unsafeIndex rows (i-1) +
            V.unsafeIndex cols (j-1) +
            helper t1 t2 +
            V.last t1 * V.last t2
        else
          M.matrix m p $
          \(i,j) ->
          let
            t1 = V.unsafeIndex a' (i-1)
            t2 = V.unsafeIndex b' (j-1)
          in
            V.unsafeIndex rows (i-1) +
            V.unsafeIndex cols (j-1) +
            helper t1 t2

    helper r c = foldl (helper' r c) 0 [0, 2 .. n - 2]
    helper' r c acc i = let
                    y1 = V.unsafeIndex c (i)
                    y2 = V.unsafeIndex c (i+1)
                    x1 = V.unsafeIndex r (i)
                    x2 = V.unsafeIndex r (i+1)
                  in
                    acc +(x1+y2)*(x2+y1)


matrixCtor :: Int -> Int -> Int -> Int -> M.Matrix Int
matrixCtor x y m n = M.matrix m n $ \(i,j) -> x*i+y*j
