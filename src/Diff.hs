{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff(
   diff,
   DiffElement(..),
   ) where


import Data.Array

import Control.Monad.ST
import Data.Array.ST
import GHC.Generics (Generic)

splitToElem :: (a -> Bool) -> [a] -> Maybe ([a],[a])
splitToElem fn = sTC
   where
      sTC [] = Nothing
      sTC (x:xs) =
         if fn x then Just ([],xs) else
            fmap
               (\ (xs1,xs2) -> (x:xs1,xs2))
               (sTC xs)

-- -----------------------------------------------------------------------
-- Datatypes
-- -----------------------------------------------------------------------

data DiffElement v =
      InBoth [v]
   |  InFirst [v]
   |  InSecond [v] deriving (Show)

diff :: Eq v => [v] -> [v] -> [DiffElement v]
diff [] [] = []
diff a b = runST (diffST a b)

-- NB. diffST does not work if both arguments are null, so that
-- case should be handled separately.
diffST :: forall v s . Eq v => [v] -> [v] -> ST s [DiffElement v]
diffST a b =
   do
      let
         m = length a
         (aArr :: Array Int v) = listArray (1,m) a

         n = length b
         (bArr :: Array Int v) = listArray (1,n) b

         match :: Int -> Int -> Bool
         match x y = (aArr ! x) == (bArr ! y)

         -- Given (x,y) return the highest (x+k,y+k) such that (x+1,y+1),
         -- (x+2,y+2)...(x+k,y+k) match.
         scan :: Int -> Int -> (Int,Int)
         scan x y =
            if x < m && y < n
               then
                  let
                     x' = x+1
                     y' = y+1
                  in
                     if match x' y' then scan x' y' else (x,y)
               else
                  (x,y)

         max = m+n
      -- We do the computation using an STArray for V
      -- We arrange that there is always a -1 on either side of the
      -- existing range, to simplify handling of the end-cases.
      (v :: STUArray s Int Int) <- newArray (-max-1,max+1) (-1)
      writeArray v 1 0

      -- The w array contains a list of integers (x,y) such that the snakes
      -- starting from the elements (x+1,y+1) together make up all the snakes
      -- needed in the optimal solution.
      --
      -- The idea is that storage for w should not get too big, either if a
      -- and b are much the same, or if they are completely different.  Thus
      -- in most cases quadratic behaviour *should* be avoided.
      (w :: STArray s Int [(Int,Int)]) <- newArray (-max,max) []

      let
         -- step carries out the algorithm for a given (d,k), returning
         -- the appropriate w-list.
         step :: Int -> Int -> ST s [(Int,Int)]
         step d k =
            if k > d
               then
                  innerStep (d+1) (-(d+1))
               else
                  innerStep d k

         innerStep :: Int -> Int -> ST s [(Int,Int)]
         innerStep d k =
            do
               vkplus <- readArray v (k+1)
               vkminus <- readArray v (k-1)
               (x,l0) <- if vkminus < vkplus
                  then
                     do
                        l0 <- readArray w (k+1)
                        return (vkplus,l0)
                  else
                     do
                        l <- readArray w (k-1)
                        return (vkminus+1,l)
               let
                  y = x - k

                  (x',_) = scan x y

                  l1 =
                     if x' == x
                        then
                           l0
                        else
                           (x,y) : l0

               -- Can we finish now?
               if x' >= m && (y + (x' - x)) >= n
                  then
                     return l1
                  else
                     do
                        writeArray v k x'
                        writeArray w k l1
                        step d (k+2)

      snakes <- step 0 0

      let
         -- The task is now to reassemble snakes to produce a list.  Since
         -- the snakes are given in reverse order, we may as well produce the
         -- elements in that order and work backwards.

         addSnake :: (Int,Int) -> (Int,Int)
            -> [DiffElement v] -> [DiffElement v]
         addSnake (lastX,lastY) (x,y) l0 =
            -- We assume that elements a[lastX+1...] and b[lastY+1...] have
            -- been dealt with, and we now add on a segment starting with a
            -- snake which begins at (x+1,y+1).
            let
               -- Compute the end of the snake
               (x',y') = scan x y

               -- Add on elements b[y'+1..lastY]
               l1 = InSecond (map (bArr !)
                       [y'+1..lastY]) : l0
               -- Add on elements a[x'+1..lastX]
               l2 = InFirst (map (aArr !)
                       [x'+1..lastX]) : l1
               -- Add on snake
               l3 = InBoth (map (aArr !)
                       [x+1..x']) : l2
            in
               l3

         doSnakes :: (Int,Int) -> [(Int,Int)] -> [DiffElement v]
            -> [DiffElement v]
         doSnakes last [] l0 =
            -- we pretend there's a zero-length snake starting at (1,1).
            if last /= (0,0) then addSnake last (0,0) l0 else l0
         doSnakes last (s:ss) l0 =
            let
               l1 = addSnake last s l0
            in
               doSnakes s ss l1

         result0 = doSnakes (m,n) snakes []

         result1 = filter
            -- Filter out null elements
            (\ de -> case de of
               InFirst [] -> False
               InSecond [] -> False
               InBoth [] -> False
               _ -> True
               )
            result0

      return result1
