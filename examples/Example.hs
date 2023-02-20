-- 
--
-- $ ghc -O -package pqc Example.hs -threaded
-- 
--

import Test.QuickCheck.Parallel
import Data.List
import System.Environment
-- If you want to test code involving bottoms, use ChasingBottoms package.
import Test.ChasingBottoms (isBottom, bottom)

prop_head1 xs =
       (not (null xs)) ==>
       head xs == xs!!0
  where types = (xs :: [Int])
-- Test bottoms
{- test for abort -}
prop_head2 = isBottom (head [])
{- test for strictness -}
prop_head3 = isBottom (head bottom)


prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])

prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == minimum xs)
  where types = (xs :: [Int])

prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == maximum xs
  where types = (xs :: [Int])

prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

--
-- Run in x threads (that is specified by +RTS -Nx), to depth of 1000
--
main = do
    pRun 1000 $ take 100 $ cycle
        [ ("sort1", pDet prop_sort1)
        , ("sort2", pDet prop_sort2)
        , ("sort3", pDet prop_sort3)
        , ("sort4", pDet prop_sort4)
        , ("sort5", pDet prop_sort5)
        ]
