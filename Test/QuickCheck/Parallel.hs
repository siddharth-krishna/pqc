{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Parallel
-- Copyright   :  (c) Don Stewart 2006-2007, shelarcy 2011-2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  shelarcy <shelarcy@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Concurrent, GHC.Conc )
--
-- A parallel batch driver for running QuickCheck on threaded or SMP systems.
-- See the /Example.hs/ file for a complete overview.
--

module Test.QuickCheck.Parallel (
                                 module Test.QuickCheck,
                                 pRun,
                                 pRunAllProcessors,
                                 pRunWithNum,
                                 Name,
                                 Depth,
                                 Test--,
                                --  pDet,
                                --  pNon
                                 ) where

import Test.QuickCheck
import Test.QuickCheck.Gen  (unGen)
import Test.QuickCheck.Test (test)
#if MIN_VERSION_QuickCheck(2,6,0)
import Test.QuickCheck.Text (withNullTerminal)
#else
import Test.QuickCheck.Text (newNullTerminal)
#endif
import Test.QuickCheck.State

import Control.Concurrent
#if   __GLASGOW_HASKELL__ >= 706
import GHC.Conc           (getNumProcessors)
#elif __GLASGOW_HASKELL__ >= 704
import GHC.Conc           (getNumProcessors, setNumCapabilities)
#else
import GHC.Conc           (numCapabilities, forkOnIO)
#endif

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad      (forM_, unless, when)
import System.Random
import System.Exit
import System.IO          (hFlush,stdout)
import Text.Printf

-- | A name or description for test
type Name   = String
-- | Maximum number of successful test values
type Depth  = Int
-- | Test case for parallel batch driver
type Test   = (Name, Depth -> IO Result)

-- | Run a list of QuickCheck properties in parallel chunks, and test
-- to a depth of 'd' (first argument). Parallel Chunks is Haskell thread
-- that can run truly simultaneously (on separate physical processors)
-- at any given time. 
--
-- Compile your application with '-threaded' and run with the SMP runtime's
-- '-N4' (or however many OS threads you want to donate), for best results.
--
-- > import Test.QuickCheck.Parallel
-- >
-- > pRun 1000
-- >    [ ("sort1", pDet prop_sort1)
-- >    , ("sort2", pDet prop_sort2) ]
--
-- with SMP runtime's '-N[n]' flag will run 'n' threads over the property
-- list, to depth 1000. (see 'getNumCapabilities' for more details.)
--
pRun :: Depth -> [Test] -> IO ()
pRun depth tests = do
#if __GLASGOW_HASKELL__ >= 704
    num <- getNumCapabilities
#else
    let num = numCapabilities
#endif
    pRunWithNum num depth tests

-- | Variant of 'pRun'. Run a list of QuickCheck properties in parallel
-- chunks, using all Processors.
pRunAllProcessors  :: Depth -> [Test] -> IO ()
#if   __GLASGOW_HASKELL__ < 704
pRunAllProcessors depth tests
  = pRunInternal forkOnIO numCapabilities depth tests
#else
pRunAllProcessors depth tests = do
    caps <- getNumCapabilities
    pros <- getNumProcessors
    unless (caps == pros)
      $ setNumCapabilities pros
    pRunInternal forkOn pros depth tests
#endif

-- | Variant of 'pRun'. Run a list of QuickCheck properties in parallel
-- chunks, using 'n' Haskell threads (first argument), and test to a
-- depth of 'd' (second argument). Compile your application with
-- '-threaded' and run with the SMP runtime's '-N4' (or however many OS
-- threads you want to donate), for best results.
--
-- > import Test.QuickCheck.Parallel
-- >
-- > do n <- getArgs >>= readIO . head
-- >    pRunWithNum n 1000 [ ("sort1", pDet prop_sort1) ]
--
-- Will run 'n' threads over the property list, to depth 1000.
--
-- If you want to specify 'n' by using '-N[n]' or 'setNumCapabilities',
-- use 'pRun' instead of this function.
--
pRunWithNum :: Int -> Depth -> [Test] -> IO ()
pRunWithNum = pRunInternal (\_ -> forkIO)

pRunInternal :: (Int -> IO () -> IO ThreadId) -> Int -> Int -> [Test] -> IO ()
pRunInternal fork n depth tests = do
    chan <- newChan
    ps   <- getChanContents chan
    work <- newMVar tests
    ec'  <- newTVarIO ExitSuccess

    forM_ [1..n] $ \num -> fork num $ thread work chan ec' num

    let wait xs i
            | i >= n    = return () -- done
            | otherwise = case xs of
                    Nothing : ys -> wait ys $! i+1
                    Just s  : ys -> putStr s >> hFlush stdout >> wait ys i
    wait ps 0
    ec <- readTVarIO ec'
    exitWith ec

  where
    thread :: MVar [Test] -> Chan (Maybe String) -> (TVar ExitCode) -> Int -> IO ()
    thread work chan ec' me = loop
      where
        loop = do
            -- TODO modifyMVar isn't atomic, would this cause work to be dropped sometime?
            job <- modifyMVar work $ \jobs -> return $ case jobs of
                        []     -> ([], Nothing)
                        (j:js) -> (js, Just j)
            case job of
                Nothing          -> writeChan chan Nothing -- done
                Just (name,prop) -> do
                    v <- prop depth
                    doesAnyFailureTest v ec'
                    writeChan chan . Just $ printf "%d: %-25s: %s" me name $ output v
                    loop

    doesAnyFailureTest :: Result -> TVar (ExitCode) -> IO ()
    doesAnyFailureTest v ec'
      = case v of
          (GaveUp _ _ _ _ _ _)            -> noticeFailureTest ec'
#if MIN_VERSION_QuickCheck(2,6,0)
          (Failure _ _ _ _ _ _ _ _ _ _ _ _ _) -> noticeFailureTest ec'
#else
          (Failure _ _ _ _ _ _ _)   -> noticeFailureTest ec'
#endif
          _                         -> return ()

    testFailure :: ExitCode
    testFailure = ExitFailure 1

    noticeFailureTest :: TVar (ExitCode) -> IO ()
    noticeFailureTest ec' = atomically $ do
        ec <- readTVar ec'
        when (ec == ExitSuccess)
          $ writeTVar ec' testFailure

-- -- | Wrap a property, and run it on a deterministic set of data
-- pDet :: Testable a => a -> Depth -> IO Result
-- pDet a n = mycheck Det (stdArgs { maxSuccess = n }) a

-- -- | Wrap a property, and run it on a non-deterministic set of data
-- pNon :: Testable a => a -> Depth -> IO Result
-- pNon a n = mycheck NonDet (stdArgs { maxSuccess = n }) a

-- data Mode = Det | NonDet

-- ------------------------------------------------------------------------

-- mycheck :: Testable a => Mode -> Args -> a -> IO Result
-- mycheck Det config a = do
--     let rnd = mkStdGen 99  -- deterministic
--     mytests config rnd a

-- mycheck NonDet config a = do
--     rnd <- newStdGen       -- different each run
--     mytests config rnd a

-- mytests :: Testable prop => Args -> StdGen -> prop -> IO Result
-- mytests a rnd p =
-- #if MIN_VERSION_QuickCheck(2,6,0)
--   withNullTerminal $ \tm -> do
-- #else
--   do tm <- newNullTerminal
-- #endif
--      test MkState{ terminal          = tm
-- #if MIN_VERSION_QuickCheck(2,5,0)
--                  , maxSuccessTests   = if exhaustive p then 1 else maxSuccess a
--                  , maxDiscardedTests = if exhaustive p then maxDiscardRatio a else maxDiscardRatio a * maxSuccess a
--                  , numTotTryShrinks  = 0
-- #else
--                  , maxSuccessTests   = maxSuccess a
--                  , maxDiscardedTests = maxDiscard a
-- #endif
--                  , computeSize       = case replay a of
--                                          Nothing    -> computeSize'
--                                          Just (_,s) -> computeSize' `at0` s
--                  , numSuccessTests   = 0
--                  , numDiscardedTests = 0
-- #if MIN_VERSION_QuickCheck(2,5,1)
--                  , numRecentlyDiscardedTests = 0
-- #endif
--                  , collected         = []
--                  , expectedFailure   = False
--                  , randomSeed        = rnd
--                  , numSuccessShrinks = 0
--                  , numTryShrinks     = 0
--                  } (unGen (property p))
--   where computeSize' n d
--           -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
--           -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
--           | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
--             n >= maxSuccess a ||
--             maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
--           | otherwise =
--             ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
--         n `roundTo` m = (n `div` m) * m
--         at0 _ s 0 0 = s
--         at0 f _ n d = f n d
