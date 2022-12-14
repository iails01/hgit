{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Util
import Data
import qualified Base
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import qualified Data.ByteString.UTF8  as Utf8
import qualified Data.ByteString  as BS
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad (MonadPlus(mzero), void)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"

  [   testCase "toHexHash" $
        toHexHash "this is cool" `compare` "60f51187e76a9de0ff3df31f051bde04da2da891" @?= EQ

    , testCase "toHexHash" $ void . runMaybeT $ do
        hm <- getHEAD
        error "getHEAD should return Nothing"

    , testCase "compareTreeItems" $ do
        let i1 = [Base.MkTreeItem Data.Blob "abc" "file1.txt"]
            i2 = [Base.MkTreeItem Data.Blob "abc2" "file1.txt"]
        Base.compareTreeItems i1 i2 `compare` "changed: file1.txt\n" @?= EQ

  ]