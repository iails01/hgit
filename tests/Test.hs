{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Util
import Data
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import qualified Data.ByteString.UTF8  as Utf8
import qualified Data.ByteString  as BS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "toHexHash" $
      toHexHash "this is cool" `compare` "60f51187e76a9de0ff3df31f051bde04da2da891" @?= EQ
      , testCase "toHexHash" $ do
        hm <- lift getHEAD
        maybe mempty (error "getHEAD should return Nothing") hm
      , testCase "toHexHash" $ do
        bs <- BS.readFile "/home/iails/Projects/haskell/test/.hgit/objects/40bb77b91d66657478131434527b19cbb1e4b829"
        print $ break ( == "") (Utf8.lines bs)
  ]