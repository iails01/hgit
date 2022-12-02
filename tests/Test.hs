{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Util
import Data


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "toHexHash" $
      toHexHash "this is cool" `compare` "60f51187e76a9de0ff3df31f051bde04da2da891" @?= EQ 
      , testCase "toHexHash" $ do
        hm <- getHEAD 
        maybe mempty (error "getHEAD should return Nothing") hm
  ]