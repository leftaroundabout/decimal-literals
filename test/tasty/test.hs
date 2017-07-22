-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Main where

import Numeric.Literals.Decimal

import Test.Tasty
import Test.Tasty.HUnit



main = defaultMain tests


reshowTest :: FractionalLit -> String -> TestTree
reshowTest n str = reshowTestCase str n str

reshowTestCase :: String -> FractionalLit -> String -> TestTree
reshowTestCase info n str = testCase info $ show n @?= str

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Re-showing literals"
     [ reshowTest                  4 "4"
     , reshowTest             142125 "142125"
     , reshowTest            125.278 "125.278"
     , reshowTest                0.2 "0.2"
     , reshowTest               0.04 "0.04"
     , reshowTest              0.008 "8e-3"
     , reshowTest              1e-52 "1e-52"
     , reshowTest    1.167925373e-12 "1.167925373e-12"
     , reshowTest              0.254 "0.254"
     , reshowTest (-1.167925373e-12) "-1.167925373e-12"
     , reshowTest          (-142125) "-142125"
     ]
  , testGroup "Simple arithmetic"
     [ reshowTestCase "37 + 15" (37 + 15) "52"
     , reshowTestCase "37 + 15.8" (37 + 15.8) "52.8"
     , reshowTestCase "37.1 + 15.8" (37.1 + 15.8) "52.9"
     ]
  ]



