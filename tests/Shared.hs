{-# LANGUAGE OverloadedStrings #-}

module Shared where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

fastProp :: IORef Int -> Property
fastProp r = withTests 5 $ property $ do
  liftIO $ do 
    val <- readIORef r
    print val
    writeIORef r (val+1)
  True === True

slowProp :: IORef Int -> Property
slowProp r = withTests 200 $ property $ do
  liftIO $ do 
    val <- readIORef r
    print val
    writeIORef r (val+1)
  True === True

hedgehogDirectly :: IORef Int -> IO ()
hedgehogDirectly r = void $ checkSequential $ Group "hedgehog directly" [
      ("fast", fastProp r)
    , ("slow", slowProp r)
    ]

hedgehogTastyTestTree :: IORef Int -> TestTree
hedgehogTastyTestTree r =
  testGroup "tasty-hedgehog" [
    testProperty "fast" (fastProp r)
  , testProperty "slow" (slowProp r)
  ]

hedgehogTasty :: IORef Int -> IO ()
hedgehogTasty = defaultMain . hedgehogTastyTestTree
