{-# LANGUAGE OverloadedStrings #-}

module Main where

import Stockfighter
import Stockfighter.UI

main :: IO ()
main = runStockfighter "API_KEY" (withLevel "first_steps" startBlotter)
