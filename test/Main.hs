module Main (main) where

import Test.Hspec (hspec)
import ParserTest (parserTests)

main :: IO ()
main = hspec parserTests
