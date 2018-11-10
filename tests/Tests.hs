module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Properties
import qualified UnitTests

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    let allTests = Properties.tests : UnitTests.tests : ioTests
    defaultMain (testGroup "tests" allTests)
