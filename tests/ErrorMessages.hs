{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorMessages
  (
    tests
  ) where

import Prelude.Compat

import Data.Aeson (FromJSON(..), eitherDecode)
import Data.Proxy (Proxy(..))
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, assertEqual, testCase)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Strict as M

tests :: [TestTree]
tests =
    [
      testCase "Int" int
    , testCase "Integer" integer
    , testCase "Natural" natural
    , testCase "String" string
    , testCase "Map" mapAssertion
    ]

int :: Assertion
int = do
  let t = test (Proxy :: Proxy Int)
  t "\"\"" $ expected "Int" "String"
  t "[]" $ expected "Int" "Array"
  t "{}" $ expected "Int" "Object"
  t "null" $ expected "Int" "Null"

integer :: Assertion
integer = do
  let t = test (Proxy :: Proxy Integer)
  t "44.44" $ expected "Integer" "floating number 44.44"

natural :: Assertion
natural = do
  let t = test (Proxy :: Proxy Natural)
  t "44.44" $ expected "Natural" "floating number 44.44"
  t "-50" $ expected "Natural" "negative number -50"

string :: Assertion
string = do
  let t = test (Proxy :: Proxy String)
  t "1" $ expected "String" "Number"
  t "[]" $ expected "String" "Array"
  t "{}" $ expected "String" "Object"
  t "null" $ expected "String" "Null"

mapAssertion :: Assertion
mapAssertion = do
  let t = test (Proxy :: Proxy (M.Map String Int))
  t "\"\"" $ expected "Map k v" "String"
  t "[]" $ expected "Map k v" "Array"

expected :: String -> String -> String
expected ex enc = "Error in $: expected " ++ ex ++ ", encountered " ++ enc

test :: forall a proxy . (FromJSON a, Show a) => proxy a -> L.ByteString -> String -> Assertion
test _ v msg = case eitherDecode v of
    Left e -> assertEqual "Invalid error message" msg e
    Right (x :: a) -> assertFailure $ "Expected parsing to fail but it suceeded with: " ++ show x
