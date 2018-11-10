{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- These tests assert that the JSON serialization doesn't change by accident.
-----------------------------------------------------------------------------

module SerializationFormatSpec
  (
    tests
  ) where

import Prelude.Compat

import Control.Applicative (Const(..))
import Data.Aeson (FromJSON(..), decode, encode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options(..), SumEncoding(..), ToJSON(..), defaultOptions)
import Data.Fixed (Pico)
import Data.Foldable (for_, toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Time (fromGregorian)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)
import Types (Approx(..), Compose3, Compose3', I)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.DList as DList
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Tree as Tree

tests :: [TestTree]
tests =
  [
    testGroup "To JSON representation" $ fmap assertJsonEncodingExample jsonEncodingExamples
  , testGroup "From JSON representation" $ fmap assertJsonExample jsonDecodingExamples
  , testGroup "To/From JSON representation" $ fmap assertJsonExample jsonExamples

  ]

jsonExamples :: [Example]
jsonExamples =
  [
    example "Either Left" "{\"Left\":1}"  (Left 1 :: Either Int Int)
  , example "Either Right" "{\"Right\":1}"  (Right 1 :: Either Int Int)
  , example "Nothing"  "null"  (Nothing :: Maybe Int)
  , example "Just"  "1"  (Just 1 :: Maybe Int)
  , example "Proxy Int" "null"  (Proxy :: Proxy Int)
  , example "Tagged Char Int" "1"  (Tagged 1 :: Tagged Char Int)
  , example "Tagged 123 Int" "1"  (Tagged 1 :: Tagged 123 Int)
  , example "Const Char Int" "\"c\""  (Const 'c' :: Const Char Int)
  , example "Tuple" "[1,2]"  ((1, 2) :: (Int, Int))
  , example "NonEmpty" "[1,2,3]"  (1 :| [2, 3] :: NonEmpty Int)
  , example "Seq" "[1,2,3]"  (Seq.fromList [1, 2, 3] ::  Seq.Seq Int)
  , example "DList" "[1,2,3]"  (DList.fromList [1, 2, 3] :: DList.DList Int)
  , example "()" "[]"  ()

  , Example "Map Int Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (M.fromList [(0,1),(2,3)] :: M.Map Int Int)
  , Example "Map Int Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (M.fromList [(0,1),(2,3)] :: M.Map Int Int)
  , Example "Map (Tagged Int Int) Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (M.fromList [(Tagged 0,1),(Tagged 2,3)] :: M.Map (Tagged Int Int) Int)
  , example "Map [Int] Int"
        "[[[0],1],[[2],3]]"
        (M.fromList [([0],1),([2],3)] :: M.Map [Int] Int)
  , Example "Map [Char] Int"
        [ "{\"ab\":1,\"cd\":3}", "{\"cd\":3,\"ab\":1}" ]
        (M.fromList [("ab",1),("cd",3)] :: M.Map String Int)
  , Example "Map [I Char] Int"
        [ "{\"ab\":1,\"cd\":3}", "{\"cd\":3,\"ab\":1}" ]
        (M.fromList [(map pure "ab",1),(map pure "cd",3)] :: M.Map [I Char] Int)

  , example "nan :: Double" "null"  (Approx $ 0/0 :: Approx Double)

  , example "Ordering LT" "\"LT\"" LT
  , example "Ordering EQ" "\"EQ\"" EQ
  , example "Ordering GT" "\"GT\"" GT

  , example "Float" "3.14" (3.14 :: Float)
  , example "Pico" "3.14" (3.14 :: Pico)
  , example "Scientific" "3.14" (3.14 :: Scientific)

  , example "Set Int" "[1,2,3]" (Set.fromList [3, 2, 1] :: Set.Set Int)
  , example "IntSet"  "[1,2,3]" (IntSet.fromList [3, 2, 1])
  , example "IntMap" "[[1,2],[3,4]]" (IntMap.fromList [(3,4), (1,2)] :: IntMap.IntMap Int)
  , example "List" "[1,2,3]" ([1, 2, 3] :: [Int])
  , example "Set Int" "[1,2,3]" (Set.fromList [3, 2, 1] :: Set.Set Int)
  , example "Tree Int" "[1,[[2,[[3,[]],[4,[]]]],[5,[]]]]" (let n = Tree.Node in n 1 [n 2 [n 3 [], n 4 []], n 5 []] :: Tree.Tree Int)

  -- Three separate cases, as ordering in Map is not defined
  , example "Map Float Int, NaN" "{\"NaN\":1}"  (Approx $ M.singleton (0/0) 1 :: Approx (M.Map Float Int))
  , example "Map Float Int, Infinity" "{\"Infinity\":1}"  (M.singleton (1/0) 1 :: M.Map Float Int)
  , example "Map Float Int, +Infinity" "{\"-Infinity\":1}"  (M.singleton (negate 1/0) 1 :: M.Map Float Int)

  -- Functors
  , example "Identity Int" "1"  (pure 1 :: Identity Int)

  , example "Identity Char" "\"x\""      (pure 'x' :: Identity Char)
  , example "Identity String" "\"foo\""  (pure "foo" :: Identity String)
  , example "[Identity Char]" "\"xy\""   ([pure 'x', pure 'y'] :: [Identity Char])

  , example "Maybe Char" "\"x\""              (pure 'x' :: Maybe Char)
  , example "Maybe String" "\"foo\""          (pure "foo" :: Maybe String)
  , example "Maybe [Identity Char]" "\"xy\""  (pure [pure 'x', pure 'y'] :: Maybe [Identity Char])

  , example "Day; year >= 1000" "\"1999-10-12\""        (fromGregorian 1999    10 12)
  , example "Day; year > 0 && < 1000" "\"0500-03-04\""  (fromGregorian 500     3  4)
  , example "Day; year == 0" "\"0000-02-20\""           (fromGregorian 0       2  20)
  , example "Day; year < 0" "\"-0234-01-01\""           (fromGregorian (-234)  1  1)
  , example "Day; year < -1000" "\"-1234-01-01\""       (fromGregorian (-1234) 1  1)

  , example "Product I Maybe Int" "[1,2]"         (Pair (pure 1) (pure 2) :: Product I Maybe Int)
  , example "Product I Maybe Int" "[1,null]"      (Pair (pure 1) Nothing :: Product I Maybe Int)
  , example "Product I [] Char" "[\"a\",\"foo\"]" (Pair (pure 'a') "foo" :: Product I [] Char)

  , example "Sum I [] Int: InL"  "{\"InL\":1}"       (InL (pure 1) :: Sum I [] Int)
  , example "Sum I [] Int: InR"  "{\"InR\":[1,2]}"   (InR [1, 2] :: Sum I [] Int)
  , example "Sum I [] Char: InR" "{\"InR\":\"foo\"}" (InR "foo" :: Sum I [] Char)

  , example "Compose I  I  Int" "1"      (pure 1 :: Compose I I   Int)
  , example "Compose I  [] Int" "[1]"    (pure 1 :: Compose I []  Int)
  , example "Compose [] I  Int" "[1]"    (pure 1 :: Compose [] I  Int)
  , example "Compose [] [] Int" "[[1]]"  (pure 1 :: Compose [] [] Int)

  , example "Compose I  I  Char" "\"x\""    (pure 'x' :: Compose I  I  Char)
  , example "Compose I  [] Char" "\"x\""    (pure 'x' :: Compose I  [] Char)
  , example "Compose [] I  Char" "\"x\""    (pure 'x' :: Compose [] I  Char)
  , example "Compose [] [] Char" "[\"x\"]"  (pure 'x' :: Compose [] [] Char)

  , example "Compose3 I  I  I  Char" "\"x\""      (pure 'x' :: Compose3 I  I  I  Char)
  , example "Compose3 I  I  [] Char" "\"x\""      (pure 'x' :: Compose3 I  I  [] Char)
  , example "Compose3 I  [] I  Char" "\"x\""      (pure 'x' :: Compose3 I  [] I  Char)
  , example "Compose3 I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3 I  [] [] Char)
  , example "Compose3 [] I  I  Char" "\"x\""      (pure 'x' :: Compose3 [] I  I  Char)
  , example "Compose3 [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3 [] I  [] Char)
  , example "Compose3 [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3 [] [] I  Char)
  , example "Compose3 [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3 [] [] [] Char)

  , example "Compose3' I  I  I  Char" "\"x\""      (pure 'x' :: Compose3' I  I  I  Char)
  , example "Compose3' I  I  [] Char" "\"x\""      (pure 'x' :: Compose3' I  I  [] Char)
  , example "Compose3' I  [] I  Char" "\"x\""      (pure 'x' :: Compose3' I  [] I  Char)
  , example "Compose3' I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3' I  [] [] Char)
  , example "Compose3' [] I  I  Char" "\"x\""      (pure 'x' :: Compose3' [] I  I  Char)
  , example "Compose3' [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3' [] I  [] Char)
  , example "Compose3' [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3' [] [] I  Char)
  , example "Compose3' [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3' [] [] [] Char)

  , example "MyEither Int String: Left"  "42"      (MyLeft 42     :: MyEither Int String)
  , example "MyEither Int String: Right" "\"foo\"" (MyRight "foo" :: MyEither Int String)

  -- newtypes from Monoid/Semigroup
  , example "Monoid.Dual Int" "2" (pure 2 :: Monoid.Dual Int)
  , example "Monoid.First Int" "2" (pure 2 :: Monoid.First Int)
  , example "Monoid.Last Int" "2" (pure 2 :: Monoid.Last Int)
  , example "Semigroup.Min Int" "2" (pure 2 :: Semigroup.Min Int)
  , example "Semigroup.Max Int" "2" (pure 2 :: Semigroup.Max Int)
  , example "Semigroup.First Int" "2" (pure 2 :: Semigroup.First Int)
  , example "Semigroup.Last Int" "2" (pure 2 :: Semigroup.Last Int)
  , example "Semigroup.WrappedMonoid Int" "2" (Semigroup.WrapMonoid 2 :: Semigroup.WrappedMonoid Int)
  , example "Semigroup.Option Just" "2" (pure 2 :: Semigroup.Option Int)
  , example "Semigroup.Option Nothing" "null" (Semigroup.Option (Nothing :: Maybe Bool))
  ]

jsonEncodingExamples :: [Example]
jsonEncodingExamples =
  [
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
    example "Just Nothing" "null" (Just Nothing :: Maybe (Maybe Int))
  -- infinities cannot be recovered, null is decoded as NaN
  , example "inf :: Double" "null" (Approx $ 1/0 :: Approx Double)
  ]

jsonDecodingExamples :: [Example]
jsonDecodingExamples = [
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
    MaybeExample "Nothing"      "null" (Just Nothing :: Maybe (Maybe Int))
  , MaybeExample "Just"         "1"    (Just $ Just 1 :: Maybe (Maybe Int))
  , MaybeExample "Just Nothing" "null" (Just Nothing :: Maybe (Maybe (Maybe Int)))
  -- Integral values are truncated, and overflowed
  -- https://github.com/bos/aeson/issues/317
  , MaybeExample "Word8 3"    "3"    (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.00" "3.00" (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.14" "3.14" (Nothing :: Maybe Word8)
  , MaybeExample "Word8 -1"   "-1"   (Nothing :: Maybe Word8)
  , MaybeExample "Word8 300"  "300"  (Nothing :: Maybe Word8)
  -- Negative zero year, encoding never produces such:
  , MaybeExample "Day -0000-02-03" "\"-0000-02-03\"" (Just (fromGregorian 0 2 3))
  ]

data Example where
    Example
        :: (Eq a, Show a, ToJSON a, FromJSON a)
        => String -> [L.ByteString] -> a -> Example -- empty bytestring will fail, any p [] == False
    MaybeExample
        :: (Eq a, Show a, FromJSON a)
        => String -> L.ByteString -> Maybe a -> Example

example :: (Eq a, Show a, ToJSON a, FromJSON a)
        => String -> L.ByteString -> a -> Example
example n bs x = Example n [bs] x

data MyEither a b = MyLeft a | MyRight b
  deriving (Generic, Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (MyEither a b) where
    toJSON = genericToJSON defaultOptions { sumEncoding = UntaggedValue }
    toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

instance (FromJSON a, FromJSON b) => FromJSON (MyEither a b) where
    parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

assertJsonExample :: Example -> TestTree
assertJsonExample (Example name bss val) = testCase name $ do
    assertSomeEqual "encode"           bss        (encode val)
    assertSomeEqual "encode/via value" bss        (encode $ toJSON val)
    for_ bss $ \bs ->
        assertEqual "decode"           (Just val) (decode bs)
assertJsonExample (MaybeExample name bs mval) = testCase name $
    assertEqual "decode" mval (decode bs)

assertJsonEncodingExample :: Example -> TestTree
assertJsonEncodingExample (Example name bss val) = testCase name $ do
    assertSomeEqual "encode"           bss (encode val)
    assertSomeEqual "encode/via value" bss (encode $ toJSON val)
assertJsonEncodingExample (MaybeExample name _ _) = testCase name $
    assertFailure "cannot encode MaybeExample"

assertSomeEqual :: (Eq a, Show a, Foldable f) => String -> f a -> a -> IO ()
assertSomeEqual preface expected actual
    | actual `elem` expected = return ()
    | otherwise = assertFailure $ preface
        ++ ": expecting one of " ++ show (toList expected)
        ++ ", got " ++ show actual
