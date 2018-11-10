{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Encoders (module Encoders) where

import Prelude.Compat

import Data.Aeson.Types
import Options
import Types

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------

thNullaryToJSONString :: Nullary -> Value
thNullaryToJSONString = genericToJSON defaultOptions

thNullaryToEncodingString :: Nullary -> Encoding
thNullaryToEncodingString = genericToEncoding defaultOptions

thNullaryParseJSONString :: Value -> Parser Nullary
thNullaryParseJSONString = genericParseJSON defaultOptions


thNullaryToJSON2ElemArray :: Nullary -> Value
thNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

thNullaryToEncoding2ElemArray :: Nullary -> Encoding
thNullaryToEncoding2ElemArray = genericToEncoding opts2ElemArray

thNullaryParseJSON2ElemArray :: Value -> Parser Nullary
thNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


thNullaryToJSONTaggedObject :: Nullary -> Value
thNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

thNullaryToEncodingTaggedObject :: Nullary -> Encoding
thNullaryToEncodingTaggedObject = genericToEncoding optsTaggedObject

thNullaryParseJSONTaggedObject :: Value -> Parser Nullary
thNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
thNullaryToJSONObjectWithSingleField =
  genericToJSON optsObjectWithSingleField

thNullaryToEncodingObjectWithSingleField :: Nullary -> Encoding
thNullaryToEncodingObjectWithSingleField =
  genericToEncoding optsObjectWithSingleField

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryToEncodingString :: Nullary -> Encoding
gNullaryToEncodingString = genericToEncoding defaultOptions

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryToEncoding2ElemArray :: Nullary -> Encoding
gNullaryToEncoding2ElemArray = genericToEncoding opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONTaggedObject :: Nullary -> Value
gNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

gNullaryToEncodingTaggedObject :: Nullary -> Encoding
gNullaryToEncodingTaggedObject = genericToEncoding optsTaggedObject

gNullaryParseJSONTaggedObject :: Value -> Parser Nullary
gNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryToEncodingObjectWithSingleField :: Nullary -> Encoding
gNullaryToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField


--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

-- Unary types
type LiftToJSON f a =
    (a -> Value) -> ([a] -> Value) -> f a -> Value
type LiftToEncoding f a =
    (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding
type LiftParseJSON f a =
    (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)

thSomeTypeToJSON2ElemArray :: SomeType Int -> Value
thSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

thSomeTypeToEncoding2ElemArray :: SomeType Int -> Encoding
thSomeTypeToEncoding2ElemArray = genericToEncoding opts2ElemArray

thSomeTypeLiftToJSON2ElemArray :: LiftToJSON SomeType a
thSomeTypeLiftToJSON2ElemArray = genericLiftToJSON opts2ElemArray

thSomeTypeLiftToEncoding2ElemArray :: LiftToEncoding SomeType a
thSomeTypeLiftToEncoding2ElemArray = genericLiftToEncoding opts2ElemArray

thSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
thSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray

thSomeTypeLiftParseJSON2ElemArray :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSON2ElemArray = genericLiftParseJSON opts2ElemArray


thSomeTypeToJSONTaggedObject :: SomeType Int -> Value
thSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

thSomeTypeToEncodingTaggedObject :: SomeType Int -> Encoding
thSomeTypeToEncodingTaggedObject = genericToEncoding optsTaggedObject

thSomeTypeLiftToJSONTaggedObject :: LiftToJSON SomeType a
thSomeTypeLiftToJSONTaggedObject = genericLiftToJSON optsTaggedObject

thSomeTypeLiftToEncodingTaggedObject :: LiftToEncoding SomeType a
thSomeTypeLiftToEncodingTaggedObject = genericLiftToEncoding optsTaggedObject

thSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject

thSomeTypeLiftParseJSONTaggedObject :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSONTaggedObject = genericLiftParseJSON optsTaggedObject


thSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
thSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

thSomeTypeToEncodingObjectWithSingleField :: SomeType Int -> Encoding
thSomeTypeToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

thSomeTypeLiftToJSONObjectWithSingleField :: LiftToJSON SomeType a
thSomeTypeLiftToJSONObjectWithSingleField = genericLiftToJSON optsObjectWithSingleField

thSomeTypeLiftToEncodingObjectWithSingleField :: LiftToEncoding SomeType a
thSomeTypeLiftToEncodingObjectWithSingleField = genericLiftToEncoding optsObjectWithSingleField

thSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

thSomeTypeLiftParseJSONObjectWithSingleField :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSONObjectWithSingleField = genericLiftParseJSON optsObjectWithSingleField


gSomeTypeToJSON2ElemArray :: SomeType Int -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeToEncoding2ElemArray :: SomeType Int -> Encoding
gSomeTypeToEncoding2ElemArray = genericToEncoding opts2ElemArray

gSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray

gSomeTypeLiftToEncoding2ElemArray :: LiftToEncoding SomeType a
gSomeTypeLiftToEncoding2ElemArray = genericLiftToEncoding opts2ElemArray

gSomeTypeLiftToJSON2ElemArray :: LiftToJSON SomeType a
gSomeTypeLiftToJSON2ElemArray = genericLiftToJSON opts2ElemArray

gSomeTypeLiftParseJSON2ElemArray :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSON2ElemArray = genericLiftParseJSON opts2ElemArray

gSomeTypeToJSONTaggedObject :: SomeType Int -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeToEncodingTaggedObject :: SomeType Int -> Encoding
gSomeTypeToEncodingTaggedObject = genericToEncoding optsTaggedObject

gSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject

gSomeTypeLiftToEncodingTaggedObject :: LiftToEncoding SomeType a
gSomeTypeLiftToEncodingTaggedObject = genericLiftToEncoding optsTaggedObject

gSomeTypeLiftToJSONTaggedObject :: LiftToJSON SomeType a
gSomeTypeLiftToJSONTaggedObject = genericLiftToJSON optsTaggedObject

gSomeTypeLiftParseJSONTaggedObject :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSONTaggedObject = genericLiftParseJSON optsTaggedObject

gSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeToEncodingObjectWithSingleField :: SomeType Int -> Encoding
gSomeTypeToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

gSomeTypeLiftToEncodingObjectWithSingleField :: LiftToEncoding SomeType a
gSomeTypeLiftToEncodingObjectWithSingleField = genericLiftToEncoding optsObjectWithSingleField

gSomeTypeLiftToJSONObjectWithSingleField :: LiftToJSON SomeType a
gSomeTypeLiftToJSONObjectWithSingleField = genericLiftToJSON optsObjectWithSingleField

gSomeTypeLiftParseJSONObjectWithSingleField :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSONObjectWithSingleField = genericLiftParseJSON optsObjectWithSingleField

gSomeTypeToJSONOmitNothingFields :: SomeType Int -> Value
gSomeTypeToJSONOmitNothingFields = genericToJSON optsOmitNothingFields

gSomeTypeToEncodingOmitNothingFields :: SomeType Int -> Encoding
gSomeTypeToEncodingOmitNothingFields = genericToEncoding optsOmitNothingFields


--------------------------------------------------------------------------------
-- Option fields
--------------------------------------------------------------------------------

thOptionFieldToJSON :: OptionField -> Value
thOptionFieldToJSON = genericToJSON optsOptionField

thOptionFieldToEncoding :: OptionField -> Encoding
thOptionFieldToEncoding = genericToEncoding optsOptionField

thOptionFieldParseJSON :: Value -> Parser OptionField
thOptionFieldParseJSON = genericParseJSON optsOptionField

gOptionFieldToJSON :: OptionField -> Value
gOptionFieldToJSON = genericToJSON optsOptionField

gOptionFieldToEncoding :: OptionField -> Encoding
gOptionFieldToEncoding = genericToEncoding optsOptionField

gOptionFieldParseJSON :: Value -> Parser OptionField
gOptionFieldParseJSON = genericParseJSON optsOptionField

thMaybeFieldToJSON :: MaybeField -> Value
thMaybeFieldToJSON = genericToJSON optsOptionField


--------------------------------------------------------------------------------
-- IncoherentInstancesNeeded
--------------------------------------------------------------------------------

-- | This test demonstrates the need for IncoherentInstances. See the definition
-- of 'IncoherentInstancesNeeded' for a discussion of the issue.
--
-- NOTE 1: We only need to compile this test. We do not need to run it.
--
-- NOTE 2: We actually only use the INCOHERENT pragma on specific instances
-- instead of the IncoherentInstances language extension. Therefore, this is
-- only supported on GHC versions >= 7.10.
incoherentInstancesNeededParseJSONString :: FromJSON a => Value -> Parser (IncoherentInstancesNeeded a)
incoherentInstancesNeededParseJSONString = case () of
  _ | True  -> genericParseJSON defaultOptions
    | False -> genericParseJSON defaultOptions

incoherentInstancesNeededToJSON :: ToJSON a => IncoherentInstancesNeeded a -> Value
incoherentInstancesNeededToJSON = case () of
  _ | True  -> genericToJSON defaultOptions
    | False -> genericToJSON defaultOptions

-------------------------------------------------------------------------------
-- EitherTextInt encoders/decodes
-------------------------------------------------------------------------------

thEitherTextIntToJSONUntaggedValue :: EitherTextInt -> Value
thEitherTextIntToJSONUntaggedValue = genericToJSON optsUntaggedValue

thEitherTextIntToEncodingUntaggedValue :: EitherTextInt -> Encoding
thEitherTextIntToEncodingUntaggedValue = genericToEncoding optsUntaggedValue

thEitherTextIntParseJSONUntaggedValue :: Value -> Parser EitherTextInt
thEitherTextIntParseJSONUntaggedValue = genericParseJSON optsUntaggedValue


gEitherTextIntToJSONUntaggedValue :: EitherTextInt -> Value
gEitherTextIntToJSONUntaggedValue = genericToJSON optsUntaggedValue

gEitherTextIntToEncodingUntaggedValue :: EitherTextInt -> Encoding
gEitherTextIntToEncodingUntaggedValue = genericToEncoding optsUntaggedValue

gEitherTextIntParseJSONUntaggedValue :: Value -> Parser EitherTextInt
gEitherTextIntParseJSONUntaggedValue = genericParseJSON optsUntaggedValue

--------------------------------------------------------------------------------
-- Approx encoders/decoders
--------------------------------------------------------------------------------

thApproxToJSONUnwrap :: Approx String -> Value
thApproxToJSONUnwrap = genericToJSON optsUnwrapUnaryRecords

thApproxToEncodingUnwrap :: Approx String -> Encoding
thApproxToEncodingUnwrap = genericToEncoding optsUnwrapUnaryRecords

thApproxParseJSONUnwrap :: Value -> Parser (Approx String)
thApproxParseJSONUnwrap = genericParseJSON optsUnwrapUnaryRecords


thApproxToJSONDefault :: Approx String -> Value
thApproxToJSONDefault = genericToJSON defaultOptions

thApproxToEncodingDefault :: Approx String -> Encoding
thApproxToEncodingDefault = genericToEncoding defaultOptions

thApproxParseJSONDefault :: Value -> Parser (Approx String)
thApproxParseJSONDefault = genericParseJSON defaultOptions

gApproxToJSONUnwrap :: Approx String -> Value
gApproxToJSONUnwrap = genericToJSON optsUnwrapUnaryRecords

gApproxToEncodingUnwrap :: Approx String -> Encoding
gApproxToEncodingUnwrap = genericToEncoding optsUnwrapUnaryRecords

gApproxParseJSONUnwrap :: Value -> Parser (Approx String)
gApproxParseJSONUnwrap = genericParseJSON optsUnwrapUnaryRecords


gApproxToJSONDefault :: Approx String -> Value
gApproxToJSONDefault = genericToJSON defaultOptions

gApproxToEncodingDefault :: Approx String -> Encoding
gApproxToEncodingDefault = genericToEncoding defaultOptions

gApproxParseJSONDefault :: Value -> Parser (Approx String)
gApproxParseJSONDefault = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- OneConstructor encoders/decoders
--------------------------------------------------------------------------------

thOneConstructorToJSONDefault :: OneConstructor -> Value
thOneConstructorToJSONDefault = genericToJSON defaultOptions

thOneConstructorToEncodingDefault :: OneConstructor -> Encoding
thOneConstructorToEncodingDefault = genericToEncoding defaultOptions

thOneConstructorParseJSONDefault :: Value -> Parser OneConstructor
thOneConstructorParseJSONDefault = genericParseJSON defaultOptions

thOneConstructorToJSONTagged :: OneConstructor -> Value
thOneConstructorToJSONTagged = genericToJSON optsTagSingleConstructors

thOneConstructorToEncodingTagged :: OneConstructor -> Encoding
thOneConstructorToEncodingTagged = genericToEncoding optsTagSingleConstructors

thOneConstructorParseJSONTagged :: Value -> Parser OneConstructor
thOneConstructorParseJSONTagged = genericParseJSON optsTagSingleConstructors


gOneConstructorToJSONDefault :: OneConstructor -> Value
gOneConstructorToJSONDefault = genericToJSON defaultOptions

gOneConstructorToEncodingDefault :: OneConstructor -> Encoding
gOneConstructorToEncodingDefault = genericToEncoding defaultOptions

gOneConstructorParseJSONDefault :: Value -> Parser OneConstructor
gOneConstructorParseJSONDefault = genericParseJSON defaultOptions

gOneConstructorToJSONTagged :: OneConstructor -> Value
gOneConstructorToJSONTagged = genericToJSON optsTagSingleConstructors

gOneConstructorToEncodingTagged :: OneConstructor -> Encoding
gOneConstructorToEncodingTagged = genericToEncoding optsTagSingleConstructors

gOneConstructorParseJSONTagged :: Value -> Parser OneConstructor
gOneConstructorParseJSONTagged = genericParseJSON optsTagSingleConstructors
