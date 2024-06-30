module ICFP.Encoding.Encode
  ( encodeValue
  , encodeExpression
  ) where

import qualified Data.ByteString.Builder as BB

import ICFP.AST
import ICFP.Encoding.Utils (encodeInteger', encodeString)

encodeLambda :: Int -> Expression ctx -> BB.Builder
encodeLambda arg body = "L" <> encodeInteger' arg <> " " <> encodeExpression body

encodeValue :: Value ctx -> BB.Builder
encodeValue (VInt i) = "I" <> encodeInteger' i
encodeValue (VBool True) = "T"
encodeValue (VBool False) = "F"
encodeValue (VString s) = "S" <> BB.byteString (encodeString s)
encodeValue (VLambda _ctx arg body) = encodeLambda arg body

encodeCallStrategy :: CallStrategy -> BB.Builder
encodeCallStrategy CallByName = "$"
encodeCallStrategy CallByValue = "!"
encodeCallStrategy CallByNeed = "~"

encodeExpression :: Expression ctx -> BB.Builder
encodeExpression (EValue v) = encodeValue v
encodeExpression (EUnary op arg) = "U" <> BB.char7 op <> " " <> encodeExpression arg
encodeExpression (EBinary op arg1 arg2) = "B" <> BB.char7 op <> " " <> encodeExpression arg1 <> " " <> encodeExpression arg2
encodeExpression (EIf cond then' else') = "I" <> " " <> encodeExpression cond <> " " <> encodeExpression then' <> " " <> encodeExpression else'
encodeExpression (ELambda arg body) = encodeLambda arg body
encodeExpression (EApply strategy lambda arg) = encodeCallStrategy strategy <> " " <> encodeExpression lambda <> " " <> encodeExpression arg
encodeExpression (EVariable v) = "v" <> encodeInteger' v
