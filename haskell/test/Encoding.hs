{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Data.Word (Word8)
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import ICFP.AST
import ICFP.Encoding.Decode (parseIcfpExpression)
import ICFP.Encoding.Encode (encodeValue)
import ICFP.Encoding.Utils (stringAlphabet)
import ICFP.Evaluate (EvalExpression)

testDecoding :: BS.ByteString -> EvalExpression -> IO ()
testDecoding input outp = assertEqual (Right outp) (parseIcfpExpression input)

test_basic :: IO ()
test_basic = testDecoding "S'%4}).$%8" (EValue (VString "get index"))

encodeableChar :: Gen Word8
encodeableChar = elements (BS.unpack stringAlphabet)

encodeableString :: Gen BS.ByteString
encodeableString = BS.pack <$> listOf1 encodeableChar

prop_stringsRoundRobin :: Property
prop_stringsRoundRobin = forAll encodeableString $ \str ->
  let encoded = BL.toStrict $ BB.toLazyByteString $ encodeValue (VString str)
  in case parseIcfpExpression encoded :: Either String EvalExpression of
       Right (EValue (VString decoded)) | decoded == str -> True
       e -> error $ "Unexpected result: " ++ show e

main :: IO ()
main = htfMain htf_thisModulesTests
