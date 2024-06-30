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
import Data.Void (Void)

testDecoding :: BS.ByteString -> Expression Void -> IO ()
testDecoding input outp = assertEqual (Right outp) (parseIcfpExpression input)

test_basic :: IO ()
test_basic = testDecoding "S'%4}).$%8" (EValue (VString "get index"))

-- Checked by hand.
test_lambdaman6 :: IO ()
test_lambdaman6 = testDecoding lambdaman6 lambdaman6Tree
  where lambdaman6 = "B. SF B$ B$ L\" B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L# ? B= v# I\" v\" B. v\" B$ v$ B- v# I\" Sl I#,"
        lambdaman6Tree =
          EBinary '.'
          (EValue (VString "L"))
          (EApply CallByName
           (EApply CallByName
            (ELambda 1 (EApply CallByName
                        (ELambda 1 (EApply CallByName
                                    (ELambda 2 (EApply CallByName
                                                (EVariable 1)
                                                (EApply CallByName (EVariable 2) (EVariable 2))
                                               ))
                                    (ELambda 2 (EApply CallByName
                                                (EVariable 1)
                                                (EApply CallByName (EVariable 2) (EVariable 2))
                                               ))
                                   ))
                        (ELambda 3 (ELambda 2 (EIf
                                               (EBinary '=' (EVariable 2) (EValue (VInt 1)))
                                               (EVariable 1)
                                               (EBinary '.' (EVariable 1) (EApply CallByName
                                                                           (EVariable 3)
                                                                           (EBinary '-' (EVariable 2) (EValue (VInt 1)))
                                                                          ))
                                              )))
                       ))
            (EValue (VString "."))
           )
           (EValue (VInt 199))
          )


encodeableChar :: Gen Word8
encodeableChar = elements (BS.unpack stringAlphabet)

encodeableString :: Gen BS.ByteString
encodeableString = BS.pack <$> listOf encodeableChar

prop_stringsRoundRobin :: Property
prop_stringsRoundRobin = forAll encodeableString $ \str ->
  let encoded = BL.toStrict $ BB.toLazyByteString $ encodeValue (VString str)
  in case parseIcfpExpression encoded :: Either String (Expression Void) of
       Right (EValue (VString decoded)) | decoded == str -> True
       e -> error $ "Unexpected result: " ++ show e

main :: IO ()
main = htfMain htf_thisModulesTests
