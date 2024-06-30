module ICFP.Encoding.Utils
  ( stringAlphabet
  , validTokenStart
  , validTokenEnd
  , isValidToken
  , encodeString
  , decodeString
  , encodeInteger'
  , encodeInteger
  , decodeInteger
  ) where

import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import GHC.Stack (HasCallStack)

stringAlphabet :: BS.ByteString
stringAlphabet
  | BS.length alphabet == fromIntegral validTokenBase = alphabet
  | otherwise = error "stringAlphabet: invalid alphabet"
  where alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

validTokenStart :: Word8
validTokenStart = 33

validTokenEnd :: Word8
validTokenEnd = 126

isValidToken :: Word8 -> Bool
isValidToken c = c >= validTokenStart && c <= validTokenEnd

validTokenBase :: Word8
validTokenBase = validTokenEnd - validTokenStart + 1

decodeString :: HasCallStack => BS.ByteString -> BS.ByteString
decodeString = BS.map mapOne
  where mapOne c = stringAlphabet `BS.index` fromIntegral (c - validTokenStart)

encodeString :: HasCallStack => BS.ByteString -> BS.ByteString
encodeString = BS.map mapOne
  where mapOne c = case BS.elemIndex c stringAlphabet of
          Just i -> validTokenStart + fromIntegral i
          Nothing -> error $ "encodeString: invalid character: " ++ show (toEnum (fromIntegral c) :: Char)

decodeInteger :: BS.ByteString -> Maybe Int
decodeInteger = BS.foldl' foldOne (Just 0)
  where foldOne Nothing _ = Nothing
        foldOne (Just acc) char
          | char < validTokenStart || char > validTokenEnd = Nothing
          | otherwise = Just $ fromIntegral char - fromIntegral validTokenStart + acc * fromIntegral validTokenBase

encodeInteger' :: Int -> BB.Builder
encodeInteger' 0 = BB.word8 validTokenStart
encodeInteger' int0 = encode int0
  where encode 0 = mempty
        encode int = encode nextInt <> BB.word8 (fromIntegral digit + validTokenStart)
          where (nextInt, digit) = int `divMod` fromIntegral validTokenBase

encodeInteger :: Int -> BS.ByteString
encodeInteger = BL.toStrict . BB.toLazyByteString . encodeInteger'
