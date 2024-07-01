module ICFP.Encoding.Decode
  ( icfpExpression
  , parseIcfpExpression
  ) where

import Data.Functor (($>))
import Control.Applicative ((<|>), optional)
import Data.Word (Word8)
import Text.Megaparsec
    ( (<?>),
      runParser,
      satisfy,
      errorBundlePretty,
      MonadParsec(eof, takeWhile1P),
      ParseErrorBundle,
      Stream(Tokens, Token) )
import Text.Megaparsec.Byte ( string )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Void (Void)
import Data.Maybe (fromJust)

import ICFP.AST
import ICFP.Encoding.Utils (decodeInteger, decodeString, isValidToken)

type MonadICFPParsec e s m = (MonadFail m, MonadParsec e s m, Token s ~ Word8, Tokens s ~ BS.ByteString)

icfpExpression :: MonadICFPParsec e s m => m (Expression ctx)
icfpExpression =
  icfpBoolean
  <|> icfpInteger
  <|> icfpString
  <|> icfpIf
  <|> icfpLambda
  <|> icfpApply
  <|> icfpVariable
  -- These should be tried last so we catch applications as special cases.
  <|> icfpUnaryOperator
  <|> icfpBinaryOperator

token :: MonadICFPParsec e s m => m BS.ByteString
token = takeWhile1P (Just "token") isValidToken

tokenChar :: MonadICFPParsec e s m => m Char
tokenChar = toEnum . fromIntegral <$> satisfy isValidToken <?> "token char"

icfpBoolean :: MonadICFPParsec e s m => m (Expression ctx)
icfpBoolean =
  string "T" $> EValue (VBool True)
  <|> string "F" $> EValue (VBool False)

icfpInteger :: MonadICFPParsec e s m => m (Expression ctx)
icfpInteger = do
  tok <- string "I" *> token
  case decodeInteger tok of
    Just i -> return $ EValue (VInt i)
    Nothing -> fail $ "icfpInteger: invalid integer: " ++ BS8.unpack tok

icfpString :: MonadICFPParsec e s m => m (Expression ctx)
icfpString = string "S" *> (EValue . VString . maybe "" decodeString <$> optional token)

icfpUnaryOperator :: MonadICFPParsec e s m => m (Expression ctx)
icfpUnaryOperator = EUnary <$> (string "U" *> tokenChar) <*> (string " " *> icfpExpression)

icfpBinaryOperator :: MonadICFPParsec e s m => m (Expression ctx)
icfpBinaryOperator = EBinary <$> (string "B" *> tokenChar) <*> (string " " *> icfpExpression) <*> (string " " *> icfpExpression)

icfpIf :: MonadICFPParsec e s m => m (Expression ctx)
icfpIf = EIf <$> (string "? " *> icfpExpression) <*> (string " " *> icfpExpression) <*> (string " " *> icfpExpression)

icfpLambda :: MonadICFPParsec e s m => m (Expression ctx)
-- Cannot fail because `token` checks that the chars are in the range.
icfpLambda = ELambda <$> (fromIntegral . fromJust . decodeInteger <$> (string "L" *> token)) <*> (string " " *> icfpExpression)

icfpApply :: MonadICFPParsec e s m => m (Expression ctx)
icfpApply = EApply <$> icfpCallStrategy <*> (string " " *> icfpExpression) <*> (string " " *> icfpExpression)

icfpCallStrategy :: MonadICFPParsec e s m => m CallStrategy
icfpCallStrategy =
  string "B$" $> CallByName
  <|> string "B!" $> CallByValue
  <|> string "B~" $> CallByNeed

icfpVariable :: MonadICFPParsec e s m => m (Expression ctx)
-- Cannot fail because `token` checks that the chars are in the range.
icfpVariable = EVariable <$> (fromIntegral . fromJust . decodeInteger <$> (string "v" *> token))

parseIcfpExpression :: BS.ByteString -> Either String (Expression ctx)
parseIcfpExpression input =
  case runParser (icfpExpression <* eof) "<stdin>" input of
    Left err -> Left $ errorBundlePretty (err :: ParseErrorBundle BS.ByteString Void)
    Right expr -> Right expr
