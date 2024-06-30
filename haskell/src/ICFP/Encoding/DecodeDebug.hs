module ICFP.Encoding.DecodeDebug
  ( icfpDebugExpression
  , parseIcfpDebugExpression
  ) where

import Data.Functor (($>))
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Text.Megaparsec
    ( anySingle,
      runParser,
      errorBundlePretty,
      between,
      manyTill,
      MonadParsec(eof),
      ParseErrorBundle,
      Stream(Tokens, Token) )
import Text.Megaparsec.Char ( string, char, space )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

import ICFP.AST
import Data.Text.Encoding (encodeUtf8)

type MonadICFPParsec e s m = (MonadFail m, MonadParsec e s m, Token s ~ Char, Tokens s ~ T.Text)

lexeme :: MonadICFPParsec e s m => m a -> m a
lexeme = L.lexeme space

symbol :: MonadICFPParsec e s m => T.Text -> m T.Text
symbol = L.symbol space

symbol' :: MonadICFPParsec e s m => T.Text -> m T.Text
symbol' = L.symbol' space

icfpDebugExpression :: MonadICFPParsec e s m => m (Expression ctx)
icfpDebugExpression =
  icfpBoolean
  <|> icfpInteger
  <|> icfpString
  <|> icfpIf
  <|> icfpLambda
  <|> icfpApply
  <|> icfpVariable
  <|> icfpUnaryOperator
  <|> icfpBinaryOperator
  <|> between (symbol "(") (symbol ")") icfpDebugExpression

icfpBoolean :: MonadICFPParsec e s m => m (Expression ctx)
icfpBoolean =
  symbol' "true" $> EValue (VBool True)
  <|> symbol' "false" $> EValue (VBool False)

icfpInteger :: MonadICFPParsec e s m => m (Expression ctx)
icfpInteger = EValue . VInt <$> lexeme L.decimal

icfpString :: MonadICFPParsec e s m => m (Expression ctx)
icfpString = EValue . VString . encodeUtf8 . T.pack <$> (char '"' *> manyTill L.charLiteral (lexeme (char '"')))

icfpUnaryOperator :: MonadICFPParsec e s m => m (Expression ctx)
icfpUnaryOperator = EUnary <$> (string "U" *> lexeme anySingle) <*> icfpDebugExpression

icfpBinaryOperator :: MonadICFPParsec e s m => m (Expression ctx)
icfpBinaryOperator = EBinary <$> (string "B" *> lexeme anySingle) <*> icfpDebugExpression <*> icfpDebugExpression

icfpIf :: MonadICFPParsec e s m => m (Expression ctx)
icfpIf = EIf <$> (symbol "?" *> icfpDebugExpression) <*> icfpDebugExpression <*> icfpDebugExpression

icfpLambda :: MonadICFPParsec e s m => m (Expression ctx)
-- Cannot fail because `token` checks that the chars are in the range.
icfpLambda = ELambda <$> (string "L." *> lexeme L.decimal) <*> icfpDebugExpression

icfpApply :: MonadICFPParsec e s m => m (Expression ctx)
icfpApply = EApply <$> icfpCallStrategy <*> icfpDebugExpression <*> icfpDebugExpression

icfpCallStrategy :: MonadICFPParsec e s m => m CallStrategy
icfpCallStrategy =
  symbol "$" $> CallByName
  <|> symbol "!" $> CallByValue
  <|> symbol "~" $> CallByNeed

icfpVariable :: MonadICFPParsec e s m => m (Expression ctx)
-- Cannot fail because `token` checks that the chars are in the range.
icfpVariable = EVariable <$> (string "v" *> lexeme L.decimal)

parseIcfpDebugExpression :: T.Text -> Either String (Expression ctx)
parseIcfpDebugExpression input =
  case runParser (space *> icfpDebugExpression <* eof) "<stdin>" input of
    Left err -> Left $ errorBundlePretty (err :: ParseErrorBundle T.Text Void)
    Right expr -> Right expr
