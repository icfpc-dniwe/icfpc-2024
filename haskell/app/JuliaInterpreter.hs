import qualified Data.ByteString.Char8 as BS

import System.IO (hPrint, stderr)

import ICFP.Encoding.Decode (parseIcfpExpression)
import ICFP.AST (Value(VString))

import ICFP.Translate (translateAndEvalExpression)

main :: IO ()
main = do
  rawExpr <- BS.strip <$> BS.getContents
  case parseIcfpExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> do
      hPrint stderr expr
      translateAndEvalExpression expr >>= \case
        VString str -> BS.putStrLn str
        val -> print val
