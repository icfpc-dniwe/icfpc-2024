import qualified Data.ByteString.Char8 as BS

import Data.Void (Void)

import ICFP.AST
import ICFP.Encoding.Decode (parseIcfpExpression)

main :: IO ()
main = do
  rawExpr <- BS.strip <$> BS.getContents
  case parseIcfpExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> print (expr :: Expression Void)
