import System.IO (stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Builder as BB

import ICFP.Encoding.DecodeDebug (parseIcfpDebugExpression)
import ICFP.Encoding.Encode (encodeExpression)

main :: IO ()
main = do
  rawExpr <- T.strip <$> T.getContents
  case parseIcfpDebugExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> do
      let encoded = encodeExpression expr
      BB.hPutBuilder stdout encoded
      putStrLn ""
