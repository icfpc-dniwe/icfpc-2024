import System.IO (stdout)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BB

import ICFP.AST
import ICFP.Encoding.Encode (encodeValue)

main :: IO ()
main = do
  str <- BS.strip <$> BS.getContents
  let encoded = encodeValue $ VString str
  BB.hPutBuilder stdout encoded
  putStrLn ""
