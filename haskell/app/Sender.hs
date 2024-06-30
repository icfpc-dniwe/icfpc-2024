import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Configuration.Dotenv as Dotenv
import qualified Data.HashMap.Strict as HMS
import Network.Wreq (responseBody, Payload (Raw), defaults, header, postWith)
import Control.Lens ((^.), (.~), (&))
import Network.HTTP.Client (RequestBody(RequestBodyBS))

main :: IO ()
main = do
  env <- HMS.fromList <$> Dotenv.parseFile ".env"
  rawExpr <- BS.strip <$> BS.getContents
  let url = "https://boundvariable.space/communicate"
      key = env HMS.! "ACCESS_TOKEN"
      opts = defaults & header "Authorization" .~ ["Bearer " <> BS.pack key]
  r <- postWith opts url $ Raw "application/octet-stream" (RequestBodyBS rawExpr)
  BSL.putStrLn $ r ^. responseBody
