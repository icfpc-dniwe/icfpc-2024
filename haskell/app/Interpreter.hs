import qualified Data.ByteString.Char8 as BS

import System.IO (hPrint, stderr)

import ICFP.Encoding.Decode (parseIcfpExpression)
import ICFP.AST (Value(VString))

import ICFP.Evaluate.Operators (icfpOperators)
import ICFP.Evaluate (evaluateTopLevel, EvalResult (EvalResult, evalValue))
-- import ICFP.Translate (translateAndEvalExpression)

main :: IO ()
main = do
  rawExpr <- BS.strip <$> BS.getContents
  case parseIcfpExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> do
      hPrint stderr expr
      case evaluateTopLevel icfpOperators expr of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right (EvalResult { evalValue = VString str }) -> BS.putStrLn str
        Right (EvalResult { evalValue = val }) -> print val
      {--translateAndEvalExpression expr >>= \case
        VString str -> BS.putStrLn str
        val -> print val-}
