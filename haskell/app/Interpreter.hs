import qualified Data.ByteString.Char8 as BS

import ICFP.Encoding.Decode (parseIcfpExpression)
import ICFP.AST (Value(VString))
import System.IO (stderr, hPrint)

import ICFP.Evaluate.Operators (icfpOperators)
import ICFP.Optimize (liftSubexprs, rewriteY)
import ICFP.Evaluate (evaluateTopLevel, EvalResult (EvalResult, evalValue))

main :: IO ()
main = do
  rawExpr <- BS.strip <$> BS.getContents
  case parseIcfpExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> do
      let expr' = rewriteY expr
      hPrint stderr expr'
      case evaluateTopLevel icfpOperators expr' of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right (EvalResult { evalValue = VString str }) -> BS.putStrLn str
        Right (EvalResult { evalValue = val }) -> print val
