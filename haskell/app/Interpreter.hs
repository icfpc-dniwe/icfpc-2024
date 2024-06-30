import qualified Data.ByteString.Char8 as BS

import ICFP.Encoding.Decode (parseIcfpExpression)
import ICFP.Operators (icfpOperators)
import ICFP.Evaluate (evaluateTopLevel, EvalResult (EvalResult, evalValue))
import ICFP.AST (Value(VString))

main :: IO ()
main = do
  rawExpr <- BS.strip <$> BS.getContents
  case parseIcfpExpression rawExpr of
    Left err -> fail $ "Parse error: " ++ err
    Right expr -> do
      print expr
      case evaluateTopLevel icfpOperators expr of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right (EvalResult { evalValue = VString val }) -> BS.putStrLn val
        Right (EvalResult { evalValue = val }) -> print val
