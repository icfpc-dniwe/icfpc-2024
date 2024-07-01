module ICFP.Evaluate.Operators
  ( unaryOps
  , binaryOps
  , icfpOperators
  ) where

import Control.Monad ((>=>))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Error.Class (MonadError(throwError))

import ICFP.AST
import ICFP.Evaluate
import ICFP.Encoding.Utils (encodeInteger, decodeInteger, decodeString, encodeString)

type UnaryOpPair = (Char, UnaryOp)

strictUnaryOp :: Char -> (forall s. EvalValue s -> MonadEval s (EvalValue s)) -> (Char, UnaryOp)
strictUnaryOp c f = (c, UnaryOp (evaluate >=> f))

unaryNegate :: UnaryOpPair
unaryNegate = strictUnaryOp '-' $ \case
  VInt i -> return $ VInt (-i)
  arg -> throwError $ "unaryNegate: invalid argument: " ++ show arg

unaryNot :: UnaryOpPair
unaryNot = strictUnaryOp '!' $ \case
  VBool b -> return $ VBool (not b)
  arg -> throwError $ "unaryNot: invalid argument: " ++ show arg

unaryStringToInt :: UnaryOpPair
unaryStringToInt = strictUnaryOp '#' $ \case
  VString s ->
    case decodeInteger (encodeString s) of
      Just i -> return $ VInt i
      Nothing -> throwError $ "unaryStringToInt: invalid integer: " ++ BS8.unpack s
  arg -> throwError $ "unaryStringToInt: invalid argument: " ++ show arg

unaryIntToString :: UnaryOpPair
unaryIntToString = strictUnaryOp '$' $ \case
  VInt i -> return $ VString $ decodeString $ encodeInteger i
  arg -> throwError $ "unaryIntToString: invalid argument: " ++ show arg

unaryFix :: UnaryOpPair
unaryFix = ('Y', UnaryOp f)
  where f e = evaluate $ EApply CallByValue y e
        -- L.1 ($ (L.2 ($ v1 ($ v2 v2))) (L.2 ($ v1 ($ v2 v2))))
        y = ELambda 1 $ EApply CallByValue yHalf yHalf
        yHalf = ELambda 2 $ EApply CallByValue (EVariable 1) (EApply CallByValue (EVariable 2) (EVariable 2))

unaryOps :: HM.HashMap Char UnaryOp
unaryOps = HM.fromList $
  [ unaryNegate
  , unaryNot
  , unaryStringToInt
  , unaryIntToString
  , unaryFix
  ]

type BinaryOpPair = (Char, BinaryOp)

strictBinaryOp :: Char -> (forall s. (EvalValue s, EvalValue s) -> MonadEval s (EvalValue s)) -> BinaryOpPair
strictBinaryOp c f = (c, BinaryOp $ \e1 e2 -> evaluate e1 >>= \v1 -> evaluate e2 >>= \v2 -> f (v1, v2))

binaryAdd :: BinaryOpPair
binaryAdd = strictBinaryOp '+' $ \case
  (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
  (arg1, arg2) -> throwError $ "binaryAdd: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binarySubtract :: BinaryOpPair
binarySubtract = strictBinaryOp '-' $ \case
  (VInt i1, VInt i2) -> return $ VInt (i1 - i2)
  (arg1, arg2) -> throwError $ "binarySubtract: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryMultiply :: BinaryOpPair
binaryMultiply = ('*', BinaryOp f)
  where f _ (EValue (VInt 0)) = return $ VInt 0
        f (EValue (VInt 0)) _ = return $ VInt 0
        f a b = do
          a' <- evaluate a
          b' <- evaluate b
          case (a', b') of
            (VInt i1, VInt i2) -> return $ VInt (i1 * i2)
            _ -> throwError $ "binaryMultiply: invalid arguments: " ++ show a' ++ ", " ++ show b'

binaryDivide :: BinaryOpPair
binaryDivide = strictBinaryOp '/' $ \case
  (VInt i1, VInt i2) -> return $ VInt (i1 `quot` i2)
  (arg1, arg2) -> throwError $ "binaryDivide: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryModulo :: BinaryOpPair
binaryModulo = strictBinaryOp '%' $ \case
  (VInt i1, VInt i2) -> return $ VInt (i1 `rem` i2)
  (arg1, arg2) -> throwError $ "binaryModulo: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryLessThan :: BinaryOpPair
binaryLessThan = strictBinaryOp '<' $ \case
  (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
  (arg1, arg2) -> throwError $ "binaryLessThan: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryGreaterThan :: BinaryOpPair
binaryGreaterThan = strictBinaryOp '>' $ \case
  (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
  (arg1, arg2) -> throwError $ "binaryGreaterThan: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryEqual :: BinaryOpPair
binaryEqual = strictBinaryOp '=' $ \case
  (VInt i1, VInt i2) -> return $ VBool (i1 == i2)
  (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
  (VString s1, VString s2) -> return $ VBool (s1 == s2)
  (arg1, arg2) -> throwError $ "binaryEqual: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryAnd :: BinaryOpPair
binaryAnd = ('&', BinaryOp f)
  where f (EValue (VBool False)) _ = return $ VBool False
        f _ (EValue (VBool False)) = return $ VBool False
        f a b =
          evaluate a >>= \case
            VBool False -> return $ VBool False
            VBool True -> evaluate b
            arg -> throwError $ "binaryAnd: invalid argument: " ++ show arg

binaryOr :: BinaryOpPair
binaryOr = ('|', BinaryOp f)
  where f (EValue (VBool True)) _ = return $ VBool True
        f _ (EValue (VBool True)) = return $ VBool True
        f a b =
          evaluate a >>= \case
            VBool True -> return $ VBool True
            VBool False -> evaluate b
            arg -> throwError $ "binaryOr: invalid argument: " ++ show arg

binaryConcat :: BinaryOpPair
binaryConcat = strictBinaryOp '.' $ \case
  (VString s1, VString s2) -> return $ VString (s1 <> s2)
  (arg1, arg2) -> throwError $ "binaryConcat: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryTake :: BinaryOpPair
binaryTake = strictBinaryOp 'T' $ \case
  (VInt i, VString s) -> return $ VString (BS.take (fromIntegral i) s)
  (arg1, arg2) -> throwError $ "binaryTake: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryDrop :: BinaryOpPair
binaryDrop = strictBinaryOp 'D' $ \case
  (VInt i, VString s) -> return $ VString (BS.drop (fromIntegral i) s)
  (arg1, arg2) -> throwError $ "binaryDrop: invalid arguments: " ++ show arg1 ++ ", " ++ show arg2

binaryOps :: HM.HashMap Char BinaryOp
binaryOps = HM.fromList $
  [ binaryAdd
  , binarySubtract
  , binaryMultiply
  , binaryDivide
  , binaryModulo
  , binaryLessThan
  , binaryGreaterThan
  , binaryEqual
  , binaryAnd
  , binaryOr
  , binaryConcat
  , binaryTake
  , binaryDrop
  ]

icfpOperators :: ICFPOperators
icfpOperators = ICFPOperators { lookupUnaryOp = \op -> HM.lookup op unaryOps
                              , lookupBinaryOp = \op -> HM.lookup op binaryOps
                              }
