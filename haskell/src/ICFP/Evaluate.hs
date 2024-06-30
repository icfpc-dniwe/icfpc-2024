module ICFP.Evaluate
  ( EvalExpression
  , EvalValue
  , EvalClosure
  , UnaryOp(..)
  , BinaryOp(..)
  , ICFPOperators(..)
  , MonadEval
  , evaluate
  , evaluateTopLevel
  , VariableValue(..)
  , LambdaClosure(..)
  , EvalResult(..)
  ) where

import Control.Monad (when, unless)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader (ReaderT, asks, runReaderT, MonadReader (local))
import Control.Monad.State.Strict (StateT, runStateT, modify, gets)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Data.Void (Void)
import Control.Monad.ST.Class (MonadST(liftST))

import ICFP.AST

type EvalExpression s = Expression (EvalClosure s)
type EvalValue s = Value (EvalClosure s)

newtype UnaryOp = UnaryOp (forall s. EvalExpression s -> MonadEval s (EvalValue s))
newtype BinaryOp = BinaryOp (forall s. EvalExpression s -> EvalExpression s -> MonadEval s (EvalValue s))

data ICFPOperators = ICFPOperators { lookupUnaryOp :: !(Char -> Maybe UnaryOp)
                                   , lookupBinaryOp :: !(Char -> Maybe BinaryOp)
                                   }

data CachedResult s = CachedResult { cachedValue :: !(EvalValue s)
                                   , cachedBetaReductions :: !Int
                                   }

data EvalVariableValue s = EVVByNameOrNeed { varMemoize :: !Bool
                                           , varExpression :: !(EvalExpression s)
                                           , varVariables :: !(EvalVariables s)
                                           , varResult :: !(STRef s (Maybe (CachedResult s)))
                                           }
                         | EVVByValue { varValue :: !(EvalValue s)
                                      }

appPrec :: Int
appPrec = 10

instance Show (EvalVariableValue s) where
  showsPrec d (EVVByNameOrNeed {..}) =
    showParen (d > appPrec) $
      showString "EVVByNameOrNeed " .
      showsPrec (appPrec + 1) varMemoize .
      showChar ' ' .
      showsPrec (appPrec + 1) varExpression
  showsPrec d (EVVByValue {..}) =
    showParen (d > appPrec) $
      showString "EVVByValue " .
      showsPrec (appPrec + 1) varValue

type EvalVariables s = HM.HashMap Variable (EvalVariableValue s)

newtype EvalClosure s = EvalClosure { evalVariables :: EvalVariables s
                                    }
                      deriving (Show)

data EvalContext s = EvalContext { icfpOperators :: !ICFPOperators
                                 , icfpVariables :: !(EvalVariables s)
                                 }

newtype ICFPState = ICFPState { icfpBetaReductions :: Int
                              }
                  deriving (Show, Eq)

newtype MonadEval s a = MonadEval { runMonadEval :: ReaderT (EvalContext s) (StateT ICFPState (ExceptT String (ST s))) a }
                      deriving (Functor, Applicative, Monad, MonadError String)

localVars :: EvalVariables s -> MonadEval s a -> MonadEval s a
localVars vars = MonadEval . local (\ctx -> ctx { icfpVariables = vars }) . runMonadEval

evaluateVariable :: Variable -> MonadEval s (EvalValue s)
evaluateVariable name = MonadEval $ do
  vars <- asks icfpVariables
  case HM.lookup name vars of
    Nothing -> throwError $ "evaluate: unknown variable: " ++ show name
    Just (EVVByValue v) -> return v
    Just (EVVByNameOrNeed {..}) -> do
      cached <- liftST $ readSTRef varResult
      case cached of
        Just (CachedResult {..}) -> do
          when (cachedBetaReductions > 0) $ modify $ \s -> s { icfpBetaReductions = icfpBetaReductions s + cachedBetaReductions }
          return cachedValue
        Nothing -> do
          newVal <-
            if varMemoize then do
              val <- runMonadEval $ localVars varVariables $ evaluate varExpression
              return $ CachedResult { cachedValue = val, cachedBetaReductions = 0 }
            else do
              startBetaReductions <- gets icfpBetaReductions
              val <- runMonadEval $ localVars varVariables $ evaluate varExpression
              endBetaReductions <- gets icfpBetaReductions
              return $ CachedResult { cachedValue = val, cachedBetaReductions = endBetaReductions - startBetaReductions }
          liftST $ writeSTRef varResult $ Just newVal
          return $ cachedValue newVal

variableByNameOrNeed :: Bool -> EvalExpression s -> MonadEval s (EvalVariableValue s)
variableByNameOrNeed memoize expr = MonadEval $ do
  varResult <- liftST $ newSTRef Nothing
  vars <- asks icfpVariables
  return $ EVVByNameOrNeed { varMemoize = memoize
                           , varExpression = expr
                           , varVariables = vars
                           , varResult = varResult
                           }

checkClosure :: EvalVariables s -> EvalExpression s -> MonadEval s ()
checkClosure _vars (EValue _val) = return ()
checkClosure vars (EVariable v) =
  unless (HM.member v vars) $ throwError $ "checkClosure: unknown variable: " ++ show v
checkClosure vars (EUnary _op arg) = checkClosure vars arg
checkClosure vars (EBinary _op arg1 arg2) = do
  checkClosure vars arg1
  checkClosure vars arg2
-- We can skip checking the inner lambda now; it will be checked later.
checkClosure _vars (ELambda _arg _body) = return ()
checkClosure vars (EApply _strategy lambda arg) = do
  checkClosure vars lambda
  checkClosure vars arg
checkClosure vars (EIf cond then' else') = do
  checkClosure vars cond
  checkClosure vars then'
  checkClosure vars else'

evaluate :: EvalExpression s -> MonadEval s (EvalValue s)
evaluate (EValue v) = return v
evaluate (EVariable v) = evaluateVariable v
evaluate (EUnary op arg) = MonadEval $ do
  lookupOp <- asks $ lookupUnaryOp . icfpOperators
  case lookupOp op of
    Just (UnaryOp f) -> runMonadEval $ f arg
    Nothing -> throwError $ "evaluate: unknown unary operator: " ++ show op
evaluate (EBinary op arg1 arg2) = MonadEval $ do
  lookupOp <- asks $ lookupBinaryOp . icfpOperators
  case lookupOp op of
    Just (BinaryOp f) -> runMonadEval $ f arg1 arg2
    Nothing -> throwError $ "evaluate: unknown binary operator: " ++ show op
evaluate lambda@(ELambda arg body) = do
  vars <- MonadEval $ asks icfpVariables
  checkClosure vars lambda
  return $ VLambda (EvalClosure vars) arg body
evaluate (EApply strategy lambda argVal) = do
  lambdaV <- evaluate lambda
  case lambdaV of
    VLambda closure arg body -> do
      MonadEval $ modify $ \s -> s { icfpBetaReductions = icfpBetaReductions s + 1 }
      reductions <- MonadEval $ gets icfpBetaReductions
      when (reductions > 10000) $ throwError $ "evaluate: too many beta reductions, current expr:\n" ++ show lambdaV ++ "\narg:\n" ++ show argVal
      var <-
        case strategy of
          CallByValue -> EVVByValue <$> evaluate argVal
          CallByName -> variableByNameOrNeed False argVal
          CallByNeed -> variableByNameOrNeed True argVal
      let vars' = HM.insert arg var $ evalVariables closure
      localVars vars' $ evaluate body
    _ -> throwError $ "evaluate: invalid lambda: " ++ show lambdaV
evaluate (EIf cond then' else') = do
  b <- evaluate cond
  case b of
    VBool True -> evaluate then'
    VBool False -> evaluate else'
    _ -> throwError $ "evaluate: invalid condition: " ++ show b

data VariableValue
  = VVByName !(Expression LambdaClosure)
  | VVByNeed !(Expression LambdaClosure) !(Maybe (Value LambdaClosure))
  | VVByValue !(Value LambdaClosure)
  deriving (Show, Eq)

newtype LambdaClosure = LambdaClosure { closureVariables :: HM.HashMap Variable VariableValue }
                      deriving (Show, Eq)

data EvalResult = EvalResult { evalValue :: !(Value LambdaClosure)
                             , evalBetaReductions :: !Int
                             }

freezeClosure :: forall s. EvalClosure s -> MonadEval s LambdaClosure
freezeClosure (EvalClosure vars) = LambdaClosure <$> traverse freezeVariable vars
  where freezeVariable :: EvalVariableValue s -> MonadEval s VariableValue
        freezeVariable (EVVByValue v) = VVByValue <$> traverse freezeClosure v
        freezeVariable (EVVByNameOrNeed {..}) = do
          expr <- traverse freezeClosure varExpression
          if varMemoize then do
            cached <- MonadEval $ liftST $ readSTRef varResult
            result <- traverse (traverse freezeClosure . cachedValue) cached
            return $ VVByNeed expr result
          else
            return $ VVByName expr

evaluateTopLevel :: ICFPOperators -> Expression Void -> Either String EvalResult
evaluateTopLevel ops expr =
  runST $ runExceptT (mapResult <$> runStateT (runReaderT (runMonadEval $ evaluate thawed >>= traverse freezeClosure) initialContext) initialState)
  where initialContext = EvalContext { icfpOperators = ops
                                     , icfpVariables = HM.empty
                                     }
        initialState = ICFPState { icfpBetaReductions = 0
                                 }
        mapResult (val, s) = EvalResult { evalValue = val, evalBetaReductions = icfpBetaReductions s }
        -- Convert the expression to the internal form. `Void` cannot happen, so this is safe.
        thawed = fmap (const undefined) expr
