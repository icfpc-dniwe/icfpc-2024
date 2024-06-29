module ICFP.Evaluate
  ( EvalExpression
  , EvalValue
  , ICFPClosure
  , UnaryOp(..)
  , BinaryOp(..)
  , ICFPOperators(..)
  , MonadICFPT
  , evaluate
  , evaluateTopLevel
  , EvalResult(..)
  ) where

import Control.Monad (when)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader (ReaderT, asks, runReaderT, MonadReader (ask))
import Control.Monad.State.Strict (StateT, runStateT, modify, gets, MonadState (state))
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Trans (lift)
import Control.Monad.Except (runExcept)

import ICFP.AST

type EvalExpression = Expression ICFPClosure
type EvalValue = Value ICFPClosure

newtype UnaryOp = UnaryOp (forall m. EvalExpression -> MonadICFPT m EvalValue)
newtype BinaryOp = BinaryOp (forall m. EvalExpression -> EvalExpression -> MonadICFPT m EvalValue)

data ICFPOperators = ICFPOperators { lookupUnaryOp :: !(Char -> Maybe UnaryOp)
                                   , lookupBinaryOp :: !(Char -> Maybe BinaryOp)
                                   }

type Reference = Int
type Variables = HM.HashMap Variable (Either EvalValue Reference)

newtype ICFPClosure = ICFPClosure { closureVariables :: Variables
                                  }
                    deriving (Show, Eq)

data ICFPContext = ICFPContext { icfpOperators :: !ICFPOperators
                               , icfpVariables :: !Variables
                               }

data CachedValue
  = CVExpression { cachedExpression :: !EvalExpression
                 , cachedMemoize :: !Bool
                 }
  | CVResult { cachedValue :: !EvalValue
             , cachedBetaReductions :: !Int
             }
  deriving (Show, Eq)

data ICFPState = ICFPState { icfpBetaReductions :: !Int
                           , icfpVariablesCache :: !(HM.HashMap Reference CachedValue)
                           , icfpNextReference :: !Int
                           }
               deriving (Show, Eq)

type MonadICFPT m a = (MonadError String m) => ReaderT ICFPContext (StateT ICFPState m) a

evaluateVariable :: Variable -> MonadICFPT m EvalValue
evaluateVariable name = do
  vars <- asks icfpVariables
  case HM.lookup name vars of
    Nothing -> throwError $ "evaluate: unknown variable: " ++ show name
    Just (Left v) -> return v
    Just (Right ref) -> do
      cache <- gets $ HM.lookup ref . icfpVariablesCache
      case cache of
        Nothing -> throwError $ "evaluate: unknown reference: " ++ show ref
        Just (CVExpression {..}) -> do
          cachedVal <-
            if cachedMemoize then do
              val <- evaluate cachedExpression
              return $ CVResult { cachedValue = val, cachedBetaReductions = 0 }
            else do
              startBetaReductions <- gets icfpBetaReductions
              val <- evaluate cachedExpression
              endBetaReductions <- gets icfpBetaReductions
              return $ CVResult { cachedValue = val, cachedBetaReductions = endBetaReductions - startBetaReductions }
          modify $ \s -> s { icfpVariablesCache = HM.insert ref cachedVal (icfpVariablesCache s) }
          return $ cachedValue cachedVal
        Just (CVResult {..}) -> do
          when (cachedBetaReductions > 0) $ modify $ \s -> s { icfpBetaReductions = icfpBetaReductions s + cachedBetaReductions }
          return cachedValue

cacheVariable :: Bool -> EvalExpression -> MonadICFPT m Reference
cacheVariable memoize expr = state op
  where op s = (ref, s')
          where ref = icfpNextReference s
                cached = CVExpression { cachedExpression = expr, cachedMemoize = memoize }
                s' = s { icfpNextReference = ref + 1
                       , icfpVariablesCache = HM.insert ref cached (icfpVariablesCache s)
                       }

evaluate :: EvalExpression -> MonadICFPT m EvalValue
evaluate (EValue v) = return v
evaluate (EVariable v) = evaluateVariable v
evaluate (EUnary op arg) = do
  lookupOp <- asks $ lookupUnaryOp . icfpOperators
  case lookupOp op of
    Just (UnaryOp f) -> f arg
    Nothing -> throwError $ "evaluate: unknown unary operator: " ++ show op
evaluate (EBinary op arg1 arg2) = do
  lookupOp <- asks $ lookupBinaryOp . icfpOperators
  case lookupOp op of
    Just (BinaryOp f) -> f arg1 arg2
    Nothing -> throwError $ "evaluate: unknown binary operator: " ++ show op
evaluate (ELambda arg body) = do
  vars <- asks icfpVariables
  let closure = ICFPClosure vars
  return $ VLambda closure arg body
evaluate (EApply strategy lambda argVal) = do
  lambdaV <- evaluate lambda
  case lambdaV of
    VLambda closure arg body -> do
      var <-
        case strategy of
          CallByValue -> Left <$> evaluate argVal
          CallByName -> Right <$> cacheVariable False argVal
          CallByNeed -> Right <$> cacheVariable True argVal
      let vars' = HM.insert arg var $ closureVariables closure
      ctx <- ask
      lift $ runReaderT (evaluate body) (ctx { icfpVariables = vars' })
    _ -> throwError $ "evaluate: invalid lambda: " ++ show lambdaV
evaluate (EIf cond then' else') = do
  b <- evaluate cond
  case b of
    VBool True -> evaluate then'
    VBool False -> evaluate else'
    _ -> throwError $ "evaluate: invalid condition: " ++ show b

data EvalResult = EvalResult { evalValue :: !EvalValue
                             , evalBetaReductions :: !Int
                             }

evaluateTopLevel :: ICFPOperators -> EvalExpression -> Either String EvalResult
evaluateTopLevel ops expr =
  runExcept (mapResult <$> runStateT (runReaderT (evaluate expr) initialContext) initialState)
  where initialContext = ICFPContext { icfpOperators = ops
                                     , icfpVariables = HM.empty
                                     }
        initialState = ICFPState { icfpBetaReductions = 0
                                 , icfpVariablesCache = HM.empty
                                 , icfpNextReference = 0
                                 }
        mapResult (val, s) = EvalResult { evalValue = val, evalBetaReductions = icfpBetaReductions s }
