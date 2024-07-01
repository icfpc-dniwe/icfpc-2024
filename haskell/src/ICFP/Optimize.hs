module ICFP.Optimize
  ( liftSubexprs
  , rewriteY
  ) where

import Control.Monad.State.Strict (State, evalState, MonadTrans (lift), MonadState (state))
import Control.Monad.Writer.Strict (WriterT (runWriterT), MonadWriter (tell))

import ICFP.AST
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)

maxVariable :: Expression ctx -> Int
maxVariable (EValue _) = -1
maxVariable (EVariable v) = v
maxVariable (EUnary _ e) = maxVariable e
maxVariable (EBinary _ e1 e2) = max (maxVariable e1) (maxVariable e2)
maxVariable (EIf e1 e2 e3) = max (maxVariable e1) $ max (maxVariable e2) $ maxVariable e3
maxVariable (ELambda v e) = max v $ maxVariable e
maxVariable (EApply _ e1 e2) = max (maxVariable e1) $ maxVariable e2

newtype OptimizeState = OptimizeState { liftSubexprsNextVariable :: Int
                                      }
                  deriving (Eq, Show)

liftSubexprs :: Expression ctx -> Expression ctx
liftSubexprs e = evalState (extractTopLevelSubnodes e) initialState
  where initialState = OptimizeState { liftSubexprsNextVariable = maxVariable e + 1 }

type LiftSubExprsMonad ctx a = WriterT [(Variable, Expression ctx)] (State OptimizeState) a

extractFreeSubnode :: Expression ctx -> LiftSubExprsMonad ctx (Expression ctx)
extractFreeSubnode e = do
  nextVar <- state $ \s -> (liftSubexprsNextVariable s, s { liftSubexprsNextVariable = liftSubexprsNextVariable s + 1 })
  tell [(nextVar, e)]
  return $ EVariable nextVar

data FreeStatus
  = Trivial
  | Free
  | Bound
  deriving (Eq, Ord, Show)

makeComplex :: FreeStatus -> FreeStatus
makeComplex Trivial = Free
makeComplex s = s

extractIfWorthIt :: FreeStatus -> Expression ctx -> LiftSubExprsMonad ctx (Expression ctx)
extractIfWorthIt Free e = extractFreeSubnode e
extractIfWorthIt _ e = return e

extractFreeSubnodes2 :: (Expression ctx -> Expression ctx -> Expression ctx) -> Variable -> Expression ctx -> Expression ctx -> LiftSubExprsMonad ctx (FreeStatus, Expression ctx)
extractFreeSubnodes2 constr lambdaVar e1 e2 = do
  (free1, e1') <- extractFreeSubnodes lambdaVar e1
  (free2, e2') <- extractFreeSubnodes lambdaVar e2
  if free1 < Bound && free2 < Bound then
    return (Free, constr e1' e2')
  else do
    e1'' <- extractIfWorthIt free1 e1'
    e2'' <- extractIfWorthIt free2 e2'
    return (Bound, constr e1'' e2'')

extractFreeSubnodes3 :: HasCallStack => (Expression ctx -> Expression ctx -> Expression ctx -> Expression ctx) -> Variable -> Expression ctx -> Expression ctx -> Expression ctx -> LiftSubExprsMonad ctx (FreeStatus, Expression ctx)
extractFreeSubnodes3 constr lambdaVar e1 e2 e3 = do
  (free1, e1') <- extractFreeSubnodes lambdaVar e1
  (free2, e2') <- extractFreeSubnodes lambdaVar e2
  (free3, e3') <- extractFreeSubnodes lambdaVar e3
  if free1 < Bound && free2 < Bound && free3 < Bound then
    return (Free, constr e1' e2' e3')
  else do
    e1'' <- extractIfWorthIt free1 e1'
    e2'' <- extractIfWorthIt free2 e2'
    e3'' <- extractIfWorthIt free3 e3'
    return (Bound, constr e1'' e2'' e3'')

checkInnerExtracted :: Variable -> (Variable, Expression ctx) -> LiftSubExprsMonad ctx (Maybe (Variable, Expression ctx))
checkInnerExtracted lambdaVar (subVar, subExpr) = do
  (free, subExpr') <- extractFreeSubnodes lambdaVar subExpr
  if free < Bound then do
    tell [(subVar, subExpr')]
    return $ Nothing
  else
    return $ Just (subVar, subExpr')

addExtractedExpressions :: Expression ctx -> [(Variable, Expression ctx)] -> Expression ctx
addExtractedExpressions = foldr wrap
  where wrap (var, subExpr) e = EApply CallByNeed (ELambda var e) subExpr

expressionHasVariable :: Variable -> Expression ctx -> Bool
expressionHasVariable _lambdaVar (EValue _) = False
expressionHasVariable lambdaVar (EVariable var) = lambdaVar == var
expressionHasVariable lambdaVar (EUnary _op e1) = expressionHasVariable lambdaVar e1
expressionHasVariable lambdaVar (EBinary _op e1 e2) =
  expressionHasVariable lambdaVar e1
  || expressionHasVariable lambdaVar e2
expressionHasVariable lambdaVar (EIf e1 e2 e3) =
  expressionHasVariable lambdaVar e1
  || expressionHasVariable lambdaVar e2
  || expressionHasVariable lambdaVar e3
expressionHasVariable lambdaVar (ELambda newVar body)
  | lambdaVar == newVar = False
  | otherwise = expressionHasVariable lambdaVar body
expressionHasVariable lambdaVar (EApply _strategy e1 e2) =
  expressionHasVariable lambdaVar e1
  || expressionHasVariable lambdaVar e2

extractFreeSubnodes :: HasCallStack => Variable -> Expression ctx -> LiftSubExprsMonad ctx (FreeStatus, Expression ctx)
extractFreeSubnodes _lambdaVar e@(EValue _) = return (Trivial, e)
extractFreeSubnodes lambdaVar e@(EVariable v) = return (status, e)
  where status = if v == lambdaVar then Bound else Trivial
extractFreeSubnodes lambdaVar (EUnary op e1) = do
  (free1, e1') <- extractFreeSubnodes lambdaVar e1
  return (makeComplex free1, EUnary op e1')
extractFreeSubnodes lambdaVar (EBinary op e1 e2) = extractFreeSubnodes2 (EBinary op) lambdaVar e1 e2
extractFreeSubnodes lambdaVar (EIf e1 e2 e3) = extractFreeSubnodes3 EIf lambdaVar e1 e2 e3
extractFreeSubnodes lambdaVar (ELambda newVar body) = do
  -- Collect the free subnodes for the lambbda.
  (e', extracted) <- lift $ runWriterT $ do
    (bodyFree, body') <- extractFreeSubnodes newVar body
    case bodyFree of
      Free -> do
        -- If the whole body is free, replace the lambda with `const`.
        bodyVar <- extractFreeSubnode body'
        return $ ELambda newVar bodyVar
      _ -> return $ ELambda newVar body'
  -- Filter the extracted subnodes that are also free now.
  localExtracted <- catMaybes <$> mapM (checkInnerExtracted lambdaVar) extracted
  let freeStatus
        | not (null localExtracted) || expressionHasVariable lambdaVar e' = Bound
        | otherwise = Trivial
  let e'' = addExtractedExpressions e' localExtracted
  return (freeStatus, e'')
extractFreeSubnodes lambdaVar (EApply strategy e1 e2) = extractFreeSubnodes2 (EApply strategy) lambdaVar e1 e2

extractTopLevelSubnodes :: Expression ctx -> State OptimizeState (Expression ctx)
extractTopLevelSubnodes e@(EValue _) = return e
extractTopLevelSubnodes (EVariable v) = error $ "extractTopLevelSubnodes: unexpected variable: " ++ show v
extractTopLevelSubnodes (EUnary op e1) = EUnary op <$> extractTopLevelSubnodes e1
extractTopLevelSubnodes (EBinary op e1 e2) = EBinary op <$> extractTopLevelSubnodes e1 <*> extractTopLevelSubnodes e2
extractTopLevelSubnodes (EIf e1 e2 e3) = EIf <$> extractTopLevelSubnodes e1 <*> extractTopLevelSubnodes e2 <*> extractTopLevelSubnodes e3
extractTopLevelSubnodes (ELambda var body) = do
  ((_free, body'), extracted) <- runWriterT $ extractFreeSubnodes var body
  return $ addExtractedExpressions (ELambda var body') extracted
extractTopLevelSubnodes (EApply strategy e1 e2) = EApply strategy <$> extractTopLevelSubnodes e1 <*> extractTopLevelSubnodes e2

rewriteY :: Expression ctx -> Expression ctx
rewriteY (EApply _ (ELambda a1_0 (EApply _
                                  (ELambda a2_0 (EApply _ (EVariable a1_1) (EApply _ (EVariable a2_1) (EVariable a2_2))))
                                  (ELambda a3_0 (EApply _ (EVariable a1_2) (EApply _ (EVariable a3_1) (EVariable a3_2))))
                                 )) f)
  | a1_0 == a1_1 && a1_1 == a1_2
    && a2_0 == a2_1 && a2_1 == a2_2
    && a3_0 == a3_1 && a3_1 == a3_2 = EUnary 'Y' (rewriteY f)
rewriteY (ELambda a1_0 (EApply _
                        (ELambda a2_0 (EApply _ (EVariable a1_1) (EApply _ (EVariable a2_1) (EVariable a2_2))))
                        (ELambda a3_0 (EApply _ (EVariable a1_2) (EApply _ (EVariable a3_1) (EVariable a3_2))))
                       ))
  | a1_0 == a1_1 && a1_1 == a1_2
    && a2_0 == a2_1 && a2_1 == a2_2
    && a3_0 == a3_1 && a3_1 == a3_2 = ELambda a1_0 (EUnary 'Y' (EVariable a1_0))
rewriteY e@(EValue _) = e
rewriteY e@(EVariable _) = e
rewriteY (EUnary op e1) = EUnary op (rewriteY e1)
rewriteY (EBinary op e1 e2) = EBinary op (rewriteY e1) (rewriteY e2)
rewriteY (EIf e1 e2 e3) = EIf (rewriteY e1) (rewriteY e2) (rewriteY e3)
rewriteY (ELambda var body) = ELambda var (rewriteY body)
rewriteY (EApply strategy e1 e2) = EApply strategy (rewriteY e1) (rewriteY e2)
