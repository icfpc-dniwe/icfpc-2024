module ICFP.AST
  ( Variable
  , Value(..)
  , CallStrategy(..)
  , Expression(..)
  ) where

import qualified Data.ByteString as BS

type Variable = Int

data Value ctx
  = VInt !Int
  | VBool !Bool
  | VString !BS.ByteString
  | VLambda !ctx !Variable !(Expression ctx)
  deriving (Eq, Functor, Foldable, Traversable)

appPrec :: Int
appPrec = 10

showsLambda :: (Show ctx', Show ctx) => Int -> ctx' -> Int -> Expression ctx -> ShowS
showsLambda p _ctx arg body =
  showParen (p > appPrec) $
    showString "L." .
    showsPrec (appPrec + 1) arg .
    showChar ' ' .
    showsPrec (appPrec + 1) body

instance Show ctx => Show (Value ctx) where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VBool b) = showsPrec p b
  showsPrec p (VString s) = showsPrec p s
  showsPrec p (VLambda ctx arg body) = showsLambda p ctx arg body

data CallStrategy
  = CallByName
  | CallByValue
  | CallByNeed
  deriving (Eq)

instance Show CallStrategy where
  show CallByName = "$"
  show CallByValue = "!"
  show CallByNeed = "~"

data Expression ctx
  = EValue !(Value ctx)
  | EUnary !Char !(Expression ctx)
  | EBinary !Char !(Expression ctx) !(Expression ctx)
  | EIf !(Expression ctx) !(Expression ctx) !(Expression ctx)
  | ELambda !Variable !(Expression ctx)
  -- We move this to prim node because it's handled in special ways.
  | EApply !CallStrategy !(Expression ctx) !(Expression ctx)
  | EVariable !Variable
  deriving (Eq, Functor, Foldable, Traversable)

instance Show ctx => Show (Expression ctx) where
  showsPrec p (EValue v) = showsPrec p v
  showsPrec p (EVariable v) = showString "v" . showsPrec p v
  showsPrec p (EUnary op arg) =
    showParen (p > appPrec) $
      showChar 'U' .
      showChar op .
      showChar ' ' .
      showsPrec (appPrec + 1) arg
  showsPrec p (EBinary op arg1 arg2) =
    showParen (p > appPrec) $
      showChar 'B' .
      showChar op .
      showChar ' ' .
      showsPrec (appPrec + 1) arg1 .
      showChar ' ' .
      showsPrec (appPrec + 1) arg2
  showsPrec p (EIf cond then' else') =
    showParen (p > appPrec) $
      showString "? " .
      showsPrec (appPrec + 1) cond .
      showChar ' ' .
      showsPrec (appPrec + 1) then' .
      showChar ' ' .
      showsPrec (appPrec + 1) else'
  showsPrec p (ELambda arg body) = showsLambda p () arg body
  showsPrec p (EApply strategy lambda arg) =
    showParen (p > appPrec) $
      showsPrec (appPrec + 1) strategy .
      showChar ' ' .
      showsPrec (appPrec + 1) lambda .
      showChar ' ' .
      showsPrec (appPrec + 1) arg
