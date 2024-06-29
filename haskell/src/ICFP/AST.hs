module ICFP.AST where

import qualified Data.ByteString as BS

type Variable = Int

data Value ctx
  = VInt !Int
  | VBool !Bool
  | VString !BS.ByteString
  | VLambda !ctx !Variable !(Expression ctx)
  deriving (Show, Eq)

data CallStrategy
  = CallByName
  | CallByValue
  | CallByNeed
  deriving (Show, Eq)

data Expression ctx
  = EValue !(Value ctx)
  | EUnary !Char !(Expression ctx)
  | EBinary !Char !(Expression ctx) !(Expression ctx)
  | EIf !(Expression ctx) !(Expression ctx) !(Expression ctx)
  | ELambda !Variable !(Expression ctx)
  -- We move this to prim node because it's handled in special ways.
  | EApply !CallStrategy !(Expression ctx) !(Expression ctx)
  | EVariable !Variable
  deriving (Show, Eq)
