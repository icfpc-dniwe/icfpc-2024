module ICFP.Translate
  ( translateTopLevelExpression
  , translateAndEvalExpression
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BB
import Data.Void (Void)
import Data.Text.Encoding (encodeUtf8, decodeASCII)
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)
import System.Process (readProcess)

import ICFP.AST

juliaStdlib :: BB.Builder
juliaStdlib = [q|
const TRANSLATION_ORDER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#\$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
const CHARACTER_START :: Int64 = 33
const CHARACTER_END :: Int64 = 126
const CHARACTER_COUNT = CHARACTER_END - CHARACTER_START + 1

if length(TRANSLATION_ORDER) != CHARACTER_COUNT
    error("Translation order length does not match character count")
end

function stringToInt(str::String)::Int64
    num :: Int64 = 0
    for i in 1:length(str)
        charOffset = findfirst(c -> c == str[i], TRANSLATION_ORDER) - 1
        num = num * CHARACTER_COUNT + charOffset
    end
    return num
end

function intToString(num::Int64)::String
    if num == 0
        return TRANSLATION_ORDER[1]
    end
    str = ""
    while num > 0
        charOffset = num % CHARACTER_COUNT + 1
        str = TRANSLATION_ORDER[charOffset] * str
        num = div(num, CHARACTER_COUNT)
    end
    return str
end

Value = Union{Int64, Bool, String, Function}

struct Generator{T <: Function}
    generate::T
end

mutable struct ArgumentByNeed{T <: Function}
    value::Union{Value, Generator{T}}
end

struct ArgumentByName{T <: Function}
    generate::T
end

function getArgument(x::ArgumentByNeed) :: Value
    if x.value isa Generator
        x.value = x.value.generate() :: Value
    end
    x.value
end

function getArgument(x::ArgumentByName) :: Value
    x.generate()
end

function getArgument(x::T)::T where T <: Value
    x
end

# We don't want to use any Julia libraries, hence.
function output(x::Value)::Nothing
    if x isa Int64
        print("i" * string(x))
    elseif x == true
        print("t")
    elseif x == false
        print("f")
    elseif x isa String
        print("s" * x)
    else
        error("Unexpected value type: " * string(x))
    end
end
|]

translateValue :: Value Void -> BB.Builder
translateValue (VInt i) = BB.intDec i
translateValue (VBool True) = "true"
translateValue (VBool False) = "false"
-- Hacky.
translateValue (VString s) = "\"" <> BB.byteString inner <> "\""
  where inner =
          encodeUtf8 $
          T.replace "\"" "\\\"" $
          T.replace "$" "\\$" $
          T.replace "\n" "\\n" $
          T.replace "\\" "\\\\" $
          decodeASCII s

translateArgument :: CallStrategy -> Expression Void -> BB.Builder
translateArgument CallByName arg = "ArgumentByName(() -> (" <> translateExpression arg <> "))"
translateArgument CallByValue arg = translateExpression arg
translateArgument CallByNeed arg = "ArgumentByNeed(() -> (" <> translateExpression arg <> "))"

translateExpression :: Expression Void -> BB.Builder
translateExpression (EValue v) = translateValue v
translateExpression (EVariable v) = "getArgument(v" <> BB.intDec v <> ")"
translateExpression (EUnary '-' arg) = "-(" <> translateExpression arg <> ")"
translateExpression (EUnary '!' arg) = "!(" <> translateExpression arg <> ")"
translateExpression (EUnary '#' arg) = "stringToInt(" <> translateExpression arg <> ")"
translateExpression (EUnary '$' arg) = "intToString(" <> translateExpression arg <> ")"
translateExpression (EUnary op _arg) = error $ "Unknown unary operator: " <> show op
translateExpression (EBinary '+' arg1 arg2) = "(" <> translateExpression arg1 <> ") + (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '-' arg1 arg2) = "(" <> translateExpression arg1 <> ") - (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '*' arg1 arg2) = "(" <> translateExpression arg1 <> ") * (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '/' arg1 arg2) = "div(" <> translateExpression arg1 <> ", " <> translateExpression arg2 <> ")"
translateExpression (EBinary '%' arg1 arg2) = "rem(" <> translateExpression arg1 <> ", " <> translateExpression arg2 <> ")"
translateExpression (EBinary '<' arg1 arg2) = "(" <> translateExpression arg1 <> ") < (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '>' arg1 arg2) = "(" <> translateExpression arg1 <> ") > (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '=' arg1 arg2) = "(" <> translateExpression arg1 <> ") == (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '&' arg1 arg2) = "(" <> translateExpression arg1 <> ") && (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '|' arg1 arg2) = "(" <> translateExpression arg1 <> ") || (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '.' arg1 arg2) = "(" <> translateExpression arg1 <> ") * (" <> translateExpression arg2 <> ")"
translateExpression (EBinary 'T' arg1 arg2) = "(" <> translateExpression arg2 <> ")[1:" <> translateExpression arg1 <> "]"
translateExpression (EBinary 'D' arg1 arg2) = "(" <> translateExpression arg2 <> ")[(" <> translateExpression arg1 <> ")+1:end]"
translateExpression (EBinary op _arg1 _arg2) = error $ "Unknown binary operator: " <> show op
translateExpression (EIf cond then' else') = "(" <> translateExpression cond <> ") ? (" <> translateExpression then' <> ") : (" <> translateExpression else' <> ")"
translateExpression (ELambda arg body) = "(v" <> BB.intDec arg <> ") -> (" <> translateExpression body <> ")"
translateExpression (EApply strategy lambda arg) = "(" <> translateExpression lambda <> ")(" <> translateArgument strategy arg <> ")"

translateTopLevelExpression :: Expression Void -> BB.Builder
translateTopLevelExpression expr =
  juliaStdlib <> "output(" <> translateExpression expr <> ")"

translateAndEvalExpression :: Expression Void -> IO (Value Void)
translateAndEvalExpression expr = do
  let !js = translateTopLevelExpression expr
  -- BB.hPutBuilder stderr js
  rawOutput <- readProcess "julia" ["-O2", "-"] (BSL.unpack $ BB.toLazyByteString js)
  case rawOutput of
    "t" -> return $ VBool True
    "f" -> return $ VBool False
    'i':rest -> return $ VInt $ read rest
    's':rest -> return $ VString $ encodeUtf8 $ T.pack rest
    _ -> fail $ "Unexpected output: " <> rawOutput
