module ICFP.Translate
  ( translateTopLevelExpression
  , translateAndEvalExpression
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BB
import Data.Void (Void)
import qualified Data.Aeson as JSON
import Data.Text.Encoding (encodeUtf8, decodeASCII)
import Text.InterpolatedString.Perl6 (q)
import System.IO (stderr)
import System.Process (readProcess)

import ICFP.AST

jsStdlib :: BB.Builder
jsStdlib = [q|
const TRANSLATION_ORDER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n";
const CHARACTER_START = 33;
const CHARACTER_END = 126;
const CHARACTER_COUNT = CHARACTER_END - CHARACTER_START + 1;

if (TRANSLATION_ORDER.length !== CHARACTER_COUNT) {
    throw new Error("Translation order length does not match character count");
}

const stringToInt = (str) => {
    let num = 0;
    const length = str.length;

    for (let i = 0; i < length; i++) {
        const charOffset = TRANSLATION_ORDER.indexOf(str[i]);
        num = num * CHARACTER_COUNT + charOffset;
    }

    return num;
};

const intToString = (num) => {
    if (num === 0) {
        return TRANSLATION_ORDER[0];
    }

    let str = "";
    while (num > 0) {
        const charOffset = num % CHARACTER_COUNT;
        str = TRANSLATION_ORDER[charOffset] + str;
        num = ~~(num / CHARACTER_COUNT);
    }

    return str;
};

const argumentByValue = (value) => () => value;
const argumentByNeed = (gen) => {
    let value = undefined;
    return () => {
        if (value === undefined) {
            value = gen();
        }
        return value;
    };
};
|]

translateValue :: Value Void -> BB.Builder
translateValue (VInt i) = JSON.fromEncoding $ JSON.toEncoding i
translateValue (VBool b) = JSON.fromEncoding $ JSON.toEncoding b
translateValue (VString s) = JSON.fromEncoding $ JSON.toEncoding $ decodeASCII s

translateArgument :: CallStrategy -> Expression Void -> BB.Builder
translateArgument CallByName arg = "() => (" <> translateExpression arg <> ")"
translateArgument CallByValue arg = "argumentByValue(" <> translateExpression arg <> ")"
translateArgument CallByNeed arg = "argumentByNeed(() => (" <> translateExpression arg <> "))"

translateExpression :: Expression Void -> BB.Builder
translateExpression (EValue v) = translateValue v
translateExpression (EVariable v) = "v" <> BB.intDec v <> "()"
translateExpression (EUnary '-' arg) = "-(" <> translateExpression arg <> ")"
translateExpression (EUnary '!' arg) = "!(" <> translateExpression arg <> ")"
translateExpression (EUnary '#' arg) = "stringToInt(" <> translateExpression arg <> ")"
translateExpression (EUnary '$' arg) = "intToString(" <> translateExpression arg <> ")"
translateExpression (EUnary op _arg) = error $ "Unknown unary operator: " <> show op
translateExpression (EBinary '+' arg1 arg2) = "(" <> translateExpression arg1 <> ") + (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '-' arg1 arg2) = "(" <> translateExpression arg1 <> ") - (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '*' arg1 arg2) = "(" <> translateExpression arg1 <> ") * (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '/' arg1 arg2) = "~~((" <> translateExpression arg1 <> ") / (" <> translateExpression arg2 <> "))"
translateExpression (EBinary '%' arg1 arg2) = "(" <> translateExpression arg1 <> ") % (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '<' arg1 arg2) = "(" <> translateExpression arg1 <> ") < (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '>' arg1 arg2) = "(" <> translateExpression arg1 <> ") > (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '=' arg1 arg2) = "(" <> translateExpression arg1 <> ") === (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '&' arg1 arg2) = "(" <> translateExpression arg1 <> ") && (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '|' arg1 arg2) = "(" <> translateExpression arg1 <> ") || (" <> translateExpression arg2 <> ")"
translateExpression (EBinary '.' arg1 arg2) = "(" <> translateExpression arg1 <> ") + (" <> translateExpression arg2 <> ")"
translateExpression (EBinary 'T' arg1 arg2) = "(" <> translateExpression arg2 <> ").slice(0, " <> translateExpression arg1 <> ")"
translateExpression (EBinary 'D' arg1 arg2) = "(" <> translateExpression arg2 <> ").slice(" <> translateExpression arg1 <> ")"
translateExpression (EBinary op _arg1 _arg2) = error $ "Unknown binary operator: " <> show op
translateExpression (EIf cond then' else') = "(" <> translateExpression cond <> ") ? (" <> translateExpression then' <> ") : (" <> translateExpression else' <> ")"
translateExpression (ELambda arg body) = "(v" <> BB.intDec arg <> ") => (" <> translateExpression body <> ")"
translateExpression (EApply strategy lambda arg) = "(" <> translateExpression lambda <> ")(" <> translateArgument strategy arg <> ")"

translateTopLevelExpression :: Expression Void -> BB.Builder
translateTopLevelExpression expr =
  jsStdlib <> "process.stdout.write(JSON.stringify(" <> translateExpression expr <> "));"

translateAndEvalExpression :: Expression Void -> IO (Value Void)
translateAndEvalExpression expr = do
  let !js = translateTopLevelExpression expr
  BB.hPutBuilder stderr js
  rawOutput <- readProcess "node" [] (BSL.unpack $ BB.toLazyByteString js)
  case JSON.eitherDecode (BSL.pack rawOutput) of
    Right (JSON.String t) -> return (VString $ encodeUtf8 t)
    Right (JSON.Number n) -> return (VInt $ floor n)
    Right (JSON.Bool b) -> return (VBool b)
    Right ret -> fail $ "Unexpected JSON output: " <> show ret
    Left err -> fail $ "Failed to decode JSON output: " <> err
