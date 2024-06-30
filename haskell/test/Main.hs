{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Evaluation
import {-@ HTF_TESTS @-} Encoding

main :: IO ()
main = htfMain (htf_importedTests ++ [htf_Main_thisModulesTests])
