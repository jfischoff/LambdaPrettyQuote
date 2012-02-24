{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Text.Parsec
import Debug.Trace.Helpers
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import UntypedTests
import SimplyTypedTests

main = defaultMainWithArgs (simply_typed_tests ++ untyped_tests) ["-a 1"]




