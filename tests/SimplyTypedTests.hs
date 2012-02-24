{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module SimplyTypedTests where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Language.Lambda.SimplyTyped.Arbitrary
import Language.Lambda.SimplyTyped.Syntax
import Language.Lambda.SimplyTyped.TypeCheck
import Language.Lambda.SimplyTyped.Pretty
--import Language.Haskell.TH
--import Language.Haskell.TH.Syntax
--import Language.Haskell.TH.Syntax.Internals
import NLam
import Data.DeriveTH

--test_env_arb  = gens 1 (gen_env  arbitrary sym_arbitrary            :: Gen (Env BaseType String))
test_expr_arb = gens 1 (gen_expr arbitrary sym_arbitrary make_const :: Gen (Expr String BaseType String))

test_gen_env_0 = do 
    env <- test_expr_arb
    assertBool "test_gen_env_0" (length env >= 0)

simply_typed_tests = [
        testGroup "Arbitrary" [
            testCase "test_gen_env_0" test_gen_env_0,
            testProperty "test_arbitrary_type_checks_0" test_arbitrary_type_checks_0
        ],
        testGroup "Type Checking" [
            testCase "test_type_check_var_0" test_type_check_var_0,
            testCase "test_type_check_app_0" test_type_check_app_0,
            testCase "test_type_check_lam_0" test_type_check_lam_0,
            testCase "test_type_check_complex_0" test_type_check_complex_0,
            testCase "test_type_check_complex_1" test_type_check_complex_1
        ]]
        
        
main = quickCheck test_arbitrary_type_checks_0
        
data BaseType = FloatType
               deriving(Show, Eq)
               
type Const = String

make_const (Base FloatType) = return $ Just "1.0"
make_const _ = return Nothing

type_check'  = type_check (const $ Base FloatType)
               
instance Arbitrary BaseType where
    arbitrary = oneof [return FloatType]
    
instance Arbitrary (Expr String BaseType String) where
    arbitrary = gen_expr arbitrary sym_arbitrary make_const
    --shrink    = shrink_expr

is_left (Left _) = True
is_left _        = False

is_right = not . is_left

test_arbitrary_type_checks_0 :: Expr String BaseType String -> Bool
test_arbitrary_type_checks_0 x = is_right $ type_check' $ traceIt x

test_type_check_var_0 = assertBool "test_type_check_var_0" $ 
    is_left $ (type_check' $ Var "test" :: Either String (Language.Lambda.SimplyTyped.Syntax.Type BaseType))
test_type_check_app_0 = assertBool "test_type_check_app_0" $ 
        is_left $ (type_check' $ App (Var "test") (Var "test") :: Either String (Language.Lambda.SimplyTyped.Syntax.Type BaseType))

test_type_check_lam_0 = actual @?= Right expected where
    actual   = type_check' $ Lam "test" (Base FloatType) $ Var "test"
    expected = Arrow (Base FloatType) (Base FloatType)  
    
test_type_check_complex_0 = actual @?= Right expected where
    actual   = type_check' $ App (Lam "test" (Base FloatType) $ Var "test") (Constant "1.0")
    expected = Base FloatType

test_type_check_complex_1 = actual @?= Right expected where
    actual   = type_check' $ Lam "x" (Arrow (Base FloatType) (Arrow (Base FloatType) (Base FloatType))) $ 
        App (Var "x") $ App (Lam "test" (Base FloatType) $ Var "test") (Constant "1.0")
    expected = Arrow (Arrow (Base FloatType) (Arrow (Base FloatType) (Base FloatType))) (Arrow (Base FloatType) (Base FloatType))








