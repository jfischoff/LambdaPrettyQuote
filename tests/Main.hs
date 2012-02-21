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
import Language.Lambda.Arbitrary
import Language.Lambda.AST
import Language.Lambda.Quote
import Language.Lambda.Parser
import Language.Lambda.Pretty
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import NLam

instance Arbitrary Expr where
    arbitrary = expr_arb
    shrink    = gexpr_shrink

main = defaultMainWithArgs tests ["-a 100"]

tests = [
        testGroup "Parse Var" [
            testCase "test_parse_var_0" test_parse_var_0
        ],
        testGroup "Parse Lam" [
            testCase "test_parse_lam_0" test_parse_lam_0,
            testCase "test_parse_lam_1" test_parse_lam_1
        ],
        testGroup "Parse App" [
            testCase "test_parse_app_0" test_parse_app_0,
            testCase "test_parse_app_1" test_parse_app_1
        ],
        testGroup "Complex Expression" [
            testCase "test_parse_exp_0" test_parse_exp_0,
            testCase "test_parse_exp_1" test_parse_exp_1,
            testProperty "prop_parse_is_inverse_of_ppr" $ mapSize (const 10) prop_parse_is_inverse_of_ppr
        ],
        testGroup "AntiQuotes Expr" [
            testCase "test_antivar_0" test_antivar_0,
            testCase "test_antivar_1" test_antivar_1,
            testCase "test_antivar_2" test_antivar_2,
            testCase "test_antivar_3" test_antivar_3,
            testCase "test_antivar_4" test_antivar_4,
            testCase "test_antivar_4" test_antivar_5
        ],
        testGroup "AntiQuotes Pat" [
            testCase "test_antipat_0" test_antipat_0,
            testCase "test_antipat_1" test_antipat_1,
            testCase "test_antipat_2" test_antipat_2,
            testCase "test_antipat_3" test_antipat_3,
            testCase "test_antipat_4" test_antipat_4,
            testCase "test_antipat_5" test_antipat_5,
            testCase "test_antipat_6" test_antipat_6,
            testCase "test_antipat_6" test_antipat_7
        ]
    ]
    
test_parse_var_0 = (meta_to_expr actual) @?= expected where
    (Right actual) = runParser (parse_var parse_sym) () "" "x"
    expected = Var "x"
    
test_parse_lam_0 = actual @?= expected where
    result = runParser (parse_lambda parse_sym) () "" "\\x.x"
    actual = case result of
        Right x -> meta_to_expr x
        Left x -> error $ show x
    expected = Lam "x" $ Var "x"


test_parse_lam_1 = actual @?= expected where
    result = runParser (parse_expr parse_sym) () "" "(\\x.x)"
    actual = case result of
        Right x -> meta_to_expr x
        Left x -> error $ show x
    expected = Lam "x" $ Var "x" 
   
test_parse_app_0 = actual @?= expected where
    result = runParser (parse_app parse_sym) () "" "x y"
    actual = case result of
        Right x -> meta_to_expr x
        Left x -> error $ show x
    expected = App (Var "x") $ Var "y"

test_parse_app_1 = actual @?= expected where
    result = runParser (parse_expr parse_sym) () "" "(x y)"
    actual = case result of
        Right x -> meta_to_expr x
        Left x -> error $ show x
    expected = App (Var "x") $ Var "y"
   
test_parse_exp_0 = actual @?= expected where
    actual = [lam| ((\x.x) y) |]
    expected = App (Lam "x" $ Var "x") $ Var "y"


test_parse_exp_1 = actual @?= expected where
    result = runParser (parse_expr parse_sym) () "" "((\\x.x) y)"
    actual = case result of
        Right x -> meta_to_expr x
        Left x -> error $ show x
    expected = App (Lam "x" $ Var "x") $ Var "y"
    
run_p = runParser (parse_expr parse_sym) () ""
    
prop_parse_is_inverse_of_ppr :: Expr -> Bool
prop_parse_is_inverse_of_ppr x = result where
    parsed = runParser (parse_expr parse_sym) () "" $ Language.Lambda.Pretty.ppr x
    result = case parsed of
        Right e -> meta_to_expr e == x
        Left _ -> trace (show x) False 
        
 
test_antivar_0 = actual @?= expected where
    actual = [lam| (\x.$y) |]
    y = Var "yo"
    expected = Lam "x" $ y

test_antivar_1 = actual @?= expected where
    actual = [lam| (x $y) |]
    y = Lam "yo" $ Var "y"
    expected = App (Var "x") $ y

test_antivar_2 = actual @?= expected where
    actual = [lam| (x $y) |]
    y = App (Lam "yo" $ Var "y") (Var "h")
    expected = App (Var "x") $ y
    
test_antivar_3 = actual @?= expected where
    actual = [lam| (\^x.y) |]
    x = "hey"
    expected = [lam| (\hey.y) |]
    
test_antivar_4 = actual @?= expected where
    actual = [lam| (^x y) |]
    x = "hey"
    expected = [lam| (hey y) |]

test_antivar_5 = actual @?= expected where
    actual = [lam| (x *y) |]
    y = "hey"
    expected = [lam| (x hey) |]

test_antipat_0 = f input @?= expected where
    f [lam| \x.$y |] = y
    input    = [lam| \x.$expected |]
    expected = [lam| (x y) |]
    
test_antipat_1 = f input @?= expected where
    f [lam| $y |] = y
    input    = [lam| $expected |]
    expected = [lam| x |]

test_antipat_2 = f input @?= expected where
    f [lam| (x $y) |] = y
    input    = [lam| (x $expected) |]
    expected = [lam| \x.(\y. x y) |]
    
test_antipat_3 = f input @?= expected where
    f [lam| \^x. y |] = x
    input    = [lam| \^expected.y |]
    expected = "hey"

test_antipat_4 = f input @?= expected where
    f [lam| \x. *y |] = y
    input    = [lam| \x. *expected |]
    expected = "hey"

test_antipat_5 = f input @?= expected where
    f [lam| (x ^y) |] = y
    input    = [lam| (x ^expected) |]
    expected = "yo"   
    
test_antipat_6 = f input @?= expected where
    f [lam| \^x.(\^y. x y) |] = (x, y)
    input    = [lam| \^x'.(\^y'. x y) |]
    expected = (x', y')
    x' = "hey"
    y' = "you"

test_antipat_7 = f input @?= expected where
    f [nlam| \^x.(\^y. x y) |] = (x, y)
    input    = [nlam| \^x'.(\^y'. x y) |]
    expected = (x', y')
    x' = mkName "hey"
    y' = mkName "you"


