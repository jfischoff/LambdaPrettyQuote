{-# LANGUAGE FlexibleContexts #-}
{-- | Parser for the lambda AST built of parsec. No Support for AntiExpr yet. Probably not efficent -}
module Language.Lambda.Parser where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token 
import Language.Lambda.AST
import Data.Functor.Identity
import Data.List
import Control.Applicative ((<$>))

type M = Identity

data MetaExpr = MVar MetaSym
              | AntiVar String
              | MApp MetaExpr MetaExpr
              | AntiApp String String
              | MLam MetaSym MetaExpr
              | AntiLam String

data MetaSym = AntiSym String
             | S Sym

type Output = MetaExpr

top_expr :: ParsecT String u M Output
top_expr = do 
    spaces
    e <- parse_expr
    spaces
    eof
    return e
    
parse_expr :: ParsecT String u M Output
parse_expr =  try parse_aexpr 
          <|> try parse_lambda
 
parse_aexpr :: ParsecT String u M Output
parse_aexpr =  try parse_app 
           <|> try parse_atom
           
parse_lambda =  try parse_anti_lambda 
            <|> try parse_lambda'

parse_anti_lambda :: ParsecT String u M Output
parse_anti_lambda = do
    _ <- string "$lam:"
    i <- (identifier haskell)
    return $ AntiLam i

parse_lambda' :: ParsecT String u M Output
parse_lambda' = do
    _ <- char '\\'
    sym  <- parse_sym <?> "lambda argument"
    _ <- char '.'
    expr <- parse_expr <?> "lambda expression"
    return $ MLam sym expr

parse_app = parse_app' 
      -- <|> try parse_anti_app

parse_app' :: ParsecT String u M Output
parse_app' = do
    expr_0 <- parse_atom <?> "first apply argument"
    spaces
    as <-  sepBy1 parse_atom spaces <?> "other apply arguments"
    return $ foldl' MApp expr_0 as

{-    
parse_anti_app :: ParsecT String u M Output
parse_anti_app = do
    _ <- string "$app:"
    i <- (identifier haskell)
    return $ AntiApp i
-}

parse_atom :: ParsecT String u M Output
parse_atom =  try  (parens'  parse_expr)
          <|> try parse_var 
          
parse_var =  try parse_var'
         <|> try parse_anti_var

parse_var' :: ParsecT String u M Output
parse_var' = do
    spaces
    sym <- parse_sym <?> "Var symbol"
    return $ MVar sym 
    
parse_anti_var :: ParsecT String u M Output
parse_anti_var = do
    _ <- string "$var:"
    i <- (identifier haskell)
    return $ AntiVar i

parse_sym =  try parse_anti_sym
         <|> (S <$> parse_sym')
         
    
parse_anti_sym :: ParsecT String u M MetaSym
parse_anti_sym = do
    _ <- string "$sym:"
    i <- (identifier haskell)
    return $ AntiSym i

parse_sym' :: ParsecT String u M Sym
parse_sym' = many1 (alphaNum <|> char '_') <?> "symbol"

parens' :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
parens' p = do 
    _ <- char '('
    e <- p
    _ <- char ')'
    return e

meta_to_expr :: MetaExpr -> Expr
meta_to_expr (MVar x)   = Var (meta_sym_to_sym x)
meta_to_expr (MApp x y) = App (meta_to_expr x) (meta_to_expr y)
meta_to_expr (MLam x y) = Lam (meta_sym_to_sym x) (meta_to_expr y)

meta_sym_to_sym :: MetaSym -> Sym
meta_sym_to_sym (S x) = x



