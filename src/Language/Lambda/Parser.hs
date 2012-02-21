{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
{-- | Parser for the lambda AST built of parsec. No Support for AntiExpr yet. Probably not efficent -}
module Language.Lambda.Parser where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token 
import Language.Lambda.AST
import Data.Functor.Identity
import Data.List
import Control.Applicative ((<$>))
import Data.Data

type M = Identity

data MetaExpr = MVar Sym
              | MApp MetaExpr MetaExpr
              | MLam Sym MetaExpr
              | AntiExpr String
              deriving(Show, Eq, Data, Typeable)

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
          <|> try parse_anti_expr
 
parse_aexpr :: ParsecT String u M Output
parse_aexpr =  try parse_app 
           <|> try parse_atom
           
parse_anti_expr :: ParsecT String u M Output
parse_anti_expr = do
    _ <- string "$"
    i <- (identifier haskell)
    return $ AntiExpr i

parse_lambda :: ParsecT String u M Output
parse_lambda = do
    _ <- char '\\'
    spaces
    sym  <- parse_sym <?> "lambda argument"
    _ <- char '.'
    spaces
    expr <- parse_expr <?> "lambda expression"
    return $ MLam sym expr

parse_app :: ParsecT String u M Output
parse_app = do
    expr_0 <- parse_atom <?> "first apply argument"
    spaces
    as <-  sepBy1 parse_atom spaces <?> "other apply arguments"
    return $ foldl' MApp expr_0 as

parse_atom :: ParsecT String u M Output
parse_atom =  try  (parens'  parse_expr)
          <|> try parse_var 
          <|> try parse_anti_expr
          
parse_var :: ParsecT String u M Output
parse_var = do
    spaces
    sym <- parse_sym <?> "Var symbol"
    return $ MVar sym 
    
parse_sym :: ParsecT String u M Sym
parse_sym = many1 (alphaNum <|> char '_') <?> "symbol"

parens' :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
parens' p = do 
    _ <- char '('
    e <- p
    _ <- char ')'
    return e

meta_to_expr :: MetaExpr -> Expr
meta_to_expr (MVar x)   = Var x
meta_to_expr (MApp x y) = App (meta_to_expr x) (meta_to_expr y)
meta_to_expr (MLam x y) = Lam x (meta_to_expr y)

to_meta (Var x) = MVar x
to_meta (App x y) = MApp (to_meta x) (to_meta y)
to_meta (Lam x y) = MLam x (to_meta y)





