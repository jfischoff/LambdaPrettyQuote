{-# LANGUAGE FlexibleContexts #-}
{-- | Parser for the lambda AST built of parsec. No Support for AntiExpr yet. Probably not efficent -}
module Language.Lambda.Parser where
import Text.Parsec 
import Language.Lambda.AST
import Data.Functor.Identity
import Data.List

type M = Identity

{-
data AntiExpr = AntiVar String
              | AntiLam String
              | AntiApp String
           
type Output = (Expr, Maybe AntiExpr)
-}

type Output = Expr

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

parse_lambda :: ParsecT String u M Output
parse_lambda = do
    _ <- char '\\'
    sym  <- parse_sym <?> "lambda argument"
    _ <- char '.'
    expr <- parse_expr <?> "lambda expression"
    return $ Lam sym expr

parse_app :: ParsecT String u M Output
parse_app = do
    expr_0 <- parse_atom <?> "first apply argument"
    spaces
    as <-  sepBy1 parse_atom spaces <?> "other apply arguments"
    return $ foldl' App expr_0 as

parse_atom :: ParsecT String u M Output
parse_atom =  try  (parens'  parse_expr)
          <|> try parse_var 

parse_var :: ParsecT String u M Output
parse_var = do
    spaces
    sym <- parse_sym <?> "Var symbol"
    return $ Var sym 

parse_sym :: ParsecT String u M String
parse_sym = many1 (alphaNum <|> char '_') <?> "symbol"

parens' :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
parens' p = do 
    _ <- char '('
    e <- p
    _ <- char ')'
    return e

















