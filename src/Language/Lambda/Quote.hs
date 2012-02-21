{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving #-}
module Language.Lambda.Quote where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Lambda.Parser
import Language.Lambda.AST
import Text.Parsec (runParser)
import Data.Data

deriving instance Typeable1 GExpr
deriving instance (Data a) =>  Data (GExpr a) 

--TODO make the pattern quoter and the anti expr
lam :: QuasiQuoter
lam = QuasiQuoter quoteExprExp undefined undefined undefined

parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr
parseExpr (file, line, col) s = result where
    result = case runParser top_expr () file s of
                  Left err  -> fail $ (show err ++ " at file " ++ file ++ " at line " ++ 
                                          show line ++ " at col " ++ show col)
                  Right e   -> return e
    
quoteExprExp :: String -> ExpQ
quoteExprExp s =  do  loc <- location
                      let pos =  (loc_filename loc,
                                 fst (loc_start loc),
                                 snd (loc_start loc))
                      parsed_expr <- parseExpr pos s
                      dataToExpQ (const Nothing) parsed_expr
                      
