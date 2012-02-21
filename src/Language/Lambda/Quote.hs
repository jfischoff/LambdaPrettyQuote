{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module Language.Lambda.Quote where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Lambda.Parser
import Language.Lambda.AST
import Text.Parsec (runParser)
import Data.Data
import Control.Applicative
import Data.Generics.Aliases
import Language.Haskell.TH.Instances
import Data.Generics.Uniplate.Data

deriving instance Typeable1 GExpr
deriving instance (Data a) =>  Data (GExpr a) 

--TODO make the pattern quoter and the anti expr
lam :: QuasiQuoter
lam = QuasiQuoter quoteExprExp quoteExprPat undefined undefined

parseExpr :: Monad m => (String, Int, Int) -> String -> m Output
parseExpr (file, line, col) s = result where
    result = case runParser top_expr () file s of
                  Left err  -> fail $ (show err ++ " at file " ++ file ++ " at line " ++ 
                                          show line ++ " at col " ++ show col)
                  Right e   -> return e
    
quoteExprExp :: String -> ExpQ
quoteExprExp s =  do  
    loc <- location
    let pos =  (loc_filename loc,
             fst (loc_start loc),
             snd (loc_start loc))
    parsed_expr <- parseExpr pos s
    appE (varE "meta_to_expr") $ dataToExpQ (const Nothing `extQ` antiExprExp) parsed_expr
             
antiExprExp :: MetaExpr -> Maybe (Q Exp)
antiExprExp  (AntiExpr v)     = Just $ appE (varE "to_meta") $ varE (mkName v)
antiExprExp  _                = Nothing


quoteExprPat :: String -> PatQ
quoteExprPat s =  do  
    loc <- location
    let pos =  (loc_filename loc,
             fst (loc_start loc),
             snd (loc_start loc))
    parsed_expr <- parseExpr pos s
    th_pat <- dataToPatQ (const Nothing `extQ` antiExprPat) parsed_expr
    return $ to_e th_pat
             
antiExprPat :: MetaExpr -> Maybe (Q Pat)
antiExprPat  (AntiExpr v)     = Just $ varP (mkName v)
antiExprPat  _                = Nothing


to_e p = transform to_e' p

to_e' (ConP n xs) = ConP (to_expr_name n) xs
to_e' x = x

to_expr_name name | show name == "MVar" = "Var" 
to_expr_name name | show name == "MApp" = "App" 
to_expr_name name | show name == "MLam" = "Lam"
to_expr_name name | otherwise = name







