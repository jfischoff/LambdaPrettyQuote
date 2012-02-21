{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Lambda.Quote (lam) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Lambda.Parser
import Text.Parsec (runParser)
import Data.Generics.Aliases
import Data.Generics.Uniplate.Data
 
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
    appE (varE $ mkName "meta_to_expr") $ dataToExpQ (const Nothing `extQ` antiExprExp) parsed_expr
             
antiExprExp :: MetaExpr -> Maybe (Q Exp)
antiExprExp  (AntiExpr v)     = Just $ appE (varE $ mkName "to_meta") $ varE (mkName v)
antiExprExp  _                = Nothing


quoteExprPat :: String -> PatQ
quoteExprPat s =  do  
    loc <- location
    let pos =  (loc_filename loc,
             fst (loc_start loc),
             snd (loc_start loc))
    parsed_expr <- parseExpr pos s
    th_pat <- dataToPatQ (const Nothing `extQ` antiExprPat) parsed_expr
    return $ to_e th_pat where
        to_e p = transform to_e' p

        to_e' (ConP n xs) = ConP (to_expr_name n) xs
        to_e' x = x

        to_expr_name name | show name == "MVar" = mkName "Var" 
        to_expr_name name | show name == "MApp" = mkName "App" 
        to_expr_name name | show name == "MLam" = mkName "Lam"
        to_expr_name name | otherwise           = name
             
antiExprPat :: MetaExpr -> Maybe (Q Pat)
antiExprPat  (AntiExpr v)     = Just $ varP (mkName v)
antiExprPat  _                = Nothing











