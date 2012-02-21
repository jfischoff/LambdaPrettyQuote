{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Lambda.Quote (lam, g_lam) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Lambda.Parser
import Text.Parsec (runParser)
import Data.Generics.Aliases
import Data.Generics.Uniplate.Data
import Language.Lambda.AST
import Data.Data
import Debug.Trace.Helpers
 
lam :: QuasiQuoter
lam = g_lam parse_sym

g_lam :: (Data s, Show s) => SymParser () s -> QuasiQuoter
g_lam sp = QuasiQuoter (g_quoteExprExp sp) (g_quoteExprPat sp) undefined undefined


parseExpr :: Monad m => SymParser () s -> (String, Int, Int) -> String -> m (Output s)
parseExpr sp (file, line, col) s = result where
    result = case runParser (top_expr sp) () file s of
                  Left err  -> fail $ (show err ++ " at file " ++ file ++ " at line " ++ 
                                          show line ++ " at col " ++ show col)
                  Right e   -> return e
    

    
g_quoteExprExp :: (Data s, Show s) => SymParser () s -> String -> ExpQ
g_quoteExprExp sp s =  do  
    loc <- location
    let pos =  (loc_filename loc,
             fst (loc_start loc),
             snd (loc_start loc))
    parsed_expr <- (parseExpr sp) pos s
    appE (varE $ mkName "meta_to_expr") $ dataToExpQ (const Nothing `extQ` antiExprExp) $ parsed_expr
             
antiExprExp :: MetaExpr Sym -> Maybe (Q Exp)
antiExprExp  (MLam (AntiSym v) x) = Just $ appE (appE (conE $ mkName "MLam") $ appE (conE $ mkName "S") $ varE (mkName v))
                                         $ dataToExpQ (const Nothing `extQ` antiExprExp) x
antiExprExp  (MVar (AntiSym v))   = Just $ appE (conE $ mkName "MVar") $ appE (conE $ mkName "S") $ varE (mkName v)
antiExprExp  (AntiExpr v)         = Just $ appE (varE $ mkName "to_meta") $ varE (mkName v)
antiExprExp  (AntiVar v)          = Just $ [| MVar (S $(varE $ mkName v)) |]
antiExprExp  _                    = Nothing

g_quoteExprPat :: (Data s, Show s) => SymParser () s ->  String -> PatQ
g_quoteExprPat sp s =  do  
    loc <- location
    let pos =  (loc_filename loc,
             fst (loc_start loc),
             snd (loc_start loc))
    parsed_expr <- (parseExpr sp) pos s
    th_pat <- dataToPatQ (const Nothing `extQ` antiExprPat) parsed_expr
    return $ to_e th_pat where
        to_e p = transform to_e' p

        to_e' (ConP n xs) | show n == "MVar" = ConP (to_expr_name n) [collapse_meta_sym $ head xs]
        to_e' (ConP n xs) | show n == "MLam" = ConP (to_expr_name n) ((collapse_meta_sym $ head xs):(tail xs))
        to_e' (ConP n xs) | otherwise        = ConP (to_expr_name n) xs
        to_e' x = x

        to_expr_name name | show name == "MVar" = mkName "Var" 
        to_expr_name name | show name == "MApp" = mkName "App" 
        to_expr_name name | show name == "MLam" = mkName "Lam"
        to_expr_name name | otherwise           = name
        
        collapse_meta_sym (ConP n xs) | nameBase n == "S" = head xs
        collapse_meta_sym p@(ConP n xs) | otherwise = error ("collapse_meta_sym not used on a S " ++ show p)
             
antiExprPat :: MetaExpr Sym -> Maybe (Q Pat)
antiExprPat  (MLam (AntiSym v) x) = Just $ conP (mkName "MLam") [conP (mkName "S") [varP (mkName v)], 
                                        dataToPatQ (const Nothing `extQ` antiExprPat) x]
antiExprPat  (MVar (AntiSym v))   = Just $ conP (mkName "MVar") [conP (mkName "S") [varP (mkName v)]]
antiExprPat  (AntiExpr v)         = Just $ varP (mkName v)
antiExprPat  (AntiVar v)          = Just $ conP (mkName "MVar") [conP (mkName "S") [varP $ mkName v]]
antiExprPat  _                    = Nothing











