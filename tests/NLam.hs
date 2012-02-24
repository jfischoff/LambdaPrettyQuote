module NLam where
    
import Language.Lambda.Untyped.Quote
import Language.Lambda.Untyped.Parser
import Language.Haskell.TH

nlam = g_lam (do {mkName `fmap` parse_sym;})
