module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs TUnit             = text "unit"
pp ii vs Zero              = text "zero"
pp ii vs (Suc t)           = text "suc" <>
                             pp ii vs t
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  vs (Free (Global s)) = text s
pp ii vs (i :@: c)         = sep [parensIf (isLam i) (pp ii vs i), 
                                  nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]  
pp ii vs (Lam t c)         = text "\\" <>
                             text (vs !! ii) <>
                             text ":" <>
                             printType t <>
                             text ". " <> 
                             pp (ii+1) vs c
pp ii vs (Let t0 t1)       = text "let" <>
                             text (vs !! ii) <>
                             text "=" <>
                             pp ii vs t0 <>
                             text "in" <>
                             pp (ii+1) vs t1
pp ii vs (As u t)          = pp ii vs u <>
                             text "as" <>
                             printType t
pp ii vs (TTup t0 t1)      = text "(" <>
                             pp ii vs t0 <>
                             text "," <>
                             pp ii vs t1 <>
                             text ")"
pp ii vs (Fst t)           = text "fst" <>
                             pp ii vs t
pp ii vs (Snd t)           = text "snd" <>
                             pp ii vs t
pp ii vs (R t0 t1 t2)      = text "R" <>
                             pp ii vs t0 <>
                             pp ii vs t1 <>
                             pp ii vs t2
                                                         
isLam (Lam _ _) = True
isLam  _      = False
   
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType Unit         = text "Unit"
printType Nat          = text "Nat"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
printType (Tup t1 t2)  = text "(" <>
                         printType t1 <>
                         text "," <>
                         printType t2 <>
                         text ")"


isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv TUnit             = []
fv Zero              = []
fv (Suc t)           = fv t
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (Free _)          = []
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let t0 t1)       = fv t0 ++ fv t1
fv (As u t)          = fv u
fv (Fst t)           = fv t
fv (Snd t)           = fv t
fv (TTup t0 t1)      = fv t0 ++ fv t1
fv (R t0 t1 t2)      = fv t0 ++ fv t1 ++ fv t2
  
---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

