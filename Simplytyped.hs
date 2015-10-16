module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b LUnit          = TUnit
conversion' b LZero          = Zero
conversion' b (LVar n)       = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LFst t)       = Fst (conversion' b t)
conversion' b (LSnd t)       = Snd (conversion' b t)
conversion' b (App t u)      = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)    = Lam t (conversion' (n:b) u)
conversion' b (LLet n t1 t2) = Let (conversion' b t1) (conversion' (n:b) t2)
conversion' b (LAs u t)      = As (conversion' b u) t
conversion' b (LTup t1 t2)   = TTup (conversion' b t1) (conversion' b t2)
conversion' b (LSuc t)       = Suc (conversion' b t)
conversion' b (LR t0 t1 t2)  = R (conversion' b t0) (conversion' b t1) (conversion' b t2)

 
--- eval ----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub _ _ TUnit                 = TUnit
sub _ _ Zero                  = Zero
sub i t (Fst t')              = Fst (sub i t t')
sub i t (Snd t')              = Snd (sub i t t')
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Let t0 t1)           = Let (sub i t t0) (sub (i+1) t t1)
sub i t (As u t')             = As (sub i t u) t'
sub i t (Suc t')              = Suc (sub i t t')
sub i t (TTup t0 t1)          = TTup (sub i t t0) (sub i t t1)
sub i t (R t0 t1 t2)          = R (sub i t t0) (sub i t t1) (sub i t t2)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ TUnit                 = VUnit
eval _ Zero                  = VZero
eval e (Suc t)               = VSuc (eval e t)
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)       = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 VUnit      -> eval e (sub 0 TUnit u)
                 VZero      -> eval e (sub 0 Zero u)
                 VSuc x     -> eval e (sub 0 (quote (VSuc x)) u)
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t0 t1)           = eval e (sub 0 (quote (eval e t0)) t1)
eval e (As u t)              = eval e u
eval e (Fst t)              = case eval e t of
                                    VTup t0 t1 -> t0
                                    _          -> error "Error de tipo en run-time, el argumento debe ser una tupla"
eval e (Snd t)              = case eval e t of
                                    VTup t0 t1 -> t1
                                    _          -> error "Error de tipo en run-time, el argumento debe ser una tupla"
eval e (TTup t0 t1)          = VTup (eval e t0) (eval e t1)
eval e (R t0 t1 t2)           = case eval e t2 of
                                    VZero  -> eval e t0
                                    VSuc t -> eval e ((t1 :@: (R t0 t1 (quote t))) :@: (quote t))
                                    _      -> error "Error de tipo en run-time, el tercer argumento de R tiene que ser Nat"

                                    

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote VUnit      = TUnit
quote VZero = Zero
quote (VSuc t) = Suc (quote t)
quote (VLam t f) = Lam t f
quote (VTup t0 t1) = TTup (quote t0) (quote t1)

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v

-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' _ _ TUnit     = ret Unit
infer' _ _ Zero      = ret Nat
infer' c e (Suc t)  = infer' c e t >>= \tt->
                            case tt of
                                Nat -> ret Nat
                                _   -> matchError Nat tt
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1) 
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (Let t0 t1) = infer' c e t0 >>= \tt0 -> 
                                infer' (tt0:c) e t1
infer' c e (As u t) = infer' c e u >>= \tu ->
                            if tu == t then ret t else matchError t tu
infer' c e (TTup t0 t1) = infer' c e t0 >>= \tt0 ->
                          infer' c e t1 >>= \tt1 ->
                               ret (Tup tt0 tt1)
infer' c e (Fst t) = infer' c e t >>= \tt ->
                        case tt of
                            Tup tt0 tt1 -> ret tt0
                            _           -> notfunError tt
infer' c e (Snd t) = infer' c e t >>= \tt ->
                        case tt of
                            Tup tt0 tt1 -> ret tt1
                            _           -> notfunError tt
infer' c e (R t0 t1 t2) = infer' c e t0 >>= \tt0 -> 
                          infer' c e t1 >>= \tt1 ->
                          infer' c e t2 >>= \tt2 -> case tt1 of
                                                        Fun t t' -> if t == tt0 && t' == Fun Nat t && tt2 == Nat
                                                                    then ret t
                                                                    else matchError (Fun t t') tt1
                                                        _        -> notfunError tt1
                                                
----------------------------------
