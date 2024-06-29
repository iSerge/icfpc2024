{-# LANGUAGE FlexibleContexts #-}

module Evaluate (eval) where

import Base94 (icfp2n, icfp2s, n2icfp, s2icfp)
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import Term

type ReductionCost = Int

eval :: Term -> (Either String Term, ReductionCost)
eval t = runIdentity . (flip runStateT 0 . runExceptT) $ evalTerm t

substTerm :: (MonadState ReductionCost m, MonadError String m) => Int -> Term -> Term -> m Term
substTerm _ _ T = return T
substTerm _ _ F = return T
substTerm _ _ i@(I _) = return i
substTerm _ _ s@(S _) = return s
substTerm n e (V m) | n == m = return e
substTerm _ _ v@(V _) = return v
substTerm n e (U op t) = do
  r <- substTerm n e t
  return $ U op r
substTerm n e (B op t1 t2) = do
  r1 <- substTerm n e t1
  r2 <- substTerm n e t2
  return $ B op r1 r2
substTerm n e (L n' l) = do
  l' <- substTerm n e l
  return $ L n' l'
substTerm n e (If c t f) = do
  c' <- substTerm n e c
  t' <- substTerm n e t
  f' <- substTerm n e f
  return $ If c' t' f'

evalTerm :: (MonadState ReductionCost m, MonadError String m) => Term -> m Term
evalTerm t@T = return t
evalTerm f@F = return f
evalTerm i@(I _) = return i
evalTerm s@(S _) = return s
evalTerm v@(V _) = return v
evalTerm (U Neg t) = do
  v <- evalTerm t
  case v of
    (I n) -> return $ I (-n)
    _ -> throwError $ "Ooperation Neg needs int instead of: " ++ show v
evalTerm (U Not t) = do
  v <- evalTerm t
  case v of
    T -> return F
    F -> return T
    _ -> throwError $ "Operation Not needs bool instead of: " ++ show v
evalTerm (U Str2Int t) = do
  v <- evalTerm t
  case v of
    (S s) -> return $ I ((icfp2n . s2icfp) s)
    _ -> throwError $ "Operation Str->Int needs str instead of: " ++ show v
evalTerm (U Int2Str t) = do
  v <- evalTerm t
  case v of
    (I n) -> return $ S ((icfp2s . n2icfp) n)
    _ -> throwError $ "Operation Int->Str needs int instead of: " ++ show v
evalTerm (B Add t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ I (a + b)
    (_, I _) -> throwError $ "Operation Add needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation Add needs int as second arg: " ++ show v2
    _ -> throwError "Operation Add unrecognised err"
evalTerm (B Sub t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ I (a - b)
    (_, I _) -> throwError $ "Operation Sub needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation Sub needs int as second arg: " ++ show v2
    _ -> throwError "Operation Sub unrecognised err"
evalTerm (B Mul t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ I (a * b)
    (_, I _) -> throwError $ "Operation Mul needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation Mul needs int as second arg: " ++ show v2
    _ -> throwError "Operation Mul unrecognised err"
evalTerm (B Div t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ I (a `quot` b)
    (_, I _) -> throwError $ "Operation Div needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation Div needs int as second arg: " ++ show v2
    _ -> throwError "Operation Div unrecognised err"
evalTerm (B Mod t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ I (a `rem` b)
    (_, I _) -> throwError $ "Operation Mod needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation Mod needs int as second arg: " ++ show v2
    _ -> throwError "Operation Mod unrecognised err"
evalTerm (B LE t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ if a < b then T else F
    (_, I _) -> throwError $ "Operation LE needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation LE needs int as second arg: " ++ show v2
    _ -> throwError "Operation LE unrecognised err"
evalTerm (B Term.GT t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ if a > b then T else F
    (_, I _) -> throwError $ "Operation GT needs int as first arg: " ++ show v1
    (I _, _) -> throwError $ "Operation GT needs int as second arg: " ++ show v2
    _ -> throwError "Operation GT unrecognised err"
evalTerm (B Term.EQ t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I a, I b) -> return $ if a == b then T else F
    (S a, S b) -> return $ if a == b then T else F
    (F, F) -> return T
    (T, T) -> return T
    (T, F) -> return F
    (F, T) -> return F
    v -> throwError $ "Operation EQ needs both arguments to be int, bool or str: " ++ show v
evalTerm (B Or t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (T, T) -> return T
    (F, T) -> return T
    (T, F) -> return T
    (F, F) -> return F
    _ -> throwError "Operation Or needs both arguments to be bool"
evalTerm (B And t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (T, T) -> return T
    (F, T) -> return F
    (T, F) -> return F
    (F, F) -> return F
    _ -> throwError "Operation And needs both arguments to be bool"
evalTerm (B Concat t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (S a, S b) -> return $ S (a ++ b)
    _ -> throwError "Operation Concat needs both arguments to be str"
evalTerm (B Take t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I n, S s) -> return $ S (take n s)
    (_, S _) -> throwError $ "Operation Take needs first argument to be int instead of: " ++ show v1
    (I _, _) -> throwError $ "Operation Take needs second argument to be str instead of: " ++ show v2
    _ -> throwError "Operation Take unrecognized error"
evalTerm (B Drop t1 t2) = do
  v1 <- evalTerm t1
  v2 <- evalTerm t2
  case (v1, v2) of
    (I n, S s) -> return $ S (drop n s)
    (_, S _) -> throwError $ "Operation Drop needs first argument to be int instead of: " ++ show v1
    (I _, _) -> throwError $ "Operation Drop needs second argument to be str instead of: " ++ show v2
    _ -> throwError "Operation Drop unrecognized error"
evalTerm l@(L _ _) = return l
evalTerm (If t1 t2 t3) = do
  c <- evalTerm t1
  case c of
    T -> evalTerm t2
    F -> evalTerm t3
    _ -> throwError $ "Operation If first needs forst argument to be bool instead of : " ++ show c
evalTerm (B Apply t e) = do
  l <- evalTerm t
  case l of
    (L n tree) -> do
      modify (+ 1)
      r <- substTerm n e tree
      evalTerm r
    _ -> throwError $ "Variable substitution should evaluate to lambda instead of: " ++ show l
