{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module Levy where

-- exercises / TODO list: 
--
-- do the richard eisenberg thing and give a slightly-dependently-typed compiler to a strongly typed evaluation core
-- to eliminate value tagging
--
-- generate a version with polymorphic types
--
-- generate a version using my style of inference
--
-- add good locations
--
-- type error slicing
--
-- parser
--
-- pretty printer
--
-- figure out a good substitution story
--
-- add cbv and cbn translations from the simply typed lambda calculus
--
-- add a small-step CESK style semantics as a step towards abstract interpretation

import Control.Monad.Reader hiding (fail)
import Prelude hiding (fail)
import Control.Monad.Fail

-- convenience
instance (s ~ String) => MonadFail (Either s) where
  fail = Left

-- types

-- simply typed call-by-push-value, with more complex types this might make a good core.
-- augmented with reference names this might make a good incremental programming framework
--
-- parametricity: consider http://www.eecs.qmul.ac.uk/tech_reports/RR-01-03.pdf sectiomn 7.2
--
-- dependent types? Pi lives in B, Sigma lives in A

-- value-types
data A
  = U B
  | UnitTy
  | IntTy
  | BoolTy
  -- | Sum A A
  -- | Ref A -- add reference types
  deriving (Eq, Show)


-- ctypes
data B
  = F A
  | Arrow A B
  deriving (Eq, Show)

type Name = String

data V
  = Var Name
  | Unit
  | Int Int
  | Bool Bool
  | Times V V
  | Plus  V V
  | Minus V V
  | Equal V V
  | Less  V V
  | Pair V V
  | Thunk M
  deriving Show

data M
  = Force V
  | Return V
  | To M Name M
  | If V M M
  | Fun Name A M
  | Apply M V
  | Rec Name B M
  | Let Name V M
--  | PmSum M Name M Name M -- needs parametricity to not suck
--  | Inl M
--  | Inr M
  deriving Show

-- inference

type Context = [(Name,A)]
type Check = ReaderT Context (Either String)

push :: MonadReader [(a,b)] m => a -> b -> m c -> m c 
-- push :: Monad m => a -> b -> ReaderT [(a,b)] m c -> ReaderT [(a,b)] m c
push a b = local ((a,b):)

-- checkV :: V -> A -> Check ()
checkV :: (MonadReader Context m, MonadFail m) => V -> A -> m ()
checkV v a = do
  a' <- inferV v
  when (a /= a') $ fail $ "expected type " <> show a <> " for value " <> show v <> " but inferred type " <> show a'

inferV :: (MonadReader Context m, MonadFail m) => V -> m A
inferV (Var x) = asks (lookup x) >>= \case
  Nothing -> fail $ "unknown identifier " <> show x
  Just a -> pure a
inferV Unit{} = pure UnitTy
inferV Int{} = pure IntTy
inferV Bool{} = pure BoolTy
-- inferV (U m) = inferM m
inferV (Thunk m)  = U <$> inferM m
inferV (Times v1 v2) = IntTy <$ (checkV v1 IntTy *> checkV v2 IntTy)
inferV (Plus v1 v2) = IntTy <$ (checkV v1 IntTy *> checkV v2 IntTy)
inferV (Minus v1 v2) = IntTy <$ (checkV v1 IntTy *> checkV v2 IntTy)
inferV (Less v1 v2) = BoolTy <$ (checkV v1 IntTy *> checkV v2 IntTy)

-- checkM :: M -> B -> Check ()
checkM :: (MonadReader Context m, MonadFail m) => M -> B -> m ()
checkM m b = do
  b' <- inferM m
  when (b /= b') $ fail $ "expected type " <> show b <> " for computation " <> show m <> " but inferred type " <> show b'

-- inferM :: M -> Check B
inferM :: (MonadReader Context m, MonadFail m) => M -> m B
inferM (Force v) = inferV v >>= \case
  U ty -> pure ty
  ty -> fail $ show v <> " was forced but its type " <> show ty <> " isn't a U-type"
inferM (Return v) = F <$> inferV v
inferM (Rec x t e) = t <$ push x (U t) (inferM e)
inferM (To e1 x e2) = inferM e1 >>= \case
  F a -> push x a $ inferM e2
  ty -> fail $ show e1 <> " was sequenced but its type " <> show ty <> " isn't an F-type"
inferM (Apply m v) = inferM m >>= \case
  Arrow a b -> b <$ checkV v a
  ty -> fail $ show m <> " was used as a function, but its type " <> show ty <> " isn't a fun type"
inferM (If v m1 m2) = do
  checkV v BoolTy
  b <- inferM m1
  b <$ checkM m2 b
inferM (Let x v m) = do
  a <- inferV v
  push x a (inferM m)


-- big-step interpretation

type Env = [(Name, VRuntime)]
data VRuntime
  = VUnit
  | VInt Int
  | VBool Bool
  | VThunk M Env

data CRuntime 
  = CFun Name M Env
  | CReturn VRuntime

type Eval = ReaderT Env (Either String)

-- evaluation for values
-- evalV :: V -> Eval Runtime
evalV :: (MonadReader Env m, MonadFail m) => V -> m VRuntime
evalV (Var x) = do 
  Just e <- asks (lookup x)
  pure e
evalV Unit = pure VUnit
evalV (Int k) = pure $ VInt k
evalV (Bool k) = pure $ VBool k
evalV (Thunk e) = VThunk e <$> ask
evalV (Times v1 v2) = do
  VInt k1 <- evalV v1
  VInt k2 <- evalV v2 
  pure $ VInt (k1 * k2)
evalV (Plus v1 v2) = do
  VInt k1 <- evalV v1
  VInt k2 <- evalV v2 
  pure $ VInt (k1 + k2)
evalV (Minus v1 v2) = do
  VInt k1 <- evalV v1
  VInt k2 <- evalV v2 
  pure $ VInt (k1 - k2)
evalV (Less v1 v2) = do
  VInt k1 <- evalV v1
  VInt k2 <- evalV v2 
  pure $ VBool (k1 < k2)

-- evaluation for computations
-- evalM :: M -> Eval Runtime
evalM :: (MonadReader Env m, MonadFail m) => M -> m CRuntime
evalM (Fun x _ e) = CFun x e <$> ask
evalM (If v1 m1 m2) = do
  VBool b <- evalV v1
  if b then evalM m1 else evalM m2
evalM (Return e) = CReturn <$> evalV e
evalM (To e1 x e2) = do
  CReturn v <- evalM e1
  push x v $ evalM e2
evalM (Force v) = do
  VThunk e env <- evalV v
  local (\_ -> env) $ evalM e
evalM (Apply m v) = do
  CFun x body env <- evalM m 
  arg <- evalV v
  local (\_ -> ((x,arg):env)) $ evalM body
evalM e@(Rec x _ e') = local (\env -> (x,VThunk e env):env) $ evalM e'
evalM (Let x v m) = do
  r <- evalV v 
  push x r $ evalM m
  
