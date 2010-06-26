{-# LANGUAGE
	MultiParamTypeClasses,
	FunctionalDependencies,
	FlexibleInstances,
	PackageImports,
	Rank2Types,
	UndecidableInstances
	#-}
module Kernel ( Tainted(..)
	      , Scrub(..)
	      , Taint(..), TaintT(..)
	      , runTaint, runTaintT
	      ) where

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Identity

-- This is the security kernel.  It should not be imported by general
-- applications!  Static analyis can be used to enforce this envariant.
-- This kernel should be imported by libraries which support Interpolique,
-- so that they can add context-sensitive escaping procedures to the
-- framework.

newtype TaintT t m a = TaintT { runTaintT' :: [(String, String)] -> m a }
type Taint t a = TaintT t Identity a

runTaint :: (forall t . Taint t a) -> [(String, String)] -> a
runTaint t kv = runIdentity (runTaintT' t kv)

runTaintT :: (forall t . TaintT t m a) -> [(String, String)] -> m a
runTaintT t kv = runTaintT' t kv


instance Monad m => Monad (TaintT t m) where
  return a = TaintT $ \_ -> return a
  m >>= f  = TaintT $ \r ->  do a <- runTaintT' m r
				let m' = f a
				runTaintT' m' r

instance MonadTrans (TaintT t) where
  lift m = TaintT $ \_ -> m




newtype Tainted t d = Tainted d

taint :: Monad m => d -> TaintT t m (Tainted t d)
taint d = return (Tainted d)


{- This instance seems frivolous, might be dangerous
instance Monad (Tainted t) where
  return a = Tainted a
  (Tainted m) >>= f = f m
-}

-- This instance is useful in allowing folks to work with tainted data without
-- first scrubbing.  Note that it passes along the taint.
instance Functor (Tainted t) where fmap f (Tainted a) = Tainted (f a)




class Scrub ctx a b | ctx a -> b where
  untaint :: Monad m => ctx -> Tainted t a -> TaintT t m b

{-
-- The generality of this instance declaration requires UndecidableInstances
instance (Monad m, Scrub ctx a b)
  => Scrub ctx (m a) (m b) where
  untaint ctx (Tainted md) =
	return $
	     do dirty <- md
		let clean = runTaint (taint dirty >>= untaint ctx) []
		return clean
-}

