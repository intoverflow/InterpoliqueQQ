{-# LANGUAGE
	Rank2Types,
	PackageImports #-}
module Taint ( Taint, runTaint
	     , TaintT, runTaintT, nestTaintT
	     , SubTaintT(..)
	     , getData
	     , Scrub, Tainted
	     ) where

import "mtl" Control.Monad.Trans

import Kernel



getData :: Monad m => String -> TaintT t m (Tainted t (Maybe String))
getData key = TaintT $ \kv -> return $ Tainted (lookup key kv)


-- The following is for supporting sub-taint, thereby giving us lattices of
-- tainting
newtype SubTaintT r s m = SubTaintT (forall a. TaintT r m a -> TaintT s m a)

nestTaintT :: Monad m
	   => (forall s . SubTaintT r s m -> TaintT s m a)
	   -> TaintT r m a
nestTaintT body = TaintT $ \kv ->
     do let witness (TaintT ma) = lift $
	     do a <- ma kv
		-- TODO: this is a natural place for automatic untainting
		return a
	runTaintT (body (SubTaintT witness)) kv

