{-# LANGUAGE
	PackageImports,
	TypeFamilies,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	TypeSynonymInstances,
	UndecidableInstances,
	Rank2Types #-}
module Taint (Taint, runTaint, TaintT, runTaintT, Tainted, untaint, getData) where

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Trans
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
import "dataenc" Codec.Binary.Base64 (encode)

newtype Tainted t d = Tainted d

instance Monad (Tainted t) where
  return a = Tainted a
  (Tainted m) >>= f = f m

instance Functor (Tainted t) where
  fmap f (Tainted a) = Tainted (f a)

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

taint :: Monad m => d -> TaintT t m (Tainted t d)
taint d = return (return d) -- cute, eh?

class Scrub a b | a -> b where
  untaint :: Monad m => Tainted t a -> TaintT t m b

instance Scrub String String where
  untaint (Tainted d) = return $ encode . unpack . pack $ d

-- This requires UndecidableInstances
instance Scrub a b => Scrub (Maybe a) (Maybe b) where
  untaint (Tainted Nothing)  = return Nothing
  untaint (Tainted (Just d)) = do d' <- (taint d) >>= untaint
  				  return $ Just d'


getData :: Monad m => String -> TaintT t m (Tainted t (Maybe String))
getData key = TaintT $ \kv -> return $ Tainted (lookup key kv)

