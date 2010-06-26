{-# LANGUAGE
	Rank2Types,
	PackageImports,
	TypeSynonymInstances,
	MultiParamTypeClasses
	#-}
module FakeDatabase
	( runDB
	, query
	, FakeDatabase
	) where

-- This is a fake database implementation, showing how a database library can
-- be written around the Taint and Interpolique modules



import Taint
import InterpoliqueQQ

-- Since this is a library, it is allowed to import the Kernel
import Kernel


import "mtl" Control.Monad.Trans
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
import "dataenc" Codec.Binary.Base64 (encode)


-- A type used to index into the Interpolique instances
-- data FakeDatabase = FakeDatabase
import Token

instance Scrub FakeDatabase String String where
  untaint _ (Tainted d) = return $ encode . unpack . pack $ d
instance Scrub FakeDatabase Int String where
  untaint _ (Tainted d) = return $ show d
instance Scrub FakeDatabase Double String where
  untaint _ (Tainted d) = return $ show d


runDB :: Monad m
      => (forall db . TaintT db m (Tainted db (Maybe String)))
      -> (forall db . (Tainted db (Maybe String) -> TaintT db m (Maybe (InterpoliquedString ctx))))
      -> m (Maybe (InterpoliquedString ctx))
runDB myQuery clenser = runTaintT (myQuery >>= clenser)
				  theDB
  where theDB = [ ("thedoc-broker-4", "brkbrkbrk << < > ")
		, ("thedoc-dakami-7", "woot woot &&& woot <foo>")
		, ("thedoc-dakami-6", "with lulz ';")
		]


query :: InterpoliquedString FakeDatabase
      -> ( forall db . TaintT db IO (Tainted db (Maybe String)) )
query theQuery =
     do lift $ putStrLn ("Executing query: " ++
			 show (projInterpoliquedString theQuery))
	getData (projInterpoliquedString theQuery)


