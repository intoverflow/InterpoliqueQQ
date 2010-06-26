{-# LANGUAGE
	Rank2Types,
	PackageImports,
	TypeSynonymInstances,
	MultiParamTypeClasses
	#-}
module FakeXML
	( asXML
	, FakeXML
	) where

-- This is a fake XML implementation.  Its skips all the hard work for the sake
-- of illustrating the idea.


import Taint
import InterpoliqueQQ

-- Since this is a library, it is allowed to import the Kernel
import Kernel


import "mtl" Control.Monad.Trans
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
import "dataenc" Codec.Binary.Base64 (encode)


-- A type used to index into the Interpolique instances
data FakeXML = FakeXML

instance Scrub FakeXML String String where
  untaint _ (Tainted d) = do let d' = encode . unpack . pack $ d
			     return $ "<base64>" ++ d' ++ "</base64>"
instance Scrub FakeXML Int String where
  untaint _ (Tainted d) = return $ show d
instance Scrub FakeXML Double String where
  untaint _ (Tainted d) = return $ show d


asXML :: InterpoliquedString FakeXML -> TaintT t IO ()
asXML xml =
     do lift $ putStrLn $ "XML: " ++
			  show (projInterpoliquedString xml)
	return ()

