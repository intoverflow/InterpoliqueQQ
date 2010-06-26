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
import Data.Maybe (fromJust)
import Data.ByteString (unpack, pack)
import Data.ByteString.Char8 (unpack, pack)
import "dataenc" Codec.Binary.Base64 (decode, encode)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)


-- A type used to index into the Interpolique instances
data FakeDatabase = FakeDatabase

instance Scrub FakeDatabase String String where
  untaint _ (Tainted d) = do let d' = show . encode . Data.ByteString.unpack . Data.ByteString.Char8.pack $ d
			     return $ "b64d(" ++ d' ++ ")"
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
     do let theQuery' = projInterpoliquedString theQuery
	lift $ putStrLn $ "Executing query:    " ++ show theQuery'
	theQuery'' <- parseQuery theQuery'
	lift $ putStrLn $ "Query evaluates as: " ++ show theQuery''
	getData theQuery''


-- This code is used to parse a query, performing base64 decoding as needed
b64d = Data.ByteString.Char8.unpack . Data.ByteString.pack . fromJust . decode

parseQuery s = case parse parseQuery' "(unknown)" s of
			Left err -> fail $ show err
			Right e  -> return e

lexer = P.makeTokenParser haskellDef
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

parseQuery' =
     do whiteSpace
	q <- many $ try $
		     do clean <- manyTill anyChar (try $ string "b64d(\"")
			b64   <- manyTill anyChar (try $ string "\")")
			return $ clean ++ (b64d b64)
	rest <- many anyChar
	return $ concat $ q ++ [rest]

