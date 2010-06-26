{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes,
	DeriveDataTypeable,
	TypeSynonymInstances,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances
	#-}
module InterpoliqueQQ
		( interpolique
		, InterpoliquedString -- note we do NOT export the constructor
		, projInterpoliquedString
		, InterpoliqueEscape
		) where

import qualified Data.Foldable as F
import Data.Typeable
import Data.Data
import Data.Generics.Aliases (extQ)
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Token

{-
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
-- This next import requires the dataenc package on Hackage
-- (cabal install dataenc)
import Codec.Binary.Base64 (encode)
-}

import Kernel


-- We don't export the constructor: the only way to get
-- an InterpoliquedString is to use the [$interpolique| ... |]
-- quasi-quoter.
-- We use a dummy type variable to denote the library that
-- we are Interpolique-ing for: be it XML, MySQL, etc.
-- Type inference can then force us to use the proper Interpolique
-- methods for the destination type.
data InterpoliquedString ctx = InterpoliquedString String deriving Show
-- Of course there is a harmless projection operator
projInterpoliquedString :: InterpoliquedString ctx -> String
projInterpoliquedString (InterpoliquedString s) = s

-- This class allows us to use Interpolique for all of the types
-- that are allowed in a SQL query: strings, floats, ints, bools,
-- etc.  It even allows us to escape them in whatever crazy way we
-- want (strings with base64, for example).
-- Users can implement their own instances, giving themselves the
-- ability to escape not-obviously-SQL-compatible types on the fly.
-- The only way to make an instance of this class is via the Scrub
-- interface defined in the security kernel
class InterpoliqueEscape ctx a where
  escapique :: Monad m => ctx -> Tainted t (Maybe a) -> TaintT t m (Maybe String)

-- This requires UndecidableInstances
instance (Scrub ctx a String) => InterpoliqueEscape ctx a where
  escapique ctx (Tainted Nothing) = return Nothing
  escapique ctx (Tainted (Just tainted)) = do c <- untaint ctx (Tainted tainted)
					      return $ Just c


-- Stuff for parsing Interpolique code
lexer = P.makeTokenParser haskellDef
identifier = P.identifier lexer

parse' s = case parse parseInterpolique "(unknown)" s of
		Left err -> fail $ show err
		Right e  -> return e

data InterpoliqueComponent =
    InterpoliqueSQL String
  | InterpoliqueVar String
  deriving (Show, Data, Typeable)

parseInterpolique =
     do interpoliqued <- many $ try $
	     do sql <- manyTill anyChar (try (string "^^"))
		i <- (try (identifier >>= return . Just)) <|> (return Nothing)
		case i of
			Just i' -> return [ InterpoliqueSQL sql
					  , InterpoliqueVar i'
					  ]
			_	-> return [InterpoliqueSQL sql]
	interpoliqued' <- many anyChar
	return $ concat $ interpoliqued ++ [[ InterpoliqueSQL interpoliqued' ]]



-- Implementing the quasi-quoter
interpolique = QuasiQuoter parseInterpoliqueExp parseInterpoliquePat

antiE :: ctx -> InterpoliqueComponent -> Maybe TH.ExpQ
antiE _ (InterpoliqueVar v) =
		Just $ (TH.appE [| escapique FakeDatabase |])
		       (TH.varE $ TH.mkName v)
antiE _ (InterpoliqueSQL s) =
		Just $ (TH.appE [| return . Just |])
		       (TH.litE $ TH.stringL s)

parseInterpoliqueExp :: String -> TH.Q TH.Exp
parseInterpoliqueExp s =
     do p <- parse' s
	let p' = dataToExpQ (const Nothing `extQ` (antiE FakeDatabase)) p
	TH.appE [| \ss -> do ss' <- sequence ss
			     let str = sequence ss'
			     return (str >>= (return . InterpoliquedString . concat)) |] p'

parseInterpoliquePat :: String -> TH.Q TH.Pat
parseInterpoliquePat s = parse' s >>= dataToPatQ (const Nothing)


