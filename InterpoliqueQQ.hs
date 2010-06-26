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
data ContextString ctx = ContextString ctx String
projContextString :: ContextString ctx -> String
projContextString (ContextString ctx s) = s
mcConcat :: [Maybe (ContextString ctx)] -> Maybe (InterpoliquedString ctx)
mcConcat mcss =
     do let mcss' = sequence mcss -- converts from [Maybe a] to Maybe [a]
	mcss' >>= \l -> return $ InterpoliquedString $ concat $ map projContextString l

class InterpoliqueEscape ctx a where
  escapique :: Monad m => Tainted t (Maybe a)
		       -> TaintT t m (Maybe (ContextString ctx))

contextUntaint :: (Scrub ctx a b, Monad m) => ctx -> Tainted t a -> TaintT t m (ctx, b)
contextUntaint ctx a =
     do b <- untaint ctx a
	return (ctx, b)

-- This requires UndecidableInstances
instance (Scrub ctx a String) => InterpoliqueEscape ctx a where
  escapique (Tainted Nothing) = return Nothing
  escapique (Tainted (Just tainted)) =
	     do (ctx, c) <- contextUntaint undefined (Tainted tainted)
		return $ Just (ContextString ctx c)


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

antiE :: InterpoliqueComponent -> Maybe TH.ExpQ
antiE (InterpoliqueVar v) = Just $ TH.appE [| escapique |]
					   (TH.varE $ TH.mkName v)
antiE (InterpoliqueSQL s) = Just $ TH.appE [| return . Just . ContextString undefined |]
					   (TH.litE $ TH.stringL s)
{-
antiE :: TH.Name -> InterpoliqueComponent -> Maybe TH.ExpQ
antiE ctx (InterpoliqueVar v) = Just $
 	  foldl (TH.appE)
		[| escapique |]
		[ TH.sigE (TH.varE $ TH.mkName "undefined") (TH.varT ctx)
		, TH.varE $ TH.mkName v
		]
antiE _ (InterpoliqueSQL s) =
		Just $ (TH.appE [| return . Just |])
		       (TH.litE $ TH.stringL s)
-}

parseInterpoliqueExp :: String -> TH.Q TH.Exp
parseInterpoliqueExp s =
     do p <- parse' s
	let p' = dataToExpQ (const Nothing `extQ` antiE) p
	let builder = [| \ss -> do ss' <- sequence ss
				   return $ mcConcat ss' |]
	TH.appE builder p'
{-
parseInterpoliqueExp s =
     do ctx <- TH.newName "ctx"
	p <- parse' s
	let p' = dataToExpQ (const Nothing `extQ` (antiE ctx)) p
	let builder = [| \ss ->
			  do ss' <- sequence ss -- convert [m (Maybe a)] to m [Maybe a]
			     let str = sequence ss' -- convert [Maybe a] to Maybe [a]
			     return (str >>= (return . InterpoliquedString . concat))
		   |]
	let sig = (TH.AppT (TH.ConT ''InterpoliquedString) (TH.VarT ctx))
	TH.sigE (TH.appE builder p')
		(return $ TH.ForallT [TH.PlainTV ctx] [] sig)
-}

parseInterpoliquePat :: String -> TH.Q TH.Pat
parseInterpoliquePat s = parse' s >>= dataToPatQ (const Nothing)


