{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}
module InterpoliqueQQ (interpolique, runQuery) where

import Data.Typeable
import Data.Data
import Data.Generics.Aliases (extQ)
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
-- This next import requires the dataenc package on Hackage
-- (cabal install dataenc)
import Codec.Binary.Base64 (encode)


-- We don't export the constructor: the only way to get
-- an InterpoliquedString is to use the [$interpolique| ... |]
-- quasi-quoter
data InterpoliquedString = InterpoliquedString String
  deriving Show

lexer = P.makeTokenParser haskellDef
identifier = P.identifier lexer

ex = "insert into posts values(^^author , ^^content );"
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
					  , InterpoliqueSQL "b64d(\""
					  , InterpoliqueVar i'
					  , InterpoliqueSQL "\")"
					  ]
			_	-> return [InterpoliqueSQL sql]
	interpoliqued' <- many anyChar
	return $ concat $ interpoliqued ++ [[ InterpoliqueSQL interpoliqued' ]]



runQuery :: InterpoliquedString -> IO ()
runQuery (InterpoliquedString s) = putStrLn s

interpolique = QuasiQuoter parseInterpoliqueExp parseInterpoliquePat


b64enc = encode . unpack . pack

antiE :: InterpoliqueComponent -> Maybe TH.ExpQ
antiE (InterpoliqueVar v) = Just $ TH.appE [| b64enc |] (TH.varE $ TH.mkName v)
antiE (InterpoliqueSQL s) = Just $ TH.litE $ TH.stringL s

parseInterpoliqueExp :: String -> TH.Q TH.Exp
parseInterpoliqueExp s =
     do p <- parse' s
	let p' = dataToExpQ (const Nothing `extQ` antiE) p
	TH.appE [| InterpoliquedString . concat |] p'

parseInterpoliquePat :: String -> TH.Q TH.Pat
parseInterpoliquePat s = parse' s >>= dataToPatQ (const Nothing)


