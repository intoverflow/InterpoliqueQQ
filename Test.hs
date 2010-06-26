{-# LANGUAGE QuasiQuotes #-}
module Test where

import InterpoliqueQQ
import FakeDatabase
import FakeXML

import Kernel

author = Tainted (Just "foo")
content = Tainted (Just "' or 1=1;")

query :: TaintT db IO (Maybe (InterpoliquedString FakeDatabase))
query = [$interpolique| insert into posts values(^^author , ^^content ); |]


uname = Tainted (Just "cauchy")
about = Tainted (Just "french mathematician")
xml :: TaintT db IO (Maybe (InterpoliquedString FakeXML))
xml = [$interpolique|
	<user>
		<username>^^uname</username>
		<about>^^about</about>
	</user>
      |]

printInterpolique :: IO (Maybe (InterpoliquedString a)) -> IO ()
printInterpolique ioa = do a <- ioa
			   printInterpolique' a
  where printInterpolique' Nothing   = putStrLn "Nothing"
	printInterpolique' (Just is) = putStrLn $ projInterpoliquedString is

{-
-- We can interpolique doubles, ints, and bools
someDouble = 3.14159 :: Double
someInt = 2 :: Int
someBool = False
someString = "foob4r"
query3 = [$interpolique| insert into sometable values(^^someDouble , ^^someInt, ^^someBool, ^^someString ); |] :: InterpoliquedString FakeDatabase

-- Example where we've Interpoliqe'd a user-defined type
data SomeCustomType = SomeCustomType Int
instance InterpoliqueEscape SomeCustomType where
  escapique (SomeCustomType i) = show i

someCustomData = SomeCustomType 7
query4 = [$interpolique| insert into weirdCustomTypes values(^^someCustomData ); |] :: InterpoliquedString FakeDatabase
-}

