{-# LANGUAGE QuasiQuotes #-}
module Test where

import InterpoliqueQQ
import FakeDatabase

import Kernel

author = Tainted (Just "foo")
content = Tainted (Just "' or 1=1;")

query :: TaintT db IO (Maybe (InterpoliquedString FakeDatabase))
query = [$interpolique| insert into posts values(^^author , ^^content ); |]


{-
author2 = "2"
content2 = "happy content"
query2 = [$interpolique| insert into posts values(^^author2 , ^^content2 ); |] :: InterpoliquedString FakeDatabase

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

