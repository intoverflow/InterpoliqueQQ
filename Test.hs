{-# LANGUAGE QuasiQuotes #-}
module Test where

import InterpoliqueQQ

author = "foo"
content = "' or 1=1;"

query = [$interpolique| insert into posts values(^^author , ^^content ); |]


author2 = "2"
content2 = "happy content"
query2 = [$interpolique| insert into posts values(^^author2 , ^^content2 ); |]
