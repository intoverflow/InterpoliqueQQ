{-# LANGUAGE
	NoMonomorphismRestriction #-}
module TestTaint where

import Taint

-- This function helps in type inference, nothing more
assertTaint :: Taint m a -> Taint m a
assertTaint = id

-- Here is some code that we are allowed to write
useTaintedData = assertTaint $
     do username <- getData "username"
	return username


{-
But if we try to *use* this code, we get a type error:
*TestTaint> runTaintT useTaintedData [("username", "dakami")]

<interactive>:1:0:
    Inferred type is less polymorphic than expected
      Quantified type variable `t' escapes
    In the first argument of `runTaintT', namely `useTaintedData'
    In the expression:
        runTaintT useTaintedData [("username", "dakami")]
    In the definition of `it':
        it = runTaintT useTaintedData [("username", "dakami")]
-}

-- Here is a version of the same function, except this one uses scrubbing first:
useScrubbedData = assertTaint $
     do username <- getData "username"
	username' <- untaint username
	return username'

-- Sure enough, we can actually use this code:
testUseScrubbedData = runTaint useScrubbedData [("username", "dakami")]

-- We're allowed to work with tainted data while still inside a Taint'd monad:
modifyUsername :: String -> String
modifyUsername s = s ++ "-user"

tryUsingTaintedData = assertTaint $
     do username <- getData "username"
	let username' = (fmap . fmap) modifyUsername username
	return username'

-- Interactive GHC can give us the type for tryUsingTaintedData:
{-
  *TestTaint> :t tryUsingTaintedData 
  tryUsingTaintedData :: Taint m (Tainted m (Maybe String))
-}
-- This shows that we haven't untainted the data before using it *within
-- the tainted monad*.  We can now untaint it and inspect its value:
testTryUsingTaintedData = runTaint (tryUsingTaintedData >>= untaint) [("username", "dakami")]

