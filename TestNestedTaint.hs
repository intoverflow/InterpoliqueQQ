{-# LANGUAGE
	Rank2Types,
	NoMonomorphismRestriction,
	QuasiQuotes #-}
module TestNestedTaint where

import Taint
import InterpoliqueQQ

import Control.Monad.Trans

{-
    In this example we have a hypothetical web framework which has a couple
    different components, each with its own notion of "tainting."
    1. The web framework takes data from the user, which is obviously tainted
       as far as *everyone* is concerned
    2. The database provides data as well (based on the user-input) which is
       tainted from the perspective of the XML output (since, after all, the
       database likely does not store the data in an XML-friendly way)
    3. The web framework demands that our implementation produce a thoroughly
       untainted result.
    So our data flow looks something like this:
    User -0-> web framework -1-> database -2-> XML generator -3-> web framework
    In this diagram, stages 1,2,3 all require some notion of untainting in our
    application.

    We deal with these requirements via a "lattice of tainting."

    Along the way we have built everything on top of the IO monad, giving us a
    way to log everything that's happened to the data to stdout.  (This is
    purely diagnostic and otherwise immaterial to the demonstration.)

    To keep things simple, the web framework and the database are both being
    faked in this module.  Naturally we could replace them with actual services,
    though the added complexity would likely obfuscate this demonstration.
-}

import FakeDatabase
import FakeXML

webFramework :: Show a => (forall web . TaintT web IO a) -> IO ()
webFramework requestHandler =
     do response <- runTaintT requestHandler
			      [("username", "dakami"), ("docID", "7")]
	putStrLn $ "Response: " ++ show response

-- And now for our imaginary web page
ourResponse =
     do uname <- getData "username"
	docID <- getData "docID"
	-- Our DB uses totally crazy notation for queries:
	docQuery <- return undefined -- [$interpolique| thedoc-^^uname-^^docID ]
	let doc = runDB (query docQuery)
			(\doc' -> [$interpolique| User's document: ^^doc' |])
	return doc


ourSite = webFramework ourResponse

