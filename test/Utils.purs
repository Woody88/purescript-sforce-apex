module Test.Utils where 

import Effect.Aff
import Prelude

import Data.Array (filter)
import Data.String (Pattern(..), contains)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as Node
import Node.Path (FilePath)


getApexFileNames :: Aff (Array FilePath)
getApexFileNames = filter isApexFile <$> Node.readdir "./test/apex"

readApexFile :: FilePath -> Aff String
readApexFile f = Node.readTextFile UTF8 $ "./test/apex/" <> f

isApexFile :: FilePath -> Boolean 
isApexFile = contains apexPattern
    where 
        apexPattern = Pattern ".cls"
        