module Data.Absolute (readVizzyFile) where
import Prelude hiding (readFile)
import Text.XML (readFile, def)
import Data.Vizzy (VizzyProgram, xmlToVizzyProgram)
import Data.Validation ( validation )
import Control.Exception (throwIO)

readVizzyFile :: FilePath -> IO VizzyProgram
readVizzyFile path =
    validation throwIO pure . xmlToVizzyProgram =<< readFile def path