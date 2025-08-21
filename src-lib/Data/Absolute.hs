module Data.Absolute (readVizzyFile) where
import Prelude hiding (readFile)
import Text.XML (readFile, def)
import Data.Vizzy (VizzyProgram, xmlToVizzyProgram)

readVizzyFile :: FilePath -> IO VizzyProgram
readVizzyFile path = xmlToVizzyProgram <$> readFile def path

