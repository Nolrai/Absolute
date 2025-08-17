module Data.Absolute (readVizzyFile) where
import Prelude hiding (readFile)
import Text.XML (readFile, def, Document)

readVizzyFile :: FilePath -> IO VizzyFile
readVizzyFile path = toVizzyFile =<< readFile def path

-- For now this is just a wrapper around an xml document, but later this will be the concrete syntax tree

newtype VizzyFile = XML {xml :: Document}
