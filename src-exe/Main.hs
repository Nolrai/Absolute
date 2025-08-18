{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where
import System.Environment as Env (getArgs)
import System.Directory as Directory ( getDirectoryContents )
import System.Exit (die)
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Absolute (readVizzyFile, xml)
import qualified Data.List
import Text.XML ( renderText, def )
import Data.Text.Lazy (show, unpack, pack)
import Data.Text.Lazy.IO (putStrLn)
import Text.Show ( Show )
import GHC.IO ( IO, FilePath ) 
import Data.Maybe ( Maybe(Just, Nothing) )
import Data.Either (either)
import Control.Category ( Category((.)) )
import Data.Monoid ( (<>) )
import Control.Applicative ( Applicative(pure), (<$>), )  
import Data.Function (($))
import Data.Foldable (traverse_)
  
newtype Config = Config { flightProgramsPath :: FilePath }
 deriving (Show, Generic)
instance FromJSON Config

readConfig :: IO Config
readConfig = do
  bstr <- BL.readFile "config.json"
  either (die . unpack . show) pure $ eitherDecode bstr

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  config <- readConfig 
  putStrLn $ "Flight Program Path: " <> pack (flightProgramsPath config)
  arg1 <- Data.List.uncons <$> Env.getArgs
  case arg1 of
    Nothing -> do
      files <- Directory.getDirectoryContents (flightProgramsPath config)
      (putStrLn . pack) `traverse_` files
    Just (fileName, _) -> do
      putStrLn $ "Flight Program File: " <> pack fileName
      let filePath = flightProgramsPath config <> "/" <> fileName
      vizzy <- readVizzyFile filePath
      putStrLn $ "Vizzy Content: " <> renderText def (xml vizzy)
