
{-|
Module      : Data.Vizzy
Description : Types and functions for parsing and representing Vizzy programs from XML.

This module defines the core data structures for Vizzy programs, including variables, blocks, and custom expressions.
It provides functions to convert XML documents into these Haskell types, enabling further analysis or transformation
of Vizzy programs within Haskell code.

Parsing functions are provided for the main program structure and its components. Some conversion functions are currently stubs.
-}

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NoImplicitPrelude, ApplicativeDo, OverloadedLists, RecordWildCards #-}

module Data.Vizzy where
import Data.Text ( unpack, Text, unlines, show)
import Text.XML ( Document, Element(Element), Name )
import Text.XML.Lens as XML ( attribute, name, named, root )
import Data.Word (Word16)
import Data.Maybe ( Maybe(..) )
import Control.Lens
    ( Traversable(traverse),
      Plated(plate),
      (<&>),
      (^..),
      (^?),
      (^.),
      (#) )
import Text.Show (Show)
import GHC.Base (Float, String)
import Control.Category ((.))
import Data.Function (($))
import Control.Applicative (Applicative(..), (<$>), Alternative (..))
import Text.Read (reads)
import Data.Validation
    ( _Failure,
      _Success,
      bindValidation,
      fromEither,
      toEither,
      validate,
      Validation(..) )
import Data.List.NonEmpty ( NonEmpty(..), toList )
import Data.CaseInsensitive (CI)
import GHC.Read (Read)
import qualified Data.Map
import Data.Semigroup ((<>))
import Control.Exception (Exception(..))

data VizzyError
    = MissingElement {vzLocalName :: CI Text}
    | UnexpectedElement {vzName :: Name}
    | MissingAttribute {vzName :: Name}
    | InvalidAttribute {vzName :: Name, vzValue :: Text}
    | Other {msg :: Text}
    deriving (Show)

type VErrors = NonEmpty VizzyError

instance Exception VErrors where
    displayException :: VErrors -> String
    displayException errors =  unpack . unlines . ("Vizzy Parse errors":) . toList $ show <$> errors

-- Note that this is not quite a lawful instance
instance Alternative (Validation VErrors) where
    (<|>) :: Validation VErrors a -> Validation VErrors a -> Validation VErrors a
    Success a <|> _ = Success a
    Failure _ <|> Success b = Success b
    Failure a <|> Failure b = Failure (a <> b)
    empty :: Validation VErrors a
    empty = Failure (pure $ Other "empty (from Alternative)")

stopOnError :: Validation VErrors a -> (a -> Validation VErrors b) -> Validation VErrors b
stopOnError = bindValidation

isNamed :: CI Text -> Element -> Validation VErrors Element
isNamed t e =
    validate ([MissingElement {vzLocalName = t}, UnexpectedElement (e ^. XML.name)] :: VErrors)
        (^? named t) e

findChildNamed :: CI Text -> Element -> Validation VErrors Element
findChildNamed t e =
    case e ^.. plate . named t of
    [] -> _Failure # (MissingElement t :| (UnexpectedElement <$> e ^.. plate . XML.name))
    [x] -> _Success # x
    (_:(y:l)) -> _Failure # (UnexpectedElement . (^. XML.name) <$> (y :| l))

onSingleSection :: CI Text -> (Element -> Validation VErrors b) -> Element -> Validation VErrors [b]
onSingleSection t f xml = 
    findChildNamed t xml `stopOnError` 
    \ section -> traverse f (section ^.. plate) 

-- | Convert an XML 'Document' to a 'VizzyProgram'.
xmlToVizzyProgram :: Document -> Validation VErrors VizzyProgram
xmlToVizzyProgram doc = 
    isNamed "Program" (doc ^. root) `stopOnError`
    \ rootElement -> do
        variables <- onSingleSection "Variables" toVariableDeclaration rootElement
        blocks <- traverse toBlock $ rootElement ^.. plate . named "Instructions"
        expressions <- onSingleSection "CustomExpressions" toCustomExpression rootElement
        pure $ VizzyProgram {..}


-- | The top-level representation of a Vizzy program, containing variables, blocks, and custom expressions.
data VizzyProgram =
    VizzyProgram
        { variables :: [VariableDeclaration] -- ^ List of variable declarations
        , blocks :: [Block]                  -- ^ List of instruction blocks
        , expressions :: [CustomExpression]   -- ^ List of custom expressions
        }
        deriving (Show)

validate' :: (a -> e) -> (a -> Maybe b) -> a -> Validation e b
validate' toError toValue x = validate (toError x) toValue x 

validateAttribute :: Name -> (Text -> Maybe b) -> Element -> Validation VErrors b
validateAttribute t f xml =
     validate (pure $ MissingAttribute t) (^. attribute t) xml
    `stopOnError` validate' (pure . InvalidAttribute t) f

maybeRead :: Read a => Text -> Maybe a
maybeRead t =
    case reads $ unpack t of
    [(n, "")] -> Just n
    _ -> Nothing

readAttribute :: Read a => Name -> Element -> Validation VErrors a
getAttribute :: Name -> Element -> Validation VErrors Text
readAttribute t = validateAttribute t maybeRead
getAttribute t = validateAttribute t Just

-- | Parse an XML 'Element' into a 'VariableDeclaration', if possible.
toVariableDeclaration :: Element -> Validation VErrors VariableDeclaration
toVariableDeclaration xml = fromEither $ do
    _ <- toEither (isNamed "Variable" xml)
    toEither $ do
        vdName <- getAttribute "name" xml
        vdNumber <- readAttribute "number" xml
        pure $ VariableDeclaration {..}

-- | Represents a variable declaration in a Vizzy program.
data VariableDeclaration = VariableDeclaration
    { vdName :: Text   -- ^ Variable name
    , vdNumber :: Word16  -- ^ Variable number
    }
    deriving (Show)


-- | Represents an instruction block in a Vizzy program.
data Block = Block
    { blockHeader :: BlockHeader -- CSPAM
    , blockBody :: [Instruction]     -- ^ List of instructions in the block
    }
    deriving (Show)

data BlockHeader = BlockHeader {bhType :: BlockType, idNum :: Word16, pos :: (Float, Float)}
    deriving (Show)

data BlockType
    = Event {event :: Event}                -- ^ Event block
    | CustomInstruction {customInstruction :: CustomInstruction} -- ^ Custom instruction block
    deriving (Show)


-- | Represents a custom instruction in a Vizzy program.
data CustomInstruction = MkCustomInstruction
    { ciCallFormat :: Text  -- ^ Format for calling the instruction
    , ciFormat :: Text      -- ^ Instruction format string
    , ciName :: Text        -- ^ Name of the custom instruction
    }
    deriving (Show)

-- | Represents a custom expression in a Vizzy program.
data CustomExpression = CustomExpression
    { ceHeader :: CustomExpressionHeader  -- ^ Header for the custom expression
    , ceBody :: Expression               -- ^ Expression body
    , idNum :: Word16                     -- ^ Expression identifier
    , pos :: (Float, Float)            -- ^ Position of the expression (x, y)
    }
    deriving (Show)

-- | Represents an event (details to be defined).
data Event = FlightStart
    deriving (Show, Read)

data Instruction = InstructionStub
    deriving (Show)

-- | Parse an XML 'Element' into a 'Block'.
toBlock :: Element -> Validation VErrors Block
toBlock xml =
    case xml ^.. plate of
    h:body' -> do
        blockHeader <- toHeader h
        blockBody <- traverse toInstruction body'
        pure $ Block {..}
    [] -> toHeader emptyElement `stopOnError` \ _ -> vizzyFail $ UnexpectedElement (xml ^. XML.name)

emptyElement :: Element
emptyElement = Element "" Data.Map.empty []

toHeader :: Element -> Validation VErrors BlockHeader
toHeader xml =
    stopOnError (toBlockType xml) $
        \ bhType -> do
            idNum <- readAttribute "id" xml
            pos <- readAttribute "pos" xml
            pure $ BlockHeader {..}

toBlockType :: Element -> Validation VErrors BlockType
toBlockType xml = toEvent xml <|> CustomInstruction <$> toCustomInstruction xml

toCustomInstruction :: Element -> Validation VErrors CustomInstruction
toCustomInstruction xml =
    stopOnError (isNamed "CustomInstruction" xml) $
        \_ -> do
            ciCallFormat <- getAttribute "ciCallFormat" xml
            ciFormat <- getAttribute "format" xml
            ciName <- getAttribute "name" xml
            pure $ MkCustomInstruction {..}

toEvent :: Element -> Validation VErrors BlockType
toEvent xml = isNamed "Event" xml `stopOnError` readAttribute "event" <&> Event

vizzyFail :: VizzyError -> Validation VErrors a
vizzyFail = Failure . pure

-- | Header for a custom expression (details to be defined).
data CustomExpressionHeader = CustomExpressionStub
    deriving (Show)

-- | Represents an expression (details to be defined).
data Expression = ExpressionStub
    deriving (Show)

-- | Parse an XML 'Element' into a 'CustomExpression'. Currently a stub.
toCustomExpression :: Element -> Validation VErrors CustomExpression
toCustomExpression xml = do
    idNum <- readAttribute "id" xml
    pos <- readAttribute "pos" xml
    let ceBody = ExpressionStub
    let ceHeader = CustomExpressionStub
    case xml ^.. plate of
        [a] -> toExpression a
        [] -> vizzyFail $ MissingAttribute "an expression"
        (_:x:xs) -> Failure $ UnexpectedElement . (^. name) <$> (x :| xs)
    pure $ CustomExpression {..}


toExpression :: Element -> Validation VErrors Expression
toExpression _xml = pure ExpressionStub

toInstruction :: Element -> Validation VErrors Instruction
toInstruction _xml = pure InstructionStub