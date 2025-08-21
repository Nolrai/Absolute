
{-|
Module      : Data.Vizzy
Description : Types and functions for parsing and representing Vizzy programs from XML.

This module defines the core data structures for Vizzy programs, including variables, blocks, and custom expressions.
It provides functions to convert XML documents into these Haskell types, enabling further analysis or transformation
of Vizzy programs within Haskell code.

Parsing functions are provided for the main program structure and its components. Some conversion functions are currently stubs.
-}

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Data.Vizzy where
import Data.Text
import Text.XML
import Text.XML.Lens
import Data.Word (Word16)
import Data.Map as Map
import Data.Maybe (maybeToList, listToMaybe)
import Control.Lens hiding (element)


-- | Convert an XML 'Document' to a 'VizzyProgram'.
xmlToVizzyProgram :: Document -> VizzyProgram
xmlToVizzyProgram doc = 
    VizzyProgram
        { variables = variables
        , blocks = blocks
        , expresions = customExpressions
        }
    where
    rootElement :: Element
    rootElement = doc ^. root
    variables :: [VariableDeclaration]
    -- | Extracts variable declarations from the XML root element.
    variables = (maybeToList . toVariableDeclaration) =<< (rootElement ^?! named "Variables") ^.. named "Variable"
    -- | Extracts blocks from the XML root element.
    blocks = maybeToList . toBlock =<< (rootElement ^.. named "Instructions")
    -- | Extracts custom expressions from the XML root element.
    customExpressions :: [CustomExpression]
    customExpressions = maybeToList . toCustomExpression =<< (rootElement ^.. named "CustomExpression")


-- | The top-level representation of a Vizzy program, containing variables, blocks, and custom expressions.
data VizzyProgram =
    VizzyProgram 
        { variables :: [VariableDeclaration] -- ^ List of variable declarations
        , blocks :: [Block]                  -- ^ List of instruction blocks
        , expresions :: [CustomExpression]   -- ^ List of custom expressions
        }


-- | Parse an XML 'Element' into a 'VariableDeclaration', if possible.
toVariableDeclaration :: Element -> Maybe VariableDeclaration
toVariableDeclaration xml = do  
    let attributes = elementAttributes xml  
    name' <- Map.lookup "name" attributes
    (number', []) <- listToMaybe . reads . unpack =<< Map.lookup "number" attributes
    pure $ VariableDeclaration { name = name', number = number'}
        


-- | Represents a variable declaration in a Vizzy program.
data VariableDeclaration = VariableDeclaration
    { name :: Text   -- ^ Variable name
    , number :: Int  -- ^ Variable number
    }


-- | Represents an instruction block in a Vizzy program.
data Block = Block
    { header :: BlockHeader      -- ^ Block header (start, event, or custom instruction)
    , id :: Word16              -- ^ Block identifier
    , body :: [Instruction]     -- ^ List of instructions in the block
    , pos :: (Float, Float)     -- ^ Position of the block (x, y)
    }


-- | Header for a block, indicating its type.
data BlockHeader 
    = Start                                 -- ^ Start block
    | Event {event :: Event}                -- ^ Event block
    | CustomInstruction {customInstruction :: CustomInstruction} -- ^ Custom instruction block


-- | Represents a custom instruction in a Vizzy program.
data CustomInstruction = MkCustomInstruction 
    { callFormat :: String  -- ^ Format for calling the instruction
    , format :: String      -- ^ Instruction format string
    , name :: Text          -- ^ Name of the custom instruction
    }
    

-- | Represents a custom expression in a Vizzy program.
data CustomExpression = CustomExpression 
    { header :: CustomExpressionHeader  -- ^ Header for the custom expression
    , id :: Word16                     -- ^ Expression identifier
    , body :: Expression               -- ^ Expression body
    , pos :: (Float, Float)            -- ^ Position of the expression (x, y)
    }


-- | Header for a custom expression (details to be defined).
data CustomExpressionHeader

-- | Represents an expression (details to be defined).
data Expression

-- | Represents an event (details to be defined).
data Event


-- | Parse an XML 'Element' into a 'Block'. Currently a stub.
toBlock :: Element -> Maybe Block
toBlock _xml = Nothing -- a stub


-- | Parse an XML 'Element' into a 'CustomExpression'. Currently a stub.
toCustomExpression :: Element -> Maybe CustomExpression
toCustomExpression _xml = Nothing -- a stub