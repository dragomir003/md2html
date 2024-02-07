module MdParser.Internal
    ( heading
    ,
    ) where

import Text.Parsec

import MdAst
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

heading :: Parsec String () Document
heading = do
    level <- headingLevel
    spaces
    Heading level <$> headingValue

headingValue :: Parsec String () String
headingValue = do
    title <- manyTill anyChar ((char '\n' >> return ()) <|> eof)
    if null title
    then fail "Heading's value must contain at least one character"
    else return $ dropWhileEnd isSpace title -- Can just trim from the end,
                                             -- because the space in the beginning
                                             -- will already be skipped.

headingLevel :: Parsec String () HeadingLevel
headingLevel = do
    level <- length <$> many1 (char '#')
    if level < 7
    then return $ HeadingLevel level
    else do
        fail "Cannot create heading with more than 6 '#'s"

