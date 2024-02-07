module MdAst where

newtype HeadingLevel = HeadingLevel Int
    deriving (Show, Eq)

data Document =
    Heading HeadingLevel String
  | Paragraph [Inline]
  deriving (Show, Eq)

data Inline =
    PlainText String
  | Bold [Inline]
  | Italic [Inline]
  deriving (Show, Eq)

