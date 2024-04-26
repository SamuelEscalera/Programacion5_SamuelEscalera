
-- GRAMATICA LIBRE DE CONTEXTO
module AbstractGrammar where

type Strings = String
type Paragraph = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String
type Link = String
type Background = String
type Progress = String
type Img = String

data Slides = Slides [Slide]
    deriving Show

data Slide = Slide TitleSlide BodySlide
    deriving Show

data TitleSlide = TitleSlide Strings
    deriving Show

data Cad = Cad Strings
    deriving Show

data BodySlide = BodySlide [MarkdownBlock]
    deriving Show

data MarkdownBlock = MdParagraph Paragraph
                    | MdH1 H1
                    | MdH2 H2
                    | MdH3 H3
                    | MdH4 H4
                    | MdH5 H5
                    | MdH6 H6
                    | MdLink Link
                    | MdBackground Background
                    | MdProgress Progress
                    | MdImg Img
        deriving Show