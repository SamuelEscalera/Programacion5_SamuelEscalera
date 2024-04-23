module AbstractGrammar where 
--- Gramatica libre de contexto

data Slides = Slides [Slide]
    deriving (Show)

data Slide = Slide TitleSlide BodySlide
    deriving(Show)

data TitleSlide = TitleSlide String
    deriving(Show)


data BodySlide = BodySlide [MarckdownBlock]
    deriving(Show)

data MarckdownBlock = MdParagraph String 
                    | MdHeaderH1 String
    deriving (Show)

