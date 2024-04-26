module HTMLGenerator where

import Parser
import AbstractGrammar


generateHTMLForSlides :: Slides -> String
generateHTMLForSlides (Slides slides) = 
    "<!DOCTYPE html>\n" ++
    "<html>\n" ++
    "<head>\n" ++
    "<title>index</title>\n" ++
    "</head>\n" ++
    "<body>\n" ++
    concatMap generateHTMLForSlide slides ++
    "</body>\n" ++
    "</html>\n"

generateHTMLForSlide :: Slide -> String
generateHTMLForSlide slide =
    let titleHTML = generateTitleHTML (getTitleSlide slide)
        bodyHTML = generateBodyHTML (getBodySlide slide)
    in "<div class=\"slide\">\n" ++ -- Aquí se abre un nuevo div para cada slide
       titleHTML ++
       bodyHTML ++
       "</div>\n" -- Aquí se cierra el div de la slide

generatorContent :: String -> String
generatorContent a = a

getSlide :: Slides -> Slide
getSlide (Slides []) = error "No slides"
getSlide (Slides (x:_)) = x

getTitleSlide :: Slide -> TitleSlide
getTitleSlide (Slide titleSlide _) =  titleSlide

getBodySlide :: Slide -> BodySlide
getBodySlide (Slide _ bodySlide) = bodySlide

generateTitleHTML :: TitleSlide -> String
generateTitleHTML (TitleSlide title) = "<title>" ++ title ++ "</title>"

generateBodyHTML :: BodySlide -> String
generateBodyHTML (BodySlide blocks) = concatMap generateBlockHTML blocks ++ "\n"

generateBlockHTML :: MarkdownBlock -> String
generateBlockHTML (MdParagraph paragraph) = "<p>" ++ paragraph ++ "</p>\n"
generateBlockHTML (MdH1 title) = "<h1>" ++ title ++ "</h1>\n"
generateBlockHTML (MdH2 title) = "<h2>" ++ title ++ "</h2>\n"
generateBlockHTML (MdH3 title) = "<h3>" ++ title ++ "</h3>\n"
generateBlockHTML (MdH4 title) = "<h4>" ++ title ++ "</h4>\n"
generateBlockHTML (MdH5 title) = "<h5>" ++ title ++ "</h5>\n"
generateBlockHTML (MdH6 title) = "<h6>" ++ title ++ "</h6>\n"
generateBlockHTML (MdLink link) = "<a href="++ link ++">" ++ link ++ "</a>\n"
generateBlockHTML (MdBackground color) = "<style>\n body {\n background-color: #" ++ color ++ ";\n }\n </style>\n" 
generateBlockHTML (MdProgress value) = "<progress value=" ++ value ++ " max=100></progress>"
generateBlockHTML (MdImg url) = "<img src=" ++ url ++ ">"