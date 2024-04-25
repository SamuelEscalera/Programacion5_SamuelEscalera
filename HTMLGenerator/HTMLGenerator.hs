module HTMLGenerator where

import Parser
import AbstractGrammar


generateHTMLForSlides :: Slides -> String
generateHTMLForSlides (Slides slides) = concatMap generateHTMLForSlide slides

generateHTMLForSlide :: Slide -> String
generateHTMLForSlide slide =
    let titleHTML = generateTitleHTML (getTitleSlide slide)
        bodyHTML = generateBodyHTML (getBodySlide slide)
    in "<!DOCTYPE html>\n" ++
       "<html>\n" ++
       "<head>\n" ++
       titleHTML ++
       "</head>\n" ++
       "<body>\n" ++
       bodyHTML ++
       "</body>\n" ++
       "</html>\n"

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
generateBodyHTML (BodySlide blocks) = "<body>\n" ++ concatMap generateBlockHTML blocks ++ "</body>\n"

generateBlockHTML :: MarkdownBlock -> String
generateBlockHTML (MdParagraph paragraph) = "<p>" ++ paragraph ++ "</p>\n"
generateBlockHTML (MdH1 title) = "<h1>" ++ title ++ "</h1>\n"
generateBlockHTML (MdH2 title) = "<h2>" ++ title ++ "</h2>\n"
generateBlockHTML (MdH3 title) = "<h3>" ++ title ++ "</h3>\n"
generateBlockHTML (MdH4 title) = "<h4>" ++ title ++ "</h4>\n"
generateBlockHTML (MdH5 title) = "<h5>" ++ title ++ "</h5>\n"
generateBlockHTML (MdH6 title) = "<h6>" ++ title ++ "</h6>\n"
