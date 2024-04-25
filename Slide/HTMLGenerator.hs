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
generateBodyHTML (BodySlide blocks) = "<body>\n" ++ concatMap generateBlockHTML blocks ++ "</body>"

generateBlockHTML :: MarkdownBlock -> String
generateBlockHTML (MdParagraph paragraph) = "<p>" ++ paragraph ++ "</p>\n"
generateBlockHTML (MdH1 title) = "<h1>" ++ title ++ "</h1>\n"