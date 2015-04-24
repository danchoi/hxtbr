{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Main where
import Text.XML.HXT.Core
import System.Environment
import Data.List
import Control.Arrow.ArrowNavigatableTree
import Data.Tree.NTree.TypeDefs
import Data.Tree.NavigatableTree.Class
import Text.XML.HXT.DOM.TypeDefs
import Numeric
import Data.String.Utils (replace)

-- removes BR tags that have any P tag following-siblings
stripInterstitialBR = 
        none
        `when`
        ( 
            withoutNav (hasName "br")
            >>>
            followingSiblingAxis >>> filterAxis (isElem >>> hasName "p")
        )
    


main = do  
  args <- getArgs
  input <- case args of
            [infile] -> readFile infile
            _ -> getContents
  res <- runX (readString [ withValidate no,withWarnings no,withParseHTML yes ] input
        >>>
        ( 
              
            processTopDown (
              replaceChildren (
                addNav 
                >>> getChildren 
                >>> stripInterstitialBR 
                >>> remNav
              ) 
            )

        
        )
        >>>
        writeDocument [
                      withIndent yes
                      ,withOutputHTML
                      ,withXmlPi no
                      ] "-"
        )
  return ()

