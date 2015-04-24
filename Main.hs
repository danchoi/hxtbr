{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Main where
import Text.XML.HXT.Core
import System.Environment
import Data.List
import Control.Arrow.ArrowNavigatableTree
import Data.Tree.NavigatableTree.Class


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
  res <- runX (readString [ withWarnings no,withParseHTML yes ] input
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
        writeDocument [ withOutputHTML
                      , withXmlPi no
                      ] "-"
        )
  return ()

