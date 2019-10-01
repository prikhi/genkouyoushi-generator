module Main where

import           Diagrams.Backend.Rasterific
import           Diagrams.Size                  ( dims )

import           Genkoyoushi                    ( usLetterVertical
                                                , render
                                                )

main :: IO ()
main =
    let d = render usLetterVertical
    in  renderRasterific "test.png" (dims $ size d) d
