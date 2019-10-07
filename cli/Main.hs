{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Diagrams.Backend.Rasterific
import           Diagrams.Size                  ( dims )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , helpArg
                                                , details
                                                , typ
                                                , program
                                                , summary
                                                , name
                                                , argPos
                                                , def
                                                )

import           Genkouyoushi                   ( Config(Config)
                                                , JoinDirection(..)
                                                , render
                                                )

-- | Parse the arguments, then render the diagram.
main :: IO ()
main = do
    args <- cmdArgs argSpec
    let d = render $ convertArgs args
    renderRasterific (fileName args) (dims $ size d) d


-- | CLI arguments
data Args
    = Args
        { dpi :: Integer
        , height :: Double
        , width :: Double
        , furigana_Boxes :: Bool
        , rows :: Integer
        , columns :: Integer
        , margin_Top :: Double
        , margin_Bottom :: Double
        , margin_Left :: Double
        , margin_Right :: Double
        , box_Spacing :: Double
        , join_Direction :: JoinDirection
        , fileName :: FilePath
        } deriving (Show, Data, Typeable)

-- | Convert the arguments into a Config for the library renderer.
convertArgs :: Args -> Config
convertArgs a = Config (dpi a)
                       (realToFrac $ height a)
                       (realToFrac $ width a)
                       (furigana_Boxes a)
                       (rows a)
                       (columns a)
                       (realToFrac $ margin_Top a)
                       (realToFrac $ margin_Right a)
                       (realToFrac $ margin_Bottom a)
                       (realToFrac $ margin_Left a)
                       (realToFrac $ box_Spacing a)
                       (join_Direction a)


-- | Default arguements draws an 8.5x11" diagram with 7x18 boxes in
-- a columnar layout.
argSpec :: Args
argSpec =
    Args
            { dpi            = 300 &= help "dots per inch"
            , height         = 11 &= typ "INCHES" &= help "document height"
            , width          = 8.5 &= typ "INCHES" &= help "document width"
            , furigana_Boxes = False &= help "include furigana boxes"
            , rows           = 18 &= help "boxes per column"
            , columns        = 7 &= help "boxes per row"
            , margin_Top     = 0.75
                &= typ "INCHES"
                &= help "top margin of document"
                &= name "mt"
            , margin_Bottom  = 0.75
                &= typ "INCHES"
                &= help "bottom margin of document"
                &= name "mb"
            , margin_Left    = 0.5
                &= typ "INCHES"
                &= help "left margin of document"
                &= name "ml"
            , margin_Right   = 0.5
                &= typ "INCHES"
                &= help "right margin of document"
                &= name "mr"
            , box_Spacing    = 0.25 &= typ "INCHES" &= help
                "space between unjoined boxes"
            , join_Direction = JoinColumns
                &= help "direction to join adjacent boxes"
            , fileName       = def &= argPos 0 &= typ "OUTPUT.EXT"
            }
        &= program "genkouyoushi-gen"
        &= summary
               "Genkouyoushi Generator v1.0.0.0, (c) Pavan Rikhi 2019, GPL-3.0+"
        &= helpArg [name "h"]
        &= details
               [ "Generate grids of boxes for practicing Kanji & Kana."
               , ""
               , "The default arguments will generate an 8.5x11\" grid at 300 DPI"
                   ++ "with 7 columns of 18 kana boxes."
               , ""
               , "You can customize the generated layout and size by passing any "
                   ++ "of the above arguments."
               , ""
               , "You must pass the name of the file to generate. Various "
                   ++ "filetypes are supported: .pdf, .png, .gif, .jpg, .bmp, & .tif."
               , ""
               , "Example usage:"
               , "\tgenkouyoushi-gen -d 120 -r 10 -c 5 -j joinnothing  -f my-paper.pdf"
               ]
