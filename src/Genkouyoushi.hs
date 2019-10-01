module Genkouyoushi where

import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , runReaderT
                                                )
import           Data.Colour.Names              ( white )
import           Diagrams.Prelude        hiding ( height
                                                , boxGrid
                                                , width
                                                , size
                                                )
import           Diagrams.Backend.Rasterific
                                         hiding ( size )
import           Data.Scientific                ( Scientific )


-- TODO: Split into 2 possible layouts
-- 1. define number of rows/columns - calculate box size
-- 2. define box size - calculate rows/columns
-- TODO: Double sided mode. Generate a 2 page PDF for double sided
-- printing, flipping the left & right margins for 2nd page.
data Config = Config
    { dpi :: Integer
    -- ^ Dots Per Inch
    , height :: Scientific
    -- ^ Inches
    , width :: Scientific
    -- ^ Inches
    , furiganaBoxes :: Bool
    -- ^ Place narrow boxes for Furigana to the right of each Kana box.
    -- TODO: Make `Maybe Scientific`. If nothing, no boxes. If value then
    -- use to generate width of furigana boxes(relative to box width).
    -- E.g.  @Just 0.5@ makes boxes half the width of the Kana boxes.
    , rows :: Integer
    -- ^ Boxes per row
    , columns :: Integer
    -- ^ Boxes per column
    , marginTop :: Scientific
    -- ^ Inches
    , marginRight :: Scientific
    -- ^ Inches
    , marginBottom :: Scientific
    -- ^ Inches
    , marginLeft :: Scientific
    -- ^ Inches
    , boxSpacing :: Scientific
    -- ^ Inches
    , joinDirection :: JoinDirection
    } deriving (Read, Show)

-- | Calculate the pixels given by an inch value in `Config`, using the
-- defined DPI.
fromInches :: (Config -> Scientific) -> ReaderT Config Identity Double
fromInches selector = do
    dpi_ <- asks dpi
    val  <- asks selector
    return . realToFrac $ fromIntegral dpi_ * val


usLetterVertical :: Config
usLetterVertical = Config
    { dpi           = 300
    , height        = 11
    , width         = 8.5
    , furiganaBoxes = True
    , rows          = 18
    , columns       = 7
    , marginTop     = 0.75
    , marginRight   = 0.5
    , marginBottom  = 0.75
    , marginLeft    = 0.5
    , boxSpacing    = 0.25
    , joinDirection = JoinColumns
    }


data JoinDirection
    = JoinNothing
    | JoinColumns
    | JoinRows
    deriving (Read, Show, Eq)


render :: Config -> Diagram B
render = runIdentity . runReaderT render_
  where
    render_ = do
        mTop    <- fromInches marginTop
        mRight  <- fromInches marginRight
        mBot    <- fromInches marginBottom
        mLeft   <- fromInches marginLeft
        boxGrid <- renderGrid
        return
            $ vcat
                  [ strutY mTop
                  , strutX mLeft ||| boxGrid ||| strutX mRight
                  , strutY mBot
                  ]
            # bg white


renderGrid :: ReaderT Config Identity (Diagram B)
renderGrid = do
    gHeight        <- gridHeight
    gWidth         <- gridWidth
    boxesPerRow    <- asks columns
    boxesPerColumn <- asks rows
    joinDirection_ <- asks joinDirection
    boxSpacing_    <- fromInches boxSpacing
    withFurigana   <- asks furiganaBoxes
    let boxesPerRowFurigana = if withFurigana
            then fromIntegral boxesPerRow * 1.5
            else fromIntegral boxesPerRow
        boxWidth_ = if joinDirection_ == JoinRows
            then gWidth / boxesPerRowFurigana
            else
                (gWidth - fromIntegral (boxesPerRow - 1) * boxSpacing_)
                    / boxesPerRowFurigana
        boxHeight_ = if joinDirection_ == JoinColumns
            then gHeight / fromIntegral boxesPerColumn
            else (gHeight - fromIntegral (boxesPerColumn - 1) * boxSpacing_)
                / fromIntegral boxesPerColumn
        boxSide              = min boxWidth_ boxHeight_
        (xSpacing, ySpacing) = case joinDirection_ of
            JoinNothing -> (boxSpacing_, boxSpacing_)
            JoinRows    -> (0.0, boxSpacing_)
            JoinColumns -> (boxSpacing_, 0.0)
        xPadding =
            ( gWidth
                - (boxSide * boxesPerRowFurigana)
                - (xSpacing * fromIntegral (boxesPerRow - 1))
                )
                / 2
        yPadding =
            ( gHeight
                - (boxSide * fromIntegral boxesPerColumn)
                - (ySpacing * fromIntegral (boxesPerRow - 1))
                )
                / 2
    kanaBox <- renderScaledBox boxSide
    let singleRow =
            hsep xSpacing $ replicate (fromIntegral boxesPerRow) kanaBox
        grid =
            vsep ySpacing $ replicate (fromIntegral boxesPerColumn) singleRow
    return
        (   strutY yPadding
        === (strutX xPadding ||| grid ||| strutX xPadding)
        === strutY yPadding
        )
  where
    gridHeight = do
        total <- fromInches height
        mTop  <- fromInches marginTop
        mBot  <- fromInches marginBottom
        return $ total - mTop - mBot
    gridWidth = do
        total  <- fromInches width
        mLeft  <- fromInches marginLeft
        mRight <- fromInches marginRight
        return $ total - mLeft - mRight
    renderScaledBox :: Double -> ReaderT Config Identity (Diagram B)
    renderScaledBox size = do
        withFurigana <- asks furiganaBoxes
        return $ renderBox withFurigana # scale size


renderBox :: Bool -> Diagram B
renderBox withFurigana = kanaBox ||| furiganaRect
  where
    kanaBox =
        square 1
            #  lwN 0.001
            <> crosshairs
            #  dashingN [0.0025, 0.0025] 0
            #  opacity 0.2
    furiganaRect = if withFurigana then rect 0.5 1 # lwN 0.0005 else mempty
    crosshairs   = hrule 1 # lwN 0.001 <> vrule 1 # lwN 0.001
