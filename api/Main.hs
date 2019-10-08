{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Diagrams.Backend.Rasterific    ( renderPdfBSWithDPI
                                                , size
                                                )
import           Diagrams.Size                  ( dims )
import           Diagrams.Prelude               ( V
                                                , V2(..)
                                                , N
                                                , Enveloped
                                                , boundingBox
                                                , boxExtents
                                                , SizeSpec
                                                , getSpec
                                                )
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )
import           Servant                        ( (:>)
                                                , ReqBody
                                                , JSON
                                                , OctetStream
                                                , Post
                                                , Proxy(..)
                                                , Server
                                                , serve
                                                )

import           Genkouyoushi                   ( Config(..)
                                                , JoinDirection(..)
                                                , render
                                                )

import qualified Data.ByteString.Lazy          as LBS


main :: IO ()
main = run 8080 $ logStdout app

app :: Application
app = serve api server

type API = ReqBody '[JSON] GenkouConfig :> Post '[OctetStream] LBS.ByteString

api :: Proxy API
api = Proxy


newtype GenkouConfig = GenkouConfig
    { fromGenkouConfig :: Config
    } deriving (Read, Show)

instance FromJSON GenkouConfig where
    parseJSON = withObject "GenkouyoushiConfig" $ \v -> do
        dpi <- v .: "dpi"
        height <- v .: "height"
        width <- v .: "width"
        furiganaBoxes <- v .: "furigana"
        rows <- v .: "rows"
        columns <- v .: "columns"
        marginTop <- v .: "marginTop"
        marginRight <- v .: "marginRight"
        marginBottom <- v .: "marginBottom"
        marginLeft <- v .: "marginLeft"
        boxSpacing <- v .: "spacing"
        joinDirection <- parseDirection =<< v .: "joinDirection"
        return $ GenkouConfig Config {..}
      where
        parseDirection str = case str of
            "col" ->
                return JoinColumns
            "row" ->
                return JoinRows
            "none" ->
                return JoinNothing
            "all" ->
                return JoinAll
            _ ->
                fail $ "Could not parse joinDirection: " <> str



server :: Server API
server gcfg =
    let cfg    = fromGenkouConfig gcfg
        d      = render cfg
        s      = (dims $ size d)
        (w, h) = specToDims (aspectRatio d) s
    in  return $ renderPdfBSWithDPI (round w)
                                    (round h)
                                    (fromIntegral $ dpi cfg)
                                    s
                                    d
  where
    -- From diagrams-rasterific
    aspectRatio :: (V a ~ V2, Enveloped a) => a -> N a
    aspectRatio d = h / w where V2 w h = boxExtents (boundingBox d)
    -- From diagrams-rasterific
    specToDims :: (Fractional a, Ord a) => a -> SizeSpec V2 a -> (a, a)
    specToDims ar s = case getSpec s of
        V2 (Just w) (Just h) -> (w, h)
        V2 (Just w) Nothing  -> (w, ar * w)
        V2 Nothing  (Just h) -> (h / ar, h)
        V2 Nothing  Nothing  -> (100, 100)
