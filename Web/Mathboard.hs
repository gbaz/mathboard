{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Web.Mathboard where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forever)
import           Data.Aeson (Result(..), fromJSON, FromJSON(..))
import           Data.Aeson.Types (parse)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Web.Scotty (scotty, get, file, html)
import qualified Web.Scotty.Comet as KC
import           Text.Heredoc
import           GHC.Generics
import           Graphics.Svg.Core (renderText)

import Diagrams.Prelude
import Diagrams.Backend.SVG

data Board = Board { inChan :: TChan Text
                   , outChan :: TChan Event
                   , boardThread :: ThreadId }

initBoard :: IO Board
initBoard = do
     inQueue  <- atomically newTChan
     outQueue <- atomically newTChan
     kComet <- KC.kCometPlugin
     let kc_opts :: KC.Options
         kc_opts = KC.Options { KC.prefix = "/mathboard", KC.verbose = 0} --TODO make configurable or set to 0
     connectApp <- KC.connect kc_opts $ \kc_doc -> do
        _ <- forkIO $ forever $ do
                val <- atomically . readTChan . KC.eventQueue $ kc_doc
                case fromJSON val of
                  Success (event :: Event) -> do
                          atomically $ writeTChan outQueue event
                  _ -> return ()
        _ <- forkIO $ forever $ do
                act <- atomically $ readTChan inQueue
                KC.send kc_doc act
        return ()
        -- TODO send setup commands
     tid <- forkIO $ scotty 3000 $ do  --TODO Configure port number
             get "/kansas-comet.js" $ file $ kComet
             get "/svg.js"          $ file $  "static/svg.js"
             get "/mathboard.js"    $ file $  "static/mathboard.js"
             get "/" $ html $
               [here|
        <html>
        <head>
        <title>Mathboard</title>
        <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
        <script type="text/javascript" src="kansas-comet.js"></script>
        <script type="text/javascript" src="svg.js"></script>
        <script type="text/javascript" src="mathboard.js"></script>
        <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css">
        <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
        </head>
        <body>
        <script type="text/javascript">
            $(document).ready(function() {
                $.kc.connect("/mathboard");
            });
        </script>
        </body>
               |]
             connectApp
     return $ Board inQueue outQueue tid

shutdownBoard :: Board -> IO ()
shutdownBoard  = killThread . boardThread

sendBoard b = atomically . writeTChan (inChan b)

data Event = Event deriving Generic
instance FromJSON Event

type SVGD = QDiagram SVG V2 Double Any

sendDia :: Board -> QDiagram SVG V2 Double Any -> IO ()
sendDia b d = sendBoard b $ T.concat ["mathboard.addSVG('", svg, "')"]
  where svg = LT.toStrict $ renderText $ renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] False) d

-- myDia = circle 1

-- TODO add state to board, have events to send draw commands to canvas and add them to boardstate. Then have JS to listen on clicks.

-- Purpose: to replay diagrams.

-- Also layout combinators for sending to board, and size combinators

-- board regions, also overlays