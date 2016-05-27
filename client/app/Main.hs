module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Async (race_)
import Data.List (isPrefixOf)

import System.IO
import Network.Simple.TCP
import Network

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Buttons.Button


untilM :: Monad m => m Bool -> m ()
untilM f = do
  r <- f
  if r
    then return ()
    else untilM f


pickUsername handle window = do
  dia <- dialogNew
  set dia [ windowTitle := "Pick a username", windowTransientFor := window ]

  hbox <- hBoxNew False 0

  nick <- entryNew
  label <- labelNew $ Just "Pick a username"

  button <- buttonNew
  set button [ buttonLabel := "Select" ]

  boxPackStart hbox nick PackGrow 0
  boxPackEnd hbox button PackNatural 0

  vbox <- vBoxNew False 0
  boxPackStart vbox label PackNatural 0
  boxPackEnd vbox hbox PackNatural 0

  w <- dialogGetActionArea dia
  boxPackEnd (castToBox w) vbox PackNatural 0

  button `on` buttonPressEvent $ liftIO (tellServer dia nick label)

  widgetShowAll dia
  dialogRun dia

  where
    tellServer dia nick label = do
      un <- entryGetText nick
      hPutStrLn handle ("connect:" ++ un)
      sl <- hGetLine handle
      if "error" `isPrefixOf` sl
        then do
          labelSetText label ("Username " ++ un ++ " is already taken. Pick another one.")
          return False
        else do
          widgetDestroy dia
          return True


main :: IO ()
main = do
  handle <- connectTo "localhost" (PortNumber 8080)
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering


  initGUI
  window <- windowNew
  button <- buttonNew
  input <- entryNew
  textview <- textViewNew
  textViewSetEditable textview False
  textViewSetCursorVisible textview False
  hbox <- hBoxNew False 0
  vbox <- vBoxNew False 0

  set input [ widgetHExpand := True, widgetHExpandSet := True,
              widgetMarginRight := 10 ]
  set window [ containerBorderWidth := 10, containerChild := vbox ]
  set button [ buttonLabel := "Send" ]
  set textview [ widgetVExpand := True, widgetVExpandSet := True,
                 widgetMarginBottom := 10 ]

  boxPackStart hbox input PackGrow 0
  boxPackStart hbox button PackNatural 0

  boxPackStart vbox textview PackGrow 0
  boxPackStart vbox hbox PackNatural 0

  button `on` buttonPressEvent $ liftIO (toServer handle input) >> return False
  window `on` deleteEvent $ liftIO mainQuit >> return False

  pickUsername handle window

  forkIO $ fromServer handle textview

  widgetShowAll window
  mainGUI

fromServer handle textview = do
  ineof <- hIsEOF handle
  if ineof
    then return ()
    else do
      line <- hGetLine handle
      buf <- textViewGetBuffer textview
      textBufferInsertAtCursor buf (line ++ "\n")
      fromServer handle textview

toServer handle input = do
  line <- entryGetText input
  case line of
    "/quit" -> do
      hPutStrLn handle "disconnect"
      return ()
    _ ->  do
      hPutStrLn handle ("bcast:" ++ line)
      return ()
