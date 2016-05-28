module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Async (race_)
import Data.List (isPrefixOf, elemIndex)
import Data.String (fromString)

import System.IO
import Network

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Buttons.Button


pickUsername handle window = do
  guiUsernameDialog window callServer
  where
    callServer onError onSuccess username = do
      hPutStrLn handle ("connect:" ++ username)
      sl <- hGetLine handle
      if "error" `isPrefixOf` sl
        then onError
        else onSuccess

main :: IO ()
main = do
  handle <- connect "localhost" 8080
  (window, textview) <- guiChat handle
  pickUsername handle window
  forkIO $ fromServer handle textview

  widgetShowAll window
  mainGUI

{- Server IO -}

connect host port = do
  handle <- connectTo "localhost" (PortNumber 8080)
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  return handle

fromServer handle textview = do
  ineof <- hIsEOF handle
  if ineof
    then return ()
    else do
      line <- hGetLine handle
      buf <- textViewGetBuffer textview
      case ':' `elemIndex` line of
        Just x -> showMessage buf $ drop (x + 1) line
        Nothing -> return ()
      fromServer handle textview
  where
    showMessage buf line = textBufferInsertAtCursor buf (line ++ "\n")

toServer handle input = do
  line <- entryGetText input
  entrySetText input ""
  case line of
    "/quit" -> do
      hPutStrLn handle "disconnect"
      return ()
    _ ->  do
      hPutStrLn handle ("bcast:" ++ line)
      return ()


{- GUI setup -}

filterEnterEvent :: EventM EKey () -> EventM EKey ()
filterEnterEvent f = do
  key <- eventKeyName
  if key == (fromString "Return")
    then f
    else return ()

guiUsernameDialog window callServer = do
  (dialog, entryUsername, buttonSelect, labelText) <- createElements window
  setSubmitCallbacks callServer dialog entryUsername buttonSelect labelText

  widgetShowAll dialog
  dialogRun dialog

  where
    createElements window = do
      dia <- dialogNew
      set dia [ windowTitle := "Pick a username", windowTransientFor := window ]

      hbox <- hBoxNew False 0

      nick <- entryNew
      set nick [ widgetMarginRight := 10 ]
      label <- labelNew $ Just "Pick a username"

      button <- buttonNew
      set button [ buttonLabel := "Select" ]

      boxPackStart hbox nick PackGrow 0
      boxPackEnd hbox button PackNatural 0

      vbox <- vBoxNew False 0
      boxPackStart vbox label PackNatural 10
      boxPackEnd vbox hbox PackNatural 0

      w <- dialogGetActionArea dia
      boxPackEnd (castToBox w) vbox PackNatural 10
      set dia [ containerBorderWidth := 10 ]

      widgetGrabFocus nick

      return (dia, nick, button, label)

    setSubmitCallbacks callServer dialog entryUsername buttonSelect labelText = do
      buttonSelect `on` buttonPressEvent $  onPick >> return False
      entryUsername `on` keyPressEvent $ filterEnterEvent (onPick) >> return False

      where
        onPick :: EventM any ()
        onPick = liftIO $ do
          un <- entryGetText entryUsername
          callServer (onError un) onSuccess un
        onError un = labelSetText labelText ("Username " ++ un ++ " is already taken. Pick another one.")
        onSuccess  = widgetDestroy dialog

guiChat handle = do
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

  input `on` keyPressEvent $ filterEnterEvent (liftIO $ toServer handle input) >> return False
  button `on` buttonPressEvent $ liftIO (toServer handle input) >> return False
  window `on` deleteEvent $ liftIO mainQuit >> return False

  widgetGrabFocus input

  return (window, textview)
