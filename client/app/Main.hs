module Main where

import Control.Monad (forever)
import Control.Exception (finally)
import System.IO
import Network.Simple.TCP
import Network
import Control.Concurrent.Async (race)

main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber 8080)

  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

  putStr "Pick a username: "
  hFlush stdout
  line <- getLine
  hPutStrLn handle ("connect:" ++ line)

  talk handle `finally` hClose handle

talk :: Handle -> IO ()
talk handle = do
    _ <- race fromServer toServer
    return ()

    where
      fromServer = do
        ineof <- hIsEOF handle
        if ineof
          then return ()
          else do
            line <- hGetLine handle
            putStrLn line
            fromServer

      toServer = do
        line <- getLine
        case line of
      -- server accepts /quit as disconnect command so better send it to the server
          "/quit" -> do
            hPutStrLn handle "disconnect"
            return "Quit"
          _ ->  do
            hPutStrLn handle ("bcast:" ++ line)
            toServer

