module Main where

import Control.Monad (forever)
import Control.Exception (finally)
import Data.List (isPrefixOf)
import System.IO
import Network.Simple.TCP
import Network
import Control.Concurrent.Async (race)

untilM :: Monad m => m Bool -> m ()
untilM f = do
  r <- f
  if r
    then return ()
    else untilM f

main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber 8080)

  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

  untilM $ pickUsername handle
  talk handle `finally` hClose handle

  where
    pickUsername handle  = do
      putStr "Pick a username: "
      hFlush stdout
      line <- getLine
      hPutStrLn handle ("connect:" ++ line)

      sl <- hGetLine handle
      if "error" `isPrefixOf` sl
        then do
          putStrLn sl
          return False
        else return True


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
          "/quit" -> do
            hPutStrLn handle "disconnect"
            return ()
          _ ->  do
            hPutStrLn handle ("bcast:" ++ line)
            toServer

