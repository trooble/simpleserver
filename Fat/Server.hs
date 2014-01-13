-- This file is part of Karim's Haskell Learning Project.

-- Karim's Haskell Learning Project is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 3 of the License, or (at your option) any later version.

-- Karim's Haskell Learning Project is distributed in the hope that it will
-- be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public
-- License  along with Karim's Haskell Learning Project.  If not, see
-- <http://www.gnu.org/licenses/>.

--{-# LANGUAGE OverloadedStrings #-}
module Fat.Server where
import Data.Monoid
import Network
import qualified Network.Socket as S
import System.IO -- (Handle, hClose, hFlush, hSetBuffering, BufferMode(..))
import System.IO.Error (isEOFError, tryIOError, ioError)
import qualified Data.ByteString.Char8 as C

run :: Integer -> (Handle -> IO ()) -> IO ()
run port app =
  do ss <- listenOn $ PortNumber $ fromInteger port
     serviceRequest ss app
     S.close ss

serviceRequest :: S.Socket -> (Handle -> IO ()) -> IO ()
serviceRequest sock app =
  do (handle, peerhost, peerport) <- accept sock
     withSocketsDo $ do app handle
     hClose handle
     serviceRequest sock app

-- TODO: rfc2616 says, in effect, that a request is one line, so all this
-- is unnecessary.
getLines :: Handle -> IO [String]
getLines hdl =
  do hSetBuffering hdl LineBuffering
     getLines' []
  where getLines' iostrings =
          do line <- tryIOError (hGetLine hdl)
             case line of
               Left e -> if isEOFError e
                            then return (reverse iostrings)
                            else ioError e
               Right inp -> do getLines' (inp:iostrings)

getRequest :: Handle -> IO String
getRequest h =
  do oldBuffering <- hGetBuffering h
     hSetBuffering h LineBuffering
     line <- hGetLine h
     hSetBuffering h oldBuffering
     return line

app1 :: Handle -> IO ()
app1 h =
  do line <- getRequest h
     let (parsed, remainder) = matchGet line
     -- C.hPut h $ htmlPage $ C.pack (concat lines)
     hPutStr h $ htmlPage $ concat ["parsed=", parsed, "\r\n",
                                 "remainder=", remainder, "\r\n"]
     hFlush h


-- htmlPage :: C.ByteString -> C.ByteString
htmlPage :: String -> String
htmlPage inp =
  let header = "HTTP/1.0 200 OK\r\n" ++
               "Content-type: text/html\r\n\r\n"
      pre    =  "<html><head><title>Hello</title></head><body>"
      suf    = "</body>"
--  in  C.concat [C.pack header, C.pack pre, bytes, C.pack suf]
  in concat [header, pre, inp, suf]

matchGet :: String -> (String, String)
matchGet s = let m = Prelude.take 3 s
              in if m == "GET"
                  then ("GET", Prelude.drop 3 s)
                  else ("", s)
