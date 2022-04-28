-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as S
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
-- import Data.IORef
-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Control.Applicative ( Applicative(liftA2) )
import Debug.Trace
import Data.Binary
import Control.Exception ( try, IOException )
import Data.ByteString.Lazy (toStrict, fromStrict)


import Cli ( cli, Opts(Client, Server) )


-- |
-- = Our program's main that ties it together
main :: IO ()
main = do
    opts <- cli
    putStrLn ("Starting our " <> show opts)
    case opts of
        Server -> runTCPCombine (Just "127.0.0.1") "3000" (runServer $ approvalFlow 0)
        Client -> runTCPCombine (Just "127.0.0.1") "3000" (runClient $ approvalFlow 0)
    pure ()


-- use mtl or polysemy, we'll use mtl, then port to polysemy

runTCPCombine :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPCombine mhost port handshake = withSocketsDo $ do
    addr <- resolve
    E.bracket (openCombine addr) (\(_,s) -> close s) runHandshake
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    openCombine addr = openClient addr <|> openServer addr
    openServer addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return (Server, sock)
    openClient addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return (Client, sock)
    runHandshake (Client, sock) = handshake sock
    runHandshake (Server, sock) = trace "here?" $ forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (handshake conn) (const $ gracefulClose conn 5000)

class Networked m where
  clientSend :: (Monad m, Binary t) => t -> m t
--   clientDo :: Monad m => m t -> m (Maybe t)
  serverSend :: (Monad m, Binary t) => t -> m t

-- Pretty much the reader monad. Do I have a better way?
-- IO seems heavy handed
newtype Client' a = Client' { runClient :: Socket -> IO a }
newtype Server' a = Server' { runServer :: Socket -> IO a }

instance Functor Client' where
    fmap f a = Client' $ \s -> f <$> runClient a s

instance Applicative Client' where
    pure a = Client' $ \_ -> pure a
    liftA2 f a b = Client' $ \s -> f <$> runClient a s <*> runClient b s

instance Monad Client' where
    return a = Client' $ \_ -> pure a
    m >>= k = Client' $ \s -> runClient m s >>= (\r -> runClient (k r) s)


instance Functor Server' where
    fmap f a = Server' $ \s -> f <$> runServer a s

instance Applicative Server' where
    pure a = Server' $ \_ -> pure a
    liftA2 f a b = Server' $ \s -> f <$> runServer a s <*> runServer b s

instance Monad Server' where
    return a = Server' $ \_ -> pure a
    m >>= k = Server' $ \s -> runServer m s >>= (\r -> runServer (k r) s)

instance Networked Client' where
  clientSend t = Client' $ \s -> sendAll s (encode t) $> t
--   clientDo m   = Just <$> m
  serverSend _ = Client' $ \s -> decode <$> recv s 1024

instance Networked Server' where
  serverSend t = Server' $ \s -> sendAll s (encode t) $> t
--   clientDo _   = pure Nothing
  clientSend _ = Server' $ \s -> decode <$> recv s 1024

-- runClientApprovalFlow = runClient approvalFlow
-- runServerApprovalFlow = runServer approvalFlow

-- approvalFlow :: Networked Client'
approvalFlow :: (Monad f, Networked f) => Int -> f ()
approvalFlow i0 = do
    i1 <- clientSend $ trace (concat ["c1 - R: ", show i0, " S: ", show $ succ i0]) $ succ i0
    i2 <- serverSend $ trace (concat ["c1 - R: ", show i1, " S: ", show $ succ i1]) $ succ i1
    i3 <- clientSend $ trace (concat ["c1 - R: ", show i2, " S: ", show $ succ i2]) $ succ i2
    i4 <- serverSend $ trace (concat ["c1 - R: ", show i3, " S: ", show $ succ i3]) $ succ i3
    i5 <- clientSend $ trace (concat ["c1 - R: ", show i4, " S: ", show $ succ i4]) $ succ i4
    -- traceM "hello"
    -- -- _  <- clientSend $ trace "Do this" $ ("hello" :: String)
    i6 <- serverSend $ trace (concat ["c1 - R: ", show i5, " S: ", show $ succ i5]) $ succ i5
    i7 <- clientSend $ trace (concat ["c1 - R: ", show i6, " S: ", show $ succ i6]) $ succ i6
    i8 <- serverSend $ trace (concat ["c1 - R: ", show i7, " S: ", show $ succ i7]) $ succ i7
    -- pure ()
    approvalFlow i8

-- approvalFlow :: Networked Client'
approvalFlow' :: (Monad f, MonadIO f, Networked f) => Int -> f ()
approvalFlow' i0 = do
    i1 <- clientSend $ trace (concat ["c1 - R: ", show i0, " S: ", show $ succ i0]) $ succ i0
    -- readString <- clientDo getLine
    readString <- clientSend getLine    -- when we do this, MonadIO is just only on the client. Not the server! How can I encode this?
    i2 <- serverSend $ trace (concat ["c1 - R: ", show i1, " S: ", show $ succ i1]) $ succ i1
    -- pure ()
    approvalFlow i2


