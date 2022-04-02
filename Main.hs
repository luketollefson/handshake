-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.IORef


import Cli


-- |
-- = Our program's main that ties it together
main :: IO ()
main = do
    opts <- cli
    putStrLn ("Starting our " <> show opts)
    case opts of
        Server -> mainServer
        Client -> do ref <- newIORef (0 :: Int)
                     forever $ do
                        v <- readIORef ref
                        putStr (show v <> ": ")
                        mainClient
                        modifyIORef ref (+1)
-- use mtl or polysemy, we'll use mtl, then port to polysemy


-- |
-- = Server code
mainServer :: IO ()
mainServer = do
    runTCPServer Nothing "3000" talk
        where talk s = do
                msg <- recv s 1024
                unless (S.null msg) $ do
                    sendAll s msg
                    talk s

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)



-- |
-- = Client code
mainClient :: IO ()
mainClient = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "Hello, world!"
    msg <- recv s 1024
    threadDelay 300000
    putStr "Received: "
    C.putStrLn msg

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock


class Networked m where
  clientSend :: Text -> m Text
  serverSend :: Text -> m Text

newtype Client a = Client { runClient :: IO a }
newtype Server a = Server { runServer :: IO a }

instance Networked Client where
  clientSend t = sendViaNetwork t
  serverSend _ = recvViaNetwork

instance Networked Server where
  serverSend t = sendViaNetwork t
  clientSend _ = recvViaNetwork

runClientApprovalFlow = runClient approvalFlow
runServerApprovalFlow = runServer approvalFlow


combineCode :: Socket -> IO ()
combineCode s = do
    client $ sendAll s "Hello, world!"
    msg <- client $ recv s 1024
    client $ putStr "Received: "
    client $ C.putStrLn msg
    msg <- server $ recv s 1024
    server $ unless (S.null msg) $ do
        server $ sendAll s msg
        server $ combineCode s
    where client = undefined
          server = undefined

combineCode' :: IO ()
combineCode' =
    msg <- clientSend $ "Hello, world"
    server $ unless (S.null msg) $ do
        server $ serverSend msg
        server $ combineCode s
    msg' <- serverSend $ msg
    client $ putStr "Received: "
    client $ C.putStrLn msg

-- start with mtl, good examples. Move to polysemy
-- Lets go simpiler

combineCode'' :: IO ()
combineCode'' =
    msg <- client $ pure "Hello, world"
    unless (S.null msg) $ do
        msg' <- server $ pure msg
        client $ putStr "Received: "
        client $ C.putStrLn msg'
        server $ combineCode''

combineCode3 :: IO ()
combineCode3 =
    msg <- clientSend "Hello, world"
    unless (S.null msg) $ do
        msg' <- serverSend msg
        clientDo $ putStr "Received: "
        clientDo $ C.putStrLn msg'
        server $ combineCode''


