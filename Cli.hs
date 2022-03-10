module Cli (
    Opts(..)
,   cli
) where

import Options.Applicative

-- | Our command line options 
data Opts
    -- | Run handshake as a server
    = Server
    -- | Run handshake as a client 
    | Client deriving (Eq, Show)

-- | The cli entrypoint 
cli :: IO Opts
cli = execParser optsParser

-- | The cli parser
optsParser :: ParserInfo Opts
optsParser = 
    info (helper <*> programOptions)
         (fullDesc
         <> header "Server-Client combined demo"
         <> progDesc "Write server-client code as a combined function")

-- | The cli's options
programOptions :: Parser Opts
programOptions = server <|> client
    where 
        server = flag' Server
            ( long "server"
            <> short 's'
            <> help "Run as a server" )
        client = flag' Client
            ( long "client"
            <> short 'c'
            <> help "Run as a client" )
