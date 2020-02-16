module Cli where

import           Options.Applicative
import           Data.Semigroup           ((<>))

data Command
    = RunCommand 
        { port :: Int
        }
    | CreateUserCommand
        { username :: String
        , password :: String
        , email    :: Maybe String
        , isAdmin  :: Bool
        }
    deriving (Eq, Show)

runCommand :: Parser Command
runCommand = RunCommand
          <$> option auto
            ( long "port"
            <> short 'p'
            <> help "Port number to run the server on"
            <> showDefault
            <> value 8081 
            <> metavar "INT"
            )

runOptions :: ParserInfo Command
runOptions = info (runCommand <**> helper)
    ( fullDesc
    <> progDesc "Run the API server"
    <> header   "run" )

createUserCommand :: Parser Command
createUserCommand = CreateUserCommand
                 <$> strOption
                    ( long "username"
                    <> short 'u'
                    <> help "Username for new user")
                 <*> strOption
                    ( long "password"
                    <> short 'p'
                    <> help "Password for new user")
                 <*> optional (strOption
                    ( long "email"
                    <> short 'e'
                    <> help "Email for new user"))
                <*> switch
                    ( long "admin"
                    <> short 'a'
                    <> help "New user is administrator" )

createUserOptions :: ParserInfo Command
createUserOptions = info (createUserCommand <**> helper)
    ( fullDesc
    <> progDesc "Create a new user"
    <> header   "createuser" )

opts :: Parser Command
opts = subparser
    ( command "run" runOptions
    <> command "createuser" createUserOptions
    )
