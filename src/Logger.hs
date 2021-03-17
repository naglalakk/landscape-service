{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Logger where

import Colog
import Config (Environment (..))
import Control.Monad.IO.Class
  ( MonadIO,
  )
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
  ( modifyIORef',
    newIORef,
    readIORef,
  )
import qualified Data.Text as T
import qualified Data.TypeRepMap as TM
import Network.HTTP.Types.Status
import Network.Wai

formatStr :: String -> String -> String -> String -> String -> String -> String
formatStr path host method status req res =
  "Path: "
    ++ path
    ++ " - "
    ++ "Host: "
    ++ host
    ++ " - "
    ++ "Method: "
    ++ method
    ++ " - "
    ++ "Status code: "
    ++ status
    ++ " - "
    ++ "Request: "
    ++ req
    ++ " - "
    ++ "Response: "
    ++ response
  where
    response =
      if length res < 500 then res else take 500 res ++ "...output truncated..."

getWaiRequestBody :: Request -> IO BS.ByteString
getWaiRequestBody request = BS.concat <$> getChunks
  where
    getChunks :: IO [BS.ByteString]
    getChunks = getRequestBodyChunk request >>= \chunk ->
      if chunk == BS.empty then pure [] else (chunk :) <$> getChunks

responseBody :: Response -> IO BL.ByteString
responseBody res =
  let (status, headers, body) = responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content

loggerAction :: LogAction IO Message
loggerAction = richMessageAction

logMessage :: (WithLog env Message m, MonadIO m) => T.Text -> Severity -> m ()
logMessage msg sev = do
  case sev of
    Debug -> logDebug msg
    Info -> logInfo msg
    Warning -> logWarning msg
    Error -> logError msg

customLogger :: Environment -> Middleware
customLogger env app req sendResponse = do
  app req $ \rsp -> do
    rBody <- getWaiRequestBody req
    resBody <- responseBody rsp
    print (requestBodyLength req)
    let (status, headers, _) = responseToStream rsp
        sCode = statusCode status
        method = requestMethod req
        host = remoteHost req
        path = pathInfo req
        level = if sCode >= 400 then Error else Debug
        msg =
          T.pack $
            ( formatStr
                (show path)
                (show host)
                (show method)
                (show sCode)
                (show rBody)
                (show resBody)
            )
    case env of
      Production -> withLogTextFile "logs/server.log" $
        \logTextFile -> do
          let runApp :: LogAction IO Message -> IO ()
              runApp action = usingLoggerT action (logMessage msg level)
          let textAction = logTextStdout <> logTextStderr <> logTextFile
          let richMessageAction = cmapM fmtRichMessageDefault textAction
          let fullMessageAction = upgradeMessageAction defaultFieldMap richMessageAction
          let semiMessageAction =
                upgradeMessageAction
                  (TM.delete @"threadId" defaultFieldMap)
                  richMessageAction
          runApp fullMessageAction
      _ -> usingLoggerT loggerAction (logMessage msg level)
    sendResponse rsp
