module Logger where

import Data.IORef (newIORef,modifyIORef',readIORef)
import Data.Monoid ((<>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import System.Log.Logger
import Network.Wai
import Network.HTTP.Types.Status

formatStr :: String -> String -> String -> String -> String -> String -> String
formatStr path host method status req res = 
    "Path: " ++ path ++ "\n" ++
    "Host: " ++ host ++ "\n" ++ 
    "Method: " ++ method ++ "\n" ++
    "Status code: " ++ status ++ "\n" ++
    "Request: " ++ req ++ "\n" ++ 
    "Response: " ++ response ++ "\n"
    where
        response = if length res < 500 then res else take 500 res ++ "...output truncated..."


responseBody :: Response -> IO ByteString
responseBody res =
    let (status,headers,body) = responseToStream res in
    body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content

apiFileLogger :: Middleware
apiFileLogger app req sendResponse = do
    app req $ \rsp -> do
        let (status, headers, _) = responseToStream rsp
        let sCode = statusCode status
        rBody <- (strictRequestBody req)
        let method = requestMethod req
        let host = remoteHost req
        let path = pathInfo req
        resBody <- responseBody rsp
        let level = if sCode >= 400 then errorM else debugM
        level "BlogService"         (formatStr 
                                    (show path) 
                                    (show host) 
                                    (show method) 
                                    (show sCode) 
                                    (show rBody) 
                                    (show resBody))
        sendResponse rsp
