{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Tokens represent Native Tokens, minted on the Cardano blockchain.
--   This API keeps track of their state and transactions
module Api.Token where

import Config (AppT (..))
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Aeson
import Data.Either (Either (..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe
  ( Maybe (..),
    fromJust,
    fromMaybe,
    isJust,
  )
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.Bloodhound as BH
import Database.Persist.Sql
  ( (!=.),
    (<-.),
    (=.),
    (==.),
    Entity (..),
    Filter (..),
    SelectOpt (..),
    delete,
    fromSqlKey,
    insert,
    selectFirst,
    selectList,
    toSqlKey,
    updateGet,
  )
import Db (runDb)
import Model.Token
import Model.User (User (..))
import Network.HTTP.Client (responseBody)
import Servant

type TokenUnprotecedAPI =
  "tokens"
    :> Capture "tokenId" Int
    :> "request"
    :> Get '[JSON] (Either String TokenTransactionJSON)
    :<|> "tokens"
    :> "transactions"
    :> Capture "hash" T.Text
    :> Get '[JSON] (Maybe TokenTransactionJSON)

type TokenProtectedAPI =
  "tokens"
    :> Get '[JSON] [Entity Token]
    :<|> "tokens"
    :> Capture "tokenId" Int
    :> Get '[JSON] (Maybe (Entity Token))
    :<|> "tokens"
    :> ReqBody '[JSON] Token
    :> Post '[JSON] (Maybe (Entity Token))
    :<|> "tokens"
    :> Capture "tokenId" Int
    :> "update"
    :> ReqBody '[JSON] Token
    :> Post '[JSON] (Maybe (Entity Token))
    :<|> "tokens"
    :> Capture "tokenId" Int
    :> Delete '[JSON] ()
    :<|> "tokens"
    :> "transactions"
    :> QueryParam "status" T.Text
    :> Get '[JSON] [TokenTransactionJSON]
    :<|> "tokens"
    :> "transactions"
    :> "update"
    :> ReqBody '[JSON] TokenTransaction
    :> Post '[JSON] (Maybe TokenTransactionJSON)

type TokenAPI =
  TokenUnprotecedAPI
    :<|> BasicAuth "user-auth" (Entity User)
    :> TokenProtectedAPI

tokenProtectedServer ::
  MonadIO m =>
  Entity User ->
  ServerT TokenProtectedAPI (AppT m)
tokenProtectedServer (user :: (Entity User)) =
  allTokens
    :<|> getToken
    :<|> createToken
    :<|> updateToken
    :<|> deleteToken
    :<|> allTokenTransactions
    :<|> updateTokenTransaction

tokenUnprotectedServer ::
  MonadIO m =>
  ServerT TokenUnprotecedAPI (AppT m)
tokenUnprotectedServer =
  requestToken
    :<|> getTokenTransaction

tokenServer ::
  MonadIO m =>
  ServerT TokenAPI (AppT m)
tokenServer =
  tokenUnprotectedServer
    :<|> tokenProtectedServer

-- | GET all registered tokens.
--   Endpoint: </tokens/>
allTokens :: MonadIO m => AppT m [Entity Token]
allTokens = runDb $ selectList [] []

-- | GET a token by ID.
--   Endpoint: </tokens/id/>
getToken :: MonadIO m => Int -> AppT m (Maybe (Entity Token))
getToken tokenId = runDb $ selectFirst [TokenId ==. sqlKey] []
  where
    sqlKey = toSqlKey $ fromIntegral tokenId

-- | Create a token.
--   Endpoint: </tokens/>
createToken :: MonadIO m => Token -> AppT m (Maybe (Entity Token))
createToken token = do
  tkn <- runDb $ insert token
  return $ Just $ Entity tkn token

-- | Update a token.
--   Endpoint: </tokens/id/update/>
updateToken :: MonadIO m => Int -> Token -> AppT m (Maybe (Entity Token))
updateToken tokenId token = do
  now <- liftIO getCurrentTime
  updatedRec <-
    runDb $
      updateGet
        sqlKey
        [ TokenPolicyId =. tokenPolicyId token,
          TokenAmount =. tokenAmount token,
          TokenQuantity =. tokenQuantity token,
          TokenMetadata =. tokenMetadata token,
          TokenUpdatedAt =. Just now
        ]
  return $ Just $ Entity sqlKey updatedRec
  where
    sqlKey = toSqlKey $ fromIntegral tokenId

-- | Delete a token.
--   Endpoint: </tokens/id/delete/>
deleteToken :: MonadIO m => Int -> AppT m ()
deleteToken tokenId = runDb $ delete sqlKey
  where
    sqlKey = toSqlKey $ fromIntegral tokenId :: TokenId

-- | Request a token.
--   Endpoint: </tokens/id/request/>
requestToken ::
  MonadIO m =>
  Int ->
  AppT m (Either String TokenTransactionJSON)
requestToken tokenId = do
  tkn <- runDb $ selectFirst [TokenId ==. tokenKey] []
  case tkn of
    Just (Entity tknId token) -> do
      -- We need to compare the current amount of
      -- active transactions against the quantity
      transactions <-
        runDb $
          selectList
            [ TokenTransactionToken ==. Just tokenKey,
              TokenTransactionStatus !=. "expired"
            ]
            []
      let transAmount = length transactions
          quantity = tokenQuantity token
      if transAmount == quantity
        then return $ Left "Error: All tokens have been requested"
        else do
          now <- liftIO getCurrentTime
          uuid <- liftIO $ UUID.toString <$> UUID.nextRandom
          let tokenTransaction =
                TokenTransaction
                  { tokenTransactionToken = Just tokenKey,
                    tokenTransactionStatus = "request",
                    tokenTransactionHash = T.pack uuid,
                    tokenTransactionTxHash = Nothing,
                    tokenTransactionCreatedAt = now,
                    tokenTransactionUpdatedAt = Nothing
                  }
          tknTrans <- runDb $ insert tokenTransaction
          json <- tokenTxToJSON (Entity tknTrans tokenTransaction)
          return $ Right json
    Nothing -> return $ Left "Token not found"
  where
    tokenKey = toSqlKey $ fromIntegral tokenId

-- | GET all TokenTransactions.
--   Endpoint: </tokens/transactions/>
--
--   This endpoint takes a QueryParam 'status'
--   as optional argument
--
--   e.g. list all completed TokenTransactions
--   </tokens/transactions/?status=completed>
allTokenTransactions ::
  MonadIO m =>
  Maybe T.Text ->
  AppT m [TokenTransactionJSON]
allTokenTransactions status = do
  let filt = case status of
        Just st -> [TokenTransactionStatus ==. st]
        Nothing -> []
  tokenTxs <- runDb $ selectList filt []
  mapM tokenTxToJSON tokenTxs

-- | Update a TokenTransaction.
--   Endpoint: </tokens/transactions/update>
--
--   Only the status property will be affected on update.
--   All other fields will be ignored.
updateTokenTransaction ::
  MonadIO m =>
  TokenTransaction ->
  AppT m (Maybe TokenTransactionJSON)
updateTokenTransaction tokenTx = do
  lookup <- runDb $ selectFirst [TokenTransactionHash ==. tokenTransactionHash tokenTx] []
  case lookup of
    Just (Entity tokenTxId tokenTx) -> do
      now <- liftIO getCurrentTime
      updatedRec <-
        runDb $
          updateGet
            tokenTxId
            [ TokenTransactionStatus =. tokenTransactionStatus tokenTx,
              TokenTransactionUpdatedAt =. Just now
            ]
      json <- tokenTxToJSON $ Entity tokenTxId updatedRec
      return $ Just json
    Nothing -> return Nothing

-- | GET a TokenTransaction by hash.
--   Endpoint: </tokens/transactions/hash/>
--
--   This endpoint is intended to be public
--   for users that require a Token
--   and want to check on status
getTokenTransaction ::
  MonadIO m =>
  T.Text ->
  AppT m (Maybe TokenTransactionJSON)
getTokenTransaction hash = do
  tknTx <- runDb $ selectFirst [TokenTransactionHash ==. hash] []
  case tknTx of
    Just tokenTx -> do
      json <- tokenTxToJSON tokenTx
      return $ Just json
    Nothing -> return Nothing
