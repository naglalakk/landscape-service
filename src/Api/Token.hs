{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Tokens represent Native Tokens, minted on the Cardano blockchain.
--   This API keeps track of their state and transactions
module Api.Token where

import Config (AppT (..))
import Control.Monad (when)
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
    update,
    updateGet,
    (||.),
  )
import Db (runDb)
import Model.Token
import Model.User (User (..))
import Network.HTTP.Client (responseBody)
import Servant

type TokenUnprotecedAPI =
  "tokens"
    :> Get '[JSON] [Entity Token]
    :<|> "tokens"
    :> Capture "tokenId" Int
    :> Get '[JSON] (Maybe (Entity Token))
    :<|> "tokens"
    :> "amount"
    :> Capture "hash" T.Text
    :> Get '[JSON] (Maybe Int)
    :<|> "tokens"
    :> Capture "tokenId" Int
    :> "request"
    :> Get '[JSON] (Maybe TokenTransactionJSON)
    :<|> "tokens"
    :> "transactions"
    :> Capture "hash" T.Text
    :> Get '[JSON] (Maybe TokenTransactionJSON)
    :<|> "tokens"
    :> "transactions"
    :> "update"
    :> "status"
    :> Capture "hash" T.Text
    :> Capture "status" T.Text
    :> Get '[JSON] (Maybe TokenTransactionJSON)

type TokenProtectedAPI =
  "tokens"
    :> Capture "tokenId" Int
    :> "amount"
    :> Get '[JSON] (Maybe Int)
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
  getTokenAmountById
    :<|> createToken
    :<|> updateToken
    :<|> deleteToken
    :<|> allTokenTransactions
    :<|> updateTokenTransaction

tokenUnprotectedServer ::
  MonadIO m =>
  ServerT TokenUnprotecedAPI (AppT m)
tokenUnprotectedServer =
  allTokens
    :<|> getToken
    :<|> getTokenAmountByHash
    :<|> requestToken
    :<|> getTokenTransaction
    :<|> updateTxStatus

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

-- | Get token amount in Lovelace
--   Endpoint: </tokens/id/amount>
getTokenAmountById :: MonadIO m => Int -> AppT m (Maybe Int)
getTokenAmountById tokenId = do
  token <- runDb $ selectFirst [TokenId ==. sqlKey] []
  case token of
    Just (Entity tkId tkn) -> return $ Just $ tokenAmount tkn
    Nothing -> return Nothing
  where
    sqlKey = toSqlKey $ fromIntegral tokenId

-- | Get token amount by TokenTransaction hash
--   Endpoint: </tokens/hash/amount/>
getTokenAmountByHash :: MonadIO m => T.Text -> AppT m (Maybe Int)
getTokenAmountByHash hash = do
  tx <- runDb $ selectFirst [TokenTransactionHash ==. hash] []
  case tx of
    Just (Entity txId tokenTrans) -> do
      case tokenTransactionToken tokenTrans of
        Just tId -> do
          tk <- runDb $ selectFirst [TokenId ==. tId] []
          case tk of
            Just (Entity tokenId token) -> return $ Just $ tokenAmount token
            Nothing -> return Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing

-- | Create a token.
--   Endpoint: </tokens/>
createToken :: MonadIO m => Token -> AppT m (Maybe (Entity Token))
createToken token = do
  now <- liftIO getCurrentTime
  tkn <-
    runDb $ insert $
      token
        { tokenAvailable = tokenQuantity token,
          tokenCreatedAt = now
        }
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
        [ TokenTitle =. tokenTitle token,
          TokenPolicyId =. tokenPolicyId token,
          TokenAmount =. tokenAmount token,
          TokenQuantity =. tokenQuantity token,
          TokenAvailable =. tokenAvailable token,
          TokenMinted =. tokenMinted token,
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
  AppT m (Maybe TokenTransactionJSON)
requestToken tokenId = do
  tkn <- runDb $ selectFirst [TokenId ==. tokenKey] []
  case tkn of
    Just (Entity tknId token) -> do
      -- compare the current amount of
      -- active transactions against the quantity
      transactions <-
        runDb $
          selectList
            ( [TokenTransactionToken ==. Just tokenKey]
                <> ( [TokenTransactionStatus !=. "expired"]
                       ++ [TokenTransactionStatus !=. "cancelled"]
                   )
            )
            []
      let transAmount = length transactions
          quantity = tokenQuantity token
      if transAmount == quantity
        then return Nothing
        else do
          -- Update token availability
          runDb $ update tknId [TokenAvailable =. tokenAvailable token - 1]
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
          return $ Just json
    Nothing -> return Nothing
  where
    tokenKey = toSqlKey $ fromIntegral tokenId

-- | Updates the status of a TokenTransaction
-- Allowed options: expired, cancelled, completed
-- requires the TokenTransaction hash and status
-- returns said TokenTransaction if a valid status is passed
-- Endpoint: </tokens/transactions/update/status/hash/status>
updateTxStatus :: MonadIO m => T.Text -> T.Text -> AppT m (Maybe TokenTransactionJSON)
updateTxStatus hash status =
  if status == "request" || status == "minting"
    then return Nothing
    else do
      tx <- runDb $ selectFirst [TokenTransactionHash ==. hash] []
      case tx of
        Just (Entity txId tokenTx) ->
          case tokenTransactionToken tokenTx of
            Just tokenId -> do
              token <- runDb $ selectFirst [TokenId ==. tokenId] []
              case token of
                Just (Entity tkId tkn) -> do
                  -- Update token availability
                  -- if user cancelled or Token expired
                  when (status == "cancelled" || status == "expired")
                    $ runDb
                    $ update tkId [TokenAvailable =. tokenAvailable tkn + 1]
                  now <- liftIO getCurrentTime
                  newTx <- runDb $ updateGet txId [TokenTransactionStatus =. status, TokenTransactionUpdatedAt =. Just now]
                  json <- tokenTxToJSON $ Entity txId newTx
                  return $ Just json
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing

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
    Just (Entity tokenTxId tTx) -> do
      now <- liftIO getCurrentTime
      updatedRec <-
        runDb $
          updateGet
            tokenTxId
            [ TokenTransactionStatus =. tokenTransactionStatus tokenTx,
              TokenTransactionTxHash =. tokenTransactionTxHash tokenTx,
              TokenTransactionUpdatedAt =. Just now
            ]
      json <- tokenTxToJSON $ Entity tokenTxId updatedRec
      return $ Just json
    Nothing -> return Nothing

-- | GET a TokenTransaction by hash.
--   Endpoint: </tokens/transactions/hash/>
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
