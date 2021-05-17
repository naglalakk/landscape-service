{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Token where

import Config (Config)
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
  )
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
  ( (.:),
    (.:?),
    (.=),
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )
import Database.Persist.Sql
  ( (==.),
    Entity (..),
    fromSqlKey,
    insert,
    selectFirst,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Db (runDb)
import GHC.Generics (Generic)
import Model.Image

share
  [mkPersist sqlSettings, mkMigrate "migrateToken"]
  [persistLowerCase|

-- | The Token type is used to register Tokens
Token json
  policyId Text
  amount Int            -- Amount in Lovelace
  UniqueAmount amount   -- Amounts are identifiers
  quantity Int 
  metadata Text Maybe
  createdAt UTCTime
  updatedAt UTCTime Maybe
  deriving Eq Generic

-- | TokenTransaction is used to track
--   requests for tokens
TokenTransaction json
  token TokenId Maybe
  status Text
  hash Text 
  UniqueHash hash
  txHash Text Maybe
  createdAt UTCTime
  updatedAt UTCTime Maybe
  deriving Eq Generic
|]

data TokenTransactionJSON
  = TokenTransactionJSON
      (Entity TokenTransaction)
      (Maybe (Entity Token))

instance ToJSON TokenTransactionJSON where
  toJSON (TokenTransactionJSON (Entity txId tokenTx) token) =
    object
      [ "id" .= fromSqlKey txId,
        "token" .= token,
        "status" .= tokenTransactionStatus tokenTx,
        "hash" .= tokenTransactionHash tokenTx,
        "txHash" .= tokenTransactionTxHash tokenTx,
        "createdAt" .= tokenTransactionCreatedAt tokenTx,
        "updatedAt" .= tokenTransactionUpdatedAt tokenTx
      ]

tokenTxToJSON ::
  (MonadReader Config m, MonadIO m) =>
  Entity TokenTransaction ->
  m TokenTransactionJSON
tokenTxToJSON (Entity tokenTxId tokenTx) = do
  let tkn = tokenTransactionToken tokenTx
  token <- case tkn of
    Just tId -> runDb $ selectFirst [TokenId ==. tId] []
    Nothing -> return Nothing
  return $
    TokenTransactionJSON
      (Entity tokenTxId tokenTx)
      token
