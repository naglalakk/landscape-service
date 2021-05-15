module Models where

import Database.Persist.Sql
  ( SqlPersistT,
    runMigration,
  )
import Model.BlogPost
import Model.Exhibition
import Model.Image
import Model.Item
import Model.Tag
import Model.Token
import Model.User

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration migrateBlogPost
  runMigration migrateExhibition
  runMigration migrateImage
  runMigration migrateItem
  runMigration migrateTag
  runMigration migrateToken
  runMigration migrateUser
