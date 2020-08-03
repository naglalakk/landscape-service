module Models where

import           Database.Persist.Sql           ( SqlPersistT
                                                , runMigration
                                                )

import           Model.BlogPost
import           Model.Image
import           Model.User
import           Model.Tag

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration migrateBlogPost
  runMigration migrateImage
  runMigration migrateUser
  runMigration migrateTag
