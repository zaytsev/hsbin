{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DB (migrate, findItem, listItems, deleteItem, saveItem, getConnectionSettings) where

import Data.FileEmbed (embedDir)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, rowsAffectedStatement, singletonStatement, vectorStatement)

import qualified Data.ByteString.Char8 as Char8
import Data.Profunctor (Profunctor (dimap, rmap))
import Data.Vector (Vector)
import Hasql.Connection (Settings)
import Hasql.Migration (MigrationCommand (..), MigrationError, runMigration)
import Hasql.Session (Session)
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)
import Types

getConnectionSettings :: IO Settings
getConnectionSettings =
    Char8.unwords . catMaybes
        <$> sequence
            [ setting "host" $ defaultEnv "POSTGRES_HOST" "localhost"
            , setting "port" $ defaultEnv "POSTGRES_PORT" "5432"
            , setting "user" $ defaultEnv "POSTGRES_USER" "postgres"
            , setting "password" $ maybeEnv "POSTGRES_PASSWORD"
            , setting "dbname" $ defaultEnv "POSTGRES_DBNAME" "postgres"
            ]
  where
    maybeEnv env = fmap Char8.pack <$> lookupEnv env
    defaultEnv env val = Just . fromMaybe val <$> maybeEnv env
    setting label getEnv = do
        val <- getEnv
        return $ (\v -> label <> "=" <> v) <$> val

migrate :: Session [Maybe Hasql.Migration.MigrationError]
migrate = do
    let rawMigrations = $(embedDir "migrations")
    _ <- tx $ runMigration MigrationInitialization
    mapM applyMigration rawMigrations
  where
    tx = transaction ReadCommitted Write
    applyMigration (filePath, sqlSource) = do
        let migrationScript = MigrationScript filePath sqlSource
        tx $ runMigration migrationScript

findItem :: Statement PasteId (Maybe PasteItem)
findItem =
    rmap
        ( \case
            Just (i, title, body) -> Just $ PasteItem i title body
            Nothing -> Nothing
        )
        [maybeStatement|
          select id :: int4, title :: text, body :: text
          from "paste_item"
          where id = $1 :: int4
          |]

listItems :: Statement (Int, Int) (Vector PasteItem)
listItems =
    dimap
        (bimap fromIntegral fromIntegral)
        (fmap (\(i, t, b) -> PasteItem i t b))
        [vectorStatement|
                select id :: int4, title :: text, body :: text
                from "paste_item"
                limit $2 :: int4 offset $1 :: int4
                |]

saveItem :: Statement (Text, Text) PasteId
saveItem =
    [singletonStatement|
        insert into "paste_item" (title, body)
        values ($1 :: text, $2 :: text)
        returning id :: int4
        |]

deleteItem :: Statement Int32 Bool
deleteItem =
    rmap
        (> 0)
        [rowsAffectedStatement|
            delete from "paste_item"
            where id = $1 :: int4
            |]
