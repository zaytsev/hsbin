{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Lib (
    api,
    app,
    mkEnv,
) where

import Control.Exception (try)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON (..), defaultOptions, genericParseJSON)
import Servant (Capture, DeleteNoContent, Get, Handler, HasServer (ServerT), JSON, NoContent (..), PostCreated, QueryParam, ReqBody, err500, hoistServer, type (:<|>) (..), type (:>))
import Servant.Server (Handler (Handler), Server, err404)
import Prelude hiding (ask)

import Hasql.Pool (Pool, use)
import Hasql.Session (statement)
import Types

import qualified DB

data PasteEntry = Entry
    { entryTitle :: Text
    , entryBody :: Text
    }
    deriving
        (Show, Generic)

instance FromJSON PasteEntry where
    parseJSON = genericParseJSON defaultOptions

type ListItems = QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] [PasteItem]
type GetItem = Capture "paste_id" PasteId :> Get '[JSON] PasteItem
type AddEntry = ReqBody '[JSON] PasteEntry :> PostCreated '[JSON] PasteItem
type DeleteEntry = Capture "entryid" PasteId :> DeleteNoContent
type PasteBinAPI = "paste" :> (ListItems :<|> GetItem :<|> AddEntry :<|> DeleteEntry)

data Env = Env
    { _db :: Pool
    }

mkEnv :: Pool -> Env
mkEnv = Env

newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
    deriving
        (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadUnliftIO, MonadReader Env)

api :: Proxy PasteBinAPI
api = Proxy

server :: ServerT PasteBinAPI AppM
server = listItems :<|> getItem :<|> createItem :<|> deleteItem

app :: Env -> Server PasteBinAPI
app config = hoistServer api (nt config) server
  where
    nt :: Env -> AppM a -> Handler a
    nt env m = Handler $ ExceptT $ try $ runReaderT (runAppM m) env

deleteItem :: PasteId -> AppM NoContent
deleteItem i = do
    Env{_db = db} <- ask
    deleteResult <- liftIO $ use db $ statement i DB.deleteItem
    case deleteResult of
        Left _ -> throwM err500
        Right deleted ->
            if deleted
                then return NoContent
                else throwM err404

createItem :: PasteEntry -> AppM PasteItem
createItem Entry{entryTitle = title, entryBody = body} = do
    Env{_db = db} <- ask
    createResult <- liftIO $ use db $ statement (title, body) DB.saveItem
    case createResult of
        Left _ -> throwM err500
        Right newId ->
            return $
                PasteItem
                    { itemId = newId
                    , itemTitle = title
                    , itemBody = body
                    }

getItem :: PasteId -> AppM PasteItem
getItem i = do
    Env{_db = db} <- ask
    findResult <- liftIO $ use db $ statement i DB.findItem
    case findResult of
        Left _ -> throwM err500
        Right item -> case item of
            Nothing -> throwM err404
            Just x -> return x

listItems :: Maybe Int -> Maybe Int -> AppM [PasteItem]
listItems offset limit = do
    Env{_db = db} <- ask
    listResult <- liftIO $ use db $ statement (fromMaybe 0 offset, fromMaybe 25 limit) DB.listItems
    case listResult of
        Left _ -> throwM err500
        Right items -> return $ toList items
