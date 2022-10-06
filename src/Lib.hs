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
import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON)
import Servant (Capture, DeleteNoContent, Get, Handler, HasServer (ServerT), JSON, NoContent (..), PostCreated, QueryParam, ReqBody, hoistServer, type (:<|>) (..), type (:>))
import Servant.Server (Handler (Handler), Server, err404)
import Prelude hiding (ask)

data PasteEntry = Entry
    { entryTitle :: Text
    , entryBody :: Text
    }
    deriving
        (Show, Generic)

instance FromJSON PasteEntry where
    parseJSON = genericParseJSON defaultOptions

type PasteId = Int

data PasteItem = PasteItem
    { itemId :: PasteId
    , itemTitle :: Text
    , itemBody :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON PasteItem

type ListItems = QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] [PasteItem]
type GetItem = Capture "paste_id" PasteId :> Get '[JSON] PasteItem
type AddEntry = ReqBody '[JSON] PasteEntry :> PostCreated '[JSON] PasteItem
type DeleteEntry = Capture "entryid" PasteId :> DeleteNoContent
type PasteBinAPI = "paste" :> (ListItems :<|> GetItem :<|> AddEntry :<|> DeleteEntry)

data Env = Env
    { pasteItems :: TVar [PasteItem]
    , pasteCounter :: TVar Int
    }

mkEnv :: IO Env
mkEnv = do
    items <- newTVarIO []
    counter <- newTVarIO 0
    return
        Env
            { pasteItems = items
            , pasteCounter = counter
            }

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

deleteItem :: Int -> AppM NoContent
deleteItem i = do
    env <- ask
    atomically $ modifyTVar' (pasteItems env) $ filter (\x -> itemId x /= i)
    return NoContent

createItem :: PasteEntry -> AppM PasteItem
createItem entry = do
    Env{pasteItems = items, pasteCounter = counter} <- ask
    i <- readTVarIO counter
    let newItem =
            PasteItem
                { itemId = i
                , itemTitle = entryTitle entry
                , itemBody = entryBody entry
                }
    atomically $ do
        modifyTVar' counter (+ 1)
        modifyTVar' items (newItem :)
    return newItem

getItem :: Int -> AppM PasteItem
getItem i = do
    env <- ask
    items <- readTVarIO $ pasteItems env
    let item = find (\x -> i == itemId x) items
    case item of
        Nothing -> throwM err404
        Just x -> return x

listItems :: Maybe Int -> Maybe Int -> AppM [PasteItem]
listItems _offset _limit = do
    env <- ask
    readTVarIO $ pasteItems env
