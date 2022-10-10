{-# LANGUAGE DeriveGeneric #-}

module Types (PasteId, PasteItem (..)) where

import Data.Aeson (ToJSON)

type PasteId = Int32

data PasteItem = PasteItem
    { itemId :: PasteId
    , itemTitle :: Text
    , itemBody :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON PasteItem
