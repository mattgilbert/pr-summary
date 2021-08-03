{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import GHC.Generics
import Data.Aeson (FromJSON(..))
import Data.Text as T
import Data.Time.Clock

data Config = Config
    { username :: Text
    , token :: Text
    , orgName :: Text
    }
    deriving (Show)

data Repository = Repository
    { name :: Text
    , url :: Text
    }
    deriving (Generic, Show)

instance FromJSON Repository

data PRList = PRList
    { total_count :: Int
    , incomplete_results :: Bool
    , items :: [PR]
    }
    deriving (Generic, Show)

data PRUser = PRUser
    { login :: Text
    , id :: Int
    }
    deriving (Generic, Show)

type PRNumber = Int
data PR = PR 
    { id :: Int
    , number :: Int 
    , title :: Text
    , created_at :: UTCTime
    , repository_url :: Text
    , html_url :: Text
    , user :: PRUser
    , draft :: Bool
    , labels :: [PRLabel]
    }
    deriving (Generic, Show)

data PRLabel = PRLabel
    { name :: Text 
    }
    deriving (Generic, Show)

data PRReview = PRReview
    { user :: PRUser
    , state :: Text
    }
    deriving (Generic, Show)

data PRComment = PRComment
    { id :: Int
    , user :: PRUser
    }
    deriving (Generic, Show)

data PRReviewSummary = PRReviewSummary { reviewCount :: Int, curUserApproved :: Bool }

instance FromJSON PRReview
instance FromJSON PRComment
instance FromJSON PRUser
instance FromJSON PRList
instance FromJSON PR
instance FromJSON PRLabel

instance Eq PR where
    PR{id=id1} == PR{id=id2} = id1 == id2
instance Ord PR where
    compare PR{id=id1} PR{id=id2} = compare id1 id2
