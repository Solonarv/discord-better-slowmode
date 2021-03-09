{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Orphans where

import Data.Aeson (FromJSONKey(..), ToJSONKey(..))
import Data.Hashable (Hashable(..))
import Discord.Types (Snowflake(..))

deriving newtype instance FromJSONKey Snowflake
deriving newtype instance ToJSONKey Snowflake

deriving newtype instance Hashable Snowflake