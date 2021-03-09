{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Foldable
import Data.Functor
import GHC.Generics (Generic)
import qualified System.Environment as System
import System.Exit (die)

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import Discord
import Discord.Types
import qualified Discord.Requests as R
import UnliftIO

import THashMap (THashMap)
import qualified THashMap
import Orphans ()

main :: IO ()
main = do
  cfg <- getConfig
  err <- runDiscord def
    { discordToken = cfgToken cfg
    , discordOnEvent = handler cfg
    }
  Text.hPutStrLn stderr err

-- | The bot's internal state while running.
data Config = Config
  { cfgToken :: Text
  , cfgDatabaseFile :: FilePath
  , cfgDatabase :: THashMap ChannelId ChannelData
  }

-- | Describes how the bot should behave in a given channel.
data ChannelData = ChannelData
  { cdModerators :: (HashSet UserId, HashSet RoleId)
  , cdLastMessageTimes :: HashMap UserId UTCTime
  , cdRules :: [Rule]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A single rule that applies to some set of users in a given channel.
-- Reads as: "If a user in <specific set of users and roles> posts a message, 
-- and their last message was less than <ruleMinimumDelay> ago, take the actions
-- specified in <ruleConsequences>."
data Rule = Rule
  { ruleAffectedUsers :: HashSet UserId
  , ruleIgnoredUsers :: HashSet UserId
  , ruleAffectedRoles :: HashSet RoleId
  , ruleIgnoredRoles :: HashSet RoleId
  , ruleMinmumDelay :: NominalDiffTime
  , ruleDeleteGracePeriod :: NominalDiffTime
  , ruleConsequences :: Consequences
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Decide whether a rule applies to a given user based on their roles.
ruleShouldAffect :: Rule -> UserId -> [RoleId] -> Bool
ruleShouldAffect Rule{..} authorId roles = not ignored && affected
  where
    affected = authorId `HashSet.member` ruleAffectedUsers
      || any (`HashSet.member` ruleAffectedRoles) roles
    ignored = authorId `HashSet.member` ruleIgnoredUsers
      || any (`HashSet.member` ruleIgnoredRoles) roles

-- Various actions which the bot can take in response to a too-early message.
data Consequences = Consequences
  { csqDeleteMessage :: Bool
  , csqAssignRoles :: HashSet RoleId
  , csqSendMessages :: HashSet ChannelId
  }
  deriving (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Consequences where
  Consequences d a m <> Consequences d' a' m'
    = Consequences (d || d') (a `HashSet.union` a') (m `HashSet.union` m')

instance Monoid Consequences where
  mempty = Consequences False mempty mempty

getConfig :: IO Config
getConfig = do
  tokenFile <- getEnv "DISCORD_BETTER_SLOWMODE_TOKEN" "token"
  token <- Text.readFile tokenFile `catch`
    \(e :: IOException) -> die ("Error reading token from file: " <> show e)

  dbFile <- getEnv "DISCORD_BETTER_SLOWMODE_DATABASE" "better-slowmode.json"
  dbData <- (Json.eitherDecodeFileStrict' dbFile `catch`
    \(e :: IOException) -> die ("Error reading database from file: " <> show e))
    >>= either (die . ("Error decoding database: " <>)) pure

  Config token dbFile <$> atomically (THashMap.thaw dbData)

handler :: Config -> Event -> DiscordHandler ()
handler cfg@(Config _ _ db) = \case
  -- We got a message. Let's analyze it.
  MessageCreate msg

    -- Message starts with !dbs_config => print the configuration
    -- (dumped as JSON)
    -- this is of course temporary!
    | "!dbs_config" `Text.isPrefixOf` messageText msg
    , let channel = messageChannel msg
    , let author = messageAuthor msg
    , not (userIsBot author || userIsWebhook author) -- no bots/webhooks allowed!
    -> do
      cfgText <- atomically $ THashMap.lookup channel db
        <&> \case
          Nothing -> "No configuration for this channel. You can create one with `!dbs_setconfig`."
          Just cfg -> "Current configuration:\n```json\n" <> Text.decodeUtf8 (LBS.toStrict (Json.encode cfg)) <> "\n```"
      () <$ restCall (R.CreateMessage channel cfgText)
    
    -- Message starts with !dbs_setconfig => set configuration to given value
    -- configuration taken as JSON
    -- only mods (as defined by the config.) may do this.
    | Just rest <- Text.stripPrefix "!dbs_setconfig" (messageText msg)
    , let channel = messageChannel msg
    , let author = messageAuthor msg
    , let authorId = userId author
    , not (userIsBot author || userIsWebhook author) -- no bots/webhooks allowed!
    , Just guild <- messageGuild msg
    -> do
      -- Is the message author a mod?
      GuildMember{ memberRoles = roles } <- restCall (R.GetGuildMember guild authorId)
          >>= either (fail . show) pure
      mods <- maybe mempty cdModerators <$> atomically (THashMap.lookup channel db)
      let isMod = authorId `HashSet.member` fst mods
            || any (`HashSet.member` snd mods) roles
      
      -- Decode JSON from rest of message
      when isMod case Json.eitherDecodeStrict' (Text.encodeUtf8 (Text.strip rest)) of
        -- Fail: report error, abort.
        Left err -> () <$ restCall (R.CreateMessage channel ("Error decoding config: " <> Text.pack err))
        -- Succeed: save new config and announce that.
        Right newCfg -> do
          atomically $ THashMap.insert channel newCfg db
          saveConfig cfg
          () <$ restCall (R.CreateMessage channel "Saved new config!")

    -- Some other message. We police those!
    | let channel = messageChannel msg
    , let author = messageAuthor msg
    , let authorId = userId author
    , let time = messageTimestamp msg
    , not (userIsBot author || userIsWebhook author) -- no bots/webhooks allowed!
    , Just guild <- messageGuild msg -- this fails on DMs, which is good because we don't/can't police those
    ->
      -- Look up the rules for this channel. If we don't have any stored, the entire following section is skipped.
      atomically (THashMap.lookup channel db) >>= traverse_ \cd@(ChannelData mods messageTimes rules) -> do

        -- Check whether the author is a mod.
        GuildMember{ memberRoles = roles } <- restCall (R.GetGuildMember guild authorId)
          >>= either (fail . show) pure
        let isMod = authorId `HashSet.member` fst mods
              || any (`HashSet.member` snd mods) roles
            lastMessageTime = HashMap.lookup authorId messageTimes
        
        -- Mods' messages aren't policed.
        when (not isMod) do
          -- Go through all stored rules and combine the punishments of the rules that were violated
          let response = flip foldMap rules \rule ->
                let earliestAllowedTime = addUTCTime (ruleMinmumDelay rule) <$> lastMessageTime
                in if ruleShouldAffect rule authorId roles && Just time < earliestAllowedTime
                  then ruleConsequences rule
                  else mempty
          
          -- If a rule says to delete this message, do so.
          if csqDeleteMessage response
            then () <$ restCall (R.DeleteMessage (channel, messageId msg))
            else do
              -- If we DIDN'T delete the message, it gets stored as the new "most recent message" time.
              atomically (THashMap.insert channel cd{ cdLastMessageTimes = HashMap.insert authorId time messageTimes } db)
              saveConfig cfg
          
          -- If a rule says to assign one or more rules, do so.
          when (not (null (csqAssignRoles response))) do
            -- Compute the user/victim's new roles: old roles (already retrieved for the isMod check above)
            -- plus newly-assigned roles
            let newRoles = HashSet.toList (csqAssignRoles response `HashSet.union` HashSet.fromList roles)
                opts = R.ModifyGuildMemberOpts Nothing (Just newRoles) Nothing Nothing Nothing
            () <$ restCall (R.ModifyGuildMember guild authorId opts)
          
          -- Construct a message which will be sent to all channels that the rules mention.
          let -- Describe the user. Format: User `Tim#6969 <@id-here>`
              userFrag = "User `" <> userName author <> "#" <> userDiscrim author <> " <@" <> tshow (userId author) <> ">`"

              -- Describe the channel that the violation occured in. Format: <#id-here> (creates a channel link).
              channelFrag = "channel <#" <> tshow channel <> ">"

              -- Link to the message, if we didn't delete it.
              messageFrag = if csqDeleteMessage response
                then ""
                else ": <https://discord.com/channels/" <> tshow guild <> "/" <> tshow channel <> "/" <> tshow (messageId msg) <> ">"

              -- Put together all the fragments into a coherent sentence.
              explanation = userFrag <> " posted a forbidden message (too soon after last message) in " <> channelFrag
          
          -- Send this message to each listed channel.
          for_ (csqSendMessages response) \dest -> restCall (R.CreateMessage dest explanation)
          

  _ -> pure ()

-- Save the current internal state/configuration to the file specified on startup
saveConfig :: MonadIO m => Config -> m ()
saveConfig (Config _ fp db) = liftIO $ Json.encodeFile fp =<< atomically (THashMap.freeze db)
  `catch` \(e :: IOException) -> die ("Error while saving database: " <> show e)

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- Get the value of an environment variable, or return a default value
-- if the env. var. is empty/absent.
getEnv :: String -> String -> IO String
getEnv k d = fixup <$> System.lookupEnv k
  where
    fixup (Just s) | s /= "" = s
    fixup _ = d