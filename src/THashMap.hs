{-# LANGUAGE BlockArguments #-}
module THashMap where

import Data.Foldable
import Data.Monoid
import Data.Traversable

import Control.Concurrent.STM
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

newtype THashMap k v = THashMap (TVar (HashMap k (TVar v)))

lookup :: (Eq k, Hashable k) => k -> THashMap k v -> STM (Maybe v)
lookup k (THashMap hm) = traverse readTVar =<< HashMap.lookup k <$> readTVar hm

member :: (Eq k, Hashable k) => k -> THashMap k v -> STM Bool
member k (THashMap hm) = HashMap.member k <$> readTVar hm

modify :: (Eq k, Hashable k) => k -> (v -> v) -> THashMap k v -> STM ()
modify k f (THashMap hm) = traverse_ (`modifyTVar'` f) =<< HashMap.lookup k <$> readTVar hm

insert :: (Eq k, Hashable k) => k -> v -> THashMap k v -> STM ()
insert k v (THashMap hm) = do
  m <- readTVar hm
  case HashMap.lookup k m of
    Just c -> writeTVar c v
    Nothing -> do
      c <- newTVar v
      modifyTVar' hm (HashMap.insert k c)

insertAll :: (Eq k, Hashable k, Foldable t) => t (k, v) -> THashMap k v -> STM ()
insertAll kvs (THashMap hm) = do
  m <- readTVar hm
  inserts <- getAp $ flip foldMap kvs $ \(k,v) -> Ap case HashMap.lookup k m of
    Just c -> Nothing <$ writeTVar c v
    Nothing -> do
      c <- newTVar v
      pure (Just (Dual (Endo (HashMap.insert k c))))
  for_ inserts \(Dual (Endo f)) -> modifyTVar' hm f

delete :: (Eq k, Hashable k) => k -> THashMap k v -> STM ()
delete k (THashMap hm) = modifyTVar' hm (HashMap.delete k)

freeze :: THashMap k v -> STM (HashMap k v)
freeze (THashMap hm) = traverse readTVar =<< readTVar hm

thaw :: HashMap k v -> STM (THashMap k v)
thaw hm = fmap THashMap . newTVar =<< traverse newTVar hm

newEmpty :: STM (THashMap k v)
newEmpty = THashMap <$> newTVar HashMap.empty
