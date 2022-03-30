module Cardano.Sdk.Adapter.Node.Shared
  ( module X
  , queryChainPoint
  , queryInShelly
  ) where

import           Cardano.Api
import           Cardano.Sdk.Adapter.Node.Connection
import qualified Cardano.Sdk.Adapter.Node.Connection as X
import           RIO

queryChainPoint :: MonadIO m => NodeConn -> m ChainPoint
queryChainPoint conn = liftIO $ chainTipToChainPoint <$> getLocalChainTip conn

queryInShelly :: MonadIO m => NodeConn -> QueryInShelleyBasedEra AlonzoEra a -> m a
queryInShelly conn query = do
  cp <- queryChainPoint conn
  let q = QueryInEra AlonzoEraInCardanoMode $
                QueryInShelleyBasedEra ShelleyBasedEraAlonzo query
  resOrErr <- liftIO $ queryNodeLocalState conn (Just cp) q
  case resOrErr of
    Right (Right pools) -> return pools
    Left err            -> throwString $ show err
    Right (Left err)    -> throwString $ show err
