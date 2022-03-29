module Cardano.Sdk.Adapter.Node.Shared
  ( module X
  , queryChainPoint
  ) where

import           Cardano.Api
import           Cardano.Sdk.Adapter.Node.Connection
import qualified Cardano.Sdk.Adapter.Node.Connection as X
import           RIO

queryChainPoint :: MonadIO m => NodeConn -> m ChainPoint
queryChainPoint conn = liftIO $ chainTipToChainPoint <$> getLocalChainTip conn
