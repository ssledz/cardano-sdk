module Cardano.Sdk.Transaction.Adapter.Node where

import           Cardano.Api
import qualified Data.Set    as Set
import           RIO

type NodeConn = LocalNodeConnectInfo CardanoMode

queryChainPoint :: MonadIO m => LocalNodeConnectInfo mode -> m ChainPoint
queryChainPoint conn = liftIO $ chainTipToChainPoint <$> getLocalChainTip conn

queryUtxo :: MonadIO m => NodeConn -> [AddressAny] -> m (UTxO AlonzoEra)
queryUtxo conn addrs = do
    cp <- queryChainPoint conn
    utxosOrErr <- liftIO $ queryNodeLocalState conn (Just cp) query
    case utxosOrErr of
        Right (Right utxos) -> return utxos
        err                 -> throwString $ show err
  where
    query = QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO queryByAddress)
    queryByAddress :: QueryUTxOFilter
    queryByAddress = QueryUTxOByAddress $ Set.fromList addrs

connInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
connInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))
