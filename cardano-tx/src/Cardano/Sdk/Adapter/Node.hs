{-# LANGUAGE FlexibleInstances #-}

module Cardano.Sdk.Adapter.Node where

import           Cardano.Api
import           Cardano.Sdk.Transaction.Data
import qualified Data.Map                     as M
import qualified Data.Set                     as Set
import qualified Ledger.Tx.CardanoAPI         as Conv
import qualified PlutusTx.Foldable            as P
import           RIO


type NodeConn = LocalNodeConnectInfo CardanoMode

instance ToLedgerValue (UTxO AlonzoEra) where
  toLedgerValue (UTxO xs) = P.fold $ toLedgerValue <$> M.elems xs

instance ToLedgerValue (TxOut ctx era) where
  toLedgerValue (TxOut _ value _) = toLedgerValue value

instance ToLedgerValue (TxOutValue era) where
  toLedgerValue = Conv.fromCardanoValue . txOutValueToValue

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
