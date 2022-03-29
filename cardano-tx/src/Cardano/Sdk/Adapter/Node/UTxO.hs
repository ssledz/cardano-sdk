{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Sdk.Adapter.Node.UTxO where

import           Cardano.Api                     hiding (UTxO, unUTxO)
import qualified Cardano.Api                     as C
import           Cardano.Sdk.Adapter.Node.Shared
import           Cardano.Sdk.UTxO
import           Data.Either.Extra
import qualified Data.Map                        as M
import qualified Data.Set                        as Set
import qualified Ledger                          as L
import qualified Ledger.Tx.CardanoAPI            as Conv
import qualified PlutusTx.Foldable               as P
import qualified PlutusTx.Prelude                as PlutusTx
import           RIO


instance ToLedgerValue (C.UTxO era) where
  toLedgerValue (C.UTxO xs) = P.fold $ toLedgerValue <$> M.elems xs

instance ToLedgerValue (C.TxOut ctx era) where
  toLedgerValue (TxOut _ value _) = toLedgerValue value

instance ToLedgerValue (TxOutValue era) where
  toLedgerValue = Conv.fromCardanoValue . txOutValueToValue

instance ToLedgerTxIn TxIn where
  toLedgerTxIn txIn = L.TxIn (Conv.fromCardanoTxIn txIn) Nothing

instance ToLedgerTxOut (C.TxOut CtxUTxO era) where
  toLedgerTxOut (C.TxOut addr value datumHash) = L.TxOut addr' value' datumHash'
    where
      -- TODO: make it safe
      addr' = fromRight (error $ "error during parsing address: " <> show addr) $ Conv.fromCardanoAddress addr
      value' = toLedgerValue value
      datumHash' = case datumHash of
                     TxOutDatumNone -> Nothing
                     TxOutDatumHash _ h -> Just $ L.DatumHash $ PlutusTx.toBuiltin (serialiseToRawBytes h)

instance ToLedgerUTxO (C.UTxO era) where
  toLedgerUTxO (C.UTxO out) = UTxO . M.fromList $ f <$> M.toList out
    where
      f (txIn, txOut) = (toLedgerTxIn txIn, toLedgerTxOut txOut)


queryUTxo :: MonadIO m => NodeConn -> [AddressAny] -> m (C.UTxO AlonzoEra)
queryUTxo conn addrs = do
    cp <- queryChainPoint conn
    utxosOrErr <- liftIO $ queryNodeLocalState conn (Just cp) query
    case utxosOrErr of
        Right (Right utxos) -> return utxos
        err                 -> throwString $ show err
  where
    query = QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO queryByAddress)
    queryByAddress :: QueryUTxOFilter
    queryByAddress = QueryUTxOByAddress $ Set.fromList addrs
