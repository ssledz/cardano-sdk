{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.Sdk.Adapter.Koios.UTxO
  ( KoiosConfig (..)
  , queryUTxo
  , queryValue
  )where

import qualified Cardano.Api            as C
import           Cardano.Sdk.Address
import           Cardano.Sdk.UTxO
import           Data.Aeson
import qualified Data.ByteString.Base16 as BS16 (decode)
import qualified Data.ByteString.Char8  as BSC
import           Data.Either.Extra
import qualified Data.Map               as M
import           Data.String
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Ledger                 as L
import           Network.HTTP.Simple
import qualified Plutus.V1.Ledger.Ada   as L
import qualified Plutus.V1.Ledger.Value as LV
import qualified PlutusTx.AssocMap      as P
import           RIO

data KoiosConfig = KoiosConfig
  { kcUrl       :: T.Text
  , kcNetworkId :: C.NetworkId
  }

data WithAddress a = WithAddress
  { address :: L.Address
  , entity  :: a
  } deriving (Show, Eq)

data AddressUtxo = AddressUtxo
  { tx_hash    :: T.Text
  , tx_index   :: Integer
  , value      :: T.Text
  , datum_hash :: Maybe T.Text
  , asset_list :: [AddressAsset]
  } deriving (Generic, Show, FromJSON)

data AddressInfo = AddressInfo
  { balance        :: T.Text
  , script_address :: Bool
  , utxo_set       :: [AddressUtxo]
  } deriving (Generic, Show, FromJSON)

data AddressAsset = AddressAsset
  { policy_id  :: T.Text
  , asset_name :: T.Text
  , quantity   :: String
  } deriving (Generic, Show, FromJSON)

instance ToLedgerTxId AddressUtxo where
  toLedgerTxId AddressUtxo{..} = fromString $ T.unpack tx_hash

instance ToLedgerTxIn AddressUtxo where
  toLedgerTxIn utxo@AddressUtxo{..} = L.TxIn txInRef txInType
    where
      txInRef = L.TxOutRef (toLedgerTxId utxo) tx_index
      txInType = Nothing

instance ToLedgerValue AddressInfo where
  toLedgerValue AddressInfo{..} = toLedgerValue utxo_set

instance ToLedgerValue AddressUtxo where
  toLedgerValue AddressUtxo{..} = adaValue <> assetValue
    where
      adaValue = LV.singleton L.adaSymbol L.adaToken $ read (T.unpack value)
      assetValue = toLedgerValue asset_list

instance ToLedgerValue AddressAsset where
  toLedgerValue AddressAsset{..} =
    LV.Value $ P.fromList [(currency, P.fromList [(token, read quantity)])]
      where
        currency = fromString $ T.unpack policy_id
        unhex s = BSC.unpack <$> BS16.decode (BSC.pack s)
        token = let str = T.unpack asset_name
                in fromString $ fromRight str (unhex str)

instance ToLedgerValue a => ToLedgerValue (WithAddress a) where
  toLedgerValue (WithAddress _ a) = toLedgerValue a

instance ToLedgerTxOut (WithAddress AddressUtxo) where
  toLedgerTxOut (WithAddress addr utxo@AddressUtxo{..}) = L.TxOut addr value' datumHash
    where
      value' = toLedgerValue utxo
      datumHash = fromString . T.unpack <$> datum_hash

instance ToLedgerUTxO (WithAddress AddressInfo) where
  toLedgerUTxO (WithAddress addr AddressInfo{..}) = UTxO $ M.fromList $ f <$> utxo_set
    where
      f utxo = (toLedgerTxIn utxo, toLedgerTxOut (WithAddress addr utxo))


queryAddressInfo :: KoiosConfig -> L.Address -> IO [WithAddress AddressInfo]
queryAddressInfo KoiosConfig {..} addr = do
  addr' <- maybe (throwString $ "Error rendering address: " <> show addr) return $ renderShellyAddress kcNetworkId addr
  request' <- parseRequest $ T.unpack ("GET " <> kcUrl)
  let request = setRequestHeaders [("Accept","application/json")]
              $ setRequestPath "/api/v0/address_info"
              $ setRequestQueryString [("_address", Just $ TE.encodeUtf8 addr')]
                request'
  info <- getResponseBody <$> httpJSON request
  return $ WithAddress addr <$> info

queryAddressInfos :: KoiosConfig -> [L.Address] -> IO [WithAddress AddressInfo]
queryAddressInfos cfg addrs = mconcat <$> mapM (queryAddressInfo cfg) addrs

queryUTxo :: MonadIO m => KoiosConfig -> [L.Address] -> m UTxO
queryUTxo cfg addrs = liftIO $ toLedgerUTxO <$> queryAddressInfos cfg addrs

queryValue :: MonadIO m => KoiosConfig -> [L.Address] -> m L.Value
queryValue cfg addrs = liftIO $ toLedgerValue <$> queryAddressInfos cfg addrs
