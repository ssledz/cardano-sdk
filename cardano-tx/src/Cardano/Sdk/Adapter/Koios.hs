{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.Sdk.Adapter.Koios where

import qualified Cardano.Api                  as C
import           Cardano.Sdk.Address
import           Cardano.Sdk.Transaction.Data
import           Data.Aeson
import qualified Data.ByteString.Base16       as BS16 (decode)
import qualified Data.ByteString.Char8        as BSC
import           Data.Either.Extra
import qualified Data.Map                     as M
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           GHC.Generics                 (Generic)
import qualified Ledger                       as L
import           Network.HTTP.Simple
import qualified Plutus.V1.Ledger.Ada         as L
import qualified Plutus.V1.Ledger.Value       as LV
import qualified PlutusTx.AssocMap            as P
import           PlutusTx.Foldable
import           RIO

newtype KoiosConfig = KoiosConfig { url :: T.Text }

data WithAddress a = WithAddress
  { address :: T.Text
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
  toLedgerTxOut (WithAddress addr utxo@AddressUtxo{..}) = L.TxOut addr' value datumHash
    where
      addr' = fromMaybe undefined $ readShellyAddress addr
      value = toLedgerValue utxo
      datumHash = fromString . T.unpack <$> datum_hash

instance ToLedgerUtxO (WithAddress AddressInfo) where
  toLedgerUtxO (WithAddress addr AddressInfo{..}) = UtxO $ M.fromList $ f <$> utxo_set
    where
      f utxo = (toLedgerTxIn utxo, toLedgerTxOut (WithAddress addr utxo))


queryAddressInfo' :: KoiosConfig -> T.Text -> IO [AddressInfo]
queryAddressInfo' KoiosConfig {..} addr = do
  request' <- parseRequest $ T.unpack ("GET " <> url)
  let request = setRequestHeaders [("Accept","application/json")]
              $ setRequestPath "/api/v0/address_info"
              $ setRequestQueryString [("_address", Just $ TE.encodeUtf8 addr)]
                request'
  getResponseBody <$> httpJSON request

queryAddressInfo :: KoiosConfig -> T.Text -> IO [WithAddress AddressInfo]
queryAddressInfo cfg addr = fmap conv <$> queryAddressInfo' cfg addr
  where
    conv a = WithAddress addr a

getAda :: AddressInfo -> L.Ada
getAda = L.fromValue . toLedgerValue

