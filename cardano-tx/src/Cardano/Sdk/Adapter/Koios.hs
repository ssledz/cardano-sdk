{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.Sdk.Adapter.Koios where

import qualified Cardano.Api                  as C
import           Cardano.Sdk.Transaction.Data
import           Data.Aeson
import           Data.Either.Extra
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           GHC.Generics                 (Generic)
import           Network.HTTP.Simple
import qualified Plutus.V1.Ledger.Ada         as L
import qualified Plutus.V1.Ledger.Api         as L
import qualified Plutus.V1.Ledger.Tx          as L
import qualified Plutus.V2.Ledger.Api         as L

import qualified Data.ByteString.Base16       as BS16 (decode)
import qualified Data.ByteString.Char8        as BSC
import           Data.String
import           PlutusTx.Foldable

newtype KoiosConfig = KoiosConfig { url :: T.Text }

data AddressUtxo = AddressUtxo { tx_hash    :: T.Text
                               , tx_index   :: Integer
                               , value      :: T.Text
                               , datum_hash :: Maybe T.Text
                               , asset_list :: [AddressAsset]
                               } deriving (Generic, Show, FromJSON)

data AddressInfo = AddressInfo { balance        :: T.Text
                               , script_address :: Bool
                               , utxo_set       :: [AddressUtxo]
                               } deriving (Generic, Show, FromJSON)

data AddressAsset = AddressAsset { policy_id  :: T.Text
                                 , asset_name :: T.Text
                                 , quantity   :: String
                                 } deriving (Generic, Show, FromJSON)

instance ToLedgerTxId AddressUtxo where
  toLedgerTxId AddressUtxo{..} = L.TxId hash
    where
      hash = fromString $ T.unpack tx_hash

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
      adaValue = L.singleton L.adaSymbol L.adaToken $ read (T.unpack value)
      assetValue = toLedgerValue asset_list

instance ToLedgerValue AddressAsset where
  toLedgerValue AddressAsset{..} =
    L.Value $ L.fromList [(currency, L.fromList [(token, read quantity)])]
      where
        currency = fromString $ T.unpack policy_id
        unhex s = BSC.unpack <$> BS16.decode (BSC.pack s)
        token = let str = T.unpack asset_name
                in fromString $ fromRight str (unhex str)

queryAddressInfo :: KoiosConfig -> T.Text -> IO [AddressInfo]
queryAddressInfo KoiosConfig {..} addr = do
  request' <- parseRequest $ T.unpack ("GET " <> url)
  let request = setRequestHeaders [("Accept","application/json")]
              $ setRequestPath "/api/v0/address_info"
              $ setRequestQueryString [("_address", Just $ TE.encodeUtf8 addr)]
                request'
  getResponseBody <$> httpJSON request

getAda :: AddressInfo -> L.Ada
getAda = L.fromValue . toLedgerValue

