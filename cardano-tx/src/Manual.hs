{-# LANGUAGE RecordWildCards #-}

module Manual where

import qualified Cardano.Api                                       as C
import           Cardano.CLI.Shelley.Key                           as K
import           Cardano.Sdk.Adapter.Koios.UTxO                    as K
import           Cardano.Sdk.Adapter.Node.Connection
import           Cardano.Sdk.Adapter.Node.Network                  as N
import           Cardano.Sdk.Adapter.Node.UTxO                     as N
import           Cardano.Sdk.Address
import           Cardano.Sdk.Transaction
import           Cardano.Sdk.Transaction.Data
import           Cardano.Sdk.UTxO
import qualified Data.ByteString                                   as BS
import qualified Data.Set                                          as S
import qualified Ledger                                            as L
import qualified Ledger.Ada                                        as Ada
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import           Plutus.V1.Ledger.Api                              as A
import           Plutus.V1.Ledger.Interval
import           RIO

network = C.Testnet $ C.NetworkMagic 1097911063
koiosConfig = KoiosConfig "https://testnet.koios.rest" network
nodeConn = connInfo network "/home/ssledz/git/simple-swap-playground/testnet/node.socket"


main :: IO ()
main = do
  let networkId = C.Testnet $ C.NetworkMagic 1097911063
  let conn = connInfo networkId "/home/ssledz/git/simple-swap-playground/testnet/node.socket"
  let addr = "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  addr' <- maybe (throwString "address parsing error") return (readShellyAddress addr)
  utxos1 <- K.queryUTxo koiosConfig [addr']
  utxos2' <- N.queryUTxo conn (maybeToList (readAnyAddress addr))
  let utxos2 = toLedgerUTxO utxos2'
  value1 <- K.queryValue koiosConfig [addr']
  let value2 = toLedgerValue utxos2'
  print $ value1 == value2
  print $ utxos1 == utxos2

liftEither :: (Show err, MonadIO m) => Either err a -> m a
liftEither (Left err) = throwString $ show err
liftEither (Right a)  = return a

payToWallet :: IO ()
payToWallet = do
  networkParams <- N.queryNetworkParams nodeConn network
  let liftMaybe msg ma = maybe (throwString msg) return ma
  let addrOrError addr = liftMaybe "address parsing error" (readShellyAddress addr)
  userAddr <- addrOrError "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  userDestAddr <- addrOrError "addr_test1qpeghkawyqww4xawdmp960qdd42dw4t4nrl94cmghhd5ecktqrffap5mpleqjnqllukhhy4plc93pshej87hqqfqwz0spsrd8a"
  utxo <- K.queryUTxo koiosConfig [userAddr]
  let userTxIn = parseTxIn "053f6b052ae5da8fbf217e10834f77c6c34148136bbe889a79125f5aba13746b" 0
  let userTxRef = L.txInRef userTxIn
  userTxOut <- liftMaybe "Can't find txOut" $ findTxOutByTxIn utxo userTxIn
  --let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0

  let userDatum = Nothing
  let userTxOutput = TxOutput userTxRef userTxOut userDatum
  let txBuilderInputs = [TxInCandidate userTxOutput L.ConsumePublicKeyAddress]
  let txBuilderOutputs = [TxOutputCandidate (L.TxOut userDestAddr (Ada.lovelaceValueOf 2000001) Nothing) Nothing]
  let txBuilderChangeAddr = ChangeAddress userAddr
  let txBulderCollateral = Nothing
  let txBuilderValidRange = always
  let txBuilderSigners = []
  let txBuilder = TxBuilder {..}
  (C.BalancedTxBody txb _ _) <- liftEither $ buildBalancedTx networkParams txBuilderChangeAddr S.empty txBuilder
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  print userTxOut
  submitTx nodeConn tx
  print "Done."

payToScript :: IO ()
payToScript = do
  networkParams <- N.queryNetworkParams nodeConn network
  let liftMaybe msg ma = maybe (throwString msg) return ma
  let addrOrError addr = liftMaybe "address parsing error" (readShellyAddress addr)
  userAddr <- addrOrError "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  scriptAddr <- addrOrError "addr_test1wz8npwnc5wcsml6uzu8a6u4qcqxaxfevsx4ep64wm6jxfhcnpl4zh"
  utxo <- K.queryUTxo koiosConfig [userAddr]
  let userTxIn = parseTxIn "56d7effe127cdefc551d33ca60271eaf84c587d3dcdffc0deed596003f2d62bb" 0
  let userTxRef = L.txInRef userTxIn
  userTxOut <- liftMaybe "Can't find txOut" $ findTxOutByTxIn utxo userTxIn
  --let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0

  let scriptDatum = (A.fromBuiltinData . A.toBuiltinData) (11250625991234 :: Integer) :: Maybe Datum
  let scriptDatumHash = L.datumHash <$> scriptDatum
  print $ "datum Hash: " <> show scriptDatumHash
  let userDatum = Nothing
  let userTxOutput = TxOutput userTxRef userTxOut userDatum
  let txBuilderInputs = [TxInCandidate userTxOutput L.ConsumePublicKeyAddress]
  let txBuilderOutputs = [TxOutputCandidate (L.TxOut scriptAddr (Ada.lovelaceValueOf 2000002) scriptDatumHash) scriptDatum]
  let txBuilderChangeAddr = ChangeAddress userAddr
  let txBulderCollateral = Nothing
  let txBuilderValidRange = always
  let txBuilderSigners = []
  let txBuilder = TxBuilder {..}
  (C.BalancedTxBody txb _ _) <- liftEither $ buildBalancedTx networkParams txBuilderChangeAddr S.empty txBuilder
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  print userTxOut
  submitTx nodeConn tx
  print "Done."

signingKey' :: String -> IO C.ShelleyWitnessSigningKey
signingKey' keyPath = C.WitnessPaymentKey <$> signingKey keyPath

signingExtKey' :: String -> IO C.ShelleyWitnessSigningKey
signingExtKey' keyPath = C.WitnessPaymentExtendedKey <$> signingExtKey keyPath

signingKey :: String -> IO (C.SigningKey C.PaymentKey)
signingKey keyPath = do
    liftIO $ BS.readFile keyPath >>= skeyFrom
  where
    skeyFrom content = case K.deserialiseInputAnyOf bech32Types textEnvTypes content of
        Right key -> return key
        Left err  -> throwString $ show err
      where
        bech32Types = [ C.FromSomeType (C.AsSigningKey C.AsPaymentKey) id
                      ]
        textEnvTypes = [ C.FromSomeType (C.AsSigningKey C.AsPaymentKey) id
                       ]

signingExtKey :: String -> IO (C.SigningKey C.PaymentExtendedKey)
signingExtKey keyPath = do
    liftIO $ BS.readFile keyPath >>= skeyFrom
  where
    skeyFrom content = case K.deserialiseInputAnyOf bech32Types textEnvTypes content of
        Right key -> return key
        Left err  -> throwString $ show err
      where
        bech32Types = [ C.FromSomeType (C.AsSigningKey C.AsPaymentExtendedKey) id
                      ]
        textEnvTypes = [ C.FromSomeType (C.AsSigningKey C.AsPaymentExtendedKey) id
                       ]

submitTx :: NodeConn -> C.Tx C.AlonzoEra -> IO ()
submitTx conn tx = do
  res <- C.submitTxToNodeLocal conn $ C.TxInMode tx C.AlonzoEraInCardanoMode
  case res of
    SubmitSuccess     -> return ()
    SubmitFail reason -> throwString $ show reason
