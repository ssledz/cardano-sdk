{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Ledger.Constraints                                as LC
import qualified Ledger.Constraints.TxConstraints                  as LCT
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import           Plutus.V1.Ledger.Api                              as A
import           Plutus.V1.Ledger.Interval
import           RIO

import           Cardano.Sdk.Network                               (NetworkParameters)
import qualified Cardano.Sdk.Script                                as Sdk
import qualified Cardano.Sdk.Transaction.BalanceTx                 as BTx
import qualified Cardano.Sdk.Transaction.Data                      as Sdk
import qualified Data.Map                                          as M
import qualified Data.Text                                         as T
import qualified Ledger.Tx.CardanoAPI                              as Conv

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
  let userTxIn = parseTxIn "3507ed06c59c4373d31b1af4e59803475228d14c8566a804085a7552d7cfe81d" 0
  let userTxRef = L.txInRef userTxIn
  userTxOut <- liftMaybe "Can't find txOut" $ findTxOutByTxIn utxo userTxIn
  --let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0

  let scriptDatum = (A.fromBuiltinData . A.toBuiltinData) (911250625991234 :: Integer) :: Maybe Datum
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


liftMaybe msg = maybe (throwString msg) return

addrOrError addr = liftMaybe "address parsing error" (readShellyAddress addr)

payToScript2 :: IO ()
payToScript2 = do
  networkParams <- N.queryNetworkParams nodeConn network
  userAddr <- addrOrError "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  --scriptAddr <- addrOrError "addr_test1wz8npwnc5wcsml6uzu8a6u4qcqxaxfevsx4ep64wm6jxfhcnpl4zh"
  scriptAddr <- addrOrError "addr_test1wp3tms7sf5zrwm23d5ckvj2ykfww8tl6wmghlz67zfumf8gs6jh83"
  utxo <- K.queryUTxo koiosConfig [userAddr]
  --let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0

  let scriptDatum = (A.fromBuiltinData . A.toBuiltinData) (911250625991234 :: Integer) :: Maybe Datum
  --let scriptDatum = Nothing :: Maybe Datum
  let scriptDatumHash = L.datumHash <$> scriptDatum
  --print $ "datum Hash: " <> show scriptDatumHash
  let txBalancingInputs = txInCandidatesFromUTxO utxo
  let txBuilderOutputs = [TxOutputCandidate (L.TxOut scriptAddr (Ada.lovelaceValueOf 4001114) scriptDatumHash) scriptDatum]
  let txBalancingChangeAddr = ChangeAddress userAddr
  let txBalancingCollateral = S.empty
  let txBuilderInputs = []
  let txBuilderValidRange = always
  let txBuilderSigners = []
  let txBuilder = TxBuilder {..}
  let txBalancing = Sdk.TxBalancing {..}
  (C.BalancedTxBody txb _ _) <- liftEither $ BTx.buildBalancedTx networkParams txBuilder txBalancing
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  --print userTxOut
  submitTx nodeConn tx
  print "Done."


data TestCase = TestCase
  { userAddr      :: L.Address
  , scriptAddr    :: L.Address
  , script        :: L.Script
  , rawDatum      :: Integer
  , datum         :: L.Datum
  , redeemer      :: L.Redeemer
  , validator     :: L.Validator
  , validatorHash :: L.ValidatorHash
  , networkParams :: NetworkParameters
  , collaterals   :: S.Set Sdk.TxInCollateral
  }

mkTestCase :: IO TestCase
mkTestCase = do
  networkParams <- N.queryNetworkParams nodeConn network
  userAddr <- addrOrError "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  scriptAny <- Sdk.readFileScriptInAnyLang "/home/ssledz/git/simple-swap-playground/scripts/simple-always-ok.script"
  scriptInEra <- liftMaybe "error during to script in era" $ C.toScriptInEra C.AlonzoEra scriptAny
  script <- liftMaybe "" $ Conv.fromCardanoScriptInEra scriptInEra
  scriptAddr <- addrOrError "addr_test1wp3tms7sf5zrwm23d5ckvj2ykfww8tl6wmghlz67zfumf8gs6jh83"
  let rawDatum = 911250625991234
  datum <- liftMaybe "Can't make datum" $ (A.fromBuiltinData . A.toBuiltinData) rawDatum
  redeemer <- liftMaybe "Can't make redeemer" $ (A.fromBuiltinData . A.toBuiltinData) (0 :: Integer)
  let validator = Validator script
  let validatorHash = L.validatorHash validator
  let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0
  let collaterals = S.singleton collateral

  return TestCase {..}

toPaymentPubKeyHash :: L.Address -> Maybe L.PaymentPubKeyHash
toPaymentPubKeyHash addr = L.PaymentPubKeyHash <$> L.toPubKeyHash addr

payToScriptUsingConstraints :: IO ()
payToScriptUsingConstraints = do
  TestCase{..} <- mkTestCase
  userUtxo <- K.queryUTxo koiosConfig [userAddr]
  userPubKeyHash <- liftMaybe "can't make pub key hash" $ toPaymentPubKeyHash userAddr
  let constraints :: LC.TxConstraints Void Void
      constraints  =
        LCT.mustPayToOtherScript validatorHash datum (Ada.lovelaceValueOf 2171107)
          <> LC.mustIncludeDatum datum
     --     <> LC.mustPayToPubKey userPubKeyHash (Ada.lovelaceValueOf 100000)
  let lookups = LC.otherScript validator
  unBalancedTx <- liftEither $ LC.mkTx @Void lookups constraints
  print unBalancedTx
  let userUtxo' =  M.mapKeys (\(L.TxIn ref _) -> ref) $ unUTxO userUtxo
  (C.BalancedTxBody txb _ _) <- liftEither $ BTx.buildBalancedTx2 networkParams userUtxo' (ChangeAddress userAddr) unBalancedTx S.empty
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  submitTx nodeConn tx
  print "Done."

parseTxOutRef :: T.Text -> Integer -> TxOutRef
parseTxOutRef txId idx = L.txInRef $ parseTxIn txId idx

toChainIndexUTxO :: UTxO -> M.Map L.TxOutRef L.ChainIndexTxOut
toChainIndexUTxO (UTxO utxo) = M.fromList $mapMaybe translate (M.toList utxo)
  where
    --translate (L.TxIn txRef _, txOut) =  (\o -> (txRef, o)) <$> L.fromTxOut txOut
    translate (L.TxIn txRef _, txOut) =  (txRef, ) <$> L.fromTxOut txOut


adjustDatum :: L.Datum  -> M.Map L.TxOutRef L.ChainIndexTxOut -> M.Map L.TxOutRef L.ChainIndexTxOut
adjustDatum datum xs =
    let dh = L.datumHash datum
        --ys = (\(k, v) -> (k, f dh v)) <$> M.toList xs
        ys = second (f dh) <$> M.toList xs
    in M.fromList ys
  where
    f :: L.DatumHash -> L.ChainIndexTxOut -> L.ChainIndexTxOut
    f dh o@(L.ScriptChainIndexTxOut _ _ (Left dh2) _) =
      if dh == dh2 then o {L._ciTxOutDatum = Right datum} else o
    f dh o = o

spendFromScriptUsingConstraints :: IO ()
spendFromScriptUsingConstraints = do
  TestCase{..} <- mkTestCase
  userUtxo <- K.queryUTxo koiosConfig [userAddr]
  scriptUTxO <- K.queryUTxo koiosConfig [scriptAddr]
  userPubKeyHash <- liftMaybe "can't make pub key hash" $ toPaymentPubKeyHash userAddr

  let scriptCIUTxO = adjustDatum datum $ toChainIndexUTxO scriptUTxO
  let txOutRef = parseTxOutRef "a4104aab8cceb09fd701095da7d3a1900a4e268f056d2ed02a96f54f10edc602" 1

  let restricredCIUTxO = M.restrictKeys scriptCIUTxO $ S.singleton txOutRef

  print restricredCIUTxO

  let constraints :: LC.TxConstraints Void Void
      constraints = LC.mustSpendScriptOutput txOutRef redeemer
                 <> LC.mustPayToPubKey userPubKeyHash (Ada.lovelaceValueOf 1171107)

  let lookups :: LC.ScriptLookups Void
      lookups = LC.otherScript validator
              <> LC.unspentOutputs restricredCIUTxO

  let userUtxo' =  M.mapKeys (\(L.TxIn ref _) -> ref) $ unUTxO userUtxo
  unBalancedTx <- liftEither $ LC.mkTx @Void lookups constraints
  print unBalancedTx
  (C.BalancedTxBody txb _ _) <- liftEither $ BTx.buildBalancedTx2 networkParams userUtxo' (ChangeAddress userAddr) unBalancedTx collaterals
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  submitTx nodeConn tx
  print "Done."


spendFromScript :: IO ()
spendFromScript = do
  networkParams <- N.queryNetworkParams nodeConn network

  userAddr <- addrOrError "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  --scriptAddr <- addrOrError "addr_test1wz8npwnc5wcsml6uzu8a6u4qcqxaxfevsx4ep64wm6jxfhcnpl4zh"
  scriptAddr <- addrOrError "addr_test1wp3tms7sf5zrwm23d5ckvj2ykfww8tl6wmghlz67zfumf8gs6jh83"
  scriptUtxo <- K.queryUTxo koiosConfig [scriptAddr]
  forM_ (M.toList $ unUTxO scriptUtxo) (\a -> print "-------\n" >> print a)

  --script <- Sdk.readFileScriptInAnyLang "/home/ssledz/git/simple-swap-playground/scripts/beneficiary/1/beneficiary.script"
  script <- Sdk.readFileScriptInAnyLang "/home/ssledz/git/simple-swap-playground/scripts/simple-always-ok.script"

  scriptInEra <- liftMaybe "error during to script in era" $ C.toScriptInEra C.AlonzoEra script

  plutusScript <- liftMaybe "" $ Conv.fromCardanoScriptInEra scriptInEra

  let scriptTxIn = parseTxIn "3b9b1249f999ea3df99b6fa0ab415cb6cc604c2a331e59fe18db6e70e9833b12" 1
  --let scriptTxIn = parseTxIn "5e776338474f48d365fa6affe7f10ec667d92f425957e649625f24900531fdb5" 1
  let scriptTxRef = L.txInRef scriptTxIn
  scriptTxOut <- liftMaybe "Can't find txOut" $ findTxOutByTxIn scriptUtxo scriptTxIn

  let validator = Validator plutusScript

  datum <- liftMaybe "Can't make datum" $ (A.fromBuiltinData . A.toBuiltinData) (911250625991234 :: Integer)
  redeemer <- liftMaybe "Can't make redeemer" $ (A.fromBuiltinData . A.toBuiltinData) (0 :: Integer)

  let scriptTxOutput = TxOutput scriptTxRef scriptTxOut Nothing
  let scriptTxInTyp = L.ConsumeScriptAddress validator redeemer datum

  let collateral = collateralFromTxIn $ parseTxIn "17aac1962138c904f82db6e0c2b5c06ebb972a5cfa2ee2e81daff5ffd7c23d88" 0
  let txBalancingInputs = []
  let txBalancingChangeAddr = ChangeAddress userAddr
  let txBalancingCollateral = S.singleton collateral
  let txBalancing = Sdk.TxBalancing {..}

  let txBuilderOutputs = []
  let txBuilderInputs = [TxInCandidate scriptTxOutput scriptTxInTyp]
  let txBuilderValidRange = always
  let txBuilderSigners = []
  let txBuilder = TxBuilder {..}

  (C.BalancedTxBody txb _ _) <- liftEither $ BTx.buildBalancedTx networkParams txBuilder txBalancing
  print "Loading key..."
  sKey <- signingExtKey' "/home/ssledz/git/simple-swap-playground/wallets/root/payment.skey"
  print "Signing tx..."
  let tx = signTx txb [sKey]
  --print userTxOut
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
