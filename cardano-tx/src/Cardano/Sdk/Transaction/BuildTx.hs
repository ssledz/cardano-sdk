{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction.BuildTx where

import           Cardano.Api
import           Cardano.Api.Shelley          (ProtocolParameters (..))
import qualified Cardano.Api.Shelley          as C
import qualified Cardano.Sdk.Network          as Sdk
import qualified Cardano.Sdk.Transaction.Data as Sdk
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Ledger                       as L
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Tx.CardanoAPI         as Conv
import qualified Plutus.V1.Ledger.Api         as Api
import           RIO


buildTxBodyContent
  :: ProtocolParameters
  -> NetworkId
  -> S.Set Sdk.TxInCollateral
  -> Sdk.TxBuilder
  -> Either Sdk.TransactionError (TxBodyContent BuildTx AlonzoEra)
buildTxBodyContent pparams network collateral Sdk.TxBuilder{..} = do
  txIns           <- buildTxIns txBuilderInputs
  txInsCollateral <- buildTxCollateral $ S.elems collateral
  txOuts          <- buildTxOuts network txBuilderOutputs
  txFee           <- adaptError $ Conv.toCardanoFee dummyFee
  txValidityRange <- adaptError $ Conv.toCardanoValidityRange txBuilderValidRange
  wits <- adaptError $ traverse Conv.toCardanoPaymentKeyHash txBuilderSigners
  pure $ TxBodyContent
    { txIns             = txIns
    , txInsCollateral   = txInsCollateral
    , txOuts            = txOuts
    , txFee             = txFee
    , txValidityRange   = txValidityRange
    , txMintValue       = TxMintNone
    , txProtocolParams  = BuildTxWith $ Just pparams
    , txExtraKeyWits    = TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra wits
    , txScriptValidity  = TxScriptValidityNone
    , txMetadata        = TxMetadataNone
    , txAuxScripts      = TxAuxScriptsNone
    , txWithdrawals     = TxWithdrawalsNone
    , txCertificates    = TxCertificatesNone
    , txUpdateProposal  = TxUpdateProposalNone
    }

buildTxOuts
  :: NetworkId
  -> [Sdk.TxOutputCandidate]
  -> Either Sdk.TransactionError [TxOut CtxTx AlonzoEra]
buildTxOuts network =
    mapM (adaptError . tr)
  where
    tr out@Sdk.TxOutputCandidate{..} =
      Conv.toCardanoTxOut network (lookupDatum $ Sdk.txOutputToDatumMap out) txOutputCandidateOut

buildTxCollateral
  :: [Sdk.TxInCollateral]
  -> Either Sdk.TransactionError (TxInsCollateral AlonzoEra)
buildTxCollateral collateral = do
  let txIns = flip L.TxIn Nothing . Sdk.txInCollateralRef <$> collateral
  adaptError $ Conv.toCardanoTxInsCollateral (S.fromList txIns)

buildTxIns
  :: [Sdk.TxInCandidate]
  -> Either Sdk.TransactionError [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
buildTxIns =
    mapM (adaptError . tr)
  where
    tr Sdk.TxInCandidate {txInCandidateTxOut=Sdk.TxOutput{..}, ..} = do
      wit <- Conv.toCardanoTxInWitness txInCandidateType
      txIn <- Conv.toCardanoTxIn txOutputRef
      pure (txIn, BuildTxWith wit)

buildInputsUTxO
  :: NetworkId
  -> [Sdk.TxInCandidate]
  -> Either Sdk.TransactionError (UTxO AlonzoEra)
buildInputsUTxO network inputs = do
    mapM (adaptError . tr) inputs <&> UTxO . M.fromList
  where
    tr :: Sdk.TxInCandidate ->  Either Conv.ToCardanoError (TxIn, TxOut CtxUTxO AlonzoEra)
    tr Sdk.TxInCandidate{txInCandidateTxOut=out@Sdk.TxOutput{..}} = do
      txIn  <- Conv.toCardanoTxIn txOutputRef
      let dm = Sdk.txOutputToDatumMap $ Sdk.txOutputToCandidate out
      txOut <- Conv.toCardanoTxOut network (lookupDatum dm) txOutputOut
      pure (txIn, toCtxUTxOTxOut txOut)

lookupDatum :: Map L.DatumHash L.Datum -> Maybe L.DatumHash -> Either Conv.ToCardanoError (TxOutDatum CtxTx AlonzoEra)
lookupDatum datums datumHash =
    case flip M.lookup datums =<< datumHash of
        Just datum -> pure $ TxOutDatum ScriptDataInAlonzoEra (toCardanoScriptData $ L.getDatum datum)
        Nothing    -> Conv.toCardanoTxOutDatumHash datumHash

toCardanoScriptData :: Api.BuiltinData -> ScriptData
toCardanoScriptData = C.fromPlutusData . Api.builtinDataToData

dummyFee :: L.Value
dummyFee = Ada.lovelaceValueOf 0

dummyChange :: L.Value
dummyChange = Ada.lovelaceValueOf 0

adaptError :: Either Conv.ToCardanoError b -> Either Sdk.TransactionError b
adaptError = mapLeft Sdk.TxCardanoError
