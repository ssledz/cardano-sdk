{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction.BalanceTx where

import           Cardano.Api
import           Cardano.Api.Shelley             (ProtocolParameters (..))
import qualified Cardano.Sdk.Network             as Sdk
import           Cardano.Sdk.Transaction.BuildTx
import           Cardano.Sdk.Transaction.Data
import qualified Cardano.Sdk.Transaction.Data    as Sdk
import           Data.List
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Ledger                          as L
import qualified Ledger.Tx.CardanoAPI            as Conv
import           RIO

buildBalancedTx
  :: Sdk.NetworkParameters
  -> Sdk.TxBuilder
  -> Sdk.TxBalancing
  -> Either Sdk.TransactionError (BalancedTxBody AlonzoEra)
buildBalancedTx Sdk.NetworkParameters{..}  txb@Sdk.TxBuilder{..} Sdk.TxBalancing{txBalancingChangeAddr=Sdk.ChangeAddress{..},..}= do
    txContent  <- ensureMinAda =<< buildTxBodyContent pparams network txBalancingCollateral txb
    changeAddress' <- adaptError $ Conv.toCardanoAddress network changeAddress
    initialInputValue <- adaptError $ Conv.toCardanoValue (mconcat $ txInCandidateValue <$> txBuilderInputs)
    balanceTx pparams network initialInputValue txBalancingInputs changeAddress' txContent
  where
    ensureMinAda txContent = do
      ensuredTxOuts <- mapM (ensureMinAdaTxOut pparams) $ txOuts txContent
      pure $ txContent {txOuts = ensuredTxOuts}

newtype OrderedValue = OV Value deriving (Eq, Show)

instance Ord OrderedValue where
  OV a <= OV b =
    let valueList = valueToList a
    in all (\(assetId, quantity) -> quantity <= selectAsset b assetId) valueList

balanceTx
  :: ProtocolParameters
  -> NetworkId
  -> Value
  -> [Sdk.TxInCandidate]
  -> AddressInEra AlonzoEra
  -> TxBodyContent BuildTx AlonzoEra
  -> Either Sdk.TransactionError (BalancedTxBody AlonzoEra)
balanceTx pparams network initialInputValue inputs changeAddr txContent = do
    UTxO utxo <- buildInputsUTxO network inputs
    let inputs' = M.toList utxo
    let presortedInputs = sortOn (Down . valueToLovelace . txOutValue . snd) inputs'
    go 0 presortedInputs
  where
    updateTx change fee txInputs = txContent
      { txIns = txIns txContent <> txInputs
      , txOuts = change : txOuts txContent
      , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra fee
      }
    witnessedTxIn txIn = (txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
    updateChange value = TxOut changeAddr (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumNone
    txOutValue (TxOut _ value _) = txOutValueToValue value
    valueUsefulness :: Value -> Value -> Quantity
    valueUsefulness target value =
      let valueList = valueToList value
      in sum $ map (\(assetId, quantity) -> quantity `min` selectAsset target assetId) valueList
    go additionalLovelace inputs' = do
      let outs = txOuts txContent
      let totalOutputValue = mconcat $ lovelaceToValue additionalLovelace : (txOutValue <$> outs)

      let selectCoins selected acc availableInputs
            | OV totalOutputValue <= OV acc           = Right (selected, acc)
            | null availableInputs                    = Left TxInsufficentFundsError
            | otherwise                               =
              let (input:remaining) = sortOn
                                      ( Down
                                      . valueUsefulness totalOutputValue
                                      . (acc<>)
                                      . txOutValue
                                      . snd
                                      ) availableInputs
                  selected' = input:selected
                  acc' = acc <> (txOutValue . snd) input
              in selectCoins selected' acc' remaining
      (selected, selectedValue) <- selectCoins [] initialInputValue inputs'

      let changeValue = selectedValue <> negateValue totalOutputValue <> lovelaceToValue additionalLovelace
      let witnessedTxIns = witnessedTxIn . fst <$> selected

      let adjustFee fee = do
             let newChange = updateChange (changeValue <> negateValue (lovelaceToValue fee))
             let newTx = updateTx newChange fee witnessedTxIns
             fee' <- calculateFee pparams newTx
             if fee' > fee then adjustFee fee' else pure (newTx, newChange, fee)

      (txContent', change, fee) <- adjustFee 0

      changeMinAda <- minAdaTxOut pparams change
      let adaFromTxOut = selectLovelace . txOutValue
      let changeActualAda = adaFromTxOut change
      let neededAda = changeMinAda - changeActualAda

      if changeMinAda > changeActualAda
        then go (additionalLovelace + neededAda) inputs'
        else do
         txBody <- adaptToOtherError $ makeTransactionBody txContent'
         return $ BalancedTxBody txBody change fee

debug :: Show a => T.Text -> a -> b -> b
debug msg a = trace ("[DEBUG]   " <> msg <> ": " <> (T.pack . show) a)

adaptToOtherError :: Show err => Either err  b -> Either TransactionError b
adaptToOtherError = mapLeft (Sdk.TxOtherError . T.pack . show)

minAdaTxOut :: ProtocolParameters -> TxOut CtxTx AlonzoEra -> Either Sdk.TransactionError Lovelace
minAdaTxOut pparams txOut =
  adaptToOtherError $ selectLovelace <$> calculateMinimumUTxO ShelleyBasedEraAlonzo txOut pparams

ensureMinAdaTxOut :: ProtocolParameters -> TxOut CtxTx AlonzoEra -> Either Sdk.TransactionError (TxOut CtxTx AlonzoEra)
ensureMinAdaTxOut pparams txOut@(TxOut addr value datum) = do
    expectedMinAda <- minAdaTxOut pparams txOut
    let diff = expectedMinAda - txOutValueToLovelace value
    Right $ if diff > 0 then TxOut addr (addValue value diff) datum else txOut
  where
    addValue (TxOutAdaOnly oas lovlace) l = TxOutAdaOnly oas (lovlace <> l)
    addValue (TxOutValue mas value)     l = TxOutValue mas (value <> lovelaceToValue l)

calculateFee :: ProtocolParameters -> TxBodyContent BuildTx AlonzoEra -> Either Sdk.TransactionError Lovelace
calculateFee params content =
    adaptError $ estimateFee <$> makeTransactionBody content
  where
    shelleySigs = estimateTransactionKeyWitnessCount content
    byronSigs = 0
    estimateFee body = evaluateTransactionFee params body shelleySigs byronSigs
    adaptError = mapLeft (Sdk.TxCardanoError . Conv.TxBodyError . show)


negateLovelace :: Lovelace -> Lovelace
negateLovelace (Lovelace v) = Lovelace (-v)
