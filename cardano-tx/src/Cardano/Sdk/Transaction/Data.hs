{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction.Data where

import           Cardano.Sdk.Address
import           Cardano.Sdk.Network
import           Cardano.Sdk.UTxO
import qualified Data.Map            as M
import           Data.Maybe
import           Ledger

newtype ChangeAddress = ChangeAddress
  { unChangeAddress :: Address } deriving (Show)

data TxInCandidate = TxInCandidate
  { txInCandidateTxOut :: TxOutput
  , txInCandidateType  :: TxInType
  } deriving Show

newtype TxInCollateral = TxInCollateral
  { txInCollateralRef :: TxOutRef
  } deriving Show

data TxOutput = TxOutput
  { txOutputRef   :: TxOutRef
  , txOutputOut   :: TxOut
  , txOutputDatum :: Maybe Datum
  } deriving Show

data TxBuilder = TxBuilder
  { txBuilderInputs     :: [TxInCandidate]
  , txBuilderOutputs    :: [TxOutput]
  , txBuilderChangeAddr :: ChangeAddress
  , txBulderCollateral  :: Maybe TxInCollateral
  , txBuilderValidRange :: SlotRange
  , txBuilderSigners    :: [PaymentPubKeyHash]
  } deriving (Show)

txOutputToDatumMap :: TxOutput -> M.Map DatumHash Datum
txOutputToDatumMap TxOutput{..} =
  fromMaybe mempty (txOutDatumHash txOutputOut >>= (\hash -> fmap (M.singleton hash) txOutputDatum))
