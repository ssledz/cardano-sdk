module Cardano.Sdk.Transaction.Data where

import qualified Cardano.Api       as C
import qualified Data.Map          as M

import           Ledger
import           PlutusTx.Foldable (fold)


newtype UtxO = UtxO { unUtO :: M.Map TxIn TxOut }

class ToLedgerTxOutRef a where
  toLedgerTxOutRef :: a -> TxOutRef

class ToLedgerTxId a where
  toLedgerTxId :: a -> TxId

class ToLedgerTxIn a where
  toLedgerTxIn :: a -> TxIn

class ToLedgerTxOut a where
  toLedgerTxOut :: a -> TxOut

class ToLedgerValue a where
  toLedgerValue :: a -> Value

instance ToLedgerValue a => ToLedgerValue [a] where
  toLedgerValue xs = fold $ toLedgerValue <$> xs


