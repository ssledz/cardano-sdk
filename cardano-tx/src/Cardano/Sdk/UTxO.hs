module Cardano.Sdk.UTxO where

import qualified Data.Map          as M

import           Data.String
import qualified Data.Text         as T
import           Ledger
import           PlutusTx.Foldable (fold)

newtype UTxO = UTxO { unUTxO :: M.Map TxIn TxOut } deriving (Show, Eq)

instance Semigroup UTxO where
  UTxO a <> UTxO b = UTxO $ a <> b

instance Monoid UTxO where
  mempty = UTxO mempty

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

class ToLedgerUTxO a where
  toLedgerUTxO :: a -> UTxO

instance ToLedgerValue a => ToLedgerValue [a] where
  toLedgerValue xs = fold $ toLedgerValue <$> xs

instance ToLedgerUTxO a => ToLedgerUTxO [a] where
  toLedgerUTxO xs = mconcat $ toLedgerUTxO <$> xs

parseTxIn :: T.Text -> Integer -> TxIn
parseTxIn hash index = TxIn ref Nothing
  where
    txId' = fromString $ T.unpack hash
    ref = TxOutRef txId' index

findTxOutByTxIn :: UTxO -> TxIn -> Maybe TxOut
findTxOutByTxIn utxo txIn = M.lookup txIn $ unUTxO utxo
