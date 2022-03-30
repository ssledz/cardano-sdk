module Cardano.Sdk.Network
  ( NetworkParameters (..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PoolId, ProtocolParameters)
import           Cardano.Slotting.Time (SystemStart)
import qualified Data.Set              as Set


data NetworkParameters =  NetworkParameters
  { pparams    :: ProtocolParameters
  , network    :: NetworkId
  , sysstart   :: SystemStart
  , pools      :: Set.Set PoolId
  , eraHistory :: EraHistory CardanoMode
  }
