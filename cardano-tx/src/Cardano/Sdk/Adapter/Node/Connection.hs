module Cardano.Sdk.Adapter.Node.Connection where

import           Cardano.Api

type NodeConn = LocalNodeConnectInfo CardanoMode

connInfo :: NetworkId -> FilePath -> NodeConn
connInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))
