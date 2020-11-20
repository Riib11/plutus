{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Trace.Playground(
    PlaygroundTrace
    -- * Constructing traces
    , Waiting.waitUntilSlot
    , Waiting.waitNSlots
    , Waiting.nextSlot
    , EmulatedWalletAPI.payToWallet
    , RunContractPlayground.callEndpoint
    -- * Running traces
    , EmulatorConfig(..)
    , initialDistribution
    , defaultEmulatorConfig
    , runPlaygroundStream
    -- * Interpreter
    , interpretPlaygroundTrace
    , walletInstanceTag
    ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer (Member, Eff, interpret, raise)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import           Control.Monad.Freer.Extras                      (raiseEnd3)
import           Control.Monad.Freer.Log                         (LogMsg (..), mapLog, LogMessage)
import qualified Data.Aeson as JSON
import Control.Monad.Freer.State (State, evalState)
import           Control.Monad.Freer.Coroutine                   (Yield)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map

import           Language.Plutus.Contract                      (Contract (..), HasBlockchainActions)
import           Wallet.Emulator.Wallet (Wallet (..))
import Plutus.Trace.Emulator.Types (walletInstanceTag)
import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds)
import           Plutus.Trace.Scheduler                          (SystemCall, runThreads, ThreadId)
import Wallet.Emulator.Stream (runTraceStream, EmulatorConfig(..), EmulatorErr(..), initialDistribution, defaultEmulatorConfig)
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect)
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent' (..),
                                                                  MultiAgentEffect,
                                                                  schedulerEvent, EmulatorEvent)
import Wallet.Types (ContractInstanceId)
import           Plutus.Trace.Emulator.ContractInstance          (EmulatorRuntimeError)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, 
                                                                  EmulatorMessage (..), EmulatorThreads)
import qualified Plutus.Trace.Effects.RunContractPlayground as RunContractPlayground
import Plutus.Trace.Effects.RunContractPlayground (RunContractPlayground, handleRunContractPlayground)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI, handleEmulatedWalletAPI)
import qualified Plutus.Trace.Effects.EmulatedWalletAPI as EmulatedWalletAPI
import Plutus.Trace.Effects.Waiting (Waiting, handleWaiting)
import qualified Plutus.Trace.Effects.Waiting as Waiting
import Streaming (Stream)
import Streaming.Prelude (Of)

type PlaygroundTrace a =
    Eff
        '[ RunContractPlayground
         , Waiting
         , EmulatedWalletAPI
        ] a

handlePlaygroundTrace ::
    forall s e effs a.
    ( HasBlockchainActions s
    , ContractConstraints s
    , Show e
    , JSON.ToJSON e
    , Member MultiAgentEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (Error EmulatorRuntimeError) effs
    , Member (State (Map Wallet ContractInstanceId)) effs
    , Member (State EmulatorThreads) effs
    , Member ContractInstanceIdEff effs
    )
    => Contract s e ()
    -> PlaygroundTrace a
    -> Eff (Reader ThreadId ': Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) a
handlePlaygroundTrace contract =
    interpret handleEmulatedWalletAPI
    . interpret (handleWaiting @_ @effs)
    . interpret (handleRunContractPlayground @s @e @_ @effs contract)
    . raiseEnd3


-- | Run a 'Trace Playground', streaming the log messages as they arrive
runPlaygroundStream :: forall s e effs a.
    ( HasBlockchainActions s
    , ContractConstraints s
    , Show e
    , JSON.ToJSON e
    )
    => EmulatorConfig
    -> Contract s e ()
    -> PlaygroundTrace a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runPlaygroundStream conf contract = runTraceStream conf . interpretPlaygroundTrace contract (conf ^. initialDistribution . to Map.keys)

interpretPlaygroundTrace :: forall s e effs a.
    ( Member MultiAgentEffect effs
    , Member (Error EmulatorRuntimeError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , HasBlockchainActions s
    , ContractConstraints s
    , Show e
    , JSON.ToJSON e
    )
    => Contract s e () -- ^ The contract
    -> [Wallet] -- ^ Wallets that should be simulated in the emulator
    -> PlaygroundTrace a
    -> Eff effs ()
interpretPlaygroundTrace contract wallets action =
    evalState @EmulatorThreads mempty
        $ evalState @(Map Wallet ContractInstanceId) Map.empty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            raise $ launchSystemThreads wallets
            void
                $ handlePlaygroundTrace contract 
                $ do
                    void $ Waiting.nextSlot
                    traverse_ RunContractPlayground.launchContract wallets
                    action
