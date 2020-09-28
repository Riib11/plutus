{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator(
    Emulator
    , EmulatorTrace
    , EmulatorErr(..)
    , ContractHandle(..)
    , ContractInstanceTag
    , ContractConstraints
    -- * Constructing Traces
    , RunContract.activateContract
    , RunContract.activateContractWallet
    , RunContract.walletInstanceTag
    , RunContract.callEndpoint
    , EmulatedWalletAPI.liftWallet
    , EmulatedWalletAPI.payToWallet
    , Waiting.nextSlot
    , Waiting.waitUntilSlot
    , Waiting.waitNSlots
    , EmulatorControl.freezeContractInstance
    , EmulatorControl.thawContractInstance
    -- ** Inspecting the chain state
    , EmulatorControl.setSigningProcess
    , EmulatorControl.chainState
    , ChainState.chainNewestFirst
    , ChainState.txPool
    , ChainState.index
    , ChainState.currentSlot
    -- ** Inspecting the agent states
    , EmulatorControl.agentState
    , Wallet.ownPrivateKey
    , Wallet.nodeClient
    , Wallet.chainIndex
    , Wallet.signingProcess
    -- * Running traces
    , EmulatorConfig(..)
    , initialDistribution
    , defaultEmulatorConfig
    , runEmulatorStream
    -- * Interpreter
    , interpretEmulatorTrace
    ) where

import           Control.Lens
import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error)
import           Control.Monad.Freer.Extras                      (raiseEnd4)
import           Control.Monad.Freer.Log                         (LogMessage (..), LogMsg (..),
                                                                  mapLog)
import           Control.Monad.Freer.State                       (State, evalState)
import qualified Data.Map                                        as Map
import           Plutus.Trace.Scheduler                          (SystemCall, runThreads)
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect)
import qualified Wallet.Emulator.Chain                           as ChainState
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent, EmulatorEvent' (..), EmulatorState,
                                                                  MultiAgentEffect,
                                                                  schedulerEvent)
import qualified Wallet.Emulator.Wallet                          as Wallet
import Wallet.Emulator.Stream (runTraceStream, EmulatorConfig(..), EmulatorErr(..), initialDistribution, defaultEmulatorConfig)

import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI, handleEmulatedWalletAPI)
import qualified Plutus.Trace.Effects.EmulatedWalletAPI as EmulatedWalletAPI
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl, handleEmulatorControl)
import qualified Plutus.Trace.Effects.EmulatorControl as EmulatorControl
import Plutus.Trace.Effects.RunContract (RunContract, handleRunContract)
import qualified Plutus.Trace.Effects.RunContract as RunContract
import Plutus.Trace.Effects.Waiting (Waiting, handleWaiting)
import qualified Plutus.Trace.Effects.Waiting as Waiting
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..), Emulator,
                                                                  EmulatorMessage (..), EmulatorThreads, ContractInstanceTag)
import Streaming (Stream)
import Streaming.Prelude (Of)

type EmulatorTrace a =
        Eff
            '[ RunContract
            , Waiting
            , EmulatorControl
            , EmulatedWalletAPI
            ] a

handleEmulatorTrace ::
    forall effs a.
    ( Member MultiAgentEffect effs
    , Member (State EmulatorThreads) effs
    , Member (State EmulatorState) effs
    , Member (Error ContractInstanceError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member ContractInstanceIdEff effs
    )
    => EmulatorTrace a
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) a
handleEmulatorTrace =
    interpret handleEmulatedWalletAPI
    . interpret (handleEmulatorControl @_ @effs)
    . interpret (handleWaiting @_ @effs)
    . interpret (handleRunContract @_ @effs)
    . raiseEnd4

-- | Run a 'Trace Emulator', streaming the log messages as they arrive
runEmulatorStream :: forall effs a.
    EmulatorConfig
    -> EmulatorTrace a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runEmulatorStream conf = runTraceStream conf . interpretEmulatorTrace conf

-- | Interpret a 'Trace Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretEmulatorTrace :: forall effs a.
    ( Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => EmulatorConfig
    -> EmulatorTrace a
    -> Eff effs ()
interpretEmulatorTrace conf action =
    -- add a wait action to the beginning to ensure that the
    -- initial transaction gets validated before the wallets
    -- try to spend their funds
    let action' = Waiting.nextSlot >> action
        wallets = conf ^. initialDistribution . to Map.keys
    in
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            launchSystemThreads wallets
            void $ handleEmulatorTrace action'
