{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | Running emulator actions that produce streams of events
module Wallet.Emulator.Stream(
    -- * Emulator streams
    EmulatorConfig(..)
    , EmulatorErr(..)
    , InitialChainState
    , initialChainState
    , initialDist
    , onInitialThreadStopped
    , defaultEmulatorConfig
    , runTraceStream
    -- * Stream manipulation
    , takeUntilSlot
    , filterLogLevel
    -- * Consuming streams
    , foldStreamM
    , foldEmulatorStreamM
    ) where

import qualified Control.Foldl                          as L
import           Control.Lens                           (filtered, makeLenses, preview, view)
import           Control.Monad.Freer                    (Eff, Member, interpret, reinterpret, run, subsume, type (~>))
import           Control.Monad.Freer.Coroutine          (Yield, yield)
import           Control.Monad.Freer.Error              (Error, runError)
import           Control.Monad.Freer.Extras             (raiseEnd6, wrapError)
import           Control.Monad.Freer.Log                (LogLevel, LogMessage (..), LogMsg (..), logMessageContent,
                                                         mapMLog)
import           Control.Monad.Freer.State              (State, evalState, gets)
import           Control.Monad.Freer.Stream             (runStream)
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromMaybe)
import qualified Data.Set                               as Set
import qualified Ledger.AddressMap                      as AM
import           Ledger.Blockchain                      (Block)
import           Ledger.Slot                            (Slot)
import           Ledger.Value                           (Value)
import           Streaming                              (Stream)
import qualified Streaming                              as S
import           Streaming.Prelude                      (Of)
import qualified Streaming.Prelude                      as S
import           Wallet.API                             (WalletAPIError)
import           Wallet.Emulator                        (EmulatorEvent, EmulatorEvent')
import qualified Wallet.Emulator                        as EM
import           Wallet.Emulator.Chain                  (ChainControlEffect, ChainEffect, _SlotAdd)
import           Wallet.Emulator.MultiAgent             (EmulatorState, EmulatorTimeEvent (..), MultiAgentEffect,
                                                         chainEvent, eteEvent)
import           Wallet.Emulator.Wallet                 (Wallet (..), walletAddress)

-- TODO: Move these two to 'Wallet.Emulator.XXX'?
import           Language.Plutus.Contract.Trace         (InitialDistribution, defaultDist)
import           Plutus.Trace.Emulator.ContractInstance (EmulatorRuntimeError)
import           Plutus.Trace.Scheduler                 (OnInitialThreadStopped (Stop))

-- | Finish the stream at the end of the given slot.
takeUntilSlot :: forall effs a. Slot -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
takeUntilSlot maxSlot = S.takeWhile (maybe True (\sl -> sl <= maxSlot) . preview (logMessageContent . eteEvent . chainEvent . _SlotAdd))

-- | Remove from the stream all log messages whose log level is lower than the
--   the given level.
filterLogLevel :: forall effs a. LogLevel -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
filterLogLevel lvl = S.mapMaybe (preview (filtered (\LogMessage{_logLevel} -> lvl <= _logLevel)))

-- | Apply a fold to an effectful stream of events.
foldStreamM :: forall m a b c.
    Monad m
    => L.FoldM m a b
    -> S.Stream (S.Of a) m c
    -> m (S.Of b c)
foldStreamM theFold = L.impurely S.foldM theFold

-- | Consume an emulator event stream. Make sure that the stream terminates
--   (either with 'takeUntilSlot', or by using a short-circuiting effect
--   such as 'Error')
foldEmulatorStreamM :: forall effs a b.
    L.FoldM (Eff effs) EmulatorEvent b
    -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
    -> Eff effs (S.Of b a)
foldEmulatorStreamM theFold =
    foldStreamM (L.premapM (pure . view logMessageContent) theFold)

-- | Turn an emulator action into a potentially infinite 'Stream' of emulator
--   log messages.
runTraceStream :: forall effs.
    EmulatorConfig
    -> Eff '[ State EmulatorState
            , LogMsg EmulatorEvent'
            , MultiAgentEffect
            , ChainEffect
            , ChainControlEffect
            , Error EmulatorRuntimeError
            ] ()
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runTraceStream conf =
    fmap (either Just (const Nothing))
    . S.hoist (pure . run)
    . runStream @(LogMessage EmulatorEvent) @_ @'[]
    . evalState (initialState conf)
    . interpret handleLogCoroutine
    . reinterpret @_ @(LogMsg EmulatorEvent) (mkTimedLogs @EmulatorEvent')
    . runError
    . wrapError WalletErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated
    . subsume
    . subsume @(State EmulatorState)
    . raiseEnd6

data EmulatorConfig =
    EmulatorConfig
        { _initialChainState      :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        , _onInitialThreadStopped :: OnInitialThreadStopped -- ^ What to do when the initial thread is done
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution Block

-- | The wallets' initial funds
initialDist :: InitialChainState -> InitialDistribution
initialDist = either id walletFunds where
    walletFunds :: Block -> Map Wallet Value
    walletFunds theBlock =
        let values = AM.values $ AM.fromChain [theBlock]
            getFunds wllt = fromMaybe mempty $ Map.lookup (walletAddress wllt) values
        in Map.fromSet getFunds (Set.fromList $ Wallet <$> [1..10])

defaultEmulatorConfig :: EmulatorConfig
defaultEmulatorConfig =
    EmulatorConfig
        { _initialChainState = Left defaultDist
        , _onInitialThreadStopped = Stop
        }

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{_initialChainState} =
    either
        (EM.emulatorStateInitialDist . Map.mapKeys EM.walletPubKey)
        EM.emulatorStatePool
        _initialChainState

data EmulatorErr =
    WalletErr WalletAPIError
    | AssertionErr EM.AssertionError
    | InstanceErr EmulatorRuntimeError
    deriving (Show)

handleLogCoroutine :: forall e effs.
    Member (Yield (LogMessage e) ()) effs
    => LogMsg e
    ~> Eff effs
handleLogCoroutine = \case LMessage m -> yield m id

-- | Annotate emulator log messages with the current system time
--   (slot number)
mkTimedLogs :: forall a effs.
    ( Member (LogMsg (EmulatorTimeEvent a)) effs
    , Member (State EmulatorState) effs
    )
    => LogMsg a
    ~> Eff effs
mkTimedLogs = mapMLog f where
    f :: a -> Eff effs (EmulatorTimeEvent a)
    f a =
        EmulatorTimeEvent
            <$> gets (view $ EM.chainState . EM.currentSlot)
            <*> pure a

makeLenses ''EmulatorConfig
