{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Testing contracts with HUnit and Tasty
module Language.Plutus.Contract.Test(
      module X
    , TracePredicate
    , ContractConstraints
    , Language.Plutus.Contract.Test.not
    , (.&&.)
    -- * Assertions
    , endpointAvailable
    , interestingAddress
    , queryingUtxoAt
    , assertDone
    , assertNotDone
    , assertContractError
    , Outcome(..)
    , assertOutcome
    , assertInstanceLog
    , assertNoFailedTransactions
    , assertFailedTransaction
    , assertHooks
    , assertResponses
    , assertUserLog
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , waitingForSlot
    , walletWatchingAddress
    , valueAtAddress
    -- * Checking predicates
    , checkPredicate
    , checkPredicateOptions
    , CheckOptions
    , defaultCheckOptions
    , minLogLevel
    , maxSlot
    ) where

import           Control.Applicative                             (liftA2)
import           Control.Foldl                                   (FoldM)
import qualified Control.Foldl                                   as L
import           Control.Lens                                    (at, makeLenses, (^.))
import           Control.Monad                                   (guard, unless)
import           Control.Monad.Freer                             (Eff, reinterpret, runM, sendM)
import           Control.Monad.Freer.Error                       (Error, runError)
import           Control.Monad.Freer.Log                         (LogLevel (..), LogMessage (..))
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.Writer                      (Writer (..), tell)
import           Data.Foldable                                   (fold, toList)
import           Data.Maybe                                      (mapMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Row                                        (Forall, HasType)
import           Data.String                                     (IsString (..))
import qualified Data.Text                                       as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text           (renderStrict)
import           Data.Void
import           GHC.TypeLits                                    (KnownSymbol, Symbol, symbolVal)
import           Ledger.Tx                                       (Tx)
import qualified Test.Tasty.HUnit                                as HUnit
import           Test.Tasty.Providers                            (TestTree)

import qualified Language.PlutusTx.Prelude                       as P

import           Language.Plutus.Contract.Resumable              (Request (..), Response (..))
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Types                  (Contract (..))
import           Ledger.Constraints.OffChain                     (UnbalancedTx)

import           Language.Plutus.Contract.Effects.AwaitSlot      (SlotSymbol)
import qualified Language.Plutus.Contract.Effects.AwaitSlot      as AwaitSlot
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoints
import qualified Language.Plutus.Contract.Effects.UtxoAt         as UtxoAt
import qualified Language.Plutus.Contract.Effects.WatchAddress   as WatchAddress
import           Language.Plutus.Contract.Effects.WriteTx        (HasWriteTx)

import           Ledger.Address                                  (Address)
import           Ledger.Index                                    (ValidationError)
import           Ledger.Slot                                     (Slot)
import           Ledger.Value                                    (Value)
import           Wallet.Emulator                                 (EmulatorEvent, EmulatorTimeEvent)

import           Language.Plutus.Contract.Schema                 (Event (..), Handlers (..), Input, Output)
import           Language.Plutus.Contract.Trace                  as X
import           Plutus.Trace                                    (defaultEmulatorConfig)
import           Plutus.Trace.Emulator                           (EmulatorConfig (..), EmulatorTrace, runEmulatorStream)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractInstanceLog,
                                                                  ContractInstanceTag, UserThreadMsg)
import qualified Streaming                                       as S
import qualified Streaming.Prelude                               as S
import           Wallet.Emulator.Folds                           (EmulatorFoldErr, Outcome (..), postMapM)
import qualified Wallet.Emulator.Folds                           as Folds
import           Wallet.Emulator.Stream                          (filterLogLevel, foldEmulatorStreamM, takeUntilSlot)

type TracePredicate = FoldM (Eff '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]) EmulatorEvent Bool

infixl 3 .&&.

(.&&.) :: TracePredicate -> TracePredicate -> TracePredicate
(.&&.) = liftA2 (&&)

not :: TracePredicate -> TracePredicate
not = fmap Prelude.not

-- | Options for running the
data CheckOptions =
    CheckOptions
        { _minLogLevel :: LogLevel -- ^ Minimum log level for emulator log messages to be included in the test output (printed if the test fails)
        , _maxSlot     :: Slot -- ^ When to stop the emulator
        } deriving (Eq, Show)

makeLenses ''CheckOptions

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
    CheckOptions
        { _minLogLevel = Info
        , _maxSlot = 125
        }

type TestEffects = '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]

-- | Check if the emulator trace meets the condition
checkPredicate ::
    String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicate = checkPredicateOptions defaultCheckOptions

-- | A version of 'checkPredicate' with configurable 'CheckOptions'
checkPredicateOptions ::
    CheckOptions -- ^ Options to use
    -> String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicateOptions CheckOptions{_minLogLevel, _maxSlot} nm predicate action = HUnit.testCaseSteps nm $ \step -> do
    let cfg = defaultEmulatorConfig
        dist = _initialDistribution cfg
        theStream :: forall effs. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
        theStream = takeUntilSlot _maxSlot $ runEmulatorStream cfg action
        consumeStream :: forall a. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff TestEffects) a -> Eff TestEffects (S.Of Bool a)
        consumeStream = foldEmulatorStreamM @TestEffects predicate
    result <- runM
                $ reinterpret @(Writer (Doc Void)) @IO  (\case { Tell d -> sendM $ step $ Text.unpack $ renderStrict $ layoutPretty defaultLayoutOptions d })
                $ runError
                $ runReader dist
                $ consumeStream theStream

    unless (fmap S.fst' result == Right True) $ do
        step "Test failed."
        step "Emulator log:"
        S.mapM_ step
            $ S.hoist runM
            $ S.map (Text.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty)
            $ filterLogLevel _minLogLevel
            theStream

        case result of
            Left err -> do
                step "Error:"
                step (show err)
                HUnit.assertBool nm False
            Right _ -> HUnit.assertBool nm False

endpointAvailable
    :: forall (l :: Symbol) s e a.
       ( HasType l Endpoints.ActiveEndpoint (Output s)
       , KnownSymbol l
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> TracePredicate
endpointAvailable contract inst =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        if any (Endpoints.isActive @l @s) (rqRequest <$> rqs)
            then pure True
            else do
                tell @(Doc Void) ("missing endpoint:" <+> fromString (symbolVal (Proxy :: Proxy l)))
                pure False

interestingAddress
    :: forall s e a.
       ( WatchAddress.HasWatchAddress s
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> Address
    -> TracePredicate
interestingAddress contract inst addr =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks = mapMaybe WatchAddress.watchedAddress (rqRequest <$> rqs)
        if any (== addr) hks
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "Interesting addresses of " <+> pretty inst <> colon
                    <+> nest 2 (concatWith (surround (comma <> space))  (viaShow <$> toList hks))
                , "Missing address:", viaShow addr
                ]
            pure False

queryingUtxoAt
    :: forall s e a.
       ( UtxoAt.HasUtxoAt s
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> Address
    -> TracePredicate
queryingUtxoAt contract inst addr =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks = mapMaybe UtxoAt.utxoAtRequest (rqRequest <$> rqs)
        if any (== addr) hks
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "UTXO queries of " <+> pretty inst <> colon
                    <+> nest 2 (concatWith (surround (comma <> space))  (viaShow <$> toList hks))
                , "Missing address:", viaShow addr
                ]
            pure False

tx
    :: forall s e a.
       ( HasWriteTx s
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate
tx contract inst flt nm =
    flip postMapM (Folds.instanceTransactions contract inst) $ \unbalancedTxns -> do
        if any flt unbalancedTxns
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "Unbalanced transactions of" <+> pretty inst <> colon
                    <+> nest 2 (vsep (fmap pretty unbalancedTxns))
                , "No transaction with '" <> fromString nm <> "'"]
            pure False

walletWatchingAddress :: Wallet -> Address -> TracePredicate
walletWatchingAddress w addr = flip postMapM (L.generalize $ Folds.walletWatchingAddress w addr) $ \r -> do
    unless r $ do
        tell @(Doc Void) $ "Wallet" <+> pretty w <+> "not watching address" <+> pretty addr
    pure r

assertEvents
    :: forall s e a.
       ( Forall (Input s) Pretty
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> ([Event s] -> Bool)
    -> String
    -> TracePredicate
assertEvents contract inst pr nm =
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let responses = fmap State.rspResponse rqs
            result = pr responses
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Event log for" <+> pretty inst <> ":"
                , nest 2 (vsep (fmap pretty responses))
                , "Fails" <+> squotes (fromString nm)
                ]
        pure result

-- | Check that the funds at an address meet some condition.
valueAtAddress :: Address -> (Value -> Bool) -> TracePredicate
valueAtAddress address check =
    flip postMapM (L.generalize $ Folds.valueAtAddress address) $ \vl -> do
        let result = check vl
        unless result $ do
            tell @(Doc Void) ("Funds at address" <+> pretty address <+> "were" <> pretty vl)
        pure result

waitingForSlot
    :: forall s e a.
       ( HasType SlotSymbol AwaitSlot.WaitingForSlot (Output s)
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> Slot
    -> TracePredicate
waitingForSlot contract inst sl =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs ->
        case mapMaybe (\e -> AwaitSlot.request e >>= guard . (==) sl) (rqRequest <$> rqs) of
            [] -> do
                tell @(Doc Void) $ pretty inst <+> "not waiting for any slot notifications. Expected:" <+>  viaShow sl
                pure False
            _ -> pure True

anyTx
    :: forall s e a.
       ( HasWriteTx s
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> TracePredicate
anyTx contract inst = tx contract inst (const True) "anyTx"

assertHooks
    :: forall s e a.
       ( Forall (Output s) Pretty
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> ([Handlers s] -> Bool)
    -> String
    -> TracePredicate
assertHooks contract inst p nm =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks = rqRequest <$> rqs
            result = p hks
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Handlers for" <+> pretty inst <> colon
                , nest 2 (pretty hks)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Make an assertion about the responses provided to the contract instance.
assertResponses
    :: forall s e a.
       ( Forall (Input s) Pretty
       , ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> ([Response (Event s)] -> Bool)
    -> String
    -> TracePredicate
assertResponses contract inst p nm =
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let result = p rqs
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Record:"
                , nest 2 (pretty rqs)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | A 'TracePredicate' checking that the wallet's contract instance finished
--   without errors.
assertDone
    :: forall s e a.
    ( ContractConstraints s
    )
    => Contract s e a
    -> ContractInstanceTag
    -> (a -> Bool)
    -> String
    -> TracePredicate
assertDone contract inst pr = assertOutcome contract inst (\case { Done a -> pr a; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance is
--   waiting for input.
assertNotDone
    :: forall s e a.
    ( ContractConstraints s
    )
    => Contract s e a
    -> ContractInstanceTag
    -> String
    -> TracePredicate
assertNotDone contract inst = assertOutcome contract inst (\case { NotDone -> True; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance
--   failed with an error.
assertContractError
    :: forall s e a.
    ( ContractConstraints s
    )
    => Contract s e a
    -> ContractInstanceTag
    -> (e -> Bool)
    -> String
    -> TracePredicate
assertContractError contract inst p = assertOutcome contract inst (\case { Failed err -> p err; _ -> False })

assertOutcome
    :: forall s e a.
       ( ContractConstraints s
       )
    => Contract s e a
    -> ContractInstanceTag
    -> (Outcome e a -> Bool)
    -> String
    -> TracePredicate
assertOutcome contract inst p nm =
    flip postMapM (Folds.instanceOutcome contract inst) $ \outcome -> do
        let result = p outcome
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Outcome of" <+> pretty inst <> colon
                , indent 2 (viaShow result)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

walletFundsChange :: Wallet -> Value -> TracePredicate
walletFundsChange w dlt =
    flip postMapM (L.generalize $ Folds.walletFunds w) $ \finalValue -> do
        initialDist <- ask @InitialDistribution
        let initialValue = fold (initialDist ^. at w)
            result = initialValue P.+ dlt == finalValue
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Expected funds of" <+> pretty w <+> "to change by" <+> viaShow dlt
                , "but they changed by", viaShow (finalValue P.- initialValue)]
        pure result

-- | Assert that at least one transaction failed to validate, and that all
--   transactions that failed meet the predicate.
assertFailedTransaction :: (Tx -> ValidationError -> Bool) -> TracePredicate
assertFailedTransaction predicate =
    flip postMapM (L.generalize Folds.failedTransactions) $ \case
        [] -> do
            tell @(Doc Void) $ "No transactions failed to validate."
            pure False
        xs -> pure (all (uncurry predicate) xs)

-- | Assert that no transaction failed to validate.
assertNoFailedTransactions :: TracePredicate
assertNoFailedTransactions =
    flip postMapM (L.generalize Folds.failedTransactions) $ \case
        [] -> pure True
        xs -> do
            tell @(Doc Void) $ vsep ("Transactions failed to validate:" : fmap pretty xs)
            pure False

assertInstanceLog ::
    ContractInstanceTag
    -> ([EmulatorTimeEvent ContractInstanceLog] -> Bool)
    -> TracePredicate
assertInstanceLog tag pred' = flip postMapM (L.generalize $ Folds.instanceLog tag) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("Contract instance log failed to validate:" : fmap pretty lg))
    pure result

assertUserLog ::
    ([EmulatorTimeEvent UserThreadMsg] -> Bool)
    -> TracePredicate
assertUserLog pred' = flip postMapM (L.generalize Folds.userLog) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("User log failed to validate:" : fmap pretty lg))
    pure result
