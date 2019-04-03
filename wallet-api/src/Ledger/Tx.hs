{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Tx(
    -- * Transactions
    Tx(..),
    TxIdOf(..),
    TxId,
    inputs,
    outputs,
    txOutRefs,
    unspentOutputsTx,
    spentOutputs,
    updateUtxo,
    validValuesTx,
    -- ** Hashing transactions
    preHash,
    hashTx,
    -- ** Stripped transactions
    TxStripped(..),
    strip,
    -- * Transaction outputs
    TxOutOf(..),
    TxOutType(..),
    TxOut,
    TxOutRefOf(..),
    TxOutRef,
    isPubKeyOut,
    isPayToScriptOut,
    outAddress,
    outValue,
    outType,
    txOutPubKey,
    txOutData,
    pubKeyTxOut,
    scriptTxOut,
    -- * Transaction inputs
    TxInOf(..),
    TxInType(..),
    TxIn,
    inRef,
    inType,
    inScripts,
    inSignature,
    validRange,
    pubKeyTxIn,
    scriptTxIn,
    -- * Addresses
    AddressOf(..),
    Address,
    pubKeyAddress,
    scriptAddress
    ) where

import qualified Codec.CBOR.Write                         as Write
import           Codec.Serialise.Class                    (Serialise, decode, encode)
import           Control.Lens                             hiding (lifted)
import           Crypto.Hash                              (Digest, SHA256, digestFromByteString, hash)
import           Data.Aeson                               (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson                               as JSON
import qualified Data.Aeson.Extras                        as JSON
import qualified Data.ByteArray                           as BA
import qualified Data.ByteString                          as BSS
import qualified Data.ByteString.Char8                    as BS8
import           Data.Maybe                               (isJust)
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.Proxy                               (Proxy(Proxy))
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Data.Swagger.Internal.Schema             (ToSchema(declareNamedSchema), plain, paramSchemaToSchema)
import           Language.PlutusTx.Lift                   (makeLift)

import           Ledger.Ada
import           Ledger.Crypto
import           Ledger.Slot
import           Ledger.Scripts
import           Ledger.Value
import qualified Ledger.Value.TH                          as V

{- Note [Serialisation and hashing]

We use cryptonite for generating hashes, which requires us to serialise values
to a strict ByteString (to implement `Data.ByteArray.ByteArrayAccess`).

Binary serialisation could be achieved via

1. The `binary` package
2. The `cbor` package

(1) is used in the cardano-sl repository, and (2) is used in the
`language-plutus-core` project in this repository.

In this module we use (2) because of the precedent. This means however that we
may generate different hashes for the same transactions compared to cardano-sl.
This might become a problem if/when we want to support "imports" of some real
blockchain state into the emulator.

However, it should be easy to change the serialisation mechanism later on,
especially because we only need one direction (to binary).

-}

instance Serialise (Digest SHA256) where
  encode = encode . BA.unpack
  decode = do
    d <- decode
    let md = digestFromByteString . BSS.pack $ d
    case md of
      Nothing -> fail "couldn't decode to Digest SHA256"
      Just v  -> pure v

instance ToJSON (Digest SHA256) where
  toJSON = JSON.String . JSON.encodeSerialise

instance ToSchema (Digest SHA256) where
  declareNamedSchema _ = plain . paramSchemaToSchema $ (Proxy :: Proxy String)

instance FromJSON (Digest SHA256) where
  parseJSON = JSON.decodeSerialise

-- | A transaction ID, using some id type.
newtype TxIdOf h = TxIdOf { getTxId :: h }
    deriving (Eq, Ord, Show)
    deriving stock (Generic)

makeLift ''TxIdOf

-- | A transaction id, using a SHA256 hash as the transaction id type.
type TxId = TxIdOf (Digest SHA256)

deriving newtype instance Serialise TxId
deriving anyclass instance ToJSON a => ToJSON (TxIdOf a)
deriving anyclass instance FromJSON a => FromJSON (TxIdOf a)
deriving anyclass instance ToSchema a => ToSchema (TxIdOf a)

-- | A payment address using some id type. This corresponds to a Bitcoin pay-to-witness-script-hash.
newtype AddressOf h = AddressOf { getAddress :: h }
    deriving (Eq, Ord, Show, Generic)

-- | A payment address using a SHA256 hash as the address id type.
type Address = AddressOf (Digest SHA256)

deriving newtype instance Serialise Address
deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address

-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs     :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txOutputs    :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txForge      :: !Value,
    -- ^ The 'Value' forged by this transaction.
    txFee        :: !Ada,
    -- ^ The fee for this transaction.
    txValidRange :: !SlotRange
    -- ^ The 'SlotRange' during which this transaction may be validated.
    } deriving (Show, Eq, Ord, Generic, Serialise, ToJSON, FromJSON)

-- | The inputs of a transaction.
inputs :: Lens' Tx (Set.Set TxIn)
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The outputs of a transaction.
outputs :: Lens' Tx [TxOut]
outputs = lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

-- | The validity range of a transaction.
validRange :: Lens' Tx SlotRange
validRange = lens g s where
    g = txValidRange
    s tx o = tx { txValidRange = o }

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs && nonNegative txForge  && txFee >= 0 where
    nonNegative i = $$(V.geq) i $$(V.zero)

-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs  :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedOutputs :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedForge   :: !Value,
    -- ^ The 'Value' forged by this transaction.
    txStrippedFee     :: !Ada
    -- ^ The fee for this transaction.
    } deriving (Show, Eq, Ord)

instance BA.ByteArrayAccess TxStripped where
    length = BA.length . BS8.pack . show
    withByteArray = BA.withByteArray . BS8.pack . show

strip :: Tx -> TxStripped
strip Tx{..} = TxStripped i txOutputs txForge txFee where
    i = Set.map txInRef txInputs

-- | Hash a stripped transaction once.
preHash :: TxStripped -> Digest SHA256
preHash = hash

-- | Double hash of a transaction, excluding its witnesses.
hashTx :: Tx -> TxId
hashTx = TxIdOf . hash . preHash . strip

-- | A reference to a transaction output, using some transaction id type. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
data TxOutRefOf h = TxOutRefOf {
    txOutRefId  :: TxIdOf h,
    txOutRefIdx :: Int -- ^ Index into the referenced transaction's outputs
    } deriving (Show, Eq, Ord, Generic)

-- | A reference to a transaction output, using a SHA256 hash.
type TxOutRef = TxOutRefOf (Digest SHA256)

deriving instance Serialise TxOutRef
deriving instance ToJSON TxOutRef
deriving instance FromJSON TxOutRef
deriving instance ToSchema TxOutRef

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRefOf txId i)
    txId = hashTx t

-- | The type of a transaction input.
data TxInType =
      ConsumeScriptAddress !ValidatorScript !RedeemerScript -- ^ A transaction input that consumes a script address with the given validator and redeemer pair.
    | ConsumePublicKeyAddress !Signature -- ^ A transaction input that consumes a public key address, with a witness that it is allowed to do so.
    deriving (Show, Eq, Ord, Generic, Serialise, ToJSON, FromJSON)

-- | A transaction input using some transaction id type, consisting of a transaction output reference and an input type.
data TxInOf h = TxInOf {
    txInRef  :: !(TxOutRefOf h),
    txInType :: !TxInType
    } deriving (Show, Eq, Ord, Generic)

-- | A transaction input, using a SHA256 hash as the transaction id type.
type TxIn = TxInOf (Digest SHA256)

deriving instance Serialise TxIn
deriving instance ToJSON TxIn
deriving instance FromJSON TxIn

-- | The 'TxOutRefOf' spent by a transaction input.
inRef :: Lens (TxInOf h) (TxInOf g) (TxOutRefOf h) (TxOutRefOf g)
inRef = lens txInRef s where
    s txi r = txi { txInRef = r }

-- | The type of a transaction input.
inType :: Lens' (TxInOf h) TxInType
inType = lens txInType s where
    s txi t = txi { txInType = t }

-- | Validator and redeemer scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxInOf h -> Maybe (ValidatorScript, RedeemerScript)
inScripts TxInOf{ txInType = t } = case t of
    ConsumeScriptAddress v r  -> Just (v, r)
    ConsumePublicKeyAddress _ -> Nothing

-- | Signature of a transaction input that spends a "pay to public key" output.
inSignature :: TxInOf h -> Maybe Signature
inSignature TxInOf{ txInType = t } = case t of
    ConsumeScriptAddress _ _  -> Nothing
    ConsumePublicKeyAddress s -> Just s

-- | A transaction input that spends a "pay to public key" output, given the witness.
pubKeyTxIn :: TxOutRefOf h -> Signature -> TxInOf h
pubKeyTxIn r = TxInOf r . ConsumePublicKeyAddress

-- | A transaction input that spends a "pay to script" output, given witnesses.
scriptTxIn :: TxOutRefOf h -> ValidatorScript -> RedeemerScript -> TxInOf h
scriptTxIn r v = TxInOf r . ConsumeScriptAddress v

instance BA.ByteArrayAccess TxIn where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The type of a transaction output.
data TxOutType =
    PayToScript !DataScript -- ^ A pay-to-script output with the given data script.
    | PayToPubKey !PubKey -- ^ A pay-to-pubkey output.
    deriving (Show, Eq, Ord, Generic, Serialise, ToJSON, FromJSON)

-- | A transaction output, using the given transaction id type, consisting of a target address,
-- a value, and an output type.
data TxOutOf h = TxOutOf {
    txOutAddress :: !(AddressOf h),
    txOutValue   :: !Value,
    txOutType    :: !TxOutType
    }
    deriving (Show, Eq, Ord, Generic)

-- | A transaction output, using a SHA256 hash as the transaction id type.
type TxOut = TxOutOf (Digest SHA256)

deriving instance Serialise TxOut
deriving instance ToJSON TxOut
deriving instance FromJSON TxOut

instance BA.ByteArrayAccess TxOut where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The data script attached to a 'TxOutOf', if there is one.
txOutData :: TxOutOf h -> Maybe DataScript
txOutData TxOutOf{txOutType = t} = case  t of
    PayToScript s -> Just s
    PayToPubKey _ -> Nothing

-- | The public key attached to a 'TxOutOf', if there is one.
txOutPubKey :: TxOutOf h -> Maybe PubKey
txOutPubKey TxOutOf{txOutType = t} = case  t of
    PayToPubKey k -> Just k
    _             -> Nothing

-- | The address of a transaction output.
outAddress :: Lens (TxOutOf h) (TxOutOf g) (AddressOf h) (AddressOf g)
outAddress = lens txOutAddress s where
    s tx a = tx { txOutAddress = a }

-- | The value of a transaction output.
-- | TODO: Compute address again
outValue :: Lens' (TxOutOf h) Value
outValue = lens txOutValue s where
    s tx v = tx { txOutValue = v }

-- | The output type of a transaction output.
-- | TODO: Compute address again
outType :: Lens' (TxOutOf h) TxOutType
outType = lens txOutType s where
    s tx d = tx { txOutType = d }

-- | Whether the output is a pay-to-pubkey output.
isPubKeyOut :: TxOutOf h -> Bool
isPubKeyOut = isJust . txOutPubKey

-- | Whether the output is a pay-to-script output.
isPayToScriptOut :: TxOutOf h -> Bool
isPayToScriptOut = isJust . txOutData

-- | The address that should be targeted by a transaction output locked by the given public key.
pubKeyAddress :: PubKey -> AddressOf (Digest SHA256)
pubKeyAddress pk = AddressOf $ hash h where
    h :: Digest SHA256 = hash $ Write.toStrictByteString e
    e = encode pk

-- | The address that should be used by a transaction output locked by the given validator script.
scriptAddress :: ValidatorScript -> AddressOf (Digest SHA256)
scriptAddress vl = AddressOf $ hash h where
    h :: Digest SHA256 = hash $ Write.toStrictByteString e
    e = encode vl

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> ValidatorScript -> DataScript -> TxOut
scriptTxOut v vl ds = TxOutOf a v tp where
    a = scriptAddress vl
    tp = PayToScript ds

-- | Create a transaction output locked by a public key.
pubKeyTxOut :: Value -> PubKey -> TxOut
pubKeyTxOut v pk = TxOutOf a v tp where
    a = pubKeyAddress pk
    tp = PayToPubKey pk

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRefOf (hashTx t) idx, o)

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> Set.Set TxOutRef
spentOutputs = Set.map txInRef . txInputs

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo t unspent = (unspent `Map.difference` lift' (spentOutputs t)) `Map.union` outs where
    lift' = Map.fromSet (const ())
    outs = unspentOutputsTx t