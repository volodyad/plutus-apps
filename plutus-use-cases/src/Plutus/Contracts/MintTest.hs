{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.MintTest where

import Control.Lens (makeClassyPrisms, review, view)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (Datum (..), DatumHash, POSIXTime, PubKeyHash, TxId, ValidatorHash, getCardanoTxId, interval,
               scriptOutputsAt, txSignedBy, valuePaidTo)
import Ledger qualified
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (ScriptContext (..), ScriptPurpose (..), TxInfo (..))
import Ledger.Interval (after, before, from)
import Ledger.Interval qualified as Interval
import Ledger.Scripts qualified as LedgerScripts
import Ledger.Tx qualified as LedgerScripts
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (AssetClass (..), CurrencySymbol, TokenName (..), Value, assetClassValue, geq, lt, symbols, valueOf)
import Ledger.Value qualified as Value

import Plutus.Contract
import Plutus.Contract.Typed.Tx qualified as Typed
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Ledger (Validator (..), mintingPolicyHash, ownCurrencySymbol, unMintingPolicyScript)
import Prelude (Semigroup (..), foldMap)
import Prelude qualified as Haskell
data MintRequest = MintRequest
    {} deriving ( Haskell.Show,  Haskell.Eq, Haskell.Ord)

PlutusTx.unstableMakeIsData ''MintRequest
PlutusTx.makeLift ''MintRequest

{-# INLINABLE tokenName #-}
tokenName :: TokenName
tokenName = TokenName "mytoken"

{-# INLINABLE mintPolicy #-}
mintPolicy :: MintRequest -> () -> ScriptContext -> Bool
mintPolicy request r ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} =
    traceIfFalse "Should forge one token" (forgedCount == 1)
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedSymbolsCount = length $ symbols forged
        forgedCount = valueOf forged ownSymbol tokenName


tokenPolicy :: MintRequest -> LedgerScripts.MintingPolicy
tokenPolicy req = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \req -> Scripts.wrapMintingPolicy (mintPolicy req) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode req

mintPolicyHash :: MintRequest -> LedgerScripts.MintingPolicyHash
mintPolicyHash = mintingPolicyHash . tokenPolicy

tokenSymbol :: MintRequest -> CurrencySymbol
tokenSymbol = Value.mpsSymbol . mintPolicyHash

mintTokenClass :: MintRequest -> AssetClass
mintTokenClass request = AssetClass (tokenSymbol request, tokenName)

mintTokenValue:: MintRequest -> Value
mintTokenValue request = assetClassValue (mintTokenClass request) 1

mintingPlutusScript :: MintRequest -> Ledger.Script
mintingPlutusScript =
  unMintingPolicyScript . tokenPolicy

mintingValidator :: MintRequest -> Validator
mintingValidator =  Validator . mintingPlutusScript


--script
data TestParams = TestParams
    {} deriving (Haskell.Show, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''TestParams

data TestDatum = TestDatum
    {}
    deriving (Haskell.Show, Haskell.Eq)

PlutusTx.makeIsDataIndexed ''TestDatum [('TestDatum, 0)]
PlutusTx.makeLift ''TestDatum

data TestRedeemer = TestRedeemer
    deriving Haskell.Show
PlutusTx.makeIsDataIndexed ''TestRedeemer [('TestRedeemer, 0)]
PlutusTx.makeLift ''TestRedeemer

data TestType
instance Scripts.ValidatorTypes TestType where
    type instance DatumType TestType = TestDatum
    type instance RedeemerType TestType = TestRedeemer


typedTestValidator :: TestParams -> Scripts.TypedValidator TestType
typedTestValidator params = Scripts.mkTypedValidator @TestType
    ($$(PlutusTx.compile [|| mkTestValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TestDatum @TestRedeemer

{-# INLINABLE mkTestValidator #-}
mkTestValidator :: TestParams -> TestDatum -> TestRedeemer -> ScriptContext -> Bool
mkTestValidator params datum r ctx = True

testValidator :: TestParams -> Validator
testValidator = Scripts.validatorScript . typedTestValidator

testValidatorHash :: TestParams -> Ledger.ValidatorHash
testValidatorHash params = LedgerScripts.validatorHash . testValidator $ params

testAddress :: TestParams -> Ledger.Address
testAddress = Ledger.scriptAddress . testValidator
