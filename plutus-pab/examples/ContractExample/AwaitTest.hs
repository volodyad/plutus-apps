{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
module ContractExample.AwaitTest(runCreateUtxo, runWaitUtxo) where

import Control.Lens (view)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (either)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints (adjustUnbalancedTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Datum (..), Redeemer (..), unitRedeemer)
import Ledger.Tx
import Ledger.Tx (ChainIndexTxOut (..), getCardanoTxId)
import Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Plutus.Contract
import Plutus.Contract as Contract
import Plutus.Contracts.AwaitTest
import Plutus.V1.Ledger.Tx (TxOutRef (..))
import PlutusTx qualified as PlutusTx
import Prelude as Haskell
import Text.Printf (printf)

runCreateUtxo :: Contract () EmptySchema Text ()
runCreateUtxo = runError runCreateUtxo' >>= \case
    Left err -> logWarn @Haskell.String (show err)
    Right () -> pure ()

runCreateUtxo' :: Contract () EmptySchema Text ()
runCreateUtxo' = do
    let params = TestParams
    pkh <- Contract.ownPaymentPubKeyHash
    logInfo @Haskell.String "********Start create utxo."
    let inst = typedTestValidator params
        mrScript = testValidator params
        address = testAddress params
        mintRequestParams = MintRequest
        tokenMintingPolicy = tokenPolicy mintRequestParams
        forgedToken = mintTokenValue mintRequestParams
        datum = TestDatum
        mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ ()
    let lookups =
                Constraints.typedValidatorLookups inst
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy
        tx      = Constraints.mustPayToTheScript datum (forgedToken <> (Ada.toValue Ledger.minAdaTxOut))
                <> Constraints.mustMintValueWithRedeemer mintRedeemer forgedToken

    mkTxConstraints @TestType lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @Haskell.String "*******Create utxo completed."

awaitNextUtxo:: Promise () EmptySchema Text ()
awaitNextUtxo = do
    let params = TestParams
    let address = testAddress params
    promiseBind (utxoIsProduced address) $ \_ -> do
                        logInfo @String $ printf "******Await Utxo created success*****"

runWaitUtxo :: Contract () EmptySchema Text ()
runWaitUtxo = forever $ selectList [awaitNextUtxo]

findUtxo::
    forall w s. TestParams

    -> Contract w s Text (TxOutRef, ChainIndexTxOut, TestDatum)
findUtxo params = do
        let addr = testAddress params
            tokenClass = mintTokenClass $ MintRequest
        utxos <- utxosAt addr
        go  [x | x@(_, o) <- Map.toList utxos, assetClassValueOf (view ciTxOutValue o) tokenClass == 1]
    where
        go [(oref, o)] = do
            logInfo @String $ printf "getDatum"
            d <- getTheDatum o
            return (oref, o, d)
        go _ = do
            logInfo @String $ printf "instance not found"
            throwError "instance not found"


getTheDatum :: ChainIndexTxOut -> Contract w s Text TestDatum
getTheDatum o =
    case o of
        PublicKeyChainIndexTxOut {} ->
            throwError "no datum for a txout of a public key address"
        ScriptChainIndexTxOut { _ciTxOutDatum } -> do
            (Datum e) <- either getDatum pure _ciTxOutDatum
            maybe (throwError "datum hash wrong type")
                pure
                (PlutusTx.fromBuiltinData e)
    where
        getDatum :: Ledger.DatumHash -> Contract w s Text Datum
        getDatum dh =
            datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                       Just d  -> pure d
