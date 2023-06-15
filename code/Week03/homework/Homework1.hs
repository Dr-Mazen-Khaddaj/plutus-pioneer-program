{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import Plutus.V2.Ledger.Api

    ( BuiltinData
    , Validator
    , mkValidatorScript
    , ScriptContext (scriptContextTxInfo)
    , TxInfo (txInfoSignatories, txInfoValidRange)
    , PubKeyHash, POSIXTime, Interval
    )

import PlutusTx

    ( compile
    , unstableMakeIsData
    )

import PlutusTx.Prelude

    ( Bool (..)
    , elem
    , otherwise
    , ($)
    , (.), traceIfFalse
    )

import Utilities

    ( wrapValidator
    )

import Plutus.V1.Ledger.Interval
    ( after
    , before
    )

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{- Data Types -}
data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }
unstableMakeIsData ''VestingDatum

{- INLINABLEs -}
{-# INLINABLE  mkVestingValidator        #-}
{-# INLINABLE  mkWrappedVestingValidator #-}

{- This should validate if
    either
        beneficiary1 has signed the transaction
        and the current slot is before or at the deadline
    or
        beneficiary2 has signed the transaction
        and the deadline has passed.
-}

mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator    vestingDatum    ()    scriptContext

    | deadlineTime `after`  txValidTimeRange   =   traceIfFalse "Only Beneficiary 1 can sign the transaction!" $ signedBy beneficiary1
    | deadlineTime `before` txValidTimeRange   =   traceIfFalse "Only Beneficiary 2 can sign the transaction!" $ signedBy beneficiary2
    | otherwise                                =   traceIfFalse "Time is Out of Bound!" False

    where
        signatories      :: [PubKeyHash]
        signedBy         :: (VestingDatum -> PubKeyHash) -> Bool
        txValidTimeRange :: Interval POSIXTime
        deadlineTime     :: POSIXTime

        signatories         =   txInfoSignatories $ scriptContextTxInfo scriptContext
        signedBy            =   (`elem` signatories) . ($ vestingDatum)
        txValidTimeRange    =   txInfoValidRange $ scriptContextTxInfo scriptContext
        deadlineTime        =   deadline vestingDatum

mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

{- Validator -}
validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])