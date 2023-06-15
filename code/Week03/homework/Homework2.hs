{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api   ( BuiltinData
                                        , Validator, mkValidatorScript
                                        , ScriptContext (scriptContextTxInfo)
                                        , TxInfo (txInfoSignatories, txInfoValidRange)
                                        , PubKeyHash, POSIXTime
                                        , Interval (ivFrom)
                                        , LowerBound, lowerBound
                                        )
import           PlutusTx               ( applyCode
                                        , compile
                                        , liftCode
                                        )
import           PlutusTx.Prelude       ( Bool, (&&)
                                        , Ord ((>=)), elem
                                        , ($), (.)
                                        )
import           Utilities              ( wrapValidator
                                        )

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{- INLINABLEs -}
{-# INLINABLE  mkParameterizedVestingValidator          #-}
{-# INLINABLE  mkWrappedParameterizedVestingValidator   #-}

{- This should validate if
    the transaction has a signature from the parameterized beneficiary
    and the deadline has passed.
-}

mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () scriptContext = txStartingTime >= deadlineTime && signedByBeneficiary 
    where
        signatories   ::  [PubKeyHash]
        signatories   =   txInfoSignatories $ scriptContextTxInfo scriptContext

        signedByBeneficiary :: Bool
        signedByBeneficiary = beneficiary `elem` signatories

        txStartingTime, deadlineTime :: LowerBound POSIXTime
        txStartingTime   = ivFrom $ txInfoValidRange $ scriptContextTxInfo scriptContext
        deadlineTime     = lowerBound deadline


mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
