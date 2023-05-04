{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework2 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (unstableMakeIsData, compile)
import           PlutusTx.Prelude     (Bool (..), BuiltinData, (++))
import           Prelude              ((/=), IO, FilePath)
import           Utilities            (wrapValidator, writeDataToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ r _ = (/=) (flag1 r) (flag2 r)

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

writeMyRedeemer :: FilePath -> IO ()
writeMyRedeemer filePath = do
    let wrappedDoubleTrue    =  MyRedeemer  True   True
    let wrappedDoubleFalse   =  MyRedeemer  False  False
    let wrappedTrueFalse     =  MyRedeemer  True   False
    let wrappedFalseTrue     =  MyRedeemer  False  True
    writeDataToFile ( filePath ++ "wrappedDoubleTrue.json"  )  wrappedDoubleTrue
    writeDataToFile ( filePath ++ "wrappedDoubleFalse.json" )  wrappedDoubleFalse
    writeDataToFile ( filePath ++ "wrappedTrueFalse.json"   )  wrappedTrueFalse
    writeDataToFile ( filePath ++ "wrappedFalseTrue.json"   )  wrappedFalseTrue