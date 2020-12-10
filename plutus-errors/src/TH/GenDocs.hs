{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module TH.GenDocs (genDocs) where

import Language.Haskell.TH as TH
import Numeric.Natural
import Text.Printf
import TH.GenCodes
import Data.List
import Errors

-- | Generate haddock documentation for all errors and their codes,
-- by creating type-synonyms to lifted dataconstructors using a DataKinds trick.
genDocs :: Q [TH.Dec]
genDocs = let allCodes = $(genCodes allErrors)
          in case findDuplicates allCodes of
                 [] -> pure $ fmap mkTySyn (zip allErrors allCodes)
                 -- Fail at compile time if duplicate error codes are found.
                 dups -> fail $ "ErrorCode instances have some duplicate error-code numbers: " ++ show dups

-- | An alias (type-synonym) for a given error,
-- using naming scheme "E+error-code".
mkTySyn :: (TH.Name,Natural) -> TH.Dec
mkTySyn (err, code) =
    let aliasName = mkName $ printf "E%03d" code
    in TySynD aliasName [] $ ConT err

-- | find the duplicate occurences in a list
findDuplicates :: Ord a => [a] -> [[a]]
findDuplicates xs = filter (\g -> length g >1) . group $ sort xs
