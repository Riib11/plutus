{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Errors
import           TH.GenCodes

-- | Executable to help developers by returning a currently-unused error code
main :: IO ()
main =  putStrLn $ "An error code that is not currently in-use is: "
                 ++ show (maximum $(genCodes allErrors) + 1)
