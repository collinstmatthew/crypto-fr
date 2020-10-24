-- module for implementing back testing functionality
{-# LANGUAGE ScopedTypeVariables #-}
module BackTest (
                 BackTestConf(..)
                ) where

-- contains all the important information for backtesting
data BackTestConf = BackTestConf { val :: Bool}


