{-# LANGUAGE DeriveDataTypeable #-}
module Test02 where

import Data.ByteString
import Data.Typeable

data Store = Store
    { storeName_    :: ByteString
    , syncCode_     :: ByteString
    , minOrderVal_  :: Float
    , delivAddress_ :: ByteString
    , remainders_   :: Remainders
    } deriving (Typeable, Show)

type Remainders = [Float]
