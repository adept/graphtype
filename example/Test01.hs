{-# LANGUAGE DeriveDataTypeable #-}
module Test01 where

import Data.ByteString
import Data.Map
import Data.Word
import Data.Typeable

data Organization = Organization
    { orgName_       :: ByteString
    , physAddress_   :: ByteString
    , legalAddress_  :: ByteString
    , details_       :: ByteString
    , phone_         :: ByteString
    , contactPerson_ :: ByteString
    , bank_          :: ByteString
    , isVendor_      :: Bool
    , importMode_    :: ImportMode
    , stores_        :: Map StoreId Store
    , costs_         :: Costs
    , edrpou_        :: Word64
    , taxNum_        :: Word64
    , certNum_       :: Word64
    , transAccount_  :: Word64
    , mfo_           :: Word64
    } deriving Typeable


data ImportMode = ImportMode
type StoreId = Int
newtype Costs = Costs [Integer]
