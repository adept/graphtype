{-# LANGUAGE DeriveDataTypeable #-}
module Test01 where

import Data.ByteString
import Data.Map
import Data.Word
import Data.Typeable
import Test02

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
    } | SomeOtherShit String deriving Typeable


data Store = Store
    { storeName_    :: ByteString
    , syncCode_     :: ByteString
    , minOrderVal_  :: Float
    , delivAddress_ :: ByteString
    , remainders_   :: Remainders
    } deriving (Typeable, Show)

data Remainders = Remainders
    { prodData      :: ProdData
    , quantityData  :: QuantityData
    } deriving (Typeable, Show)

data ProdInfo = ProdInfo
    { bufferInfo    :: BufferInfo
    , greenRevision :: Revision
    , redRevision   :: Revision
    , packing       :: Packing
    , factor        :: Factor
    , localProdName :: ByteString
    , localArticle  :: Article
    , reason        :: Reason
    , updPeriod     :: Revision
    , display       :: Quantity
    , reliability   :: Float
    } deriving (Typeable, Show)

type Sessions     = Map SessionKey SessionData
type QuantityArr  = Map Day QuantityCell
type Quantity     = Word32
type Mass         = Word32
type Volume       = Float
type Revision     = Word8
type Column       = Word8
type Article      = Word16
type SessionKey   = Word64
type UserName     = ByteString
type Users        = Map UserName UserInfo
type ProdData     = Map Article ProdInfo
type QuantityData = Map Article QuantityArr
type ProdId       = Article
type ProdName     = ByteString
type Date         = Day
type UpdTrig      = Bool
type PrimeCost    = Float
type ImportMode   = Either TableImport XMLImport
type XMLImport    = ()
type Cost         = Float
