{-# LANGUAGE DeriveDataTypeable #-}
module Test02 where

import Data.ByteString
import Data.Typeable
import Data.Map
import Data.Word

data Buffer = Buffer
    { buffer    :: Quantity
    , greenPart :: Double
    , redPart   :: Double
    } deriving (Typeable, Show)

data QuantityCell = QuantityCell
    { increase  :: Increase
    , decrease  :: Decrease
    , remainder :: Remainder
    , updateT   :: UpdTrig
    } deriving (Typeable, Show)

type Costs        = Map Article (Cost, PrimeCost)
type BufferInfo   = Map Day Buffer
type Reason       = ByteString
type ProdGroup    = Word16
type Barcode      = Word64
