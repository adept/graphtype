module Test02 where

data Store = Store
    { storeName_    :: ByteString
    , syncCode_     :: ByteString
    , minOrderVal_  :: Float
    , delivAddress_ :: ByteString
    , remainders_   :: Remainders
    } deriving (Typeable, Show)

type Remainders = [Float]