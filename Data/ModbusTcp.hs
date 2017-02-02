module Data.ModbusTcp
  ( ModTcpRequestFrame(..)
  , ModTcpResponseFrame(..)
  ) where

import qualified Data.ByteString as BS
import           Data.Modbus
import           Data.Serialize
import           Data.Word

data ModTcpRequestFrame = ModTcpRequestFrame { qTransactionId :: Word16
                                             , qProtocolId :: Word16
                                             , qUnitId :: Word8
                                             , qModRequest :: ModRequest
                                             } deriving (Eq, Show)

data ModTcpResponseFrame = ModTcpResponseFrame { rTransactionId :: Word16
                                               , rProtocolId :: Word16
                                               , rUnitId :: Word8
                                               , rModResponse :: ModResponse
                                               } deriving (Eq, Show)

instance Serialize ModTcpRequestFrame where
  get = ModTcpRequestFrame <$>
    getWord16be <*>
    (getWord16be <* getWord16be) <*>
    getWord8 <*>
    get

  put (ModTcpRequestFrame tid pid uid req) = putTcp tid pid uid req

instance Serialize ModTcpResponseFrame where
  get = ModTcpResponseFrame <$>
    getWord16be <*>
    (getWord16be <* getWord16be) <*>
    getWord8 <*>
    get

  put (ModTcpResponseFrame tid pid uid res)= putTcp tid pid uid res

putTcp :: Serialize t => Word16 -> Word16 -> Word8 -> t -> PutM ()
putTcp tid pid uid body = do
    putWord16be tid
    putWord16be pid
    putWord16be len
    putWord8 uid
    putByteString resBS

      where
        resBS = encode body
        len = toEnum . fromEnum $ 1 + BS.length resBS

-- TODO: add a mkException??
