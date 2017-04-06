{-# LANGUAGE  ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ModbusTcpSpec
  ( spec
  ) where

import Data.ByteString (ByteString, pack)
import Data.DeriveTH
import Data.Modbus
import Data.Modbus.Tcp
import Data.Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  describe "ModTcpRequestFrame" $ do
    it "should serialize properly" $
      (encode <$> allReqFrames) `shouldBe` allReqFramesEncoded
    it "should deserialize to the original frame" $
      (decode <$> allReqFramesEncoded) `shouldBe` (Right <$> allReqFrames)

  describe "ModTcpResponseFrame" $ do
    it "should serialize properly" $
      (encode <$> allRespFrames) `shouldBe` allRespFramesEncoded
    it "should deserialize to the original frame" $
      (decode <$> allRespFramesEncoded) `shouldBe` (Right <$> allRespFrames)

  describe "(decode . encode) == id" $ do
    prop "ModTcpRequestFrame" $ \ (req :: ModTcpRequestFrame) -> (decode . encode) req == Right req
    prop "ModTcpResponseFrame" $ \ (res :: ModTcpResponseFrame) -> (decode . encode) res == Right (fixResponseFrame res)

fixResponseFrame :: ModTcpResponseFrame -> ModTcpResponseFrame
fixResponseFrame f@(ModTcpResponseFrame {rModResponse=r}) = f{rModResponse = go r}
  where
    go (ReadCoilsResponse bl) = extend ReadCoilsResponse bl
    go (ReadDiscreteInputsResponse bl) = extend ReadDiscreteInputsResponse bl
    go r' = r'

    extend c bl =
      let l = Prelude.length bl
          l' = ((7 + l) `div` 8) * 8
      in c (bl ++ Prelude.replicate (l' - l) False)

allReqFrames :: [ModTcpRequestFrame]
allReqFrames = ModTcpRequestFrame 0 1 2 <$> allRequests
  where
    allRequests = [ ReadCoils 1 1
                  , ReadDiscreteInputs 1 1
                  , ReadHoldingRegisters 1 1
                  , ReadInputRegisters 1 1
                  , WriteSingleCoil 1 True
                  , WriteSingleRegister 1 1
                  , WriteDiagnosticRegister 1 1
                  , WriteMultipleCoils 0 [False, False, True]
                  , WriteMultipleRegisters 0 [0x1234, 0x3412]
                  ]

allReqFramesEncoded :: [ByteString]
allReqFramesEncoded = pack <$> [ [0,0,0,1,0,6,2] ++ [1,0,1,0,1]
                               , [0,0,0,1,0,6,2] ++ [2,0,1,0,1]
                               , [0,0,0,1,0,6,2] ++ [3,0,1,0,1]
                               , [0,0,0,1,0,6,2] ++ [4,0,1,0,1]
                               , [0,0,0,1,0,6,2] ++ [5,0,1,0xff,0]
                               , [0,0,0,1,0,6,2] ++ [6,0,1,0,1]
                               , [0,0,0,1,0,6,2] ++ [8,0,1,0,1]
                               , [0,0,0,1,0,8,2] ++ [15,0,0,0,3,1,4]
                               , [0,0,0,1,0,11,2] ++ [16,0,0,0,2,4,18,52,52,18]
                               ]

allRespFrames :: [ModTcpResponseFrame]
allRespFrames = ModTcpResponseFrame 0 1 2 <$> allResponses
  where
    allResponses = [ ReadCoilsResponse [False, True, False, False, False, False, False, False]
                   , ReadDiscreteInputsResponse [False, True, False, False, False, False, False, False]
                   , ReadHoldingRegistersResponse [2]
                   , ReadInputRegistersResponse [2]
                   , WriteSingleCoilResponse 1 True
                   , WriteSingleRegisterResponse 1 1
                   , WriteDiagnosticRegisterResponse 1 1
                   , WriteMultipleCoilsResponse 1 1
                   , WriteMultipleRegistersResponse 1 1
                   , ReadHoldingRegistersException IllegalFunction
                   ]

allRespFramesEncoded :: [ByteString]
allRespFramesEncoded = pack <$> [ [0,0,0,1,0,4,2] ++ [1,1,2]
                                , [0,0,0,1,0,4,2] ++ [2,1,2]
                                , [0,0,0,1,0,5,2] ++ [3,2,0,2]
                                , [0,0,0,1,0,5,2] ++ [4,2,0,2]
                                , [0,0,0,1,0,6,2] ++ [5,0,1,0xff,0]
                                , [0,0,0,1,0,6,2] ++ [6,0,1,0,1]
                                , [0,0,0,1,0,6,2] ++ [8,0,1,0,1]
                                , [0,0,0,1,0,6,2] ++ [15,0,1,0,1]
                                , [0,0,0,1,0,6,2] ++ [16,0,1,0,1]
                                , [0,0,0,1,0,3,2] ++ [131,1]
                                ]

derive makeArbitrary ''ModRequest
derive makeArbitrary ''ModResponse
derive makeArbitrary ''ExceptionCode
derive makeArbitrary ''ModTcpRequestFrame
derive makeArbitrary ''ModTcpResponseFrame
