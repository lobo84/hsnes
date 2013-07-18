import Data.Bits
import Control.Monad.Error
import Data.List
import Data.Maybe
import Data.Word
import Test.QuickCheck


data Cpu = Cpu {
  memory :: Memory,
  registers :: Registers  
} deriving Show

type RegValue = Word8

data Flag = Carry
          |Zero
          | IrqDis
          | DecMode
          | BrkCommand
          | Unused
          | OverFlow
          | Neg
            
data Registers = Registers {            
  pc :: RegValue,
  status :: RegValue,
  acc :: RegValue
} deriving Show

type Address = Word8     
type OpCode = Word8       
type Memory = [(Address,Word8)]

flagPos :: Flag -> Int
flagPos Carry = 0
flagPos Zero = 1
flagPos IrqDis = 2
flagPos DecMode = 3
flagPos BrkCommand = 4
flagPos Unused = 5
flagPos OverFlow = 6
flagPos Neg = 7


updateFlag :: Flag -> Bool -> RegValue -> RegValue
updateFlag flag True regValue = setBit regValue (flagPos flag)
updateFlag flag False regValue = clearBit regValue (flagPos flag)

clc :: Cpu -> Cpu
clc = updateFlagOp Carry False 
cld = updateFlagOp DecMode False
cli = updateFlagOp IrqDis False
clv = updateFlagOp OverFlow False
sec = updateFlagOp Carry True
sed = updateFlagOp DecMode True
sei = updateFlagOp IrqDis True

immediateArg :: RegValue -> Memory -> Word8
immediateArg pcValue mem = 

adcImmediate :: Cpu -> Cpu
adcImmediate (Cpu mem regs) = Cpu mem (acc(regs)+(immediateArg (pc(regs)) mem))

updateFlagOp :: Flag -> Bool -> Cpu -> Cpu
updateFlagOp flag value (Cpu mem regs) = Cpu mem (regs {pc = pc(regs)+1, 
                                                        status = updateFlag flag value (status(regs)) 
                                                       })


opCodeToFunc :: OpCode -> (Cpu -> Cpu)
opCodeToFunc 0x18 = clc
opCodeToFunc 0xD8 = cld
opCodeToFunc 0x58 = cli
opCodeToFunc 0xB8 = clv
opCodeToFunc 0x38 = sec
opCodeToFunc 0xF8 = sed
opCodeToFunc 0x78 = sei



prop_clc cpu = testBit (status(registers (clc cpu))) 0 == False



instance Arbitrary Cpu where
    arbitrary = do
      regs <- arbitrary
      mem <- arbitrary
      return (Cpu mem regs)

instance Arbitrary Registers where
    arbitrary = do
      acc <- arbitrary
      status <- arbitrary
      pc <- arbitrary
      return (Registers pc status acc)

  

      
        




