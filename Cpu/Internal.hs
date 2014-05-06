module Cpu.Internal(
    Cpu,  
    registers,
    Registers,
    RegisterType(X),    
    stepCpu,
    initCpu,
    runCpu,
    runCpuInteractive,
    immediateArg,
    absoluteArg,
    absoluteArgPtr,    
    status,
    acc,
    pc,
    x,
    y,
    sp,
    absoluteXargPtr,
    updateRegister,
    showCpu,
    resetVector,
    isDead,
    isDeadAtExec,
    cyc
) where
import Debug.Trace
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Data.Int
import Numeric(showHex)
import Mem

data Cpu = Cpu {
  memory :: Memory Int Int,
  registers :: Registers,
  cyc :: Int
} deriving Show

type RegValue = Int
type Cyc = Int

data Flag = Carry
          | Zero
          | IrqDis
          | DecMode
          | BrkCommand   -- exists only on stack
          | Bit5         -- exists only on stack
          | OverFlow
          | Neg

data Registers = Registers {            
  pc :: RegValue,
  status :: RegValue,
  acc :: RegValue,
  x :: RegValue,
  y :: RegValue,
  sp :: RegValue
} 

instance Show Registers where
  show (Registers pc status acc x y sp) = 
    "{" ++ 
    "pc=" ++ h pc ++
    " status=" ++ h status ++
    " acc=" ++ h acc ++    
    " x=" ++ h x ++    
    " y=" ++ h y ++    
    " sp=" ++ h sp ++        
    "}"
    where h val = showHex val "h"

data RegisterType = Pc | Status | Acc | X | Y | Sp deriving (Eq, Show)

type OpCode = Int       
type AddressingMode = Cpu -> Int
type AccFuncTwoArg = RegValue -> RegValue -> RegValue
type AccFuncOneArg = RegValue -> RegValue
type OpSize = Int
type Address = Int
type MemEntry = Int
type BitPos = Int
type Condition = Cpu -> Bool
  
flagPos :: Flag -> Int
flagPos Carry = 0
flagPos Zero = 1
flagPos IrqDis = 2
flagPos DecMode = 3
flagPos BrkCommand = 4
flagPos Bit5 = 5
flagPos OverFlow = 6
flagPos Neg = 7

readFlag :: Flag -> RegValue -> Bool
readFlag flag val = testBit val (flagPos flag)

updateFlags :: [(Flag,Bool)] -> RegValue -> RegValue
updateFlags ((f,v):vs) regValue = updateFlags vs (updateFlag f v regValue)
updateFlags [] r = r

updateFlag :: Flag -> Bool -> RegValue -> RegValue
updateFlag flag True regValue = setBit regValue (flagPos flag)
updateFlag flag False regValue = clearBit regValue (flagPos flag)

resetVector :: Int
resetVector = 0xFFFC
stackStart = 0xFD
stackBase = 0x100

updateRegister :: RegisterType -> RegValue -> Registers -> Registers
updateRegister Pc value regs = regs{pc=value} 
updateRegister Status value regs = regs{status=value} 
updateRegister Acc value regs = regs{acc=value} 
updateRegister X value regs = regs{x=value} 
updateRegister Y value regs = regs{y=value} 
updateRegister Sp value regs = regs{sp=value}

updateRegisters :: [(RegisterType,RegValue)] -> Registers -> Registers
updateRegisters ((r,v):vs) regValue = updateRegisters vs (updateRegister r v regValue)
updateRegisters [] r = r

readRegister :: RegisterType -> Registers -> RegValue
readRegister Pc regs = pc(regs)
readRegister Status regs = status(regs)
readRegister Acc regs = acc(regs)
readRegister X regs = x(regs)
readRegister Y regs = y(regs)
readRegister Sp regs = sp(regs)

clc :: Cpu -> Cpu
clc = updateFlagOp Carry False 
cld = updateFlagOp DecMode False
cli = updateFlagOp IrqDis False
clv = updateFlagOp OverFlow False
sec = updateFlagOp Carry True
sed = updateFlagOp DecMode True
sei = updateFlagOp IrqDis True

addWillCarry :: Int -> Int -> Bool -> Bool
addWillCarry a v c = s > 255
  where s = a + v + cInt
        cInt = if c then 1 else 0

addWillOverflow :: Int -> Int -> Bool -> Bool
addWillOverflow a v c = val == 0x80
  where val = and (and (xor a s) (xor v s)) 0x80
        s = a + v + cInt
        cInt = if c then 1 else 0
        and = (Data.Bits..&.)

subWillCarry :: Int -> Int -> Bool -> Bool
subWillCarry a v c = newCarry
  where s = a - v - (1 - cInt)
        cInt = if c then 1 else 0
        newCarry = if (s < 0) then True else False

subWillOverflow :: Int -> Int -> Bool -> Bool
subWillOverflow a v c = val == 0x80
  where val = and (and (xor a s) (xor (255 - v) s)) 0x80
        s = a - v - (1 - cInt)
        cInt = if c then 1 else 0
        and = (Data.Bits..&.)

fstArg :: Cpu -> Int
fstArg cpu = readMem (pc(registers cpu)+1) (memory cpu)

secArg :: Cpu -> Int
secArg cpu = readMem (pc(regs)+2) mem
  where mem = memory cpu
        regs = registers cpu

relativeArg :: Cpu -> Int
relativeArg cpu = fromIntegral(fromIntegral((fstArg cpu))::Int8)::Int
  where mem = memory cpu
        regs = registers cpu

immediateArg :: Cpu -> Int
immediateArg = fstArg

absoluteArgPtr :: Cpu -> Int
absoluteArgPtr cpu = readMem address mem
  where mem = memory cpu
        address = (absoluteArg cpu)
        --regs = registers cpu
        --temp = readMem 0x0180 mem
        
absoluteArg :: Cpu -> Int
absoluteArg cpu = args16Address cpu
--  where mem = memory cpu
--        regs = registers cpu
        
args16Address :: Cpu -> Address
args16Address cpu = toAddress (secArg(cpu)) (fstArg(cpu))

args8Address :: Cpu -> Address
args8Address cpu = toAddress (fstArg(cpu)) (secArg(cpu))

absoluteXargPtr :: Cpu -> Int
absoluteXargPtr cpu = absoluteRegArgPtr (x(regs)) (args16Address cpu) cpu
  where mem = memory cpu
        regs = registers cpu

absoluteYargPtr :: Cpu -> Int
absoluteYargPtr cpu = absoluteRegArgPtr (y(regs)) (args16Address cpu) cpu
  where mem = memory cpu
        regs = registers cpu

add :: Int -> Int -> Int
add a b = (fromIntegral(a)::Int) + b

absoluteRegArgPtr :: RegValue -> Address -> Cpu -> Int
absoluteRegArgPtr regValue address cpu = readMem (add regValue address) mem
  where mem = memory cpu
        regs = registers cpu

zeroPageArg :: Cpu -> Int
zeroPageArg cpu = absoluteRegArgPtr 0 (args8Address cpu) cpu

zeroPageXArg :: Cpu -> Int
zeroPageXArg cpu = absoluteRegArgPtr (x(regs)) (args8Address cpu) cpu
  where mem = memory cpu
        regs = registers cpu

zeroPageYArg :: Cpu -> Int
zeroPageYArg cpu = absoluteRegArgPtr (y(regs)) (args8Address cpu) cpu
  where mem = memory cpu
        regs = registers cpu

accumulatorArg :: Cpu -> Int
accumulatorArg cpu = acc(regs)
  where mem = memory cpu
        regs = registers cpu

indirectXarg :: Cpu -> Int
indirectXarg cpu = error "inderectXarg is not implemented"

indirectYarg :: Cpu -> Int
indirectYarg cpu = error "indirectYarg is not implemented"

toAddress :: Int -> Int -> Int
toAddress a b = (Data.Bits..|.) (Data.Bits.shiftL a 8) b

fromAddress :: Int -> (Int,Int)
fromAddress value = (shiftR ((Data.Bits..&.) 0xFF00 value) 8,(Data.Bits..&.) 0x00FF value)
        
add8 :: Int -> Int -> Int -> Int
add8 a v c = mod (a + v + c) 256

add16 :: Int -> Int -> Int
add16 a b = mod (a + b) 65536

sub8 :: Int -> Int -> Int
sub8 a b = mod (a - b) 256

sub8c :: Int -> Int -> Int -> Int
sub8c a v c = mod (a - v - (1-c)) 256

updateStatusFlagsNumericOp :: RegValue -> RegValue -> RegValue
updateStatusFlagsNumericOp currentStatus newValue = newStatus
  where newStatus = updateFlags [(Zero,zeroFlag),
                                 (Neg,negFlag)] currentStatus
        zeroFlag = newValue == 0
        negFlag = testBit newValue 7

adcOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
adcOp f size c cpu = Cpu mem newRegs newC
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = add8 acc1 acc2 oldCarryInt
        oldCarryInt = if oldCarryFlag then 1 else 0
        oldCarryFlag = readFlag Carry (status regs)
        acc1 = acc(regs)
        acc2 = f cpu
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc        
        newStatus = updateFlags [(Carry,carryFlag), (OverFlow,overflFlag)] newAccStatus
        carryFlag = addWillCarry acc1 acc2 oldCarryFlag
        overflFlag = addWillOverflow acc1 acc2 oldCarryFlag
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

sbcOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
sbcOp f size c cpu = Cpu mem newRegs newC
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = sub8c acc1 acc2 oldCarryInt
        oldCarryInt = if oldCarryFlag then 1 else 0
        oldCarryFlag = readFlag Carry (status regs)
        acc1 = acc(regs)
        acc2 = f cpu
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc        
        newStatus = updateFlags [(Carry,not carryFlag), (OverFlow,overflFlag)] newAccStatus
        carryFlag = subWillCarry acc1 acc2 oldCarryFlag
        overflFlag = subWillOverflow acc1 acc2 oldCarryFlag
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

ldOp ::  RegisterType -> AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
ldOp rType f size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(rType,newR), (Pc,newPc), (Status,newStatus)] regs
        newR = f cpu
        newPc = pc(regs) + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newR
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

stOp ::  RegisterType -> AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
stOp rType f size c cpu = Cpu newMem newRegs newC
  where newRegs = updateRegister Pc newPc regs
        newPc = pc(regs) + size
        memAddress = f cpu
        newMem = writeMem memAddress (readRegister rType regs) mem
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

bitOp :: AccFuncTwoArg -> AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
bitOp aluOp f size c cpu = Cpu mem newRegs newC
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = (aluOp) acc1 acc2 
        acc1 = acc(regs)
        acc2 = f cpu
        newPc = pc(regs) + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

shiftOp :: AccFuncOneArg -> BitPos -> AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
shiftOp aluOp bitPos f size c cpu = Cpu mem newRegs newC
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = mod newAccVal 256
        newAccVal = (aluOp (f cpu))
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc        
        newStatus = updateFlag Carry carryFlag newAccStatus
        carryFlag = testBit (acc(regs)) bitPos
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c


transferOp :: RegisterType -> RegisterType -> OpSize -> Cyc -> Cpu -> Cpu
transferOp from to size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(to, fromValue), 
                                (Pc,pc(regs) + size),
                                (Status, newStatus)] regs
        fromValue = readRegister from regs
        newStatus = if to == Sp
                    then status(regs) -- no change if TXS
                    else updateStatusFlagsNumericOp (status(regs)) fromValue
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c
        
andOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
andOp = bitOp (Data.Bits..&.)

eorOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
eorOp = bitOp xor

oraOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
oraOp = bitOp (.|.)

aslOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
aslOp = shiftOp ((flip shiftL) 1) 7

lsrOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
lsrOp = shiftOp ((flip shiftR) 1) 0

rolOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rolOp f size cyc cpu = shiftOp ((flip rotateL8) (readFlag Carry (status(registers(cpu))))) 7 f size cyc cpu
--rolOp f size cyc cpu = (shiftOp (rotateL8)) 7 f size cyc cpu

rorOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rorOp f size cyc cpu = shiftOp ((flip rotateR8) (readFlag Carry (status(registers(cpu))))) 0 f size cyc cpu
--rorOp = shiftOp ((flip rotateR8) True) 0

rotateR8 :: Int -> Bool -> Int
rotateR8 value oldCarry = newValue
  where newValShift = mod (shiftR value 1) 256
        copyBitFunc = if oldCarry then setBit else clearBit
        newValue = (copyBitFunc) newValShift 7

rotateL8 :: Int -> Bool -> Int
rotateL8 value oldCarry = newValue
  where newValShift = mod (shiftL value 1) 256
        copyBitFunc = if oldCarry then setBit else clearBit
        newValue = (copyBitFunc) newValShift 0

pushOp :: RegisterType -> OpSize -> Cyc -> Cpu -> Cpu
pushOp regType size c cpu = push pushValue newC (Cpu mem newRegs newC)
  where regValue = readRegister regType newRegs
        pushValue = if regType == Status 
                    then updateFlags [(BrkCommand,True),(Bit5,True)] regValue
                    else regValue
        mem = memory cpu
        regs = registers cpu
        newPc = (pc regs) + size
        newRegs = updateRegister Pc newPc regs
        newC = (cyc cpu) + c
        
pullOp :: RegisterType -> OpSize -> Cyc -> Cpu -> Cpu
pullOp regType size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(Status, newStatus),
                                   (Pc, pc(regs)+size)] pulledRegs
        newStatus = if regType == Status 
                    then pullMergeReg oldReg pulledReg
                    else updateStatusFlagsNumericOp (status(regs)) pulledReg
        oldReg     = readRegister regType regs
        pulledReg  = readRegister regType pulledRegs
        pulledRegs = registers (pull regType cpu)
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) +c

pullMergeReg oldReg pulledReg = or (and oldReg 0x30) (and pulledReg 0xCF)
  where 
    and = (Data.Bits..&.)
    or = (.|.)

--(trace ("JSR " ++ (showHex returnPoint "")) Cpu)
jsrOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu                         
jsrOp f size c cpu = Cpu newMem newRegs newC
  where returnPoint = pc(regs) + size - 1
        pushed = pushAddr (returnPoint) cpu
        newMem = memory(pushed)
        newRegs = updateRegister Pc jumpTo (registers(pushed))
        jumpTo = f cpu
        --mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

rtsOp :: Cyc -> Cpu -> Cpu
rtsOp c cpu = Cpu mem newRegs newC
  where mem = memory cpu
        newC = (cyc cpu) + c
        newRegs =  updateRegister Pc (pc(pulledRegs)+1) pulledRegs
          where pulledRegs = registers(pullPc cpu)
        
rtiOp :: Cyc -> Cpu -> Cpu
rtiOp c cpu = Cpu mem newRegs newC
  where mem = memory cpu
        newC = (cyc cpu) + c
        statusCpu = pull Status cpu
        oldRegs = registers cpu
        oldReg = readRegister Status oldRegs
        newReg = readRegister Status (registers statusCpu)
        newStatus = (pullMergeReg oldReg newReg)
        newRegs =  updateRegisters [(Pc, (pc(pulledRegs))), (Status, newStatus)] pulledRegs
          where
              pulledRegs = registers(pullPc statusCpu)
        
        -- (trace ("PULL " ++ (showHex stackValue "") ++ " FROM " ++ (showHex spValue "")) Cpu)
pull :: RegisterType -> Cpu -> Cpu
pull regType cpu = Cpu mem newRegs 0
  where stackValue = readMem (newSp + stackBase) mem
        newRegs = updateRegisters [(regType,stackValue),
                                   (Sp, newSp)] regs
        spValue = sp(regs)
        newSp = mod (spValue + 1) 256
        mem = memory cpu
        regs = registers cpu
                  
                              
pullPc :: Cpu -> Cpu        
pullPc cpu = Cpu mem newRegs 0
  where newRegs = updateRegisters [(Pc,stackValue),
                                   (Sp, spValueHigh)] regs
        stackValue = toAddress high low
        spValueLow = mod (sp(regs) +1) 256
        spValueHigh = mod (spValueLow + 1) 256
        --newSp = mod (spValueLow + 1) 256
        low = readMem (spValueLow + stackBase) mem
        high = readMem (spValueHigh + stackBase) mem
        mem = memory cpu
        regs = registers cpu
                         

pushAddr :: Int -> Cpu -> Cpu
pushAddr addr cpu = push addrLow 0 (push addrHigh 0 cpu)
  where addrHigh = fst(fromAddress(addr))
        addrLow = snd(fromAddress(addr))

  -- (trace ("PUSH " ++ (showHex value "") ++ " TO " ++ (showHex spValue "")) Cpu)
push :: Int -> Cyc -> Cpu -> Cpu                                     
push value c cpu = Cpu newMem newRegs c
  where newMem = writeMem (spValue + stackBase) value mem
        spValue = sp(regs)
        newRegs = updateRegister Sp newSp regs
        newSp = mod (spValue - 1) 256
        mem = memory cpu
        regs = registers cpu
        

  
updateFlagOp :: Flag -> Bool -> Cpu -> Cpu
updateFlagOp flag value cpu = Cpu (memory(cpu)) newRegs newC
  where regs = registers(cpu) 
        newRegs = (regs {pc = pc(regs)+1, status = statusValue})
        statusValue = updateFlag flag value (status(regs))         
        newC = (cyc cpu) + 2
        
incOp :: RegisterType -> Int -> Int -> Cyc -> Cpu -> Cpu        
incOp regType value size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(regType, newRegValue),
                                   (Pc,pc(regs)+size),
                                   (Status,newStatus)] regs
        newRegValue = mod (oldRegValue + value) 256 
        oldRegValue = readRegister regType regs
        newStatus = updateStatusFlagsNumericOp (status(regs)) newRegValue
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

cmpOp :: AddressingMode -> RegisterType -> OpSize -> Cyc -> Cpu -> Cpu
cmpOp f regType size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(Status,newStatus),
                                   (Pc,pc(regs)+size)] regs 
        regValue = readRegister regType regs
        carry = regValue >= memValue
        zero = regValue == memValue
        negative = testBit (sub8 regValue memValue) 7 
        memValue = f cpu
        newStatus = updateFlags [(Carry,carry),
                                 (Neg,negative),
                                 (Zero,zero)] (status(regs))
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

bneOp :: AddressingMode -> OpSize -> Cpu -> Cpu
bneOp f size cpu = Cpu mem newRegs 0
  where newRegs = updateRegister Pc newPc regs
        newPc = if zeroClear then newPcValue else pc(regs)+size
        newPcValue = add16 (pc(regs)) (f cpu) 
        zeroClear = not (readFlag Zero (status(regs)))
        mem = memory cpu
        regs = registers cpu


isZeroState :: Condition
isZeroState cpu = readFlag Zero (status(regs))
  where regs = registers cpu

isPositiveState :: Condition
isPositiveState cpu = readFlag Neg (status regs)
  where regs = registers cpu


isOverflowState :: Condition
isOverflowState cpu = readFlag OverFlow (status(regs))
  where regs = registers cpu

isCarryState :: Condition
isCarryState cpu = readFlag Carry (status(regs))
  where regs = registers cpu

branchOp :: AddressingMode -> Condition -> OpSize -> Cpu -> Cpu
branchOp f c size cpu = Cpu mem newRegs newC
  where newRegs = updateRegister Pc newPcVal regs
        newPcVal = if doBranch then bPcVal else pcVal
        bPcVal = (add16 (pc(regs)) (f cpu) ) + size
        pcVal = pc(regs)+size
        doBranch = c cpu
        mem = memory cpu
        regs = registers cpu
        oldC = cyc cpu
        newC = if doBranch then oldC+3 else oldC+2

jmpOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
jmpOp f size c cpu = Cpu mem newRegs newC
 where newPcVal = f cpu
       newRegs = updateRegister Pc newPcVal regs
       mem = memory cpu
       regs = registers cpu
       newC = (cyc cpu) + c
               
nop :: OpSize -> Cpu -> Cpu
nop size cpu = cpu{memory=mem,registers=newRegs, cyc=newC}
  where newRegs = updateRegister Pc (pc(regs)+size) regs
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu)+2

bitTstOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
bitTstOp f size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(Status,newStatus),
                                 (Pc,newPc)] regs
        newPc = (pc regs) + size
        masked = (Data.Bits..&.) (acc regs) memArg
        newZero = masked == 0
        newOverflow = readFlag OverFlow memArg
        newNegative = readFlag Neg memArg
        memArg = readMem (f cpu) mem
        newStatus = updateFlags [(OverFlow,newOverflow),
                                 (Neg, newNegative),
                                 (Zero, newZero)] (status regs)
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

opCodeToFunc :: OpCode -> (Cpu -> Cpu)
opCodeToFunc 0x18 = clc
opCodeToFunc 0xD8 = cld
opCodeToFunc 0x58 = cli
opCodeToFunc 0xB8 = clv
opCodeToFunc 0x38 = sec
opCodeToFunc 0xF8 = sed
opCodeToFunc 0x78 = sei

opCodeToFunc 0x69 = adcOp immediateArg    2 2
opCodeToFunc 0x65 = adcOp zeroPageArg     2 3
opCodeToFunc 0x75 = adcOp zeroPageXArg    2 4
opCodeToFunc 0x6d = adcOp absoluteArgPtr  3 4
opCodeToFunc 0x7d = adcOp absoluteXargPtr 3 4 -- +1 
opCodeToFunc 0x79 = adcOp absoluteYargPtr 3 4 -- +1 

opCodeToFunc 0x29 = andOp immediateArg 2 2
opCodeToFunc 0x25 = andOp zeroPageArg 2 3
opCodeToFunc 0x35 = andOp zeroPageXArg 2 4
opCodeToFunc 0x2d = andOp absoluteArgPtr 3 4
opCodeToFunc 0x3d = andOp absoluteXargPtr 3 4 -- +1 
opCodeToFunc 0x39 = andOp absoluteYargPtr 3 4 -- +1

opCodeToFunc 0x49 = eorOp immediateArg 2 2
opCodeToFunc 0x45 = eorOp zeroPageArg 2 3
opCodeToFunc 0x55 = eorOp zeroPageXArg 2 4
opCodeToFunc 0x4f = eorOp absoluteArgPtr 3 4 
opCodeToFunc 0x5d = eorOp absoluteXargPtr 3 4 -- +1
opCodeToFunc 0x59 = eorOp absoluteYargPtr 3 4 -- +1

opCodeToFunc 0x09 = oraOp immediateArg 2 2
opCodeToFunc 0x05 = oraOp zeroPageArg 2 3
opCodeToFunc 0x15 = oraOp zeroPageXArg 2 4
opCodeToFunc 0x0d = oraOp absoluteArgPtr 3 4
opCodeToFunc 0x1d = oraOp absoluteXargPtr 3 4 -- +1
opCodeToFunc 0x19 = oraOp absoluteYargPtr 3 4 -- +1

opCodeToFunc 0x2a = rolOp accumulatorArg  1 2
opCodeToFunc 0x26 = rolOp zeroPageArg     2 5
opCodeToFunc 0x36 = rolOp zeroPageXArg    2 6
opCodeToFunc 0x2e = rolOp absoluteArgPtr  3 6
opCodeToFunc 0x3e = rolOp absoluteXargPtr 3 7

opCodeToFunc 0x6a = rorOp accumulatorArg  1 2
opCodeToFunc 0x66 = rorOp zeroPageArg     2 5
opCodeToFunc 0x76 = rorOp zeroPageXArg    2 6
opCodeToFunc 0x6e = rorOp absoluteArgPtr  3 6
opCodeToFunc 0x7e = rorOp absoluteXargPtr 3 7

opCodeToFunc 0x0a = aslOp accumulatorArg  1 2
opCodeToFunc 0x06 = aslOp zeroPageArg     2 5
opCodeToFunc 0x16 = aslOp zeroPageXArg    2 6
opCodeToFunc 0x0e = aslOp absoluteArgPtr  3 6
opCodeToFunc 0x1e = aslOp absoluteXargPtr 3 7

opCodeToFunc 0x4a = lsrOp accumulatorArg  1 2
opCodeToFunc 0x46 = lsrOp zeroPageArg     2 5
opCodeToFunc 0x56 = lsrOp zeroPageXArg    2 6
opCodeToFunc 0x4e = lsrOp absoluteArgPtr  3 6
opCodeToFunc 0x5e = lsrOp absoluteXargPtr 3 7

opCodeToFunc 0xa9 = ldOp Acc immediateArg 2 2
opCodeToFunc 0xa5 = ldOp Acc zeroPageArg 2 3
opCodeToFunc 0xb5 = ldOp Acc zeroPageXArg 2 4
opCodeToFunc 0xad = ldOp Acc absoluteArgPtr 3 4
opCodeToFunc 0xbd = ldOp Acc absoluteXargPtr 3 4
opCodeToFunc 0xb9 = ldOp Acc absoluteYargPtr 3 4
opCodeToFunc 0xa1 = ldOp Acc indirectXarg 2 6 
opCodeToFunc 0xb1 = ldOp Acc indirectYarg 2 5

opCodeToFunc 0xa2 = ldOp X immediateArg    2 2
opCodeToFunc 0xa6 = ldOp X zeroPageArg     2 3
opCodeToFunc 0xb6 = ldOp X zeroPageYArg    2 4
opCodeToFunc 0xae = ldOp X absoluteArgPtr  3 4
opCodeToFunc 0xbe = ldOp X absoluteYargPtr 3 4 -- +1

opCodeToFunc 0xa0 = ldOp Y immediateArg 2 2
opCodeToFunc 0xa4 = ldOp Y zeroPageArg 2 3
opCodeToFunc 0xb4 = ldOp Y zeroPageYArg 2 4
opCodeToFunc 0xac = ldOp Y absoluteArgPtr 3 4
opCodeToFunc 0xbc = ldOp Y absoluteYargPtr 3 4

opCodeToFunc 0x85 = stOp Acc zeroPageArg 2 3
opCodeToFunc 0x95 = stOp Acc zeroPageXArg 2 4
opCodeToFunc 0x8d = stOp Acc absoluteArgPtr 3 4
opCodeToFunc 0x9d = stOp Acc absoluteXargPtr 3 5
opCodeToFunc 0x99 = stOp Acc absoluteYargPtr 3 5
opCodeToFunc 0x81 = stOp Acc indirectXarg 2 6
opCodeToFunc 0x91 = stOp Acc indirectYarg 2 6

opCodeToFunc 0x86 = stOp X zeroPageArg 2 3
opCodeToFunc 0x96 = stOp X zeroPageYArg 2 4
opCodeToFunc 0x8e = stOp X absoluteArg 3 4

opCodeToFunc 0x84 = stOp Y zeroPageArg 2 3 
opCodeToFunc 0x94 = stOp Y zeroPageYArg 2 4
opCodeToFunc 0x8c = stOp Y absoluteArgPtr 3 4

opCodeToFunc 0xaa = transferOp Acc X 1 2
opCodeToFunc 0xa8 = transferOp Acc Y 1 2
opCodeToFunc 0xba = transferOp Sp X  1 2
opCodeToFunc 0x8a = transferOp X Acc 1 2
opCodeToFunc 0x9a = transferOp X Sp  1 2
opCodeToFunc 0x98 = transferOp Y Acc 1 2

opCodeToFunc 0x48 = pushOp Acc 1 3
opCodeToFunc 0x08 = pushOp Status 1 3

opCodeToFunc 0x68 = pullOp Acc 1 4
opCodeToFunc 0x28 = pullOp Status 1 4

opCodeToFunc 0x20 = jsrOp absoluteArg 3 6
opCodeToFunc 0x60 = rtsOp 6
opCodeToFunc 0x40 = rtiOp 6

opCodeToFunc 0xc9 = cmpOp immediateArg    Acc 2 2
opCodeToFunc 0xc5 = cmpOp zeroPageArg     Acc 2 3
opCodeToFunc 0xd5 = cmpOp zeroPageXArg    Acc 2 4
opCodeToFunc 0xcd = cmpOp absoluteArgPtr  Acc 3 4
opCodeToFunc 0xdd = cmpOp absoluteXargPtr Acc 3 4 -- +1
opCodeToFunc 0xd9 = cmpOp absoluteYargPtr Acc 3 4 -- +1
opCodeToFunc 0xc1 = cmpOp indirectXarg    Acc 2 6
opCodeToFunc 0xd1 = cmpOp indirectYarg    Acc 2 5 -- +1

opCodeToFunc 0xe0 = cmpOp immediateArg   X 2 2
opCodeToFunc 0xe4 = cmpOp zeroPageArg    X 2 3
opCodeToFunc 0xec = cmpOp absoluteArgPtr X 3 4

opCodeToFunc 0xc0 = cmpOp immediateArg   Y 2 2
opCodeToFunc 0xc4 = cmpOp zeroPageArg    Y 2 3
opCodeToFunc 0xcc = cmpOp absoluteArgPtr Y 3 4

opCodeToFunc 0xe8 = incOp X   1  1 2
opCodeToFunc 0xc8 = incOp Y   1  1 2
opCodeToFunc 0xca = incOp X (-1) 1 2
opCodeToFunc 0x88 = incOp Y (-1) 1 2

opCodeToFunc 0xf0 = branchOp relativeArg isZeroState 2
opCodeToFunc 0xd0 = branchOp relativeArg (not . isZeroState) 2
opCodeToFunc 0xb0 = branchOp relativeArg isCarryState 2
opCodeToFunc 0x90 = branchOp relativeArg (not . isCarryState) 2
opCodeToFunc 0x10 = branchOp relativeArg (not . isPositiveState) 2
opCodeToFunc 0x30 = branchOp relativeArg isPositiveState 2
opCodeToFunc 0x70 = branchOp relativeArg isOverflowState 2
opCodeToFunc 0x50 = branchOp relativeArg (not . isOverflowState) 2

opCodeToFunc 0x4c = jmpOp absoluteArg 3 3

opCodeToFunc 0x24 = bitTstOp zeroPageArg 2 3
opCodeToFunc 0x2c = bitTstOp absoluteArgPtr 2 4

opCodeToFunc 0xea = nop 1

opCodeToFunc 0xe9 = sbcOp immediateArg    2 2
opCodeToFunc 0xe5 = sbcOp zeroPageArg     2 3
opCodeToFunc 0xf5 = sbcOp zeroPageXArg    2 4
opCodeToFunc 0xed = sbcOp absoluteArgPtr  3 4
opCodeToFunc 0xfd = sbcOp absoluteXargPtr 3 4 -- +1 
opCodeToFunc 0xf9 = sbcOp absoluteYargPtr 3 4 -- +1 

opCodeToFunc opCode = error ("op code " ++ (showHex(opCode) "") ++ " Not implemented")

runCpu :: Cpu -> Cpu
runCpu cpu
    | isDead cpu = cpu
    | otherwise  = runCpu (stepCpu cpu)


isDeadAtExec :: Cpu -> Int -> Bool
isDeadAtExec cpu@(Cpu mem (Registers pc _ _ _ _ _) _) cn = isDead cpu && cn == 0

isDead :: Cpu -> Bool
isDead cpu@(Cpu mem (Registers pc _ _ _ _ _) _) = readMem(pc) mem == 0

-- (trace (show (addr, code, temp, stackFF, stackFE, stackFD, stackFC, stackFB, stackFA)) opCode)
stepCpu :: Cpu -> Cpu
stepCpu cpu = (opCodeToFunc opCode) cpu
  where opCode = nextOpCode cpu
        addr = showHex (pc (registers cpu)) ""
        code = showHex opCode ""
        mem = memory cpu
        temp = showHex (readMem 0x0180 mem) ""
        stackFF = showHex (readMem 0x17F mem) ""
        stackFE = showHex (readMem 0x17E mem) ""
        stackFD = showHex (readMem 0x17D mem) ""
        stackFC = showHex (readMem 0x17C mem) ""
        stackFB = showHex (readMem 0x17B mem) ""
        stackFA = showHex (readMem 0x17A mem) ""

        
nextOpCode :: Cpu -> Int
nextOpCode cpu = readMem (pc(regs)) mem
  where mem = memory cpu
        regs = registers cpu

showCpu :: Cpu -> String
showCpu cpu = (show regs) ++ " pc+1=" ++ mem1 ++ " pc+2=" ++ mem2
  where nextMem n = readMem ((pc(regs))+n) mem
        mem1 = showHex (nextMem 1) ""
        mem2 = showHex (nextMem 2) ""
        mem = memory cpu
        regs = registers cpu


runCpuInteractive :: Cpu -> IO ()
runCpuInteractive cpu = do
  putStrLn (showCpu cpu)
  putStrLn ("next op code: " ++ showHex (nextOpCode cpu) "")
  wait <- getLine
  runCpuInteractive (stepCpu cpu)


initCpu :: Int -> [Int] -> [(Int,Int)]-> Cpu
initCpu startAddr program memory = Cpu mem regs 0
  where mem = initMem (cpuProgram ++ memory)
        regs = Registers {pc=startAddr, status=0x24, acc=0, x=0, y=0, sp=stackStart}
        cpuProgram = zip [startAddr..] program
        




