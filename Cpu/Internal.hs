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
    absoluteAddr,
    absolute,
    status,
    acc,
    pc,
    x,
    y,
    sp,
    absoluteX,
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
          | Neg deriving (Eq, Show)

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
type AddressingMode = Cpu -> MemValue
type AddressingCalc = Cpu -> (Address, Bool)
type AccFuncTwoArg = RegValue -> RegValue -> RegValue
type AccFuncOneArg = RegValue -> RegValue
type OpSize = Int
type Address = Int
type MemValue = Int
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

showFlag :: Flag -> RegValue -> String
showFlag f reg = (show f) ++ ": " ++ (show (readFlag f reg))

showStatus :: RegValue -> String
showStatus v =
  (showFlag Carry      v) ++ ", " ++
  (showFlag Zero       v) ++ ", " ++
  (showFlag IrqDis     v) ++ ", " ++
  (showFlag DecMode    v) ++ ", " ++
  (showFlag BrkCommand v) ++ ", " ++
  (showFlag Bit5       v) ++ ", " ++
  (showFlag OverFlow   v) ++ ", " ++
  (showFlag Neg        v)

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

immediateArg :: Cpu -> MemValue
immediateArg cpu = readMem ea mem
  where (ea, _) = immediateArgAddr cpu
        mem     = memory cpu

immediateArgAddr :: AddressingCalc
immediateArgAddr cpu = (pc (registers cpu) + 1, False)

absolute :: Cpu -> MemValue
absolute cpu = readMem address mem
  where mem = memory cpu
        (address, _) = absoluteAddr cpu
        --regs = registers cpu
        --temp = readMem 0x0180 mem

absoluteAddr :: AddressingCalc
absoluteAddr cpu = (args16Address cpu, False)
--  where mem = memory cpu
--        regs = registers cpu

args16Address :: Cpu -> Address
args16Address cpu = toAddress (secArg(cpu)) (fstArg(cpu))

args8Address :: Cpu -> Address
args8Address cpu = toAddress 0 (fstArg(cpu))
--args8Address cpu = toAddress (fstArg(cpu)) (secArg(cpu))

absoluteX :: Cpu -> MemValue
absoluteX cpu = readMem ea mem
  where (ea, _) = absoluteXAddr cpu
        mem = memory cpu

absoluteY :: Cpu -> MemValue
absoluteY cpu = readMem ea mem
  where (ea, _) = absoluteYAddr cpu
        mem = memory cpu

absoluteXAddr :: AddressingCalc
absoluteXAddr = absoluteRegAddr x

absoluteYAddr :: AddressingCalc
absoluteYAddr = absoluteRegAddr y

absoluteRegAddr :: (Registers -> RegValue) -> AddressingCalc
absoluteRegAddr reg cpu = (addr, pageCross)
  where regs = registers cpu
        offs = reg regs
        addr = add16 offs base
        base = args16Address cpu
        pageCross = pageIsCrossed base addr

absoluteReg :: RegValue -> Address -> Cpu -> Int
absoluteReg regValue address cpu = readMem (add16 regValue address) mem
  where mem = memory cpu
        regs = registers cpu

zeroPageArg :: Cpu -> MemValue
zeroPageArg = readCpuMem zeroPageArgAddr

zeroPageArgAddr :: AddressingCalc
zeroPageArgAddr cpu = (addr, False)
  where addr = offsetAddr (\x -> 0) args8Address cpu

zeroPageXArg :: Cpu -> Int
zeroPageXArg = readCpuMem zeroPageXArgAddr

zeroPageYArg :: Cpu -> MemValue
zeroPageYArg = readCpuMem zeroPageYArgAddr

zeroPageXArgAddr :: AddressingCalc
zeroPageXArgAddr cpu = (addr, False)
  where addr = offsetAddr x args8Address cpu

zeroPageYArgAddr :: AddressingCalc
zeroPageYArgAddr cpu = (addr, False)
  where addr = offsetAddr y args8Address cpu

readCpuMem :: AddressingCalc -> Cpu -> MemValue
readCpuMem addresser cpu = readMem addr (memory cpu)
  where (addr, _) = addresser cpu


offsetAddr :: (Registers -> Int) -> (Cpu -> Address) -> Cpu -> Address
offsetAddr reg addresser cpu = add8 addr offset 0
  where addr   = addresser cpu
        offset = reg regs
        regs   = registers cpu


accumulatorArg :: Cpu -> Int
accumulatorArg cpu = acc(regs)
  where mem = memory cpu
        regs = registers cpu

indirectAddr :: AddressingCalc
indirectAddr cpu = (addr2, False)
  where mem = memory cpu
        page = secArg cpu
        offs = fstArg cpu
        addr1lsb = toAddress page offs
        addr1msb = toAddress page ((offs+1) `mod` 256)
        addr2 = toAddress (readMem addr1msb mem) (readMem addr1lsb mem)


indirectXargAddr :: AddressingCalc
indirectXargAddr cpu = (newAddr, pageCrossed)
  where mem = memory cpu
        regs = registers cpu
        indAddr = toAddress 0 (mod (add16 (fstArg cpu) (x regs)) 256)
        low = readMem indAddr mem
        high = readMem (mod (indAddr + 1) 256) mem
        newAddr = toAddress high low
        pageCrossed = pageIsCrossed indAddr newAddr


indirectXarg :: Cpu -> Int
indirectXarg cpu = readMem newAddr mem
  where mem = memory cpu
        (newAddr, _) = indirectXargAddr cpu

--indirectXarg cpu = error "inderectXarg is not implemented"
--
-- X + Arg = newAddr1
-- low = readMem newAddr1
-- high = readMem newAddr1 + 1
-- return (readmem toAddress low high)

indirectYargAddr :: AddressingCalc
indirectYargAddr cpu = (newAddr, pageCrossed)
  where mem = memory cpu
        regs = registers cpu
        newAddr = add16 indAddr (y regs)
        readFrom = fstArg cpu
        readFrom2 = mod (readFrom + 1) 256
        low = readMem readFrom mem
        high = readMem readFrom2 mem
        indAddr = toAddress high low
        pageCrossed = pageIsCrossed indAddr newAddr

indirectYarg :: Cpu -> Int
indirectYarg cpu = readMem newAddr mem
  where mem = memory cpu
        (newAddr, _) = indirectYargAddr cpu

toAddress :: Int -> Int -> Int
toAddress a b = (Data.Bits..|.) (Data.Bits.shiftL a 8) b

fromAddress :: Int -> (Int,Int)
fromAddress value = (high, low)
  where high = shiftR ((Data.Bits..&.) 0xFF00 value) 8
        low  = (Data.Bits..&.) 0x00FF value

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

adcOp :: AddressingCalc -> OpSize -> Cyc -> Int -> Cpu -> Cpu
adcOp ac size c penalty cpu = Cpu mem newRegs newC
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = add8 acc1 acc2 oldCarryInt
        oldCarryInt = if oldCarryFlag then 1 else 0
        oldCarryFlag = readFlag Carry (status regs)
        acc1 = acc regs
        (ea, cross) = ac cpu
        acc2 = readMem ea mem
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        newStatus = updateFlags [(Carry,carryFlag), (OverFlow,overflFlag)] newAccStatus
        carryFlag = addWillCarry acc1 acc2 oldCarryFlag
        overflFlag = addWillOverflow acc1 acc2 oldCarryFlag
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c + (if cross then penalty else 0)

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

ldOp ::  RegisterType -> AddressingCalc -> OpSize -> Cyc -> Int -> Cpu -> Cpu
ldOp rType ac size c penalty cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(rType,newR), (Pc,newPc), (Status,newStatus)] regs
        (ea, cross) = ac cpu
        newR = readMem ea mem
        newPc = pc regs + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newR
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c + (if cross then penalty else 0)

traceHex :: (Integral a, Show a) => String -> a -> a
traceHex msg a = trace (msg ++ ": " ++ showHex a "h") a


stOp ::  RegisterType -> AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
stOp rType ac size c cpu = Cpu newMem newRegs newC
  where newRegs = updateRegister Pc newPc regs
        newPc = pc(regs) + size
        (memAddress, _) = ac cpu
        value = readRegister rType regs
        newMem = writeMem memAddress value mem
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
        newAccVal = (aluOp currentVal)
        currentVal = (f cpu)
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        newStatus = updateFlag Carry carryFlag newAccStatus
        carryFlag = testBit (currentVal) bitPos
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

shiftOpMem :: AccFuncOneArg -> BitPos -> AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
shiftOpMem aluOp bitPos ac size c cpu = Cpu newMem newRegs newC
  where newRegs = updateRegisters [(Pc, newPc), (Status, newStatus)] regs
        newValue = mod (aluOp currentVal) 256
        currentVal = (readMem addr mem)
        (addr, _) = ac cpu
        newPc = pc(regs) + size
        newValueStatus = updateStatusFlagsNumericOp (status(regs)) newValue
        newStatus = updateFlag Carry carryFlag newValueStatus
        carryFlag = testBit (currentVal) bitPos
        mem = memory cpu
        newMem = writeMem addr newValue mem
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

aslToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
aslToMemOp = shiftOpMem ((flip shiftL) 1) 7

lsrOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
lsrOp = shiftOp ((flip shiftR) 1) 0

lsrToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
lsrToMemOp = shiftOpMem ((flip shiftR) 1) 0

rolOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rolOp f size cyc cpu = shiftOp ((flip rotateL8) (readFlag Carry (status(registers(cpu))))) 7 f size cyc cpu
--rolOp f size cyc cpu = (shiftOp (rotateL8)) 7 f size cyc cpu

rorOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rorOp f size cyc cpu = shiftOp ((flip rotateR8) (readFlag Carry (status(registers(cpu))))) 0 f size cyc cpu
--rorOp = shiftOp ((flip rotateR8) True) 0

rolToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rolToMemOp ac size cyc cpu = shiftOpMem ((flip rotateL8) (readFlag Carry (status(registers(cpu))))) 7 ac size cyc cpu
--rolOp f size cyc cpu = (shiftOp (rotateL8)) 7 f size cyc cpu

rorToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rorToMemOp ac size cyc cpu = shiftOpMem ((flip rotateR8) (readFlag Carry (status(registers(cpu))))) 0 ac size cyc cpu
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
jsrOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
jsrOp ac size c cpu = Cpu newMem newRegs newC
  where returnPoint = pc(regs) + size - 1
        pushed = pushAddr (returnPoint) cpu
        newMem = memory(pushed)
        newRegs = updateRegister Pc jumpTo (registers(pushed))
        (jumpTo, _) = ac cpu
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

incMemOp :: AddressingCalc -> Int -> OpSize -> Cyc -> Cpu -> Cpu
incMemOp ac value size c cpu = Cpu newMem newRegs newC
  where newRegs = updateRegisters [(Pc,pc(regs)+size),
                                   (Status,newStatus)] regs
        newMemValue = mod (oldMemValue + value) 256
        oldMemValue = readMem addr mem
        (addr, _) = ac cpu
        newStatus = updateStatusFlagsNumericOp (status(regs)) newMemValue
        mem = memory cpu
        newMem = writeMem addr newMemValue mem
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

jmpOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
jmpOp ac size c cpu = Cpu mem newRegs newC
 where (newPcVal, _) = ac cpu
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

bitTstOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
bitTstOp ac size c cpu = Cpu mem newRegs newC
  where newRegs = updateRegisters [(Status,newStatus),
                                 (Pc,newPc)] regs
        newPc = (pc regs) + size
        masked = (Data.Bits..&.) (acc regs) memArg
        newZero = masked == 0
        newOverflow = readFlag OverFlow memArg
        newNegative = readFlag Neg memArg
        (addr, _) = ac cpu
        memArg = readMem addr mem
        newStatus = updateFlags [(OverFlow,newOverflow),
                                 (Neg, newNegative),
                                 (Zero, newZero)] (status regs)
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

pageIsCrossed :: Int -> Int -> Bool
pageIsCrossed a b = page a /= page b
                      where page = fst . fromAddress

pagePenalty :: Int -> Int -> Int -> Int
pagePenalty penalty addrA addrB =
  if pageIsCrossed addrA addrB then penalty else 0

opCodeToFunc :: OpCode -> (Cpu -> Cpu)
opCodeToFunc 0x18 = clc
opCodeToFunc 0xD8 = cld
opCodeToFunc 0x58 = cli
opCodeToFunc 0xB8 = clv
opCodeToFunc 0x38 = sec
opCodeToFunc 0xF8 = sed
opCodeToFunc 0x78 = sei

opCodeToFunc 0x69 = adcOp immediateArgAddr 2 2 0
opCodeToFunc 0x65 = adcOp zeroPageArgAddr  2 3 0
opCodeToFunc 0x75 = adcOp zeroPageXArgAddr 2 4 0
opCodeToFunc 0x6d = adcOp absoluteAddr     3 4 0
opCodeToFunc 0x7d = adcOp absoluteXAddr    3 4 1 -- +1
opCodeToFunc 0x79 = adcOp absoluteYAddr    3 4 1 -- +1
opCodeToFunc 0x61 = adcOp indirectXargAddr 2 6 0
opCodeToFunc 0x71 = adcOp indirectYargAddr 2 5 1 -- +1

opCodeToFunc 0x29 = andOp immediateArg 2 2
opCodeToFunc 0x25 = andOp zeroPageArg 2 3
opCodeToFunc 0x35 = andOp zeroPageXArg 2 4
opCodeToFunc 0x2d = andOp absolute 3 4
opCodeToFunc 0x3d = andOp absoluteX 3 4 -- +1
opCodeToFunc 0x39 = andOp absoluteY 3 4 -- +1
opCodeToFunc 0x21 = andOp indirectXarg    2 6
opCodeToFunc 0x31 = andOp indirectYarg    2 5 -- +1

opCodeToFunc 0x49 = eorOp immediateArg 2 2
opCodeToFunc 0x45 = eorOp zeroPageArg 2 3
opCodeToFunc 0x55 = eorOp zeroPageXArg 2 4
opCodeToFunc 0x4d = eorOp absolute 3 4
opCodeToFunc 0x5d = eorOp absoluteX 3 4 -- +1
opCodeToFunc 0x59 = eorOp absoluteY 3 4 -- +1
opCodeToFunc 0x41 = eorOp indirectXarg    2 6
opCodeToFunc 0x51 = eorOp indirectYarg    2 5 -- +1

opCodeToFunc 0x09 = oraOp immediateArg    2 2
opCodeToFunc 0x05 = oraOp zeroPageArg     2 3
opCodeToFunc 0x15 = oraOp zeroPageXArg    2 4
opCodeToFunc 0x0d = oraOp absolute        3 4
opCodeToFunc 0x1d = oraOp absoluteX       3 4 -- +1
opCodeToFunc 0x19 = oraOp absoluteY       3 4 -- +1
opCodeToFunc 0x01 = oraOp indirectXarg    2 6
opCodeToFunc 0x11 = oraOp indirectYarg    2 5 -- +1

opCodeToFunc 0x2a = rolOp accumulatorArg  1 2

opCodeToFunc 0x26 = rolToMemOp zeroPageArgAddr 2 5
opCodeToFunc 0x36 = rolToMemOp zeroPageXArgAddr    2 6
opCodeToFunc 0x2e = rolToMemOp absoluteAddr    3 6
opCodeToFunc 0x3e = rolToMemOp absoluteXAddr       3 7

opCodeToFunc 0x6a = rorOp accumulatorArg  1 2

opCodeToFunc 0x66 = rorToMemOp zeroPageArgAddr 2 5
opCodeToFunc 0x76 = rorToMemOp zeroPageXArgAddr    2 6
opCodeToFunc 0x6e = rorToMemOp absoluteAddr    3 6
opCodeToFunc 0x7e = rorToMemOp absoluteXAddr       3 7

opCodeToFunc 0x0a = aslOp accumulatorArg  1 2

opCodeToFunc 0x06 = aslToMemOp zeroPageArgAddr 2 5
opCodeToFunc 0x16 = aslToMemOp zeroPageXArgAddr    2 6
opCodeToFunc 0x0e = aslToMemOp absoluteAddr    3 6
opCodeToFunc 0x1e = aslToMemOp absoluteXAddr       3 7

opCodeToFunc 0x4a = lsrOp accumulatorArg       1 2

opCodeToFunc 0x46 = lsrToMemOp zeroPageArgAddr 2 5
opCodeToFunc 0x56 = lsrToMemOp zeroPageXArgAddr    2 6
opCodeToFunc 0x4e = lsrToMemOp absoluteAddr    3 6
opCodeToFunc 0x5e = lsrToMemOp absoluteXAddr       3 7

opCodeToFunc 0xa9 = ldOp Acc immediateArgAddr 2 2 0
opCodeToFunc 0xa5 = ldOp Acc zeroPageArgAddr  2 3 0
opCodeToFunc 0xb5 = ldOp Acc zeroPageXArgAddr 2 4 0
opCodeToFunc 0xad = ldOp Acc absoluteAddr     3 4 0
opCodeToFunc 0xbd = ldOp Acc absoluteXAddr    3 4 1 -- +1
opCodeToFunc 0xb9 = ldOp Acc absoluteYAddr    3 4 1 -- +1
opCodeToFunc 0xa1 = ldOp Acc indirectXargAddr 2 6 0
opCodeToFunc 0xb1 = ldOp Acc indirectYargAddr 2 5 1 -- +1

opCodeToFunc 0xa2 = ldOp X immediateArgAddr 2 2 0
opCodeToFunc 0xa6 = ldOp X zeroPageArgAddr  2 3 0
opCodeToFunc 0xb6 = ldOp X zeroPageYArgAddr 2 4 0
opCodeToFunc 0xae = ldOp X absoluteAddr     3 4 0
opCodeToFunc 0xbe = ldOp X absoluteYAddr    3 4 1 -- +1

opCodeToFunc 0xa0 = ldOp Y immediateArgAddr 2 2 0
opCodeToFunc 0xa4 = ldOp Y zeroPageArgAddr  2 3 0
opCodeToFunc 0xb4 = ldOp Y zeroPageXArgAddr 2 4 0
opCodeToFunc 0xac = ldOp Y absoluteAddr     3 4 0
opCodeToFunc 0xbc = ldOp Y absoluteYAddr    3 4 0

opCodeToFunc 0x85 = stOp Acc zeroPageArgAddr  2 3
opCodeToFunc 0x95 = stOp Acc zeroPageXArgAddr     2 4
opCodeToFunc 0x8d = stOp Acc absoluteAddr     3 4
opCodeToFunc 0x9d = stOp Acc absoluteXAddr        3 5
opCodeToFunc 0x99 = stOp Acc absoluteYAddr        3 5
opCodeToFunc 0x81 = stOp Acc indirectXargAddr 2 6
opCodeToFunc 0x91 = stOp Acc indirectYargAddr 2 6

opCodeToFunc 0x86 = stOp X zeroPageArgAddr 2 3
opCodeToFunc 0x96 = stOp X zeroPageYArgAddr    2 4
opCodeToFunc 0x8e = stOp X absoluteAddr    3 4

opCodeToFunc 0x84 = stOp Y zeroPageArgAddr  2 3
opCodeToFunc 0x94 = stOp Y zeroPageXArgAddr 2 4
opCodeToFunc 0x8c = stOp Y absoluteAddr     3 4

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

opCodeToFunc 0x20 = jsrOp absoluteAddr 3 6
opCodeToFunc 0x60 = rtsOp 6
opCodeToFunc 0x40 = rtiOp 6

opCodeToFunc 0xc9 = cmpOp immediateArg    Acc 2 2
opCodeToFunc 0xc5 = cmpOp zeroPageArg     Acc 2 3
opCodeToFunc 0xd5 = cmpOp zeroPageXArg    Acc 2 4
opCodeToFunc 0xcd = cmpOp absolute  Acc 3 4
opCodeToFunc 0xdd = cmpOp absoluteX Acc 3 4 -- +1
opCodeToFunc 0xd9 = cmpOp absoluteY Acc 3 4 -- +1
opCodeToFunc 0xc1 = cmpOp indirectXarg    Acc 2 6
opCodeToFunc 0xd1 = cmpOp indirectYarg    Acc 2 5 -- +1

opCodeToFunc 0xe0 = cmpOp immediateArg   X 2 2
opCodeToFunc 0xe4 = cmpOp zeroPageArg    X 2 3
opCodeToFunc 0xec = cmpOp absolute X 3 4

opCodeToFunc 0xc0 = cmpOp immediateArg   Y 2 2
opCodeToFunc 0xc4 = cmpOp zeroPageArg    Y 2 3
opCodeToFunc 0xcc = cmpOp absolute Y 3 4

opCodeToFunc 0xe6 = incMemOp zeroPageArgAddr 1 2 5
opCodeToFunc 0xf6 = incMemOp zeroPageXArgAddr    1 2 6
opCodeToFunc 0xee = incMemOp absoluteAddr 1 3 6
opCodeToFunc 0xfe = incMemOp absoluteXAddr 1 3 7

opCodeToFunc 0xc6 = incMemOp zeroPageArgAddr (-1) 2 5
opCodeToFunc 0xd6 = incMemOp zeroPageXArgAddr    (-1) 2 6
opCodeToFunc 0xce = incMemOp absoluteAddr (-1) 3 6
opCodeToFunc 0xde = incMemOp absoluteXAddr (-1) 3 7

opCodeToFunc 0xe8 = incOp X   1  1 2
opCodeToFunc 0xc8 = incOp Y   1  1 2
opCodeToFunc 0xca = incOp X (-1) 1 2
opCodeToFunc 0x88 = incOp Y (-1) 1 2

opCodeToFunc 0xf0 = branchOp relativeArg isZeroState             2
opCodeToFunc 0xd0 = branchOp relativeArg (not . isZeroState)     2
opCodeToFunc 0xb0 = branchOp relativeArg isCarryState            2
opCodeToFunc 0x90 = branchOp relativeArg (not . isCarryState)    2
opCodeToFunc 0x10 = branchOp relativeArg (not . isPositiveState) 2
opCodeToFunc 0x30 = branchOp relativeArg isPositiveState         2
opCodeToFunc 0x70 = branchOp relativeArg isOverflowState         2
opCodeToFunc 0x50 = branchOp relativeArg (not . isOverflowState) 2

opCodeToFunc 0x4c = jmpOp absoluteAddr 3 3
opCodeToFunc 0x6c = jmpOp indirectAddr 3 5

opCodeToFunc 0x24 = bitTstOp zeroPageArgAddr  2 3
opCodeToFunc 0x2c = bitTstOp absoluteAddr  3 4

opCodeToFunc 0xea = nop 1

opCodeToFunc 0xe9 = sbcOp immediateArg  2 2
opCodeToFunc 0xe5 = sbcOp zeroPageArg   2 3
opCodeToFunc 0xf5 = sbcOp zeroPageXArg  2 4
opCodeToFunc 0xed = sbcOp absolute      3 4
opCodeToFunc 0xfd = sbcOp absoluteX     3 4 -- +1
opCodeToFunc 0xf9 = sbcOp absoluteY     3 4 -- +1
opCodeToFunc 0xe1 = sbcOp indirectXarg  2 6
opCodeToFunc 0xf1 = sbcOp indirectYarg  2 5 -- +1


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
stepCpu cpu = (opCodeToFunc (trace (show (addr, code, arg1, arg2, regs, p)) opCode)) cpu
  where opCode = nextOpCode cpu
        addr = showHex (pc (registers cpu)) ""
        code = showHex opCode ""
        arg1 =  "arg1: " ++ showHex (fstArg cpu) ""
        arg2 =  "arg2: " ++ showHex (secArg cpu) ""
        rs = registers cpu
        regs = "regs: [" ++ (showHex (acc rs) "") ++ "]"
        mem = memory cpu
        p = showStatus (status rs)


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





