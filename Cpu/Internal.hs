module Cpu.Internal(
    Cpu,
    registers,
    Registers,
    RegisterType(X),
    stepCpu,
    initCpu,
    runCpu,
    runCpuInteractive,
    immediate,
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
    cyc,
    textAt,
    debugTestStatus,
    valueAt
) where


import qualified Debug.Trace as T
import Text.Printf
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Data.Int
import Data.Char (ord, chr)
import Numeric(showHex)
import Mem
import Cpu.OpInfo

data Cpu = Cpu { memory :: Memory Int Int
               , registers :: Registers
               , cyc :: Int
               , debug :: Bool
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
    where h val = showB val

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



-- Arithemtic -----------------------------------------------------------------
-------------------------------------------------------------------------------


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



-- Arguments and addressing ---------------------------------------------------
-------------------------------------------------------------------------------


fstArg :: Cpu -> Int
fstArg cpu = readMem (pc(registers cpu)+1) (memory cpu)

secArg :: Cpu -> Int
secArg cpu = readMem (pc(regs)+2) mem
  where mem = memory cpu
        regs = registers cpu

relative :: Cpu -> MemValue
relative cpu = fromIntegral(fromIntegral((fstArg cpu))::Int8)::Int
  where mem = memory cpu
        regs = registers cpu

immediate :: Cpu -> MemValue
immediate cpu = readMem ea mem
  where (ea, _) = immediateAddr cpu
        mem     = memory cpu

immediateAddr :: AddressingCalc
immediateAddr cpu = (pc (registers cpu) + 1, False)

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

zeroPage :: Cpu -> MemValue
zeroPage = readCpuMem zeroPageAddr

zeroPageAddr :: AddressingCalc
zeroPageAddr cpu = (addr, False)
  where addr = offsetAddr (\x -> 0) args8Address cpu

zeroPageX :: Cpu -> Int
zeroPageX = readCpuMem zeroPageXAddr

zeroPageYArg :: Cpu -> MemValue
zeroPageYArg = readCpuMem zeroPageYArgAddr

zeroPageXAddr :: AddressingCalc
zeroPageXAddr cpu = (addr, False)
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


indirectXAddr :: AddressingCalc
indirectXAddr cpu = (newAddr, pageCrossed)
  where mem = memory cpu
        regs = registers cpu
        indAddr = toAddress 0 (mod (add16 (fstArg cpu) (x regs)) 256)
        low = readMem indAddr mem
        high = readMem (mod (indAddr + 1) 256) mem
        newAddr = toAddress high low
        pageCrossed = pageIsCrossed indAddr newAddr


indirectX :: Cpu -> Int
indirectX cpu = readMem newAddr mem
  where mem = memory cpu
        (newAddr, _) = indirectXAddr cpu

indirectYAddr :: AddressingCalc
indirectYAddr cpu = (newAddr, pageCrossed)
  where mem = memory cpu
        regs = registers cpu
        newAddr = add16 indAddr (y regs)
        readFrom = fstArg cpu
        readFrom2 = mod (readFrom + 1) 256
        low = readMem readFrom mem
        high = readMem readFrom2 mem
        indAddr = toAddress high low
        pageCrossed = pageIsCrossed indAddr newAddr

indirectY :: Cpu -> Int
indirectY cpu = readMem newAddr mem
  where mem = memory cpu
        (newAddr, _) = indirectYAddr cpu

toAddress :: Int -> Int -> Int
toAddress a b = (Data.Bits..|.) (Data.Bits.shiftL a 8) b

fromAddress :: Int -> (Int,Int)
fromAddress value = (high, low)
  where high = shiftR ((Data.Bits..&.) 0xFF00 value) 8
        low  = (Data.Bits..&.) 0x00FF value

pageIsCrossed :: Int -> Int -> Bool
pageIsCrossed a b = page a /= page b
                      where page = fst . fromAddress

readMemValue :: AddressingCalc -> Cpu -> MemValue
readMemValue ac cpu = readMem addr (memory cpu)
  where (addr, _) = ac cpu



-- Operations -----------------------------------------------------------------
-------------------------------------------------------------------------------


adcOp :: AddressingCalc -> OpSize -> Cyc -> Int -> Cpu -> Cpu
adcOp ac s c p cpu = (cpuProgress s c) (adcBase ac p cpu)

adcBase :: AddressingCalc -> Int -> Cpu -> Cpu
adcBase ac penalty cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
  where newRegs = regs {acc=newAcc, status=newStatus}
        newAcc = add8 acc1 acc2 oldCarryInt
        oldCarryInt = if oldCarryFlag then 1 else 0
        oldCarryFlag = readFlag Carry (status regs)
        acc1 = acc regs
        (ea, cross) = ac cpu
        acc2 = readMem ea mem
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        newStatus = updateFlags [(Carry,carryFlag), (OverFlow,overflFlag)] newAccStatus
        carryFlag = addWillCarry acc1 acc2 oldCarryFlag
        overflFlag = addWillOverflow acc1 acc2 oldCarryFlag
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + (if cross then penalty else 0)

ldOp ::  [RegisterType] -> AddressingCalc -> OpSize -> Cyc -> Int -> Cpu -> Cpu
ldOp rTypes ac size c penalty cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
  where newRegs = updateRegisters (newValueRegs ++ [(Pc,newPc), (Status,newStatus)]) regs
        newValueRegs = map (\t -> (t,newR)) rTypes
        (ea, cross) = ac cpu
        newR = readMem ea mem
        newPc = pc regs + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newR
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c + (if cross then penalty else 0)

traceHex :: (Integral a, Show a) => Cpu -> String -> a -> a
traceHex cpu msg a = trace cpu (msg ++ ": " ++ showHex a "h") a


stOp ::  RegisterType -> AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
stOp rType ac size c cpu = cpu { memory = newMem, registers = newRegs, cyc = newC }
  where newRegs = updateRegister Pc newPc regs
        newPc = pc(regs) + size
        (memAddress, _) = ac cpu
        value = readRegister rType regs
        newMem = writeMem memAddress value mem
        mem = memory cpu
        regs = registers cpu
        newC = (cyc cpu) + c

bitBase :: AccFuncTwoArg -> AddressingCalc -> Cpu -> Cpu
bitBase aluOp ac cpu = cpu { registers = newRegs, cyc = (cyc cpu) }
  where newRegs = regs {acc=newAcc, status=newStatus}
        newAcc = (aluOp) acc1 acc2
        acc1 = acc(regs)
        acc2 = readMemValue ac cpu
        newStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        regs = registers cpu

shiftOp :: AccFuncOneArg -> BitPos -> AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
shiftOp aluOp bitPos f size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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
shiftOpMem aluOp bitPos ac s c cpu = (cpuProgress s c) (shiftMemBase aluOp bitPos ac cpu)

shiftMemBase :: AccFuncOneArg -> BitPos -> AddressingCalc -> Cpu -> Cpu
shiftMemBase aluOp bitPos ac cpu = cpu { memory = newMem, registers = newRegs, cyc = (cyc cpu) }
  where newRegs = updateRegisters [(Status, newStatus)] regs
        newValue = mod (aluOp currentVal) 256
        currentVal = (readMem addr mem)
        (addr, _) = ac cpu
        newValueStatus = updateStatusFlagsNumericOp (status(regs)) newValue
        newStatus = updateFlag Carry carryFlag newValueStatus
        carryFlag = testBit (currentVal) bitPos
        mem = memory cpu
        newMem = writeMem addr newValue mem
        regs = registers cpu


transferOp :: RegisterType -> RegisterType -> OpSize -> Cyc -> Cpu -> Cpu
transferOp from to size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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

saxOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
saxOp ac size c cpu = cpu { memory = mem', registers = regs', cyc = cycs' }
  where and' = (Data.Bits..&.)
        mem   = memory cpu
        regs  = registers cpu
        op1 = ((acc regs) `and'` (x regs))
        op2 = fstArg cpu
        rx' = op1
        fc' = False
        regs' = updateRegisters [(Pc, pc')] regs
        pc' = (pc regs) + size
        (addr, cross) = ac cpu
        mem' = writeMem addr rx' (memory cpu)
        cycs' = (cyc cpu) + c + (if cross then 1 else 0)

axsOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
axsOp ac size c cpu = (cpuProgress size c) (cpu { registers = regs'})
  where regs' = updateRegisters [(X, x'), (Status, status'')] regs
        value = readMem addr (memory cpu)
        (addr, _) = ac cpu
        andResult = and currentAcc currentX
        x' = sub8 andResult value
        newCarry = subWillCarry andResult value True
        regs = registers cpu
        currentAcc = acc regs
        currentX = x regs
        status' = updateStatusFlagsNumericOp (status regs) x' 
        status'' = updateFlag Carry (not newCarry) status'
        and = (Data.Bits..&.)

shaOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
shaOp ac size c cpu = (cpuProgress size c) cpu { memory = mem' }
  where regs = registers cpu
        val = and currentAcc (and currentX (high + 1))
        currentAcc = acc regs
        currentX = x regs
        (addr, pageCrossed) = ac cpu
        (high, low) = fromAddress addr
        mem = memory cpu
        storeAddr = toAddress newHigh low
        mem' = writeMem storeAddr val mem
        newHigh = if pageCrossed then val else high
        and = (Data.Bits..&.)


shxOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
shxOp ac size c cpu = (cpuProgress size c) (shBase ac X cpu)

shyOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
shyOp ac size c cpu = (cpuProgress size c) (shBase ac Y cpu)

shBase :: AddressingCalc -> RegisterType -> Cpu -> Cpu
shBase ac regType cpu = cpu { memory = mem'}
  where regs = registers cpu
        val = and currentRegVal (high + 1)
        currentRegVal = readRegister regType regs
        (addr, pageCrossed) = ac cpu
        (high, low) = fromAddress addr
        mem = memory cpu
        storeAddr = toAddress newHigh low
        mem' = writeMem storeAddr val mem
        newHigh = if pageCrossed then val else high
        and = (Data.Bits..&.)

andBase :: AddressingCalc -> Cpu -> Cpu
andBase = bitBase (Data.Bits..&.)

andOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
andOp ac s c cpu = (cpuProgress s c) (andBase ac cpu)

eorBase :: AddressingCalc -> Cpu -> Cpu
eorBase = bitBase xor

eorOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
eorOp ac s c cpu = (cpuProgress s c) (eorBase ac cpu)

oraBase :: AddressingCalc -> Cpu -> Cpu
oraBase = bitBase (.|.)

oraOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
oraOp ac s c cpu = (cpuProgress s c) (oraBase ac cpu)

aslOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
aslOp = shiftOp ((flip shiftL) 1) 7

aslToMemBase :: AddressingCalc -> Cpu -> Cpu
aslToMemBase = shiftMemBase ((flip shiftL) 1) 7

aslToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
aslToMemOp ac s c cpu = (cpuProgress s c) (aslToMemBase ac cpu)

ancOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
ancOp ac s c cpu = (cpuProgress s c) (cpu' { registers = regs' })
  where cpu' = andBase ac cpu
        currentStatus = status regs
        regs = registers cpu'
        neg' = readFlag Neg currentStatus
        status' = updateFlag Carry neg' currentStatus
        regs' = updateRegister Status status' regs

lsrOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
lsrOp = shiftOp ((flip shiftR) 1) 0

lsrToMemBase :: AddressingCalc -> Cpu -> Cpu
lsrToMemBase = shiftMemBase ((flip shiftR) 1) 0

lsrToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
lsrToMemOp ac s c cpu = (cpuProgress s c) (lsrToMemBase ac cpu)

rolOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rolOp f size cyc cpu = shiftOp ((flip rotateL8) (readFlag Carry (status(registers(cpu))))) 7 f size cyc cpu

rorOp :: AddressingMode -> OpSize -> Cyc -> Cpu -> Cpu
rorOp f size cyc cpu = shiftOp ((flip rotateR8) (readFlag Carry (status(registers(cpu))))) 0 f size cyc cpu

rolToMemBase :: AddressingCalc -> Cpu -> Cpu
rolToMemBase ac cpu = shiftMemBase ((flip rotateL8) (readFlag Carry (status(registers(cpu))))) 7 ac cpu

rolToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rolToMemOp ac s c cpu = (cpuProgress s c) (rolToMemBase ac cpu)

rorToMemBase :: AddressingCalc -> Cpu -> Cpu
rorToMemBase ac cpu = shiftMemBase ((flip rotateR8) (readFlag Carry (status(registers(cpu))))) 0 ac cpu

rorToMemOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rorToMemOp ac s c cpu = (cpuProgress s c) (rorToMemBase ac cpu)

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
pushOp regType size c cpu = push pushValue (cpu { memory = mem, registers = newRegs, cyc = newC })
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
pullOp regType size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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

jsrOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
jsrOp ac size c cpu = cpu { memory = newMem, registers = newRegs, cyc = newC }
  where returnPoint = pc(regs) + size - 1
        pushed = pushAddr (returnPoint) cpu
        newMem = memory(pushed)
        newRegs = updateRegister Pc jumpTo (registers(pushed))
        (jumpTo, _) = ac cpu
        regs = registers cpu
        newC = (cyc cpu) + c

rtsOp :: Cyc -> Cpu -> Cpu
rtsOp c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
  where mem = memory cpu
        newC = (cyc cpu) + c
        newRegs =  updateRegister Pc (pc(pulledRegs)+1) pulledRegs
          where pulledRegs = registers(pullPc cpu)

rtiOp :: Cyc -> Cpu -> Cpu
rtiOp c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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

pull :: RegisterType -> Cpu -> Cpu
pull regType cpu = cpu { memory = mem, registers = newRegs, cyc = 0 }
  where stackValue = readMem (newSp + stackBase) mem
        newRegs = updateRegisters [(regType,stackValue),
                                   (Sp, newSp)] regs
        spValue = sp(regs)
        newSp = mod (spValue + 1) 256
        mem = memory cpu
        regs = registers cpu


pullPc :: Cpu -> Cpu
pullPc cpu = cpu { memory = mem, registers = newRegs, cyc = 0 }
  where newRegs = updateRegisters [(Pc,stackValue),
                                   (Sp, spValueHigh)] regs
        stackValue = toAddress high low
        spValueLow = mod (sp(regs) +1) 256
        spValueHigh = mod (spValueLow + 1) 256
        low = readMem (spValueLow + stackBase) mem
        high = readMem (spValueHigh + stackBase) mem
        mem = memory cpu
        regs = registers cpu


pushAddr :: Int -> Cpu -> Cpu
pushAddr addr cpu = push addrLow (push addrHigh cpu)
  where addrHigh = fst(fromAddress(addr))
        addrLow = snd(fromAddress(addr))

push :: Int -> Cpu -> Cpu
push value cpu = cpu { memory = newMem, registers = newRegs, cyc = (cyc cpu) }
  where newMem = writeMem (spValue + stackBase) value mem
        spValue = sp(regs)
        newRegs = updateRegister Sp newSp regs
        newSp = mod (spValue - 1) 256
        mem = memory cpu
        regs = registers cpu

updateFlagOp :: Flag -> Bool -> Cpu -> Cpu
updateFlagOp flag value cpu = cpu { memory = (memory(cpu)), registers = newRegs, cyc = newC }
  where regs = registers(cpu)
        newRegs = (regs {pc = pc(regs)+1, status = statusValue})
        statusValue = updateFlag flag value (status(regs))
        newC = (cyc cpu) + 2

incOp :: RegisterType -> Int -> Int -> Cyc -> Cpu -> Cpu
incOp regType value size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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
cmpOp f regType size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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
bneOp f size cpu = cpu { memory = mem, registers = newRegs, cyc = 0 }
  where newRegs = updateRegister Pc newPc regs
        newPc = if zeroClear then newPcValue else pc(regs)+size
        newPcValue = add16 (pc(regs)) (f cpu)
        zeroClear = not (readFlag Zero (status(regs)))
        mem = memory cpu
        regs = registers cpu


dcpOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
dcpOp ac size c cpu = cpu { memory = newMem, registers = newRegs, cyc = newC }
  where newRegs = updateRegisters [(Pc,pc(regs)+size),
                                   (Status,newStatus2)] regs
        newStatus2 = updateFlags [(Carry,ra >= newMemValue),
                                  (Neg,negative),
                                  (Zero,ra == newMemValue)] (newStatus1)
        newMemValue = mod (oldMemValue - 1) 256
        oldMemValue = readMem addr mem
        (addr, _) = ac cpu
        newStatus1 = updateStatusFlagsNumericOp (status(regs)) newMemValue
        newMem = writeMem addr newMemValue mem
        negative = testBit (sub8 ra newMemValue) 7
        regs = registers cpu
        newC = (cyc cpu) + c
        ra = acc regs
        mem = memory cpu


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
branchOp am c size cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
  where newRegs = updateRegister Pc newPcVal regs
        newPcVal = if doBranch then bPcVal else pcVal
        bPcVal = (add16 (pc regs) offset ) + size
        offset = am cpu
        cross = pageIsCrossed bPcVal pcVal
        pcVal = pc regs + size
        doBranch = c cpu
        mem = memory cpu
        regs = registers cpu
        oldC = cyc cpu
        pagePenalty = if cross then 1 else 0
        newC = if doBranch
               then oldC + 3 + pagePenalty
               else oldC + 2

jmpOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
jmpOp ac size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
 where (newPcVal, _) = ac cpu
       newRegs = updateRegister Pc newPcVal regs
       mem = memory cpu
       regs = registers cpu
       newC = (cyc cpu) + c

nop :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
nop ac size c cpu = cpu{memory=mem,registers=newRegs, cyc=newC}
  where newRegs = updateRegister Pc (pc(regs)+size) regs
        mem = memory cpu
        (_, cross) = ac cpu
        regs = registers cpu
        newC = (cyc cpu) + c + (if cross then 1 else 0)

bitTstOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
bitTstOp ac size c cpu = cpu { memory = mem, registers = newRegs, cyc = newC }
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


sbcOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
sbcOp ac s c cpu = (cpuProgress s c) (sbcBase ac cpu)

cpuProgress :: OpSize -> Cyc -> Cpu -> Cpu
cpuProgress s c cpu = cpu { memory = mem', registers = regs', cyc = cycs' }
  where mem'  = memory cpu
        regs' = regs {pc=pc'}
        regs  = registers cpu
        pc'   = (pc regs) + s
        cycs' = (cyc cpu) + c

sbcBase :: AddressingCalc -> Cpu -> Cpu
sbcBase ac cpu = cpu { memory = mem, registers = newRegs, cyc = (cyc cpu) }
  where newRegs = regs {acc=newAcc, status=newStatus}
        newAcc = sub8c acc1 acc2 oldCarryInt
        oldCarryInt = if oldCarryFlag then 1 else 0
        oldCarryFlag = readFlag Carry (status regs)
        acc1 = acc(regs)
        acc2 = readMemValue ac cpu
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc
        newStatus = updateFlags [(Carry,not carryFlag), (OverFlow,overflFlag)] newAccStatus
        carryFlag = subWillCarry acc1 acc2 oldCarryFlag
        overflFlag = subWillOverflow acc1 acc2 oldCarryFlag
        mem = memory cpu
        regs = registers cpu


incMemOp :: AddressingCalc -> Int -> OpSize -> Cyc -> Cpu -> Cpu
incMemOp ac v s c cpu = (cpuProgress s c) (incMemBase ac v cpu)


incMemBase :: AddressingCalc -> Int -> Cpu -> Cpu
incMemBase ac value cpu = cpu { memory = newMem, registers = newRegs, cyc = (cyc cpu) }
  where newRegs = regs { status = newStatus }
        newMemValue = mod (oldMemValue + value) 256
        oldMemValue = readMem addr mem
        (addr, _) = ac cpu
        newStatus = updateStatusFlagsNumericOp (status(regs)) newMemValue
        mem = memory cpu
        newMem = writeMem addr newMemValue mem
        regs = registers cpu

alrOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
alrOp ac s c cpu = (cpuProgress s c) cpu' { registers = regs'}
  where cpu' = andBase ac cpu
        regs = registers cpu'
        andAcc = acc regs
        shiftAcc = shiftR andAcc 1
        oldBit0 = testBit andAcc 0
        status' = updateStatusFlagsNumericOp (status regs) shiftAcc
        status'' = updateFlag Carry oldBit0 status'
        regs' = updateRegisters [(Acc, shiftAcc), (Status, status'')] regs


arrOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
arrOp ac s c cpu = (cpuProgress s c) cpu' { registers = regs'}
  where cpu' = andBase ac cpu
        acc' = acc regs
        rotAcc = rotateR8 acc' (readFlag Carry currentStatus)
        regs = registers cpu'
        currentStatus = status regs
        bit5 = testBit rotAcc 5
        bit6 = testBit rotAcc 6
        bit6xorbit5 = xor bit6 bit5
        regs' = updateRegisters [(Status, status''), (Acc, rotAcc)] regs
        status' = updateStatusFlagsNumericOp currentStatus rotAcc
        status'' = updateFlags [(Carry, bit6), (OverFlow,bit6xorbit5)] status'


iscOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
iscOp ac s c cpu = (cpuProgress s c) (sbcBase ac (incMemBase ac 1 cpu))

sloOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
sloOp ac s c cpu = (cpuProgress s c) (oraBase ac (aslToMemBase ac cpu))

rlaOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rlaOp ac s c cpu = (cpuProgress s c) (andBase ac (rolToMemBase ac cpu))

sreOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
sreOp ac s c cpu = (cpuProgress s c) (eorBase ac (lsrToMemBase ac cpu))

rraOp :: AddressingCalc -> OpSize -> Cyc -> Cpu -> Cpu
rraOp ac s c cpu = (cpuProgress s c) (adcBase ac 0 (rorToMemBase ac cpu))

brkOp :: Cpu -> Cpu
brkOp cpu = cpu'' { registers = regs', cyc = (cyc cpu) + 7 }
  where cpu'  = push pcl (push pch cpu)
        cpu'' = push rp cpu'
        (pch, pcl) = fromAddress (pc regs)
        regs  = (registers cpu)
        regs' = regs { pc = vector }
        mem = memory cpu
        vector = toAddress (readMem 0xFFFF mem) (readMem 0xFFFE mem)
        rp = updateFlag BrkCommand True (status regs)








-- Op codes -------------------------------------------------------------------
-------------------------------------------------------------------------------



opCodeToFunc :: OpCode -> (Cpu -> Cpu)

opCodeToFunc 0x00 = brkOp

opCodeToFunc 0x18 = clc
opCodeToFunc 0xD8 = cld
opCodeToFunc 0x58 = cli
opCodeToFunc 0xB8 = clv
opCodeToFunc 0x38 = sec
opCodeToFunc 0xF8 = sed
opCodeToFunc 0x78 = sei

opCodeToFunc 0x69 = adcOp immediateAddr 2 2 0
opCodeToFunc 0x65 = adcOp zeroPageAddr  2 3 0
opCodeToFunc 0x75 = adcOp zeroPageXAddr 2 4 0
opCodeToFunc 0x6d = adcOp absoluteAddr  3 4 0
opCodeToFunc 0x7d = adcOp absoluteXAddr 3 4 1
opCodeToFunc 0x79 = adcOp absoluteYAddr 3 4 1
opCodeToFunc 0x61 = adcOp indirectXAddr 2 6 0
opCodeToFunc 0x71 = adcOp indirectYAddr 2 5 1

opCodeToFunc 0x29 = andOp immediateAddr 2 2
opCodeToFunc 0x25 = andOp zeroPageAddr  2 3
opCodeToFunc 0x35 = andOp zeroPageXAddr 2 4
opCodeToFunc 0x2d = andOp absoluteAddr  3 4
opCodeToFunc 0x3d = andOp absoluteXAddr 3 4 -- +1
opCodeToFunc 0x39 = andOp absoluteYAddr 3 4 -- +1
opCodeToFunc 0x21 = andOp indirectXAddr 2 6
opCodeToFunc 0x31 = andOp indirectYAddr 2 5 -- +1

opCodeToFunc 0x49 = eorOp immediateAddr 2 2
opCodeToFunc 0x45 = eorOp zeroPageAddr  2 3
opCodeToFunc 0x55 = eorOp zeroPageXAddr 2 4
opCodeToFunc 0x4d = eorOp absoluteAddr  3 4
opCodeToFunc 0x5d = eorOp absoluteXAddr 3 4 -- +1
opCodeToFunc 0x59 = eorOp absoluteYAddr 3 4 -- +1
opCodeToFunc 0x41 = eorOp indirectXAddr 2 6
opCodeToFunc 0x51 = eorOp indirectYAddr 2 5 -- +1

opCodeToFunc 0x09 = oraOp immediateAddr 2 2
opCodeToFunc 0x05 = oraOp zeroPageAddr  2 3
opCodeToFunc 0x15 = oraOp zeroPageXAddr 2 4
opCodeToFunc 0x0d = oraOp absoluteAddr  3 4
opCodeToFunc 0x1d = oraOp absoluteXAddr 3 4 -- +1
opCodeToFunc 0x19 = oraOp absoluteYAddr 3 4 -- +1
opCodeToFunc 0x01 = oraOp indirectXAddr 2 6
opCodeToFunc 0x11 = oraOp indirectYAddr 2 5 -- +1

opCodeToFunc 0x2a = rolOp accumulatorArg 1 2

opCodeToFunc 0x26 = rolToMemOp zeroPageAddr  2 5
opCodeToFunc 0x36 = rolToMemOp zeroPageXAddr 2 6
opCodeToFunc 0x2e = rolToMemOp absoluteAddr  3 6
opCodeToFunc 0x3e = rolToMemOp absoluteXAddr 3 7

opCodeToFunc 0x6a = rorOp accumulatorArg 1 2

opCodeToFunc 0x66 = rorToMemOp zeroPageAddr  2 5
opCodeToFunc 0x76 = rorToMemOp zeroPageXAddr 2 6
opCodeToFunc 0x6e = rorToMemOp absoluteAddr  3 6
opCodeToFunc 0x7e = rorToMemOp absoluteXAddr 3 7

opCodeToFunc 0x0a = aslOp accumulatorArg 1 2

opCodeToFunc 0x06 = aslToMemOp zeroPageAddr  2 5
opCodeToFunc 0x16 = aslToMemOp zeroPageXAddr 2 6
opCodeToFunc 0x0e = aslToMemOp absoluteAddr  3 6
opCodeToFunc 0x1e = aslToMemOp absoluteXAddr 3 7

opCodeToFunc 0x4a = lsrOp accumulatorArg 1 2

opCodeToFunc 0x46 = lsrToMemOp zeroPageAddr  2 5
opCodeToFunc 0x56 = lsrToMemOp zeroPageXAddr 2 6
opCodeToFunc 0x4e = lsrToMemOp absoluteAddr  3 6
opCodeToFunc 0x5e = lsrToMemOp absoluteXAddr 3 7

opCodeToFunc 0xa9 = ldOp [Acc] immediateAddr 2 2 0
opCodeToFunc 0xa5 = ldOp [Acc] zeroPageAddr  2 3 0
opCodeToFunc 0xb5 = ldOp [Acc] zeroPageXAddr 2 4 0
opCodeToFunc 0xad = ldOp [Acc] absoluteAddr  3 4 0
opCodeToFunc 0xbd = ldOp [Acc] absoluteXAddr 3 4 1
opCodeToFunc 0xb9 = ldOp [Acc] absoluteYAddr 3 4 1
opCodeToFunc 0xa1 = ldOp [Acc] indirectXAddr 2 6 0
opCodeToFunc 0xb1 = ldOp [Acc] indirectYAddr 2 5 1

opCodeToFunc 0xa3 = ldOp [Acc, X] indirectXAddr    2 6 0
opCodeToFunc 0xa7 = ldOp [Acc, X] zeroPageAddr     2 3 0
opCodeToFunc 0xab = ldOp [Acc, X] immediateAddr    2 2 0
opCodeToFunc 0xaf = ldOp [Acc, X] absoluteAddr     3 4 0
opCodeToFunc 0xb3 = ldOp [Acc, X] indirectYAddr    2 5 1
opCodeToFunc 0xb7 = ldOp [Acc, X] zeroPageYArgAddr 2 4 0
opCodeToFunc 0xbf = ldOp [Acc, X] absoluteYAddr    3 4 1

opCodeToFunc 0xa2 = ldOp [X] immediateAddr    2 2 0
opCodeToFunc 0xa6 = ldOp [X] zeroPageAddr     2 3 0
opCodeToFunc 0xb6 = ldOp [X] zeroPageYArgAddr 2 4 0
opCodeToFunc 0xae = ldOp [X] absoluteAddr     3 4 0
opCodeToFunc 0xbe = ldOp [X] absoluteYAddr    3 4 1

opCodeToFunc 0xa0 = ldOp [Y] immediateAddr 2 2 0
opCodeToFunc 0xa4 = ldOp [Y] zeroPageAddr  2 3 0
opCodeToFunc 0xb4 = ldOp [Y] zeroPageXAddr 2 4 0
opCodeToFunc 0xac = ldOp [Y] absoluteAddr  3 4 0
opCodeToFunc 0xbc = ldOp [Y] absoluteXAddr 3 4 1

opCodeToFunc 0x85 = stOp Acc zeroPageAddr  2 3
opCodeToFunc 0x95 = stOp Acc zeroPageXAddr 2 4
opCodeToFunc 0x8d = stOp Acc absoluteAddr  3 4
opCodeToFunc 0x9d = stOp Acc absoluteXAddr 3 5
opCodeToFunc 0x99 = stOp Acc absoluteYAddr 3 5
opCodeToFunc 0x81 = stOp Acc indirectXAddr 2 6
opCodeToFunc 0x91 = stOp Acc indirectYAddr 2 6

opCodeToFunc 0x86 = stOp X zeroPageAddr     2 3
opCodeToFunc 0x96 = stOp X zeroPageYArgAddr 2 4
opCodeToFunc 0x8e = stOp X absoluteAddr     3 4

opCodeToFunc 0x84 = stOp Y zeroPageAddr  2 3
opCodeToFunc 0x94 = stOp Y zeroPageXAddr 2 4
opCodeToFunc 0x8c = stOp Y absoluteAddr  3 4

opCodeToFunc 0x83 = saxOp indirectXAddr    2 5
opCodeToFunc 0x87 = saxOp zeroPageAddr     2 3
opCodeToFunc 0x8f = saxOp absoluteAddr     3 4
opCodeToFunc 0x97 = saxOp zeroPageYArgAddr 2 4

opCodeToFunc 0xaa = transferOp Acc X   1 2
opCodeToFunc 0xa8 = transferOp Acc Y   1 2
opCodeToFunc 0xba = transferOp Sp  X   1 2
opCodeToFunc 0x8a = transferOp X   Acc 1 2
opCodeToFunc 0x9a = transferOp X   Sp  1 2
opCodeToFunc 0x98 = transferOp Y   Acc 1 2

opCodeToFunc 0x48 = pushOp Acc    1 3
opCodeToFunc 0x08 = pushOp Status 1 3

opCodeToFunc 0x68 = pullOp Acc    1 4
opCodeToFunc 0x28 = pullOp Status 1 4

opCodeToFunc 0x20 = jsrOp absoluteAddr 3 6
opCodeToFunc 0x60 = rtsOp 6
opCodeToFunc 0x40 = rtiOp 6

opCodeToFunc 0xc9 = cmpOp immediate Acc 2 2
opCodeToFunc 0xc5 = cmpOp zeroPage  Acc 2 3
opCodeToFunc 0xd5 = cmpOp zeroPageX Acc 2 4
opCodeToFunc 0xcd = cmpOp absolute  Acc 3 4
opCodeToFunc 0xdd = cmpOp absoluteX Acc 3 4 -- +1
opCodeToFunc 0xd9 = cmpOp absoluteY Acc 3 4 -- +1
opCodeToFunc 0xc1 = cmpOp indirectX Acc 2 6
opCodeToFunc 0xd1 = cmpOp indirectY Acc 2 5 -- +1

opCodeToFunc 0xe0 = cmpOp immediate X 2 2
opCodeToFunc 0xe4 = cmpOp zeroPage  X 2 3
opCodeToFunc 0xec = cmpOp absolute  X 3 4

opCodeToFunc 0xc0 = cmpOp immediate Y 2 2
opCodeToFunc 0xc4 = cmpOp zeroPage  Y 2 3
opCodeToFunc 0xcc = cmpOp absolute  Y 3 4

opCodeToFunc 0xe6 = incMemOp zeroPageAddr  1 2 5
opCodeToFunc 0xf6 = incMemOp zeroPageXAddr 1 2 6
opCodeToFunc 0xee = incMemOp absoluteAddr  1 3 6
opCodeToFunc 0xfe = incMemOp absoluteXAddr 1 3 7

opCodeToFunc 0xc6 = incMemOp zeroPageAddr  (-1) 2 5
opCodeToFunc 0xd6 = incMemOp zeroPageXAddr (-1) 2 6
opCodeToFunc 0xce = incMemOp absoluteAddr  (-1) 3 6
opCodeToFunc 0xde = incMemOp absoluteXAddr (-1) 3 7

opCodeToFunc 0xe8 = incOp X   1  1 2
opCodeToFunc 0xc8 = incOp Y   1  1 2
opCodeToFunc 0xca = incOp X (-1) 1 2
opCodeToFunc 0x88 = incOp Y (-1) 1 2

opCodeToFunc 0xf0 = branchOp relative isZeroState             2
opCodeToFunc 0xd0 = branchOp relative (not . isZeroState)     2
opCodeToFunc 0xb0 = branchOp relative isCarryState            2
opCodeToFunc 0x90 = branchOp relative (not . isCarryState)    2
opCodeToFunc 0x10 = branchOp relative (not . isPositiveState) 2
opCodeToFunc 0x30 = branchOp relative isPositiveState         2
opCodeToFunc 0x70 = branchOp relative isOverflowState         2
opCodeToFunc 0x50 = branchOp relative (not . isOverflowState) 2

opCodeToFunc 0x4c = jmpOp absoluteAddr 3 3
opCodeToFunc 0x6c = jmpOp indirectAddr 3 5

opCodeToFunc 0x24 = bitTstOp zeroPageAddr  2 3
opCodeToFunc 0x2c = bitTstOp absoluteAddr  3 4

opCodeToFunc 0x1a = nop_implied
opCodeToFunc 0x3a = nop_implied
opCodeToFunc 0x5a = nop_implied
opCodeToFunc 0x7a = nop_implied
opCodeToFunc 0xda = nop_implied
opCodeToFunc 0xea = nop_implied
opCodeToFunc 0xfa = nop_implied

opCodeToFunc 0x80 = nop_immediate
opCodeToFunc 0x82 = nop_immediate
opCodeToFunc 0x89 = nop_immediate
opCodeToFunc 0xc2 = nop_immediate
opCodeToFunc 0xe2 = nop_immediate

opCodeToFunc 0x04 = nop_zeropage
opCodeToFunc 0x44 = nop_zeropage
opCodeToFunc 0x64 = nop_zeropage

opCodeToFunc 0x0c = nop_absolute

opCodeToFunc 0x14 = nop_zeropage_x
opCodeToFunc 0x34 = nop_zeropage_x
opCodeToFunc 0x54 = nop_zeropage_x
opCodeToFunc 0x74 = nop_zeropage_x
opCodeToFunc 0xd4 = nop_zeropage_x
opCodeToFunc 0xf4 = nop_zeropage_x

opCodeToFunc 0x1c = nop_absolute_x
opCodeToFunc 0x3c = nop_absolute_x
opCodeToFunc 0x5c = nop_absolute_x
opCodeToFunc 0x7c = nop_absolute_x
opCodeToFunc 0xdc = nop_absolute_x
opCodeToFunc 0xfc = nop_absolute_x

opCodeToFunc 0xeb = sbcOp immediateAddr  2 2
opCodeToFunc 0xe9 = sbcOp immediateAddr  2 2
opCodeToFunc 0xe5 = sbcOp zeroPageAddr   2 3
opCodeToFunc 0xf5 = sbcOp zeroPageXAddr  2 4
opCodeToFunc 0xed = sbcOp absoluteAddr   3 4
opCodeToFunc 0xfd = sbcOp absoluteXAddr  3 4 -- +1
opCodeToFunc 0xf9 = sbcOp absoluteYAddr  3 4 -- +1
opCodeToFunc 0xe1 = sbcOp indirectXAddr  2 6
opCodeToFunc 0xf1 = sbcOp indirectYAddr  2 5 -- +1


opCodeToFunc 0x03 = sloOp indirectXAddr 2 8
opCodeToFunc 0x07 = sloOp zeroPageAddr  2 5
opCodeToFunc 0x0f = sloOp absoluteAddr  3 6
opCodeToFunc 0x13 = sloOp indirectYAddr 2 8
opCodeToFunc 0x17 = sloOp zeroPageXAddr 2 6
opCodeToFunc 0x1b = sloOp absoluteYAddr 3 7
opCodeToFunc 0x1f = sloOp absoluteXAddr 3 7

opCodeToFunc 0x23 = rlaOp indirectXAddr 2 8
opCodeToFunc 0x27 = rlaOp zeroPageAddr  2 5
opCodeToFunc 0x2f = rlaOp absoluteAddr  3 6
opCodeToFunc 0x33 = rlaOp indirectYAddr 2 8
opCodeToFunc 0x37 = rlaOp zeroPageXAddr 2 6
opCodeToFunc 0x3b = rlaOp absoluteYAddr 3 7
opCodeToFunc 0x3f = rlaOp absoluteXAddr 3 7

opCodeToFunc 0x43 = sreOp indirectXAddr 2 8
opCodeToFunc 0x47 = sreOp zeroPageAddr  2 5
opCodeToFunc 0x4f = sreOp absoluteAddr  3 6
opCodeToFunc 0x53 = sreOp indirectYAddr 2 8
opCodeToFunc 0x57 = sreOp zeroPageXAddr 2 6
opCodeToFunc 0x5b = sreOp absoluteYAddr 3 7
opCodeToFunc 0x5f = sreOp absoluteXAddr 3 7

opCodeToFunc 0x63 = rraOp indirectXAddr 2 8
opCodeToFunc 0x67 = rraOp zeroPageAddr  2 5
opCodeToFunc 0x6f = rraOp absoluteAddr  3 6
opCodeToFunc 0x73 = rraOp indirectYAddr 2 8
opCodeToFunc 0x77 = rraOp zeroPageXAddr 2 6
opCodeToFunc 0x7b = rraOp absoluteYAddr 3 7
opCodeToFunc 0x7f = rraOp absoluteXAddr 3 7

opCodeToFunc 0xc3 = dcpOp indirectXAddr 2 8
opCodeToFunc 0xc7 = dcpOp zeroPageAddr  2 5
opCodeToFunc 0xcf = dcpOp absoluteAddr  3 6
opCodeToFunc 0xd3 = dcpOp indirectYAddr 2 8
opCodeToFunc 0xd7 = dcpOp zeroPageXAddr 2 6
opCodeToFunc 0xdb = dcpOp absoluteYAddr 3 7
opCodeToFunc 0xdf = dcpOp absoluteXAddr 3 7

opCodeToFunc 0xe3 = iscOp indirectXAddr 2 8
opCodeToFunc 0xe7 = iscOp zeroPageAddr  2 5
opCodeToFunc 0xef = iscOp absoluteAddr  3 6
opCodeToFunc 0xf3 = iscOp indirectYAddr 2 8
opCodeToFunc 0xf7 = iscOp zeroPageXAddr 2 6
opCodeToFunc 0xfb = iscOp absoluteYAddr 3 7
opCodeToFunc 0xff = iscOp absoluteXAddr 3 7


opCodeToFunc 0x0b = ancOp immediateAddr 2 2
opCodeToFunc 0x2b = ancOp immediateAddr 2 2

opCodeToFunc 0x93 = shaOp indirectYAddr 2 2 -- cyc unknown
opCodeToFunc 0x9f = shaOp absoluteYAddr 3 2 -- cyc unknown

opCodeToFunc 0x9c = shyOp absoluteXAddr 3 2 -- cyc unknown
opCodeToFunc 0x9e = shxOp absoluteYAddr 3 2 -- cyc unknown

opCodeToFunc 0x4b = alrOp immediateAddr 2 2

opCodeToFunc 0x6b = arrOp immediateAddr 2 2

opCodeToFunc 0xcb = axsOp immediateAddr 2 2
--
-- opCodeToFunc 0x8b = xaaOp immediateAddr
--
-- opCodeToFunc 0x9b = tasOp absoluteYAddr
--
-- opCodeToFunc 0xbb = lasOp absoluteYAddr



opCodeToFunc opCode = error ("op code " ++ (showHex(opCode) "") ++ " Not implemented")

nop_implied    = nop immediateAddr 1 2
nop_immediate  = nop immediateAddr 2 2
nop_zeropage   = nop zeroPageAddr  2 3
nop_absolute   = nop absoluteAddr  3 4
nop_zeropage_x = nop zeroPageXAddr 2 4
nop_absolute_x = nop absoluteXAddr 3 4




-- Running --------------------------------------------------------------------
-------------------------------------------------------------------------------


trace :: Cpu -> String -> a -> a
trace cpu msg a = if debug cpu
                  then T.trace msg a
                  else a

runCpu :: Cpu -> Cpu
runCpu cpu
    | isDead cpu = cpu
    | otherwise  = runCpu (stepCpu cpu)


isDeadAtExec :: Cpu -> Int -> Bool
isDeadAtExec cpu cn = isDead cpu && cn == 0

isDead :: Cpu -> Bool
isDead cpu = readMem (pc (registers cpu)) (memory cpu) == 0

stepCpu :: Cpu -> Cpu
stepCpu cpu = (opCodeToFunc (trace cpu (debugPrint cpu) opCode)) cpu
  where opCode = nextOpCode cpu

showB v = printf "%02X" v
showW v = printf "%04X" v

debugPrint :: Cpu -> String
debugPrint cpu = unwords [
      ( showW (pc (registers cpu)))
    , ( showB opCode )
    , ( opInfo opCode )
    , ( "a1: " ++ showB (fstArg cpu) )
    , ( "a2: " ++ showB (secArg cpu) )
    , ( textAt 0x6004 cpu )
--  , ( point 0x6000 mem )
--  , ( point 0x6004 mem )
--  , ( show rs )
--  , ( showStatus (status rs) )
  ]
  where mem = memory cpu
        rs = registers cpu
        opCode = nextOpCode cpu

valueAt :: Address -> Cpu -> Int
valueAt addr cpu = readMem addr (memory cpu)

textAt :: Address -> Cpu -> String
textAt addr cpu = map chr validBytes
  where allBytes   = map (\p -> readMem p mem) [addr..]
        validBytes = takeWhile (\v -> v /= 0x00) allBytes
        mem = memory cpu

debugTestStatus cpu = readMem 0x6001 (memory cpu)

point p mem = "*" ++ (showW p) ++ " = " ++ (showW val)
  where val = (shiftL msb 8) + lsb
        msb = (readMem (p+1) mem)
        lsb = (readMem (p) mem)

readMemWord p mem = (shiftL msb 8) + lsb
  where msb = (readMem (p+1) mem)
        lsb = (readMem (p) mem)

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


initCpu :: [Int] -> [(Int,Int)] -> Bool -> Cpu
initCpu program memory debug = Cpu mem regs 0 debug
  where mem = initMem (memory)
        regs = Registers {pc=startAddr, status=0x24, acc=0, x=0, y=0, sp=stackStart}
        startAddr = readMemWord resetVector mem




