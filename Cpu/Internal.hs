module Cpu.Internal(
    Cpu,
    RegisterType(X),    
    stepCpu,
    initCpu,
    runCpu,
    runCpuInteractive,
    immediateArg,
    absoluteArg,
    absoluteArgPtr,    
    pc,
    status,
    acc,
    x,
    y,
    sp,
    registers,
    absoluteXargPtr,
    updateRegister,
    showCpu,
    resetVector,
    isDead,
    isDeadAtExec
) where

import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Data.Int
import Numeric(showHex)
import Mem

data Cpu = Cpu {
  memory :: Memory Int Int,
  registers :: Registers  
} deriving Show

type RegValue = Int

data Flag = Carry
          | Zero
          | IrqDis
          | DecMode
          | BrkCommand
          | Unused
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

data RegisterType = Pc | Status | Acc | X | Y | Sp deriving Eq

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
flagPos Unused = 5
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
stackStart = 0x100
stackEnd = 0x1FF

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

clc :: Cpu -> Cpu
clc = updateFlagOp Carry False 
cld = updateFlagOp DecMode False
cli = updateFlagOp IrqDis False
clv = updateFlagOp OverFlow False
sec = updateFlagOp Carry True
sed = updateFlagOp DecMode True
sei = updateFlagOp IrqDis True

isOverflow :: Int -> Int -> Bool
isOverflow a b = abSum > 255
  where abSum = (fromIntegral(a)::Int) + (fromIntegral(b)::Int)

fstArg :: Cpu -> Int
fstArg (Cpu mem regs) = readMem (pc(regs)+1) mem

secArg :: Cpu -> Int
secArg (Cpu mem regs) = readMem (pc(regs)+2) mem

relativeArg :: Cpu -> Int
relativeArg cpu@(Cpu mem regs) = fromIntegral(fromIntegral((fstArg cpu))::Int8)::Int

immediateArg :: Cpu -> Int
immediateArg = fstArg

absoluteArgPtr :: Cpu -> Int
absoluteArgPtr cpu@(Cpu mem regs) = readMem (absoluteArg cpu) mem
        
absoluteArg :: Cpu -> Int
absoluteArg cpu@(Cpu mem regs) = args16Address cpu
        
args16Address :: Cpu -> Address
args16Address cpu = toAddress (secArg(cpu)) (fstArg(cpu))

args8Address :: Cpu -> Address
args8Address cpu = toAddress (fstArg(cpu)) (secArg(cpu))

absoluteXargPtr :: Cpu -> Int
absoluteXargPtr cpu@(Cpu mem regs) = absoluteRegArgPtr (x(regs)) (args16Address cpu) cpu

absoluteYargPtr :: Cpu -> Int
absoluteYargPtr cpu@(Cpu mem regs) = absoluteRegArgPtr (y(regs)) (args16Address cpu) cpu

add :: Int -> Int -> Int
add a b = (fromIntegral(a)::Int) + b

absoluteRegArgPtr :: RegValue -> Address -> Cpu -> Int
absoluteRegArgPtr regValue address cpu@(Cpu mem regs) = readMem (add regValue address) mem

zeroPageArg :: Cpu -> Int
zeroPageArg cpu = absoluteRegArgPtr 0 (args8Address cpu) cpu

zeroPageXArg :: Cpu -> Int
zeroPageXArg cpu@(Cpu mem regs) = absoluteRegArgPtr (x(regs)) (args8Address cpu) cpu

zeroPageYArg :: Cpu -> Int
zeroPageYArg cpu@(Cpu mem regs) = absoluteRegArgPtr (y(regs)) (args8Address cpu) cpu

accumulatorArg :: Cpu -> Int
accumulatorArg (Cpu _ regs) = acc(regs)

indirectXarg :: Cpu -> Int
indirectXarg cpu = error "not implemented"

indirectYarg :: Cpu -> Int
indirectYarg cpu = error "not implemented"

toAddress :: Int -> Int -> Int
toAddress a b = (Data.Bits..|.) (Data.Bits.shiftL a 8) b

fromAddress :: Int -> (Int,Int)
fromAddress value = (shiftR ((Data.Bits..&.) 0xFF00 value) 8,(Data.Bits..&.) 0x00FF value)
        
add8 :: Int -> Int -> Int
add8 a b = mod (a + b) 256

add16 :: Int -> Int -> Int
add16 a b = mod (a + b) 65536


sub8 :: Int -> Int -> Int
sub8 a b = mod (a - b) 256

updateStatusFlagsNumericOp :: RegValue -> RegValue -> RegValue
updateStatusFlagsNumericOp currentStatus newAcc = newStatus
  where newStatus = updateFlags [(Zero,zeroFlag),
                                 (Neg,negFlag)] currentStatus
        zeroFlag = newAcc == 0
        negFlag = testBit newAcc 7

adcOp :: AddressingMode -> OpSize -> Cpu -> Cpu
adcOp f size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = add8 acc1 acc2 
        acc1 = acc(regs)
        acc2 = f cpu
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc        
        newStatus = updateFlag Carry carryFlag newAccStatus
        carryFlag = isOverflow acc1 acc2

ldOp ::  RegisterType -> AddressingMode -> OpSize -> Cpu -> Cpu
ldOp rType f size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegisters [(rType,newR), (Pc,newPc), (Status,newStatus)] regs
        newR = f cpu
        newPc = pc(regs) + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newR        

stOp ::  RegisterType -> AddressingMode -> OpSize -> Cpu -> Cpu
stOp rType f size cpu@(Cpu mem regs) = Cpu newMem newRegs
  where newRegs = updateRegister Pc newPc regs
        newPc = pc(regs) + size
        memAddress = f cpu
        newMem = writeMem memAddress (readRegister rType regs) mem

bitOp :: AccFuncTwoArg -> AddressingMode -> OpSize -> Cpu -> Cpu
bitOp aluOp f size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = (aluOp) acc1 acc2 
        acc1 = acc(regs)
        acc2 = f cpu
        newPc = pc(regs) + size
        newStatus = updateStatusFlagsNumericOp (status(regs)) newAcc

shiftOp :: AccFuncOneArg -> BitPos -> AddressingMode -> OpSize -> Cpu -> Cpu
shiftOp aluOp bitPos f size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = regs {acc=newAcc, pc=newPc, status=newStatus}
        newAcc = aluOp (f cpu)
        newPc = pc(regs) + size
        newAccStatus = updateStatusFlagsNumericOp (status(regs)) newAcc        
        newStatus = updateFlag Carry carryFlag newAccStatus
        carryFlag = testBit (acc(regs)) bitPos

transferOp :: RegisterType -> RegisterType -> OpSize -> Cpu -> Cpu
transferOp from to size (Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegisters [(to, fromValue), 
                                (Pc,pc(regs) + size),
                                (Status, newStatus)] regs
        fromValue = readRegister from regs
        newStatus = updateStatusFlagsNumericOp (status(regs)) fromValue
        
andOp :: AddressingMode -> OpSize -> Cpu -> Cpu
andOp = bitOp (Data.Bits..&.)

eorOp :: AddressingMode -> OpSize -> Cpu -> Cpu
eorOp = bitOp xor

oraOp :: AddressingMode -> OpSize -> Cpu -> Cpu
oraOp = bitOp (.|.)

aslOp :: AddressingMode -> OpSize -> Cpu -> Cpu
aslOp = shiftOp ((flip shiftL) 1) 7

lsrOp :: AddressingMode -> OpSize -> Cpu -> Cpu
lsrOp = shiftOp ((flip shiftR) 1) 0

rolOp :: AddressingMode -> OpSize -> Cpu -> Cpu
rolOp = shiftOp ((flip rotateL) 1) 7

rorOp :: AddressingMode -> OpSize -> Cpu -> Cpu
rorOp = shiftOp ((flip rotateR) 1) 0

pushOp :: RegisterType -> OpSize -> Cpu -> Cpu
pushOp regType size cpu@(Cpu mem regs) = push regValue cpu
  where regValue = readRegister regType regs

pullOp :: RegisterType -> OpSize -> Cpu -> Cpu
pullOp regType size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegisters [(Status, newStatus),
                                   (Pc, pc(regs)+size)] pulledRegs
        fromStack = readRegister regType newRegs
        newStatus = if regType == Status 
                    then fromStack 
                    else updateStatusFlagsNumericOp (status(regs)) fromStack
        pulledRegs = registers(pull regType cpu)
        
jsrOp :: AddressingMode -> OpSize -> Cpu -> Cpu                         
jsrOp f size cpu@(Cpu mem regs) = Cpu newMem newRegs
  where returnPoint = pc(regs)
        pushed = pushAddr returnPoint cpu
        newMem = memory(pushed)
        newRegs = updateRegister Pc jumpTo (registers(pushed))
        jumpTo = f cpu
        
rtsOp :: Cpu -> Cpu
rtsOp cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs =  updateRegister Pc (pc(pulledRegs)+3) pulledRegs
          where pulledRegs = registers(pullPc cpu)

pull :: RegisterType -> Cpu -> Cpu
pull regType (Cpu mem regs) = Cpu mem newRegs
  where stackValue = readMem (sp(regs)) mem
        newRegs = updateRegisters [(regType,stackValue),
                                   (Sp, sp(regs)+1)] regs
                              
pullPc :: Cpu -> Cpu        
pullPc (Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegisters [(Pc,stackValue),
                                   (Sp, sp(regs)+2)] regs
        stackValue = toAddress high low
        low = readMem (sp(regs)+1) mem
        high = readMem (sp(regs)+2) mem
                         

pushAddr :: Int -> Cpu -> Cpu
pushAddr addr cpu = push addrLow (push addrHigh cpu) 
  where addrHigh = fst(fromAddress(addr))
        addrLow = snd(fromAddress(addr))

  
push :: Int -> Cpu -> Cpu                                     
push value (Cpu mem regs) = Cpu newMem newRegs
  where newMem = writeMem spValue value mem
        spValue = sp(regs)
        newRegs = updateRegister Sp (spValue-1) regs

  
updateFlagOp :: Flag -> Bool -> Cpu -> Cpu
updateFlagOp flag value cpu = Cpu (memory(cpu)) newRegs
  where regs = registers(cpu) 
        newRegs = (regs {pc = pc(regs)+1, status = statusValue})
        statusValue = updateFlag flag value (status(regs))         
        
incOp :: RegisterType -> Int -> Int -> Cpu -> Cpu        
incOp regType value size (Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegisters [(regType, newRegValue),
                                   (Pc,pc(regs)+size),
                                   (Status,newStatus)] regs
        newRegValue = oldRegValue + value
        oldRegValue = readRegister regType regs
        newStatus = updateStatusFlagsNumericOp (status(regs)) oldRegValue

cmpOp :: AddressingMode -> RegisterType -> OpSize -> Cpu -> Cpu
cmpOp f regType size cpu@(Cpu mem regs) = Cpu mem newRegs
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

bneOp :: AddressingMode -> OpSize -> Cpu -> Cpu
bneOp f size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegister Pc newPc regs
        newPc = if zeroClear then newPcValue else pc(regs)+size
        newPcValue = add16 (pc(regs)) (f cpu) 
        zeroClear = not (readFlag Zero (status(regs)))


isZeroState :: Condition
isZeroState (Cpu mem regs) = readFlag Zero (status(regs))

isPositiveState :: Condition
isPositiveState (Cpu mem regs) = readFlag Neg (status(regs))

isOverflowState :: Condition
isOverflowState (Cpu mem regs) = readFlag OverFlow (status(regs))

isCarryState :: Condition
isCarryState (Cpu mem regs) = readFlag Carry (status(regs))

branchOp :: AddressingMode -> Condition -> OpSize -> Cpu -> Cpu
branchOp f c size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegister Pc newPcVal regs
        newPcVal = if doBranch then bPcVal else pcVal
        bPcVal = (add16 (pc(regs)) (f cpu) ) + size
        pcVal = pc(regs)+size
        doBranch = c cpu
        

jmpOp :: AddressingMode -> OpSize -> Cpu -> Cpu
jmpOp f size cpu@(Cpu mem regs) = Cpu mem newRegs
 where newPcVal = f cpu
       newRegs = updateRegister Pc newPcVal regs

nop :: OpSize -> Cpu -> Cpu
nop size cpu@(Cpu mem regs) = Cpu mem newRegs
  where newRegs = updateRegister Pc (pc(regs)+size) regs

bitTstOp :: AddressingMode -> OpSize -> Cpu -> Cpu
bitTstOp f size cpu@(Cpu mem regs) = Cpu mem newRegs
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
        

opCodeToFunc :: OpCode -> (Cpu -> Cpu)
opCodeToFunc 0x18 = clc
opCodeToFunc 0xD8 = cld
opCodeToFunc 0x58 = cli
opCodeToFunc 0xB8 = clv
opCodeToFunc 0x38 = sec
opCodeToFunc 0xF8 = sed
opCodeToFunc 0x78 = sei

opCodeToFunc 0x69 = adcOp immediateArg 2
opCodeToFunc 0x6d = adcOp absoluteArgPtr 3 
opCodeToFunc 0x7d = adcOp absoluteXargPtr 3
opCodeToFunc 0x79 = adcOp absoluteYargPtr 3
opCodeToFunc 0x65 = adcOp zeroPageArg 2
opCodeToFunc 0x75 = adcOp zeroPageXArg 2

opCodeToFunc 0x29 = andOp immediateArg 2
opCodeToFunc 0x25 = andOp zeroPageArg 2 
opCodeToFunc 0x35 = andOp zeroPageXArg 2 
opCodeToFunc 0x2d = andOp absoluteArgPtr 3 
opCodeToFunc 0x3d = andOp absoluteXargPtr 3
opCodeToFunc 0x39 = andOp absoluteYargPtr 3

opCodeToFunc 0x49 = eorOp immediateArg 2
opCodeToFunc 0x45 = eorOp zeroPageArg 2 
opCodeToFunc 0x55 = eorOp zeroPageXArg 2 
opCodeToFunc 0x4f = eorOp absoluteArgPtr 3 
opCodeToFunc 0x5d = eorOp absoluteXargPtr 3
opCodeToFunc 0x59 = eorOp absoluteYargPtr 3

opCodeToFunc 0x09 = oraOp immediateArg 2
opCodeToFunc 0x05 = oraOp zeroPageArg 2 
opCodeToFunc 0x15 = oraOp zeroPageXArg 2 
opCodeToFunc 0x0d = oraOp absoluteArgPtr 3 
opCodeToFunc 0x1d = oraOp absoluteXargPtr 3
opCodeToFunc 0x19 = oraOp absoluteYargPtr 3

opCodeToFunc 0x2a = rolOp accumulatorArg 1
opCodeToFunc 0x26 = rolOp zeroPageArg 2 
opCodeToFunc 0x36 = rolOp zeroPageXArg 2 
opCodeToFunc 0x2e = rolOp absoluteArgPtr 3 
opCodeToFunc 0x3e = rolOp absoluteXargPtr 3

opCodeToFunc 0x6a = rorOp accumulatorArg 1
opCodeToFunc 0x66 = rorOp zeroPageArg 2 
opCodeToFunc 0x76 = rorOp zeroPageXArg 2 
opCodeToFunc 0x6e = rorOp absoluteArgPtr 3 
opCodeToFunc 0x7e = rorOp absoluteXargPtr 3

opCodeToFunc 0x0a = aslOp accumulatorArg 1
opCodeToFunc 0x06 = aslOp zeroPageArg 2 
opCodeToFunc 0x16 = aslOp zeroPageXArg 2 
opCodeToFunc 0x0e = aslOp absoluteArgPtr 3
opCodeToFunc 0x1e = aslOp absoluteXargPtr 3

opCodeToFunc 0x4a = lsrOp accumulatorArg 1
opCodeToFunc 0x46 = lsrOp zeroPageArg 2 
opCodeToFunc 0x56 = lsrOp zeroPageXArg 2 
opCodeToFunc 0x4e = lsrOp absoluteArgPtr 3
opCodeToFunc 0x5e = lsrOp absoluteXargPtr 3

opCodeToFunc 0xa9 = ldOp Acc immediateArg 2
opCodeToFunc 0xa5 = ldOp Acc zeroPageArg 2
opCodeToFunc 0xb5 = ldOp Acc zeroPageXArg 2
opCodeToFunc 0xad = ldOp Acc absoluteArgPtr 3
opCodeToFunc 0xbd = ldOp Acc absoluteXargPtr 3
opCodeToFunc 0xb9 = ldOp Acc absoluteYargPtr 3
opCodeToFunc 0xa1 = ldOp Acc indirectXarg 2
opCodeToFunc 0xb1 = ldOp Acc indirectYarg 2

opCodeToFunc 0xa2 = ldOp X immediateArg 2
opCodeToFunc 0xa6 = ldOp X zeroPageArg 2
opCodeToFunc 0xb6 = ldOp X zeroPageYArg 2
opCodeToFunc 0xae = ldOp X absoluteArgPtr 3
opCodeToFunc 0xbe = ldOp X absoluteYargPtr 3

opCodeToFunc 0xa0 = ldOp Y immediateArg 2
opCodeToFunc 0xa4 = ldOp Y zeroPageArg 2
opCodeToFunc 0xb4 = ldOp Y zeroPageYArg 2
opCodeToFunc 0xac = ldOp Y absoluteArgPtr 3
opCodeToFunc 0xbc = ldOp Y absoluteYargPtr 3

opCodeToFunc 0x85 = stOp Acc zeroPageArg 2
opCodeToFunc 0x95 = stOp Acc zeroPageXArg 2
opCodeToFunc 0x8d = stOp Acc absoluteArgPtr 3
opCodeToFunc 0x9d = stOp Acc absoluteXargPtr 3
opCodeToFunc 0x99 = stOp Acc absoluteYargPtr 3
opCodeToFunc 0x81 = stOp Acc indirectXarg 2
opCodeToFunc 0x91 = stOp Acc indirectYarg 2

opCodeToFunc 0x86 = stOp X zeroPageArg 2
opCodeToFunc 0x96 = stOp X zeroPageYArg 2
opCodeToFunc 0x8e = stOp X absoluteArgPtr 3

opCodeToFunc 0x84 = stOp Y zeroPageArg 2
opCodeToFunc 0x94 = stOp Y zeroPageYArg 2
opCodeToFunc 0x8c = stOp Y absoluteArgPtr 3

opCodeToFunc 0xaa = transferOp Acc X 1
opCodeToFunc 0xa8 = transferOp Acc Y 1
opCodeToFunc 0xba = transferOp Sp X 1
opCodeToFunc 0x8a = transferOp X Acc 1
opCodeToFunc 0x9a = transferOp X Sp 1
opCodeToFunc 0x98 = transferOp Y Acc 1

opCodeToFunc 0x48 = pushOp Acc 1
opCodeToFunc 0x08 = pushOp Status 1

opCodeToFunc 0x68 = pullOp Acc 1
opCodeToFunc 0x28 = pullOp Status 1

opCodeToFunc 0x20 = jsrOp absoluteArg 3
opCodeToFunc 0x60 = rtsOp

opCodeToFunc 0xc9 = cmpOp immediateArg Acc 2
opCodeToFunc 0xc5 = cmpOp zeroPageArg Acc 2
opCodeToFunc 0xd5 = cmpOp zeroPageXArg Acc 2
opCodeToFunc 0xcd = cmpOp absoluteArgPtr Acc 3
opCodeToFunc 0xdd = cmpOp absoluteXargPtr Acc 3
opCodeToFunc 0xd9 = cmpOp absoluteYargPtr Acc 3
opCodeToFunc 0xc1 = cmpOp indirectXarg Acc 2
opCodeToFunc 0xd1 = cmpOp indirectYarg Acc 2

opCodeToFunc 0xe0 = cmpOp immediateArg X 2
opCodeToFunc 0xe4 = cmpOp zeroPageArg X 3
opCodeToFunc 0xec = cmpOp absoluteArgPtr X 4

opCodeToFunc 0xc0 = cmpOp immediateArg Y 2
opCodeToFunc 0xc4 = cmpOp zeroPageArg Y 3
opCodeToFunc 0xcc = cmpOp absoluteArgPtr Y 4

opCodeToFunc 0xe8 = incOp X 1 1
opCodeToFunc 0xc8 = incOp Y 1 1
opCodeToFunc 0xca = incOp X (-1) 1
opCodeToFunc 0x88 = incOp Y (-1) 1

opCodeToFunc 0xf0 = branchOp relativeArg isZeroState 2
opCodeToFunc 0xd0 = branchOp relativeArg (not . isZeroState) 2
opCodeToFunc 0xb0 = branchOp relativeArg isCarryState 2
opCodeToFunc 0x90 = branchOp relativeArg (not . isCarryState) 2
opCodeToFunc 0x30 = branchOp relativeArg (not . isPositiveState) 2
opCodeToFunc 0x10 = branchOp relativeArg isPositiveState 2
opCodeToFunc 0x70 = branchOp relativeArg isOverflowState 2
opCodeToFunc 0x50 = branchOp relativeArg (not . isOverflowState) 2

opCodeToFunc 0x4c = jmpOp absoluteArg 3

opCodeToFunc 0x24 = bitTstOp zeroPageArg 2
opCodeToFunc 0x2c = bitTstOp absoluteArgPtr 2

opCodeToFunc 0xea = nop 1

opCodeToFunc opCode = error ("op code " ++ (showHex(opCode) "") ++ " Not implemented")

runCpu :: Cpu -> Cpu
runCpu cpu
    | isDead cpu = cpu
    | otherwise  = runCpu (stepCpu cpu)


isDeadAtExec :: Cpu -> Int -> Bool
isDeadAtExec cpu@(Cpu mem (Registers pc _ _ _ _ _)) cn = isDead cpu && cn == 0

isDead :: Cpu -> Bool
isDead cpu@(Cpu mem (Registers pc _ _ _ _ _)) = readMem(pc) mem == 0

stepCpu :: Cpu -> Cpu
stepCpu cpu = (opCodeToFunc opCode) cpu
  where opCode = nextOpCode cpu

        
nextOpCode :: Cpu -> Int
nextOpCode (Cpu mem regs) = readMem (pc(regs)) mem

showCpu :: Cpu -> String
showCpu (Cpu mem regs) = (show regs) ++ " pc+1=" ++ mem1 ++ " pc+2=" ++ mem2
  where nextMem n = readMem ((pc(regs))+n) mem
        mem1 = showHex (nextMem 1) ""
        mem2 = showHex (nextMem 2) ""

runCpuInteractive :: Cpu -> IO ()
runCpuInteractive cpu = do
  putStrLn (showCpu cpu)
  putStrLn ("next op code: " ++ showHex (nextOpCode cpu) "")
  wait <- getLine
  runCpuInteractive (stepCpu cpu)


initCpu :: Int -> [Int] -> [(Int,Int)]-> Cpu
initCpu startAddr program memory = Cpu mem regs
  where mem = initMem (cpuProgram ++ memory)
        regs = Registers {pc=startAddr, status=0, acc=0, x=0, y=0, sp=stackStart}
        cpuProgram = zip [startAddr..] program
        




