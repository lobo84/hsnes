import Test.HUnit
import Nes.Internal
import Cpu.Internal
import Ppu


testProgram = TestCase (assertEqual "" (expAcc, expCycles) (actAcc, c))
  where (cpuRun, _, (Cycles cn cd pn c)) = run (initCpu 0 prog [], initPpu)
        regs = registers(cpuRun)
        actAcc = acc(regs)
        expAcc = 24
        expCycles = 24
        prog = [0xa9, 23, 0x69, 1]        


parseTest parse exp reg = TestCase (assertEqual "" exp (parse reg))



tests = TestList [TestLabel "testProgram"         testProgram,
                  TestLabel "testParsePPUCTRL1"   (parseTest parsePPUCTRL   (PPUCTRL False ReadBackdrop Size8x8  X0000 X0000 Add1Across X2000) 0x00),
                  TestLabel "testParsePPUCTRL2"   (parseTest parsePPUCTRL   (PPUCTRL False OutputColor  Size8x8  X1000 X0000 Add32Down  X2800) 0x56),
                  TestLabel "testParsePPUCTRL3"   (parseTest parsePPUCTRL   (PPUCTRL True  OutputColor  Size8x16 X1000 X1000 Add32Down  X2C00) 0xFF),
                  TestLabel "testParsePPUMASK1"   (parseTest parsePPUMASK   (PPUMASK True  False True  False True  False True  False) 0xAA),
                  TestLabel "testParsePPUMASK2"   (parseTest parsePPUMASK   (PPUMASK False False False False False False False False) 0x00),
                  TestLabel "testParsePPUMASK3"   (parseTest parsePPUMASK   (PPUMASK True  True  True  True  True  True  True  True ) 0xFF),
                  TestLabel "testParsePPUSTATUS1" (parseTest parsePPUSTATUS (PPUSTATUS True  False True ) 0xAA),
                  TestLabel "testParsePPUSTATUS2" (parseTest parsePPUSTATUS (PPUSTATUS False False False) 0x00),
                  TestLabel "testParsePPUSTATUS3" (parseTest parsePPUSTATUS (PPUSTATUS True  True  True ) 0xFF)
                  ]