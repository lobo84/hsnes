
import Test.HUnit
import Cpu.Internal


testProgram = TestCase (assertEqual "" expectedAcc actualAcc)
  where cpuRun = (runCpu (initCpu 0 prog []))
        regs = registers(cpuRun)
        actualAcc = acc(regs)
        expectedAcc = 0x08
        -- LDA #$01
        -- STA $0200
        -- LDA #$05
        -- STA $0201
        -- LDA #$08
        -- STA $0202
        prog = [0xa9,0x01,0x8d,0x00,0x02,0xa9,0x05,0x8d,0x01,0x02,0xa9,0x08,0x8d,0x02,0x02]

immediateArgTest = TestCase (assertEqual "" expected actual)
  where cpu = (initCpu 0 [0x01, 0x02] [])
        actual = immediateArg cpu 
        expected = 0x02
        
absoluteArgTest = TestCase (assertEqual "" expected actual)
  where cpu = (initCpu 0 [0x01, 0x10, 0x10] [(0x1010,0x04),(0x1020,0x01)])
        actual = absoluteArg cpu 
        expected = 0x04

absoluteXArgTest = TestCase (assertEqual "" expected actual)
  where cpu = memSetCpu{registers=regs}
        memSetCpu = (initCpu 0 [0x01, 0x10, 0x20] [(0x2012,0x04)])
        regs = updateRegister X xVal (registers(memSetCpu))
        actual = absoluteXarg cpu 
        xVal = 2
        expected = 0x04
        


tests = TestList [TestLabel "testProgram" testProgram,
                  TestLabel "immediateArgTest" immediateArgTest,
                  TestLabel "absoluteArgTest" absoluteArgTest,
                  TestLabel "absoluteXArgTest" absoluteXArgTest]