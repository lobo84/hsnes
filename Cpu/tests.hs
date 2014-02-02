
import Test.HUnit
import Cpu

-- LDA #$01
-- STA $0200
-- LDA #$05
-- STA $0201
-- LDA #$08
-- STA $0202
prog = [0xa9,0x01,0x8d,0x00,0x02,0xa9,0x05,0x8d,0x01,0x02,0xa9,0x08,0x8d,0x02,0x02]

testProgram = TestCase (assertEqual "cpu" actualAcc expectedAcc )
  where cpuRun = (runCpu (initCpu 0 prog []))
        regs = registers(cpuRun)
        actualAcc = acc(regs)
        expectedAcc = 0x08



tests = TestList [TestLabel "testProgram" testProgram]