
module Cpu.OpInfo where

opInfo :: Int -> String
opInfo 0x00 = "BRK        "  --  $00: bytes: 0 cycles: 0 _____=>_____ __
opInfo 0x01 = "ORA izx    "  --  $01: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0x02 = "*KIL       "  --  $02: CRASH
opInfo 0x03 = "*SLO izx   "  --  $03: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0x04 = "*NOP zp    "  --  $04: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x05 = "ORA zp     "  --  $05: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x06 = "ASL zp     "  --  $06: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0x07 = "*SLO zp    "  --  $07: bytes: 2 cycles: 5 A____=>A___P RW zp
opInfo 0x08 = "PHP        "  --  $08: bytes: 1 cycles: 3 ___SP=>___S_ _W
opInfo 0x09 = "ORA imm    "  --  $09: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0x0A = "ASL        "  --  $0A: bytes: 1 cycles: 2 A____=>A___P __
opInfo 0x0B = "*ANC imm   "  --  $0B: bytes: 2 cycles: 2 A____=>____P __
opInfo 0x0C = "*NOP abs   "  --  $0C: bytes: 3 cycles: 4 _____=>_____ R_ abs
opInfo 0x0D = "ORA abs    "  --  $0D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x0E = "ASL abs    "  --  $0E: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0x0F = "*SLO abs   "  --  $0F: bytes: 3 cycles: 6 A____=>A___P RW abs
opInfo 0x10 = "BPL rel  * "  --  $10: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x11 = "ORA izy  * "  --  $11: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0x12 = "*KIL       "  --  $12: CRASH
opInfo 0x13 = "*SLO izy   "  --  $13: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0x14 = "*NOP zpx   "  --  $14: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x15 = "ORA zpx    "  --  $15: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x16 = "ASL zpx    "  --  $16: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0x17 = "*SLO zpx   "  --  $17: bytes: 2 cycles: 6 A____=>A___P RW zpx
opInfo 0x18 = "CLC        "  --  $18: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x19 = "ORA aby  * "  --  $19: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x1A = "*NOP       "  --  $1A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x1B = "*SLO aby   "  --  $1B: bytes: 3 cycles: 7 A____=>A___P RW absy
opInfo 0x1C = "*NOP abx  *"  --  $1C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x1D = "ORA abx  * "  --  $1D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x1E = "ASL abx    "  --  $1E: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0x1F = "*SLO abx   "  --  $1F: bytes: 3 cycles: 7 A____=>A___P RW absx
opInfo 0x20 = "JSR abs    "  --  $20: bytes: X cycles: 6 ___S_=>___S_ _W
opInfo 0x21 = "AND izx    "  --  $21: bytes: 2 cycles: 6 _____=>A___P R_ izx
opInfo 0x22 = "*KIL       "  --  $22: CRASH
opInfo 0x23 = "*RLA izx   "  --  $23: bytes: 2 cycles: 8 ____P=>A___P RW izx
opInfo 0x24 = "BIT zp     "  --  $24: bytes: 2 cycles: 3 A____=>____P R_ zp
opInfo 0x25 = "AND zp     "  --  $25: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x26 = "ROL zp     "  --  $26: bytes: 2 cycles: 5 ____P=>____P RW zp
opInfo 0x27 = "*RLA zp    "  --  $27: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0x28 = "PLP        "  --  $28: bytes: 1 cycles: 4 ___S_=>___SP __
opInfo 0x29 = "AND imm    "  --  $29: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x2A = "ROL        "  --  $2A: bytes: 1 cycles: 2 A___P=>A___P __
opInfo 0x2B = "*ANC imm   "  --  $2B: bytes: 2 cycles: 2 A____=>____P __
opInfo 0x2C = "BIT abs    "  --  $2C: bytes: 3 cycles: 4 A____=>____P R_ abs
opInfo 0x2D = "AND abs    "  --  $2D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x2E = "ROL abs    "  --  $2E: bytes: 3 cycles: 6 ____P=>____P RW abs
opInfo 0x2F = "*RLA abs   "  --  $2F: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0x30 = "BMI rel  * "  --  $30: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x31 = "AND izy  * "  --  $31: bytes: 2 cycles: 5 _____=>A___P R_ izy
opInfo 0x32 = "*KIL       "  --  $32: CRASH
opInfo 0x33 = "*RLA izy   "  --  $33: bytes: 2 cycles: 8 ____P=>A___P RW izy
opInfo 0x34 = "*NOP zpx   "  --  $34: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x35 = "AND zpx    "  --  $35: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x36 = "ROL zpx    "  --  $36: bytes: 2 cycles: 6 ____P=>____P RW zpx
opInfo 0x37 = "*RLA zpx   "  --  $37: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0x38 = "SEC        "  --  $38: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x39 = "AND aby  * "  --  $39: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x3A = "*NOP       "  --  $3A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x3B = "*RLA aby   "  --  $3B: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0x3C = "*NOP abx  *"  --  $3C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x3D = "AND abx  * "  --  $3D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x3E = "ROL abx    "  --  $3E: bytes: 3 cycles: 7 ____P=>____P RW absx
opInfo 0x3F = "*RLA abx   "  --  $3F: bytes: 3 cycles: 7 A___P=>A___P RW absx
opInfo 0x40 = "RTI        "  --  $40: bytes: X cycles: 6 ___S_=>___SP __
opInfo 0x41 = "EOR izx    "  --  $41: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0x42 = "*KIL       "  --  $42: CRASH
opInfo 0x43 = "*SRE izx   "  --  $43: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0x44 = "*NOP zp    "  --  $44: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x45 = "EOR zp     "  --  $45: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x46 = "LSR zp     "  --  $46: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0x47 = "*SRE zp    "  --  $47: bytes: 2 cycles: 5 A____=>A___P RW zp
opInfo 0x48 = "PHA        "  --  $48: bytes: 1 cycles: 3 A__S_=>___S_ _W
opInfo 0x49 = "EOR imm    "  --  $49: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x4A = "LSR        "  --  $4A: bytes: 1 cycles: 2 A____=>A___P __
opInfo 0x4B = "*ALR imm   "  --  $4B: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x4C = "JMP abs    "  --  $4C: bytes: X cycles: 3 _____=>_____ __
opInfo 0x4D = "EOR abs    "  --  $4D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x4E = "LSR abs    "  --  $4E: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0x4F = "*SRE abs   "  --  $4F: bytes: 3 cycles: 6 A____=>A___P RW abs
opInfo 0x50 = "BVC rel  * "  --  $50: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x51 = "EOR izy  * "  --  $51: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0x52 = "*KIL       "  --  $52: CRASH
opInfo 0x53 = "*SRE izy   "  --  $53: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0x54 = "*NOP zpx   "  --  $54: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x55 = "EOR zpx    "  --  $55: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x56 = "LSR zpx    "  --  $56: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0x57 = "*SRE zpx   "  --  $57: bytes: 2 cycles: 6 A____=>A___P RW zpx
opInfo 0x58 = "CLI        "  --  $58: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x59 = "EOR aby  * "  --  $59: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x5A = "*NOP       "  --  $5A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x5B = "*SRE aby   "  --  $5B: bytes: 3 cycles: 7 A____=>A___P RW absy
opInfo 0x5C = "*NOP abx  *"  --  $5C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x5D = "EOR abx  * "  --  $5D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x5E = "LSR abx    "  --  $5E: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0x5F = "*SRE abx   "  --  $5F: bytes: 3 cycles: 7 A____=>A___P RW absx
opInfo 0x60 = "RTS        "  --  $60: bytes: X cycles: 6 ___S_=>___S_ __
opInfo 0x61 = "ADC izx    "  --  $61: bytes: 2 cycles: 6 A___P=>A___P R_ izx
opInfo 0x62 = "*KIL       "  --  $62: CRASH
opInfo 0x63 = "*RRA izx   "  --  $63: bytes: 2 cycles: 8 A___P=>A___P RW izx
opInfo 0x64 = "*NOP zp    "  --  $64: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x65 = "ADC zp     "  --  $65: bytes: 2 cycles: 3 A___P=>A___P R_ zp
opInfo 0x66 = "ROR zp     "  --  $66: bytes: 2 cycles: 5 ____P=>____P RW zp
opInfo 0x67 = "*RRA zp    "  --  $67: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0x68 = "PLA        "  --  $68: bytes: 1 cycles: 4 ___S_=>A__SP __
opInfo 0x69 = "ADC imm    "  --  $69: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0x6A = "ROR        "  --  $6A: bytes: 1 cycles: 2 A___P=>A___P __
opInfo 0x6B = "*ARR imm   "  --  $6B: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0x6C = "JMP ind    "  --  $6C: bytes: X cycles: 5 _____=>_____ __
opInfo 0x6D = "ADC abs    "  --  $6D: bytes: 3 cycles: 4 A___P=>A___P R_ abs
opInfo 0x6E = "ROR abs    "  --  $6E: bytes: 3 cycles: 6 ____P=>____P RW abs
opInfo 0x6F = "*RRA abs   "  --  $6F: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0x70 = "BVS rel  * "  --  $70: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x71 = "ADC izy  * "  --  $71: bytes: 2 cycles: 5 A___P=>A___P R_ izy
opInfo 0x72 = "*KIL       "  --  $72: CRASH
opInfo 0x73 = "*RRA izy   "  --  $73: bytes: 2 cycles: 8 A___P=>A___P RW izy
opInfo 0x74 = "*NOP zpx   "  --  $74: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x75 = "ADC zpx    "  --  $75: bytes: 2 cycles: 4 A___P=>A___P R_ zpx
opInfo 0x76 = "ROR zpx    "  --  $76: bytes: 2 cycles: 6 ____P=>____P RW zpx
opInfo 0x77 = "*RRA zpx   "  --  $77: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0x78 = "SEI        "  --  $78: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x79 = "ADC aby  * "  --  $79: bytes: 3 cycles: 4 A___P=>A___P R_ absy
opInfo 0x7A = "*NOP       "  --  $7A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x7B = "*RRA aby   "  --  $7B: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0x7C = "*NOP abx  *"  --  $7C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x7D = "ADC abx  * "  --  $7D: bytes: 3 cycles: 4 A___P=>A___P R_ absx
opInfo 0x7E = "ROR abx    "  --  $7E: bytes: 3 cycles: 7 ____P=>____P RW absx
opInfo 0x7F = "*RRA abx   "  --  $7F: bytes: 3 cycles: 7 A___P=>A___P RW absx
opInfo 0x80 = "*NOP imm   "  --  $80: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x81 = "STA izx    "  --  $81: bytes: 2 cycles: 6 A____=>_____ RW izx
opInfo 0x82 = "*NOP imm   "  --  $82: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x83 = "*SAX izx   "  --  $83: bytes: 2 cycles: 6 _____=>_____ RW izx
opInfo 0x84 = "STY zp     "  --  $84: bytes: 2 cycles: 3 __Y__=>_____ _W zp
opInfo 0x85 = "STA zp     "  --  $85: bytes: 2 cycles: 3 A____=>_____ _W zp
opInfo 0x86 = "STX zp     "  --  $86: bytes: 2 cycles: 3 _X___=>_____ _W zp
opInfo 0x87 = "*SAX zp    "  --  $87: bytes: 2 cycles: 3 _____=>_____ _W zp
opInfo 0x88 = "DEY        "  --  $88: bytes: 1 cycles: 2 __Y__=>__Y_P __
opInfo 0x89 = "*NOP imm   "  --  $89: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x8A = "TXA        "  --  $8A: bytes: 1 cycles: 2 _X___=>A___P __
opInfo 0x8B = "*XAA imm   "  --  $8B: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0x8C = "STY abs    "  --  $8C: bytes: 3 cycles: 4 __Y__=>_____ _W abs
opInfo 0x8D = "STA abs    "  --  $8D: bytes: 3 cycles: 4 A____=>_____ _W abs
opInfo 0x8E = "STX abs    "  --  $8E: bytes: 3 cycles: 4 _X___=>_____ _W abs
opInfo 0x8F = "*SAX abs   "  --  $8F: bytes: 3 cycles: 4 _____=>_____ _W abs
opInfo 0x90 = "BCC rel  * "  --  $90: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x91 = "STA izy    "  --  $91: bytes: 2 cycles: 6 A____=>_____ RW izy
opInfo 0x92 = "*KIL       "  --  $92: CRASH
opInfo 0x93 = "*AHX izy   "  --  $93: bytes: 2 cycles: 6 _____=>_____ RW izy
opInfo 0x94 = "STY zpx    "  --  $94: bytes: 2 cycles: 4 __Y__=>_____ RW zpx
opInfo 0x95 = "STA zpx    "  --  $95: bytes: 2 cycles: 4 A____=>_____ RW zpx
opInfo 0x96 = "STX zpy    "  --  $96: bytes: 2 cycles: 4 _X___=>_____ RW zpy
opInfo 0x97 = "*SAX zpy   "  --  $97: bytes: 2 cycles: 4 _____=>_____ RW zpy
opInfo 0x98 = "TYA        "  --  $98: bytes: 1 cycles: 2 __Y__=>A___P __
opInfo 0x99 = "STA aby    "  --  $99: bytes: 3 cycles: 5 A____=>_____ RW absy
opInfo 0x9A = "TXS        "  --  $9A: bytes: X cycles: 2 _X___=>___S_ __
opInfo 0x9B = "*TAS aby   "  --  $9B: bytes: X cycles: 5 __Y__=>___S_ _W
opInfo 0x9C = "*SHY abx   "  --  $9C: bytes: 3 cycles: 5 __Y__=>_____ RW absx
opInfo 0x9D = "STA abx    "  --  $9D: bytes: 3 cycles: 5 A____=>_____ RW absx
opInfo 0x9E = "*SHX aby   "  --  $9E: bytes: 3 cycles: 5 _X___=>_____ RW absy
opInfo 0x9F = "*AHX aby   "  --  $9F: bytes: 3 cycles: 5 _____=>_____ RW absy
opInfo 0xA0 = "LDY imm    "  --  $A0: bytes: 2 cycles: 2 _____=>__Y_P __
opInfo 0xA1 = "LDA izx    "  --  $A1: bytes: 2 cycles: 6 _____=>A___P R_ izx
opInfo 0xA2 = "LDX imm    "  --  $A2: bytes: 2 cycles: 2 _____=>_X__P __
opInfo 0xA3 = "*LAX izx   "  --  $A3: bytes: 2 cycles: 6 _____=>AX__P R_ izx
opInfo 0xA4 = "LDY zp     "  --  $A4: bytes: 2 cycles: 3 _____=>__Y_P R_ zp
opInfo 0xA5 = "LDA zp     "  --  $A5: bytes: 2 cycles: 3 _____=>A___P R_ zp
opInfo 0xA6 = "LDX zp     "  --  $A6: bytes: 2 cycles: 3 _____=>_X__P R_ zp
opInfo 0xA7 = "*LAX zp    "  --  $A7: bytes: 2 cycles: 3 _____=>AX__P R_ zp
opInfo 0xA8 = "TAY        "  --  $A8: bytes: 1 cycles: 2 A____=>__Y_P __
opInfo 0xA9 = "LDA imm    "  --  $A9: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0xAA = "TAX        "  --  $AA: bytes: 1 cycles: 2 A____=>_X__P __
opInfo 0xAB = "*LAX imm   "  --  $AB: bytes: 2 cycles: 2 A____=>AX__P __
opInfo 0xAC = "LDY abs    "  --  $AC: bytes: 3 cycles: 4 _____=>__Y_P R_ abs
opInfo 0xAD = "LDA abs    "  --  $AD: bytes: 3 cycles: 4 _____=>A___P R_ abs
opInfo 0xAE = "LDX abs    "  --  $AE: bytes: 3 cycles: 4 _____=>_X__P R_ abs
opInfo 0xAF = "*LAX abs   "  --  $AF: bytes: 3 cycles: 4 _____=>AX__P R_ abs
opInfo 0xB0 = "BCS rel  * "  --  $B0: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xB1 = "LDA izy  * "  --  $B1: bytes: 2 cycles: 5 _____=>A___P R_ izy
opInfo 0xB2 = "*KIL       "  --  $B2: CRASH
opInfo 0xB3 = "*LAX izy  *"  --  $B3: bytes: 2 cycles: 5 _____=>AX__P R_ izy
opInfo 0xB4 = "LDY zpx    "  --  $B4: bytes: 2 cycles: 4 _____=>__Y_P R_ zpx
opInfo 0xB5 = "LDA zpx    "  --  $B5: bytes: 2 cycles: 4 _____=>A___P R_ zpx
opInfo 0xB6 = "LDX zpy    "  --  $B6: bytes: 2 cycles: 4 _____=>_X__P R_ zpy
opInfo 0xB7 = "*LAX zpy   "  --  $B7: bytes: 2 cycles: 4 _____=>AX__P R_ zpy
opInfo 0xB8 = "CLV        "  --  $B8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xB9 = "LDA aby  * "  --  $B9: bytes: 3 cycles: 4 _____=>A___P R_ absy
opInfo 0xBA = "TSX        "  --  $BA: bytes: 1 cycles: 2 ___S_=>_X__P __
opInfo 0xBB = "*LAS aby  *"  --  $BB: bytes: 3 cycles: 4 ___S_=>AX_SP R_ absy
opInfo 0xBC = "LDY abx  * "  --  $BC: bytes: 3 cycles: 4 _____=>__Y_P R_ absx
opInfo 0xBD = "LDA abx  * "  --  $BD: bytes: 3 cycles: 4 _____=>A___P R_ absx
opInfo 0xBE = "LDX aby  * "  --  $BE: bytes: 3 cycles: 4 _____=>_X__P R_ absy
opInfo 0xBF = "*LAX aby  *"  --  $BF: bytes: 3 cycles: 4 _____=>AX__P R_ absy
opInfo 0xC0 = "CPY imm    "  --  $C0: bytes: 2 cycles: 2 __Y__=>____P __
opInfo 0xC1 = "CMP izx    "  --  $C1: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0xC2 = "*NOP imm   "  --  $C2: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xC3 = "*DCP izx   "  --  $C3: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0xC4 = "CPY zp     "  --  $C4: bytes: 2 cycles: 3 __Y__=>____P R_ zp
opInfo 0xC5 = "CMP zp     "  --  $C5: bytes: 2 cycles: 3 A____=>____P R_ zp
opInfo 0xC6 = "DEC zp     "  --  $C6: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0xC7 = "*DCP zp    "  --  $C7: bytes: 2 cycles: 5 A____=>____P RW zp
opInfo 0xC8 = "INY        "  --  $C8: bytes: 1 cycles: 2 __Y__=>__Y_P __
opInfo 0xC9 = "CMP imm    "  --  $C9: bytes: 2 cycles: 2 A____=>____P __
opInfo 0xCA = "DEX        "  --  $CA: bytes: 1 cycles: 2 _X___=>_X__P __
opInfo 0xCB = "*AXS imm   "  --  $CB: bytes: 2 cycles: 2 _____=>_X__P __
opInfo 0xCC = "CPY abs    "  --  $CC: bytes: 3 cycles: 4 __Y__=>____P R_ abs
opInfo 0xCD = "CMP abs    "  --  $CD: bytes: 3 cycles: 4 A____=>____P R_ abs
opInfo 0xCE = "DEC abs    "  --  $CE: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0xCF = "*DCP abs   "  --  $CF: bytes: 3 cycles: 6 A____=>____P RW abs
opInfo 0xD0 = "BNE rel  * "  --  $D0: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0xD1 = "CMP izy  * "  --  $D1: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0xD2 = "*KIL       "  --  $D2: CRASH
opInfo 0xD3 = "*DCP izy   "  --  $D3: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0xD4 = "*NOP zpx   "  --  $D4: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0xD5 = "CMP zpx    "  --  $D5: bytes: 2 cycles: 4 A____=>____P R_ zpx
opInfo 0xD6 = "DEC zpx    "  --  $D6: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0xD7 = "*DCP zpx   "  --  $D7: bytes: 2 cycles: 6 A____=>____P RW zpx
opInfo 0xD8 = "CLD        "  --  $D8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xD9 = "CMP aby  * "  --  $D9: bytes: 3 cycles: 4 A____=>____P R_ absy
opInfo 0xDA = "*NOP       "  --  $DA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xDB = "*DCP aby   "  --  $DB: bytes: 3 cycles: 7 A____=>____P RW absy
opInfo 0xDC = "*NOP abx  *"  --  $DC: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0xDD = "CMP abx  * "  --  $DD: bytes: 3 cycles: 4 A____=>____P R_ absx
opInfo 0xDE = "DEC abx    "  --  $DE: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0xDF = "*DCP abx   "  --  $DF: bytes: 3 cycles: 7 A____=>____P RW absx
opInfo 0xE0 = "CPX imm    "  --  $E0: bytes: 2 cycles: 2 _X___=>____P __
opInfo 0xE1 = "SBC izx    "  --  $E1: bytes: 2 cycles: 6 A___P=>A___P R_ izx
opInfo 0xE2 = "*NOP imm   "  --  $E2: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xE3 = "*ISC izx   "  --  $E3: bytes: 2 cycles: 8 A___P=>A___P RW izx
opInfo 0xE4 = "CPX zp     "  --  $E4: bytes: 2 cycles: 3 _X___=>____P R_ zp
opInfo 0xE5 = "SBC zp     "  --  $E5: bytes: 2 cycles: 3 A___P=>A___P R_ zp
opInfo 0xE6 = "INC zp     "  --  $E6: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0xE7 = "*ISC zp    "  --  $E7: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0xE8 = "INX        "  --  $E8: bytes: 1 cycles: 2 _X___=>_X__P __
opInfo 0xE9 = "SBC imm    "  --  $E9: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0xEA = "NOP        "  --  $EA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xEB = "*SBC imm   "  --  $EB: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0xEC = "CPX abs    "  --  $EC: bytes: 3 cycles: 4 _X___=>____P R_ abs
opInfo 0xED = "SBC abs    "  --  $ED: bytes: 3 cycles: 4 A___P=>A___P R_ abs
opInfo 0xEE = "INC abs    "  --  $EE: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0xEF = "*ISC abs   "  --  $EF: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0xF0 = "BEQ rel  * "  --  $F0: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xF1 = "SBC izy  * "  --  $F1: bytes: 2 cycles: 5 A___P=>A___P R_ izy
opInfo 0xF2 = "*KIL       "  --  $F2: CRASH
opInfo 0xF3 = "*ISC izy   "  --  $F3: bytes: 2 cycles: 8 A___P=>A___P RW izy
opInfo 0xF4 = "*NOP zpx   "  --  $F4: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0xF5 = "SBC zpx    "  --  $F5: bytes: 2 cycles: 4 A___P=>A___P R_ zpx
opInfo 0xF6 = "INC zpx    "  --  $F6: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0xF7 = "*ISC zpx   "  --  $F7: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0xF8 = "SED        "  --  $F8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xF9 = "SBC aby  * "  --  $F9: bytes: 3 cycles: 4 A___P=>A___P R_ absy
opInfo 0xFA = "*NOP       "  --  $FA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xFB = "*ISC aby   "  --  $FB: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0xFC = "*NOP abx  *"  --  $FC: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0xFD = "SBC abx  * "  --  $FD: bytes: 3 cycles: 4 A___P=>A___P R_ absx
opInfo 0xFE = "INC abx    "  --  $FE: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0xFF = "*ISC abx   "  --  $FF: bytes: 3 cycles: 7 A___P=>A___P RW absx
