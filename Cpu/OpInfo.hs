
module Cpu.OpInfo where

opInfo :: Int -> String
opInfo 0x00 = "BRK 7      "  --  $00: bytes: 0 cycles: 0 _____=>_____ __
opInfo 0x01 = "ORA izx 6  "  --  $01: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0x02 = "*KIL       "  --  $02: CRASH
opInfo 0x03 = "*SLO izx 8 "  --  $03: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0x04 = "*NOP zp 3  "  --  $04: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x05 = "ORA zp 3   "  --  $05: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x06 = "ASL zp 5   "  --  $06: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0x07 = "*SLO zp 5  "  --  $07: bytes: 2 cycles: 5 A____=>A___P RW zp
opInfo 0x08 = "PHP 3      "  --  $08: bytes: 1 cycles: 3 ___SP=>___S_ _W
opInfo 0x09 = "ORA imm 2  "  --  $09: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0x0A = "ASL 2      "  --  $0A: bytes: 1 cycles: 2 A____=>A___P __
opInfo 0x0B = "*ANC imm 2 "  --  $0B: bytes: 2 cycles: 2 A____=>____P __
opInfo 0x0C = "*NOP abs 4 "  --  $0C: bytes: 3 cycles: 4 _____=>_____ R_ abs
opInfo 0x0D = "ORA abs 4  "  --  $0D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x0E = "ASL abs 6  "  --  $0E: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0x0F = "*SLO abs 6 "  --  $0F: bytes: 3 cycles: 6 A____=>A___P RW abs
opInfo 0x10 = "BPL rel 2* "  --  $10: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x11 = "ORA izy 5* "  --  $11: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0x12 = "*KIL       "  --  $12: CRASH
opInfo 0x13 = "*SLO izy 8 "  --  $13: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0x14 = "*NOP zpx 4 "  --  $14: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x15 = "ORA zpx 4  "  --  $15: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x16 = "ASL zpx 6  "  --  $16: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0x17 = "*SLO zpx 6 "  --  $17: bytes: 2 cycles: 6 A____=>A___P RW zpx
opInfo 0x18 = "CLC 2      "  --  $18: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x19 = "ORA aby 4* "  --  $19: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x1A = "*NOP 2     "  --  $1A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x1B = "*SLO aby 7 "  --  $1B: bytes: 3 cycles: 7 A____=>A___P RW absy
opInfo 0x1C = "*NOP abx 4*"  --  $1C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x1D = "ORA abx 4* "  --  $1D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x1E = "ASL abx 7  "  --  $1E: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0x1F = "*SLO abx 7 "  --  $1F: bytes: 3 cycles: 7 A____=>A___P RW absx
opInfo 0x20 = "JSR abs 6  "  --  $20: bytes: X cycles: 6 ___S_=>___S_ _W
opInfo 0x21 = "AND izx 6  "  --  $21: bytes: 2 cycles: 6 _____=>A___P R_ izx
opInfo 0x22 = "*KIL       "  --  $22: CRASH
opInfo 0x23 = "*RLA izx 8 "  --  $23: bytes: 2 cycles: 8 ____P=>A___P RW izx
opInfo 0x24 = "BIT zp 3   "  --  $24: bytes: 2 cycles: 3 A____=>____P R_ zp
opInfo 0x25 = "AND zp 3   "  --  $25: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x26 = "ROL zp 5   "  --  $26: bytes: 2 cycles: 5 ____P=>____P RW zp
opInfo 0x27 = "*RLA zp 5  "  --  $27: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0x28 = "PLP 4      "  --  $28: bytes: 1 cycles: 4 ___S_=>___SP __
opInfo 0x29 = "AND imm 2  "  --  $29: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x2A = "ROL 2      "  --  $2A: bytes: 1 cycles: 2 A___P=>A___P __
opInfo 0x2B = "*ANC imm 2 "  --  $2B: bytes: 2 cycles: 2 A____=>____P __
opInfo 0x2C = "BIT abs 4  "  --  $2C: bytes: 3 cycles: 4 A____=>____P R_ abs
opInfo 0x2D = "AND abs 4  "  --  $2D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x2E = "ROL abs 6  "  --  $2E: bytes: 3 cycles: 6 ____P=>____P RW abs
opInfo 0x2F = "*RLA abs 6 "  --  $2F: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0x30 = "BMI rel 2* "  --  $30: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x31 = "AND izy 5* "  --  $31: bytes: 2 cycles: 5 _____=>A___P R_ izy
opInfo 0x32 = "*KIL       "  --  $32: CRASH
opInfo 0x33 = "*RLA izy 8 "  --  $33: bytes: 2 cycles: 8 ____P=>A___P RW izy
opInfo 0x34 = "*NOP zpx 4 "  --  $34: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x35 = "AND zpx 4  "  --  $35: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x36 = "ROL zpx 6  "  --  $36: bytes: 2 cycles: 6 ____P=>____P RW zpx
opInfo 0x37 = "*RLA zpx 6 "  --  $37: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0x38 = "SEC 2      "  --  $38: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x39 = "AND aby 4* "  --  $39: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x3A = "*NOP 2     "  --  $3A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x3B = "*RLA aby 7 "  --  $3B: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0x3C = "*NOP abx 4*"  --  $3C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x3D = "AND abx 4* "  --  $3D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x3E = "ROL abx 7  "  --  $3E: bytes: 3 cycles: 7 ____P=>____P RW absx
opInfo 0x3F = "*RLA abx 7 "  --  $3F: bytes: 3 cycles: 7 A___P=>A___P RW absx
opInfo 0x40 = "RTI 6      "  --  $40: bytes: X cycles: 6 ___S_=>___SP __
opInfo 0x41 = "EOR izx 6  "  --  $41: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0x42 = "*KIL       "  --  $42: CRASH
opInfo 0x43 = "*SRE izx 8 "  --  $43: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0x44 = "*NOP zp 3  "  --  $44: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x45 = "EOR zp 3   "  --  $45: bytes: 2 cycles: 3 A____=>A___P R_ zp
opInfo 0x46 = "LSR zp 5   "  --  $46: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0x47 = "*SRE zp 5  "  --  $47: bytes: 2 cycles: 5 A____=>A___P RW zp
opInfo 0x48 = "PHA 3      "  --  $48: bytes: 1 cycles: 3 A__S_=>___S_ _W
opInfo 0x49 = "EOR imm 2  "  --  $49: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x4A = "LSR 2      "  --  $4A: bytes: 1 cycles: 2 A____=>A___P __
opInfo 0x4B = "*ALR imm 2 "  --  $4B: bytes: 2 cycles: 2 A____=>A___P __
opInfo 0x4C = "JMP abs 3  "  --  $4C: bytes: X cycles: 3 _____=>_____ __
opInfo 0x4D = "EOR abs 4  "  --  $4D: bytes: 3 cycles: 4 A____=>A___P R_ abs
opInfo 0x4E = "LSR abs 6  "  --  $4E: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0x4F = "*SRE abs 6 "  --  $4F: bytes: 3 cycles: 6 A____=>A___P RW abs
opInfo 0x50 = "BVC rel 2* "  --  $50: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x51 = "EOR izy 5* "  --  $51: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0x52 = "*KIL       "  --  $52: CRASH
opInfo 0x53 = "*SRE izy 8 "  --  $53: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0x54 = "*NOP zpx 4 "  --  $54: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x55 = "EOR zpx 4  "  --  $55: bytes: 2 cycles: 4 A____=>A___P R_ zpx
opInfo 0x56 = "LSR zpx 6  "  --  $56: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0x57 = "*SRE zpx 6 "  --  $57: bytes: 2 cycles: 6 A____=>A___P RW zpx
opInfo 0x58 = "CLI 2      "  --  $58: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x59 = "EOR aby 4* "  --  $59: bytes: 3 cycles: 4 A____=>A___P R_ absy
opInfo 0x5A = "*NOP 2     "  --  $5A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x5B = "*SRE aby 7 "  --  $5B: bytes: 3 cycles: 7 A____=>A___P RW absy
opInfo 0x5C = "*NOP abx 4*"  --  $5C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x5D = "EOR abx 4* "  --  $5D: bytes: 3 cycles: 4 A____=>A___P R_ absx
opInfo 0x5E = "LSR abx 7  "  --  $5E: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0x5F = "*SRE abx 7 "  --  $5F: bytes: 3 cycles: 7 A____=>A___P RW absx
opInfo 0x60 = "RTS 6      "  --  $60: bytes: X cycles: 6 ___S_=>___S_ __
opInfo 0x61 = "ADC izx 6  "  --  $61: bytes: 2 cycles: 6 A___P=>A___P R_ izx
opInfo 0x62 = "*KIL       "  --  $62: CRASH
opInfo 0x63 = "*RRA izx 8 "  --  $63: bytes: 2 cycles: 8 A___P=>A___P RW izx
opInfo 0x64 = "*NOP zp 3  "  --  $64: bytes: 2 cycles: 3 _____=>_____ R_ zp
opInfo 0x65 = "ADC zp 3   "  --  $65: bytes: 2 cycles: 3 A___P=>A___P R_ zp
opInfo 0x66 = "ROR zp 5   "  --  $66: bytes: 2 cycles: 5 ____P=>____P RW zp
opInfo 0x67 = "*RRA zp 5  "  --  $67: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0x68 = "PLA 4      "  --  $68: bytes: 1 cycles: 4 ___S_=>A__SP __
opInfo 0x69 = "ADC imm 2  "  --  $69: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0x6A = "ROR 2      "  --  $6A: bytes: 1 cycles: 2 A___P=>A___P __
opInfo 0x6B = "*ARR imm 2 "  --  $6B: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0x6C = "JMP ind 5  "  --  $6C: bytes: X cycles: 5 _____=>_____ __
opInfo 0x6D = "ADC abs 4  "  --  $6D: bytes: 3 cycles: 4 A___P=>A___P R_ abs
opInfo 0x6E = "ROR abs 6  "  --  $6E: bytes: 3 cycles: 6 ____P=>____P RW abs
opInfo 0x6F = "*RRA abs 6 "  --  $6F: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0x70 = "BVS rel 2* "  --  $70: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x71 = "ADC izy 5* "  --  $71: bytes: 2 cycles: 5 A___P=>A___P R_ izy
opInfo 0x72 = "*KIL       "  --  $72: CRASH
opInfo 0x73 = "*RRA izy 8 "  --  $73: bytes: 2 cycles: 8 A___P=>A___P RW izy
opInfo 0x74 = "*NOP zpx 4 "  --  $74: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0x75 = "ADC zpx 4  "  --  $75: bytes: 2 cycles: 4 A___P=>A___P R_ zpx
opInfo 0x76 = "ROR zpx 6  "  --  $76: bytes: 2 cycles: 6 ____P=>____P RW zpx
opInfo 0x77 = "*RRA zpx 6 "  --  $77: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0x78 = "SEI 2      "  --  $78: bytes: 1 cycles: 2 _____=>____P __
opInfo 0x79 = "ADC aby 4* "  --  $79: bytes: 3 cycles: 4 A___P=>A___P R_ absy
opInfo 0x7A = "*NOP 2     "  --  $7A: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0x7B = "*RRA aby 7 "  --  $7B: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0x7C = "*NOP abx 4*"  --  $7C: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0x7D = "ADC abx 4* "  --  $7D: bytes: 3 cycles: 4 A___P=>A___P R_ absx
opInfo 0x7E = "ROR abx 7  "  --  $7E: bytes: 3 cycles: 7 ____P=>____P RW absx
opInfo 0x7F = "*RRA abx 7 "  --  $7F: bytes: 3 cycles: 7 A___P=>A___P RW absx
opInfo 0x80 = "*NOP imm 2 "  --  $80: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x81 = "STA izx 6  "  --  $81: bytes: 2 cycles: 6 A____=>_____ RW izx
opInfo 0x82 = "*NOP imm 2 "  --  $82: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x83 = "*SAX izx 6 "  --  $83: bytes: 2 cycles: 6 _____=>_____ RW izx
opInfo 0x84 = "STY zp 3   "  --  $84: bytes: 2 cycles: 3 __Y__=>_____ _W zp
opInfo 0x85 = "STA zp 3   "  --  $85: bytes: 2 cycles: 3 A____=>_____ _W zp
opInfo 0x86 = "STX zp 3   "  --  $86: bytes: 2 cycles: 3 _X___=>_____ _W zp
opInfo 0x87 = "*SAX zp 3  "  --  $87: bytes: 2 cycles: 3 _____=>_____ _W zp
opInfo 0x88 = "DEY 2      "  --  $88: bytes: 1 cycles: 2 __Y__=>__Y_P __
opInfo 0x89 = "*NOP imm 2 "  --  $89: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0x8A = "TXA 2      "  --  $8A: bytes: 1 cycles: 2 _X___=>A___P __
opInfo 0x8B = "*XAA imm 2 "  --  $8B: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0x8C = "STY abs 4  "  --  $8C: bytes: 3 cycles: 4 __Y__=>_____ _W abs
opInfo 0x8D = "STA abs 4  "  --  $8D: bytes: 3 cycles: 4 A____=>_____ _W abs
opInfo 0x8E = "STX abs 4  "  --  $8E: bytes: 3 cycles: 4 _X___=>_____ _W abs
opInfo 0x8F = "*SAX abs 4 "  --  $8F: bytes: 3 cycles: 4 _____=>_____ _W abs
opInfo 0x90 = "BCC rel 2* "  --  $90: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0x91 = "STA izy 6  "  --  $91: bytes: 2 cycles: 6 A____=>_____ RW izy
opInfo 0x92 = "*KIL       "  --  $92: CRASH
opInfo 0x93 = "*AHX izy 6 "  --  $93: bytes: 2 cycles: 6 _____=>_____ RW izy
opInfo 0x94 = "STY zpx 4  "  --  $94: bytes: 2 cycles: 4 __Y__=>_____ RW zpx
opInfo 0x95 = "STA zpx 4  "  --  $95: bytes: 2 cycles: 4 A____=>_____ RW zpx
opInfo 0x96 = "STX zpy 4  "  --  $96: bytes: 2 cycles: 4 _X___=>_____ RW zpy
opInfo 0x97 = "*SAX zpy 4 "  --  $97: bytes: 2 cycles: 4 _____=>_____ RW zpy
opInfo 0x98 = "TYA 2      "  --  $98: bytes: 1 cycles: 2 __Y__=>A___P __
opInfo 0x99 = "STA aby 5  "  --  $99: bytes: 3 cycles: 5 A____=>_____ RW absy
opInfo 0x9A = "TXS 2      "  --  $9A: bytes: X cycles: 2 _X___=>___S_ __
opInfo 0x9B = "*TAS aby 5 "  --  $9B: bytes: X cycles: 5 __Y__=>___S_ _W
opInfo 0x9C = "*SHY abx 5 "  --  $9C: bytes: 3 cycles: 5 __Y__=>_____ RW absx
opInfo 0x9D = "STA abx 5  "  --  $9D: bytes: 3 cycles: 5 A____=>_____ RW absx
opInfo 0x9E = "*SHX aby 5 "  --  $9E: bytes: 3 cycles: 5 _X___=>_____ RW absy
opInfo 0x9F = "*AHX aby 5 "  --  $9F: bytes: 3 cycles: 5 _____=>_____ RW absy
opInfo 0xA0 = "LDY imm 2  "  --  $A0: bytes: 2 cycles: 2 _____=>__Y_P __
opInfo 0xA1 = "LDA izx 6  "  --  $A1: bytes: 2 cycles: 6 _____=>A___P R_ izx
opInfo 0xA2 = "LDX imm 2  "  --  $A2: bytes: 2 cycles: 2 _____=>_X__P __
opInfo 0xA3 = "*LAX izx 6 "  --  $A3: bytes: 2 cycles: 6 _____=>AX__P R_ izx
opInfo 0xA4 = "LDY zp 3   "  --  $A4: bytes: 2 cycles: 3 _____=>__Y_P R_ zp
opInfo 0xA5 = "LDA zp 3   "  --  $A5: bytes: 2 cycles: 3 _____=>A___P R_ zp
opInfo 0xA6 = "LDX zp 3   "  --  $A6: bytes: 2 cycles: 3 _____=>_X__P R_ zp
opInfo 0xA7 = "*LAX zp 3  "  --  $A7: bytes: 2 cycles: 3 _____=>AX__P R_ zp
opInfo 0xA8 = "TAY 2      "  --  $A8: bytes: 1 cycles: 2 A____=>__Y_P __
opInfo 0xA9 = "LDA imm 2  "  --  $A9: bytes: 2 cycles: 2 _____=>A___P __
opInfo 0xAA = "TAX 2      "  --  $AA: bytes: 1 cycles: 2 A____=>_X__P __
opInfo 0xAB = "*LAX imm 2 "  --  $AB: bytes: 2 cycles: 2 A____=>AX__P __
opInfo 0xAC = "LDY abs 4  "  --  $AC: bytes: 3 cycles: 4 _____=>__Y_P R_ abs
opInfo 0xAD = "LDA abs 4  "  --  $AD: bytes: 3 cycles: 4 _____=>A___P R_ abs
opInfo 0xAE = "LDX abs 4  "  --  $AE: bytes: 3 cycles: 4 _____=>_X__P R_ abs
opInfo 0xAF = "*LAX abs 4 "  --  $AF: bytes: 3 cycles: 4 _____=>AX__P R_ abs
opInfo 0xB0 = "BCS rel 2* "  --  $B0: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xB1 = "LDA izy 5* "  --  $B1: bytes: 2 cycles: 5 _____=>A___P R_ izy
opInfo 0xB2 = "*KIL       "  --  $B2: CRASH
opInfo 0xB3 = "*LAX izy 5*"  --  $B3: bytes: 2 cycles: 5 _____=>AX__P R_ izy
opInfo 0xB4 = "LDY zpx 4  "  --  $B4: bytes: 2 cycles: 4 _____=>__Y_P R_ zpx
opInfo 0xB5 = "LDA zpx 4  "  --  $B5: bytes: 2 cycles: 4 _____=>A___P R_ zpx
opInfo 0xB6 = "LDX zpy 4  "  --  $B6: bytes: 2 cycles: 4 _____=>_X__P R_ zpy
opInfo 0xB7 = "*LAX zpy 4 "  --  $B7: bytes: 2 cycles: 4 _____=>AX__P R_ zpy
opInfo 0xB8 = "CLV 2      "  --  $B8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xB9 = "LDA aby 4* "  --  $B9: bytes: 3 cycles: 4 _____=>A___P R_ absy
opInfo 0xBA = "TSX 2      "  --  $BA: bytes: 1 cycles: 2 ___S_=>_X__P __
opInfo 0xBB = "*LAS aby 4*"  --  $BB: bytes: 3 cycles: 4 ___S_=>AX_SP R_ absy
opInfo 0xBC = "LDY abx 4* "  --  $BC: bytes: 3 cycles: 4 _____=>__Y_P R_ absx
opInfo 0xBD = "LDA abx 4* "  --  $BD: bytes: 3 cycles: 4 _____=>A___P R_ absx
opInfo 0xBE = "LDX aby 4* "  --  $BE: bytes: 3 cycles: 4 _____=>_X__P R_ absy
opInfo 0xBF = "*LAX aby 4*"  --  $BF: bytes: 3 cycles: 4 _____=>AX__P R_ absy
opInfo 0xC0 = "CPY imm 2  "  --  $C0: bytes: 2 cycles: 2 __Y__=>____P __
opInfo 0xC1 = "CMP izx 6  "  --  $C1: bytes: 2 cycles: 6 A____=>____P R_ izx
opInfo 0xC2 = "*NOP imm 2 "  --  $C2: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xC3 = "*DCP izx 8 "  --  $C3: bytes: 2 cycles: 8 A____=>____P RW izx
opInfo 0xC4 = "CPY zp 3   "  --  $C4: bytes: 2 cycles: 3 __Y__=>____P R_ zp
opInfo 0xC5 = "CMP zp 3   "  --  $C5: bytes: 2 cycles: 3 A____=>____P R_ zp
opInfo 0xC6 = "DEC zp 5   "  --  $C6: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0xC7 = "*DCP zp 5  "  --  $C7: bytes: 2 cycles: 5 A____=>____P RW zp
opInfo 0xC8 = "INY 2      "  --  $C8: bytes: 1 cycles: 2 __Y__=>__Y_P __
opInfo 0xC9 = "CMP imm 2  "  --  $C9: bytes: 2 cycles: 2 A____=>____P __
opInfo 0xCA = "DEX 2      "  --  $CA: bytes: 1 cycles: 2 _X___=>_X__P __
opInfo 0xCB = "*AXS imm 2 "  --  $CB: bytes: 2 cycles: 2 _____=>_X__P __
opInfo 0xCC = "CPY abs 4  "  --  $CC: bytes: 3 cycles: 4 __Y__=>____P R_ abs
opInfo 0xCD = "CMP abs 4  "  --  $CD: bytes: 3 cycles: 4 A____=>____P R_ abs
opInfo 0xCE = "DEC abs 6  "  --  $CE: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0xCF = "*DCP abs 6 "  --  $CF: bytes: 3 cycles: 6 A____=>____P RW abs
opInfo 0xD0 = "BNE rel 2* "  --  $D0: bytes: 2 cycles: 3 ____P=>_____ __
opInfo 0xD1 = "CMP izy 5* "  --  $D1: bytes: 2 cycles: 5 A____=>____P R_ izy
opInfo 0xD2 = "*KIL       "  --  $D2: CRASH
opInfo 0xD3 = "*DCP izy 8 "  --  $D3: bytes: 2 cycles: 8 A____=>____P RW izy
opInfo 0xD4 = "*NOP zpx 4 "  --  $D4: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0xD5 = "CMP zpx 4  "  --  $D5: bytes: 2 cycles: 4 A____=>____P R_ zpx
opInfo 0xD6 = "DEC zpx 6  "  --  $D6: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0xD7 = "*DCP zpx 6 "  --  $D7: bytes: 2 cycles: 6 A____=>____P RW zpx
opInfo 0xD8 = "CLD 2      "  --  $D8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xD9 = "CMP aby 4* "  --  $D9: bytes: 3 cycles: 4 A____=>____P R_ absy
opInfo 0xDA = "*NOP 2     "  --  $DA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xDB = "*DCP aby 7 "  --  $DB: bytes: 3 cycles: 7 A____=>____P RW absy
opInfo 0xDC = "*NOP abx 4*"  --  $DC: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0xDD = "CMP abx 4* "  --  $DD: bytes: 3 cycles: 4 A____=>____P R_ absx
opInfo 0xDE = "DEC abx 7  "  --  $DE: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0xDF = "*DCP abx 7 "  --  $DF: bytes: 3 cycles: 7 A____=>____P RW absx
opInfo 0xE0 = "CPX imm 2  "  --  $E0: bytes: 2 cycles: 2 _X___=>____P __
opInfo 0xE1 = "SBC izx 6  "  --  $E1: bytes: 2 cycles: 6 A___P=>A___P R_ izx
opInfo 0xE2 = "*NOP imm 2 "  --  $E2: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xE3 = "*ISC izx 8 "  --  $E3: bytes: 2 cycles: 8 A___P=>A___P RW izx
opInfo 0xE4 = "CPX zp 3   "  --  $E4: bytes: 2 cycles: 3 _X___=>____P R_ zp
opInfo 0xE5 = "SBC zp 3   "  --  $E5: bytes: 2 cycles: 3 A___P=>A___P R_ zp
opInfo 0xE6 = "INC zp 5   "  --  $E6: bytes: 2 cycles: 5 _____=>____P RW zp
opInfo 0xE7 = "*ISC zp 5  "  --  $E7: bytes: 2 cycles: 5 A___P=>A___P RW zp
opInfo 0xE8 = "INX 2      "  --  $E8: bytes: 1 cycles: 2 _X___=>_X__P __
opInfo 0xE9 = "SBC imm 2  "  --  $E9: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0xEA = "NOP 2      "  --  $EA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xEB = "*SBC imm 2 "  --  $EB: bytes: 2 cycles: 2 A___P=>A___P __
opInfo 0xEC = "CPX abs 4  "  --  $EC: bytes: 3 cycles: 4 _X___=>____P R_ abs
opInfo 0xED = "SBC abs 4  "  --  $ED: bytes: 3 cycles: 4 A___P=>A___P R_ abs
opInfo 0xEE = "INC abs 6  "  --  $EE: bytes: 3 cycles: 6 _____=>____P RW abs
opInfo 0xEF = "*ISC abs 6 "  --  $EF: bytes: 3 cycles: 6 A___P=>A___P RW abs
opInfo 0xF0 = "BEQ rel 2* "  --  $F0: bytes: 2 cycles: 2 _____=>_____ __
opInfo 0xF1 = "SBC izy 5* "  --  $F1: bytes: 2 cycles: 5 A___P=>A___P R_ izy
opInfo 0xF2 = "*KIL       "  --  $F2: CRASH
opInfo 0xF3 = "*ISC izy 8 "  --  $F3: bytes: 2 cycles: 8 A___P=>A___P RW izy
opInfo 0xF4 = "*NOP zpx 4 "  --  $F4: bytes: 2 cycles: 4 _____=>_____ R_ zpx
opInfo 0xF5 = "SBC zpx 4  "  --  $F5: bytes: 2 cycles: 4 A___P=>A___P R_ zpx
opInfo 0xF6 = "INC zpx 6  "  --  $F6: bytes: 2 cycles: 6 _____=>____P RW zpx
opInfo 0xF7 = "*ISC zpx 6 "  --  $F7: bytes: 2 cycles: 6 A___P=>A___P RW zpx
opInfo 0xF8 = "SED 2      "  --  $F8: bytes: 1 cycles: 2 _____=>____P __
opInfo 0xF9 = "SBC aby 4* "  --  $F9: bytes: 3 cycles: 4 A___P=>A___P R_ absy
opInfo 0xFA = "*NOP 2     "  --  $FA: bytes: 1 cycles: 2 _____=>_____ __
opInfo 0xFB = "*ISC aby 7 "  --  $FB: bytes: 3 cycles: 7 A___P=>A___P RW absy
opInfo 0xFC = "*NOP abx 4*"  --  $FC: bytes: 3 cycles: 4 _____=>_____ R_ absx
opInfo 0xFD = "SBC abx 4* "  --  $FD: bytes: 3 cycles: 4 A___P=>A___P R_ absx
opInfo 0xFE = "INC abx 7  "  --  $FE: bytes: 3 cycles: 7 _____=>____P RW absx
opInfo 0xFF = "*ISC abx   "  --  $FF: bytes: 3 cycles: 7 A___P=>A___P RW absx
