#!/bin/sed -f

s/[A-Z][A-Z][A-Z]/\L&\EOp/
s/(d,x)/indirectXAddr/
s/(d),y/indirectYAddr/
s/d,x/zeroPageXAddr/
s/d,y/zeroPageYAddr/
s/a,x/absoluteXAddr/
s/a,y/absoluteYAddr/
s/#i/immediateAddr/
s/ d$/ zeroPageAddr/
s/ a$/ absoluteAddr/



s/^[0-9a-f][0-9a-f]/opCodeToFunc 0x& =/