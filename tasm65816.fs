\ A Typist's 65816 Assembler in Forth 
\ Copyright 2015 Scot W. Stevenson <scot.stevenson@gmail.com>
\ Written with gforth 0.7
\ First version: 31. May 2015
\ This version: 18. July 2015

\ This program is free software: you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation, either version 3 of the License, or
\ (at your option) any later version.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

\ You should have received a copy of the GNU General Public License
\ along with this program.  If not, see <http://www.gnu.org/licenses/>.

hex

\ Initial target address on 65816 machine. This is a 24-bit number. 
\ If user provides no initial address, we use 0000, which on the 65816
\ allows relocation by bank. 
variable lc0  0 lc0 ! 

0ffffff 1+ constant maxmemory     \ 65816 has 24 bit address space
create staging maxmemory allot    \ buffer to store assembled machine code
staging maxmemory erase 

\ buffer counter, offset to start of staging area. Points to the next free
\ byte, not the last byte saved 
variable bc  0 bc !  


\ -----------------------
\ LOW LEVEL AND HELPER DIRECTIVES

\ Return single bytes from a 16- or 24-bit number; assumes HEX
: lsb  ( u -- u8 )  0ff and ; 
: msb  ( u -- u8 )  0ff00 and  8 rshift ; 
: bank  ( u -- u8 ) 0ff0000 and  10 rshift ; 

\ Convert 16- or 24-bit address to little-endian. Leave LSB on top of stack 
\ because we'll be saving it first. Note this is the reverse oder to the 
\ Crude 65816 Simulator
: 16>msb/lsb  ( u -- msb lsb )  dup msb swap lsb ; 
: 24>bank/msb/lsb  ( u -- bank msb lsb )  
   dup            ( u u ) 
   16>msb/lsb     ( u msb lsb )  
   rot            ( msb lsb u ) 
   bank -rot ;    ( bank msb lsb ) 

\ take a little-endian 16 bit address and turn it into a "normal"
\ big-endian number. Note stack order is reverse of 16>msb/lsb
: lsb/msb>16  ( lsb msb - u ) 8 lshift  0ff00 and  or  0ffff and ;
: lsb/msb/bank>24        ( lsb msb bank - u )  
   -rot lsb/msb>16       ( bank u16 ) 
   swap                  ( u16 bank ) 
   0ff and  10 lshift    ( u16 u8 ) 
   or ; 

\ Calculate currect location counter from target address and buffer offset
: lc  ( -- )  lc0 @  bc @  + ; 

\ Save one byte in staging area, 
: b,  ( c -- )  staging  bc @  +   c!   1 bc +! ; 

\ Save one word (16 bit) in staging area, converting to little-endian
: w,  ( w -- )  16>msb/lsb  b, b, ; 

\ Save one long word (24 bit) in the staging area, converting to little-endian
: lw, ( lw -- ) 24>bank/msb/lsb  b, b, b, ;  

\ Save ASCII string provided by S" instruction (S, is reserved by gforth) 
\ Note OVER + SWAP is also BOUNDS in gforth 
: str, ( addr u -- )  over + swap  ?do  i c@ b,  loop ; 

\ Save zero-terminated ASCII string provided by S" instruction
: str0, ( addr u -- ) str, 0 b, ; 

\ Save linefeed-terminated  ASCII string provided by S" instruction
: strlf, ( addr u -- ) str, 0a b, ; 

\ Make sure branches are in range 
: branch-in-range?   ( n -- f )  -80 7f within ;      \ 8 bit branch
: branch-in-range.l? ( n -- f )  -8000 7fff within ;  \ 16 bit branch

\ For error messages: Locate error. This brings the SPACE so you don't have
\ to. Assumes it will be printed after actual error message with no CR
: .errorlocation ( -- )  
   space ." lc: " lc s>d <# # # # # # # #> type 
   space ." bc: " bc . ; 


\ -----------------------
\ HIGH LEVEL DIRECTIVES

\ set intial target address on 65816 machine. If no such command is
\ present in the source code, we start assembling at 000000
: origin ( 65addr -- )  lc0 ! ; 

\ move to a given address, filling the space inbetween with zeros
: advance ( 65addr -- ) 
   staging bc @ +  ( 65addr addr )
   over lc -       ( 65addr addr u ) 
   erase           ( 65addr )
   lc0 @ -  bc ! ;

\ mark end of assembler source text, return buffer location and size
\ This command must be present in the source code
: end  ( -- addr u )  staging  bc @ ; 

\ save assembled program to file, overwriting any file with same name
\ WARNING: In some cases, it seems that the file content is only written
\ to the drive when Gforth is closed; before then, an empty file will
\ be created
: save ( addr u "name" -- )
   parse-name w/o create-file
   drop write-file if 
      ." Error writing file" then ; 


\ -----------------------
\ HANDLE UNRESOLVED FORWARD LABELS (FUTURE REFERENCES) 

\ Calculate offset to the start of the staging area for the operand of
\ JMP/JSR/BRA etc opcodes. In other words, skip one byte forward from our
\ current location, leaving one byte space for the opcode. This is the
\ start of the "hole" we are going to leave to put the operand in later.
: hole  ( -- offset )  bc @ 1+ ; 

\ Handle forward unresolved references (future symbols) by either creating 
\ a new linked list of locations in the staging area or by adding a new 
\ entry an existing list. Each entry is composed of 1) a link to the next 
\ entry in the list (a zero marks the tail), 2) the offset to the start of
\ the staging area where the real address will later go, and 3) space for 
\ the branch or jump instruction to save a reference (xt) to its specialized
\ routine that will later create the actual address. New entries are added
\ at the head. Note we only add 1) here, 2) and 3) are added later. 
: addlabel  ( "name" -- ) 
   parse-name 2dup find-name    ( addr u nt|0 )
   dup if   
     name>int    ( addr u xt )     \ Gforth uses "name token" (nt), need xt
     >body       ( addr u l-addr ) \ get location of this symbol
     dup @       ( addr u l-addr curr ) \ get link to current entry
     here        ( addr u l-addr curr new ) 
     rot !       ( addr u curr )  \ save link to new entry in old entry
     ,           ( addr u )  \ save link to old entry in new entry
     2drop       \ don't need name string 
   else     
     \ new name, create new list. NEXTNAME is specific to 
     \ Gforth and provides a string for a defining word such as CREATE
     drop nextname create  
     0 ,          \ mark tail of new linked list with zero
   then 
   hole , ;       \ Save offset to start of staging area


\ REPLACEMENT ROUTINES for various forward branches. These are used by
\ once we know the actual address we will be jumping or branching to so
\ we can replace dummy references we saved. The xt of these routines is 
\ saved in the linked list of future symbols. 
\ TODO find a better way to do these, because they are all ugly as sin

\ Replace dummy references to an ABSOLUTE 16-bit address 
: dummy>abs  ( buffer-offset -- )
   staging +  dup c@  ( addr lsb ) 
   over char+ c@      ( addr lsb msb ) 
   lsb/msb>16         ( addr u ) 
   lc +  16>msb/lsb   ( addr msb lsb ) 
   rot tuck           ( msb addr lsb addr ) 
   c!                 ( msb addr ) 
   char+              ( msb addr+1 ) 
   c! ; 

\ Replace dummy references to an ABSOLUTE 24-bit address 
: dummy>abs.l  ( buffer-offset -- )  

   \ step one: assemble 65addr from dummy parts
   staging +  dup c@  ( addr lsb )  
   over char+         ( addr lsb addr+1 ) 
   dup c@             ( addr lsb addr+1 msb ) 
   swap char+ c@      ( addr lsb msb bank ) 
   lsb/msb/bank>24    ( addr u ) 

   \ step two: add current location
   lc +               ( addr 65addr ) 

   \ step three: save 24-bit address in little-endian 
   dup >r             ( addr 65addr )   ( R: 65addr) 
   lsb over           ( addr lsb addr ) ( R: 65addr) 
   c! char+ r>        ( addr+1 65addr ) 
   dup >r             ( addr+1 65addr ) ( R: 65addr)
   msb over           ( addr+1 msb addr+1 ) ( R: 65addr) 
   c! char+ r>        ( addr+2 65addr )
   bank swap c! ; 

\ replace dummy references to an RELATIVE offset 
: dummy>rel          ( buffer-offset -- )
   dup staging +     ( b-off addr ) 
   bc @              ( b-off addr bc ) 
   rot -  1+         ( addr 65off ) 

   \ make sure branch is still in range
   dup branch-in-range? invert if 
      cr ." ERROR: Short branch out of range." .errorlocation then

   \ Get dummy to enable address math
   over c@ +         
   swap c! ;         

\ replace dummy references to an RELATIVE LONG offset 
: dummy>rel.l        ( buffer-offset -- ) 
   dup staging +     ( b-off addr ) 
   bc @              ( b-off addr bc ) 
   rot -  1+         ( addr 65off )

   \ get dummy of address math
   over dup          ( addr 65off addr addr ) 
   c@                ( addr 65off addr lsb ) 
   swap char+ c@     ( addr 65off lsb msb ) 
   lsb/msb>16        ( addr 65off u ) 
   + 

   \ make sure branch is still in range
   dup branch-in-range.l? invert if 
      cr ." ERROR: Long branch out of range." .errorlocation then

   \ replace dummy with final result
   16>msb/lsb        ( addr msb lsb )
   rot               ( msb lsb addr ) 
   tuck              ( msb addr lsb addr ) 
   c!                ( msr addr ) 
   char+ c! ; 

\ create future reference directives
: <j  addlabel  ['] dummy>abs ,  0 ;    \ absolute ( 2byte operand) 
: <jl addlabel  ['] dummy>abs.l ,  0 ;  \ absolute long (3 byte operand) 
: <b  addlabel  ['] dummy>rel ,  lc ;   \ branch (1 byte operand)
: <bl addlabel  ['] dummy>rel.l ,  lc ; \ branch long (2 byte operand) 

\ TODO add references for <bank <msb <lsb

\ synonyms for non-jump/branch future references (eg phe.#) 
: <a <j ;      : <al <jl ;     : <r <b ;      : <rl <bl ; 


\ -----------------------
\ LABELS 

\ Define a label. Assume that the user knows what they are doing and
\ doesn't try to name label twice. If there were unresolved forward 
\ references, resolve them here and replace the complicated label handling
\ routine with simple new one. Yes, "-->" would be easer to read, but it 
\ is used by old block syntax of Forth
: ->  ( "name" -- )
   parse-name 2dup find-name    ( addr u nt|0 )
   \ if we find the name here twice, it must be an unresolved 
   \ forward reference. Now we can replace the dummy values we have been 
   \ collecting with the real stuff 
   dup if      
      name>int    ( addr u xt )  \ gforth uses "name token" (nt), need xt
      >body       ( addr u l-addr ) 

      \ walk through the list and replace dummy addresses and offsets
      begin
         dup      ( addr u l-addr l-addr ) \ stops if l-addr is zero 
      while 
         dup cell+ @   ( addr u l-addr offset ) 
         over          ( addr u l-addr offset l-addr )  
         2 cells +  @  ( addr u l-addr offset xt ) 
         execute       ( addr u l-addr ) 
         @             ( addr u next-l-addr ) 
      repeat 
   then

   \ One way or another, we now (re)define the label. NEXTNAME is specific 
   \ to Gforth and provides a string for a defining word such as CREATE
   drop nextname  create lc ,  does> @ ; 

  
\ -----------------------
\ DEFINING WORDS FOR OPCODES 

\ As with TIMELESS, the number of bytes refers to the operand
: 0byte  ( opcode -- ) ( -- ) 
   create c,
   does> c@ b, ; 

: 1byte ( opcode -- ) ( c -- )
   create c,
   does> c@ b, b, ; 

: 2byte ( opcode -- ) ( w -- )
   create c,
   does> c@ b, w, ; 

: 3byte ( opcode -- ) ( lw -- )
   create c, 
   does> c@ b, lw, ; 

\ Create short and long braches for instructions such as bra and bra.l. 
\ Includes test if branch is too long. Note BRANCH is reserved by Forth.
: twig  ( 65addr -- )
  create c, 
  does> c@ b, 
      lc - 1-
      dup branch-in-range? if b, 
         else 
            cr ." Error: Short branch out of range." .errorlocation drop
         then ; 

: twig.l ( 65addr -- ) 
   create c, 
   does> c@ b, 
      lc - 2 -
      dup branch-in-range.l? if w, 
         else 
            cr ." Error: Long branch out of range." .errorlocation drop
         then ; 

\ Handle block move instructions (MVN, MVP), which have a reverse order of
\ operands in machine code and assembler. BLOCK and MOVE are reserved by Forth
: moveblock ( opc -- ) ( opr opr opc -- ) 
   create c,
   does> c@ b,          \ Save opcode 
      lsb b,  lsb b, ;  \ save operands in correct sequence


\ -----------------------
\ OPCODE TABLE 
\ TODO recheck these manually

00 1byte brk     01 1byte ora.dxi   02 1byte cop      03 1byte ora.s
04 1byte tsb.d   05 1byte ora.d     06 1byte asl.d    07 1byte ora.dil
08 0byte php     ( 09 see below )   0a 0byte asl.a    0b 0byte phd
0c 2byte tsb     0d 2byte ora       0e 2byte asl      0f 3byte ora.l

10 twig bpl      11 1byte ora.diy   12 1byte ora.di   13 1byte ora.siy   
14 1byte trb.d   15 1byte ora.dx    16 1byte asl.dx   17 1byte ora.dily  
18 0byte clc     19 2byte ora.y     1a 0byte inc.a    1b 0byte tcs      
1c 2byte trb     1d 2byte ora.x     1e 2byte asl.x    1f 3byte ora.lx

20 2byte jsr     21 1byte and.dxi   22 3byte jsr.l    23 1byte and.s
24 1byte bit.d   25 1byte and.d     26 1byte rol.d    27 1byte and.dil
28 0byte plp     ( 29 see below )   2a 0byte rol.a    2b 0byte pld
2c 2byte bit     2d 2byte and. ( !) 2e 2byte rol      2f 3byte and.l

30 twig bmi      31 1byte and.diy   32 1byte and.di   33 1byte and.siy   
34 1byte bit.dxi 35 1byte and.dx    36 1byte rol.dx   37 1byte and.dily
38 0byte sec     39 2byte and.y     3a 0byte dec.a    3b 0byte tsc
3c 2byte bit.x   3d 2byte and.x     3e 2byte rol.x    3f 3byte and.lx

40 0byte rti     41 1byte eor.dxi   ( 42 see below )  43 1byte eor.s
44 moveblock mvp 45 1byte eor.d     46 1byte lsr.d    47 1byte eor.dil
48 0byte pha     ( 49 see below )   4a 0byte lsr.a    4b 0byte phk
4c 2byte jmp     4d 2byte eor       4e 2byte lsr      4f 3byte eor.l

50 twig bvc      51 1byte eor.diy   52 1byte eor.di   53 1byte eor.siy
54 moveblock mvn 55 1byte eor.dx    56 1byte lsr.dx   57 1byte eor.dily
58 0byte cli     59 2byte eor.y     5a 0byte phy      5b 0byte tcd      
5c 3byte jmp.l   5d 2byte eor.x     5e 2byte lsr.x    5f 3byte eor.lx

60 0byte rts     61 1byte adc.dxi   62 twig.l phe.r   63 1byte adc.s
64 1byte stz.d   65 1byte adc.d     66 1byte ror.d    67 1byte adc.dil   
68 0byte pla     ( 69 see below )   6a 0byte ror.a    6b 0byte rts.l                       
6c 2byte jmp.i   6d 2byte adc       6e 2byte ror      6f 3byte adc.l

70 twig bvs      71 1byte adc.diy   72 1byte adc.di   73 1byte adc.siy
74 1byte stz.dx  75 1byte adc.dx    76 1byte ror.dx   77 1byte adc.dily  
78 0byte sei     79 2byte adc.y     7a 0byte ply      7b 0byte tdc      
7c 2byte jmp.xi  7d 2byte adc.x     7e 2byte ror.x    7f 3byte adc.lx

80 twig bra      81 1byte sta.dxi   82 twig.l bra.l   83 1byte sta.s
84 1byte sty.d   85 1byte sta.d     86 1byte stx.d    87 1byte sta.dil
88 0byte dey     ( 89 see below )   8a 0byte txa      8b 0byte phb
8c 2byte sty     8d 2byte sta       8e 2byte stx      8f 3byte sta.l

90 twig bcc      91 1byte sta.diy   92 1byte sta.di   93 1byte sta.siy   
94 1byte sty.dx  95 1byte sta.dx    96 1byte stx.dy   97 1byte sta.dily  
98 0byte tya     99 2byte sta.y     9a 0byte txs      9b 0byte txy      
9c 2byte stz     9d 2byte sta.x     9e 2byte stz.x    9f 3byte sta.lx

( a0 see below )  0a1 1byte lda.dxi  ( a2 see below )  0a3 1byte lda.s
0a4 1byte ldy.d   0a5 1byte lda.d    0a6 1byte ldx.d   0a7 1byte lda.dil
0a8 0byte tay     ( a9 see below )   0aa 0byte tax     0ab 0byte plb
0ac 2byte ldy     0ad 2byte lda      0ae 2byte ldx     0af 3byte lda.l

0b0 twig bcs      0b1 1byte lda.diy  0b2 1byte lda.di  0b3 1byte lda.siy 
0b4 1byte ldy.dx  0b5 1byte lda.dx   0b6 1byte ldx.dy  0b7 1byte lda.dily 
0b8 0byte clv     0b9 2byte lda.y    0ba 0byte tsx     0bb 0byte tyx     
0bc 2byte ldy.x   0bd 2byte lda.x    0be 2byte ldx.y   0bf 3byte lda.lx

( c0 see below )  0c1 1byte cmp.dxi  0c2 1byte rep     0c3 1byte cmp.s
0c4 1byte cpy.d   0c5 1byte cmp.d    0c6 1byte dec.d   0c7 1byte cmp.dil
0c8 0byte iny     ( c9 see below )   0ca 0byte dex     0cb 0byte wai
0cc 2byte cpy     0cd 2byte cmp      0ce 2byte dec     0cf 3byte cmp.l

0d0 twig bne      0d1 1byte cmp.diy  0d2 1byte cmp.di  0d3 1byte cmp.siy 
0d4 1byte phe.di  0d5 1byte cmp.dx   0d6 1byte dec.dx  0d7 1byte cmp.dily 
0d8 0byte cld     0d9 2byte cmp.y    0da 0byte phx     0db 0byte stp       
0dc 2byte jmp.il  0dd 2byte cmp.x    0de 2byte dec.x   0df 3byte cmp.lx

( 0e0 see below ) 0e1 1byte sbc.dxi  0e2 1byte sep     0e3 1byte sbc.s
0e4 1byte cpx.d   0e5 1byte sbc.d    0e6 1byte inc.d   0e7 1byte sbc.dil
0e8 0byte inx     ( 0e9 see below )  0ea 0byte nop     0eb 0byte xba 
0ec 2byte cpx     0ed 2byte sbc      0ee 2byte inc     0ef 3byte sbc.l

0f0 twig beq      0f1 1byte sbc.diy  0f2 1byte sbc.di  0f3 1byte sbc.siy 
0f4 2byte phe.#   0f5 1byte sbc.dx   0f6 1byte inc.dx  0f7 1byte sbc.dily 
0f8 0byte sed     0f9 2byte sbc.y    0fa 0byte plx     ( fb see below )    
0fc 2byte jsr.xi  0fd 2byte sbc.x    0fe 2byte inc.x   0ff 3byte sbc.lx


\ -----------------------
\ REGISTER SIZE AND CPU MODES

variable e-flag   \ native (0) or emulation (1) CPU mode
variable m-flag   \ 16-bit (0) or 8-bit (1) A register
variable x-flag   \ 16-bit (0) or 8 bit (1) X and Y registers

\ switch emulation/native mode. Use these commands instead of coding 
\ the instructions directly
: emulated  ( -- )  true e-flag !  38 b, 0fb b, ;  \ CLC 
: native  ( -- )  false e-flag !  18 b, 0fb b, ;  \ SEC 

\ make things easier for the poor humans and make asserting easier
: a=8?  ( -- f )  m-flag @  ; 
: a=16?  ( -- f )  m-flag @  invert ; 
: xy=8?  ( -- f )  x-flag @  ; 
: xy=16?  ( -- f )  x-flag @  invert ; 
: mode?  ( -- f ) e-flag @ ; \ true means "emulated", false means "native"


\ -----------------------
\ 8/16-BIT HYBRID INSTRUCTIONS

\ We have twelve instructions that need to be handled separately depending 
\ on the size of A and/or the X/Y Registers

 09 1byte ora.#8     09 2byte ora.#16    defer ora.# 
 29 1byte and.#8     29 2byte and.#16    defer and.# 
 49 1byte eor.#8     49 2byte eor.#16    defer eor.# 
 69 1byte adc.#8     69 2byte adc.#16    defer adc.# 
 89 1byte bit.#8     89 2byte bit.#16    defer bit.# 
0a0 1byte ldy.#8    0a0 2byte ldy.#16    defer ldy.# 
0a2 1byte ldx.#8    0a2 2byte ldx.#16    defer ldx.# 
0a9 1byte lda.#8    0a9 2byte lda.#16    defer lda.# 
0c0 1byte cpy.#8    0c0 2byte cpy.#16    defer cpy.# 
0c9 1byte cmp.#8    0c9 2byte cmp.#16    defer cmp.# 
0e0 1byte cpx.#8    0e0 2byte cpx.#16    defer cpx.# 
0e9 1byte sbc.#8    0e9 2byte sbc.#16    defer sbc.# 


: a8defines ( -- )  true m-flag ! 
   ['] ora.#8 is ora.#  ['] and.#8 is and.#  
   ['] eor.#8 is eor.#  ['] adc.#8 is adc.#  
   ['] bit.#8 is bit.#  ['] lda.#8 is lda.#  
   ['] cmp.#8 is cmp.#  ['] sbc.#8 is sbc.# ;    

: xy8defines ( -- )  true x-flag ! 
   ['] ldy.#8 is ldy.#  ['] ldx.#8 is ldx.# 
   ['] cpy.#8 is cpy.#  ['] cpx.#8 is cpx.# ;

: axy8defines ( -- ) a8defines  xy8defines ; 


: a16defines ( -- )  false m-flag ! 
   ['] ora.#16 is ora.#  ['] and.#16 is and.#  
   ['] eor.#16 is eor.#  ['] adc.#16 is adc.#  
   ['] bit.#16 is bit.#  ['] lda.#16 is lda.#  
   ['] cmp.#16 is cmp.#  ['] sbc.#16 is sbc.# ;    

: xy16defines ( -- )   false x-flag ! 
   ['] ldy.#16 is ldy.#  ['] ldx.#16 is ldx.# 
   ['] cpy.#16 is cpy.#  ['] cpx.#16 is cpx.# ;

: axy16defines ( -- )  a16defines  xy16defines ;


\ SPECIAL OPCODES: We define these outside of the normal table 
\ above so we can include special checks. 

\ xce: Check if previous command was either CLC or SEC
: xce ( -- ) 
   0fb b,
   bc @  2 -  staging  +  \ get address of previous instruction
   c@ dup               
   18 = invert  swap  38 = invert  and  if 
         cr ." WARNING: No CLC or SEC before XCE." .errorlocation
      then ; 

\ wdm: Warn if we encounter this command
: wdm ( b -- ) 
   42 b, b, 
   cr ." WARNING: WDM instruction encountered." .errorlocation ; 


\ SYNONYMS: We use systematic names ("jmp.l") where WDC defines 
\ distinct opcodes ("JML"). For people who insist on these, we
\ we define the WDC codes as synonyms
: .synonym-nag  ( -- )  cr ." WARNING: Non-standard mnemonic. Change " ; 

: brl bra.l .synonym-nag ." BRL to BRA.L" .errorlocation ; 
: jsl jsr.l .synonym-nag ." JSL to JSR.L" .errorlocation ; 
: jml jmp.l .synonym-nag ." JML to JMP.L" .errorlocation ; 
: pea phe.# .synonym-nag ." PEA to PHE.#" .errorlocation ; 
: pei phe.di .synonym-nag ." PEI to PHE.DI" .errorlocation ; 
: per phe.r .synonym-nag ." PER to PHE.R" .errorlocation ; 
: rtl rts.l .synonym-nag ." RTL to RTS.L" .errorlocation ; 


\ Switch to 8/16 bit with SEP/REP instructions 
\ use these commands instead of coding the instructions directly
: a:8 ( -- )  a8defines  0e2 b, 20 b, ; \ 20 SEP 
: xy:8 ( -- )  xy8defines  0e2 b, 10 b, ; \ 10 SEP
: axy:8 ( -- )  axy8defines  0e2 b, 30 b, ; \ 30 SEP 
: a:16 ( -- )  a16defines  0c2 b, 20 b, ; \ 20 REP 
: xy:16 ( -- )  xy16defines  0c2 b, 10 b, ; \ 10 REP 
: axy:16 ( -- )  axy16defines  0c2 b, 30 b, ; \ 30 REP 


\ start assembler in emulation mode. Don't use a:8 and xy:8 here
\ because we don't want to save any bytes to the staging area yet 
true e-flag !  axy8defines

\ END
