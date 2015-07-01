\ A Typist's 65816 Assembler in Forth 
\ Copyright 2015 Scot W. Stevenson <scot.stevenson@gmail.com>
\ Written with gforth 0.7
\ First version: 31. May 2015
\ This version: 02. July 2015

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


\ make sure branch offset is the right size
: short-branchable? ( n -- f )  -80 7f within ;
: long-branchable?  ( n -- f )  -8000 7fff within ; 

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


\ Split up a 24 bit address in lsb, msb/lsb or bank/msb/lsb
\ depending on the parameter 1 to 3 given with the address. 
create splittable 
   ' lsb ,   ' 16>msb/lsb ,   ' 24>bank/msb/lsb , 

: splitbytes  ( 24bit u -- lsb | msb lsb | bank msb lsb ) 
   1- cells       ( offset )  \ index to splittable
   splittable +   ( addr ) 
   @ execute ; 

\ Use bytesplitter to save split-up 24-bit address in little-endian
\ format
: split&save  ( 24bit u -- lsb | msb lsb | bank msb lsb ) 
   dup >r
   splitbytes
   r> 1+ 1 do b, loop ;  



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
: save ( addr u "name" -- )
   parse-name w/o create-file
   drop write-file if 
      ." Error writing file" then ; 


\ -----------------------
\ LABELS 

\ calculate offset to the start of the staging area for the operand of
\ JMP/JSR/BRA etc opcodes. In other words, skip one byte to leave space
\ for these opcodes
: bc+1  ( -- offset )  bc @ 1+ ; 

\ Use flag to tell branch or jump instruction that its label is an 
\ unresolved future reference that it needs to take care of. If value is
\ zero, there is no forward reference, otherwise the variable holds the 
\ address of the incomplete entry in the linked list of this reference
: clear ( -- ) 0 swap ! ; 
variable futurecall  futurecall clear
: futurecall? ( -- f )  futurecall @  0<> ; 


\ TODO replace this
\ replace dummy reference to the MOST SIGNIFICANT BYTE in a list of 
\ forward references with the real MSB of the label
: dummy>msb  ( buffer-offset -- ) 
   staging +  lc msb  ( addr msb ) 
   swap c! ; 

\ TODO replace this
\ replace dummy reference to the LEAST SIGNIFICANT BYTE in a list of 
\ forward references with the real LSB of the label
: dummy>lsb  ( buffer-offset -- ) 
   staging +  lc lsb  ( addr lsb ) 
   swap c! ; 

\ TODO replace this
\ replace dummy reference to the BANK BYTE in a list of forward 
\ references with the real LSB of the label
: dummy>bank  ( buffer-offset -- ) 
   staging +  lc bank  ( addr bank ) 
   swap c! ; 



\ TODO replace this
\ replace dummy references to an RELATIVE offset byte in list of 
\ unresolved forward references by BRA with the real offset.  
: dummy>rel  ( buffer-offset -- )
   dup staging +     ( b-off addr ) 
   bc @              ( b-off addr bc ) 
   rot -  1-         ( addr 65off ) 
   swap c! ; 

\ TODO replace this
\ replace dummy references to an RELATIVE offset byte in list of 
\ unresolved forward references by BRA.L with the real offset.  
: dummy>rel16  ( buffer-offset -- ) 
   dup staging +     ( b-off addr ) 
   bc @              ( b-off addr bc ) 
   rot -  2 -        ( addr 65off )
   16>msb/lsb        ( addr msb lsb )
   rot               ( msb lsb addr ) 
   tuck              ( msb addr lsb addr ) 
   c!                ( msr addr ) 
   char+ c! ; 


\ Handle forward unresolved references (future symbols) by either creating 
\ a new linked list of locations in the staging area or by adding a new 
\ entry an existing list. Each entry is composed of 1) a link to the next 
\ entry in the list (a zero marks the tail), 2) the offset to the start of
\ the staging area where the real address will later go, and 3) space for 
\ the branch or jump instruction to save a reference (xt) to its specialized
\ routine that will later create the actual address. New entries are added
\ at the head
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
   bc+1 ,      \ save location of operand hole (address) 
   0 , ;       \ save space for type of link (xt) 


\ Mark an unresolved future symbol (forward label reference) for branches
\ and jumps. This is a generic declaration used for all variants. Pushes 0
\ to stack as a dummy entry for the address, allowing modification of the
\ label even when unknown (eg. "<? lower 1+  jmp") 
: <? ( "name" -- addr )  addlabel 
      here 1 cells -  futurecall !  \ save location of xt slot
      0 ;         \ push dummy address on the stack 



\ Replace dummy references to an ABSOLUTE 16-bit address in the list of
\ unresolved forward references with the real address (for JMP/JSR). Used 
\ by "->" once we know what the actually address is
: dummy>abs  ( buffer-offset -- )
   staging +  dup c@  ( addr lsb ) 
   over char+ c@      ( addr lsb msb ) 
   lsb/msb>16         ( addr u ) 
   lc +  16>msb/lsb   ( addr msb lsb ) 
   rot tuck           ( msb addr lsb addr ) 
   c!                 ( msb addr ) 
   char+              ( msb addr+1 ) 
   c! ; 

\ Replace dummy references to an ABSOLUTE 24-bit address in the list of
\ unresolved forward references with the real address (JMP.L/JSR.L). Used 
\ by "->" once we know what the actually address is
\ TODO Argh this is ugly, try to refactor or something, anything
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


\ Build routines to handle diffent types of future references. Note: 
\ these routines may require a sonic screwdriver 
: buildbooth ( xt -- ) ( -- ) 
   create , 
   does> @           ( xt ) 
      futurecall @   ( xt curr ) 
      ! 
      futurecall clear ; 

' dummy>abs   buildbooth tardis
' dummy>abs.l buildbooth tardis.l


\ Create instuctions that must be able to accept a future reference 
\ created by <? . Works by handing over the xt of the correct tardis 
\ routine, the opcode of the instruction in question and the number of
\ of bytes the operand is long (must be at least 1, so this routine will
\ not work with single-byte instructions). Call the resulting word
\ with the operand, such as " ' tardis 4c 2 futureproof jmp "
\ TODO find other word than "futureproof"
: futureproof ( xt opc bytes -- ) ( opr -- ) 
   create c, c, , 
   does> dup c@         ( opr addr bytes )
         swap char+     ( opr bytes addr+1 ) 
         dup c@         ( opr bytes addr+1 opc )
         swap char+ @   ( opr bytes opc xt ) 
         futurecall? if execute else drop then  ( opr bytes ) 
         b,             ( opr bytes )  \ store opcode 
         split&save ; 


' tardis   4c 2  futureproof jmp 
' tardis   20 2  futureproof jsr

' tardis.l 5c 3  futureproof jmp.l 
' tardis.l 22 3  futureproof jsr.l 




\ TODO figure out what to do with this
\ Create an unresolved forward reference to the MOST SIGNIFICANT BYTE
\ of an unresolved forward label reference
\ : msb> ( "name" -- adr ) 
   \ addlabel 
   \ cell 2* ,     \ save 2* cell size as offset to the replacement jump table
   \ 0 ;           \ save 0000 as dummy value 

\ TODO figure out what to do with this
\ Create an unresolved forward reference to the LEAST SIGNIFICANT BYTE
\ of an unresolved forward label reference
\ : lsb> ( "name" -- adr ) 
   \ addlabel 
   \ cell 3 * ,    \ save 3* cell size as offset to the replacement jump table
   \ 0 ;           \ save 0000 as dummy value 

\ TODO figure out what to do with this
\ Create an unresolved forward reference to the BANK BYTE
\ of an unresolved forward label reference
\ : bank> ( "name" -- adr ) 
   \ addlabel 
   \ cell 4 * ,    \ save 4* cell size as offset to the replacement jump table
   \ 0 ;           \ save 0000 as dummy value 


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
\ OPCODE HELPER FUNCTIONS

\ caclulate short branch (8-bit, BRA etc). This assumes we are given 
\ the address of where we branch to, not the offset, which we calculate. 
\ The assumption is that the branch will always be used with labels. 
: makeshortbranch ( 65addr -- ) 
   lc -  1-
   dup short-branchable?  if b, 
      else  ." Error: Short branch out of range, lc: " lc .  
            space ." bc: " bc . cr  drop then ; 

\ caclulate long branch (16-bit, BRA.L ) This assumes we are given 
\ the address of where we branch to, not the offset, which we calculate. 
\ The assumption is that the branch will always be used with labels. 
: makelongbranch ( 65addr -- ) 
   lc -  2 -  
   dup long-branchable?  if w, 
      else  ." Error: Long branch out of range, lc: " lc .  
            space ." bc: " bc . cr  drop then ; 

\ format and fix sequence of operands for the block move commands
\ MVN and MVP: Assembler is "src dest mvn", and machine code sequence
\ is "<opc> <dest> <src>". 
: saveblocks ( src dest -- )  lsb b, lsb b, ; 

  
\ -----------------------
\ DEFINE OPCODES: WORK ROUTINES 

\ handle SIMPLE opcodes
: 1byte  ( opcode -- ) ( -- ) 
   create c,
   does> c@ b, ; 

: 2byte ( opcode -- ) ( c -- )
   create c,
   does> c@ b, b, ; 

: 3byte ( opcode -- ) ( w -- )
   create c,
   does> c@ b, w, ; 

: 4byte ( opcode -- ) ( lw -- )
   create c, 
   does> c@ b, lw, ; 


\ handle SHORT branch instructions (BRA, etc); BRANCH is reserved by Forth
: twig  ( opcode -- )  ( w -- ) 
   create c,
   does> c@ b, makeshortbranch ; 

\ handle LONG branch instructions (BRA.L); BRANCH is reserved by Forth
: bigtwig  ( opcode -- )  ( w -- ) 
   create c,
   does> c@ b, makelongbranch ; 

\ handle BLOCK MOVE instructions (MVN, MVP), which have a reverse order of
\ operands in machine code and assembler. BLOCK AND MOVE are reserved by Forth
: blkmov ( opcode -- ) ( c c -- ) 
   create c,
   does> c@ b, saveblocks ; 


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
\ OPCODE TABLE 

00 2byte brk       01 2byte ora.dxi    02 2byte cop      03 2byte ora.s
04 2byte tsb.d     05 2byte ora.d      06 2byte asl.d    07 2byte ora.dil
08 1byte php       ( 09 see below )    0a 1byte asl.a    0b 1byte phd
0c 3byte tsb       0d 3byte ora        0e 3byte asl      0f 4byte ora.l

10 twig bpl        11 2byte ora.diy    12 2byte ora.di   13 2byte ora.siy
14 2byte trb.d     15 2byte ora.dx     16 2byte asl.dx   17 2byte ora.dily
18 1byte clc       19 3byte ora.y      1a 1byte inc.a    1b 1byte tcs
1c 3byte trb       1d 3byte ora.x      1e 3byte asl.x    1f 4byte ora.lx

( 20 see above )   21 2byte and.dxi    ( 22 see above )  23 2byte and.s
24 2byte bit.d     25 2byte and.d      26 2byte rol.d    27 2byte and.dil
28 1byte plp       ( 29 see below )    2a 1byte rol.a    2b 1byte pld
2c 3byte bit       2d 3byte and. ( !)  2e 3byte rol      2f 4byte and.l

30 twig bmi        31 2byte and.diy    32 2byte and.di   33 2byte and.siy
34 2byte bit.dxi   35 2byte and.dx     36 2byte rol.dx   37 2byte and.dily
38 1byte sec       39 3byte and.y      3a 1byte dec.a    3b 1byte tsc
3c 3byte bit.x     3d 3byte and.x      3e 3byte rol.x    3f 4byte and.lx

40 1byte rti       41 2byte eor.dxi    ( 42 see below )  43 2byte eor.s
44 blkmov mvp      45 2byte eor.d      46 2byte lsr.d    47 2byte eor.dil
48 1byte pha       ( 49 see below )    4a 1byte lsr.a    4b 1byte phk
( 4c see above)    4d 3byte eor        4e 3byte lsr      4f 4byte eor.l

50 twig bvc        51 2byte eor.diy    52 2byte eor.di   53 2byte eor.siy
54 blkmov mvn      55 2byte eor.dx     56 2byte lsr.dx   57 2byte eor.dily
58 1byte cli       59 3byte eor.y      5a 1byte phy      5b 1byte tcd
( 5c see above)    5d 3byte eor.x      5e 3byte lsr.x    5f 4byte eor.lx

60 1byte rts       61 2byte adc.dxi    62 bigtwig phe.r  63 2byte adc.s
64 2byte stz.d     65 2byte adc.d      66 2byte ror.d    67 2byte adc.dil
68 1byte pla       ( 69 see below )    6a 1byte ror.a    6b 1byte rts.l
6c 3byte jmp.i     6d 3byte adc        6e 3byte ror      6f 4byte adc.l

70 twig bvs        71 2byte adc.diy    72 2byte adc.di   73 2byte adc.siy
74 2byte stz.dx    75 2byte adc.dx     76 2byte ror.dx   77 2byte adc.dily
78 1byte sei       79 3byte adc.y      7a 1byte ply      7b 1byte tdc
7c 3byte jmp.xi    7d 3byte adc.x      7e 3byte ror.x    7f 4byte adc.lx

80 twig bra        81 2byte sta.dxi    82 bigtwig bra.l  83 2byte sta.s
84 2byte sty.d     85 2byte sta.d      86 2byte stx.d    87 2byte sta.dil
88 1byte dey       ( 89 see below )    8a 1byte txa      8b 1byte phb
8c 3byte sty       8d 3byte sta        8e 3byte stx      8f 4byte sta.l

90 twig bcc        91 2byte sta.diy    92 2byte sta.di   93 2byte sta.siy
94 2byte sty.dx    95 2byte sta.dx     96 2byte stx.dy   97 2byte sta.dily
98 1byte tya       99 3byte sta.y      9a 1byte txs      9b 1byte txy 
9c 3byte stz       9d 3byte sta.x      9e 3byte stz.x    9f 4byte sta.lx

( a0 see below )  0a1 2byte lda.dxi   ( a2 see below )  0a3 2byte lda.s
0a4 2byte ldy.d   0a5 2byte lda.d     0a6 2byte ldx.d   0a7 2byte lda.dil
0a8 1byte tay     ( a9 see below )    0aa 1byte tax     0ab 1byte plb
0ac 3byte ldy     0ad 3byte lda       0ae 3byte ldx     0af 4byte lda.l

0b0 twig bcs      0b1 2byte lda.diy   0b2 2byte lda.di  0b3 2byte lda.siy
0b4 2byte ldy.dx  0b5 2byte lda.dx    0b6 2byte ldx.dy  0b7 2byte lda.dily
0b8 1byte clv     0b9 3byte lda.y     0ba 1byte tsx     0bb 1byte tyx
0bc 3byte ldy.x   0bd 3byte lda.x     0be 3byte ldx.y   0bf 4byte lda.lx

( c0 see below )  0c1 2byte cmp.dxi   0c2 2byte rep     0c3 2byte cmp.s
0c4 2byte cpy.d   0c5 2byte cmp.d     0c6 2byte dec.d   0c7 2byte cmp.dil
0c8 1byte iny     ( c9 see below )    0ca 1byte dex     0cb 1byte wai
0cc 3byte cpy     0cd 3byte cmp       0ce 3byte dec     0cf 4byte cmp.l

0d0 twig bne      0d1 2byte cmp.diy   0d2 2byte cmp.di  0d3 2byte cmp.siy
0d4 2byte phe.di  0d5 2byte cmp.dx    0d6 2byte dec.dx  0d7 2byte cmp.dily
0d8 1byte cld     0d9 3byte cmp.y     0da 1byte phx     0db 1byte stp 
0dc 3byte jmp.il  0dd 3byte cmp.x     0de 3byte dec.x   0df 4byte cmp.lx

( 0e0 see below ) 0e1 2byte sbc.dxi   0e2 2byte sep     0e3 2byte sbc.s
0e4 2byte cpx.d   0e5 2byte sbc.d     0e6 2byte inc.d   0e7 2byte sbc.dil
0e8 1byte inx     ( 0e9 see below )   0ea 1byte nop     0eb 1byte xba 
0ec 3byte cpx     0ed 3byte sbc       0ee 3byte inc     0ef 4byte sbc.l

0f0 twig beq      0f1 2byte sbc.diy   0f2 2byte sbc.di  0f3 2byte sbc.siy
0f4 3byte phe.#   0f5 2byte sbc.dx    0f6 2byte inc.dx  0f7 2byte sbc.dily
0f8 1byte sed     0f9 3byte sbc.y     0fa 1byte plx     ( fb xce see below )
0fc 3byte jsr.xi  0fd 3byte sbc.x     0fe 3byte inc.x   0ff 4byte sbc.lx


\ 8/16-BIT HYBRID INSTRUCTIONS: We have twelve instructions that need to be 
\ handled separately depending on the size of A and/or the X/Y Registers

 09 2byte ora.#8     09 3byte ora.#16    defer ora.# 
 29 2byte and.#8     29 3byte and.#16    defer and.# 
 49 2byte eor.#8     49 3byte eor.#16    defer eor.# 
 69 2byte adc.#8     69 3byte adc.#16    defer adc.# 
 89 2byte bit.#8     89 3byte bit.#16    defer bit.# 
0a0 2byte ldy.#8    0a0 3byte ldy.#16    defer ldy.# 
0a2 2byte ldx.#8    0a2 3byte ldx.#16    defer ldx.# 
0a9 2byte lda.#8    0a9 3byte lda.#16    defer lda.# 
0c0 2byte cpy.#8    0c0 3byte cpy.#16    defer cpy.# 
0c9 2byte cmp.#8    0c9 3byte cmp.#16    defer cmp.# 
0e0 2byte cpx.#8    0e0 3byte cpx.#16    defer cpx.# 
0e9 2byte sbc.#8    0e9 3byte sbc.#16    defer sbc.# 

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
      18 = invert  swap  38 = invert  and
      if cr ." Warning: No CLC or SEC before XCE in byte " lc . cr then ; 

\ wdm: Warn if we encounter this command
: wdm ( b -- ) 
   42 b, b, 
   cr ." Warning: WDM instruction encountered in byte " lc . cr ; 


\ SYNONYMS: We use systematic names ("jmp.l") where WDC defines 
\ distinct opcodes ("JML"). For people who insist on these, we
\ we define the WDC codes as synonyms
: jsl jsr.l ;   : jml jmp.l ;   : per phe.r ;   : rtl rts.l ; 
: brl bra.l ;   : pei phe.di ;  : pea phe.# ; 


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
