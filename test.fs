\ Testing Suite for 
\ A Typist's 65816 Assembler in Forth
\ Scot W. Stevenson <scot.stevenson@gmail.com>
\ First version: 12. Jun 2015
\ This version: 26. Jun 2015

\ This is a primitive testing suite. Load it after starting Gforth with 
\ "gforth -m 1G" and "include tasm65816.fs" with the instruction
\ "include test.fs". This will create a test.bin binary file for an 
\ emulator, and also print its own testing results. 

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

   hex
   cr .( Starting test suite ... )

\ --- DEFINITIONS ---

   0e000 origin  \ required  

   00f001 value putchr  \ py65mon address for character output
   00f004 value getchr  \ py65mon address to receive character input

\ --- TESTING IMMEDIATE MODE --- 

   cr .( ... testing immediate mode, emulation ...) 
   emulated          \  38 0fb  --  A, X and Y should be 8 bit
      
      00 lda.#       \ 0a9 00 
      10 ora.#       \  09 10
     0ff and.#       \  29 0ff
      0f eor.#       \  49 0f
      01 adc.#       \  69 01
      01 sbc.#       \ 0e9 01 
      02 bit.#       \  89 02 
     0ff cmp.#       \ 0c9 0ff 
      00 ldy.#       \ 0a0 00
      00 cpy.#       \ 0c0 00 
     0ff ldx.#       \ 0a2 0ff
      01 cpx.#       \ 0e0 01 

   cr .( ... testing immediate mode, native, 8-bit ...) 
   native            \  18 0fb -- A, X and Y should still be 8 bit 

      00 lda.#       \ 0a9 00 
      10 ora.#       \  09 10
     0ff and.#       \  29 0ff
      0f eor.#       \  49 0f
      01 adc.#       \  69 01
      01 sbc.#       \ 0e9 01 
      02 bit.#       \  89 02 
     0ff cmp.#       \ 0c9 0ff 
      00 ldy.#       \ 0a0 00
      00 cpy.#       \ 0c0 00 
     0ff ldx.#       \ 0a2 0ff
      01 cpx.#       \ 0e0 01 


   cr .( ... testing immediate mode, native, 16-bit ...) 
         axy:16      \ 0c2 30  -- A, X and Y should be all 16 bit

    0001 lda.#       \ 0a9 01 00 
    1010 ora.#       \  09 10 10 
   0ffaa and.#       \  29 0aa 0ff
    0f0e eor.#       \  49 0e 0f
    0001 adc.#       \  69 01 00 
    0001 sbc.#       \ 0e9 01 00 
    0302 bit.#       \  89 02 03 
   0ffff cmp.#       \ 0c9 0ff 0ff
    0000 ldy.#       \ 0a0 00 00
    0000 cpy.#       \ 0c0 00 00
   0ffcc ldx.#       \ 0a2 0cc 0ff
    0001 cpx.#       \ 0e0 01 00 


    cr .( ... testing branching, must redefine BOTTOMLINK: ) 

   -> toplink      nop        \ 0ea

           toplink bra.l      \ 82 0fc 0ff 
           toplink bra        \ 80 0fa
           toplink jmp        \ 4c 5a 0e0
           toplink jsr        \ 20 5a 0e0
           toplink jmp.l      \ 5c 5a 0e0 00
           toplink jsr.l      \ 22 5a 0e0 00

    b>  bottomlink bra        \ 80 11
    j>  bottomlink jmp        \ 4c 81 0e0
    j>  bottomlink jsr        \ 20 81 0e0
    jl> bottomlink jmp.l      \ 5c 81 0e0 00
    jl> bottomlink jsr.l      \ 22 81 0e0 00
    bl> bottomlink bra.l      \ 82 00 00       

   -> bottomlink   nop        \ 0ea 

   cr .( ... testing move instructions ... ) 

   \ Note that MVP and MVN reverse the order of the operands from the 
   \ assembler to the actual machine code
            00 0a mvp         \ 44 0a 00 
           0ff 00 mvn         \ 54 00 0ff


\ ----------------------------------- 
   cr .( ... testing finished. ) cr

   end 

   \ uncomment next line to save the hex dump to the file "test.bin"
   2dup save test.bin 

   \ Put dump on screen for later comparison
   2dup dump 


\ --- CORRECT MACHINE CODE --- 

   create correctdump
   \ immediate mode, emulated
   38 c, 0fb c,   0a9 c, 00 c,   09 c, 10 c,    29 c, 0ff c, 
   49 c, 0f c,    69 c, 01 c,    0e9 c, 01 c,   89 c, 02 c, 
   0c9 c, 0ff c,  0a0 c, 00 c,   0c0 c, 00 c,   0a2 c, 0ff c, 
   0e0 c, 01 c, 

   \ native mode, 8-bit
   18 c, 0fb c,   0a9 c, 00 c,   09 c, 10 c,    29 c, 0ff c, 
   49 c, 0f c,    69 c, 01 c,    0e9 c, 01 c,   89 c, 02 c, 
   0c9 c, 0ff c,  0a0 c, 00 c,   0c0 c, 00 c,   0a2 c, 0ff c, 
   0e0 c, 01 c, 

   \ native mode, 16-bit 
         0c2 c, 30 c,    0a9 c, 01 c, 00 c,   
   09 c, 10 c, 10 c,     29 c, 0aa c, 0ff c, 
   49 c, 0e c, 0f c,     69 c, 01 c, 00 c, 
   0e9 c, 01 c, 00 c,    89 c, 02 c, 03 c, 
   0c9 c, 0ff c, 0ff c,  0a0 c, 00 c, 00 c, 
   0c0 c, 00 c, 00 c,    0a2 c, 0cc c, 0ff c, 
   0e0 c, 01 c, 00 c, 

   \ branch testing
 
   0ea c,   82 c, 0fc c, 0ff c,   80 c, 0fa c, 
   4c c, 5a c, 0e0 c,    20 c, 5a c, 0e0 c, 
   5c c,  5a c, 0e0 c, 00 c,    22 c, 5a c, 0e0 c, 00 c,    
   80 c, 11 c,    4c c, 81 c, 0e0 c,   20 c, 81 c, 0e0 c, 
   5c c, 81 c, 0e0 c, 00 c,    22 c, 81 c, 0e0 c, 00 c,    
   82 c, 00 c, 00 c,   0ea c,

   \ block moves 

   44 c, 0a c, 00 c,   54 c, 00 c, 0ff c, 



\ --- VALIDATION ROUTINES ---

   variable errorflag
   false errorflag !

   \ validate: make sure staging area and coredump are equal
   : comparedumps ( addr n -- addr n ) 
      dup 0  do  staging i + c@   correctdump i + c@
         = invert if ." Error at byte " i . cr  true errorflag !  then 
      loop ; 

   : .flagstatus ( f -- ) 
      invert if cr ." ### This was a triumph: No errors found! ### " cr  then ; 


\ --- COMPARE DUMPS ---

   cr .( Running dump comparison ...) cr 

   comparedumps  
   errorflag @  .flagstatus

   cr .( ... testing complete. There is cake in the lobby. ) cr  
