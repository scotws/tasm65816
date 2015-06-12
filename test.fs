\ Testing Suite for 
\ A Typist's 65816 Assembler in Forth
\ Scot W. Stevenson <scot.stevenson@gmail.com>
\ First version: 12. Jun 2015
\ This version: 12. Jun 2015

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
   emulation mode    \  38 0fb  --  A, X and Y should be 8 bit
      
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
   native mode       \  18 0fb -- A, X and Y should still be 8 bit 

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


   end 

\ ----------------------------------- 
   cr .( ... testing finished. ) cr

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
