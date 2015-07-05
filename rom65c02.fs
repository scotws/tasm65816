\ Example 8 KB ROM System for 
\ A Typist's 65816 Assembler in Forth
\ Scot W. Stevenson <scot.stevenson@gmail.com>
\ First version: 31. May 2015
\ This version: 28. Jun 2015 (Tau Day) 

\ After assembly, this creates an 8 kb binary file that can be 
\ loaded to $E000 a 65c02 simulator such as p65mon you can use this 
\ as template. No 65816 instructions are used to allow of further testing. 

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

   hex
   cr .( Starting assembly ... )

\ --- DEFINITIONS ---

   0e000 origin 

   00f001 value putchr  \ py65mon address for character output
   00f004 value getchr  \ py65mon address to receive character input

\ --- STRINGS ---
\ Life is easier if you put these at the beginning of the text: It's
\ tricky to put unresolved forward references in macros

   -> intro
      s" ------------------------------------------------" strlf, 
      s" Test ROM for A Typist's 65816 Assembler in Forth" strlf, 
      s" Scot W. Stevenson <scot.stevenson@gmail.com>" strlf, 
      s" ------------------------------------------------" str0, 


\ --- SUBROUTINES --- 
\ These, too, should go before the main code if at all possible

   \ Print a zero-terminated string. Assumes address in $00, $01
   \ Assumes A and Y are in 8-bit mode
   -> prtstr
                   phy 
                00 ldy.#
   -> nxtchr
                00 lda.diy
           <? fini beq
            putchr sta
                   iny
            nxtchr bra
   -> fini 
                   ply
                   rts

\ --- MACROS ---
\ There are no real "macros" in the traditional sense because we have the
\ whole Forth language as a basis. Define words normally in the assembler
\ code to use as macros:

   \ Macro to print one linefeed. Assumes A in 8-bit mode
   : .linefeed  ( -- )   
                0a lda.#   
            putchr sta  ; 

   \ Macro to print a string. Note this doesn't work with strings
   \ that were defined lower down because it gets tricky with
   \ unresolved links. Gforth already uses .STRING 
   \ Assumes A is in 8-bit mode
   : .str ( link -- ) 
      dup  
               lsb lda.#   
                00 sta.d
               msb lda.#   
                01 sta.d
            prtstr jsr  ; 


\ --- MAIN CODE --- 

   \ All of our vectors go here because we're cheap 
   -> vectors 

   \ Print the intro string
   intro .str   
         .linefeed 

   \ Print a bunch of 'a' so we have at least one loop
                09 ldy.# 
            char a lda.#      
   -> nexta
            putchr sta
                   dey
             nexta bne

    \ That's about enough for today. Note signature byte with BRK
                01 brk


\ --- INTERRUPT VECTORS --- 
   
   \ skip to interrupt vectors, filling rest of the image with zeros
   0fffa advance 
   
   vectors w, \ NMI vector
   vectors w, \ Reset vector
   vectors w, \ IRQ vector

   end            

\ ----------------------------------- 
   cr .( ... assembly finished. ) 

   \ uncomment next line to save the hex dump to the file "rom.bin"
   2dup save rom65c02.bin 
