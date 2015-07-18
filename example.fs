\ Example assembly source file for 
\ A Typist's 65816 Assembler in Forth
\ Scot W. Stevenson <scot.stevenson@gmail.com>
\ First version: 31. May 2015
\ This version: 18. July 2015

\ Remember this is assembler source file is actually a Forth programm listing
\ as far as Forth is concerned. As such, the file type should be .fs instead
\ of .asm if you want correct syntax highlighting with an editor such as vi

\ To test assembly: Start gforth with "-m 1G" parameter, then 
\ INCLUDE tasm65816.fs, INCLUDE example.fs  However, do not try to run the 
\ resulting code, which contains infinite loops!

        \ we can use all the normal Forth commands, which is why this line
        \ is a comment. The semicolon will not work for comments
        
        \ HEX should actually be redundant, but still. Now we get rid of
        \ all those nasty dollar signs. If you need decimal, just use 
        \ the Forth command DECIMAL
        hex

        \ comments marked with .( will be printed during assembly
        cr .( WARNING: DO NOT TRY TO EXECUTE THE RESULTING CODE ) 
        cr .( Starting assembly ... )

        \ ORIGIN sets the target address on 65816 machine. If you don't
        \ provide this, the code will start to assemble at 000000, or 
        \ at the beginning of a bank. Note we start hex numbers that start
        \ with a letter with an extra zero so there can be no confusion
        \ with a Forth command
        0c000 origin

        \ The assembler starts out with the assumption that the processor is
        \ in emulated mode like after a reset. To be sure, we can include
        \ the mode sequence
        emulated

        \ because this is actually a Forth file, we can put more than one 
        \ instruction in a row. See MANUAL.txt for spacing conventions
        nop nop nop nop nop 

        \ Oh, and we can put comments in brackets right in the middle of 
        \ the code
        nop ( not again! ) nop ( stop! ) nop ( help! ) nop 

        \ Instructions that have an operand put it before the opcode (the 
        \ Forth "reverse polish notation" (RPN) or "postfix" thing). See 
        \ MANUAL.txt for the syntax of various addressing modes
          00 lda.#     \ conventional syntax: LDA #$00
             tax
        1020 sta.x     \ conventional syntax: STA $1020,X

        \ Special case: AND gets a dot in absolute mode to distinguish it 
        \ from the Forth command of the same name
        1021 and.

        \ store bytes with the B, assembler command, not the normal Forth C, 
        \ instruction because they go in the staging area, not the Forth
        \ dictionary
        0ff b, 

        \ if we want to do lots of this, we can just use a LOOP
        : 4xff ( -- )  4 0 do  0ff b,  loop ; 
        4xff

        \ store words in correct little-endian format with W,
        1122 w, 3344 w,    \ results in 22 11 44 33

        \ store those 24-bit words in correct little-end format with LW,
        556677 lw,         \ results in 77 66 55

        \ store strings with a combination of S" and STR, (S, is reserved 
        \ by Gforth). There are also words to store strings zero-terminated
        \ or linefeed-terminated
        s" cats are cool" str, 

        \ define variables with VALUE instead of a special command such as
        \ ".EQU" because this is Forth
        88 value animal 
             animal lda.d

        \ conditional assembly is trivial with Forth 
        : cat? ( u --- ) 
            88 = if  s" nice" str,  else  s" damn" str,  then ; 

        animal cat?  \ stores the "nice" string (of course) 

        \ LC gives us the current address being assembled (the "*"
        \ of other assemblers). Use normal Forth math functions to 
        \ manipulate it
        lc 2 +   jmp 
                 nop 

        \ we define labels with "->" ("-->" would be easier to read, 
        \ but it is already used by ancient Forth BLOCK commands). Put 
        \ the word at the very beginning of the line or the indent level
        -> hither

        \ backward jumps: just put the label (or absolute address) first,
        \ no other commands required
          hither jmp 

        \ backward branches: work the same, because branch instructions 
        \ assume they will be handed either absolute addresses or labels 
          hither bra

        \ if we want to enter the relative address by hand, we trick the
        \ assembler by adding the offset in bytes to the current address
        \ (note this might warrent a separate word in future)
                 nop 
           lc 1- bra 

        \ forward jumps are a pain in the rear for single-pass assemblers
        \ (which is what this is). We solve this by adding future reference
        \ directives such as <j and <b. 
         <j frog jsr

        \ once we have defined the label, we can go back to using normal
        \ links as above
        -> frog
             0ff lda.# 
              00 sta.d 
            frog bra

        \ ready for the big times? Then lets switch to 16-bit mode
        native a:16 xy:16 

        \ let's make sure that really worked 
        : a16assert ( -- ) a=16? if  cr ." Yes, A is 16 bits" cr then ; 
        a16assert

        \ now our LDA commands are bigger
           0aabb lda.# 
            1000 sta

        \ well, enough of this 
                 stp

        \ more comments printed to the screen
        .( all done.) cr 

        \ End assembly, put buffer address and length of compiled machine 
        \ This is required to get to the next step
        end            

        \ or have the machine print out the hex code at the end itself
        cr 2dup dump

        \ uncomment next line to save the hex dump to the file "example.bin"
        \ 2dup save example.bin 
