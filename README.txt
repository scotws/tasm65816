A Typist's 65816 Assembler in Forth
Scot W. Stevenson <scot.stevenson@gmail.com>
First version: 31. May 2015
This version:  14. July 2015 (Pluto flyby day)

This is an single-pass cross-assembler for the 65815 8/16-bit hybrid MPU, the
big brother of the famous 6502, written in Gforth. It is BETA software, which
means that though it has all the functionality it should and nothing is
obviously broken, it hasn't been tested as completely as it should. Use at your
own risk. 

This assembler has its own syntax that is optimized for touch-typists -- few
upper case characters, few special characters, some commands renamed. For
example, instead of 
        
        LDA $1000,x

we would write

        lda.x 1000 

except that this is an assembler in Forth, so we use postfix notation (aka 
"Reverse Polish Notation" (RPN) or "that ass-backwards stuff"), so we end up with

        1000 lda.x

Yes, this takes a bit getting used to. However, being able to use the full power of
Forth in the assembler code is well worth it. Well, at least I think so.

If you've read this far, you probably want to take a look at the manual in 
docs/MANUAL.txt . There are also two example assembler files included: 

        example.fs      - with comments on the most common features
        rom65c02.fs     - a ROM for the 65c02 for testing

The binary file that results from the ROM listing does not contain any 65816
commands that are not available on the 65c02, so it can be tested with a 65c02
emulator such as py65mon. 

Finally, there is thread at 6502.org about the assembler: 
http://forum.6502.org/viewtopic.php?f=2&t=3326 . 
