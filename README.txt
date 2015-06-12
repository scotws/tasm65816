A Typist's 65816 Assembler in Forth
Scot W. Stevenson <scot.stevenson@gmail.com>
First version: 31. May 2015
This version: 12. Jun 2015 (ALPHA 0.1)

This is an assembler for the 65815 8/16-bit hybrid MPU written in gforth. It is ALPHA software, which means that lots of stuff is missing and other stuff won't work.


WHAT YOU SHOULD DO

Read MANUAL.txt where everything is explained. Then look at the example files

        example.fs
        rom.fs 

You can use a simulator such as crude65816 (https://github.com/scotws/crude65816) to test the result. 


WHAT YOU SHOULD KNOW

This assembler makes a few assumptions:

- It is written by and primary for people who touch-type -- hence the name. This is why there are few special characters (like $ or [) in the syntax: They take too much time to reach with your fingers. 

- It is written for people who have at least some basic familiarity with Forth. The Manual does some hand-holding, but not much.  

- It assumes you are using gforth (https://www.gnu.org/software/gforth/). The source code points out where gforth-specific words are used that are not part of ANS Forth.

Again, read the Manual.


WHAT ELSE IS IMPORTANT

Use this software at your own risk.

