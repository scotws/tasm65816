\ Unused code fragments from the tasm65816.fs
\ Scot W. Stevenson <scot.stevenson@gmail.com>
\ First version: 18. July 2015
\ This version: 18. July 2015

\ These might not work, might be dangerous, use at own risk. 


\ ---------------------
\ SPLITTING FUNCTIONS

\ Split up a 24 bit address in lsb, msb/lsb or bank/msb/lsb
\ depending on the parameter 1 to 3 given with the address. 
create splittable
   ' lsb ,   ' 16>msb/lsb ,   ' 24>bank/msb/lsb ,

: split-into-bytes  ( 24bit u -- lsb | msb lsb | bank msb lsb )
   1- cells       ( offset )  \ index to splittable
   splittable +   ( addr )
   @ execute ;

\ Use bytesplitter to save split-up 24-bit address in little-endian
\ format to staging area
: split&save  ( 24bit u -- )
   dup >r
   split-into-bytes
   r> 1+ 1 do b, loop ;

