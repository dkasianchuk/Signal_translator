# Signal_translator
GRAMMAR:


  1. <signal-program> --> <program> 
  2. <program> --> PROGRAM <procedure-identifier>;<block>.
  3. <block> --> <declarations> BEGIN <statements-list> END 
  4. <declarations> --> <constant-declarations> 
  5. <constant-declarations> --> CONST <constant-declarations-list> 
                               |<empty> 
  6. <constant-declarations-list> --> <constant-declaration> <constant-declarations-list> 
                                    |<empty> 
  7. <constant-declaration> --> <constant-identifier> = <constant>; 
  8. <statements-list> --> <statement> <statements-list> 
                         |<empty>
  9. <statement> --> <variable-identifier> := <constant> ; 
  10. <constant> --> <unsigned-integer> 
  11. <constant> --> - <unsigned-integer> 
  12. <constant-identifier> --> <identifier> 
  13. <variable-identifier> --> <identifier> 
  14. <procedure-identifier> --> <identifier> 
  15. <identifier> --> <letter><string> 
  16. <string> --> <letter><string> 
                 |<digit><string> 
                 |<empty> 
  17. <unsigned-integer> --> <digit><digits-string> 
  18. <digits-string> --> <digit><digits-string>
                        |<empty> 
  19. <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
  20. <letter> --> A | B | C | D | ... | Z  

