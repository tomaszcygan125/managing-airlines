       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02271.                                              
      ******************************************************************
      *                                                                 
      * THIS PROGRAM IS CALLED WHEN USER WILL CHOOSE OPTION '4' ON      
      * THE FIRST PROGRAM (Z02131) "FLIGHT MANAGMENTS"                  
      *                                                                 
      * HERE PROGRAM WILL HAVE TO CHOOSE 1 OUT OF 2 OPTIONS:            
      *                                                                 
      *         1. ADD A FLIGHT                                         
      *         2. REMOVE A FLIGHT                                      
      *                                                                 
      * USER WILL CHOOSE 1 OPTION BY PLACING 'X' NEXT TO IT             
      * AND PRESSING ENTER                                              
      *                                                                 
      *  IF HE WILL CHOOSE OPTION '1' THEN PROGRAM Z02281 WILL BE       
      * CALLED                                                          
      *                                                                 
      *   IF HE WILL CHOOSE OPTION '2' THEN PROGRAM Z022321 WILL BE     
      * CALLED                                                          
      ******************************************************************
      *                  CHANGE LOG                                     
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
                                                                        
      ******************************************************************
      *                        DATA DIVISION                            
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0227.                                               
           COPY ZZEC0215.                                               
       01 WS-ITER     PIC S9(4) COMP VALUE 0.                           
      * COMMAREA                                                        
       01 WS-COMMAREA PIC X VALUE 'A'.                                  
       01 WS-EIBRESP-TEMP  PIC X(10) VALUE SPACE.                       
      * CONSTANTS                                                       
       01 CT-CONSTANTS.                                                 
           05 CT-ERROR-ROUTINE                PIC X(8) VALUE 'Z02141'.  
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02271'.  
           05 CT-FINAL-MESSAGE                PIC X(79)                 
                                                 VALUE 'END OF PROGRAM'.
           05 CT-FIRST-PROGRAM-NAME           PIC X(8) VALUE 'Z02131  '.
                                                                        
           05 CT-ADD-A-FLIGHT                 PIC X(8) VALUE 'Z02292  '.
           05 CT-REMOVE-PROG                  PIC X(8) VALUE 'Z02321  '.
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME               PIC X.        
               88 SO-FIRST-TIME-PROGRAM-RUNS              VALUE 'Y'.    
               88 SO-NOT-FIRST-TIME-PROGRAM-RUNS          VALUE 'N'.    
           05 SW-WHAT-TYPE-OF-SEND                        PIC X.        
               88 SO-SEND-WHOLE-MAP                       VALUE 'M'.    
               88 SO-SEND-ONLY-DATA                       VALUE 'D'.    
           05 SW-WHAT-TYPE-OF-FINAL                       PIC X.        
               88 SO-FINAL-WITH-COMMAREA                  VALUE 'C'.    
               88 SO-FINAL-TERMINATION                    VALUE 'F'.    
           05 SW-USER-CHOICE                              PIC X.        
               88 SO-BOOK-A-FLIGHT                        VALUE '1'.    
               88 SO-FIND-A-FLIGHT                        VALUE '2'.    
               88 SO-FIND-BOOKING                         VALUE '3'.    
               88 SO-FLIGHT-MANAGMENT                     VALUE '4'.    
       01 WS-VARIABLES.                                                 
           05 WS-FIRST-OPTION                   PIC X.                  
           05 WS-SECOND-OPTION                  PIC X.                  
      ******************************************************************
      *                      PROCEDURE DIVISION                         
      ******************************************************************
       PROCEDURE DIVISION.    
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                     1000-INIT                                   
      ******************************************************************
       1000-INIT.                                                       
           PERFORM 1010-CHECK-IF-FIRST-TIME                             
           PERFORM 1011-INITIALIZE-COPYBOOK                             
           .                                                            
      ******************************************************************
      *                  1005-CICS-IGNORE                               
      ******************************************************************
       1005-CICS-IGNORE.                                                
           EXEC CICS                                                    
            IGNORE CONDITION ERROR                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                1010-CHECK-IF-FIRST-TIME                         
      ******************************************************************
       1010-CHECK-IF-FIRST-TIME.                                        
           IF EIBCALEN = 0         THEN                                 
                                                                        
              PERFORM 1005-CICS-IGNORE                                  
              SET SO-FIRST-TIME-PROGRAM-RUNS TO TRUE                    
           ELSE                                                         
              SET SO-NOT-FIRST-TIME-PROGRAM-RUNS TO TRUE                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  1011-INITIALIZE-COPYBOOK                       
      ******************************************************************
       1011-INITIALIZE-COPYBOOK. 
           MOVE LOW-VALUES TO WS-ZZEC0215                               
           .                                                            
      ******************************************************************
      *                      2000-PROCESS                               
      ******************************************************************
       2000-PROCESS.                                                    
           IF SO-FIRST-TIME-PROGRAM-RUNS THEN                           
              PERFORM 2001-PROCESS-IF-FIRST-TIME                        
           ELSE                                                         
              PERFORM 2002-PROCESS-IF-NOT-FIRST-TIME                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2001-PROCESS-IF-FIRST-TIME                       
      ******************************************************************
       2001-PROCESS-IF-FIRST-TIME.                                      
           PERFORM 2050-SEND-CLEAN-MAP                                  
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           .                                                            
      ******************************************************************
      *               2002-PROCESS-IF-NOT-FIRST-TIME                    
      ******************************************************************
       2002-PROCESS-IF-NOT-FIRST-TIME.                                  
           EVALUATE EIBAID                                              
             WHEN DFHENTER                                              
               PERFORM 2150-PROCESS-USER-CHOICE                         
             WHEN DFHPF3                                                
               SET SO-FINAL-TERMINATION TO TRUE                         
             WHEN OTHER                                                 
               MOVE 'YOU HAVE PRESSED NO ACTION KEY '                   
               TO WS-Z02141-I-ERROR-MESSAGE(1)                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2050-SEND-CLEAN-MAP      
      ******************************************************************
       2050-SEND-CLEAN-MAP.                                             
           MOVE LOW-VALUES TO MP0227O                                   
           SET SO-SEND-WHOLE-MAP TO TRUE                                
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                    2050-SEND-CLEAN-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
                                                                        
           EXEC CICS                                                    
             SEND MAP('MP0227') MAPSET('MP0227')                        
             FROM(MP0227O)                                              
             ERASE                                                      
           END-EXEC                                                     
                                                                        
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2150-PROCESS-USER-CHOICE                       
      ******************************************************************
       2150-PROCESS-USER-CHOICE.                                        
           PERFORM 2151-GET-USER-CHOICE                                 
           PERFORM 2152-EVALUATE-USER-CHOICE                            
           .                                                            
      ******************************************************************
      *                    2151-GET-USER-CHOICE                         
      ******************************************************************
       2151-GET-USER-CHOICE.                                            
           MOVE LOW-VALUES TO MP0227I                                   
                                                                        
           EXEC CICS                                                    
           RECEIVE MAP('MP0227') MAPSET('MP0227')                       
           INTO(MP0227I)                                                
           NOHANDLE        
           END-EXEC                                                     
           MOVE CHOI1I TO WS-FIRST-OPTION                               
           MOVE CHOI2I TO WS-SECOND-OPTION                              
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      * USER CAN CHOOSE 1 OUT OF 2 OPTIONS                              
      * FIRST OPTION IS ( "ADD A FLIGHT" )                              
      * SECOND OPTION IS ( "REMOVE A FLGIHT" )                          
      *                                                                 
      * USER CAN'T CHOOSE BOTH OPTIONS                                  
      *                                                                 
      *                                                                 
      * IF USER WILL PROVIDE VALID CHOICE THEN NEXT PROGRAMS WILL BE    
      * CALLED                                                          
      *                                                                 
      * IF NOT ERROR MESSAGE WILL BE DISPLAYED                          
      *                                                                 
      * IF FIRST OPTION IS TRUE      Z02292   WILL BE CALLED            
      * IF SECOND OPTION IS TRUE     Z022321  WILL BE CALLED            
      ******************************************************************
       2152-EVALUATE-USER-CHOICE.                                       
           IF WS-FIRST-OPTION = 'X' AND WS-SECOND-OPTION = 'X' THEN     
              MOVE ' PLEASE CHOOSE ONLY 1 OPTION   '                    
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           IF WS-FIRST-OPTION = 'X' THEN                                
              PERFORM  2610-CALL-ADD-A-FLIGHT                           
              DISPLAY 'CALL A AFLIGHT'                                  
           END-IF                                                       
           IF WS-SECOND-OPTION = 'X' THEN                               
              PERFORM  2620-CALL-REMOVE-A-FLIGHT                        
              DISPLAY 'CALL A REMOVE FLIGHT '                           
           END-IF         
           MOVE ' PROVIDE VALID INPUT        '                          
           TO WS-Z02141-I-ERROR-MESSAGE(1)                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
      * CALL DO SE ZEBY WYSWIETLIL WIADOMOSC                            
              DISPLAY 'MAPFAIL'                                         
              MOVE ' YOU NEED TO PROVIDE  DATA IN CHOICE FIELD '        
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
      * CALL DO PROGRAM KTORY WYSYPUJE                                  
              DISPLAY 'UNKNOWN EIBERSP '                                
              DISPLAY 'EIBRESP VALUE : ' EIBRESP                        
              MOVE EIBRESP TO WS-EIBRESP-TEMP                           
              STRING '  UNKNOWN EIBERSP '  'EIBRESP VALUE : '           
              WS-EIBRESP-TEMP                                           
              DELIMITED BY SIZE INTO WS-Z02141-I-ERROR-MESSAGE(1)       
              END-STRING                                                
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2300-CALL-ERROR-ROUTINE                        
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
           SET SO-Z02141-I-FIRST-TIME TO TRUE
                                                                        
           MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM     
           SET SO-Z02141-M-NO-DATA  TO TRUE                             
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE) COMMAREA(WS-ZZEC0215)        
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    2610-CALL-ADD-A-FLIGHT                       
      * PROGRAM MOVES LOGIC TO NEXT PROGRAM                             
      * NEXT PROGRAM DON'T USE COMMAREA SO WE  PASS NOTHING             
      ******************************************************************
       2610-CALL-ADD-A-FLIGHT.                                          
           SET  SO-M-FIRST-WITHOUT TO TRUE                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-ADD-A-FLIGHT)                              
                COMMAREA(WS-ZZEC0215)                                   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2620-CALL-REMOVE-A-FLIGHT                    
      ******************************************************************
       2620-CALL-REMOVE-A-FLIGHT.                                       
           DISPLAY 'JESTESMY W 2620 CALL TO REMOVE'                     
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           EXEC CICS                                                    
             XCTL PROGRAM(CT-REMOVE-PROG) COMMAREA(WS-ZZEC0215)         
                LENGTH(0)                                               
           END-EXEC                                                     
           DISPLAY 'AFTER 2620 CALL '                                   
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     3000-FINAL                                  
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3001-REUTRN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
              PERFORM 3002-REUTRN-TO-CALLING-PROG                       
           WHEN OTHER                                                   
              MOVE ' 3000 PARA ERROR SHOULDNT HAPPEN  '                 
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    3001-REUTRN-WITH-TRANSID                     
      * PARAGRAPH WILL END PROGRAM WITH OPTION TO RETRIGGER AFTER       
      * USER WILL PRESS ATTENTION KEY                                   
      ******************************************************************
       3001-REUTRN-WITH-TRANSID.                                        
           EXEC CICS                                                    
               RETURN TRANSID('0222') COMMAREA(WS-ZZEC0215)             
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   3002-REUTRN-TO-CALLING-PROG                   
      * PARAGRAPH WILL RETURN CONTROL TO CALLING PROGRAM                
      *                                                                 
      * CALLING PROGRAM HERE IS A FIRST PROGRAM IN THIS APPLICATION     
      * THIS PROGRAM DON'T USE FLAGS TO DETERMINE IF IT IS CALLED       
      * FOR THE FIRST TIME OR NOT                                       
      *                                                                 
      * SO WE HAVE TO CALL IT WITH LENGTH(0) ONLY THEN PROGRAM WILL     
      * BE CALLED CORRECTLY                                             
      ******************************************************************
       3002-REUTRN-TO-CALLING-PROG.                                     
           EXEC CICS
              XCTL PROGRAM(CT-FIRST-PROGRAM-NAME)   
                 COMMAREA(WS-ZZEC0215) LENGTH(0)    
           END-EXEC                                 
           PERFORM 2200-CHECK-EIBRESP               
           .                                                                                            

                           
                                                                        
                                              
                                             
                    
                                       
                                          
