       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02231.                                              
      ******************************************************************
      *                                                                 
      *      PROGRAM IS CALLED WHER USER WANTS TO DELETE A FLIGTS       
      *      HERE USER WILL HAVE TO SPECIFY IF HE WANTS TO              
      *      DELETE A SINGLE FLIGHT OR A SCHEDULE FLIGHTS'              
      *                                                                 
      *     IN CASE HE WANTS TO DELETE A SINGLE FLIGHT Z02221 IS CALLED 
      *     IN CASE HE WANTS TO DELETE A SCHEUDLE THEN Z02312 IS CALLED 
      *                                                                 
      *                          (0226)                                 
      *                                                                 
      *                                                                 
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
           COPY ZZMP0232.                                               
           COPY ZZEC0215.                                               
       01 WS-ITER     PIC S9(4) COMP VALUE 0.                           
      * COMMAREA                                                        
       01 WS-COMMAREA PIC X VALUE 'A'.                                  
       01 WS-EIBRESP-TEMP  PIC X(10) VALUE SPACE.                       
      * CONSTANTS                                                       
       01 CT-CONSTANTS.                                                 
           05 CT-ERROR-ROUTINE                PIC X(8) VALUE 'Z02141'.  
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02321'.  
           05 CT-FINAL-MESSAGE                PIC X(79)                 
                                                 VALUE 'END OF PROGRAM'.
           05 CT-FIRST-PROGRAM-NAME           PIC X(8) VALUE 'Z02131  '.
                                                                        
           05 CT-CALLIING-PROGRAM-NAME        PIC X(8) VALUE 'Z02271  '.
           05 CT-ADD-SINGLE-PROG              PIC X(8) VALUE 'Z02292  '.
           05 CT-REM-A-SCHEDULE               PIC X(8) VALUE 'Z02312  '.
           05 CT-REM-A-SINGLE-PROG            PIC X(8) VALUE 'Z02221  '.
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
           05 WS-FIRST-CHOICE                  PIC X.                   
           05 WS-SECOND-CHOICE                 PIC X.                   
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
           MOVE LOW-VALUES TO MP0232O                                   
           SET SO-SEND-WHOLE-MAP TO TRUE 
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                    2050-SEND-CLEAN-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
                                                                        
           EXEC CICS                                                    
             SEND MAP('MP0232') MAPSET('MP0232')                        
             FROM(MP0232O)                                              
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
           MOVE LOW-VALUES TO MP0232I                                   
                                                                        
           EXEC CICS                                                    
           RECEIVE MAP('MP0232') MAPSET('MP0232')                       
           INTO(MP0232I)                                                
           NOHANDLE                                                     
           END-EXEC                                                     
           MOVE  CHOI1I  TO WS-FIRST-CHOICE                             
           MOVE  CHOI2I  TO WS-SECOND-CHOICE                            
           PERFORM 2200-CHECK-EIBRESP 
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      * WS-FIRST-CHOICE   -   REMOVE A SINGLE FLIGHT                    
      * WS-SECOND-CHOICE  -   REMOVE A SCHEDULE                         
      ******************************************************************
       2152-EVALUATE-USER-CHOICE.                                       
           DISPLAY 'CHOI1I ' CHOI1I                                     
           IF WS-FIRST-CHOICE = 'X' AND WS-SECOND-CHOICE = 'X' THEN     
              MOVE ' PLEASE CHOOSE ONLY 1 OPTION   '                    
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           IF WS-FIRST-CHOICE = 'X' THEN                                
              PERFORM  2610-CALL-REM-A-SINGLE-FLIGHT                    
              DISPLAY 'CALL ADD SINGLE '                                
           END-IF                                                       
           IF WS-SECOND-CHOICE = 'X' THEN                               
              PERFORM  2620-CALL-REM-A-SCHED-FLIGHT                     
              DISPLAY 'CALL ADD SCHEDULED '                             
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
      ******** *********************************************************
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
      *                   2610-CALL-REM-A-SINGLE-FLIGHT                 
      ******************************************************************
       2610-CALL-REM-A-SINGLE-FLIGHT.                                   
           CONTINUE                                                     
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-PROVIDE-ONLY-FLIGHT-N TO TRUE                         
           EXEC CICS                                                    
            XCTL PROGRAM(CT-REM-A-SINGLE-PROG) COMMAREA(WS-ZZEC0215)   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2620-CALL-REM-A-SCHED-FLIGHT                  
      ******************************************************************
       2620-CALL-REM-A-SCHED-FLIGHT.                                    
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           EXEC CICS                                                    
            XCTL PROGRAM(CT-REM-A-SCHEDULE) COMMAREA(WS-ZZEC0215)       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     3000-FINAL                                  
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              EXEC CICS                                                 
                  RETURN TRANSID('0226') COMMAREA(WS-ZZEC0215)          
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN SO-FINAL-TERMINATION                                    
              EXEC CICS                                                 
                 XCTL PROGRAM(CT-CALLIING-PROGRAM-NAME)                 
                    COMMAREA(WS-ZZEC0215) LENGTH(0)                     
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN OTHER                                                   
              DISPLAY 'MAPFAIL'                                         
              MOVE ' YOU NEED TO PROVIDE  DATA IN CHOICE FIELD '        
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
       ******************************************************************
      *                3001-TERMINATION-WITH-MSG                        
      ******************************************************************
       3001-TERMINATION-WITH-MSG.                                       
           EXEC CICS                                                    
           SEND TEXT                                                    
           FROM(CT-FINAL-MESSAGE)                                       
           ERASE                                                        
           END-EXEC                                                     
                                                                        
           EXEC CICS                                                    
           RETURN                                                       
           END-EXEC                                                     
           .
                                      
                                  
                               
                                                          
