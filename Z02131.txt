       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02131.                                              
      ******************************************************************
      *                                                                 
      *   MAIN PROGRAM IN AIRLINE SYSTEM                                
      *                                                                 
      *  THIS PROGRAM WILL DISPLAY IT'S MAP TO THE USER                 
      * ON THIS MAP USER WILL SE 4 MAIN OPTIONS OF THIS APPLICATION     
      *                                                                 
      *    1. RESERVATE A FLIGHT                                        
      *    2. FIND A FLIGHT                                             
      *    3. FIND A BOOKING                                            
      *    4. AND/REMOVE FLIGHTS                                        
      *                                                                 
      *    USER WILL HAVE TO CHOOSE 1 OF THIS 4 OPTIONS AND BASED ON    
      * HIS CHOICE CONTROL WILL BE PASSED TO OTHER PROGRAMS IN THIS     
      * APPLICATION                                                     
      *                                                                 
      *   IF USER WILL CHOOSE OPTION '1' THEN PROGRAM Z02152 WILL       
      * BE CALLED                                                       
      *                                                                 
      *   IF USER WILL CHOOSE OPTION '2' THEN PROGRAM Z02221 WILL       
      * BE CALLED                                                       
      *                                                                 
      *   IF USER WILL CHOOSE OPTION '3' THEN PROGRAM Z02252 WILL       
      * BE CALLED                                                       
      *                                                                 
      *   IF USER WILL CHOOSE OPTION '4' THEN PROGRAM Z02271 WILL       
      * BE CALLED                                                       
      *                                                                 
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
           COPY ZZMP0213.                                               
           COPY ZZEC0215.                                               
       01 WS-ITER     PIC S9(4) COMP VALUE 0.                           
      * COMMAREA                                                        
       01 WS-COMMAREA PIC X VALUE 'A'.                                  
       01 WS-EIBRESP-TEMP  PIC X(10) VALUE SPACE.                       
      * CONSTANTS                                                       
       01 CT-CONSTANTS.                                                 
           05 CT-ERROR-ROUTINE                PIC X(8) VALUE 'Z02141'.  
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02131'.  
           05 CT-RESERVATE-FLIGHT-PROG        PIC X(8) VALUE 'Z02152  '.
           05 CT-FIND-A-FLIGHT-PROG           PIC X(8) VALUE 'Z02221  '.
           05 CT-FIND-A-BOOKING-PROG          PIC X(8) VALUE 'Z02252  '.
           05 CT-FINAL-MESSAGE                PIC X(79)                 
                                                 VALUE 'END OF PROGRAM'.
           05 CT-FLIGHT-MANAGMENT-PROG        PIC X(8) VALUE 'Z02271  '.
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
      *  CHECKS IF TRANSACTION RUNS FOR THE FIRST TIME                  
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
      * PROGRAM MOVES LOGIC TO PARAGRAPH DEPENDING ON IF THIS IS        
      * THE FIRST TIME PROGRAM RUNS OR NOT                              
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
      * IF THIS IS THE FIRST TIME THE PROGRAMS RUNS, PROGRAM WILL       
      * DISPLAY EMPTY (CLEAR WITHOUT DATA) MAP                          
      ******************************************************************
       2001-PROCESS-IF-FIRST-TIME.                                      
           PERFORM 2050-SEND-CLEAN-MAP                                  
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           .                                                            
      ******************************************************************
      *               2002-PROCESS-IF-NOT-FIRST-TIME    
      * IF THIS IS NOT FIRST TIME PROGRAM WILL CHECK                    
      * WHAT KEY WAS PRESSED BY THE USER                                
      *                                                                 
      * WHEN 'F3' -> PROGRAM WILL END (TRANSACTION WILL BE TERMINATED)  
      * WHEN 'ENTER' -> PROGRAM WILL CHECK IF USER PROVIDED VALID       
      * CHOICE AND IF SO PROGRAM WILL CALL TO OTHER PROGRAMS            
      *                                                                 
      * IF ANY OTHER KEY WAS PRESSED THEN USER WILL GET MESSAGE         
      * INDICATING THAT THIS KEY HAS NO ACTION ASSIGNED                 
      *                                                                 
      ******************************************************************
       2002-PROCESS-IF-NOT-FIRST-TIME.                                  
           EVALUATE EIBAID                                              
      * WHEN USER PRESSED ENTER THEN PROGRAM WILL RECEIVE USER INPUT    
      * AND BASED ON THAT PROGRAM WILL MAKE FURTHER DECISTIONS          
             WHEN DFHENTER                                              
               PERFORM 2150-PROCESS-USER-CHOICE                         
      * IF USER PRESSED 'F3' IT MEANS HE WANTS TO TERMINATE THIS        
      * PROGRAM                                                         
             WHEN DFHPF3                                                
               SET SO-FINAL-TERMINATION TO TRUE                         
      * IF USER PRESSED ANY OTHER KEY THEN HE WILL GET PROPER MESSAGE   
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
           MOVE LOW-VALUES TO MPS1O                                     
           SET SO-SEND-WHOLE-MAP TO TRUE                                
           PERFORM 2100-SEND-THE-MAP                                    
           .                      
      ******************************************************************
      *                    2050-SEND-CLEAN-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
                                                                        
           EXEC CICS                                                    
            SEND MAP('MPS1') MAPSET('MPS1')                             
            FROM(MPS1O)                                                 
            ERASE                                                       
           END-EXEC                                                     
                                                                        
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2150-PROCESS-USER-CHOICE                       
      * PARAGRAPH WILL GET USER CHOICE AND BASED ON WHAT USER PROVIDED  
      * ACTIONS WILL BE MADE                                            
      *                                                                 
      *  OTHER PROGRAMS WILL BE CALLED OR HE WILL GET MEESSAGE SAYING   
      * THAT HIS CHOICE IS INVALID                                      
      ******************************************************************
       2150-PROCESS-USER-CHOICE.                                        
           PERFORM 2151-GET-USER-CHOICE                                 
           PERFORM 2152-EVALUATE-USER-CHOICE                            
           .                                                            
      ******************************************************************
      *                    2151-GET-USER-CHOICE                         
      ******************************************************************
       2151-GET-USER-CHOICE.                                            
           MOVE LOW-VALUES TO MPS1I                                     
                                                                        
           EXEC CICS                                                    
           RECEIVE MAP('MPS1') MAPSET('MPS1')                           
           INTO(MPS1I)                                                  
           NOHANDLE                                                     
           END-EXEC   
           PERFORM 2200-CHECK-EIBRESP                                   
           MOVE CHOICEI TO SW-USER-CHOICE                               
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      * PROGRAM WILL CALL TO OTHER PROGRAMS DEPENDING ON WHAT           
      * CHOCICE USER MADE.                                              
      ******************************************************************
       2152-EVALUATE-USER-CHOICE.                                       
           EVALUATE TRUE                                                
             WHEN SO-BOOK-A-FLIGHT                                      
                PERFORM 2510-CALL-TO-BOOK-FLIGHT                        
             WHEN SO-FIND-A-FLIGHT                                      
                PERFORM 2520-CALL-TO-FIND-FLIGHT                        
             WHEN SO-FIND-BOOKING                                       
                PERFORM 2530-CALL-TO-FIND-BOOKING                       
             WHEN SO-FLIGHT-MANAGMENT                                   
                PERFORM 2540-CALL-TO-FLIGHT-MANAGMENT                   
             WHEN OTHER                                                 
                MOVE ' YOU NEED TO PROVIDE VALID DATA IN CHOICE FIELD ' 
                TO WS-Z02141-I-ERROR-MESSAGE(1)                         
                PERFORM 2300-CALL-ERROR-ROUTINE                         
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      * PARAGRAPH WILL BE PERFORMED AFTER ANY CICS OPERATION WILL BE    
      * MADE, IT WILL CHECK IF RESULT OF THIS OPERATION IS GOOD OR NOT  
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
      * PARAGRAPH WILL CALL TO PROGRAM THAT WILL DISPLAY ERROR          
      * MESSAGE (AFTER PRESSING F3 IN THIS ERROR MESSAGE PROGRAM        
      * CONTROL WILL RETURN TO CALLING PROGRAM  )                       
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
      *                  2510-CALL-TO-BOOK-FLIGHT                       
      * THIS PROGRAM WILL BE CALLED IF USER CHOOSE '1' OPTION           
      ******************************************************************
       2510-CALL-TO-BOOK-FLIGHT.                                        
           SET SO-M-FIRST-WITHOUT TO TRUE    
           EXEC CICS                                                    
            XCTL PROGRAM(CT-RESERVATE-FLIGHT-PROG) COMMAREA(WS-ZZEC0215)
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *              2520-CALL-TO-FIND-FLIGHT                           
      * THIS PROGRAM WILL BE CALLED IF USER CHOOSE '2' OPTION           
      ******************************************************************
       2520-CALL-TO-FIND-FLIGHT.                                        
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-PROVIDE-ALL-DATA TO TRUE                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-FIND-A-FLIGHT-PROG) COMMAREA(WS-ZZEC0215)   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 2530-CALL-TO-FIND-BOOKING                       
      * THIS PROGRAM WILL BE CALLED IF USER CHOOSE '3' OPTION           
      ******************************************************************
       2530-CALL-TO-FIND-BOOKING.                                       
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           EXEC CICS                                                    
            XCTL PROGRAM(CT-FIND-A-BOOKING-PROG) COMMAREA(WS-ZZEC0215)  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2540-CALL-TO-FLIGHT-MANAGMENT                  
      * THIS PROGRAM WILL BE CALLED IF USER CHOOSE '4' OPTION           
      ******************************************************************
       2540-CALL-TO-FLIGHT-MANAGMENT.                                   
           EXEC CICS                                                    
            XCTL PROGRAM(CT-FLIGHT-MANAGMENT-PROG)                      
             COMMAREA(WS-ZZEC0215) LENGTH(0)  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     3000-FINAL                                  
      *                                                                 
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
      * THIS OPTION WILL ALLOW PROGRAM TO BE RETRRIGERED AFTER          
      * USER WILL PRESS ATTENTION KEY                                   
                                                                        
             WHEN SO-FINAL-WITH-COMMAREA                                
               PERFORM 3002-RETURN-WITH-TRANSID                         
                                                                        
      * THIS OPTION WILL TERMINATE TRANSACTION                          
             WHEN SO-FINAL-TERMINATION                                  
               PERFORM 3001-TERMINATION-WITH-MSG                        
             WHEN OTHER                                                 
              MOVE ' Z02113 PROGRAM ERROR       '                       
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *               3002-RETURN-WITH-TRANSID                          
      ******************************************************************
       3002-RETURN-WITH-TRANSID.                                        
           EXEC CICS                                                    
             RETURN TRANSID('0208') COMMAREA(WS-COMMAREA)               
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                3001-TERMINATION-WITH-MSG                        
      * THIS MESSAGE WILL BE SENT WHEN TRANSACTION WILL BE TERMINATED   
      ******************************************************************
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

                          
                           
                                   
                                                  
                                      
                
                                
      
