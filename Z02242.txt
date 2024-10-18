       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02242.                                              
      ******************************************************************
      *               Z02242 (0219)                                     
      *     PROGRAM WILL BE USED TO DISPLAY ALL PASSENGERS THAT         
      *   WILL BE FLYING IN CERTAIN FLIGHT                              
      *                                                                 
      *    ( OR TO DISPLAY ALL PASSENGERS IN GIVEN FLIGHT WITH          
      *    GIVEN RESERVATION_ID)                                        
      *                                                                 
      * USER CAN USER PAGING LOGIC  BY PRESSING F7 & F8 KEYS            
      * OR GO BACK TO CALLING PROGRAM BY PRESSING F3                    
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0217.                                               
           COPY ZZEC0215.                                               
           COPY ZZMP0224.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL INCLUDE T06TAB END-EXEC.                            
           EXEC SQL INCLUDE T12TAB END-EXEC.                            
      * CURSOR WILL CREATE RESULT SET WITH ALL PASSENGERS DATA          
      * FOR A GIVEN FLIGHT                                              
           EXEC SQL                                                     
            DECLARE C-ALL-PASSENGERS-CURSOR CURSOR                      
            FOR                                                         
            SELECT                                                      
                  F1.ROW_NUMBER,                                        
                  F1.SEAT_LETTER,                                       
                  F2.PASSENGER_NAME,                                    
                  F2.PASSENGER_LAST_NAME,                               
                  F2.NATIONALITY,                                       
                  F2.IDENTIFICATION_NUMBER                              
            FROM                                                        
                 T04_FLIGHT_SEATS F1                                    
            INNER JOIN                                                  
                 T06_PASSENGERS_TABLE F2  ON                            
                 F2.PASSENGER_ID = F1.PASSENGER_ID                      
            WHERE F1.FLIGHT_ID = :T04-FLIGHT-ID                         
                                                                        
            FOR FETCH ONLY                                              
           END-EXEC                                                     
      * CURSOR WILL CREATE RESULT SET WITH ALL PASSENGERS DATA          
      * FOR A GIVEN FLIGHT IN A GIVEN RESERVATION                       
           EXEC SQL                                                     
            DECLARE C-RESERVATION-PASSENGERS CURSOR                     
            FOR                                                         
            SELECT                                                      
                  F1.ROW_NUMBER,                                        
                  F1.SEAT_LETTER,                                       
                  F2.PASSENGER_NAME,                                    
                  F2.PASSENGER_LAST_NAME,                               
                  F2.NATIONALITY,                                       
                  F2.IDENTIFICATION_NUMBER                              
            FROM                                                        
                 T04_FLIGHT_SEATS F1                                    
            INNER JOIN                                                  
                 T06_PASSENGERS_TABLE F2  ON                            
                 F2.PASSENGER_ID = F1.PASSENGER_ID                      
            WHERE F1.FLIGHT_ID = :T04-FLIGHT-ID  AND                    
                  F1.RESERVATION_ID = :T04-RESERVATION-ID               
            FOR FETCH ONLY                                              
           END-EXEC                                                     
       01 WS-DB2-ERROR.                                                 
               10 SW-SQLCODE                    PIC S9(5).              
                   88 SO-SQLCODE-OK             VALUE  000   100.       
                   88 SO-SQLCODE-NORMAL         VALUE 000.              
                   88 SO-SQLCODE-NOT-FOUND      VALUE 100.              
               10 WS-SQLERRMC                   PIC X(70).              
               10 WS-SQLCODE-FORMAT             PIC -(5).       
               10 SW-STATEMENT-ID               PIC X(4).               
                   88 SO-7001-PARA              VALUE '7001'.           
                   88 SO-7002-PARA              VALUE '7002'.           
                   88 SO-7003-PARA              VALUE '7003'.           
                   88 SO-7004-PARA              VALUE '7004'.           
                   88 SO-7005-PARA              VALUE '7005'.           
                   88 SO-7006-PARA              VALUE '7006'.           
                   88 SO-7007-PARA              VALUE '7007'.           
                   88 SO-7008-PARA              VALUE '7008'.           
                   88 SO-7009-PARA              VALUE '7009'.           
                   88 SO-7010-PARA              VALUE '7010'.           
                   88 SO-7011-PARA              VALUE '7011'.           
                   88 SO-7012-PARA              VALUE '7012'.           
                   88 SO-7013-PARA              VALUE '7013'.           
                   88 SO-7014-PARA              VALUE '7014'.           
                   88 SO-7015-PARA              VALUE '7015'.           
       01 WS-QUEUE-STRUCTURE.                                           
           05 QUEUE-ROW-NUMBER                  PIC 9(3).               
           05 QUEUE-SEAT-LETTER                 PIC X.                  
           05 QUEUE-PASSENGER-NAME              PIC X(50).              
           05 QUEUE-PASSENGER-LAST-NAME         PIC X(50).              
           05 QUEUE-NATIONALITY                 PIC X(3).               
           05 QUEUE-ID-NUMBER                   PIC X(12).              
       01 CT-CONSTANTS.                                                 
           05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02232  '.        
           05 CT-CALLING-PROGRAM-NAME2 PIC X(8) VALUE 'Z02261  '.       
           05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02242  '.        
           05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.        
           05 CT-PASSENGER-QUEUE      PIC X(8) VALUE '02X8    '.        
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME              PIC X.         
              88  SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'.  
              88  SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'.  
              88  SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'.  
           05 SW-WHAT-TYPE-OF-END                           PIC X.      
              88 SO-FINAL-WITH-COMMAREA                     VALUE '1'.  
              88 SO-FINAL-TERMINATION                       VALUE '2'.  
           05 SW-IF-END-OF-CURSOR                           PIC X.      
              88 SO-NOT-END-OF-CURSOR-DATA                  VALUE '1'.  
              88 SO-END-OF-CURSOR-DATA                      VALUE '2'.  
           05 SW-IF-END-OF-QUEUE                            PIC X.      
              88 SO-END-OF-QUEUE                            VALUE '1'.  
              88 SO-NOT-END-OF-QUEUE                        VALUE '2'.  
           05 SW-IF-GO-TO-PREVIOUS                          PIC X.      
              88 SO-GO-BACK-TO-PREVIOUS                     VALUE '1'.  
              88 SO-DONT-GO-BACK-TO-PREVIOUS                VALUE '2'.  
       01 WS-VARIABLES.                                                 
           05 WS-WHAT-RECORD-TO-READ             PIC S9(4) COMP VALUE 0.
           05 CT-EMPTY-FIELD         PIC X(15) VALUE 'XXXXXXXXXXXXXXX'. 
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER10                          PIC S9(4) COMP VALUE 0.
           05 WS-TEMP-STRING                     PIC X(15).             
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02242-----------START----------'                   
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02242-----------END------------'                   
           PERFORM 3000-FINAL                                           
           .                                                            
      ***************************************************************** 
      *                          1000-INIT                              
      ***************************************************************** 
       1000-INIT.                                                       
           PERFORM  1005-CHECK-IF-FIRST-TIME                            
           .                                                            
      *****************************************************************
      *                 1005-CHECK-IF-FIRST-TIME                        
      * PROGRAM CAN HAVE 3 MODES:                                       
      * 1.   SO-M-FIRST-WITHOUT HAPPENS WHEN PROGRAM RUNNING FOR THE    
      * FIRST TIME (THERE IS NO DATA THAT WERE PREVIOUSLY GENERATED     
      * BY THIS PROGRAM)                                                
      * 2. SO-M-FIRST-WITH -> PROGRAM RUNS FOR THE FIRST TIME           
      * BUT ERLIER THIS PROGRAM GENERATED SOME DATA                     
      * SO THIS TIME WE DON'T HAVE TO GENERATE THEM ONCE AGAIN,         
      * WE JUST HAVE TO DISPLAY IT                                      
      * 3. SO-M-NOT-FIRST -> PROGRAM STARTED BECAUSE USER PRESSED       
      * ATTENTION KEY                                                   
      *                                                                 
      ***************************************************************** 
       1005-CHECK-IF-FIRST-TIME.                                        
           INITIALIZE WS-ZZEC0215                                       
                                                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
                                                                        
                                                                        
           EVALUATE TRUE                                                
             WHEN SO-M-FIRST-WITHOUT                                    
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1020-DELETE-QUEUE                                
               SET SO-M-NOT-FIRST TO TRUE                               
               SET SO-PROGRAM-RUNS-FIRST-TIME TO TRUE                   
                                                                        
             WHEN SO-M-FIRST-WITH                                       
               SET SO-PROGRAM-RUNS-WITH-DATA  TO TRUE                   
               SET SO-M-NOT-FIRST TO TRUE                               
                                                                        
             WHEN SO-M-NOT-FIRST                                        
               SET SO-PROGRAM-RUNS-NOT-FIRST-TIME  TO TRUE              
             WHEN OTHER                                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID CALL' TO WS-Z02141-I-ERROR-MESSAGE(1)      
               SET SO-Z02141-M-WITH TO TRUE                             
                PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     1010-CICS-IGNORE                            
      ******************************************************************
       1010-CICS-IGNORE.                                                
           EXEC CICS                                                    
             IGNORE CONDITION ERROR                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                       1020-DELETE-QUEUE                         
      ******************************************************************
       1020-DELETE-QUEUE.                                               
           EXEC CICS                                                    
            DELETEQ TS                                                  
            QUEUE(CT-PASSENGER-QUEUE)                                   
            NOHANDLE                                                    
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) THEN                            
             CONTINUE                                                   
           ELSE                                                         
             PERFORM 2200-CHECK-EIBRESP                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                          2000-PROCESS                           
      ******************************************************************
       2000-PROCESS.                                                    
           EVALUATE TRUE                                                
           WHEN SO-PROGRAM-RUNS-FIRST-TIME                              
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2001-PROCESS-FIRST-TIME                          
           WHEN SO-PROGRAM-RUNS-WITH-DATA              
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2002-PROCESS-WITH-DATA                           
           WHEN SO-PROGRAM-RUNS-NOT-FIRST-TIME                          
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2003-PROCESS-NOT-FIRST-TIME                      
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'SERIOUS ERROR IN Z02242' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  2001-PROCESS-FIRST-TIME                        
      ****************************************************************  
       2001-PROCESS-FIRST-TIME.                                         
           PERFORM 2101-SEARCH-PASSENGERS                               
           MOVE 1 TO Z02182-Q1-LAST-REC-ID                              
           PERFORM 2115-DISPLAY-NEXT-15                                 
           .                                                            
      ****************************************************************  
      *                     2200-CHECK-EIBRESP                          
      ****************************************************************  
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2115-DISPLAY-NEXT-15                                 
           .                                                            
      ****************************************************************  
      *                2003-PROCESS-NOT-FIRST-TIME                      
      * HERE PROGRAM WILL CHECK WHAT ATTENTION KEY WAS PRESSED          
      * BY THE USER                                                     
      *                                                                 
      * IF IT WAS 'F7' THEN PROGRAM WILL BROWSE DISPLAYED DATA          
      * BACKWARDS                                                       
      *                                                                 
      * IF IT WAS 'F8' THEN PROGRAM WILL BROWS DISPLAYED DATA         
      * UPWARDS                                                         
      *                                                                 
      * IF IT WAS 'F3' THEN PROGRAM WILL RETURN CONTROL TO CALLING      
      * PROGRAM                                                         
      * IF IT WAS ANY OTHER KEY THEN USER WILL GET PROPER MESSAGE       
      * INDICATING THAT THIS KEY DON'T HAVE ANY ACTION ASSIGNED         
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHPF7                                                  
              PERFORM 2051-DISPLAY-PREV-15                              
           WHEN DFHPF8                                                  
              PERFORM 2115-DISPLAY-NEXT-15                              
           WHEN DFHPF3                                                  
              SET SO-FINAL-TERMINATION TO TRUE                          
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'NO ACTION KEY          ' TO                         
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *               2023-MOVE-PASSENGERS-TO-SCREEN                    
      ******************************************************************
       2023-MOVE-PASSENGERS-TO-SCREEN.                                  
           MOVE QUEUE-ROW-NUMBER            TO ROWNO(WS-ITER3)          
           MOVE QUEUE-SEAT-LETTER           TO SEATO(WS-ITER3)          
           MOVE QUEUE-PASSENGER-NAME        TO PASNO(WS-ITER3)          
           MOVE QUEUE-PASSENGER-LAST-NAME   TO PASLO(WS-ITER3)          
           MOVE QUEUE-NATIONALITY           TO NATO(WS-ITER3)           
           MOVE QUEUE-ID-NUMBER             TO IDNO(WS-ITER3)           
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
                                                OR   SO-END-OF-QUEUE    
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO Z02182-Q1-FIRST-REC-ID    
             END-IF                                                     
                                                                        
               PERFORM 2023-MOVE-PASSENGERS-TO-SCREEN                   
               MOVE WS-WHAT-RECORD-TO-READ TO Z02182-Q1-LAST-REC-ID     
               ADD 1 TO WS-WHAT-RECORD-TO-READ                          
               PERFORM 2116-READ-THE-QUEUE                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2051-DISPLAY-PREV-15                           
      ******************************************************************
       2051-DISPLAY-PREV-15.                                            
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2119-INITIALIZE-MAP                                  
                                                                        
            IF Z02182-Q1-FIRST-REC-ID - 15  >= 1 THEN                   
              SUBTRACT 15 FROM Z02182-Q1-FIRST-REC-ID                   
            ELSE                                                        
              MOVE 1 TO Z02182-Q1-FIRST-REC-ID                          
            END-IF                                                      
           MOVE Z02182-Q1-FIRST-REC-ID TO WS-WHAT-RECORD-TO-READ        
           PERFORM 2120-INITIALIZE-WHAT-F-NUM                           
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2117-CHECK-IF-QIDERR                                 
                                                                        
           PERFORM 2053-MOVE-QUEUE-TO-SCRREN                            
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                      2100-SEND-THE-MAP                          
      **************************************************************** 
       2100-SEND-THE-MAP.                                              
           EXEC CICS                                                   
             SEND MAP('MP0224') MAPSET('MP0224')                       
             FROM(MP0224O)                                             
             ERASE                                                     
           END-EXEC                                                    
           PERFORM 2200-CHECK-EIBRESP                                  
           .                                                           
      **************************************************************** 
      *                  2101-SEARCH-PASSENGERS                        
      * DEPEDING OF SCENARIO WE ARE IN PARAGRAPH WILL                  
      * GET ALL PASSENGERS IN A GIVEN FLIGHT                           
      *  OR                                                            
      * GET ALL PASSENGERS IN A GIVEN FLIGHT IN A GIVEN RESERVATION    
      **************************************************************** 
       2101-SEARCH-PASSENGERS.                                         
           IF SO-DISPLAY-ALL-PASSENGERS THEN                           
             PERFORM 2301-PREPARE-FLIGHT-ID                            
             PERFORM 7001-OPEN-ALL-DATA-CURSOR                         
             PERFORM 7002-FETCH-CURSOR-TO-QUEUE                        
             PERFORM 7003-CLOSE-ALL-DATA-CURSOR                        
           ELSE                                                        
      * IF PROGRAM HAVE TO DISPLAY ONLY PASSENGER THAT ARE             
      * IN GIVEN RESERVATION                                           
             PERFORM 2302-VALIDATE-RESERVATION-ID                      
             PERFORM 2301-PREPARE-FLIGHT-ID                            
             PERFORM 7005-OPEN-RESERV-CURSOR                           
             PERFORM 7006-FETCH-RESERV-TO-QUEUE                        
             PERFORM 7007-CLOSE-RESERV-CURSOR                          
           END-IF                                                      
           .                                                           
      **************************************************************** 
      *                   2111-MOVE-DATA-TO-QUEUE                      
      **************************************************************** 
       2111-MOVE-DATA-TO-QUEUE.   
           COMPUTE QUEUE-ROW-NUMBER = T04-ROW-NUMBER                    
           MOVE T04-SEAT-LETTER          TO QUEUE-SEAT-LETTER           
           MOVE PASSENGER-NAME-TEXT      TO QUEUE-PASSENGER-NAME        
           MOVE PASSENGER-LAST-NAME-TEXT TO QUEUE-PASSENGER-LAST-NAME   
           MOVE NATIONALITY              TO QUEUE-NATIONALITY           
           MOVE IDENTIFICATION-NUMBER    TO QUEUE-ID-NUMBER             
           .                                                            
      ****************************************************************  
      *                   2112-WRITE-THE-QUEUE                          
      ****************************************************************  
       2112-WRITE-THE-QUEUE.                                            
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-PASSENGER-QUEUE)                                  
             FROM(WS-QUEUE-STRUCTURE)                                   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2115-DISPLAY-NEXT-15                         
      ******************************************************************
       2115-DISPLAY-NEXT-15.                                            
           DISPLAY '2115 LEST REC ID: ' Z02182-Q1-LAST-REC-ID           
                                                                        
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2119-INITIALIZE-MAP                                  
           MOVE Z02182-Q1-LAST-REC-ID   TO WS-WHAT-RECORD-TO-READ       
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2117-CHECK-IF-QIDERR                                 
           PERFORM 2053-MOVE-QUEUE-TO-SCRREN                            
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                      2116-READ-THE-QUEUE                        
      ****************************************************************  
       2116-READ-THE-QUEUE.     
           EXEC CICS                                                    
           READQ TS                                                     
             QUEUE(CT-PASSENGER-QUEUE)                                  
             INTO(WS-QUEUE-STRUCTURE)                                   
             ITEM(WS-WHAT-RECORD-TO-READ)                               
             NOHANDLE                                                   
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(ITEMERR)                                        
              SET SO-END-OF-QUEUE TO TRUE                               
           WHEN DFHRESP(QIDERR)                                         
              CONTINUE                                                  
           WHEN OTHER                                                   
              PERFORM 2200-CHECK-EIBRESP                                
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                     2117-CHECK-IF-QIDERR                        
      ****************************************************************  
       2117-CHECK-IF-QIDERR.                                            
           IF EIBRESP = DFHRESP(QIDERR) THEN                            
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'THERE IS NO PASSENGERS ' TO                         
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
              SET SO-Z02141-M-WITH TO TRUE                              
              SET SO-GO-BACK-TO-PREVIOUS  TO TRUE                       
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2119-INITIALIZE-MAP                         
      ******************************************************************
       2119-INITIALIZE-MAP.                                             
           MOVE LOW-VALUES TO MP0224O   
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
             MOVE LOW-VALUES TO ROWNA(WS-ITER3)                         
             MOVE LOW-VALUES TO SEATA(WS-ITER3)                         
             MOVE LOW-VALUES TO PASNA(WS-ITER3)                         
             MOVE LOW-VALUES TO PASLA(WS-ITER3)                         
             MOVE LOW-VALUES TO NATA(WS-ITER3)                          
             MOVE LOW-VALUES TO IDNA(WS-ITER3)                          
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *               2120-INITIALIZE-WHAT-F-NUM                        
      ****************************************************************  
       2120-INITIALIZE-WHAT-F-NUM.                                      
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
              MOVE CT-EMPTY-FIELD   TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3)  
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                     2200-CHECK-EIBRESP                          
      ****************************************************************  
       2200-CHECK-EIBRESP.                                              
                                                                        
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
              MOVE 'YOU NEED TO MAKE A CHOICE ' TO                      
                         WS-Z02141-I-ERROR-MESSAGE(1)                   
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN DFHRESP(QIDERR)                                         
              MOVE 'QIDERR ' TO                                         
                         WS-Z02141-I-ERROR-MESSAGE(1)                   
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE  
           WHEN DFHRESP(ITEMERR)                                        
              MOVE 'ITEMERR' TO                                         
                         WS-Z02141-I-ERROR-MESSAGE(1)                   
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
              DISPLAY 'OTHER ERROR'                                     
              MOVE 'OTHER EIBRESP ERROR ' TO                            
                         WS-Z02141-I-ERROR-MESSAGE(1)                   
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2300-CALL-ERROR-ROUTINE.                    
      * ERROR ROUTINE IS A PROGRAM THAT WILL ALLOW USER TO SEE          
      * ANY ERRROR THAT OCCURED WHILE USING THIS APPLICATION            
      *                                                                 
      * AFTER PRESSING F3 IN Z02141 PROGRAM (ERROR ROTUINE) CONTROL     
      * WILL BE RETURNED TO CALLING PROGRAM (THIS ONE)                  
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
                                                                        
           MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM     
           IF SO-GO-BACK-TO-PREVIOUS THEN                               
             MOVE CT-CALLING-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM
           END-IF                                                       
           SET SO-Z02141-M-WITH TO TRUE                                 
           SET  SO-Z02141-I-FIRST-TIME TO TRUE                          
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                     2301-PREPARE-FLIGHT-ID                      
      ******************************************************************
       2301-PREPARE-FLIGHT-ID.                                          
           MOVE SPACES               TO T04-FLIGHT-ID-TEXT              
           MOVE Z02242-FLIGHT-NUMBER TO T04-FLIGHT-ID-TEXT              
           MOVE FUNCTION REVERSE(Z02242-FLIGHT-NUMBER) TO               
                        WS-TEMP-STRING                                  
           INSPECT WS-TEMP-STRING TALLYING WS-ITER10 FOR LEADING X'00'  
           COMPUTE T04-FLIGHT-ID-LEN = LENGTH OF T04-FLIGHT-ID-TEXT -   
                                      WS-ITER10                         
           .                                                            
      ******************************************************************
      *                2302-VALIDATE-RESERVATION-ID                     
      * PARAGRAPH WILL CHECK IF GIVEN RESERVATION ID IS A VALID         
      * NUMBER                                                          
      ******************************************************************
       2302-VALIDATE-RESERVATION-ID.                                    
           DISPLAY '2101 RESERVATION: ' Z02242-I-RESERVATION-ID         
           IF FUNCTION  TEST-NUMVAL(Z02242-I-RESERVATION-ID) = 0 THEN   
              COMPUTE T04-RESERVATION-ID =                              
                              FUNCTION NUMVAL(Z02242-I-RESERVATION-ID)  
           ELSE                                                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)    
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2400-INITIALIZE-ERROR-MESSAGE                 
      ******************************************************************
       2400-INITIALIZE-ERROR-MESSAGE.                                   
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 10     
             MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER2)          
           END-PERFORM                                                  
           .        
      ******************************************************************
      *                          3000-FINAL                             
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3001-RETURN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
                                                                        
              DISPLAY 'WHEN SO-FINAL-TERMINATION '                      
              IF SO-DISPLAY-ALL-PASSENGERS THEN                         
                PERFORM 3002-RETURN-TO-Z02232                           
               ELSE                                                     
                PERFORM 3003-RETURN-TO-Z02261                           
               END-IF                                                   
           WHEN OTHER                                                   
              PERFORM 3004-SEND-SERIOUS-ERROR-MSG                       
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       3001-RETURN-WITH-TRANSID                  
      ******************************************************************
       3001-RETURN-WITH-TRANSID.                                        
           SET SO-M-NOT-FIRST TO TRUE                                   
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           DISPLAY 'RETURN WITH 0219'                                   
           EXEC CICS                                                    
            RETURN TRANSID('0219') COMMAREA(DFHCOMMAREA)                
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                        3002-RETURN-TO-Z02232                    
      * Z02232  -> PROGRAM DISPLAYES FLIGHTS THAT USER SEARCH FOR       
      ******************************************************************
       3002-RETURN-TO-Z02232.   
           SET SO-M-FIRST-WITH  TO TRUE                                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                      
              COMMAREA(DFHCOMMAREA)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                        3003-RETURN-TO-Z02261                    
      * Z02261 -> PROGRAM DISPLAYES FLIGHTS IN A GIVEN RESERVATION      
      ****************************************************************  
       3003-RETURN-TO-Z02261.                                           
           SET SO-M-FIRST-WITH  TO TRUE                                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-CALLING-PROGRAM-NAME2)                     
              COMMAREA(DFHCOMMAREA)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                   3004-SEND-SERIOUS-ERROR-MSG                   
      ****************************************************************  
       3004-SEND-SERIOUS-ERROR-MSG.                                     
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)       
           SET    SO-Z02141-M-WITH TO TRUE                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ****************************************************************  
      *                      7001-OPEN-ALL-DATA-CURSOR                  
      ****************************************************************  
       7001-OPEN-ALL-DATA-CURSOR.                                       
           DISPLAY 'ZARAZ PRZED OPENEM FLIGHT ID: ' T04-FLIGHT-ID-TEXT  
           DISPLAY 'A LENGTH : ' T04-FLIGHT-ID-LEN             
           EXEC SQL                                                     
             OPEN C-ALL-PASSENGERS-CURSOR                               
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN  SO-SQLCODE-NORMAL                                      
             CONTINUE                                                   
           WHEN OTHER                                                   
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  7002-FETCH-CURSOR-TO-QUEUE                     
      ****************************************************************  
       7002-FETCH-CURSOR-TO-QUEUE.                                      
                                                                        
           MOVE LOW-VALUES TO WS-QUEUE-STRUCTURE                        
                                                                        
             PERFORM 7004-FETCH-ALL-PASSENGERS                          
             PERFORM UNTIL SO-END-OF-CURSOR-DATA                        
               PERFORM 2111-MOVE-DATA-TO-QUEUE                          
               PERFORM 2112-WRITE-THE-QUEUE                             
               PERFORM 7004-FETCH-ALL-PASSENGERS                        
             END-PERFORM                                                
           .                                                            
      ****************************************************************  
      *                      7003-CLOSE-ALL-DATA-CURSOR                 
      ****************************************************************  
       7003-CLOSE-ALL-DATA-CURSOR.                                      
           EXEC SQL                                                     
             CLOSE C-ALL-PASSENGERS-CURSOR                              
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN  SO-SQLCODE-NORMAL   
             CONTINUE                                                   
           WHEN OTHER                                                   
              SET SO-7003-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                     7004-FETCH-ALL-PASSENGERS                   
      * THIS PARAGRAPH WILL FETCH ALL PASSENGERS THAT HAVE RESERVATION  
      * IN A GIVEN FLIGHT (NOT IN A GIVEN RESERVATION)                  
      ****************************************************************  
       7004-FETCH-ALL-PASSENGERS.                                       
           SET SO-NOT-END-OF-CURSOR-DATA  TO TRUE                       
           INITIALIZE T04-ROW-NUMBER                                    
           INITIALIZE T04-SEAT-LETTER                                   
           INITIALIZE PASSENGER-NAME                                    
           INITIALIZE PASSENGER-LAST-NAME                               
           INITIALIZE NATIONALITY                                       
           INITIALIZE IDENTIFICATION-NUMBER                             
                                                                        
           EXEC SQL                                                     
            FETCH C-ALL-PASSENGERS-CURSOR INTO                          
              :T04-ROW-NUMBER,                                          
              :T04-SEAT-LETTER,                                         
              :PASSENGER-NAME,                                          
              :PASSENGER-LAST-NAME,                                     
              :NATIONALITY,                                             
              :IDENTIFICATION-NUMBER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE PO FETCH TO: ' WS-SQLCODE-FORMAT            
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
      *       CONTINUE                                                  
              DISPLAY 'T04-ROW-NUMBER: ' T04-ROW-NUMBER                 
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7004-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7005-OPEN-RESERV-CURSOR                     
      * THIS CURSOR WILL STORE PASSENGERS DETAILS                       
      * THOSE PASSENGERS ARE THE PART OF A GIVEN RESERVATION            
      ******************************************************************
       7005-OPEN-RESERV-CURSOR.                                         
           EXEC SQL                                                     
             OPEN C-RESERVATION-PASSENGERS                              
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN  SO-SQLCODE-NORMAL                                      
             CONTINUE                                                   
           WHEN OTHER                                                   
              SET SO-7005-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  7006-FETCH-RESERV-TO-QUEUE                     
      * THIS PARAGRAPH WILL FETCH ALL PASSENGERS THAT ARE A PART OF     
      * A GIVEN RESERVATION TO THE QUEUE                                
      ****************************************************************  
       7006-FETCH-RESERV-TO-QUEUE.                                      
                                                                        
           MOVE LOW-VALUES TO WS-QUEUE-STRUCTURE                        
                                                                        
             PERFORM 7008-FETCH-RESERV-PASSENGERS                       
             PERFORM UNTIL SO-END-OF-CURSOR-DATA 
               PERFORM 2111-MOVE-DATA-TO-QUEUE                          
               PERFORM 2112-WRITE-THE-QUEUE                             
               PERFORM 7008-FETCH-RESERV-PASSENGERS                     
             END-PERFORM                                                
           .                                                            
      ****************************************************************  
      *                      7007-CLOSE-RESERV-CURSOR                   
      ****************************************************************  
       7007-CLOSE-RESERV-CURSOR.                                        
           EXEC SQL                                                     
             CLOSE C-RESERVATION-PASSENGERS                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN  SO-SQLCODE-NORMAL                                      
             CONTINUE                                                   
           WHEN OTHER                                                   
              SET SO-7007-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7008-FETCH-RESERV-PASSENGERS                  
      * PARAGRAPH WILL FETCH PASSENGERS THAT ARE IN A GIVEN RESERVATION 
      ******************************************************************
       7008-FETCH-RESERV-PASSENGERS.                                    
           SET SO-NOT-END-OF-CURSOR-DATA  TO TRUE                       
           INITIALIZE T04-ROW-NUMBER                                    
           INITIALIZE T04-SEAT-LETTER                                   
           INITIALIZE PASSENGER-NAME                                    
           INITIALIZE PASSENGER-LAST-NAME                               
           INITIALIZE NATIONALITY                                       
           INITIALIZE IDENTIFICATION-NUMBER                             
                                                                        
           EXEC SQL                                                     
            FETCH C-RESERVATION-PASSENGERS INTO 
              :T04-ROW-NUMBER,                                          
              :T04-SEAT-LETTER,                                         
              :PASSENGER-NAME,                                          
              :PASSENGER-LAST-NAME,                                     
              :NATIONALITY,                                             
              :IDENTIFICATION-NUMBER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE PO FETCH TO: ' WS-SQLCODE-FORMAT            
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
      *       CONTINUE                                                  
              DISPLAY 'T04-ROW-NUMBER: ' T04-ROW-NUMBER                 
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7004-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       9000-DB2-ERROR                            
      ******************************************************************
       9000-DB2-ERROR.                                                  
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           MOVE SQLERRMC TO WS-SQLERRMC                                 
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
                                                                        
           MOVE 'DB2 ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)           
                                                                        
           STRING 'IN SATATEMENT: ' SW-STATEMENT-ID                     
             DELIMITED BY SIZE                                          
             INTO WS-Z02141-I-ERROR-MESSAGE(2)                          
           END-STRING      
                                                                        
           STRING 'SQLCODE: ' WS-SQLCODE-FORMAT                         
             DELIMITED BY SIZE                                          
             INTO WS-Z02141-I-ERROR-MESSAGE(3)                          
           END-STRING                                                   
                                                                        
           STRING 'SQLERRMC: ' WS-SQLERRMC                              
             DELIMITED BY SIZE                                          
             INTO WS-Z02141-I-ERROR-MESSAGE(4)                          
           END-STRING                                                   
                                                                        
           MOVE  'ROLLBACK PERFORMED '                                  
             TO   WS-Z02141-I-ERROR-MESSAGE(5)                          
           PERFORM 9100-ROLLBACK                                        
                                                                        
           SET SO-Z02141-M-WITH  TO TRUE                                
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                       9100-ROLLBACK                             
      ******************************************************************
       9100-ROLLBACK.                                                   
           EXEC CICS                                                    
             SYNCPOINT ROLLBACK                                         
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP.                                  
           .                                                                                                         
                                                                        
                        
                       
                                   
         
                                        
                                                    

     
                    
                                
                                        
                                     
                      
  
               

        
