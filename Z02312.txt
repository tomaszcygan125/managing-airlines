       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02312.                                              
      ******************************************************************
      *               Z02312 (0225)                                     
      *                                                                 
      * PROGRAM WILL DISPLAY ALL SCHEDULED FLIGHTS                      
      *                                                                 
      * USER WILL BE ABLE TO PLACE "X" SYMBOL NEXT TO SCHEDULE NAME     
      * AFTER PRESSING ENTER THIS SCHEDULE WILL BE DELETED AND USER     
      * WILL GET PROPER MESSAGE                                         
      * USER CAN ALSO BROWSE DATA BY USING F7 AND F8 KEYS               
      * AND PRESS F3 TO GOBACK TO CALLING PROGRAM                       
      *                                                                 
      *                                                                 
      * USER WILL BE ALSO ABLE TO PLACE LETTER 'D' NEXT TO THE          
      * SCHEDULE NAME AND AFTER PRESSING ENTER PROGRAM (Z0212) WILL     
      * BE CALLED ( THIS PROGRAM WILL DISPLAY DETAILS ABOUT THIS        
      * SCHDEDULE) AFTER PRESSING F3 IN THAT PROGRAM CONTROL WILL       
      * BE RETURNED HERE                                                
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0231.                                               
           COPY ZZEC0215.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T10TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
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
       01 CT-CONSTANTS.                                                 
           05 CT-CALLING-PROGRAM-NAME         PIC X(8) VALUE 'Z02321  '.
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02312  '.
           05 CT-ERROR-ROUTINE-NAME           PIC X(8) VALUE 'Z02141  '.
           05 CT-SCHEDULE-QUEUE               PIC X(8) VALUE '02X15   '.
           05 CT-DISPLAY-PASS-PROG            PIC X(8) VALUE 'Z02242  '.
           05 CT-DISPLAY-SEATS-PROG           PIC X(8) VALUE 'Z02192  '.
           05 CT-DELETED-STATUS.                                        
              49 CT-DELETED-STATUS-LEN        PIC S9(4) COMP VALUE 7.   
              49 CT-DELETED-STATUS-TEXT       PIC X(15) VALUE 'DELETED'.
           05 CT-EMPTY-FIELD                  PIC S9(9) COMP VALUE 0.   
           05 CT-MAXIMAL-AMOUNT-OF-ROWS       PIC S9(4) COMP VALUE 10.  
           05 CT-SCHEDULE-DETAILS             PIC X(8) VALUE 'Z02212  '.
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
           05 SW-IF-VALID-DATE                              PIC X.      
              88 SO-INVALID-DATE                            VALUE '1'.  
              88 SO-VALID-DATE                              VALUE '2'.  
           05 SW-ARRIVAL-SEARCH                             PIC X.      
              88 SO-NOT-ARRIVAL-DATE-SEARCH                 VALUE '1'.  
              88 SO-ARRIVAL-DATE-SEARCH                     VALUE '2'.  
           05 SW-DEPARTURE-SEARCH                           PIC X.      
              88 SO-NOT-DEP-DATE-SEARCH                     VALUE '1'.  
              88 SO-DEP-DATE-SEARCH                         VALUE '2'.  
           05 SW-IF-RECORD-FOUND                            PIC X.      
              88 SO-RECORD-FOUND                            VALUE '1'.  
              88 SO-RECORD-NOT-FOUND                        VALUE '2'.  
           05 SW-USER-CHOICE                                PIC X.      
              88 SO-DISPLAY-SEATS                           VALUE '1'.  
              88 SO-DISPLAY-PASSENGERS-DATA                 VALUE '2'.  
              88 SO-VALID-CHOICE                            VALUE 'X'.  
              88 SO-DISPLAY-SCHEDULE-DETAILS                VALUE 'D'.  
       01 WS-VARIABLES.                                                 
           05 SCHEDULED-FLIGHT-ID-FORMAT          PIC S9(9) VALUE 0.    
           05 WS-USER-CHOICE-POSITION             PIC S9(4) COMP.       
           05 WS-CHOICE-COUNTER                   PIC S9(4) COMP.       
           05 WS-MODIFIED-TIMESTAMP               PIC X(26).            
           05 WS-HOUR-OFFSET               PIC S9(4) COMP VALUE 0.      
           05 WS-MINUTE-OFFSET             PIC S9(4) COMP VALUE 0.      
           05 WS-HOUR-OFFSET-TEMP          PIC X(10) VALUE SPACE.       
           05 WS-MINUTE-OFFSET-TEMP        PIC X(10) VALUE SPACE.       
           05 WS-STATUS1.                                               
              49 WS-STATUS1-LEN                   PIC S9(4) COMP.       
              49 WS-STATUS1-TEXT                  PIC X(15).            
           05 WS-STATUS2.                                               
              49 WS-STATUS2-LEN                   PIC S9(4) COMP.       
              49 WS-STATUS2-TEXT                  PIC X(15).            
           05 WS-STATUS3.                                               
              49 WS-STATUS3-LEN                   PIC S9(4) COMP.       
              49 WS-STATUS3-TEXT                  PIC X(15).            
           05 WS-STATUS4.                                               
              49 WS-STATUS4-LEN                   PIC S9(4) COMP.       
              49 WS-STATUS4-TEXT                  PIC X(15).            
           05 WS-DEST-AIRPORT-LOW                          PIC X(3).    
           05 WS-DEST-AIRPORT-HIGH                         PIC X(3).    
           05 WS-ORIGIN-AIRPORT-LOW                        PIC X(3).    
           05 WS-ORIGIN-AIRPORT-HIGH                       PIC X(3).    
           05 WS-DEPARTURE-DATE-LOW                        PIC X(10).   
           05 WS-DEPARTURE-DATE-HIGH                       PIC X(10).   
           05 WS-ARRIVAL-DATE-LOW                          PIC X(10).   
           05 WS-ARRIVAL-DATE-HIGH                         PIC X(10).   
           05 WS-FLIGHT-NUMBER-LOW.                                     
              49 WS-FLIGHT-NUMBER-LOW-LEN               PIC S9(4) COMP. 
              49 WS-FLIGHT-NUMBER-LOW-TEXT              PIC X(15).      
           05 WS-FLIGHT-NUMBER-HIGH.                                    
              49 WS-FLIGHT-NUMBER-HIGH-LEN               PIC S9(4) COMP.
              49 WS-FLIGHT-NUMBER-HIGH-TEXT              PIC X(15).     
           05 WS-WHAT-RECORD-TO-READ             PIC S9(4) COMP VALUE 0.
           05 WS-ITER                            PIC S9(4) COMP VALUE 0.
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0.
           05 WS-TEMP-STRING                     PIC X(15) VALUE SPACE. 
       01 WS-QUEUE-1-STRUCTURE.                                         
           05 QUEUE-SCHEDULED-FLIGHT-ID          PIC S9(9) COMP.        
           05 QUEUE-FLIGHT-NUMBER-TO             PIC X(15).             
           05 QUEUE-FLIGHT-NUMBER-FROM           PIC X(15).             
           05 QUEUE-SCHEDULE-START-DATE          PIC X(10).             
           05 QUEUE-SCHEDULE-END-DATE            PIC X(10).             
           EXEC SQL                                                     
            DECLARE C-NAME CURSOR                                       
            FOR                                                         
            SELECT                                                      
              SCHEDULED_FLIGHT_ID,                                      
              FLIGHT_NUMBER_TO,                                         
              FLIGHT_NUMBER_FROM,                                       
              START_SCHEDULE_DATE,                                      
              END_SCHEDULE_DATE                                         
            FROM T10_SCHEDULED_FLIGHTS_TABLE                            
            WHERE                                                       
              SCHEDULED_STATUS <> :CT-DELETED-STATUS                    
                                                                        
            FOR FETCH ONLY                                              
           END-EXEC.                                                    
           EXEC SQL                                                     
            DECLARE C-NAME2 CURSOR                                      
            FOR                                                         
            SELECT DISTINCT                                             
             T04.RESERVATION_ID                                         
                                                                        
             FROM T04_FLIGHT_SEATS T04                                  
             INNER JOIN T05_FLIGHT_TABLE T05                            
             ON T05.FLIGHT_ID = T04.FLIGHT_ID                           
             INNER JOIN  T10_SCHEDULED_FLIGHTS_TABLE    T10             
              ON T10.FLIGHT_NUMBER_TO = T05.FLIGHT_NUMBER OR            
                 T10.FLIGHT_NUMBER_FROM = T05.FLIGHT_NUMBER             
            WHERE                                                       
               T10.FLIGHT_NUMBER_TO = :FLIGHT-NUMBER-TO  AND            
               T10.FLIGHT_NUMBER_FROM = :FLIGHT-NUMBER-FROM             
            FOR FETCH ONLY                                              
           END-EXEC.                                                    
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02232-----------START----------'                   
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02232-----------END------------'                   
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
      * PROGRAM WILL SET CORRECT FLAGS DEPENDING ON WHAT PROGRAM MODE   
      * IS                                                              
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
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
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
            QUEUE(CT-SCHEDULE-QUEUE)                                    
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
               MOVE 'SERIOUS ERROR IN Z02232' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  2001-PROCESS-FIRST-TIME                        
      ****************************************************************  
       2001-PROCESS-FIRST-TIME.                                         
           PERFORM 2101-SEARCH-SCHEDULED-FLIGHTS                        
           MOVE 1 TO WS-Z02172-LAST-REC-ID                              
           PERFORM 2115-DISPLAY-NEXT-10                                 
           .                                                            
      ****************************************************************  
      *                     2200-CHECK-EIBRESP                          
      ****************************************************************  
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2115-DISPLAY-NEXT-10                                 
                                                       
           .                                                            
      ****************************************************************  
      *                2003-PROCESS-NOT-FIRST-TIME                      
      * THIS PARAGRAPH WILL CHECK WHAT KEY WAS PRESSED BY THE USER      
      *                                                                 
      * IF USER PRESSED 'F7' THEN PROGRAM WILL READ PREVIOUS 10 RECORDS 
      * IF USER PRESSED 'F8' THEN PROGRAM WILL READ NEXT 10 RECORD      
      * IF USER PRESSED 'F3' THEN PROGRAM WILL RETURN CONTROL TO        
      * CALLING PROGRAM                                                 
      * IF USER PRESSED ENTER THEN PROGRAM WILL VALIDATE USER           
      * CHOICE                                                          
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHPF7                                                  
              PERFORM 2051-DISPLAY-PREV-10                              
           WHEN DFHPF8                                                  
              PERFORM 2115-DISPLAY-NEXT-10                              
           WHEN DFHENTER                                                
              PERFORM 2031-RECEIVE-USER-INPUT                           
              PERFORM 2032-PROCESS-USER-CHOICE                          
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
      *               2023-MOVE-QUEUE-1-TO-SCREEN                       
      ******************************************************************
       2023-MOVE-QUEUE-1-TO-SCREEN.                                     
           MOVE QUEUE-FLIGHT-NUMBER-TO    TO TO-NUMO(WS-ITER3)          
           MOVE QUEUE-FLIGHT-NUMBER-FROM  TO FR-NUMO(WS-ITER3)          
           MOVE QUEUE-SCHEDULE-START-DATE TO S-DATEO(WS-ITER3)          
           MOVE QUEUE-SCHEDULE-END-DATE   TO E-DATEO(WS-ITER3)          
                                                                        
           MOVE QUEUE-SCHEDULED-FLIGHT-ID TO                            
                                 WS-WHAT-SCHEDULED-ID(WS-ITER3)         
           .                                                            
      ******************************************************************
      *                  2031-RECEIVE-USER-INPUT                        
      ******************************************************************
       2031-RECEIVE-USER-INPUT.                                         
           MOVE LOW-VALUES TO MP0231I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0231') MAPSET('MP0231')                      
            INTO(MP0231I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ***************************************************************   
      *                  2032-PROCESS-USER-CHOICE                       
      * PARAGRAPH WILL GET POSITION ON THE SCREEN WHERE USER PLACED     
      *   AN 'X' ( IF HE PLACED ANY )                                   
      *  IF HE PLACED THAT ON VALID POSITION PROGRAM WILL               
      * SET SCHEDULE STATUS TO DELETED                                  
      ***************************************************************   
       2032-PROCESS-USER-CHOICE.                                        
           INITIALIZE WS-CHOICE-COUNTER                                 
           INITIALIZE WS-USER-CHOICE-POSITION                           
           PERFORM 2301-GET-CHOICE-POSITION                             
           PERFORM 2302-CHECK-CHOICE-NUMBER                             
           PERFORM 2303-CHECK-IF-LINE-EMPTY                             
           EVALUATE TRUE                                                
           WHEN SO-VALID-CHOICE                                         
      * IF USER PLACED AN 'X' NEXT TO THE SCHEDULE NAME AND PRESSED     
      * ENTER, THEN THIS SCHEDULE WILL BE DELETED                       
              PERFORM 2090-DELETE-SCHEDULED-FLIGHT                      
              SET SO-GO-BACK-TO-PREVIOUS TO TRUE                        
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SCHEDULED FLIGHT WAS DELETED  '                     
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN SO-DISPLAY-SCHEDULE-DETAILS                             
      * IF USER PLACED LETTER 'D' NEXT TO THE SCHEDULE NEME AND         
      * PRESSED ENTER THEN PROGRAM Z02212 WILL BE CALEED                
      *                                                                 
      * THIS PROGRAM WILL DISPLAY DETAILS ABOUT THE GIVEN SCHEDULE      
               MOVE WS-WHAT-SCHEDULED-ID(WS-USER-CHOICE-POSITION) TO    
                   WS-Z02312-SCHEDULE-ID                                
               PERFORM 2308-DISPLAY-SCHEDULE-DETAILS                    
           WHEN OTHER                                                   
              PERFORM 2310-SEND-INVALID-CHOICE-MSG                      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN                      
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 >        
                    CT-MAXIMAL-AMOUNT-OF-ROWS OR   SO-END-OF-QUEUE      
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-REC-ID    
             END-IF                                                     
                                                                        
               PERFORM 2023-MOVE-QUEUE-1-TO-SCREEN                      
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-REC-ID     
               ADD 1 TO WS-WHAT-RECORD-TO-READ                          
               PERFORM 2116-READ-THE-QUEUE                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2051-DISPLAY-PREV-10                          
      * IF THS IS POSSIBLE WE WILL START READING FROM RECORD THAT      
      * 10 RECORDS PREVOIUS THAT CURRENT (THE ONE AT THE TOP OF THE    
      * PAGE)                                                          
      *                                                                
      * IF THIS IS NOT POSSIBLE THEN WE WILL START READING FROM        
      * THE FIRST ROW IN QUEUE                                         
      *****************************************************************
       2051-DISPLAY-PREV-10.                                           
           SET SO-NOT-END-OF-QUEUE TO TRUE                             
           PERFORM 2119-INITIALIZE-MAP                                 
                                                                       
            IF WS-Z02172-FIRST-REC-ID - CT-MAXIMAL-AMOUNT-OF-ROWS      
                             >= 1 THEN                                 
              SUBTRACT CT-MAXIMAL-AMOUNT-OF-ROWS                       
                                             FROM WS-Z02172-FIRST-REC-I
            ELSE                                                       
              MOVE 1 TO WS-Z02172-FIRST-REC-ID                         
            END-IF                                                     
           MOVE WS-Z02172-FIRST-REC-ID TO WS-WHAT-RECORD-TO-READ       
           PERFORM 2120-INITIALIZE-WHAT-F-NUM                          
           PERFORM 2116-READ-THE-QUEUE                                 
           PERFORM 2117-CHECK-IF-QIDERR                                
                                                                       
           PERFORM 2053-MOVE-QUEUE-TO-SCRREN                           
           PERFORM 2100-SEND-THE-MAP                                   
           .                                                           
      **************************************************************** 
      *                2090-DELETE-SCHEDULED-FLIGHT                    
      * PARAGRAPH WILL SET SCHEDULE STATUS TO DELTED                   
      * LATER, PARAGRAPH WILL GET FLIGHT NUMBER "TO" AND FLIGHT        
      * NUMBER "FROM", AND THANKS TO THAT INFO PROGRAM WILL            
      * SET ALL STATUSES OF ALL FLIGHTS THAT HAS THOSE FLIGHT NUMBERS  
      *                                                                
      **************************************************************** 
       2090-DELETE-SCHEDULED-FLIGHT.                                   
           MOVE WS-WHAT-SCHEDULED-ID(WS-USER-CHOICE-POSITION) TO        
                        SCHEDULED-FLIGHT-ID                             
           DISPLAY 'SCHEDULED-FLIGHT-ID: ' SCHEDULED-FLIGHT-ID          
                                                                        
                                                                        
           PERFORM 7005-DELETE-SCHEDULED-FLIGHT                         
           PERFORM 2091-GET-FLIGHT-NUMBERS                              
           PERFORM 7006-DELETE-CONNECTED-FLIGHTS                        
           PERFORM 7007-DELETE-RESERVATIONS                             
           .                                                            
      ****************************************************************  
      *                   2091-GET-FLIGHT-NUMBERS                       
      ****************************************************************  
       2091-GET-FLIGHT-NUMBERS.                                         
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           INITIALIZE WS-QUEUE-1-STRUCTURE                              
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2117-CHECK-IF-QIDERR                                 
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM UNTIL SO-END-OF-QUEUE OR SO-RECORD-FOUND             
              PERFORM 2305-FIND-A-RECORD                                
           END-PERFORM                                                  
                                                                        
           IF SO-RECORD-NOT-FOUND THEN                                  
              PERFORM 2306-SEND-NOT-FOUND-MSG                           
           END-IF                                                       
           PERFORM 2307-PREPARE-FLIGHT-NUMBERS                          
           .                                                            
      ****************************************************************  
      *                      2100-SEND-THE-MAP                          
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0231') MAPSET('MP0231')                        
             FROM(MP0231O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                2101-SEARCH-SCHEDULED-FLIGHTS                    
      * PARAGRAPH WILL GET ALL SCHEDULED FLIGHTS FROM THE CURSOR        
      *                                                                 
      * THOSE DATA WILL BE PUT IN THE QUEUE, AND LATER WILL BE          
      * DISPLAYED ON THE SCREEN                                         
      ****************************************************************  
       2101-SEARCH-SCHEDULED-FLIGHTS.                                   
           PERFORM 7001-OPEN-CURSOR                                     
           PERFORM 7002-FETCH-CURSOR-TO-QUEUE                           
           PERFORM 7003-CLOSE-CURSOR                                    
           .                                                            
      ****************************************************************  
      *                   2111-MOVE-DATA-TO-QUEUE                       
      ****************************************************************  
       2111-MOVE-DATA-TO-QUEUE.                                         
           MOVE SCHEDULED-FLIGHT-ID TO QUEUE-SCHEDULED-FLIGHT-ID        
           MOVE FLIGHT-NUMBER-TO-TEXT    TO QUEUE-FLIGHT-NUMBER-TO      
           MOVE FLIGHT-NUMBER-FROM-TEXT  TO QUEUE-FLIGHT-NUMBER-FROM    
           MOVE START-SCHEDULE-DATE TO QUEUE-SCHEDULE-START-DATE        
           MOVE END-SCHEDULE-DATE   TO QUEUE-SCHEDULE-END-DATE          
           .                                                            
      ****************************************************************  
      *                   2112-WRITE-THE-QUEUE                          
      ****************************************************************  
       2112-WRITE-THE-QUEUE.                                            
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-SCHEDULE-QUEUE)                                   
             FROM(WS-QUEUE-1-STRUCTURE)                                 
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .    
      ******************************************************************
      *                    2115-DISPLAY-NEXT-10                         
      ******************************************************************
       2115-DISPLAY-NEXT-10.                                            
           DISPLAY '2115 LEST REC ID: ' WS-Z02172-LAST-REC-ID           
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2119-INITIALIZE-MAP                                  
           MOVE WS-Z02172-LAST-REC-ID TO WS-WHAT-RECORD-TO-READ         
           PERFORM 2120-INITIALIZE-WHAT-F-NUM                           
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
             QUEUE(CT-SCHEDULE-QUEUE)                                   
             INTO(WS-QUEUE-1-STRUCTURE)                                 
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
              MOVE 'THERE IS NO FLIGHTS THAT MEETS CRITERIA ' TO        
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
           MOVE LOW-VALUES TO MP0231O                                   
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
             MOVE LOW-VALUES TO CHOICEA(WS-ITER3)                       
             MOVE LOW-VALUES TO TO-NUMA(WS-ITER3)                       
             MOVE LOW-VALUES TO FR-NUMA(WS-ITER3)                       
             MOVE LOW-VALUES TO S-DATEA(WS-ITER3)                       
             MOVE LOW-VALUES TO E-DATEA(WS-ITER3)                       
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *               2120-INITIALIZE-WHAT-F-NUM                        
      ****************************************************************  
       2120-INITIALIZE-WHAT-F-NUM.                                      
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
              MOVE CT-EMPTY-FIELD   TO WS-WHAT-SCHEDULED-ID(WS-ITER3)   
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
      *                    2301-GET-CHOICE-POSITION                     
      * PROGRAM WILL GET POSITION WHERE USER PLACED HIS CHOICE          
      ******************************************************************
       2301-GET-CHOICE-POSITION.                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                IF CHOICEI(WS-ITER5) = SPACE OR LOW-VALUES THEN         
                  CONTINUE                                              
                ELSE                                                    
                  ADD 1 TO WS-CHOICE-COUNTER                            
                  MOVE CHOICEI(WS-ITER5) TO SW-USER-CHOICE              
                  MOVE WS-ITER5 TO WS-USER-CHOICE-POSITION              
                END-IF                                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2302-CHECK-CHOICE-NUMBER                    
      * PROGRAM WILL CHECK IF USER PLACED ONLY ONE CHOICE               
      * IF THIS NUMBER IS DIFFERNET THEN PROPER MESSAGE WILL BE         
      * DISPLAYED                                                       
      ******************************************************************
       2302-CHECK-CHOICE-NUMBER.                                        
           IF WS-CHOICE-COUNTER = 0 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO CHOSEE SOMETHING        '               
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           IF WS-CHOICE-COUNTER > 1 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU CAN ONLY CHOOSE 1 FLIGHT '                      
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2303-CHECK-IF-LINE-EMPTY                    
      * PARAGRAPH WILL CHECK IF USER PLACED HIS CHOICE NEXT TO          
      * VALID (NOT EMPTY LINE) OR NOT                                   
      ******************************************************************
       2303-CHECK-IF-LINE-EMPTY.                                        
           IF WS-WHAT-SCHEDULED-ID(WS-USER-CHOICE-POSITION) =           
                        CT-EMPTY-FIELD                                  
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PLEASE CHOOSE NOT EMPTY LINE '                      
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2305-FIND-A-RECORD                         
      ******************************************************************
       2305-FIND-A-RECORD.                                              
           IF QUEUE-SCHEDULED-FLIGHT-ID =  SCHEDULED-FLIGHT-ID          
           THEN                                                         
              SET SO-RECORD-FOUND TO TRUE                               
           ELSE                                                         
              ADD 1 TO WS-WHAT-RECORD-TO-READ                           
           END-IF                                                       
           PERFORM 2116-READ-THE-QUEUE                                  
           .                                                            
      ******************************************************************
      *                   2306-SEND-NOT-FOUND-MSG                       
      ******************************************************************
       2306-SEND-NOT-FOUND-MSG.                                         
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'SO-RECORD-NOT-FOUND -> SHOULDNT HAPPEN  ' TO           
                                WS-Z02141-I-ERROR-MESSAGE(1)            
           SET SO-Z02141-M-WITH TO TRUE                                 
           SET SO-GO-BACK-TO-PREVIOUS  TO TRUE                          
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2307-PREPARE-FLIGHT-NUMBERS                   
      ******************************************************************
       2307-PREPARE-FLIGHT-NUMBERS.                                     
           MOVE QUEUE-FLIGHT-NUMBER-TO    TO FLIGHT-NUMBER-TO-TEXT      
           MOVE QUEUE-FLIGHT-NUMBER-FROM  TO FLIGHT-NUMBER-FROM-TEXT    
           MOVE FUNCTION REVERSE(FLIGHT-NUMBER-TO-TEXT) TO              
             WS-TEMP-STRING                                             
           INSPECT WS-TEMP-STRING TALLYING WS-ITER FOR LEADING SPACES   
           COMPUTE FLIGHT-NUMBER-TO-LEN = LENGTH OF                     
                         FLIGHT-NUMBER-TO-TEXT - WS-ITER                
                                                                        
           INITIALIZE WS-TEMP-STRING                                    
           INITIALIZE WS-ITER                                           
           MOVE FUNCTION REVERSE(FLIGHT-NUMBER-FROM-TEXT) TO            
             WS-TEMP-STRING                                             
           INSPECT WS-TEMP-STRING TALLYING WS-ITER FOR LEADING SPACES   
           COMPUTE FLIGHT-NUMBER-FROM-LEN = LENGTH OF                   
                         FLIGHT-NUMBER-FROM-TEXT - WS-ITER              
           .                                                            
      ******************************************************************
      *                  2308-DISPLAY-SCHEDULE-DETAILS                  
      ******************************************************************
       2308-DISPLAY-SCHEDULE-DETAILS.                                   
           SET SO-SCHEDULE-DETAILS TO TRUE                              
           SET  SO-M-FIRST-WITHOUT  TO TRUE                             
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-SCHEDULE-DETAILS)                           
             COMMAREA(DFHCOMMAREA)                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2310-SEND-INVALID-CHOICE-MSG                  
      ******************************************************************
       2310-SEND-INVALID-CHOICE-MSG.                                    
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'PLEASE PLACE "X" NEXT TO FLIGHTS YOU WANT TO DELETE'   
                        TO WS-Z02141-I-ERROR-MESSAGE(1)                 
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
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
      *                    2610-CALL-TO-DISPALY-SEATS                   
      ******************************************************************
       2610-CALL-TO-DISPALY-SEATS.                                      
           MOVE WS-WHAT-SCHEDULED-ID(WS-USER-CHOICE-POSITION) TO        
            Z02192-ONE-WAY-FL-ID(1)                                     
           MOVE 1 TO Z02192-ONE-WAY-FLIGHT-AMOUNT                       
           MOVE 1 TO Z02192-ONE-WAY-TICKET-NUMBER                       
           MOVE 1 TO WS-FLIGHT-COUNTER                                  
           SET SO-ONLY-DISPLAY TO TRUE                                  
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-DISPLAY-SEATS-PROG)                         
                 COMMAREA(DFHCOMMAREA)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2620-CALL-TO-DISPLAY-PASS                     
      ******************************************************************
       2620-CALL-TO-DISPLAY-PASS.                                       
           MOVE WS-WHAT-SCHEDULED-ID(WS-USER-CHOICE-POSITION) TO        
                 Z02242-FLIGHT-NUMBER                                   
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-DISPLAY-ALL-PASSENGERS TO TRUE                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-DISPLAY-PASS-PROG)                          
                 COMMAREA(DFHCOMMAREA)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                          3000-FINAL                             
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              MOVE WS-ZZEC0215 TO DFHCOMMAREA                           
              DISPLAY 'RETURN WITH 0225'                                
              EXEC CICS                                                 
               RETURN TRANSID('0225') COMMAREA(DFHCOMMAREA)             
              END-EXEC                                                
              PERFORM 2200-CHECK-EIBRESP                              
           WHEN SO-FINAL-TERMINATION                                  
              SET SO-M-FIRST-WITH  TO TRUE                            
              EXEC CICS                                               
                XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                 
                 COMMAREA(DFHCOMMAREA)                                
              END-EXEC                                                
              PERFORM 2200-CHECK-EIBRESP                              
           WHEN OTHER                                                 
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
              MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)  
              SET    SO-Z02141-M-WITH TO TRUE                         
              PERFORM 2300-CALL-ERROR-ROUTINE                         
           END-EVALUATE                                               
           .                                                          
      ****************************************************************
      *                      7001-OPEN-CURSOR                         
      ****************************************************************
       7001-OPEN-CURSOR.                                              
           DISPLAY '7001 PERFORMED'                                   
           EXEC SQL                                                   
             OPEN C-NAME                                              
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
            INITIALIZE     WS-QUEUE-1-STRUCTURE                         
                                                                        
             PERFORM 7004-FETCH-C-NAME                                  
             PERFORM UNTIL SO-END-OF-CURSOR-DATA                        
               PERFORM 2111-MOVE-DATA-TO-QUEUE                          
               PERFORM 2112-WRITE-THE-QUEUE                             
               PERFORM 7004-FETCH-C-NAME                                
             END-PERFORM                                                
           .                                                            
      ****************************************************************  
      *                      7003-CLOSE-CURSOR                          
      ****************************************************************  
       7003-CLOSE-CURSOR.                                               
           EXEC SQL                                                     
             CLOSE C-NAME                                               
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
      *                     7004-FETCH-C-NAME                           
      ****************************************************************  
       7004-FETCH-C-NAME.                                               
           SET SO-NOT-END-OF-CURSOR-DATA  TO TRUE                       
           INITIALIZE SCHEDULED-FLIGHT-ID                               
           INITIALIZE FLIGHT-NUMBER-TO                                  
           INITIALIZE FLIGHT-NUMBER-FROM                                
           INITIALIZE START-SCHEDULE-DATE                               
           INITIALIZE END-SCHEDULE-DATE                                 
           EXEC SQL                                                     
            FETCH C-NAME INTO                                           
              :SCHEDULED-FLIGHT-ID,                                     
              :FLIGHT-NUMBER-TO,                                        
              :FLIGHT-NUMBER-FROM,                                      
              :START-SCHEDULE-DATE,                                     
              :END-SCHEDULE-DATE                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7004-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 7005-DELETE-SCHEDULED-FLIGHT                    
      ******************************************************************
       7005-DELETE-SCHEDULED-FLIGHT.                                    
           EXEC SQL                                                     
            UPDATE T10_SCHEDULED_FLIGHTS_TABLE                          
              SET SCHEDULED_STATUS = :CT-DELETED-STATUS                 
             WHERE SCHEDULED_FLIGHT_ID = :SCHEDULED-FLIGHT-ID           
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
               SET SO-7005-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7006-DELETE-CONNECTED-FLIGHTS                   
      ******************************************************************
       7006-DELETE-CONNECTED-FLIGHTS.                                   
           DISPLAY '7006 DATA: '                                        
           DISPLAY 'FLIGHT NUMBER TO '     FLIGHT-NUMBER-TO-TEXT        
           DISPLAY 'FLIGHT NUMBER FROM: '  FLIGHT-NUMBER-FROM-TEXT      
           DISPLAY 'FLIGHT NUMBER TO   LEN'     FLIGHT-NUMBER-TO-LEN    
           DISPLAY 'FLIGHT NUMBER FROM: LEN '  FLIGHT-NUMBER-FROM-LEN   
           DISPLAY 'CT-DELETED-STATUS: '    CT-DELETED-STATUS-TEXT      
           EXEC SQL                                                     
           UPDATE T05_FLIGHT_TABLE                                      
            SET FLIGHT_STATUS = :CT-DELETED-STATUS                      
            WHERE FLIGHT_NUMBER = :FLIGHT-NUMBER-TO OR                  
                  FLIGHT_NUMBER = :FLIGHT-NUMBER-FROM                   
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7006-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7007-DELETE-RESERVATIONS                     
      * PARAGRAPH WILL GET USER'S RESERVATIONS 1 BY 1 UNTIL             
      * ALL RESERVATIONS WILL BE SET AS DELETED                         
      ******************************************************************
       7007-DELETE-RESERVATIONS.                                        
           PERFORM 7008-OPEN-RESERV-CURSOR                              
           PERFORM 7009-FETCH-RESERV-CURSOR                             
           PERFORM 7010-CLOSE-RESERV-CURSOR                             
           .                                                            
      ******************************************************************
      *                  7008-OPEN-RESERV-CURSOR                        
      ******************************************************************
       7008-OPEN-RESERV-CURSOR.                                         
           EXEC SQL                                                     
            OPEN C-NAME2
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7008-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7010-CLOSE-RESERV-CURSOR                       
      ******************************************************************
       7010-CLOSE-RESERV-CURSOR.                                        
           EXEC SQL                                                     
            CLOSE C-NAME2                                               
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7008-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7009-FETCH-RESERV-CURSOR                       
      ******************************************************************
       7009-FETCH-RESERV-CURSOR.                                        
           PERFORM 7011-FETCH-RESERV                                    
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
              PERFORM 7012-DELETE-RESERVATIONS                          
              PERFORM 7011-FETCH-RESERV                                 
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   7011-FETCH-RESERV                             
      ******************************************************************
       7011-FETCH-RESERV.                                               
           SET SO-NOT-END-OF-CURSOR-DATA TO TRUE                        
           INITIALIZE T04-RESERVATION-ID 
           EXEC SQL                                                     
            FETCH C-NAME2                                               
            INTO                                                        
            :T04-RESERVATION-ID                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             CONTINUE                                                   
           WHEN SO-SQLCODE-NOT-FOUND                                    
             SET SO-END-OF-CURSOR-DATA TO TRUE                          
           WHEN OTHER                                                   
             SET SO-7011-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7012-DELETE-RESERVATIONS                     
      * THIS PARAGRAPH WILL SET RESERVATION STATUS TO 'DELETED'         
      ******************************************************************
       7012-DELETE-RESERVATIONS.                                        
           EXEC SQL                                                     
             UPDATE T04_FLIGHT_SEATS                                    
              SET RESERVATION_STATUS = :CT-DELETED-STATUS               
              WHERE RESERVATION_ID = :T04-RESERVATION-ID                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7012-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
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
           PERFORM 2200-CHECK-EIBRESP
           .                                   
      
                                                
                                                       
                                                        
