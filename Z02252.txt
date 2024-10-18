       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02252.                                              
      ******************************************************************
      *               Z02252 (0220)                                     
      *                                                                 
      *  USER CAN PROVIDE DATA BASED ON WHICH PROGRAM WILL SEARCH FOR   
      *  RESRVATIONS  HE CAN ALSO JUST PRESS ENTER WITHOUT PROVIDING    
      *  ANY DATA AND THEN ALL NOT DELETED RESERVATIONS WILL BE         
      *  DISPLAYED                                                      
      *                                                                 
      *  AFTER BEING DISPLAYED RESERVATION CAN BE CHOSEN BY THE USER    
      *  (HE HAVE TO PLACE 'X' NEXT TO RESERVATION DATA AND PRESS ENTER)
      *  AFTER PRESSING ENTER PROGRAM CONTROL WILL BE MOVED TO Z02261   
      *  PROGRAM.                                                       
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZEC0215.                                               
           COPY ZZMP0225.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL INCLUDE T09TAB END-EXEC.                            
           EXEC SQL INCLUDE T12TAB END-EXEC.                            
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
                   88 SO-7016-PARA              VALUE '7016'.           
                   88 SO-7018-PARA              VALUE '7018'.           
       01 CT-CONSTANTS.                                                 
           05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02131  '.        
           05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02252  '.        
           05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.        
           05 CT-RESERVATION-QUEUE    PIC X(8) VALUE '02X9    '.        
           05 CT-DISPLAY-PASS-PROG    PIC X(8) VALUE 'Z02242  '.        
           05 CT-DISPLAY-SEATS-PROG   PIC X(8) VALUE 'Z02192  '.        
           05 CT-DISPLAY-FLIGHTS      PIC X(8) VALUE 'Z02261  '.        
           05 CT-DELETED-STATUS.                                        
              49 CT-DELETED-STATUS-LEN PIC S9(4) COMP VALUE 7.          
              49 CT-DELETED-STATUS-TEXT PIC X(15) VALUE 'DELETED'.      
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
           05 SW-USER-CHOICE                                PIC X.      
              88 SO-VALID-CHOICE                            VALUE 'X'.  
              88 SO-DISPLAY-SEATS                           VALUE '1'.  
              88 SO-DISPLAY-PASSENGERS-DATA                 VALUE '2'.  
              88 SO-DELETE-RESERVATION                      VALUE '3'.  
           05 SW-IF-SCREEN-EMPTY                            PIC X.      
              88 SO-EMPTY-SCREEN-INPUT                      VALUE '1'.  
              88 SO-NOT-EMPTY-SCREEN-INPUT                  VALUE '2'.  
           05 SW-IF-THIS-IS-FIRST-TIME                      PIC X.      
              88 SO-THIS-IS-NOT-FIRST-TIME                  VALUE '1'.  
              88 SO-THIS-IS-FIRST-TIME                      VALUE '2'.  
           05 SW-IF-FINAL-PERFORM-OF-LOOP                   PIC X.      
              88 SO-FINAL-PERFORM                           VALUE '1'.  
              88 SO-NOT-FINAL-PERFORM                       VALUE '2'.  
           05 SW-TYPE-OF-RESERVATION                        PIC X.      
              88  SO-SEARCH-ACTIVE                          VALUE '1'.  
              88  SO-SEARCH-INACTIVE                        VALUE '2'.
              88  SO-SEARCH-BOTH-TYPES                      VALUE '3'.
           05 WS-TYPE-OF-SEARCH                             PIC X.    
              88   SO-ACTIVE-RESERVATION                    VALUE '1'.
              88   SO-INACTIVE-RESERVATION                  VALUE '2'.
           05 SW-IF-VALID-TYPE                              PIC X.    
              88   SO-VALID-TIME                            VALUE '1'.
              88   SO-INVALID-TIME                          VALUE '2'.
           05 SW-IF-RECORD-FOUND                            PIC X.    
              88 SO-RECORD-NOT-FOUND                        VALUE '1'.
              88 SO-RECORD-FOUND                            VALUE '2'.
                                                                      
       01 WS-VARIABLES.                                               
           05 WS-DUMMY                         PIC X VALUE SPACE.     
           05 WS-CURRECT-UTC-TIMESTAMP         PIC X(26) VALUE SPACE. 
           05 WS-TEMP-TIMEZONE.                                       
              10 WS-TIMEZONE-HOUR-AND-SIGN.                           
               15 WS-TIMEZONE-SIGN                    PIC X.          
               15 WS-TIMEZONE-HOUR                    PIC X(2).       
              10 WS-TIMEZONE-FILLER                  PIC X.           
              10 WS-TIMEZONE-MINUTE                  PIC X(2).        
           05 WS-RESERVATION-ID      PIC S9(9) COMP VALUE 0.          
           05 WS-RANDOM-VARIABLE     PIC X.                           
           05 WS-TEMP-NUMERIC        PIC S9(9) COMP VALUE 0.          
           05 WS-RESERVATION-ID-LOW  PIC S9(9) COMP VALUE 0.          
           05 WS-RESERVATION-ID-HIGH PIC S9(9) COMP VALUE 0.          
           05 WS-MAIN-PASSENGER-HIGH.                                 
              49 WS-MAIN-PASSENGER-HIGH-LEN   PIC S9(4) COMP.         
              49 WS-MAIN-PASSENGER-HIGH-TEXT  PIC X(50).              
           05 WS-MAIN-PASSENGER-LOW.                                  
              49 WS-MAIN-PASSENGER-LOW-LEN   PIC S9(4) COMP.          
              49 WS-MAIN-PASSENGER-LOW-TEXT  PIC X(50).               
           05 WS-TIMESTAMP-VAR.                                       
               10 WS-DATE.                                            
                 15 FIRST-DEP-YEAR   PIC 9(4).                        
                 15 FILLER       PIC X VALUE '-'.                     
                 15 FIRST-DEP-MONTH    PIC 9(2).                       
                 15 FILLER       PIC X VALUE '-'.                      
                 15 FIRST-DEP-DAY      PIC 9(2).                       
               10 FILLER       PIC X VALUE '-'.                        
               10 WS-TIME.                                             
                 15 FIRST-DEP-HOUR     PIC 9(2).                       
                 15 FILLER      PIC X VALUE '.'.                       
                 15 FIRST-DEP-MINUTE   PIC 9(2).                       
                 10 FILLER       PIC X VALUE '.'.                      
                 10 FIRST-DEP-SECOND   PIC 9(2).                       
                 10 FILLER       PIC X VALUE '.'.                      
                 10 FIRST-DEP-MICROSEC PIC 9(6).                       
           05 WS-USER-CHOICE-POSITION             PIC S9(4) COMP.      
           05 WS-CHOICE-COUNTER                   PIC S9(4) COMP.      
           05 WS-MODIFIED-TIMESTAMP               PIC X(26).           
           05 WS-TEMP-TIMESTAMP                   PIC X(26).           
           05 WS-TEMP-TIMESTAMP2                  PIC X(26).           
           05 WS-TEMP-INT-VALUE                   PIC S9(9) COMP.      
           05 WS-TEMP-INT-VALUE-FORMAT            PIC S9(9).           
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
              49 WS-FLIGHT-NUMBER-HIGH-LEN               PIC S9(4) COMP
              49 WS-FLIGHT-NUMBER-HIGH-TEXT              PIC X(15).    
           05 WS-WHAT-RECORD-TO-READ             PIC S9(4) COMP VALUE 0
           05 CT-EMPTY-FIELD         PIC X(15) VALUE 'XXXXXXXXXXXXXXX'.
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0
           05 WS-INNER-FLIGHT-COUNTER            PIC S9(4) COMP VALUE 0
       01 WS-QUEUE-1-STRUCTURE.                                        
            05 QUEUE-RESERVATION-ID         PIC X(10).                 
            05 QUEUE-MAIN-PASSENGER-NAME    PIC X(50).                 
            05 QUEUE-FLIGHT-AMOUNT          PIC S9(4) COMP.            
            05 QUEUE-FLIGHTS-DATA OCCURS 8 TIMES.                      
               10 QUEUE-FLIGHT-ID PIC X(15).                           
               10 QUEUE-DEPARTURE-TIMESTAMP PIC X(26).                 
               10 QUEUE-ARRIVAL-TIMESTAMP   PIC X(26).                 
               10 QUEUE-ORIGIN-AIRPORT-CODE   PIC X(3).                
               10 QUEUE-DEST-AIRPORT-CODE      PIC X(3).               
                                                                       
                                                                       
           EXEC SQL                                                    
             DECLARE C-RESERVATION-CURSOR CURSOR                       
             FOR                                                       
             SELECT DISTINCT                                         
              T09.RESERVATION_ID,                                    
              T09.MAIN_PASSENGER_LAST_NAME,                          
              T05.DEPARTURE_TIMESTAMP,                               
              T05.ARRIVAL_TIMESTAMP,                                 
              T05.DEPARTURE_AIRPORT_CODE,                            
              T05.ARRIVAL_AIRPORT_CODE,                              
              T05.FLIGHT_ID                                          
             FROM T09_RESERVATION_MAIN_PASSENGER_TABLE  T09          
             INNER JOIN                                              
             T04_FLIGHT_SEATS T04 ON                                 
             T04.RESERVATION_ID = T09.RESERVATION_ID                 
             INNER JOIN                                              
             T05_FLIGHT_TABLE T05 ON                                 
             T05.FLIGHT_ID = T04.FLIGHT_ID                           
             WHERE                                                   
                T09.RESERVATION_ID >= :WS-RESERVATION-ID-LOW AND     
                T09.RESERVATION_ID <= :WS-RESERVATION-ID-HIGH AND    
                                                                     
                T09.MAIN_PASSENGER_LAST_NAME >=                      
                                  :WS-MAIN-PASSENGER-LOW AND         
                MAIN_PASSENGER_LAST_NAME <=                          
                                  :WS-MAIN-PASSENGER-HIGH            
                              AND                                    
                T04.RESERVATION_STATUS <> :CT-DELETED-STATUS         
             FOR FETCH ONLY                                          
           END-EXEC.                                                 
                                                                     
       LINKAGE SECTION.                                              
       01 DFHCOMMAREA PIC X(17294).                                  
       PROCEDURE DIVISION USING DFHCOMMAREA.                         
           DISPLAY 'Z02252-----------START----------'                
           PERFORM 1000-INIT                                         
           PERFORM 2000-PROCESS                                      
           DISPLAY 'Z02252-----------END------------'                
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
      ***************************************************************** 
       1005-CHECK-IF-FIRST-TIME.                                        
           INITIALIZE WS-ZZEC0215                                       
                                                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
                                                                        
           EVALUATE TRUE                                                
             WHEN SO-M-FIRST-WITHOUT                                    
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1015-SET-START-FLAGS                             
               PERFORM 1020-DELETE-QUEUE                                
               SET SO-M-NOT-FIRST TO TRUE                               
               SET SO-Z02252-DISPLAY TO TRUE                            
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
      *                    1015-SET-START-FLAGS                         
      ******************************************************************
       1015-SET-START-FLAGS.                                            
           SET SO-NOT-EMPTY-SCREEN-INPUT TO TRUE                        
           SET SO-THIS-IS-FIRST-TIME     TO TRUE                        
           .                                                            
      ******************************************************************
      *                       1020-DELETE-QUEUE                         
      ******************************************************************
       1020-DELETE-QUEUE.                                               
           EXEC CICS                                                    
            DELETEQ TS                                                  
            QUEUE(CT-RESERVATION-QUEUE)                                 
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
           MOVE LOW-VALUES TO MP0225O                                   
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                     2200-CHECK-EIBRESP                          
      ****************************************************************  
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2115-DISPLAY-NEXT-15                                 
           .                                                            
      ****************************************************************  
      *                2003-PROCESS-NOT-FIRST-TIME                      
      * PROGRAM EVALUATES THROUGH ALL POSSIBLE ACTION KEYS              
      *                                                                 
      * WHAT IS INTRESTING HERE ENETER KEY HAS TWO FUNCTIONALITIES      
      * BECAUSE OF THE FACT THAT EVERYTHING IS ON THE ONE SCREEN      
      * USER CAN AT THE BEGNING PROVIDE ALL DATA HE IS SEARCHING FOR  
      * AND THEN PRESS ENTER, AFTER BEING PRESS SECOND TIME           
      * ENTER KEY, WILL START PROCESS OF SEARCHING USER CHOICE (      
      * IF USER PLACED 'X' NEXT TO THE RESRVATION DATA )              
      ****************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                   
           EVALUATE EIBAID                                            
           WHEN DFHPF7                                                
              PERFORM 2051-DISPLAY-PREV-15                            
           WHEN DFHPF8                                                
              PERFORM 2115-DISPLAY-NEXT-15                            
           WHEN DFHENTER                                              
                                                                      
                                                                      
      * ENTER KEY HAS 2 FUNCTIONALITIES DEPENING ON THE FACT          
      * IF ENTER WAS PRESSED FOR THE FIRST TIME OR IT IS A X NEXT     
      * TIME                                                          
      *                                                               
      * WHEN ENTER IS PRESSED FOR THE FIRST TIME IT WILL ALLOW        
      * TO RECEIVE DATA FROM THE USER, THEN PROGRAM WILL SEARCH       
      * RESERVATIONS THAT MEETS CRITERIA                              
      *                                                               
      * IF ENTER IS PRESSED FOR THE SECOND OR MORE TIMES THEN         
      * IT WILL START PROCESS OF FINDING USER CHOICE ( USER           
      * CAN PLACE 'X' SYMBOL NEXT TO THE RESERVATION)                 
      * IF THIS PROCESS WILL BE SUCCESSFULL THEN Z02261 PROGRAM       
      * WILL BE CALLED                                                
              IF SO-Z02252-DISPLAY THEN                               
                SET SO-Z02252-GET-CHOICE TO TRUE                      
                PERFORM 2101-SEARCH-THE-RESERVATIONS                  
                MOVE 1 TO  WS-Z02172-LAST-REC-ID                      
                PERFORM 2115-DISPLAY-NEXT-15                          
              ELSE                                                    
                PERFORM 2031-RECEIVE-USER-INPUT                       
                PERFORM 2032-PROCESS-USER-CHOICE                      
              END-IF                                                    
                                                                        
                                                                        
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
      *               2023-MOVE-RESERV-TO-SCREEN                        
      * PARAGRAPH WILL MOVE RESERVATION DATA FROM THE SCREEN TO QUEUE   
      *                                                                 
      ******************************************************************
       2023-MOVE-RESERV-TO-SCREEN.                                      
           MOVE QUEUE-RESERVATION-ID         TO RNUMO(WS-ITER3)         
           MOVE QUEUE-MAIN-PASSENGER-NAME    TO  RNAMO(WS-ITER3)        
           MOVE QUEUE-ORIGIN-AIRPORT-CODE(1) TO A-ORGO(WS-ITER3)        
           MOVE QUEUE-DEST-AIRPORT-CODE(QUEUE-FLIGHT-AMOUNT) TO         
                             A-DESO(WS-ITER3)                           
      * TIEMSTAMP WILL BE MOVED TO VARIABLE THAT WILL ALLOW             
      * TO EXTRACT THE DATE AND THE TIME OUT OF IT                      
      * AND THOSE DATA WILL BE PUT IN SCREEN VARIABLES                  
           MOVE QUEUE-DEPARTURE-TIMESTAMP(1) TO WS-TIMESTAMP-VAR        
           MOVE WS-DATE TO DEP-DO(WS-ITER3)                             
           MOVE WS-TIME TO DEP-TO(WS-ITER3)                             
           MOVE QUEUE-ARRIVAL-TIMESTAMP(QUEUE-FLIGHT-AMOUNT)            
                                       TO WS-TIMESTAMP-VAR              
           MOVE WS-DATE TO ARV-DO(WS-ITER3)                             
           MOVE WS-TIME TO ARV-TO(WS-ITER3)                             
                                                                        
      * PROGRAM USES DATA STRUCTURES THAT WERE CREATED TO WORK WITH     
      * DIFFERENT DATA SO WE ARE USING VARIALBE NAMED                 
      * WS-WHAT-FLIGHT-NUMBER -> THIS ARRAY STORES RESERVATION NUMBERS
      * THAT ARE DISPLAYED ON THE SCREEN                              
           MOVE QUEUE-RESERVATION-ID TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3
           .                                                          
      ****************************************************************
      *                  2031-RECEIVE-USER-INPUT                      
      ****************************************************************
       2031-RECEIVE-USER-INPUT.                                       
           MOVE LOW-VALUES TO MP0225I                                 
           EXEC CICS                                                  
            RECEIVE MAP('MP0225') MAPSET('MP0225')                    
            INTO(MP0225I)                                             
            NOHANDLE                                                  
           END-EXEC                                                   
           EVALUATE EIBRESP                                           
           WHEN DFHRESP(NORMAL)                                       
               CONTINUE                                               
           WHEN DFHRESP(MAPFAIL)                                      
               SET SO-EMPTY-SCREEN-INPUT TO TRUE                      
           WHEN OTHER                                                 
               PERFORM 2200-CHECK-EIBRESP                             
           END-EVALUATE                                               
           .                                                          
      *************************************************************** 
      *                  2032-PROCESS-USER-CHOICE                     
      * PARAGRAPH WILL VALIDATE USER CHOICE,                          
      * IF USER PRESSED HIS CHOICE (LETTER 'X') NEXT TO NON EMPTY     
      * LINE                                                          
      * THEN PROGRAM Z02261 WILL BE CALLED                            
      *                                                               
      * PROGRAM Z02261 WILL DISPLAY ALL FLIGHTS THAT ARE IN           
      * RESERVATION CHOSEN BY THE USER                                
      *************************************************************** 
       2032-PROCESS-USER-CHOICE.                                      
           INITIALIZE WS-CHOICE-COUNTER 
           INITIALIZE WS-USER-CHOICE-POSITION                           
                                                                        
           PERFORM 2301-GET-CHOICE-POSITION                             
           PERFORM 2302-CHECK-CHOICE-NUMBER                             
           PERFORM 2303-CHECK-IF-CHOICE-VALID                           
                                                                        
           EVALUATE TRUE                                                
            WHEN  SO-VALID-CHOICE                                       
              PERFORM 2601-PREPARE-DATA                                 
              PERFORM 2600-CALL-TO-DISPLAY-FLIGHTS                      
            WHEN OTHER                                                  
              PERFORM 2304-SEND-INVALID-CHOICE-MSG                      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN                      
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
                                                OR   SO-END-OF-QUEUE    
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-REC-ID    
             END-IF                                                     
                                                                        
               PERFORM 2023-MOVE-RESERV-TO-SCREEN                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-REC-ID     
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
                                                                        
            IF WS-Z02172-FIRST-REC-ID - 10  >= 1 THEN                   
              SUBTRACT 10 FROM WS-Z02172-FIRST-REC-ID                   
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
      *                      2100-SEND-THE-MAP                          
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0225') MAPSET('MP0225')                        
             FROM(MP0225O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                2101-SEARCH-THE-RESERVATIONS                     
      ****************************************************************  
       2101-SEARCH-THE-RESERVATIONS.                                    
           DISPLAY '2012 PERFORMED'                                     
           PERFORM 2031-RECEIVE-USER-INPUT                              
           PERFORM 2103-PREPARE-DATA                                    
           PERFORM 7001-OPEN-CURSOR                                     
           PERFORM 7002-FETCH-CURSOR-TO-QUEUE                           
           PERFORM 7003-CLOSE-CURSOR                                    
           .                                                            
      ****************************************************************  
      *                     2103-PREPARE-DATA                           
      * HERE PROGRAM WILL MOVE DATA PROVIDED BY THE USER TO THE         
      * VALID PROGRAM VARIABLES                                         
      *                                                                 
      * WHAT IS IMPORTANT THIS CURSOR WORKS LIKE THAT:                  
      * IF USER SPECIFIED FOR EXMAPLE FLIGHT-NUBMER THEN                
      * PROGRAM WILL PUT THIS VARIABLE IN FLIGHT-NUMBER-LOW AND         
      * IN FLIGHT-NUMBER-HIGH                                           
      *                                                                 
      * BUT IN THE CASE THAT USER DIDN'T SPECIFY ANYTHING THEN          
      * PROGRAM WILL MOVE  'AAAAAAAAA' TO FLIGHT-NUBMER-LOW AND         
      * 'ZZZZZZZZZZ' TO FLIGHT-NUMBER-HIGH                              
      * THANKS TO THAT PROGRAM CAN SEARCH FOR GIVEN FLIGHT-NUMBER OR    
      * FOR ALL POSSIBLE FLIGHT-NUMBERS IN THE ONE CURSOR               
      * WITHOUT THE  NEED TO CREATING ANOTHER CURSOR                    
      *                                                                 
      ****************************************************************  
       2103-PREPARE-DATA.                                               
           DISPLAY '2103 PERFORMED '                                    
           IF TYPEI = SPACE OR LOW-VALUES THEN                          
               SET SO-SEARCH-BOTH-TYPES TO TRUE                         
           ELSE                                                         
               MOVE TYPEI TO WS-TYPE-OF-SEARCH                          
               EVALUATE TRUE                                            
                WHEN SO-ACTIVE-RESERVATION                              
                   SET SO-SEARCH-ACTIVE TO TRUE                         
                WHEN SO-INACTIVE-RESERVATION                            
                   SET SO-SEARCH-INACTIVE TO TRUE                       
                WHEN OTHER                                              
                  PERFORM 2400-INITIALIZE-ERROR-MESSAGE                 
                  MOVE 'CHOOSE ACTIVE / INACTIVE'                       
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
                  SET SO-Z02141-M-WITH TO TRUE                          
                  PERFORM 2300-CALL-ERROR-ROUTINE                       
               END-EVALUATE                                             
                            
           END-IF                                                       
                                                                        
           IF RESNUMI = SPACE OR LOW-VALUES THEN                        
              MOVE 0 TO WS-RESERVATION-ID-LOW                           
              MOVE 999999999 TO WS-RESERVATION-ID-HIGH                  
           ELSE                                                         
              INSPECT RESNUMI REPLACING ALL '_' BY ' '                  
              DISPLAY 'RESNUMI BEFORE TEST NUMVAL: '  RESNUMI           
              IF FUNCTION TEST-NUMVAL(RESNUMI) =  0 THEN                
                COMPUTE WS-TEMP-NUMERIC = FUNCTION NUMVAL(RESNUMI)      
                DISPLAY 'RENUMI AFTER NUMVAL '  WS-TEMP-NUMERIC         
                MOVE WS-TEMP-NUMERIC    TO WS-RESERVATION-ID-LOW        
                MOVE WS-TEMP-NUMERIC    TO WS-RESERVATION-ID-HIGH       
                IF WS-TEMP-NUMERIC <= 00 THEN                           
                  PERFORM 2400-INITIALIZE-ERROR-MESSAGE                 
                  MOVE 'INVALID RESERVATION NUMBER'                     
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
                  SET SO-Z02141-M-WITH TO TRUE                          
                  PERFORM 2300-CALL-ERROR-ROUTINE                       
                END-IF                                                  
              ELSE                                                      
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'INVALID RESERVATION NUMBER'                       
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
              END-IF                                                    
           END-IF                                                       
                                                                        
           IF RESNAMI = SPACE OR LOW-VALUES THEN                        
                  MOVE X'00' TO                                         
                    WS-MAIN-PASSENGER-LOW-TEXT                          
                  MOVE X'FF' TO                                         
                    WS-MAIN-PASSENGER-HIGH-TEXT                         
           ELSE                                                         
              INSPECT RESNAMI REPLACING ALL '_' BY ' '                  
              MOVE    RESNAMI      TO WS-MAIN-PASSENGER-LOW-TEXT        
              MOVE    RESNAMI      TO WS-MAIN-PASSENGER-HIGH-TEXT       
           END-IF                                                       
                                                                        
           COMPUTE WS-MAIN-PASSENGER-LOW-LEN =                          
               FUNCTION LENGTH(WS-MAIN-PASSENGER-LOW-TEXT)              
           COMPUTE WS-MAIN-PASSENGER-HIGH-LEN =                         
               FUNCTION LENGTH(WS-MAIN-PASSENGER-HIGH-TEXT)             
           DISPLAY '2103 END'                                           
           .                                                            
      ****************************************************************  
      *                   2111-MOVE-DATA-TO-QUEUE                       
      ****************************************************************  
      *2111-MOVE-DATA-TO-QUEUE.                                         
      *    MOVE T05-FLIGHT-NUMBER-TEXT TO QUEUE-FLIGHT-NUMBER           
      *    MOVE T05-DEPARTURE-AIRPORT-CODE                              
      *                TO QUEUE-DEPARTURE-AIRPORT-CODE                  
      *    MOVE T05-ARRIVAL-AIRPORT-CODE                                
      *                TO QUEUE-DEST-AIRPORT-CODE                       
      *                                                                 
      *    MOVE T05-AIRLINE-CODE TO QUEUE-AIRLINE-CODE                  
      *    .                                                            
      ****************************************************************  
      *                   2112-WRITE-THE-QUEUE                          
      ****************************************************************  
       2112-WRITE-THE-QUEUE.                                            
           DISPLAY '!!!!!!!!!!!!!!!!!!!!-2112-------PERFORMED'          
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-RESERVATION-QUEUE)                                
             FROM(WS-QUEUE-1-STRUCTURE)                                 
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2115-DISPLAY-NEXT-15                         
      ******************************************************************
       2115-DISPLAY-NEXT-15.                                            
           DISPLAY '2115 LEST REC ID: ' WS-Z02172-LAST-REC-ID           
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2119-INITIALIZE-MAP                                  
           MOVE WS-Z02172-LAST-REC-ID TO WS-WHAT-RECORD-TO-READ         
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2120-INITIALIZE-WHAT-F-NUM                           
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
             QUEUE(CT-RESERVATION-QUEUE)                                
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
           MOVE LOW-VALUES TO MP0225O                                   
             MOVE LOW-VALUES TO  TYPEA                                  
             MOVE LOW-VALUES TO  RESNUMA                                
             MOVE LOW-VALUES TO  RESNAMA                                
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
             MOVE LOW-VALUES TO CHOICA(WS-ITER3)                        
             MOVE LOW-VALUES TO RNUMA(WS-ITER3)                         
             MOVE LOW-VALUES TO RNAMA(WS-ITER3)                         
             MOVE LOW-VALUES TO DEP-DA(WS-ITER3)                        
             MOVE LOW-VALUES TO DEP-TA(WS-ITER3)                        
             MOVE LOW-VALUES TO ARV-DA(WS-ITER3)                        
             MOVE LOW-VALUES TO ARV-TA(WS-ITER3)                        
             MOVE LOW-VALUES TO A-ORGA(WS-ITER3)                        
             MOVE LOW-VALUES TO A-DESA(WS-ITER3)                        
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *               2120-INITIALIZE-WHAT-F-NUM                        
      ****************************************************************  
       2120-INITIALIZE-WHAT-F-NUM.        
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
              MOVE CT-EMPTY-FIELD   TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3)  
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                  2121-MOVE-DATA-TO-QUEUE                        
      ****************************************************************  
       2121-MOVE-DATA-TO-QUEUE.                                         
           DISPLAY 'PARAGRAPH 2121 PERFORMED'                           
           DISPLAY '2121-2121 FLIGHT-COUNTER: ' WS-INNER-FLIGHT-COUNTER 
           DISPLAY 'RESERVACJA: ' T09-RESERVATION-ID                    
           MOVE T09-RESERVATION-ID           TO QUEUE-RESERVATION-ID    
           MOVE T09-MAIN-P-LAST-NAME-TEXT TO                            
                                           QUEUE-MAIN-PASSENGER-NAME    
           MOVE WS-INNER-FLIGHT-COUNTER TO QUEUE-FLIGHT-AMOUNT          
                                                                        
           DISPLAY 'WS-INNER-FILGHT-COUNTER: ' WS-INNER-FLIGHT-COUNTER  
           MOVE T05-FLIGHT-ID-TEXT           TO QUEUE-FLIGHT-ID(        
                                             WS-INNER-FLIGHT-COUNTER)   
           MOVE T05-DEPARTURE-AIRPORT-CODE   TO                         
                    QUEUE-ORIGIN-AIRPORT-CODE(WS-INNER-FLIGHT-COUNTER)  
           MOVE T05-ARRIVAL-AIRPORT-CODE     TO                         
                    QUEUE-DEST-AIRPORT-CODE(WS-INNER-FLIGHT-COUNTER)    
                                                                        
           DISPLAY 'CALY RECORD QUEUE W 2121 '                          
           DISPLAY 'QUEUE RESERV ID: ' QUEUE-RESERVATION-ID             
           DISPLAY 'QUEUE MAIN PASS: ' QUEUE-MAIN-PASSENGER-NAME        
           DISPLAY 'QUUE FLIGHT AMOUNT: ' QUEUE-FLIGHT-AMOUNT           
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 >        
                                                            8           
                 DISPLAY 'POD FLIGHT NUMER: ' WS-ITER1                  
                 DISPLAY 'QUEUE-FLIGHT-ID: ' QUEUE-FLIGHT-ID(WS-ITER1)  
                 DISPLAY 'QUEUE-DEPARTURE-TIMESTAMP '                   
                              QUEUE-DEPARTURE-TIMESTAMP(WS-ITER1)       
                 DISPLAY 'QUEUE-ARRIVAL-TIMESTAMP   '                   
                             QUEUE-ARRIVAL-TIMESTAMP(WS-ITER1)      
                 DISPLAY 'QUEUE-ORIGIN-AIRPORT-CODE '                   
                             QUEUE-ORIGIN-AIRPORT-CODE(WS-ITER1)        
                 DISPLAY 'QUEUE-DEST-AIRPORT-CODE  '                    
                            QUEUE-DEST-AIRPORT-CODE(WS-ITER1)           
           END-PERFORM                                                  
           DISPLAY 'PARAGRAPH 2121 STOP '                               
           .                                                            
      ******************************************************************
      *                    2197-PREPARE-RESERVATION-ID                  
      ******************************************************************
       2197-PREPARE-RESERVATION-ID.                                     
           IF FUNCTION TEST-NUMVAL(                                     
              WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION))  = 0 THEN 
             COMPUTE WS-RESERVATION-ID = FUNCTION NUMVAL(               
                       WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION))  
           ELSE                                                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROGRAM ERROR  RESERVATION ID INVALID ' TO          
                 WS-Z02141-I-ERROR-MESSAGE(1)                           
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
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
      *                     2123-SAME-RESERVATION                       
      * PARAGRAPH WILL MOVE DEPARTURE AND ARRIVAL TIMEZONES FROM        
      * UTC TO LOCAL AIRPORTS TIMEZONE                                  
      *                                                                 
      * AND WILL ALSO SAVE  ARRIVAL TIMEZONE RESERVATION ID,            
      * INNER FLIGHT COUNTER (COUNTER OF FLIGHTS IN RESERVATION )       
      * TO (PRE) VARIABLES THOSE VARIALBES WILL BE NEEDED LATER         
      * FOR EXMAPLE TO SAY IF THE NEXT FETCHED RESERVATION IS THE SAME  
      * AS CURRENT                                                      
      ******************************************************************
       2123-SAME-RESERVATION.                                           
           DISPLAY 'START 2123'                                         
           PERFORM 7005-PREPARE-TIMES                                   
           PERFORM 2121-MOVE-DATA-TO-QUEUE                              
           MOVE T05-ARRIVAL-TIMESTAMP TO PRE-LAST-ARRIVAL-TIME-UTC      
           MOVE T09-RESERVATION-ID TO WS-PREV-RESERVATION-ID    
           MOVE T09-MAIN-P-LAST-NAME-TEXT         TO                    
                                   PRE-PASSENGER-LAST-NAME              
           MOVE WS-INNER-FLIGHT-COUNTER TO                              
                                   PRE-INNER-FLIGHT-COUNTER             
           DISPLAY 'END   2123'                                         
           .                                                            
      ******************************************************************
      *                    2124-WRITE-THE-RESERVATION                   
      * THIS PARAGRAPH WRITES QUEUE DATA                                
      * ALL QUEUE DATA WAS PREVIOUSLY EDITED AND ARE READY TO BE USED   
      * LATER IN THE PROGRAM                                            
      ******************************************************************
       2124-WRITE-THE-RESERVATION.                                      
           PERFORM 2112-WRITE-THE-QUEUE                                 
           INITIALIZE WS-QUEUE-1-STRUCTURE                              
           .                                                            
      ******************************************************************
      *                     2300-CALL-ERROR-ROUTINE.                    
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
                                                                        
           MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM     
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           SET  SO-Z02141-I-FIRST-TIME TO TRUE                          
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    2301-GET-CHOICE-POSITION                     
      * PARAGRAPH WILL GET POSITION ON THE SCREEN WHERE USER PLACED     
      * AND "X"                                                         
      *                                                                 
      ******************************************************************
       2301-GET-CHOICE-POSITION.                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                IF CHOICI(WS-ITER5) = SPACE OR LOW-VALUES THEN          
                  CONTINUE                                              
                ELSE                                                    
                  ADD 1 TO WS-CHOICE-COUNTER                            
                  MOVE CHOICI(WS-ITER5) TO SW-USER-CHOICE               
                  MOVE WS-ITER5 TO WS-USER-CHOICE-POSITION              
                END-IF                                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      2302-CHECK-CHOICE-NUMBER                   
      * PARAGRAPH WILL CHECK IF USER CHOOSE ONLY 1 ROW (1 RESERVATION)  
      *                                                                 
      * IF HE CHOOSE DIFFERENT NUMBER THEN ERROR MESSAGE WILL BE        
      * DISPLAYED                                                       
      ******************************************************************
       2302-CHECK-CHOICE-NUMBER.                                        
           IF WS-CHOICE-COUNTER = 0 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO CHOOSE SOMETHING'                       
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
      *                     2303-CHECK-IF-CHOICE-VALID                  
      * PARAGRAPH WILL CHECK IF USER PLACED HIS CHOICE NEXT TO  
      * THE EMPTY LINE OR NOT                                           
      ******************************************************************
       2303-CHECK-IF-CHOICE-VALID.                                      
           IF WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) =          
                        CT-EMPTY-FIELD                                  
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PLEASE CHOOSE NOT EMPTY LINE '                      
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2304-SEND-INVALID-CHOICE-MSG                  
      * PARAGRAPH WILL SEND ERROR MESSAGE IF USER CHOICE IS DIFFERENT   
      * THAN 'X'                                                        
      ******************************************************************
       2304-SEND-INVALID-CHOICE-MSG.                                    
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'INVALID CHOICE          '                              
                        TO WS-Z02141-I-ERROR-MESSAGE(1)                 
           MOVE 'PLACE AN "X" TO CHOOSE THE RESERVATION '               
                        TO WS-Z02141-I-ERROR-MESSAGE(2)                 
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                       2305-FIND-A-RECORD                        
      * PARAGRAPH WILL READ THE QUEUE UNTIL THE QUEUE ENDS OR           
      * UNTIL IT FINDS A RECORD WHERE RESERVATION-ID IS THE SAME        
      * AS RESERVATION ID THAT USER CHOOSE FROM THE SCREEN              
      *                                                                 
      *                                                                 
      * PARAGRAPH WILL ALSO MOVE ITEM NUMBER (NUMBER OF ROW WHERE       
      * THIS INFORMATION WAS FOUND ) AND NAME OF THE QUEUE TO THE       
      * PROGRAM Z02162'S COMMAREA    
      *                                                                 
      *                                                                 
      *                                                                 
      * IF THIS RECORD WASNT IN THE QUEUE THIS MEANS THAT A VERY        
      * STRANGE ERROR HAPPEND AND WE HAVE TO NOTIFY THE USER ABOUT THAT 
      *                                                                 
      ******************************************************************
       2305-FIND-A-RECORD.                                              
           PERFORM UNTIL SO-RECORD-FOUND OR SO-END-OF-QUEUE             
                                                                        
                IF QUEUE-RESERVATION-ID =                               
                   WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)       
                THEN                                                    
                    MOVE WS-WHAT-RECORD-TO-READ TO Z02261-I-RECORD-ID   
                    MOVE CT-RESERVATION-QUEUE   TO Z02261-I-QUEUE-NAME  
                    SET SO-RECORD-FOUND TO TRUE                         
                ELSE                                                    
                    ADD 1 TO WS-WHAT-RECORD-TO-READ                     
                END-IF                                                  
                PERFORM 2116-READ-THE-QUEUE                             
           END-PERFORM                                                  
           IF NOT SO-RECORD-FOUND THEN                                  
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS QUEUE ERROR ' TO                            
                                            WS-Z02141-I-ERROR-MESSAGE(1)
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2306-WHEN-NOT-END-OF-CURSOR                   
      * PARAGRAPH IS CALLED FROM 7002 PARAGRAPH WHEN                    
      * WHILE FETCHING THE RESERVATION THERE WAS SCENARIO LIKE THIS:    
      *     THIS IS A NEW RESERVATION (LAST RESERVATION WAS WRIITEN TO  
      *    THE QUEUE                                                    
      *     AND WE DIDN'T MEET THE END OF A QUEUE 
      *                                                                 
      * BECAUSE WE HAVE TO FINISH THE RESERVATION ( WE FETCHED A NEW    
      * RESERVATION RECORD FROM THE QUEUE BUT FIRST WE NEED TO "CLOSE"  
      * THE OLD ONE)                                                    
      * IN ORHER TO DO THAT WE HAVE TO USE THE "PRE"  - PREVIOUS        
      * VARIABLES THAT STORES IMPORTANT INFORMATIONS ABOUT THE          
      * RESERVATION                                                     
      *                                                                 
      *                                                                 
      ******************************************************************
       2306-WHEN-NOT-END-OF-CURSOR.                                     
           IF WS-INNER-FLIGHT-COUNTER = 1 THEN                          
             MOVE   PRE-FETCHED-ARRIVAL-TIME-UTC                        
              TO WS-TEMP-TIMESTAMP                                      
           ELSE                                                         
              MOVE PRE-LAST-ARRIVAL-TIME-UTC TO                         
                   WS-TEMP-TIMESTAMP                                    
           END-IF                                                       
                                                                        
           IF WS-INNER-FLIGHT-COUNTER = 0 THEN                          
              MOVE T05-ARRIVAL-TIMESTAMP TO WS-TEMP-TIMESTAMP           
                                                                        
           END-IF                                                       
           MOVE T05-ARRIVAL-TIMESTAMP TO  PRE-FETCHED-ARRIVAL-TIME-UTC  
                                                                        
           PERFORM 7011-VALIDATE-RESERVATION                            
           IF SO-VALID-TIME THEN                                        
            PERFORM 2124-WRITE-THE-RESERVATION                          
           END-IF                                                       
           MOVE 1 TO WS-INNER-FLIGHT-COUNTER                            
           PERFORM 2123-SAME-RESERVATION                                
           .                                                            
      ******************************************************************
      *                   2307-WHEN-END-OF-CURSOR                       
      * PARAGRPH IS CALLED BY (7002) PARAGRAPH WHEN:                    
      *    CURSOR IS EMPTY BUT THE LAST FETCHED RESERVATION NEEDS   
      * TO BE DISPLAYED FOR THE USER                                    
      *                                                                 
      * PARAGRAPH WILL VALIDATE IF RESERVATION HAS GOOD TYPE            
      * IS ACTIVE OR INACTIVE                                           
      *                                                                 
      * AND IF IT IS TRUE THEN RESERVATION WILL BE SAVED TO QUEUE       
      *                                                                 
      * LATER THIS QUEUE WILL BE USED TO DISPLAY THOSE DATA             
      * FOR THE USER AND ALSO WILL ALLOW HIM TO USE THE PAGING LOGIC    
      ******************************************************************
       2307-WHEN-END-OF-CURSOR.                                         
           MOVE PRE-FETCHED-ARRIVAL-TIME-UTC TO  WS-TEMP-TIMESTAMP      
           PERFORM 7011-VALIDATE-RESERVATION                            
           IF SO-VALID-TIME THEN                                        
              DISPLAY 'TIMESTAMP: ' T05-ARRIVAL-TIMESTAMP               
              PERFORM 2124-WRITE-THE-RESERVATION                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2308-PREPARE-TIME-OFFSET                      
      * T02-TIME-ZONE2 STORES THE OFFSET THAT WE WILL USE               
      * TO MOVE TIMESTAMP FROM UTC TIMEZONE TO LOCAL AIRPORT TIMEZONE   
      * OFFSET IS STORES IN FORMAT SHH.MM WHERE S MEANS THE SIGN        
      * HH MEANS THE AMOUNT OF HOURS AND MM MEANS THE AMOUNT OF         
      * MINUTES                                                         
      ******************************************************************
       2308-PREPARE-TIME-OFFSET.                                        
           INITIALIZE WS-HOUR-OFFSET-TEMP                               
           INITIALIZE WS-MINUTE-OFFSET-TEMP                             
           MOVE T02-TIME-ZONE2            TO WS-TEMP-TIMEZONE           
           MOVE WS-TIMEZONE-HOUR-AND-SIGN TO WS-HOUR-OFFSET-TEMP        
           MOVE WS-TIMEZONE-SIGN          TO WS-MINUTE-OFFSET-TEMP(1:1) 
           MOVE WS-TIMEZONE-MINUTE        TO WS-MINUTE-OFFSET-TEMP(2:2) 
           DISPLAY 'TUTAJ POWINIEN BYC BLAD: '                          
             DISPLAY WS-HOUR-OFFSET-TEMP                                
             DISPLAY WS-MINUTE-OFFSET-TEMP   
           IF FUNCTION TEST-NUMVAL(WS-HOUR-OFFSET-TEMP) = 0 AND         
              FUNCTION TEST-NUMVAL(WS-MINUTE-OFFSET-TEMP) = 0 THEN      
                COMPUTE WS-HOUR-OFFSET = FUNCTION NUMVAL(               
                               WS-HOUR-OFFSET-TEMP)                     
                COMPUTE WS-MINUTE-OFFSET = FUNCTION NUMVAL(             
                              WS-MINUTE-OFFSET-TEMP)                    
           ELSE                                                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID DATA FROM DATABASE' TO                      
                                    WS-Z02141-I-ERROR-MESSAGE(1)        
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           DISPLAY ' PO SPRAWDZANIU OFFSETU '                           
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
      *                   F F -CALL-TO-DISPLAY-FLIGHTS                  
      ******************************************************************
       2600-CALL-TO-DISPLAY-FLIGHTS.                                    
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-DISPLAY-FLIGHTS)                            
               COMMAREA(DFHCOMMAREA)                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2601-PREPARE-DATA                            
      * PROGRAM WILL PASS TO THE NEXT PROGRAM                           
      *   1. NAME OF THE QUEUE                                          
      *   2. RECORD OF THE QUEUE WHERE OUR RESERVATION IS STORED        
      *                                                                 
      * BUT FIRST WE NEED TO GET THIS INFORMATION                       
      *                                                                 
      * TO GET THAT WE WILL READ THE RESERVATION QUEUE AS LONG AS       
      * WE WILL FIND A ROW WHERE RESERVATION ID WILL BE EQUAL TO        
      * TO RESRVATION ID THAT WAS CHOSEN BY THE USER                    
      *                                                                 
      ******************************************************************
       2601-PREPARE-DATA.                                               
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           SET SO-RECORD-NOT-FOUND TO TRUE                              
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2117-CHECK-IF-QIDERR                                 
           PERFORM 2305-FIND-A-RECORD                                   
           .                                                            
      ******************************************************************
      *                    2610-CALL-TO-DISPALY-SEATS                   
      ******************************************************************
       2610-CALL-TO-DISPALY-SEATS.                                      
           MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) TO       
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
      *                     2800-DELETE-RESERVATION                     
      ******************************************************************
       2800-DELETE-RESERVATION.                                         
           PERFORM 2197-PREPARE-RESERVATION-ID                          
           PERFORM 7012-DELETE-FROM-T09-TAB                             
           PERFORM 7013-DELETE-FROM-T12-TAB                             
           PERFORM 7014-DELETE-FROM-T04-TAB                             
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'RESERVATION DROPPED     ' TO                           
              WS-Z02141-I-ERROR-MESSAGE(1)                              
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                          3000-FINAL                             
      ******************************************************************
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              MOVE WS-ZZEC0215 TO DFHCOMMAREA                           
              DISPLAY 'RETURN WITH 0220'                                
              EXEC CICS                                                 
               RETURN TRANSID('0220') COMMAREA(DFHCOMMAREA)             
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN SO-FINAL-TERMINATION                                    
              SET SO-M-FIRST-WITH  TO TRUE                              
              EXEC CICS                                                 
               XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                    
               COMMAREA(DFHCOMMAREA) LENGTH(0)                          
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
           DISPLAY 'PRZED OTWARCIEM '                                  
           DISPLAY 'WS RESERV LOW: TEXT ' WS-MAIN-PASSENGER-LOW-TEXT   
           DISPLAY 'WS RESERV LOW: LEN  ' WS-MAIN-PASSENGER-HIGH-TEXT  
           DISPLAY 'WS RESERV HIGH: TEXT ' WS-MAIN-PASSENGER-LOW-TEXT  
           DISPLAY 'WS RESERV HIGH: LEN  ' WS-MAIN-PASSENGER-HIGH-TEXT 
           SET SO-THIS-IS-FIRST-TIME TO TRUE                           
           MOVE 0 TO WS-INNER-FLIGHT-COUNTER                           
           EXEC SQL                                                    
             OPEN C-RESERVATION-CURSOR                                 
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
      * PROGRAM WILL FETCH ALL RESERVATIONS THAT MEETS CRITERIA        
      * AND PREPARE THEM FOR BEING DISPLAYED                           
      * AFTER BEING FETCHED 
      * RESERVATION WILL BE PUT INTO THE QUEUE                          
      * LATER RECORDS FROM THE QUEU WILL BE DISPLAYED ON THE SCREEN     
      ****************************************************************  
       7002-FETCH-CURSOR-TO-QUEUE.                                      
           MOVE LOW-VALUES TO WS-QUEUE-1-STRUCTURE                      
                                                                        
           SET SO-NOT-FINAL-PERFORM TO TRUE                             
                                                                        
           PERFORM 7004-FETCH-C-NAME                                    
                                                                        
      * IF FETCH DIDN'T RETURN ANY RECORDS AT ALL                       
      * THE LOOP BELOW WAN'T BE EXECUTED AT ALL                         
           IF SO-END-OF-CURSOR-DATA THEN                                
               SET SO-FINAL-PERFORM TO TRUE                             
           END-IF                                                       
                                                                        
           PERFORM UNTIL  SO-END-OF-CURSOR-DATA AND SO-FINAL-PERFORM    
                                                                        
      * IF CUROSR IS EMPTY WE WILL END THIS LOOP                        
               IF SO-END-OF-CURSOR-DATA THEN                            
                  SET SO-FINAL-PERFORM TO TRUE                          
               END-IF                                                   
                                                                        
                                                                        
               IF T09-RESERVATION-ID = WS-PREV-RESERVATION-ID THEN      
                  DISPLAY '2123-SAME RESERVATION '                      
                  ADD 1 TO WS-INNER-FLIGHT-COUNTER                      
                  PERFORM 2123-SAME-RESERVATION                         
                                                                        
               ELSE                                                     
      * IF THE RESERVATION THAT WAS FETCHED IS DIFFERENT THAT THE       
      * PREVIOUS ONE                                                    
                                                                        
                  IF NOT SO-END-OF-CURSOR-DATA THEN                     
                    PERFORM 2306-WHEN-NOT-END-OF-CURSOR                 
                  ELSE  
                    PERFORM 2307-WHEN-END-OF-CURSOR                     
                  END-IF                                                
                                                                        
               END-IF                                                   
                                                                        
               PERFORM 7004-FETCH-C-NAME                                
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                      7003-CLOSE-CURSOR                          
      ****************************************************************  
       7003-CLOSE-CURSOR.                                               
           EXEC SQL                                                     
             CLOSE C-RESERVATION-CURSOR                                 
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
           DISPLAY '7004 FETCH  '                                       
           INITIALIZE  T09-RESERVATION-ID                               
           INITIALIZE  T09-MAIN-PASSENGER-LAST-NAME                     
           INITIALIZE  T05-DEPARTURE-TIMESTAMP                          
           INITIALIZE  T05-ARRIVAL-TIMESTAMP                            
           INITIALIZE  T05-DEPARTURE-AIRPORT-CODE                       
           INITIALIZE  T05-ARRIVAL-AIRPORT-CODE
           INITIALIZE  T05-FLIGHT-ID                                    
           EXEC SQL                                                     
            FETCH C-RESERVATION-CURSOR INTO                             
              :T09-RESERVATION-ID,                                      
              :T09-MAIN-PASSENGER-LAST-NAME,                            
              :T05-DEPARTURE-TIMESTAMP,                                 
              :T05-ARRIVAL-TIMESTAMP,                                   
              :T05-DEPARTURE-AIRPORT-CODE,                              
              :T05-ARRIVAL-AIRPORT-CODE,                                
              :T05-FLIGHT-ID                                            
                                                                        
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE PO FETCH TO: ' WS-SQLCODE-FORMAT            
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY 'FETCH RESESRVATIONID : '  T09-RESERVATION-ID     
              IF SO-THIS-IS-FIRST-TIME THEN                             
      * THIS IS FIRST FETCH IN PROGRAM                                  
                MOVE T05-ARRIVAL-TIMESTAMP TO                           
                                      PRE-FETCHED-ARRIVAL-TIME-UTC      
                DISPLAY '7004: SO-THIS-IS-FIRST-TIME '                  
                MOVE T09-RESERVATION-ID TO WS-PREV-RESERVATION-ID       
                SET SO-THIS-IS-NOT-FIRST-TIME TO TRUE                   
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY 'SO-END-OF-CURSOR-DATA '                          
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7004-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                        7005-PREPARE-TIMES                       
      * TIME-ZONE2 IS IN FORMAT SHH.MM                                  
      ******************************************************************
       7005-PREPARE-TIMES.                                              
           DISPLAY '7005 LOT LICZNIK: ' WS-INNER-FLIGHT-COUNTER         
           MOVE    T05-DEPARTURE-AIRPORT-CODE TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7006-PREPARE-THE-ORG-TIME                            
           MOVE    WS-MODIFIED-TIMESTAMP   TO QUEUE-DEPARTURE-TIMESTAMP(
                                           WS-INNER-FLIGHT-COUNTER)     
                                                                        
           MOVE    T05-ARRIVAL-AIRPORT-CODE   TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7007-PREPARE-THE-DES-TIME                            
           DISPLAY 'PRZED DZIWNYM MOVE '                                
           DISPLAY 'DEST TIMESTAMP123: ' WS-MODIFIED-TIMESTAMP          
           MOVE    WS-MODIFIED-TIMESTAMP   TO QUEUE-ARRIVAL-TIMESTAMP(  
                                         WS-INNER-FLIGHT-COUNTER)       
           DISPLAY 'PO DZIWNYM MOVE    '                                
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                      7006-PREPARE-THE-ORG-TIME                  
      * THIS PARAGRAPH WILL MOVE DEPARTURE TIMESTAMP FROM UTC TIEMZONE  
      * TO LOCAL AIRPORT TIMEZONE                                       
      ******************************************************************
       7006-PREPARE-THE-ORG-TIME.                                       
           PERFORM 2308-PREPARE-TIME-OFFSET                             
                                                                        
           INITIALIZE WS-MODIFIED-TIMESTAMP                             
                                                                        
           EXEC SQL                                                     
            SELECT CHAR(TIMESTAMPADD(4,:WS-MINUTE-OFFSET,               
            ZMIENNA))                                                   
            INTO :WS-MODIFIED-TIMESTAMP          
            FROM                                                        
            (SELECT TIMESTAMPADD(8,:WS-HOUR-OFFSET,                     
            DEPARTURE_TIMESTAMP)                                        
              AS  ZMIENNA                                               
            FROM  T05_FLIGHT_TABLE                                      
            WHERE FLIGHT_ID = :T05-FLIGHT-ID )                          
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7005-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7007-PREPARE-THE-DES-TIME                   
      * THIS PARAGRAPH WILL MOVE ARRIVAL TIMESTAMP FROM UTC TIEMZONE    
      * TO LOCAL AIRPORT TIMEZONE                                       
      ******************************************************************
       7007-PREPARE-THE-DES-TIME.                                       
           PERFORM 2308-PREPARE-TIME-OFFSET                             
           INITIALIZE WS-MODIFIED-TIMESTAMP                             
           EXEC SQL                                                     
            SELECT CHAR(TIMESTAMPADD(4,:WS-MINUTE-OFFSET,ZMIENNA))      
            INTO :WS-MODIFIED-TIMESTAMP                                 
            FROM                                                        
            (SELECT TIMESTAMPADD(8,:WS-HOUR-OFFSET,                     
              ARRIVAL_TIMESTAMP)                                        
              AS  ZMIENNA                                               
            FROM  T05_FLIGHT_TABLE                                      
            WHERE FLIGHT_ID = :T05-FLIGHT-ID)                           
           END-EXEC                                                     
            MOVE SQLCODE TO SW-SQLCODE                                  
            IF NOT SO-SQLCODE-NORMAL THEN                               
               SET SO-7012-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR        
            END-IF                                                      
           .                                                            
      ******************************************************************
      *                7010-FETCH-TIMEZONE                              
      ******************************************************************
       7010-FETCH-TIMEZONE.                                             
           INITIALIZE T02-TIME-ZONE2                                    
           DISPLAY '7010 -> FETCH TIMEZONE'                             
           DISPLAY '7010 -. AIRPORT CDOE:  ' T02-AIRPORT-CODE           
           EXEC SQL                                                     
            SELECT TIME_ZONE2                                           
            INTO :T02-TIME-ZONE2                                        
            FROM T02_AIRPORT_TABLE                                      
            WHERE AIRPORT_CODE = :T02-AIRPORT-CODE                      
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
                                                                        
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             DISPLAY '7010 T02-TIME-ZONE2: ' T02-TIME-ZONE2             
                                                                        
           WHEN OTHER                                                   
                                                                        
             SET SO-7010-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   7011-VALIDATE-RESERVATION                     
      * BEFORE PRESSING ENTER USER CAN CHOOSE IF HE WANTS TO            
      * SEARCH FOR ACTIVE OR INACTIVE RESERVATIONS (OPTIONS '1' OR '2') 
      *                                                                 
      * IF HE DIDN'T SPECIFY ANYTHING THIS MEANS THAT HE DOESN'T CARE   
      * ABOUT THAT                                                      
      *                        
      * AN ACTIVE RESERVATION MEANS THAT THERE IS LESS THAN 24 HOURS    
      * AFTER THE LANDING                                               
      *                                                                 
      * AN INCATIVE MEANS THE OPPOSITE                                  
      *                                                                 
      * THIS PARAGRAPH WILL CHECK IF THOSE TIMES ARE VALID OR NOT       
      *                                                                 
      *                                                                 
      * AT THE END OF PARAGRAPH WE WILL HAVE                            
      * 1 OUT OF 2 FLAGS SET TO TRUE:                                   
      *        SO-VALID-TIME      OR   SO-INVALID-TIME                  
                                                                        
      * IF SO-INVALID-TIME IS TRUE IT MEANS THAT CURRECT                
      * RESERVATION SHOULD NOT BE DISPLAYED                             
      ****************************************************************  
       7011-VALIDATE-RESERVATION.                                       
           SET SO-VALID-TIME TO TRUE                                    
                                                                        
           EVALUATE TRUE                                                
           WHEN SO-SEARCH-BOTH-TYPES                                    
              SET SO-VALID-TIME TO TRUE                                 
           WHEN SO-SEARCH-ACTIVE                                        
             PERFORM 7015-CHECK-IF-ACTIVE-VALID                         
           WHEN SO-SEARCH-INACTIVE                                      
                                                                        
             PERFORM 7016-CHECK-IF-INACTIVE-VALID                       
                                                                        
      * THIS SHOULDNT HAPPEN                                            
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID TIME CHECK' TO WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7012-DELETE-FROM-T09-TAB                    
      ******************************************************************
       7012-DELETE-FROM-T09-TAB.                                        
           EXEC SQL                                                     
              DELETE FROM T09_RESERVATION_MAIN_PASSENGER_TABLE          
              WHERE                                                     
              RESERVATION_ID = :WS-RESERVATION-ID                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
             SET SO-7012-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7013-DELETE-FROM-T12-TAB                      
      ******************************************************************
       7013-DELETE-FROM-T12-TAB.                                        
           EXEC SQL                                                     
            DELETE FROM T12_RESERVATION_PASSENGERS                      
            WHERE                                                       
               RESERVATION_ID = :WS-RESERVATION-ID                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
              SET SO-7013-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7014-DELETE-FROM-T04-TAB                      
      ******************************************************************
       7014-DELETE-FROM-T04-TAB.                                        
           EXEC SQL                                                     
            DELETE FROM T04_FLIGHT_SEATS                                
            WHERE              
                RESERVATION_ID = :WS-RESERVATION-ID                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
               SET SO-7014-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7015-CHECK-IF-ACTIVE-VALID                     
      * CURRENT MAINFRMAE TIME IS -6 UTC SO WE NEED ADD THIS 6 HOURS    
      * TO GET UTC TIMESTAMP                                            
      ******************************************************************
       7015-CHECK-IF-ACTIVE-VALID.                                      
           EXEC SQL                                                     
            SELECT "A"                                                  
             INTO :WS-RANDOM-VARIABLE                                   
             FROM T05_FLIGHT_TABLE                                      
             WHERE                                                      
              TIMESTAMPDIFF(8, CHAR(CURRENT_TIMESTAMP + 6 HOURS         
               - TIMESTAMP(:WS-TEMP-TIMESTAMP)))  < 24                  
                       AND                                              
               TIMESTAMPDIFF(8, CHAR(CURRENT_TIMESTAMP + 6 HOURS        
                - TIMESTAMP(:WS-TEMP-TIMESTAMP)))  > 0                  
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             SET SO-VALID-TIME TO TRUE                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
             SET SO-INVALID-TIME TO TRUE                                
           WHEN OTHER                                                   
                SET SO-7015-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE    
           .                                                            
                                                                        
      ******************************************************************
      *                  7016-CHECK-IF-INACTIVE-VALID                   
      ******************************************************************
       7016-CHECK-IF-INACTIVE-VALID.                                    
           DISPLAY '7016'                                               
           DISPLAY 'WS-TEMP TIMESTAMP: ' WS-TEMP-TIMESTAMP              
           EXEC SQL                                                     
            SELECT "A"                                                  
             INTO :WS-RANDOM-VARIABLE                                   
             FROM T05_FLIGHT_TABLE                                      
             WHERE                                                      
              TIMESTAMPDIFF(8, CHAR(CURRENT_TIMESTAMP + 6 HOURS         
               - TIMESTAMP(:WS-TEMP-TIMESTAMP)))  > 24                  
                       OR                                               
               TIMESTAMPDIFF(8, CHAR(CURRENT_TIMESTAMP + 6 HOURS        
                - TIMESTAMP(:WS-TEMP-TIMESTAMP)))  < 0                  
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             SET SO-VALID-TIME TO TRUE                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
             SET SO-INVALID-TIME TO TRUE                                
           WHEN OTHER                                                   
                SET SO-7016-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  7018-GET-CURRENT-UTC-TIMESTAMP                 
      * CURRENCT MAINFRAME TIME STAMP IS 6 HOURS BEFORE UTC TIME        
      *  SO WE NEED TO ADD THOSE 6 HOURS                                
      ******************************************************************
       7018-GET-CURRENT-UTC-TIMESTAMP.                                  
           EXEC SQL                                                     
             SELECT CURRENT_TIMESTAMP + 6 HOURS                         
             INTO :WS-CURRECT-UTC-TIMESTAMP                             
             FROM T05_FLIGHT_TABLE                                      
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7018-PARA    TO TRUE                                
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

                                             
                                         
                                         
                           
                       

                         
                                                
                                           
                                

                           
    
                     
                                   
        

        
              
    
                              

   
                              
                     
                    
