       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02232.                                              
      ******************************************************************
      *               Z02232 (0218)                                     
      * PROGRAM WILL FIND AND DISPLAY FLIGHTS THAT MEETS CRITERIA       
      * GIVEN BY THE USER                                               
      *                                                                 
      * USER CAN MOVE RESULT SET BY USING F7 AND F8 KEYES               
      *                                                                 
      *                                                                 
      *  SEARCHING PROCESS WILL BE MADE BY THE CURSOR THAT WILL RETURN  
      *  ALL POSSIBLE FLIGHTS                                           
      *                                                                 
      *                                                                 
      *                                                                 
      *  AFTER DISPLAYING FLIGHTS THAT MEETS CRITERIA                   
      *                                                                 
      *    1. IN SCENARIO 1 USER CAN PLACE '1' OR '2' NEXT TO THE FLIGHT
      *     NAME AND IF HE CHOOSES 1 THEN PROGRAM (Z02192) WILL BE      
      *     CALLED AND HE WILL SEE GRAFICAL REPRESENTATION OF THE SEATS 
      *     IF CHE CHOOSE '2' THEN HE WILL SEE LIST OF PASSENGERS FOR   
      *     THIS FLIGHT                                                 
      *    2. IN SCENARIO 2 USER CAN PLACE ONLY 'X' SYMBOL NEXT TO THE  
      *     FLIGHT, AFTER PRESSING ENTER THIS FLIGHT WILL BE DELETED    
      *    ALONG WITH ALL RESERVATIONS MADE FOR THIS FLIGHT             
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0217.                                               
           COPY ZZEC0215.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T10TAB END-EXEC.                            
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
                   88 SO-7017-PARA              VALUE '7017'.           
                   88 SO-7018-PARA              VALUE '7018'.           
                   88 SO-7019-PARA              VALUE '7019'.           
       01 CT-CONSTANTS.                                                 
           05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02221  '.        
           05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02232  '.        
           05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.        
           05 CT-FLIGHT-QUEUE         PIC X(8) VALUE '02X7    '.        
           05 CT-DISPLAY-PASS-PROG    PIC X(8) VALUE 'Z02242  '.        
           05 CT-DISPLAY-SEATS-PROG   PIC X(8) VALUE 'Z02192  '.        
           05 CT-30-MINUTES           PIC S9(4) COMP VALUE 30.          
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
           05 SW-IF-STATUS-VALID                            PIC X.      
              88 SO-STATUS-VALID                            VALUE '1'.  
              88 SO-STATUS-INVALID                          VALUE '2'.  
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
           05 SW-IF-PART-OF-SCHEDULE                        PIC X.      
              88 SO-PART-OF-A-SCHEDULE                      VALUE '1'.  
              88 SO-NOT-PART-OF-A-SCHEDULE                  VALUE '2'.  
           05 SW-IF-PART-TO-OR-FROM                         PIC X.      
              88 SO-THIS-IS-FLIGHT-TO                       VALUE '1'.  
              88 SO-THIS-IS-FLIGHT-FROM                     VALUE '2'.  
           05 SW-USER-CHOICE                                PIC X.      
              88 SO-VALID-CHOICE                            VALUE 'X'.  
              88 SO-DISPLAY-SEATS                           VALUE '1'.  
              88 SO-DISPLAY-PASSENGERS-DATA                 VALUE '2'.  
       01 WS-VARIABLES.                                                 
           05 WS-ILE-MINUT                       PIC S9(9) COMP.        
           05 WS-ILE-MINUT-FORMAT                PIC X(10) VALUE SPACE. 
           05 WS-TEMP-TIMESTAMP                  PIC X(26).             
           05 WS-CURRENT-TIMESTAMP               PIC X(26).             
           05 WS-DUMMY                           PIC X.                 
           05 WS-TEMP-TIMEZONE.                                         
              10 WS-TIMEZONE-HOUR-AND-SIGN.                             
               15 WS-TIMEZONE-SIGN               PIC X.                 
               15 WS-TIMEZONE-HOUR               PIC X(2).              
              10 WS-TIMEZONE-FILLER              PIC X.                 
              10 WS-TIMEZONE-MINUTE              PIC X(2).              
                                                                        
           05 WS-USER-CHOICE-POSITION            PIC S9(4) COMP.        
           05 WS-CHOICE-COUNTER                  PIC S9(4) COMP.        
           05 WS-MODIFIED-TIMESTAMP              PIC X(26).             
           05 WS-HOUR-OFFSET                     PIC S9(4) COMP VALUE 0.
           05 WS-MINUTE-OFFSET                   PIC S9(4) COMP VALUE 0.
           05 WS-HOUR-OFFSET-TEMP                PIC X(10) VALUE SPACE. 
           05 WS-MINUTE-OFFSET-TEMP              PIC X(10) VALUE SPACE. 
           05 WS-STATUS1.                                               
              49 WS-STATUS1-LEN                  PIC S9(4) COMP.        
              49 WS-STATUS1-TEXT                 PIC X(15).             
           05 WS-STATUS2.                                               
              49 WS-STATUS2-LEN                  PIC S9(4) COMP.        
              49 WS-STATUS2-TEXT                 PIC X(15).             
           05 WS-STATUS3.                                               
              49 WS-STATUS3-LEN                  PIC S9(4) COMP.        
              49 WS-STATUS3-TEXT                 PIC X(15).             
           05 WS-STATUS4.                                               
              49 WS-STATUS4-LEN                  PIC S9(4) COMP.        
              49 WS-STATUS4-TEXT                 PIC X(15).             
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
           05 CT-EMPTY-FIELD         PIC X(15) VALUE 'XXXXXXXXXXXXXXX'. 
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0.
       01 WS-FLIGHT-QUEUE-STRUCTURE.                                    
            05 QUEUE-FLIGHT-NUMBER          PIC X(15).                  
            05 QUEUE-FLIGHT-ID              PIC X(15).                  
            05 QUEUE-DEPARTURE-AIRPORT-CODE PIC X(3).                   
            05 QUEUE-DEPARTURE-TIMESTAMP.                               
              10 QUEUE-1-DATE-DEP.                                      
                15 QUEUE-1-DEP-YEAR   PIC 9(4).                         
                15 FILLER       PIC X VALUE '-'.                        
                15 QUEUE-1-DEP-MONTH    PIC 9(2).                       
                15 FILLER       PIC X VALUE '-'.                        
                15 QUEUE-1-DEP-DAY      PIC 9(2).                       
              10 FILLER       PIC X VALUE '-'.                          
              10 QUEUE-1-TIME-DEP.                                      
                15 QUEUE-1-DEP-HOUR     PIC 9(2).  
                15 FILLER      PIC X VALUE '.'.                         
                15 QUEUE-1-DEP-MINUTE   PIC 9(2).                       
              10 FILLER       PIC X VALUE '.'.                          
              10 QUEUE-1-DEP-SECOND   PIC 9(2).                         
              10 FILLER       PIC X VALUE '.'.                          
              10 QUEUE-1-DEP-MICROSEC PIC 9(6).                         
            05 QUEUE-ARRIVAL-AIRPORT-CODE   PIC X(3).                   
            05 QUEUE-ARRIVAL-TIMESTAMP.                                 
              10 QUEUE-1-ARV-DATE.                                      
               15 QUEUE-1-ARV-YEAR   PIC 9(4).                          
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-1-ARV-MONTH    PIC 9(2).                        
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-1-ARV-DAY      PIC 9(2).                        
              10 FILLER       PIC X VALUE '-'.                          
              10 QUEUE-1-ARV-TIME.                                      
               15 QUEUE-1-ARV-HOUR     PIC 9(2).                        
               15 FILLER      PIC X VALUE '.'.                          
               15 QUEUE-1-ARV-MINUTE   PIC 9(2).                        
              10 FILLER       PIC X VALUE '.'.                          
              10 QUEUE-1-ARV-SECOND   PIC 9(2).                         
              10 FILLER       PIC X VALUE '.'.                          
              10 QUEUE-1-ARV-MICROSEC PIC 9(6).                         
            05 QUEUE-AIRLINE-CODE           PIC X(3).                   
            05 QUEUE-TRANSFER-NUMBER        PIC X(2).                   
                                                                        
      * THIS CURSOR CAN BE USED WITHOUT LOOKING AT WHAT DATA WE HAVE    
      * IT CAN SEARCH BY GIVEN VALUE OR WILL JUST LOOK FOR EVERY POSSIBL
      * VALUE ( DATA NEEDS TO BE PREPARED BEFORE OPENING THIS CURSOR)   
           EXEC SQL                                                     
            DECLARE C-FIND-A-FLIGHT-CURSOR CURSOR                       
            FOR                                                         
            SELECT                                                      
               FLIGHT_ID,                                               
               FLIGHT_NUMBER,                                           
               PLANE_ID,     
               DEPARTURE_AIRPORT_CODE,                               
               ARRIVAL_AIRPORT_CODE,                                 
               DEPARTURE_TIMESTAMP,                                  
               ARRIVAL_TIMESTAMP,                                    
               FLIGHT_STATUS,                                        
               AIRLINE_CODE,                                         
               TIMESTAMPDIFF(4,CHAR(                                 
               TIMESTAMP(CURRENT_TIMESTAMP + 6 HOURS ) -             
               TIMESTAMP(DEPARTURE_TIMESTAMP)))                      
            FROM                                                     
               T05_FLIGHT_TABLE                                      
            WHERE                                                    
               FLIGHT_NUMBER >= :WS-FLIGHT-NUMBER-LOW                
                             AND                                     
               FLIGHT_NUMBER <= :WS-FLIGHT-NUMBER-HIGH               
                                                                     
                             AND                                     
               DEPARTURE_AIRPORT_CODE >= :WS-ORIGIN-AIRPORT-LOW      
                             AND                                     
               DEPARTURE_AIRPORT_CODE <= :WS-ORIGIN-AIRPORT-HIGH     
                             AND                                     
               ARRIVAL_AIRPORT_CODE   >= :WS-DEST-AIRPORT-LOW        
                             AND                                     
               ARRIVAL_AIRPORT_CODE   <= :WS-DEST-AIRPORT-HIGH       
                             AND                                     
               DATE(DEPARTURE_TIMESTAMP) >= :WS-DEPARTURE-DATE-LOW   
                             AND                                     
               DATE(DEPARTURE_TIMESTAMP) <= :WS-DEPARTURE-DATE-HIGH  
                             AND                                     
               DATE(ARRIVAL_TIMESTAMP)   >= :WS-ARRIVAL-DATE-LOW     
                             AND                                     
               DATE(ARRIVAL_TIMESTAMP)   <= :WS-ARRIVAL-DATE-HIGH    
                             AND                                     
               (                                                     
                  FLIGHT_STATUS = :WS-STATUS1 OR                     
                  FLIGHT_STATUS = :WS-STATUS2 OR             
                  FLIGHT_STATUS = :WS-STATUS3 OR                        
                  FLIGHT_STATUS = :WS-STATUS4                           
               )                                                        
                             AND                                        
                  FLIGHT_STATUS <> :CT-DELETED-STATUS                   
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
      *  1. SO-M-FIRST-WITHOUT THIS STATUS MEANS THAT PROGRAM           
      * IS STARTED FOR THE FIRST TIME AND NO DATA WAS GENERETED BY      
      * THE PROGRAM                                                     
      *  2. SO-M-FIRST-WITH MEANS THAT PROGRAM RUNS FOR THE FIRST TIME  
      * BUT IT ALREADY GENERADES SOME DATA ( THIS SCENARIO WILL         
      * HAPPEN WHEN PROGRAM HAVE DISPLAYED THE FLITGHTS AND USER        
      * PRESSED INVALID KEY CONTROL GOES TO Z02141 AND RETURNS TO THIS  
      * PROGRAM AND PROGRAM IS CALLED ONCE AGAIN ( BUT THIS TIME        
      * PROGRAM ALREADY HAS DATA THAT WERE PREVIOUSLY GENERATED) SO     
      * WE JUST TO HAVE DISPLY THIS DATA                                
      *  3.  SO-M-NOT-FIRST  - PROGRAM RUNS BECAUSE USER PRESSED        
      * ATTENTION KEY                                                   
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
               DISPLAY 'SO-M-FIRST-WITH  IN EVALUATE 1005'              
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
            QUEUE(CT-FLIGHT-QUEUE)                                      
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
           PERFORM 2101-SEARCH-THE-FLIGHTS                              
           MOVE 1 TO WS-Z02172-LAST-REC-ID                              
           PERFORM 2115-DISPLAY-NEXT-15                                 
           .                                                            
      ****************************************************************  
      *                     2200-CHECK-EIBRESP                          
      *                                                                 
      ****************************************************************  
       2002-PROCESS-WITH-DATA.                                          
           DISPLAY 'FIRST RECORD ID: '  WS-Z02172-FIRST-REC-ID          
           MOVE WS-Z02172-FIRST-REC-ID TO WS-Z02172-LAST-REC-ID         
                                                                        
           PERFORM 2115-DISPLAY-NEXT-15                                 
           .                                                            
      ****************************************************************  
      *                2003-PROCESS-NOT-FIRST-TIME                      
      * PROGRAM ALLOWS FOR PAGING LOGIC                                 
      *                                                                 
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHPF7                                                  
              PERFORM 2051-DISPLAY-PREV-15                              
           WHEN DFHPF8                                                  
              PERFORM 2115-DISPLAY-NEXT-15                              
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
      *               2023-MOVE-F-QUEUE-TO-SCREEN                       
      * PARAGRAPH MOVES FLIGHT QUEUE DATA TO THE SCREEN VARIABLES       
      ******************************************************************
       2023-MOVE-F-QUEUE-TO-SCREEN.                                     
           MOVE QUEUE-FLIGHT-NUMBER          TO FLIGHT-NUMBERO(WS-ITER3)
           MOVE QUEUE-DEPARTURE-AIRPORT-CODE TO  AIR-ORGO(WS-ITER3)     
           MOVE QUEUE-1-DATE-DEP            TO DEPARTURE-DATEO(WS-ITER3)
           MOVE QUEUE-1-TIME-DEP            TO DEPARTURE-TIMEO(WS-ITER3)
           MOVE QUEUE-ARRIVAL-AIRPORT-CODE   TO  AIR-DESO(WS-ITER3)     
           MOVE QUEUE-1-ARV-DATE             TO  ARRIVAL-DATEO(WS-ITER3)
           MOVE QUEUE-1-ARV-TIME             TO  ARRIVAL-TIMEO(WS-ITER3)
           MOVE QUEUE-AIRLINE-CODE           TO  AIRLINEO(WS-ITER3)     
           MOVE QUEUE-TRANSFER-NUMBER   TO TRANSFER-NUMBERO(WS-ITER3)   
                                                                        
           MOVE QUEUE-FLIGHT-ID     TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3)  
           .                                                            
      ******************************************************************
      *                  2031-RECEIVE-USER-INPUT                        
      ******************************************************************
       2031-RECEIVE-USER-INPUT.                                         
           MOVE LOW-VALUES TO MP0217I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0217') MAPSET('MP0217')                      
            INTO(MP0217I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP  
           .                                                            
      ***************************************************************   
      *                  2032-PROCESS-USER-CHOICE                       
      * PARAGRPAH WILL IDENTIFY ROW WHERE USER PLACED HIS CHOICE        
      ***************************************************************   
           INITIALIZE WS-CHOICE-COUNTER                                 
           INITIALIZE WS-USER-CHOICE-POSITION                           
                                                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 15     
                IF CHOICEI(WS-ITER5) = SPACE OR LOW-VALUES THEN         
                  CONTINUE                                              
                ELSE                                                    
                  ADD 1 TO WS-CHOICE-COUNTER                            
                  MOVE CHOICEI(WS-ITER5) TO SW-USER-CHOICE              
                  MOVE WS-ITER5 TO WS-USER-CHOICE-POSITION              
                END-IF                                                  
           END-PERFORM                                                  
           IF WS-CHOICE-COUNTER = 0 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              IF SO-SEARCH-ONLY-FLG-ID THEN                             
                MOVE 'PLANE AN "X" NEXT TO FLIGHT YOU WANT TO DELETE'   
                             TO WS-Z02141-I-ERROR-MESSAGE(1)            
              ELSE                                                      
                MOVE 'YOU NEED TO SPECIFY SOMETHING , 1 OR 2'           
                             TO WS-Z02141-I-ERROR-MESSAGE(1)            
              END-IF                                                    
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
           DISPLAY 'PONIZEJ WARTOSC TEGO WYBRANEGO: '                   
           DISPLAY WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)       
                                                                        
           IF WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) =          
                        CT-EMPTY-FIELD              
              MOVE 'PLEASE CHOOSE NOT EMPTY LINE '                    
                           TO WS-Z02141-I-ERROR-MESSAGE(1)            
              SET SO-Z02141-M-WITH TO TRUE                            
              PERFORM 2300-CALL-ERROR-ROUTINE                         
           END-IF                                                     
      * IF THIS IS SCENARIO 2                                         
           IF SO-SEARCH-ONLY-FLG-ID THEN                              
             EVALUATE TRUE                                            
             WHEN SO-VALID-CHOICE                                     
                MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)   
                TO T05-FLIGHT-ID-TEXT                                 
                COMPUTE T05-FLIGHT-ID-LEN =                           
                   FUNCTION LENGTH(T05-FLIGHT-ID-TEXT)                
                                                                      
                PERFORM 7011-IF-PART-OF-SCHEDULE                      
                PERFORM 7012-DELETE-FLIGHT-DATA                       
                                                                      
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                 
                MOVE 'TUTAJ WSZYSTKO POWINNO ZOSTAC USUNIETE '        
                           TO WS-Z02141-I-ERROR-MESSAGE(1)            
                SET SO-Z02141-M-WITH TO TRUE                          
                PERFORM 2300-CALL-ERROR-ROUTINE                       
             WHEN OTHER                                               
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                 
                MOVE 'INVALID CHOICE          '                       
                           TO WS-Z02141-I-ERROR-MESSAGE(1)            
                MOVE 'PLACE "X" NEXT TO FLIGHT YOU WANT TO DELETE'    
                           TO WS-Z02141-I-ERROR-MESSAGE(2)            
                SET SO-Z02141-M-WITH TO TRUE                          
                PERFORM 2300-CALL-ERROR-ROUTINE                       
             END-EVALUATE                                             
           ELSE                                                       
      * IF THIS IS SCENARIO 1         
             EVALUATE TRUE                                              
              WHEN  SO-DISPLAY-SEATS                                    
                                                                        
                PERFORM 2610-CALL-TO-DISPALY-SEATS                      
                                                                        
              WHEN  SO-DISPLAY-PASSENGERS-DATA                          
                PERFORM 2620-CALL-TO-DISPLAY-PASS                       
              WHEN OTHER                                                
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'INVALID CHOICE          '                         
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
                MOVE '1 TO DISPLAY SEATS     '                          
                           TO WS-Z02141-I-ERROR-MESSAGE(2)              
                MOVE '2 TO DISPLAY PASSENGERS DATA '                    
                           TO WS-Z02141-I-ERROR-MESSAGE(3)              
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
             END-EVALUATE                                               
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN                      
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
                                                OR   SO-END-OF-QUEUE    
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-REC-ID    
               DISPLAY 'WS-ITER3 = 1 ; FIRST REC: '                     
                     WS-Z02172-FIRST-REC-ID                             
             END-IF                                                     
                                                                        
               PERFORM 2023-MOVE-F-QUEUE-TO-SCREEN                      
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
                                                                        
            IF WS-Z02172-FIRST-REC-ID - 15  >= 1 THEN                   
              SUBTRACT 15 FROM WS-Z02172-FIRST-REC-ID                   
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
             SEND MAP('MP0217') MAPSET('MP0217')                        
             FROM(MP0217O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                2101-SEARCH-THE-FLIGHTS                          
      ****************************************************************  
       2101-SEARCH-THE-FLIGHTS.                                         
           PERFORM 2103-PREPARE-DATA                                    
           PERFORM 7001-OPEN-CURSOR                                     
           PERFORM 7002-FETCH-CURSOR-TO-QUEUE                           
           PERFORM 7003-CLOSE-CURSOR                                    
           .                                                            
      ****************************************************************  
      *                     2103-PREPARE-DATA                           
      * PROGRAM CAN GO INTO TWO PATHS HERE                              
      * 1. WHEN PROGRAM IS CALLED IN ORDER TO DELETE THE FLIGHTS        
      * 2. WHEN PROGRAM IS CALLED IN ORDER TO ALLOW USER TO SEE         
      * PASSENGERS OR TAKEN SEATS                                       
      *                                                                 
      * IF FIRST OPTION IS TRUE THEN WE ARE SEARCH FLIGHTS ONLY         
      * BY FLIGHT-NUMBER                                                
      *                                                                 
      * ELSE WE ARE SEARCHING THRU ALL OTHER FIELDS                     
      *                                                                 
      *                                                                 
      *  THE CODE BELOW WORKS LIKE THIS:                                
      * IF USER PROVIDED FOR EXAMPEL FLIGHT-NUMBER                      
      * THEN FLIGHT FLIGHT-NUMBER WILL BE PUT IN FLIGHT-NUMBER-LOW      
      * AND IN FLIGHT-NUMBER-HIGH                                       
      * THAT WAY OUR CURSOR WILL DISPLAY ONLY FLIGHST THAT HAS THAT     
      * FLIGHT NUMBER                                                   
      * IF USER DIDN'T SPECIFY ANYTHING THEN WE WILL PUT THERE          
      * LOW-VALUES IN FLIGHT-NUMBER-LOW                                 
      * HIGH-VALUESIN FLIGHT-NUMBER-HIGH                                
      * THAT WAY PROGRAM WILL SEARCH ALL FLIGHTS THAT HAS               
      * FLIGHT NUMBER BETWEEN THIS 2 VARIABLES.                         
      *                                                                 
      *                                                                 
      * IF USER DIDNT CHOOSE ANY STATUS IT MEANS WE SHOULD SEARCH       
      * EVERY POSSIBLE STATUS                                           
      *                                                                 
      * IF FOR EXAMPLE HE CHOOSE ONLY 1 STATUS WE WILL SEARCH ONLY FOR  
      * THIS ONE STATUS                                                 
      *                                                                 
      ****************************************************************  
       2103-PREPARE-DATA.                                               
           IF SO-SEARCH-ONLY-FLG-ID THEN                                
      * PARAGRAPH WILL PREPARE DATA FOR SEARCH WITH ONLY FLIGHT NUMBER  
             PERFORM 2301-SEARCH-FLIGHT-NUM-PREP                        
           ELSE                                                         
             PERFORM 2302-PREPERE-FLIGHT-NUMBER                         
             PERFORM 2303-PREPRE-DATE-VALUES                            
             PERFORM 2306-PREPRE-AIRPORT-VALUES                         
                                                                        
             IF SO-CONFIRMED-ST-EMPTY AND                               
                 SO-CANCELED-ST-EMPTY AND                               
                 SO-BOARDING-ST-EMPTY AND                               
                 SO-DEPARTED-ST-EMPTY THEN                              
                 PERFORM 2309-PREPARE-EMPTY-STATUS                      
             ELSE                                                       
      * IF NOT EVERY STATUS IS EMPTY                                    
                 PERFORM 2309-PREPARE-EMPTY-STATUS                      
      *          PERFORM 2310-PREPARE-NON-EMPTY-STATUS                  
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                   2111-MOVE-DATA-TO-QUEUE                       
      ****************************************************************  
       2111-MOVE-DATA-TO-QUEUE.                                         
           MOVE T05-FLIGHT-NUMBER-TEXT TO QUEUE-FLIGHT-NUMBER           
           MOVE T05-FLIGHT-ID-TEXT     TO QUEUE-FLIGHT-ID               
           MOVE T05-DEPARTURE-AIRPORT-CODE                              
                       TO QUEUE-DEPARTURE-AIRPORT-CODE                  
           MOVE T05-ARRIVAL-AIRPORT-CODE                                
                       TO QUEUE-ARRIVAL-AIRPORT-CODE                    
                                                                        
           MOVE T05-AIRLINE-CODE TO QUEUE-AIRLINE-CODE                  
           .                                                            
      ****************************************************************  
      *                   2112-WRITE-THE-QUEUE                          
      ****************************************************************  
       2112-WRITE-THE-QUEUE.                                            
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-FLIGHT-QUEUE)                                     
             FROM(WS-FLIGHT-QUEUE-STRUCTURE)                            
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
             QUEUE(CT-FLIGHT-QUEUE)                                     
             INTO(WS-FLIGHT-QUEUE-STRUCTURE)                            
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
           MOVE LOW-VALUES TO MP0217O                                   
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
             MOVE LOW-VALUES TO CHOICEA(WS-ITER3)                       
             MOVE LOW-VALUES TO FLIGHT-NUMBERA(WS-ITER3)                
             MOVE LOW-VALUES TO AIR-ORGA(WS-ITER3)                      
             MOVE LOW-VALUES TO DEPARTURE-DATEA(WS-ITER3)               
             MOVE LOW-VALUES TO DEPARTURE-TIMEA(WS-ITER3)               
             MOVE LOW-VALUES TO AIR-DESA(WS-ITER3)                      
             MOVE LOW-VALUES TO ARRIVAL-DATEA(WS-ITER3)                 
             MOVE LOW-VALUES TO ARRIVAL-TIMEA(WS-ITER3)                 
             MOVE LOW-VALUES TO AIRLINEA(WS-ITER3)                      
             MOVE LOW-VALUES TO TRANSFER-NUMBERA(WS-ITER3)              
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
      *                   2150-CHECK-IF-TO-OR-FROM                      
      ****************************************************************  
       2150-CHECK-IF-TO-OR-FROM.                                        
           EVALUATE TRUE                                                
             WHEN T05-FLIGHT-NUMBER-TEXT = FLIGHT-NUMBER-TO-TEXT        
                SET SO-THIS-IS-FLIGHT-TO   TO TRUE                      
             WHEN T05-FLIGHT-NUMBER-TEXT = FLIGHT-NUMBER-FROM-TEXT      
                SET SO-THIS-IS-FLIGHT-FROM TO TRUE                      
           END-EVALUATE                                                 
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
      *                    2301-SEARCH-FLIGHT-NUM-PREP                  
      * PARAGRAPH WILL PREPARE DATA FOR SEARCH WITH ONLY FLIGHT         
      * NUMBER                                                          
      *                                                                 
      * THIS PARAGRAPH WILL PREPARE FLIGHT NUMBER TO SEARCH FOR IT      
      *                                                                 
      * IF USER PROVIDED SOMETHING WE WILL SEARCH FOR THAT              
      * IF HE DIDNT WE WONT SEARCH FOR THAT                             
      *                                                                 
      *                                                                 
      * THE RESER OF THE DATA FOR EXMAPLE DATES, AIRPORTS AND STATUSES  
      * WILL BE SET TO SEARCH EVERY POSSIBLE VALUE NON USER VALUE       
      ******************************************************************
       2301-SEARCH-FLIGHT-NUM-PREP.                                     
           PERFORM 2302-PREPERE-FLIGHT-NUMBER                           
           MOVE '1900-01-01' TO WS-ARRIVAL-DATE-LOW                     
           MOVE '2500-12-31' TO WS-ARRIVAL-DATE-HIGH                    
           SET SO-NOT-ARRIVAL-DATE-SEARCH TO TRUE                       
           MOVE '1900-01-01'  TO  WS-DEPARTURE-DATE-LOW                 
           MOVE '2500-12-31'  TO WS-DEPARTURE-DATE-HIGH                 
           SET SO-NOT-DEP-DATE-SEARCH TO TRUE                           
           MOVE 'AAA'         TO  WS-ORIGIN-AIRPORT-LOW                 
           MOVE 'ZZZ'         TO WS-ORIGIN-AIRPORT-HIGH                 
           MOVE 'AAA'         TO  WS-DEST-AIRPORT-LOW                   
           MOVE 'ZZZ'         TO WS-DEST-AIRPORT-HIGH                   
           MOVE 'CONFIRMED'   TO WS-STATUS1-TEXT                        
           MOVE 'CANCELED'    TO WS-STATUS2-TEXT                        
           MOVE 'BOARDING'    TO WS-STATUS3-TEXT                        
           MOVE 'DEPARTED'    TO WS-STATUS4-TEXT                        
           MOVE 9 TO WS-STATUS1-LEN                                     
           MOVE 8 TO WS-STATUS2-LEN                                     
           MOVE 8 TO WS-STATUS3-LEN  
           MOVE 8 TO WS-STATUS4-LEN                                     
           .                                                            
      ******************************************************************
      *                   2302-PREPERE-FLIGHT-NUMBER                    
      ******************************************************************
       2302-PREPERE-FLIGHT-NUMBER.                                      
           IF Z02232-FLIGHT-NUMBER = SPACE OR LOW-VALUES THEN           
              MOVE 'AAAAAAA'            TO WS-FLIGHT-NUMBER-LOW-TEXT    
              MOVE 'ZZZZZZZ'            TO WS-FLIGHT-NUMBER-HIGH-TEXT   
           ELSE                                                         
              MOVE Z02232-FLIGHT-NUMBER TO WS-FLIGHT-NUMBER-LOW-TEXT    
              MOVE Z02232-FLIGHT-NUMBER TO WS-FLIGHT-NUMBER-HIGH-TEXT   
           END-IF                                                       
           COMPUTE WS-FLIGHT-NUMBER-LOW-LEN  =                          
           FUNCTION LENGTH(WS-FLIGHT-NUMBER-LOW-TEXT)                   
           COMPUTE WS-FLIGHT-NUMBER-HIGH-LEN  =                         
           FUNCTION LENGTH(WS-FLIGHT-NUMBER-HIGH-TEXT)                  
           .                                                            
      ******************************************************************
      *                   2303-PREPRE-DATE-VALUES                       
      ******************************************************************
       2303-PREPRE-DATE-VALUES.                                         
           PERFORM 2304-PREPARE-ARRIVAL-DATE                            
           PERFORM 2305-PREPARE-DEPARTURE-DATE                          
           .                                                            
      ******************************************************************
      *                   2304-PREPARE-ARRIVAL-DATE                     
      ******************************************************************
       2304-PREPARE-ARRIVAL-DATE.                                       
           IF Z02232-ARRIVAL-DATE = SPACE OR LOW-VALUES THEN            
              MOVE '1900-01-01' TO WS-ARRIVAL-DATE-LOW                  
              MOVE '2500-12-31' TO WS-ARRIVAL-DATE-HIGH                 
              SET SO-NOT-ARRIVAL-DATE-SEARCH TO TRUE                    
           ELSE                                                         
              MOVE Z02232-ARRIVAL-DATE TO WS-ARRIVAL-DATE-LOW           
              MOVE Z02232-ARRIVAL-DATE TO WS-ARRIVAL-DATE-HIGH         
              SET SO-ARRIVAL-DATE-SEARCH TO TRUE                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2305-PREPARE-DEPARTURE-DATE                   
      ******************************************************************
       2305-PREPARE-DEPARTURE-DATE.                                     
           IF Z02232-DEPARTURE-DATE = SPACE OR LOW-VALUES THEN          
              MOVE '1900-01-01'  TO  WS-DEPARTURE-DATE-LOW              
              MOVE '2500-12-31'  TO WS-DEPARTURE-DATE-HIGH              
              SET SO-NOT-DEP-DATE-SEARCH TO TRUE                        
           ELSE                                                         
              MOVE Z02232-DEPARTURE-DATE TO WS-DEPARTURE-DATE-LOW       
              MOVE Z02232-DEPARTURE-DATE TO WS-DEPARTURE-DATE-HIGH      
              SET SO-DEP-DATE-SEARCH TO TRUE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2306-PREPRE-AIRPORT-VALUES                    
      ******************************************************************
       2306-PREPRE-AIRPORT-VALUES.                                      
           PERFORM 2307-PREPARE-ORIGIN-AIRPORT                          
           PERFORM 2308-PREPARE-DEST-AIRPORT                            
           .                                                            
      ******************************************************************
      *                   2307-PREPARE-ORIGIN-AIRPORT                   
      ******************************************************************
       2307-PREPARE-ORIGIN-AIRPORT.                                     
           IF Z02232-ORIGIN-AIRPORT = SPACE OR LOW-VALUES THEN          
              MOVE 'AAAAAA'    TO  WS-ORIGIN-AIRPORT-LOW                
              MOVE 'ZZZZZZZZ'  TO WS-ORIGIN-AIRPORT-HIGH                
           ELSE                                                         
              MOVE Z02232-ORIGIN-AIRPORT TO WS-ORIGIN-AIRPORT-LOW       
              MOVE Z02232-ORIGIN-AIRPORT TO WS-ORIGIN-AIRPORT-HIGH      
           END-IF                                                       
           .                                       
      ******************************************************************
      *                   2308-PREPARE-DEST-AIRPORT                     
      ******************************************************************
       2308-PREPARE-DEST-AIRPORT.                                       
           IF Z02232-DEST-AIRPORT = SPACE OR LOW-VALUES THEN            
              MOVE 'AAAAAA'    TO  WS-DEST-AIRPORT-LOW                  
              MOVE 'ZZZZZZZZ'  TO WS-DEST-AIRPORT-HIGH                  
           ELSE                                                         
               MOVE Z02232-DEST-AIRPORT TO WS-DEST-AIRPORT-LOW          
               MOVE Z02232-DEST-AIRPORT TO WS-DEST-AIRPORT-HIGH         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2309-PREPARE-EMPTY-STATUS                      
      * IF USER DIDINT CHOOSE ANY STATUS IT MEANS THAT WE WILL          
      * SEARCH EVERY POSSIBLE STATUS,                                   
      * STATUS'S DATA WILL BE PREPARED HERE                             
      ******************************************************************
       2309-PREPARE-EMPTY-STATUS.                                       
           MOVE 'CONFIRMED'  TO WS-STATUS1-TEXT                         
           MOVE 'CANCELED'    TO WS-STATUS2-TEXT                        
           MOVE 'BOARDING'   TO WS-STATUS3-TEXT                         
           MOVE 'DEPARTED' TO WS-STATUS4-TEXT                           
           MOVE 9 TO WS-STATUS1-LEN                                     
           MOVE 8 TO WS-STATUS2-LEN                                     
           MOVE 8 TO WS-STATUS3-LEN                                     
           MOVE 8 TO WS-STATUS4-LEN                                     
           .                                                            
      ******************************************************************
      *                2310-PREPARE-NON-EMPTY-STATUS                    
      * PARAGRAPH WILL MOVE SPACE TO THE STATUS SEARCH VARIABLE IF      
      * WE SHOULDNT LOOK FOR THIS STATUS, OR ITS NAME AND LENGTH OF     
      * NAME IF WE SHOULD                                               
      *                                                                 
      ******************************************************************
       2310-PREPARE-NON-EMPTY-STATUS. 
           IF SO-CONFIRMED-ST-EMPTY THEN                                
              MOVE ' ' TO WS-STATUS1-TEXT                               
              MOVE 1 TO WS-STATUS1-LEN                                  
           ELSE                                                         
              MOVE 'CONFIRMED' TO WS-STATUS1-TEXT                       
              MOVE 9 TO WS-STATUS1-LEN                                  
           END-IF                                                       
           IF SO-CANCELED-ST-EMPTY THEN                                 
              MOVE ' ' TO WS-STATUS2-TEXT                               
              MOVE 1 TO WS-STATUS2-LEN                                  
           ELSE                                                         
              MOVE 'CANCELED' TO WS-STATUS2-TEXT                        
              MOVE 8 TO WS-STATUS2-LEN                                  
           END-IF                                                       
           IF SO-BOARDING-ST-EMPTY THEN                                 
              MOVE ' ' TO WS-STATUS3-TEXT                               
              MOVE 1 TO WS-STATUS3-LEN                                  
           ELSE                                                         
              MOVE 'BOARDING' TO WS-STATUS3-TEXT                        
              MOVE 8 TO WS-STATUS3-LEN                                  
           END-IF                                                       
           IF SO-DEPARTED-ST-EMPTY THEN                                 
              MOVE ' ' TO WS-STATUS4-TEXT                               
              MOVE 1 TO WS-STATUS4-LEN                                  
           ELSE                                                         
              MOVE 'DEPARTED' TO WS-STATUS4-TEXT                        
              MOVE 8 TO WS-STATUS4-LEN                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2311-VALIDATE-DATES                           
      * IF USER SEARCHES THE FLIGHTS BASED ON THE DATES LOGIC IS LIKE   
      * THIS:                                                           
      *                                                                 
      *   FIRST WE WILL GET ALL DATES WITHOUT ANY SEARCHING             
      *  THEN WE WILL MOVE THEM FROM UTC TIMEZONE TO LOCAL AIRPORT      
      * TIMEZONES ( THIS ARE TIMEZONES THAT USER PROVIDES DATES IN)     
      *  THEN WE WILL VALIDATE IF THOSE DATES ARE VALID OR NOT          
      *                                                                 
      *                                                                 
      ******************************************************************
       2311-VALIDATE-DATES.                                             
           SET SO-VALID-DATE   TO TRUE                                  
           IF SO-DEP-DATE-SEARCH THEN                                   
             IF QUEUE-1-DATE-DEP = Z02232-DEPARTURE-DATE THEN           
                SET SO-VALID-DATE   TO TRUE                             
             ELSE                                                       
                SET SO-INVALID-DATE TO TRUE                             
             END-IF                                                     
           END-IF                                                       
                                                                        
           IF SO-ARRIVAL-DATE-SEARCH  AND SO-VALID-DATE THEN            
               IF QUEUE-1-ARV-DATE = Z02232-ARRIVAL-DATE   THEN         
                 SET SO-VALID-DATE   TO TRUE                            
               ELSE                                                     
                 SET SO-INVALID-DATE TO TRUE                            
               END-IF                                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2312-PREPARE-TIME-OFFSET                    
      ******************************************************************
       2312-PREPARE-TIME-OFFSET.                                        
           INITIALIZE WS-HOUR-OFFSET-TEMP                               
           INITIALIZE WS-MINUTE-OFFSET-TEMP                             
           MOVE T02-TIME-ZONE2      TO WS-TEMP-TIMEZONE                 
           MOVE WS-TIMEZONE-HOUR-AND-SIGN TO WS-HOUR-OFFSET-TEMP        
           MOVE WS-TIMEZONE-SIGN          TO WS-MINUTE-OFFSET-TEMP(1:1) 
           MOVE WS-TIMEZONE-MINUTE        TO WS-MINUTE-OFFSET-TEMP(2:2) 
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
           MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) TO       
            Z02192-ONE-WAY-FL-ID(1)                                     
           MOVE 1 TO Z02192-ONE-WAY-FLIGHT-AMOUNT                       
           MOVE 1 TO Z02192-ONE-WAY-TICKET-NUMBER                       
           MOVE 1 TO WS-FLIGHT-COUNTER                                  
           SET SO-ONLY-DISPLAY TO TRUE                                  
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           DISPLAY 'DISPLAY SEATS CALL '                                
           DISPLAY 'WS-Z02172-FIRST-REC-ID ' WS-Z02172-FIRST-REC-ID     
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
           MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) TO       
                 Z02242-FLIGHT-NUMBER                                   
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-DISPLAY-ALL-PASSENGERS TO TRUE                        
           DISPLAY 'DISPLAY PASS CALL '                                 
           DISPLAY 'WS-Z02172-FIRST-REC-ID ' WS-Z02172-FIRST-REC-ID     
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
              PERFORM 3001-RETURN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
              PERFORM 3002-RETURN-TO-CALLING-PROG                       
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)    
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                    3001-RETURN-WITH-TRANSID                     
      * PROGRAM WILL END THE PROGRAM WITH OPTION TO RETRIGGER           
      * IF USER WILL PRESS ATTENTION KEY                                
      ****************************************************************  
       3001-RETURN-WITH-TRANSID.                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           DISPLAY 'RETURN WITH 0218'                                   
           EXEC CICS                                                    
             RETURN TRANSID('0218') COMMAREA(DFHCOMMAREA)               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                  3002-RETURN-TO-CALLING-PROG                    
      * PARAGRAPH RETURNS CONTROL TO CALLING PROGRAM                    
      ****************************************************************  
       3002-RETURN-TO-CALLING-PROG.                                     
           SET SO-M-FIRST-WITH     TO TRUE                              
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                      
             COMMAREA(DFHCOMMAREA)                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                      7001-OPEN-CURSOR                           
      ****************************************************************  
       7001-OPEN-CURSOR.                                                
           DISPLAY '7001 CURSOR PARAMTERS: '                            
           DISPLAY 'FLIGHT-NUMBER-LOW-TEXT: ' WS-FLIGHT-NUMBER-LOW-TEXT 
           DISPLAY 'FLIGHT-NUMBER-LOW-LEN: ' WS-FLIGHT-NUMBER-LOW-LEN   
           DISPLAY 'FLIGHT-NUMBER-H-TEXT: ' WS-FLIGHT-NUMBER-HIGH-TEXT  
           DISPLAY 'FLIGHT-NUMBER-H-LEN: ' WS-FLIGHT-NUMBER-HIGH-LEN    
           DISPLAY 'AIRPORT ORG LOW: ' WS-ORIGIN-AIRPORT-LOW            
           DISPLAY 'AIRPORT ORG H : ' WS-ORIGIN-AIRPORT-HIGH            
           DISPLAY 'DEST AIR LOW : ' WS-DEST-AIRPORT-LOW                
           DISPLAY 'DEST AIR HIGH   : ' WS-DEST-AIRPORT-HIGH            
           DISPLAY 'DEP DATE LOW: '   WS-DEPARTURE-DATE-LOW             
           DISPLAY 'DEP DATE HIGH: '  WS-DEPARTURE-DATE-HIGH            
           DISPLAY 'ARV DATE LOW: '   WS-ARRIVAL-DATE-LOW               
           DISPLAY 'ARV DATE HIGH: '  WS-ARRIVAL-DATE-HIGH              
           DISPLAY 'STATUS1 TEXT: ' WS-STATUS1-TEXT                     
           DISPLAY 'STATUS1 LEN: '  WS-STATUS1-LEN                      
           DISPLAY 'STATUS2 TEXT: ' WS-STATUS2-TEXT                     
           DISPLAY 'STATUS2 LEN: '  WS-STATUS2-LEN                      
           DISPLAY 'STATUS3 TEXT: ' WS-STATUS3-TEXT                     
           DISPLAY 'STATUS3 LEN: '  WS-STATUS3-LEN                      
           DISPLAY 'STATUS4 TEXT: ' WS-STATUS4-TEXT                     
           DISPLAY 'STATUS4 LEN: '  WS-STATUS4-LEN                      
           EXEC SQL                                                     
             OPEN C-FIND-A-FLIGHT-CURSOR                                
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
      * PARAGRAPH WILL FETCH ALL THE FLIGHTS THAT MEETS CRITERIA        
      * THEN WE WILL MOVE THIER DEPARTURE AND ARRIVAL TIMESTAMP TO      
      * LOCAL AIRPORT TIMEZONES                                         
      *                                                                 
      * LATER WE WILL SAVE THAT PREPARED FLIGHT INTO THE QUEUE          
      *                                                                 
      * QUEUE WILL BE LATER USER TO DISPLAY THOSE FLIGHT ON THE         
      * SCREEN AND ALSO IT WILL ALLOW USER TO PAGE THE DATA             
      *                                                                 
      * BEFORE THE LOOP WE WILL SELECT CURRENT TIMESTAMP UTC TIMESTAMP  
      ****************************************************************  
       7002-FETCH-CURSOR-TO-QUEUE.                                      
           MOVE LOW-VALUES TO WS-FLIGHT-QUEUE-STRUCTURE                 
                                                                        
           PERFORM 7004-FETCH-A-FLIGHT-CURSOR                           
           PERFORM 7017-GET-CURRECT-TIMESTAMP                           
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
             PERFORM 7005-CONVERT-UTC-TO-LOCAL-TIME                     
             IF SO-VALID-DATE THEN                                      
               PERFORM 7008-VALIDATE-STATUSES                           
               IF SO-STATUS-VALID THEN                                  
                 PERFORM 2111-MOVE-DATA-TO-QUEUE                        
                 PERFORM 2112-WRITE-THE-QUEUE                           
               END-IF                                                   
             END-IF                                                     
             PERFORM 7004-FETCH-A-FLIGHT-CURSOR                         
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                      7003-CLOSE-CURSOR                          
      ****************************************************************  
       7003-CLOSE-CURSOR.                                               
           EXEC SQL                                                     
             CLOSE C-FIND-A-FLIGHT-CURSOR                               
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
      *                     7004-FETCH-A-FLIGHT-CURSOR                  
      ****************************************************************  
       7004-FETCH-A-FLIGHT-CURSOR.                                      
           SET SO-NOT-END-OF-CURSOR-DATA  TO TRUE                       
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-FLIGHT-NUMBER                                 
           INITIALIZE T05-PLANE-ID                                      
           INITIALIZE T05-DEPARTURE-AIRPORT-CODE                        
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE T05-DEPARTURE-TIMESTAMP                           
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-FLIGHT-STATUS                                 
           INITIALIZE T05-AIRLINE-CODE                                  
           EXEC SQL                                                     
            FETCH C-FIND-A-FLIGHT-CURSOR INTO                           
              :T05-FLIGHT-ID,                                           
              :T05-FLIGHT-NUMBER,                                       
              :T05-PLANE-ID,                                            
              :T05-DEPARTURE-AIRPORT-CODE,                              
              :T05-ARRIVAL-AIRPORT-CODE,                                
              :T05-DEPARTURE-TIMESTAMP,                                 
              :T05-ARRIVAL-TIMESTAMP,                                   
              :T05-FLIGHT-STATUS,                                       
              :T05-AIRLINE-CODE,                                        
              :WS-ILE-MINUT                                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE PO FETCH TO: ' WS-SQLCODE-FORMAT            
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
      *                 7005-CONVERT-UTC-TO-LOCAL-TIME                  
      * PARAGRAPH WILL CONVERT DEPARTURE AND ARRIVAL TIME FROM UTC TO   
      * TIMEZONE LOCAL FOR THE AIRPORT WE WILL USER TIME-ZONE2 VARIABLE 
      * TIME-ZONE2 IS IN FORMAT SHH.MM                                  
      * AND IT STORES OFFSET -> HOW MANY HOURS AND MINUTES WE SHOULD    
      * MOVE OUR TIMEZONE TO GET LOCAL AIRPORT TIME                     
      ******************************************************************
       7005-CONVERT-UTC-TO-LOCAL-TIME.                                  
                                                                        
           MOVE    T05-DEPARTURE-AIRPORT-CODE TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7006-PREPARE-THE-ORG-TIME                            
           MOVE    WS-MODIFIED-TIMESTAMP    TO QUEUE-DEPARTURE-TIMESTAMP
                                                                        
                                                                        
           MOVE    T05-ARRIVAL-AIRPORT-CODE   TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7007-PREPARE-THE-DES-TIME                            
           MOVE    WS-MODIFIED-TIMESTAMP      TO QUEUE-ARRIVAL-TIMESTAMP
                                                                        
      * THIS PARAGRAPH WILL CHECK IF DATES AFTER SWITCHING TIMEZONES    
      * FROM UTC TO LOCAL TIMEZONES FOR THE AIRPORTS ARE VALID WITH     
      * THOSE PROVIDED BY THE USER                                      
      *                                                                 
      * USER PROVIDES DATA IN LOCAL TIMEZONE SO WE NEED TO CHECK        
      * THOSE DATES WITH LOCAL TIEMZONE TIME NON THE UTC                
           PERFORM 2311-VALIDATE-DATES                                  
           .                                                            
      ******************************************************************
      *                      7006-PREPARE-THE-ORG-TIME  
      * PARAGRAPH WILL MOVE UTC TIMESTAMP TO LOCAL TIMEZONE FOR THE     
      * AIRPORT                                                         
      * FOR THAT PROGRAM WILL USER T02-TIME-ZONE2 VARIABLE THAT         
      * STORES AMOUNT OF HOURS AND MINUTES THAT WE SHOULD MOVE TIMESTAMP
      * TO GET VALID TIMEZONE                                           
      ******************************************************************
       7006-PREPARE-THE-ORG-TIME.                                       
           PERFORM 2312-PREPARE-TIME-OFFSET                             
                                                                        
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
      ******************************************************************
       7007-PREPARE-THE-DES-TIME.                                       
           PERFORM 2312-PREPARE-TIME-OFFSET                             
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
               SET SO-7007-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
            END-IF                                                      
           .                                                            
      ******************************************************************
      *                     7008-VALIDATE-STATUSES                      
      ******************************************************************
       7008-VALIDATE-STATUSES.                                          
           SET SO-STATUS-INVALID TO TRUE                                
           IF   SO-CONFIRMED-ST-EMPTY AND                               
                 SO-CANCELED-ST-EMPTY AND                               
                 SO-BOARDING-ST-EMPTY AND                               
                 SO-DEPARTED-ST-EMPTY THEN                              
                 SET SO-STATUS-VALID TO TRUE                            
           ELSE                                                         
      * IF USER WANTS TO SEARCH FRO 'BOARDING FLIGHT '                  
             IF NOT SO-BOARDING-ST-EMPTY THEN                           
                DISPLAY 'NOT SO-BOARDING-ST-EMPTY '                     
                IF T05-FLIGHT-STATUS-TEXT = 'CONFIRMED' THEN            
                  DISPLAY 'T05-FLIGHT-STATU-TEXT CONFIRMED '            
                  PERFORM 7009-CHECK-IF-BOARDING-STATUS                 
                                                                        
                ELSE                                                    
                   DISPLAY 'BORADING INVALID (NOT CONFIRMED ) '         
                   SET SO-STATUS-INVALID TO TRUE 
                END-IF                                                  
             END-IF                                                     
                                                                        
             IF NOT SO-CONFIRMED-ST-EMPTY AND ( NOT SO-STATUS-VALID )   
                DISPLAY 'SO-CONFIRMED NOT EMPTY AND STATUS VALID )   '  
                IF T05-FLIGHT-STATUS-TEXT = 'CONFIRMED' THEN            
                  DISPLAY 'STATUS CONFIRMED (VALID) '                   
                  SET SO-STATUS-VALID TO TRUE                           
                ELSE                                                    
                  DISPLAY 'STATUS CONFIRMED (INVALID) '                 
                  SET SO-STATUS-INVALID TO TRUE                         
                END-IF                                                  
             END-IF                                                     
                                                                        
             IF NOT  SO-DEPARTED-ST-EMPTY  AND ( NOT SO-STATUS-VALID)   
                                                                        
                  PERFORM 7019-CHECK-DEPARTED-STATUS                    
             END-IF                                                     
            IF NOT SO-CANCELED-ST-EMPTY AND ( NOT SO-STATUS-VALID ) THEN
                IF T05-FLIGHT-STATUS-TEXT = 'CANCELED' THEN             
                    SET SO-STATUS-VALID TO TRUE                         
                ELSE                                                    
                    SET SO-STATUS-INVALID TO TRUE                       
                END-IF                                                  
            END-IF                                                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7009-CHECK-IF-BOARDING-STATUS                 
      ******************************************************************
       7009-CHECK-IF-BOARDING-STATUS.                                   
           IF WS-ILE-MINUT > 0 AND <= CT-30-MINUTES THEN                
              DISPLAY '7009 BOARDING VALID '                            
              SET SO-STATUS-VALID   TO TRUE                             
           ELSE                                                         
              DISPLAY '7009 BOARDING INVALID '
              SET SO-STATUS-INVALID TO TRUE                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                7010-FETCH-TIMEZONE                              
      ******************************************************************
       7010-FETCH-TIMEZONE.                                             
           INITIALIZE T02-TIME-ZONE2                                    
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
             CONTINUE                                                   
                                                                        
           WHEN OTHER                                                   
                                                                        
             SET SO-7010-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7011-IF-PART-OF-SCHEDULE                    
      * THIS CURSOR WILL RETURN VALUES IF THIS FLIGHT IS A PART OF      
      * ANY SHEDULED FLIGHTS                                            
      * IF SO WE WILL GET ALL IMPORTANT INFORMATIONS ABOUT THAT         
      ******************************************************************
       7011-IF-PART-OF-SCHEDULE.                                        
           DISPLAY T05-FLIGHT-ID                                        
           DISPLAY T05-FLIGHT-NUMBER                                    
           DISPLAY FLIGHT-NUMBER-TO                                    
           DISPLAY FLIGHT-NUMBER-FROM                                  
           DISPLAY T05-ARRIVAL-TIMESTAMP                               
           DISPLAY T05-DEPARTURE-TIMESTAMP                             
           EXEC SQL                                                    
             SELECT T05.FLIGHT_ID,                                     
                    T05.FLIGHT_NUMBER,                                 
                    FLIGHT_NUMBER_TO,                                  
                    FLIGHT_NUMBER_FROM,                                
                    T05.ARRIVAL_TIMESTAMP,                             
                    T05.DEPARTURE_TIMESTAMP                            
             INTO :T05-FLIGHT-ID,                                      
                  :T05-FLIGHT-NUMBER,                                  
                  :FLIGHT-NUMBER-TO,                                   
                  :FLIGHT-NUMBER-FROM,                                 
                  :T05-ARRIVAL-TIMESTAMP,                              
                  :T05-DEPARTURE-TIMESTAMP                             
             FROM T05_FLIGHT_TABLE T05                                 
             INNER JOIN T10_SCHEDULED_FLIGHTS_TABLE T10 ON             
              T05.FLIGHT_NUMBER = T10.FLIGHT_NUMBER_TO OR              
              T05.FLIGHT_NUMBER = T10.FLIGHT_NUMBER_FROM               
             WHERE FLIGHT_ID = :T05-FLIGHT-ID                          
                    AND                                                
              FLIGHT_STATUS <> :CT-DELETED-STATUS                      
           FETCH FIRST ROW ONLY                                        
           END-EXEC                                                    
           MOVE SQLCODE TO SW-SQLCODE                                  
           EVALUATE TRUE                                               
           WHEN SO-SQLCODE-NORMAL                                      
              SET SO-PART-OF-A-SCHEDULE      TO TRUE                   
           WHEN SO-SQLCODE-NOT-FOUND                                   
              SET SO-NOT-PART-OF-A-SCHEDULE  TO TRUE                   
           WHEN OTHER                                                  
              SET SO-7011-PARA TO TRUE                                 
              PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE   
           .                                                            
      ******************************************************************
      *                     7012-DELETE-FLIGHT-DATA                     
      * IF THE FLIGHT THAT WAS CHOSEN BY THE USER IS A SINGLE ONE       
      * (NOT A PART OF ANY SCHEDULE) PROGRAM WILL DELETE THIS FLIGHT    
      * AND ALL RESERVATIONS ON THAT FLIGHT ( STATUSES WILL BE SET TO   
      * DELETED )                                                       
      *                                                                 
      *                                                                 
      * IF THIS IS PART OF A SCHEDULE PROGRAM WILL CHECK IF             
      * THIS IS FLIGHT "TO" OR "FROM" AND CORRESPONDING FILGHT WILL     
      * BE ALSO DELETED                                                 
      * IF THIS IS FLIGHT "TO" NEXT FLIGHT "FROM" WILL BE DELETED       
      * IF THIS IS FLIGHT "FROM" NEXT FLIGHT "TO" WILL BE DELETED       
      ******************************************************************
       7012-DELETE-FLIGHT-DATA.                                         
           IF SO-NOT-PART-OF-A-SCHEDULE THEN                            
      * THIS PARAGRAPHS UPDATES BOTH TABLES BASED ON FLIGHT_ID          
               PERFORM 7013-UPDATE-T05-TABLE                            
               PERFORM 7014-UPDATE-T04-TABLE                            
           ELSE                                                         
      * DISPLAY THIS FLIGHT IS PART OF A SCHEDULE                       
      * WE NEED TO DETERMINE IF THIS IS FLIGHT "TO" OR "FROM"           
      **************************************************************    
      * AT THE BEGINING PROGRAM WILL DELETE FLIGHT CHOSEN BY THE USER   
      * LATER IT WILL DELETE CORESPONDING FLIGHT "TO" OR "FROM"         
              PERFORM 7013-UPDATE-T05-TABLE                             
              PERFORM 7014-UPDATE-T04-TABLE                             
                                                                        
              PERFORM 2150-CHECK-IF-TO-OR-FROM                          
              IF SO-THIS-IS-FLIGHT-FROM THEN                            
                INITIALIZE T05-FLIGHT-ID                                
                DISPLAY 'PIERWSZY SELECT '                              
                DISPLAY 'FLIGHT-NUMER-TO TEXT: ' FLIGHT-NUMBER-TO-TEXT  
                DISPLAY 'FLIGHT-NUMER-TO LEN : ' FLIGHT-NUMBER-TO-LEN   
                DISPLAY 'DEP-TIMESTAMP: '   T05-DEPARTURE-TIMESTAMP     
                PERFORM 7016-GET-CORR-TO-FLIGHT                         
                PERFORM 7013-UPDATE-T05-TABLE                           
                PERFORM 7014-UPDATE-T04-TABLE                           
              ELSE                                                      
      * SO THIS IS FLIGHT "TO"                                          
                INITIALIZE T05-FLIGHT-ID                                
                DISPLAY 'PIERWSZY SELECT '                              
                DISPLAY 'FLIGHT-N-FROM TEXT: ' FLIGHT-NUMBER-FROM-TEXT  
                DISPLAY 'FLIGHT-N-FROM LEN : ' FLIGHT-NUMBER-FROM-LEN   
                DISPLAY 'ARV-TIMESTAMP: '   T05-ARRIVAL-TIMESTAMP       
                PERFORM 7015-GET-CORR-FROM-FLIGHT                       
                PERFORM 7013-UPDATE-T05-TABLE                           
                PERFORM 7014-UPDATE-T04-TABLE                           
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7013-UPDATE-T05-TABLE                       
      ***************************************************************** 
       7013-UPDATE-T05-TABLE.                                           
           DISPLAY '7013 FLIGHT_ID: ' T05-FLIGHT-ID                     
           EXEC SQL                                                     
            UPDATE T05_FLIGHT_TABLE                                     
             SET FLIGHT_STATUS = :CT-DELETED-STATUS                     
            WHERE FLIGHT_ID = :T05-FLIGHT-ID                            
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7013-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           ELSE                                                         
             DISPLAY '7013 SUCCESS '                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7014-UPDATE-T04-TABLE       
      ***************************************************************** 
       7014-UPDATE-T04-TABLE.                                           
           DISPLAY '7014 FLIGHT_ID: ' T05-FLIGHT-ID                     
           EXEC SQL                                                     
            UPDATE T04_FLIGHT_SEATS                                     
             SET RESERVATION_STATUS = :CT-DELETED-STATUS                
             WHERE FLIGHT_ID = :T05-FLIGHT-ID                           
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
             WHEN SO-SQLCODE-NORMAL                                     
             WHEN SO-SQLCODE-NOT-FOUND                                  
               CONTINUE                                                 
             WHEN OTHER                                                 
               SET SO-7014-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7015-GET-CORR-FROM-FLIGHT                     
      ***************************************************************** 
       7015-GET-CORR-FROM-FLIGHT.                                       
           EXEC SQL                                                     
             SELECT FLIGHT_ID                                           
              INTO :T05-FLIGHT-ID                                       
             FROM T05_FLIGHT_TABLE                                      
             WHERE FLIGHT_NUMBER = :FLIGHT-NUMBER-FROM                  
                              AND                                       
             DEPARTURE_TIMESTAMP > :T05-ARRIVAL-TIMESTAMP               
                              AND                                       
             FLIGHT_STATUS <> :CT-DELETED-STATUS                        
             ORDER BY DEPARTURE_TIMESTAMP ASC                           
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              DISPLAY 'THIS WAS "TO" FLIGHT, DELETION OF HIS FROM'      
               ' FAILED'                                                
              SET SO-7012-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7016-GET-CORR-TO-FLIGHT                        
      ***************************************************************** 
       7016-GET-CORR-TO-FLIGHT.                                         
           EXEC SQL                                                     
              SELECT FLIGHT_ID                                          
              INTO  :T05-FLIGHT-ID                                      
             FROM T05_FLIGHT_TABLE                                      
            WHERE FLIGHT_NUMBER = :FLIGHT-NUMBER-TO                     
            AND                                                         
            TIMESTAMP(:T05-DEPARTURE-TIMESTAMP) > ARRIVAL_TIMESTAMP     
            AND                                                         
            FLIGHT_STATUS <> :CT-DELETED-STATUS                         
            ORDER BY ARRIVAL_TIMESTAMP DESC                             
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7012-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7017-GET-CURRECT-TIMESTAMP                    
      ******************************************************************
       7017-GET-CURRECT-TIMESTAMP.                                      
           EXEC SQL                                                     
              SELECT CURRENT_TIMESTAMP +  6 HOURS                       
                 INTO :WS-CURRENT-TIMESTAMP                             
              FROM T05_FLIGHT_TABLE      
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7017-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7019-CHECK-DEPARTED-STATUS                  
      * PARAGRAPH WILL CHECK IF TIME OF ARRIVAL ALREADY HAPPEND         
      *                                                                 
      ***************************************************************** 
       7019-CHECK-DEPARTED-STATUS.                                      
           EXEC SQL                                                     
             SELECT "A"                                                 
             INTO :WS-DUMMY                                             
             FROM T05_FLIGHT_TABLE                                      
             WHERE                                                      
              :T05-ARRIVAL-TIMESTAMP < :WS-CURRENT-TIMESTAMP            
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              SET SO-STATUS-VALID TO TRUE                               
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-STATUS-INVALID TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7019-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       9000-DB2-ERROR                            
      ***************************************************************** 
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
           .                                    
                               
                

                                             
                          
                       
                                                     
                
                      
                                    

                                  
                     
 
                                   
                                         
                                     
                                     

                             
                                
                          
                                                                      
                    


                                 
                       
                     
                                


        
                                           
                     
                                  
