       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02261.                                              
      ******************************************************************
      *    PROGRAM WILL DISPLAY FLIGHTS THAT ARE IN GIVEN RESERVATION   
      *                                                                 
      *   ON THIS SCREEN USER CAN USER PAGINING LOGIC (F7 AND F8)       
      *                                                                 
      *  USER CAN PRESS F3 AND GO BACK TO PREVIOUS (CALLING) PROGRAM    
      *                                                                 
      * USER CAN ALSO DELETE RESERVATION BY PLACING 'X' ON THE TOP      
      * OF THE SCREEN IN LABALED FIELDS                                 
      *                                                                 
      *  USER CAN ALSO PLANE '1' OR '2' NEXT TO FLIGHT DATA             
      *                                                                 
      * AFTER PRESSING ENTER                                            
      *   IF USER CHOOSE '1' PROGRAM Z02192 WILL BE CALLED AND USER     
      *    WILL SEE GRAFICAL REPRESENATION OF THE SEATS                 
      *   (HIS SEATS WILL BE MARKED BY 'R' SYMBOL )                     
      *                                                                 
      *   IF USER CHOOSE '2' THEN PROGRAM Z02242 WILL BE CALLED         
      *   AND USER WILL SEE LIST OF PASSENGERS IN THIS RESERVATION      
      *                                                                 
      * DATA TO THIS QUEUE IS PASSED BY THE QUEUE                       
      *   QUEUE NAME ALONG WITH ITEM NUMBER OF A SPECIFIC ROW WILL      
      *   BE PROVIDED BY CALLING PROGRAM (Z02252)                       
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0226.                                               
           COPY ZZEC0215.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
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
           05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02252  '.        
           05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02261  '.        
           05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.        
           05 CT-QUEUE-NAME           PIC X(8) VALUE '       '.         
           05 CT-DISPLAY-PASS-PROG    PIC X(8) VALUE 'Z02242  '.        
           05 CT-DISPLAY-SEATS-PROG   PIC X(8) VALUE 'Z02192  '.        
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
           05 SW-IF-DROP-RESERVATION                        PIC X.      
              88 SO-DROP-RESERVATION                        VALUE 'X'.  
              88 SO-EMPTY-RESERVATION-DROP VALUE LOW-VALUES SPACE '_'.  
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
              88 SO-DISPLAY-SEATS                           VALUE '1'.  
              88 SO-DISPLAY-PASSENGERS-DATA                 VALUE '2'.  
       01 WS-VARIABLES.                                                 
           05 WS-RESERVATION-ID                 PIC S9(9) COMP VALUE 0. 
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
           05 CT-EMPTY-FIELD         PIC X(15) VALUE 'XXXXXXXXXXXXXXX'. 
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0.
           05 WS-WORKING-TIMESTAMP.                                     
              15 WS-DATE.
               25 WS-WORK-YEAR   PIC 9(4).                              
               25 FILLER       PIC X VALUE '-'.                         
               25 WS-WORK-MONTH    PIC 9(2).                            
               25 FILLER       PIC X VALUE '-'.                         
               25 WS-WORK-DAY      PIC 9(2).                            
              15 FILLER       PIC X VALUE '-'.                          
              15 WS-TIME.                                               
               25 WS-WORK-HOUR     PIC 9(2).                            
               25 FILLER      PIC X VALUE '.'.                          
               25 WS-WORK-MINUTE   PIC 9(2).                            
              15 FILLER       PIC X VALUE '.'.                          
              15 WS-WORK-SECOND   PIC 9(2).                             
              15 FILLER       PIC X VALUE '.'.                          
              15 WS-WORK-MICROSEC PIC 9(6).                             
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
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02261-----------START----------'                   
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02261-----------END------------'                   
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
      * PROGRAM CAN HAVE 2 MODES                                        
      *                                                                 
      *   1. PROGRAM IS RUN FOR THE FIRST TIME AND IT NEEDS TO          
      *   DISPLAY FLIGHT DATA                                           
      *                                                                 
      *   2. PROGRAM IS CALLED BY THE USER BY PRESSING ATTENTION KEY    
      *                                                                 
      *                                                                 
      *  PROGRAM GETS DATA THROUGH A QUEUE ( IT GETS QUEUE NAME AND     
      * ITEM NUMBER OF A SPECIFIC ROW IN THIS QUEUE)                    
      *                                                                 
      ******SO-M-FIRST-WITH******************************************** 
       1005-CHECK-IF-FIRST-TIME.                                        
           INITIALIZE WS-ZZEC0215                                       
                                                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
                                                                        
           MOVE Z02261-I-QUEUE-NAME TO CT-QUEUE-NAME                    
           EVALUATE TRUE                                                
             WHEN SO-M-FIRST-WITHOUT                                    
               PERFORM 1010-CICS-IGNORE                                 
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
      *                          2000-PROCESS                           
      * PROGRAM WILL CALL TO PARAGRAPHS DEPENDING ON IN WHAT MODE       
      * IT CURRENLY IS                                                  
      *                                                                 
      *  FLIGHTS DATA CAN BE DISPLAYED TO THE SCREEN                    
      *                                                                 
      * OR USER INPUT CAN BE RECEIVED AND VALIDATED                     
      ******************************************************************
       2000-PROCESS.                                                    
           EVALUATE TRUE                                                
           WHEN SO-PROGRAM-RUNS-FIRST-TIME                              
           WHEN SO-PROGRAM-RUNS-WITH-DATA                               
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2001-PROCESS-FIRST-TIME                          
           WHEN SO-PROGRAM-RUNS-NOT-FIRST-TIME                          
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2003-PROCESS-NOT-FIRST-TIME                      
                                                                        
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
      *                          2000-PROCESS                           
      * PROGRAM WILL CALL TO PARAGRAPHS DEPENDING ON IN WHAT MODE       
      * IT CURRENLY IS                                                  
      *                                                                 
      *  FLIGHTS DATA CAN BE DISPLAYED TO THE SCREEN                    
      *                                                                 
      * OR USER INPUT CAN BE RECEIVED AND VALIDATED                     
      ******************************************************************
       2000-PROCESS.                                                    
           EVALUATE TRUE                                                
           WHEN SO-PROGRAM-RUNS-FIRST-TIME                              
           WHEN SO-PROGRAM-RUNS-WITH-DATA                               
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2001-PROCESS-FIRST-TIME                          
           WHEN SO-PROGRAM-RUNS-NOT-FIRST-TIME                          
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2003-PROCESS-NOT-FIRST-TIME                      
                                                                        
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE   
               MOVE 'SERIOUS ERROR IN Z02261' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  2001-PROCESS-FIRST-TIME                        
      ****************************************************************  
       2001-PROCESS-FIRST-TIME.                                         
           DISPLAY ' Z02261-I-RECORD-ID ' Z02261-I-RECORD-ID            
           MOVE Z02261-I-RECORD-ID TO WS-WHAT-RECORD-TO-READ            
           PERFORM 2101-DISPLAY-THE-FLIGHTS                             
           .                                                            
      ****************************************************************  
      *                2003-PROCESS-NOT-FIRST-TIME                      
      * PARAGRAPH IS CALLED WHEN THIS PROGRAM RUNS BECAUSE OF THE       
      * FACT THAT USER PRESSED ANTENTION KEY                            
      *                                                                 
      * IF USER WILL PRESS ENTER THEN PROGRAM WILL GET AND VALIDATE     
      * HIS CHOICE                                                      
      *                                                                 
      * IF USER PRESSED F3 THEN CONTROL WILL BE RETURNED TO CALLING     
      * PROGRAM                                                         
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
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
      *                  2031-RECEIVE-USER-INPUT                        
      ******************************************************************
       2031-RECEIVE-USER-INPUT.                                         
           MOVE LOW-VALUES TO MP0226I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0226') MAPSET('MP0226')                      
            INTO(MP0226I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              MOVE RESDROI TO SW-IF-DROP-RESERVATION                    
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO CHOOSE SOMETHING ' TO                   
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
              PERFORM 2200-CHECK-EIBRESP                                
           END-EVALUATE                                                 
           .                                                            
      ***************************************************************   
      *                  2032-PROCESS-USER-CHOICE                       
      * HERE USER CAN DELETE THE RESERVATION OR DISPLAY                 
      * GRAFICAL REPRESERNTATION OF THE SEATS  OR                       
      * DISPLAY LIST OF PASSENGERS IN GIVEN FLIGHT IN GIVEN             
      * RESERVATION                                                     
      ***************************************************************   
       2032-PROCESS-USER-CHOICE.
           INITIALIZE WS-CHOICE-COUNTER                                 
           INITIALIZE WS-USER-CHOICE-POSITION                           
           PERFORM 2301-RESERVATION-DROP-CHECK                          
           PERFORM 2302-GET-USER-CHOICE-POS                             
           PERFORM 2303-CHECK-CHOICE-NUMBER                             
           PERFORM 2304-CHECK-IF-EMPTY-LINE                             
                                                                        
      * IF USER PLACED '1' THEN FIRST  OPTION IS TRUE                   
      * IF USER PLACED '2' THEN SECOND OPTION IS TRUE                   
           EVALUATE TRUE                                                
            WHEN  SO-DISPLAY-SEATS                                      
              PERFORM 2610-CALL-TO-DISPALY-SEATS                        
            WHEN  SO-DISPLAY-PASSENGERS-DATA                            
              PERFORM 2620-CALL-TO-DISPLAY-PASS                         
            WHEN OTHER                                                  
              PERFORM 2305-SEND-INVALID-CHOICE-MSG                      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN                      
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
                                                OR   SO-END-OF-QUEUE    
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-REC-ID    
             END-IF                                                     
                                                                        
      *        PERFORM 2023-MOVE-QUEUE-1-TO-SCREEN                      
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-REC-ID     
               ADD 1 TO WS-WHAT-RECORD-TO-READ                          
               PERFORM 2116-READ-THE-QUEUE                              
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                  2097-GET-RESERVATION-ID                        
      * THIS PARAGRAPH WILL READ DATA PROVIDED BY THE CALLING PROGRAM   
      *  (BY DOING SO PROGRAM WILL GET ALL NEEDED DATA FROM THE QUEUE)  
      *                                                                 
      * ITEM NUMBER -> NUMBER THAT IDENTIFIES THE ROW AND               
      * QUEUE NAME WERE PROVIDED BY CALLING PROGRAM                     
      ****************************************************************  
       2097-GET-RESERVATION-ID.                                         
           MOVE Z02261-I-RECORD-ID TO WS-WHAT-RECORD-TO-READ            
           MOVE Z02261-I-QUEUE-NAME TO CT-QUEUE-NAME                    
           PERFORM 2116-READ-THE-QUEUE                                  
           PERFORM 2117-CHECK-IF-QIDERR                                 
           .                                                            
      ****************************************************************  
      *                  2098-CHECK-IF-RESERV-VALID                     
      * PARAGRAPH CHECKS IF GIVEN RESERVATION IS VALID NUMBER           
      ****************************************************************  
       2098-CHECK-IF-RESERV-VALID.                                      
           DISPLAY 'QUEUE RESERVATION ID BEFORE TEST NUMVAL: '          
                            QUEUE-RESERVATION-ID                        
           IF FUNCTION TEST-NUMVAL(QUEUE-RESERVATION-ID) = 0 THEN       
             CONTINUE                                                   
           ELSE                                                         
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                      
             MOVE ' RESERVAITON ID IS INVALID ERROR  ' TO               
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
             SET SO-Z02141-M-WITH TO TRUE                               
             SET SO-GO-BACK-TO-PREVIOUS  TO TRUE                        
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           COMPUTE WS-RESERVATION-ID =                                  
             FUNCTION NUMVAL(QUEUE-RESERVATION-ID)                      
           .                                                            
      ****************************************************************  
      *                  2099-DELETE-RESERVATION                        
      ****************************************************************  
       2099-DELETE-RESERVATION.      
           DISPLAY ' 2099 : QUEUE-RESERVATION-ID '                      
                        QUEUE-RESERVATION-ID                            
           PERFORM 2098-CHECK-IF-RESERV-VALID                           
           PERFORM 7001-DELETE-RESERVATION                              
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'RESERVATION DROPPED     ' TO                           
              WS-Z02141-I-ERROR-MESSAGE(1)                              
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           SET SO-GO-BACK-TO-PREVIOUS TO TRUE                           
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ****************************************************************  
      *                      2100-SEND-THE-MAP                          
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0226') MAPSET('MP0226')                        
             FROM(MP0226O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                2101-DISPLAY-THE-FLIGHTS                         
      * WE GOT ITEM NUMBER ( OF RECORD IN THE QUEUE ) FROM CALLING      
      * PROGRAM                                                         
      * IN CASE THAT THERE IS ITEMERR (ROW DON'T EXIST ) OR             
      * QIDERR ( QUEUE DONT EXIST) PROGRAM WILL CALL TO Z02141 PROGRAM  
      * WITH PROPER MESSAGE                                             
      ****************************************************************  
       2101-DISPLAY-THE-FLIGHTS.                                        
           DISPLAY '2101 PERFORMED'                                     
           PERFORM 2119-INITIALIZE-MAP                                  
           PERFORM 2120-INITIALIZE-WHAT-F-NUM                           
           DISPLAY '2101 AFTER INITIALIZE  '                            
           PERFORM 2116-READ-THE-QUEUE         
           DISPLAY '2101 AFTER READ        '                            
           PERFORM 2117-CHECK-IF-QIDERR                                 
           DISPLAY '2101 AFTER QIDERR CHECK '                           
           PERFORM 2103-PREPARE-DATA                                    
           DISPLAY '2101 AFTER PREPARE DATA '                           
           PERFORM 2100-SEND-THE-MAP                                    
           DISPLAY '2101 AFTER SEND THE MAP '                           
           .                                                            
      ****************************************************************  
      *                     2103-PREPARE-DATA                           
      * PARAGRAPH WILL MOVE FLIGHT DATA  FROM THE QUEUE  TO THE         
      * SCREEN VARIABLES                                                
      ****************************************************************  
       2103-PREPARE-DATA.                                               
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL                   
                            WS-ITER1 > QUEUE-FLIGHT-AMOUNT              
              MOVE QUEUE-FLIGHT-ID(WS-ITER1) TO                         
                                   FLIGHT-NUMBERO(WS-ITER1)             
              MOVE QUEUE-ORIGIN-AIRPORT-CODE(WS-ITER1) TO               
                                 AIR-ORGO(WS-ITER1)                     
              MOVE QUEUE-DEST-AIRPORT-CODE(WS-ITER1)   TO               
                                 AIR-DESO(WS-ITER1)                     
              MOVE QUEUE-DEPARTURE-TIMESTAMP(WS-ITER1) TO               
                       WS-WORKING-TIMESTAMP                             
              MOVE WS-DATE TO DEPARTURE-DATEO(WS-ITER1)                 
              MOVE WS-TIME TO DEPARTURE-TIMEO(WS-ITER1)                 
                                                                        
              MOVE QUEUE-ARRIVAL-TIMESTAMP(WS-ITER1) TO                 
                       WS-WORKING-TIMESTAMP                             
              MOVE WS-DATE TO ARRIVAL-DATEO(WS-ITER1)                   
              MOVE WS-TIME TO ARRIVAL-TIMEO(WS-ITER1)                   
              MOVE QUEUE-FLIGHT-ID(WS-ITER1) TO                         
                      WS-WHAT-FLIGHT-NUMBER(WS-ITER1)                   
              MOVE QUEUE-RESERVATION-ID TO Z02242-I-RESERVATION-ID      
              MOVE QUEUE-RESERVATION-ID TO RESNUMO                      
           END-PERFORM             
           .                                                            
      ****************************************************************  
      *                   2112-WRITE-THE-QUEUE                          
      ****************************************************************  
       2112-WRITE-THE-QUEUE.                                            
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-QUEUE-NAME)                                       
             FROM(WS-QUEUE-1-STRUCTURE)                                 
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                      2116-READ-THE-QUEUE                        
      ****************************************************************  
       2116-READ-THE-QUEUE.                                             
           INITIALIZE WS-QUEUE-1-STRUCTURE                              
           DISPLAY 'WARTOSC W READ Z02261-I-RECORD-ID'                  
                                    Z02261-I-RECORD-ID                  
           DISPLAY 'CT-QUEUE-NAME ' CT-QUEUE-NAME                       
           EXEC CICS                                                    
           READQ TS                                                     
             QUEUE(CT-QUEUE-NAME)                                       
             INTO(WS-QUEUE-1-STRUCTURE)                                 
             ITEM(Z02261-I-RECORD-ID)                                   
             NOHANDLE                                                   
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              DISPLAY 'READ NORMAL '                                    
           WHEN DFHRESP(ITEMERR)                                        
              DISPLAY 'READ ITEM ERROR '                                
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
           MOVE LOW-VALUES TO MP0226O                                   
             MOVE LOW-VALUES TO RESNUMA                                 
             MOVE LOW-VALUES TO RESDROA                                 
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
             MOVE LOW-VALUES TO CHOICEA(WS-ITER3)                       
             MOVE LOW-VALUES TO FLIGHT-NUMBERA(WS-ITER3)                
             MOVE LOW-VALUES TO AIR-ORGA(WS-ITER3)                      
             MOVE LOW-VALUES TO DEPARTURE-DATEA(WS-ITER3)               
             MOVE LOW-VALUES TO DEPARTURE-TIMEA(WS-ITER3)               
             MOVE LOW-VALUES TO AIR-DESA(WS-ITER3)                      
             MOVE LOW-VALUES TO ARRIVAL-DATEA(WS-ITER3)                 
             MOVE LOW-VALUES TO ARRIVAL-TIMEA(WS-ITER3)                 
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
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
                                                                        
           MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM     
           SET SO-Z02141-M-WITH TO TRUE                                 
           IF SO-GO-BACK-TO-PREVIOUS THEN                               
             MOVE CT-CALLING-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM
             SET SO-Z02141-M-WITHOUT TO TRUE                            
           END-IF                                                       
           SET  SO-Z02141-I-FIRST-TIME TO TRUE                          
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   2301-RESERVATION-DROP-CHECK                   
      * PARAGRAPH WILL CHECK IF PROGRAM SHOULD DELETE THE RESERVATION   
      *                                                                 
      ******************************************************************
       2301-RESERVATION-DROP-CHECK.                                     
           IF SO-DROP-RESERVATION THEN                                  
      * IF IN FIELD ON SECOND ROW ON THE SCREEN USER PLACED 'X'         
      * IT INDICATES THAT HE WANTS TO DELETE THE RESERVATION            
              PERFORM 2097-GET-RESERVATION-ID                           
              PERFORM 2099-DELETE-RESERVATION                           
           ELSE                                                         
      * IF RESERVATION DROP FIELD WILL BE EQUAL TO '_' SPACE OR         
      * LOW VALUES THEN WE WILL NOT DO ANYTHING                         
      * PROGRAM WILL CONSIDER IT TO BE EMPTY FIELD                      
             IF SO-EMPTY-RESERVATION-DROP THEN                          
                CONTINUE                                                
             ELSE                                                       
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'TO DROP A RESERVATION PLACE X AT THE TOP OF SCREEN '
                                                                        
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2302-GET-USER-CHOICE-POS                    
      ******************************************************************
       2302-GET-USER-CHOICE-POS.                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 15     
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
      *                   2303-CHECK-CHOICE-NUMBER                      
      * PARAGRPAH WILL CHECK IF USER PLACED ONLY 1 CHOICE               
      * IF THIS NUMBER IS GREATER OR SMALLER THEN USER WILL GET         
      * PROPER ERROR MESSAGE                                            
      ******************************************************************
       2303-CHECK-CHOICE-NUMBER.                                        
           IF WS-CHOICE-COUNTER = 0 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO SPECIFY SOMETHING , 1 OR 2'             
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
      *                     2304-CHECK-IF-EMPTY-LINE                    
      * PARAGRAPH WILL CHECK IF USER PLACED HIS CHOICE NEXT TO          
      * EMPTY LINE OR NOT                                               
      ******************************************************************
       2304-CHECK-IF-EMPTY-LINE.                                        
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
      *                   2305-SEND-INVALID-CHOICE-MSG                  
      ******************************************************************
       2305-SEND-INVALID-CHOICE-MSG.                                    
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'INVALID CHOICE          '                              
                        TO WS-Z02141-I-ERROR-MESSAGE(1)                 
           MOVE '1 TO DISPLAY SEATS     '                               
                        TO WS-Z02141-I-ERROR-MESSAGE(2)                 
           MOVE '2 TO DISPLAY PASSENGERS DATA '  
                        TO WS-Z02141-I-ERROR-MESSAGE(3)                 
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
      * PROGRAM CALLS TO Z02192 PROGRAM ( SEATS PROGRAM)                
      ******************************************************************
       2610-CALL-TO-DISPALY-SEATS.                                      
           MOVE 1 TO WS-FLIGHT-COUNTER                                  
           SET SO-ONLY-DISPLAY-RESERV  TO TRUE                          
           SET SO-M-FIRST-WITHOUT TO TRUE                               
                                                                        
           MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) TO       
                 Z02242-FLIGHT-NUMBER                                   
                                                                        
                                                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-DISPLAY-SEATS-PROG)                         
                 COMMAREA(DFHCOMMAREA)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2620-CALL-TO-DISPLAY-PASS                     
      * PROGRAM CALLS TO Z02242 ( DISPLAY PASSENGERS PROGRAM )          
      ******************************************************************
       2620-CALL-TO-DISPLAY-PASS.                                       
           MOVE WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) TO       
                 Z02242-FLIGHT-NUMBER                                   
           DISPLAY 'W Z02261 FLIGHT NUM: ' Z02242-FLIGHT-NUMBER         
           DISPLAY 'W Z02261  RESERV ID: ' Z02242-I-RESERVATION-ID      
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-DISPLAY-WITH-RESERV  TO TRUE                          
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
      ******************************************************************
      *                    3001-RETURN-WITH-TRANSID                     
      * PARAGRAPH WILL END PROGRAM WITH OPTION TO RETRIGGER IF          
      * USER WILL PRESSE ANY ANTTENTION KEY                             
      ******************************************************************
       3001-RETURN-WITH-TRANSID.                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           DISPLAY 'RETURN WITH 0221'                                   
           EXEC CICS                                                    
            RETURN TRANSID('0221') COMMAREA(DFHCOMMAREA)                
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     3002-RETURN-TO-CALLING-PROG                 
      * PROGRAM RETURNS CONTROL TO CALLING PROGRAM                      
      ******************************************************************
       3002-RETURN-TO-CALLING-PROG.                                     
           SET SO-M-FIRST-WITH  TO TRUE                                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                      
              COMMAREA(DFHCOMMAREA)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     7001-DELETE-RESERVATION                     
      ******************************************************************
       7001-DELETE-RESERVATION.                                         
           EXEC SQL                                                     
             UPDATE T04_FLIGHT_SEATS                                    
              SET RESERVATION_STATUS = :CT-DELETED-STATUS               
              WHERE RESERVATION_ID = :WS-RESERVATION-ID                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
            THEN                                                        
             SET SO-7001-PARA TO TRUE                                   
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
                                                     

                       
           
                       
                   
                                                
                                     
                         
                                  
                                        
     
                 
                
                                        
                                               

