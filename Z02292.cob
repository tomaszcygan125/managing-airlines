       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02292.                                              
      ******************************************************************
      *                                                                *
      *                        TRANSACTION 0224                        *
      * PROGRAM IS USED TO ADD A SINGLE FLIGHT OR                      *
      * TO ADD A SCHEDULE FLIGHT                                       *
      *                                                                *
      * IF THE FIRST OPTION IS TRUE THEN PROGRAM WILL DISPLAY MP0229   *
      * MAP                                                            *
      * THERE USER WILL HAVE TO PROVIDE ALL DATA THAT WILL DESCRIBE    *
      * THIS FLIGHT                                                    *
      * (TIME HE WILL PROVIDE WILL BE GIVEN IN LOCAL AIRPORT TIME )    *
      *                                                                *
      * AFTER PRESSING ENTER VALIDATION WILL BE MADE                   *
      * IF EVERYTHING WILL BE CORRECT THEN THE FLITHT WILL BE ADDED    *
      * AND PROPER MESSAGE WILL BE DISPLAYED                           *
      *                                                                *
      * IF THE SECOND OPTION IS TRUE                                   *
      * THEN MP0230 MAP WILL BE SENT                                   *
      * HERE USER WILL HAVE TO PROVIDE ALL DATA AS WELL                *
      *   IN THE FIRST SCREEN HE WILL HAVE TO PROVIDE DATA FOR FLIGHTS *
      *  "TO" AFTER PRESSING ENTER AND SUCCESSFULL VALIDATION          *
      * THIS MAP WILL BE SENT SECOND TIME BUT KNOW LARGE PART OF THAT  *
      * WILL BE SET TO PROTECTED ( USER WILL HAVE TO ONLY SPECIFY TIME *
      * OF DEPARTURE, ARRIVAL AND DAYS OF WEEK WHEN THIS FLIGHT SHOULD *
      * BE ) AFTER PRESSING ENTER ALL FLIGHTS "TO" AND "FROM" WILL     *
      * BE INSERTED INTO THE DATABASE                                  *
      *                                                                *
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0228.                                               
           COPY ZZMP0229.                                               
           COPY ZZMP0230.                                               
           COPY ZZEC0215.                                            
           COPY ZZEC0243.                                            
           COPY DFHBMSCA.                                            
                                                                     
           EXEC SQL INCLUDE SQLCA END-EXEC.                          
           EXEC SQL INCLUDE T02TAB END-EXEC.                         
           EXEC SQL INCLUDE T08TAB END-EXEC.                         
           EXEC SQL INCLUDE T07TAB END-EXEC.                         
           EXEC SQL INCLUDE T01TAB END-EXEC.                         
           EXEC SQL INCLUDE T05TAB END-EXEC.                         
           EXEC SQL INCLUDE T20TAB END-EXEC.                         
           EXEC SQL INCLUDE T10TAB END-EXEC.                         
           EXEC SQL INCLUDE T22TAB END-EXEC.                         
      * CURSOR WILL RETURN RESULT SET WITH ALL DAYS                  
      * IN GIVEN RANGE, IN GIVEN DAY OF WEEK                         
                                                                     
           EXEC SQL                                                  
             DECLARE C-NAME CURSOR                                   
             FOR                                                     
             SELECT DATE_VALUE  ,                                    
                    DATE_VALUE   + 1 DAY                             
             FROM T21_DATES                                          
             WHERE DATE_VALUE >= :WS-Z02302-START-DATE AND           
             DATE_VALUE   <= :WS-Z02302-END-DATE                     
                           AND                                       
             DAYOFWEEK(DATE_VALUE) IN (:WS-MON, :WS-TUE, :WS-WED,    
                            :WS-THU, :WS-FRI, :WS-SAT, :WS-SUN)      
             FOR FETCH ONLY                                          
           END-EXEC                                                  
      * DB2 HANDLING VARIABLES                                       
       01 WS-DB2-ERROR.                                              
           10 SW-SQLCODE                    PIC S9(5).               
               88 SO-SQLCODE-OK             VALUE  000   100.        
               88 SO-SQLCODE-NORMAL         VALUE 000.               
               88 SO-SQLCODE-NOT-FOUND      VALUE 100.               
           10 WS-SQLERRMC                   PIC X(70).               
           10 SQLCODE-FORMAT                PIC -(5).         
           10 SW-STATEMENT-ID               PIC X(4).         
               88 SO-7100-PARA              VALUE '7100'.     
               88 SO-7200-PARA              VALUE '7200'.     
               88 SO-7300-PARA              VALUE '7300'.     
               88 SO-7400-PARA              VALUE '7400'.     
               88 SO-7500-PARA              VALUE '7500'.     
               88 SO-7600-PARA              VALUE '7600'.     
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
               88 SO-7020-PARA              VALUE '7020'.     
               88 SO-7021-PARA              VALUE '7021'.     
               88 SO-7022-PARA              VALUE '7022'.     
               88 SO-7023-PARA              VALUE '7023'.     
               88 SO-7024-PARA              VALUE '7024'.     
               88 SO-7025-PARA              VALUE '7025'.     
               88 SO-7026-PARA              VALUE '7026'.     
               88 SO-7101-PARA              VALUE '7101'.     
               88 SO-7102-PARA              VALUE '7102'.     
               88 SO-7103-PARA              VALUE '7103'.               
               88 SO-7104-PARA              VALUE '7104'.               
               88 SO-7099-PARA              VALUE '7099'.               
               88 SO-7105-PARA              VALUE '7105'.               
               88 SO-7106-PARA              VALUE '7106'.               
       01 CT-CONSTANTS.                                                 
           05 CT-ONE-MINUTE                  PIC S9(4) COMP VALUE 1.    
           05 CT-24-HOURS                    PIC S9(4) COMP VALUE 1440. 
           05 CT-EARTCH-RADIUS               PIC S9(4) COMP VALUE 6371. 
           05 CT-FIRST-PROG-NAME             PIC X(8) VALUE 'Z02131  '. 
           05 CT-QUEUE-NAME                  PIC X(8) VALUE '02X14'.    
           05 CT-PI-VALUE                    PIC 9V9(6) VALUE 3.141592. 
           05 CT-THIS-PROGRAM-NAME           PIC X(8) VALUE 'Z02292  '. 
           05 CT-NAME-OF-PROG-BEFORE         PIC X(8) VALUE 'Z02271  '. 
           05 CT-ERROR-ROUTINE-NAME          PIC X(8) VALUE 'Z02141  '. 
           05 CT-EMPTY-AIR-ORG               PIC X(50)                  
             VALUE '__________________________________________________'.
           05 CT-EMPTY-AIR-DES               PIC X(50)                  
             VALUE '__________________________________________________'.
           05 CT-EMPTY-DATE-D PIC X(10) VALUE '__________'.             
           05 CT-EMPTY-DATE-R PIC X(10) VALUE '__________'.             
           05 CT-EMPTY-TIC-NUM   PIC X(2) VALUE '__'.                   
           05 CT-EMPTY-ONE-WAY PIC X VALUE '_'.                         
           05 CT-EMPTY-DIRECT PIC X VALUE '_'.                          
           05 CT-DATE-ROUTINE-NAME  PIC X(8) VALUE 'Z02043  '.          
           05 CT-FLIGHT-PROGRAM-NAME PIC X(8) VALUE 'Z02172  '.         
           05 CT-SCHEDULED-STATUS.                                      
              49 CT-SCHEDULED-STATUS-LEN PIC S9(4) COMP VALUE 6.        
              49 CT-SCHEDULED-STATUS-TEXT PIC X(15) VALUE 'NORMAL'.     
       01 SW-SWITCHES.                                                  
           05 SW-IF-GO-BACK-TO-FIRST          PIC X.                    
               88 SO-GO-TO-FIRST-PROG         VALUE '1'.                
               88 SO-NOT-GO-TO-FIRST          VALUE '2'.                
           05 SW-RUN-FIRST-TIME               PIC X.                    
               88 SO-RUN-FIRST-TIME-WITH      VALUE 'F'.                
               88 SO-RUN-FIRST-TIME-WITHOUT   VALUE 'N'.                
               88 SO-RUN-NOT-FIRST-TIME       VALUE 'C'.             
           05 SW-WHAT-TYPE-OF-END             PIC X.                 
               88 SO-GO-TO-PREVIOUS-PROGRAM   VALUE '1'.             
               88 SO-FINAL-WITH-COMMAREA      VALUE '2'.             
           05 SW-IF-AIRPORT-ORIGIN-EMPTY      PIC X.                 
               88 SO-AIR-ORG-EMPTY            VALUE 'Y'.             
               88 SO-AIR-ORG-NOT-EMPTY        VALUE 'N'.             
           05 SW-IF-AIRPORT-DES-EMPTY         PIC X.                 
               88 SO-AIR-DES-EMPTY            VALUE 'Y'.             
               88 SO-AIR-DES-NOT-EMPTY        VALUE 'N'.             
           05 SW-IF-DAPARTURE-DATE-EMPTY      PIC X.                 
               88 SO-DATE-D-EMPTY             VALUE 'Y'.             
               88 SO-DATE-D-NOT-EMPTY         VALUE 'N'.             
           05 SW-IF-RETURN-DATE-EMPTY         PIC X.                 
               88 SO-DATE-R-EMPTY             VALUE 'Y'.             
               88 SO-DATE-R-NOT-EMPTY         VALUE 'N'.             
           05 SW-IF-TICKET-NUMBER-EMPTY       PIC X.                 
               88 SO-TIC-NUM-EMPTY            VALUE 'Y'.             
               88 SO-TIC-NUM-NOT-EMPTY        VALUE 'N'.             
           05 SW-IF-ONE-WAY-FLAG-EMPTY        PIC X.                 
               88 SO-ONE-WAY-EMPTY            VALUE 'Y'.             
               88 SO-ONE-WAY-NOT-EMPTY        VALUE 'N'.             
           05 SW-IF-DIRECT-FLIGHT-FLAG-EMPTY  PIC X.                 
               88 SO-DIRECT-EMPTY             VALUE 'Y'.             
               88 SO-DIRECT-NOT-EMPTY         VALUE 'N'.             
                                                                     
           05 SW-IF-TICKET-NUMBER-VALID       PIC X.                 
               88 SO-TICKET-NUMBER-VALID      VALUE 'Y'.             
               88 SO-TICKET-NUMBER-INVALID    VALUE 'N'.             
                                                                     
           05 SW-IF-ONE-WAY-VALID             PIC X.                 
               88 SO-ONE-WAY-VALID            VALUE 'Y'.             
               88 SO-ONE-WAY-INVALID          VALUE 'N'.             
                                                                     
           05 SW-IF-DIRECT-VALID             PIC X.                  
               88 SO-DIRECT-VALID            VALUE 'Y'.              
               88 SO-DIRECT-INVALID          VALUE 'N'.          
           05 SW-IF-USER-AIRPORT-3-CHAR      PIC X.              
               88 SO-USER-AIRPORT-3-CHAR     VALUE 'Y'.          
               88 SO-USER-AIRPORT-NOT-3-CHAR VALUE 'N'.          
           05 SW-IF-VALID-IATA               PIC X.              
               88 SO-VALID-IATA              VALUE 'Y'.          
               88 SO-INVALID-IATA            VALUE 'N'.          
           05 SW-IF-VALID-TIME-CHECK         PIC X.              
               88 SO-TIME-CHECK-INVALID      VALUE 'Y'.          
               88 SO-TIME-CHECK-VALID        VALUE 'N'.          
           05 SW-IF-VALID-FULL-NAME          PIC X.              
               88 SO-VALID-NAME              VALUE 'Y'.          
               88 SO-INVALID-NAME            VALUE 'N'.          
           05 SW-IF-PLANE-VALID             PIC X.               
               88 SO-PLANE-INVALID           VALUE 'Y'.          
               88 SO-PLANE-VALID             VALUE 'N'.          
           05 SW-IF-TIME-IS-VALID           PIC X.               
               88 SO-TIME-IS-VALID           VALUE 'Y'.          
               88 SO-TIME-IS-INVALID         VALUE 'N'.          
           05 SW-IF-MODEL-NAME-VALID        PIC X.               
               88 SO-MODEL-NAME-INVALID      VALUE 'Y'.          
               88 SO-MODEL-NAME-VALID        VALUE 'N'.          
           05 SW-IF-TRANSLATION-OK          PIC X.               
               88 SO-TRANSLATION-FALIED     VALUE 'Y'.           
               88 SO-TRANSLATION-SUCCESS    VALUE 'N'.           
           05 SW-IF-VALID-WEEK-DAY          PIC X.               
               88 SO-VALID-WEEK-DAY         VALUE 'Y'.           
               88 SO-SKIP-THIS-DAY          VALUE 'N'.           
           05 SW-IF-ARRIVAL-SAME-DAY        PIC X.               
               88 SO-ARRIVAL-IS-SAME-DAY    VALUE '1'.           
               88 SO-ARRIVAL-IS-NEXT-DAY    VALUE '2'.           
           05 SW-IF-END-OF-CURSOR           PIC X.               
               88 SO-END-OF-CURSOR-DATA     VALUE '1'.           
               88 SO-NOT-END-OF-CURSOR-DATA VALUE '2'.           
           05 SW-IF-FIRST-FLIGHT-NUMBER     PIC X.               
               88 SO-FIRST-FLIGHT-NUMBER    VALUE '1'.           
               88 SO-NOT-FIRST-FLIGHT-NUMBER VALUE '2'.               
           05 SW-IF-END-OF-QUEUE-DATA       PIC X.                    
               88 SO-NOT-END-OF-QUEUE-DATA  VALUE '1'.                
               88 SO-END-OF-QUEUE-DATA       VALUE '2'.               
      * PROGRAM VARIABLES                                             
       01 WS-VARIABLES.                                               
          05 WS-FIRST-CHOICE                         PIC X.           
          05 WS-SECOND-CHOICE                        PIC X.           
          05 WS-TEMP-TIMEZONE.                                        
             10 WS-TIMEZONE-HOUR-AND-SIGN.                            
              15 WS-TIMEZONE-SIGN                    PIC X.           
              15 WS-TIMEZONE-HOUR                    PIC X(2).        
             10 WS-TIMEZONE-FILLER                  PIC X.            
             10 WS-TIMEZONE-MINUTE                  PIC X(2).         
                                                                      
          05 WS-DUMMY                      PIC X VALUE SPACE.         
          05 WS-HOUR-OFFSET                PIC S9(4) COMP VALUE 0.    
          05 WS-MINUTE-OFFSET              PIC S9(4) COMP VALUE 0.    
          05 WS-HOUR-OFFSET-TEMP           PIC X(10) VALUE SPACE.     
          05 WS-MINUTE-OFFSET-TEMP         PIC X(10) VALUE SPACE.     
          05 WS-MODIFIED-TIMESTAMP         PIC X(26).                 
          05 WS-MODIFIED-TIMESTAMP-OUT     PIC X(26).                 
          05 WS-WHAT-RECORD-TO-READ        PIC S9(4) COMP VALUE 0.    
          05  WS-TEMP-AIRPORT              PIC X(50).                 
          05  WS-MON                       PIC S9(9) COMP VALUE 0.    
          05  WS-TUE                       PIC S9(9) COMP VALUE 0.    
          05  WS-WED                       PIC S9(9) COMP VALUE 0.    
          05  WS-THU                       PIC S9(9) COMP VALUE 0.    
          05  WS-FRI                       PIC S9(9) COMP VALUE 0.    
          05  WS-SAT                       PIC S9(9) COMP VALUE 0.    
          05  WS-SUN                       PIC S9(9) COMP VALUE 0.    
                                                                      
          05  WS-TOMORROW-DATE             PIC X(10).                 
          05  WS-CURRENT-DATE              PIC X(10).                 
          05 WS-USER-DAYS.                                            
              10 WS-USER-DAYS-SUN          PIC XX.                    
              10 WS-USER-DAYS-MON          PIC XX.                     
              10 WS-USER-DAYS-TUE          PIC XX.                     
              10 WS-USER-DAYS-WED          PIC XX.                     
              10 WS-USER-DAYS-THU          PIC XX.                     
              10 WS-USER-DAYS-FRI          PIC XX.                     
              10 WS-USER-DAYS-SAT          PIC XX.                     
              10 WS-FINAL-VALUE            PIC X VALUE '0'.            
          05 WS-TIME-VARIABLES.                                        
              10 WS-TEMP-HOUR1             PIC 99 VALUE 0.             
              10 WS-TEMP-HOUR2             PIC 99 VALUE 0.             
              10 WS-TEMP-MINUTE1           PIC 99 VALUE 0.             
              10 WS-TEMP-MINUTE2           PIC 99 VALUE 0.             
                                                                       
          05 WS-WEEK-DAYS-TABLE            PIC X(7).                   
          05 WS-WEEK-DAY                   PIC S9(9) COMP VALUE 0.     
          05 WS-DAY-COUNTER                PIC S9(9) COMP VALUE 0.     
          05 WS-DAY-COUNTER2               PIC S9(9) COMP VALUE 0.     
          05 WS-AIRLINE                    PIC X(20).                  
          05 WS-TIMESTAMP-STRUCTURE.                                   
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
           05 WS-RANDOM-VALUE                PIC X     VALUE SPACE.    
           05 WS-TEMP-NUMERIC                PIC 99.                   
           05 WS-TYPE-OF-SEATS               PIC XX    VALUE SPACE.    
           05 WS-TYPE-OF-SEATS-NUMERIC       PIC S9(4) COMP VALUE 0.   
           05 WS-DEPARTURE-DATE              PIC X(10) VALUE SPACE.    
           05 WS-DEPARTURE-TIME.                                       
              10 WS-DEPARTURE-TIME-HOUR      PIC X(2).                 
              10 WS-DEPARTURE-TIME-DELIMITER PIC X.                    
              10 WS-DEPARTURE-TIME-MINUTE    PIC X(2).                 
           05 WS-ARRIVAL-DATE                PIC X(10) VALUE SPACE.    
           05 WS-ARRIVAL-TIME.                                         
               10 WS-ARRIVAL-TIME-HOUR       PIC X(2).                 
               10 WS-ARRIVAL-TIME-DELIMITER  PIC X.                    
               10 WS-ARRIVAL-TIME-MINUTE     PIC X(2).                 
           05 WS-PLANE-MODEL                 PIC X(50) VALUE SPACE.    
           05 WS-EIBRESP-TEMP                PIC X(10) VALUE SPACE.    
           05 WS-LENGTH-OF-STRING            PIC S9(4) COMP.           
           05 WS-AIRPORT-NAME-TEMP           PIC X(50) VALUE SPACE.    
           05 WS-AIRPORT-NAME-FROM-USER      PIC X(50) VALUE SPACE.    
           05 WS-AIR-ORG                     PIC X(50) VALUE SPACE.    
           05 WS-AIR-DES                     PIC X(50) VALUE SPACE.    
           05 WS-DATE-D                      PIC X(10) VALUE SPACE.    
           05 WS-DATE-R                      PIC X(10) VALUE SPACE.    
           05 WS-TIC-NUM                     PIC X(2)  VALUE SPACE.    
           05 WS-ONE-WAY                     PIC X(1)  VALUE SPACE.    
           05 WS-DIRECT                      PIC X(1)  VALUE SPACE.    
           05 WS-AIRPORT-VALUE               PIC X(50) VALUE SPACE.    
           05 WS-ITER                        PIC S9(4) COMP VALUE 0.   
           05 WS-ITERX                       PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-FORMAT                 PIC S9(4)  VALUE 0.       
           05 WS-AIRPORT-FULL-NAME           PIC X(59) VALUE SPACE.    
           05 WS-ID-NUMBER-CHAR              PIC X(10).                
           05 WS-TIME-CHECK-VAR.                                       
              10 WS-TIME-HOUR                PIC X(2).                 
              10 WS-TIME-FILLER              PIC X.                    
              10 WS-TIME-MINUTE              PIC X(2).                 
           05 WS-TIME-TEMP-NUMERIC           PIC 99.                   
           05 WS-TIME-DB-FORMAT.                                       
              10 WS-TIME-DB-HOUR             PIC 99.                    
              10 WS-TIME-DB-FILLER1          PIC X.                     
              10 WS-TIME-DB-MINUTE           PIC 99.                    
              10 WS-TIME-DB-FILLER2          PIC X.                     
              10 WS-TIME-DB-SECOND           PIC 99.                    
           05 WS-CALCULATION-VARIABLES.                                 
              10 WS-LATITUDE                         COMP-2.            
              10 WS-LONGITUDE                        COMP-2.            
              10 WS-ORG-LATITUDE                     COMP-2.            
              10 WS-ORG-LONGITUDE                    COMP-2.            
              10 WS-DES-LATITUDE                     COMP-2.            
              10 WS-DES-LONGITUDE                    COMP-2.            
              10 WS-RAD-ORG-LATITUDE                 COMP-2.            
              10 WS-RAD-ORG-LONGITUDE                COMP-2.            
              10 WS-RAD-DES-LONGITUDE                COMP-2.            
              10 WS-RAD-DES-LATITUDE                 COMP-2.            
              10 WS-A                                COMP-2.            
              10 WS-C                                COMP-2.            
              10 WS-D                                COMP-2.            
              10 WS-DISPLAY-D                     PIC 9(9).9(9).        
       01 WS-QUEUE-STRUCTURE.                                           
           05 QUEUE-FLIGHT-ID               PIC X(15).                  
           05 QUEUE-FLIGHT-NUMBER           PIC X(15).                  
           05 QUEUE-PLANE-ID                PIC S9(9) COMP.             
           05 QUEUE-DEPARTURE-AIRPORT       PIC X(3).                   
           05 QUEUE-DEPARTURE-TIMESTAMP     PIC X(26).                  
           05 QUEUE-ARRIVAL-AIRPORT         PIC X(3).                   
           05 QUEUE-ARRIVAL-TIMESTAMP       PIC X(26).                  
           05 QUEUE-FILGHT-STATUS           PIC X(15).                  
           05 QUEUE-AIRLINE-CODE            PIC X(3).                   
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA  PIC X(17294).                                    
      ******************************************************************
      *                    PROCEDURE DIVISION                           
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02292-----------------START-------------'          
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02292--------------B--FINAL-------------'          
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                           1000-INIT                             
      ******************************************************************
       1000-INIT.                                                       
           DISPLAY 'START OF Z02292'                                    
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           DISPLAY 'AFTER DFHCOMMAREA MOVE'                             
      *TEST                                                             
           DISPLAY 'WS-Z02152-I-MODEL-NAME ' WS-Z02152-I-MODEL-NAME     
      */TEST                                                            
           PERFORM 1005-CHECK-IF-FIRST-TIME                             
           .                                                            
      ******************************************************************
      *                     1005-CHECK-IF-FIRST-TIME                    
      * CORRECT FLAGS WILL BE SET AND PROCESSES WILL BE STARTED         
      * DEPENDING TO CURRECT PROGRAM MODE                               
      ******************************************************************
       1005-CHECK-IF-FIRST-TIME.                                        
           SET SO-FIRST-FLIGHT-NUMBER TO TRUE                           
           EVALUATE TRUE                                                
           WHEN SO-M-FIRST-WITHOUT                                      
              DISPLAY 'FIRST WITHOUT '                                  
              MOVE LOW-VALUES TO WS-Z02152-I-AIRLINE-CODE               
              MOVE LOW-VALUES TO WS-Z02152-I-MODEL-NAME                 
              PERFORM 1010-IGNORE-CONDITION                             
              SET SO-RUN-FIRST-TIME-WITHOUT    TO TRUE                  
              SET SO-M-NOT-FIRST               TO TRUE                  
              SET SO-FLIGHTS-TO                TO TRUE                  
              SET SO-FLIGHTS-TO-NOT-ADDED      TO TRUE                  
              SET SO-FLIGHTS-FROM-NOT-ADDED    TO TRUE                  
                                                                        
              PERFORM 1015-DELETE-QUEUE                                 
           WHEN SO-M-FIRST-WITH                                         
              SET SO-RUN-FIRST-TIME-WITH       TO TRUE                  
              SET SO-M-NOT-FIRST               TO TRUE                  
                                                                        
           WHEN SO-M-NOT-FIRST                                          
              SET SO-RUN-NOT-FIRST-TIME        TO TRUE                  
                                                                        
           WHEN OTHER                                                   
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID CALL ' TO WS-Z02141-I-ERROR-MESSAGE(1)      
              SET SO-M-FIRST-WITH TO  TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    1010-IGNORE-CONDITION                        
      ******************************************************************
       1010-IGNORE-CONDITION.                                           
           EXEC CICS                                                    
            IGNORE CONDITION ERROR                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    1015-DELETE-QUEUE                            
      ******************************************************************
       1015-DELETE-QUEUE.                                               
           EXEC CICS                                                    
            DELETEQ TS                                                  
            QUEUE(CT-QUEUE-NAME)                                        
            NOHANDLE                                                    
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR)                                 
           THEN                                                         
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                        2000-PROCESS                             
      ******************************************************************
       2000-PROCESS.                                                    
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           EVALUATE TRUE                                                
           WHEN  SO-RUN-FIRST-TIME-WITHOUT                              
             PERFORM 2001-PROCESS-FIRST-WITHOUT                         
           WHEN SO-RUN-FIRST-TIME-WITH                                  
             PERFORM 2002-PROCESS-FIRST-TIME-WITH                       
           WHEN SO-RUN-NOT-FIRST-TIME                                   
             PERFORM 2003-PROCESS-NOT-FIRST-TIME                        
             DISPLAY 'FLAGA PO 2003: ' SW-M-WHAT-MODE                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2001-PROCESS-FIRST-WITHOUT                     
      ******************************************************************
       2001-PROCESS-FIRST-WITHOUT.                                      
           SET SO-USER-CHOOSES TO TRUE                                  
           PERFORM 2351-SEND-CHOICE-MAP                                 
           .                                                            
      ******************************************************************
      *                  2002-PROCESS-FIRST-TIME-WITH                   
      *                                                                 
      ******************************************************************
       2002-PROCESS-FIRST-TIME-WITH.                                    
           IF SO-USER-CHOOSES THEN                                      
              PERFORM 2351-SEND-CHOICE-MAP                              
           ELSE                                                         
              PERFORM 2010-DFHCOMMAREA-TO-SCREEN                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2001-PROCESS-IF-FIRST-TIME                     
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           PERFORM 2060-PROCESS-INPUT                                   
           .                                                            
      ******************************************************************
      *                     2010-DFHCOMMAREA-TO-SCREEN                  
      ******************************************************************
       2010-DFHCOMMAREA-TO-SCREEN.                                      
           IF SO-ADD-A-SINGLE THEN                                      
             PERFORM 2301-SINGLE-FLIGHT-TO-COMMAREA                     
           ELSE                                                         
             PERFORM 2302-SCHEDULE-DATA-TO-COMMAREA                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2050-SEND-FRESH-MAP                        
      ******************************************************************
       2050-SEND-FRESH-MAP.                                             
           MOVE LOW-VALUES TO MP0229O                                   
           MOVE LOW-VALUES TO MP0230O                                   
           IF SO-ADD-A-SINGLE THEN                                      
                                                                        
             PERFORM 2100-SEND-THE-MAP                                  
           ELSE                                                         
             PERFORM 2101-SEND-THE-MAP-SCHED                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2060-PROCESS-INPUT                            
      * PARAGRAPH IS CALLED WHEN THIS PROGRAM STARTED BECAUSE USER HAVE 
      * PRESSED ATTENTION KEY                                           
      *                                                                 
      * IF USER PRESSED F3 THEN CONTROL WILL BE RETURNED TO CALLING     
      * PROGRAM                                                         
      *                                                                 
      * IF USER PRESSED ENTER THEN WE WILL VALIDATE ALL DATA HE         
      * PROVIDED                                                        
      *                                                                 
      * IF USER PRESSED ANY OTHER KEY THEN WE WILL GET PROPER           
      * ERROR MESSAGE                                                   
      *                                                                 
      ******************************************************************
       2060-PROCESS-INPUT.                                              
           EVALUATE EIBAID                                              
           WHEN DFHENTER                                                
               IF SO-USER-CHOOSES THEN                                  
                 PERFORM 2352-PROCESS-USER-CHOICE                       
                 PERFORM 2050-SEND-FRESH-MAP                            
               ELSE                                                     
                 SET SO-FINAL-WITH-COMMAREA TO TRUE                     
                 PERFORM 2070-GET-AND-PROCESS-DATA                      
               END-IF                                                   
           WHEN DFHPF3                                                  
               SET SO-GO-TO-PREVIOUS-PROGRAM TO TRUE                    
           WHEN OTHER                                                   
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE ' NO ACTION KEY WAS PRESSED ' TO                    
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2070-GET-AND-PROCESS-DATA                     
      ******************************************************************
       2070-GET-AND-PROCESS-DATA.                                       
           PERFORM 2071-GET-DATA-FROM-USER                              
           PERFORM 2072-CHECK-IF-DATA-IS-VALID                          
           .                                                            
      ******************************************************************
      *                   2071-GET-DATA-FROM-USER                       
      * PARAGRAPH WILL RECEIVE DATA FROM MAP MP0229 OR MP0230           
      * DEPEING ON PROGRAM MODE ( ADDING  A SINGLE FLIGHT, OR           
      *       ADDING A SCHEDULED FLIGHT )                               
      ******************************************************************
       2071-GET-DATA-FROM-USER.                                         
           IF SO-ADD-A-SINGLE THEN                                      
             PERFORM 2305-RECEIVE-SINGLE-FLIGHT-MAP                     
           ELSE                                                         
             PERFORM 2306-RECEIVE-SCHEDULE-MAP                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2072-CHECK-IF-DATA-IS-VALID                   
      *                                                                 
      * BEFORE PROGRAM WILL CHECK WHAT FIELDS WERE NOT PROVIDED BY THE  
      * USER WE WILL SAVE ALL OF THOSE FIELDS INTO THE COMMAREA         
      *                                                                 
      * THANKS TO THAT WE CAN BREAK THE PROGRAM LOGIC                   
      * WITHOUT LOOSING ANY DATA WHILE CHECKING IF INPUT FIELDS ARE     
      * EMPTY OR NOT                                                    
      ******************************************************************
       2072-CHECK-IF-DATA-IS-VALID.                                     
                                                                        
           IF SO-ADD-A-SINGLE THEN                                      
             DISPLAY 'CHECK IF EMPTY - SINGLE '                         
             PERFORM 2366-SAVE-SINGLE-TO-COMMAREA                       
             PERFORM 2073-CHECK-WHAT-IS-EMPTY                           
           ELSE                                                         
             DISPLAY 'CHECK IF EMPTY - SCHEDULED '                      
             PERFORM 2086-CHECK-IF-EMPTY-SCHEDULED                      
           END-IF                                                       
      * IF USER WANTS TO ADD A SINGLE FLIGHT                            
           IF SO-ADD-A-SINGLE THEN                                      
             PERFORM 2307-VALIDATE-SINGLE-FLIGHT                        
           ELSE                                                         
             PERFORM 2365-VALIDATE-SCHEDULE-DATA                        
      * THIS PARAGRAPH WILL PREPARE AND INSERT INTO THE QUEUE           
      * ALL FLIGHTS THAT WERE GENERATED                                 
      * PROGRAM LOGIC WILL BE IN THIS PLACE 2 TIMES                     
      * 1 TIME AFTER THE USER PRESSED ENTER FOR THE FIRST TIME          
      * WHILE ADDING FLIGHTS "TO" AND SECOND TIME AFTER USER            
      * PROVIDED DATA ABOUT FLIGHTS "FROM"                              
      * IF ALL DATA WERE SUCCESSFULL AND FLIGHTS "TO" AND "FROM"        
      * WERE ADDED TO QUEEU, LOGIC BELOW THIS PARAGRAPH WILL            
      * INSERT THOSE DATA INTO THE DATABASE                             
             PERFORM 2087-PREP-AND-INSERT-FLIGHTS                       
                                                                        
             PERFORM 2325-CONTROL-THE-DATA-FLOW                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2073-CHECK-WHAT-IS-EMPTY                      
      * PARAGRAPH WILL CHECK WHAT FIELDS USER PROVIDED                  
      *                                                                 
      * IF USER DIDN'T PROVIDE ANY OF THOSE VALUES THEN ERROR MESSAGE   
      * WILL BE DISPLAYED                                               
      ******************************************************************
       2073-CHECK-WHAT-IS-EMPTY.                                        
           PERFORM 2326-IF-ORIGIN-AIRPORT-EMPTY                         
           PERFORM 2329-IF-DEST-AIRPORT-EMPTY                           
           PERFORM 2330-IF-DEPARTURE-DATE-EMPTY                         
           PERFORM 2331-IF-DEPARTURE-TIME-EMPTY                         
           PERFORM 2332-IF-ARRIVAL-DATE-EMPTY                           
           PERFORM 2333-IF-ARRIVAL-TIME-EMPTY                           
           PERFORM 2334-IF-PLANE-MODEL-EMPTY                            
           PERFORM 2335-IF-TYPE-OF-SEATS-EMPTY                          
           PERFORM 2336-IF-AIRLINE-NAME-EMPTY                           
           .                                                            
      ******************************************************************
      *                  2074-CHECK-IF-VALID-MODEL                      
      * PARAGRAPH WILL PREAPRE SEATS-ID ( IT WILL MOVE IT TO            
      * NUMERIC VARIABLE )                                              
      * THEN PARAGRAPH WILL CHECK IF THERE IS ANY PLANE MODEL THAT      
      * HAS THIS SEATS_ID (HAS THIS SEATS TYPE)                         
      ******************************************************************
       2074-CHECK-IF-VALID-MODEL.                                       
           PERFORM 2314-PREPARE-SEAT-TYPE-DATA                          
           PERFORM 7001-CHECK-IF-MODEL-VALID                            
           .                                                            
      ******************************************************************
      *                  2077-VALIDATE-AIRPORT-CODE                     
      ******************************************************************
       2077-VALIDATE-AIRPORT-CODE.                                      
           PERFORM 7100-VALIDATE-AIRPORT-IATA                           
           IF SO-INVALID-IATA THEN                                      
              PERFORM 2078-VALIDATE-AIRPORT-NAME                        
              IF SO-VALID-NAME THEN                                     
               MOVE T02-AIRPORT-CODE TO WS-AIRPORT-VALUE                
               MOVE T02-AIRPORT-FULL-NAME-TEXT TO WS-AIRPORT-FULL-NAME  
               SET SO-VALID-IATA TO TRUE                                
              END-IF                                                    
           ELSE                                                         
      * VALID IATA                                                      
             MOVE T02-AIRPORT-FULL-NAME-TEXT TO WS-AIRPORT-FULL-NAME    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2078-VALIDATE-AIRPORT-NAME                    
      ******************************************************************
       2078-VALIDATE-AIRPORT-NAME.                                      
           IF WS-AIRPORT-VALUE = T02-AIRPORT-FULL-NAME-TEXT             
           THEN                                                         
               SET SO-VALID-NAME TO TRUE                                
           ELSE                                                         
               SET SO-INVALID-NAME TO TRUE                              
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2080-CHECK-FOR-PLANE-MODEL                    
      * PARAGRAPH WILL CHECK IF THIS PLANE_ID IS VALID OR NOT           
      * IF IT IS IT WILL DISPLAY MODEL FULL NAME ON THE SCREEN          
      ******************************************************************
       2080-CHECK-FOR-PLANE-MODEL.                                      
           IF WS-Z02152-I-PLANE-ID = 0    THEN                          
               CONTINUE                                                 
           ELSE                                                         
              MOVE  WS-Z02152-I-PLANE-ID  TO T08-PLANE-ID               
              PERFORM 7009-TRANSLATE-PLANE-ID                           
              IF SO-TRANSLATION-FALIED  THEN                            
                MOVE LOW-VALUES TO MODELO                               
                MOVE LOW-VALUES TO MODEL2O                              
              ELSE                                                      
                MOVE T08-PLANE-MODEL-TEXT TO MODELO                     
                MOVE T08-PLANE-MODEL-TEXT TO MODEL2O                    
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2081-CALCULATE-DISTANCE                      
      * PARAGRAPH WILL TELL THE DISTANCE BETWEEN 2 AIRPORTS             
      * LATER PROGRAM WILL CHECK IF THIS DISTANCE IS VALID WITH RANGE   
      * OF CHOSEN PLANE                                                 
      * WS-AIR-DES AND WS-AIR-ORG                                       
      *                                                                 
      * PARAGRAPH USES HAVERSINE FORUMLA                                
      ******************************************************************
       2081-CALCULATE-DISTANCE.                                         
      * FIRST WE NEED TO GET LATITUDE AND LONGITUDE OF AIRPORTS         
           PERFORM 2337-GET-GOGRAFICAL-POSITIONS                        
      * THEN WE NEED TO CHENGE IT TO RADIANDS                           
           PERFORM 2338-MOVE-DEGREES-TO-RADIANS                         
                                                                        
      * NOW WE HAVE TO  CALCULATE DIFFERENCE BETWEEN LONGITUDES AND     
      * LONGITUDES                                                      
           COMPUTE WS-LATITUDE = WS-RAD-DES-LATITUDE -                  
                                 WS-RAD-ORG-LATITUDE                    
           COMPUTE WS-LONGITUDE = WS-RAD-DES-LONGITUDE -                
                                 WS-RAD-ORG-LONGITUDE                   
      * NOW WE WILL CALCULATE AN A                                      
           COMPUTE WS-A = ( FUNCTION SIN(WS-LATITUDE / 2) ** 2 ) +      
            FUNCTION COS(WS-RAD-DES-LATITUDE) *                         
            FUNCTION COS(WS-RAD-ORG-LATITUDE) *                         
            FUNCTION SIN(WS-LONGITUDE / 2) ** 2                         
      * NOW WE WILL CALCULATE AN C                                      
           COMPUTE WS-C  = 2 * FUNCTION ATAN(                           
              FUNCTION SQRT(WS-A) / FUNCTION SQRT(1 - WS-A) )           
                                                                        
           COMPUTE WS-D = CT-EARTCH-RADIUS * WS-C                       
           .                                                            
      ******************************************************************
      *                  2082-CHECK-TIME-OF-FLIGHT                      
      * PARAGRAPH WILL CHECK IF USER PROVIDED VALID TIME OF FLIGHT      
      *                                                                 
      * PARAGRAPH WILL CHECK FOR EXMAPLE IF ARRIVAL TIME IS LATER       
      * THAN DEPARTURE TIME                                             
      *                                                                 
      ******************************************************************
       2082-CHECK-TIME-OF-FLIGHT.                                       
           MOVE WS-Z02152-I-TIME-D TO  WS-TIME-CHECK-VAR                
           MOVE WS-TIME-HOUR TO WS-TEMP-HOUR1                           
           MOVE WS-TIME-MINUTE TO WS-TEMP-MINUTE1                       
           MOVE WS-Z02152-I-TIME-R TO WS-TIME-CHECK-VAR                 
           MOVE WS-TIME-HOUR TO WS-TEMP-HOUR2                           
           MOVE WS-TIME-MINUTE TO WS-TEMP-MINUTE2                       
           EVALUATE TRUE                                                
           WHEN WS-Z02152-I-DATE-D = WS-Z02152-I-DATE-R                 
      * SAME DAY                                                        
               IF WS-TEMP-HOUR1 < WS-TEMP-HOUR2 THEN                    
                    SET SO-TIME-CHECK-VALID TO TRUE                     
               ELSE                                                     
                  IF WS-TEMP-HOUR1  = WS-TEMP-HOUR2                     
                      IF WS-TEMP-MINUTE1 < WS-TEMP-MINUTE2 THEN         
                         SET SO-TIME-CHECK-VALID TO TRUE                
                      ELSE                                              
                         SET SO-TIME-CHECK-INVALID TO TRUE              
                      END-IF                                            
                  ELSE                                                  
                    SET SO-TIME-CHECK-INVALID TO TRUE                   
                  END-IF                                                
               END-IF                                                   
           WHEN OTHER                                                   
      * DIFFERENT DAY                                                   
             SET SO-TIME-CHECK-VALID TO TRUE                            
           END-EVALUATE                                                 
           IF SO-TIME-CHECK-INVALID THEN                                
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID TIME ' TO                                  
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2083-ADD-A-FLIGHT                           
      ******************************************************************
       2083-ADD-A-FLIGHT.                                               
           PERFORM 7014-INSERT-THE-FLIGHT                               
                                                                        
           PERFORM 2700-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'FLIGHT WAS SUCCESSFULLY ADDED '                        
                          TO          WS-Z02141-I-ERROR-MESSAGE(1)      
           SET SO-Z02141-M-WITH TO TRUE                                 
           SET SO-GO-TO-FIRST-PROG TO TRUE                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                     2084-GET-FLIGHT-ID                          
      ******************************************************************
       2084-GET-FLIGHT-ID.                                              
           PERFORM 7012-INSERT-NEW-VALUE                                
           PERFORM 7013-GET-THIS-VALUE                                  
           .                                                            
      ******************************************************************
      *                   2085-INITIALIZE-TIMESTAMP                     
      * USER CANNOT SPECIFY SECOND AND MILISECOND OF THE DEPARTURE      
      * SO PROGRAM NEED TO SPECIFY THIS VALUES AS ZERO                  
      ******************************************************************
       2085-INITIALIZE-TIMESTAMP.                                       
           MOVE 00 TO FIRST-DEP-SECOND                                  
           MOVE 000000 TO FIRST-DEP-MICROSEC                            
           .                                                            
      ******************************************************************
      *                  2086-CHECK-IF-EMPTY-SCHEDULED                  
      * IF USER PROVIDED DATA FOR FLIGHTS "TO" THEN PROGRAM             
      * HAS TO VALIDATE ALL POSSIBLE DATA                               
      *                                                                 
      * IF HE PROVIDED DATA FOR "FROM" FLIGHT THEN PROGRAM              
      * HAS TO VALIDTAE ONLY :                                          
      *              1. WEEK DAYS                                       
      *              2. DEPARTURE TIME                                  
      *              3. ARRIVAL TIME                                    
      * THAT IS BECAUSE OTHER DATA WILL BE THE SAME AS FOR THE "TO"     
      * FLIGHTS                                                         
      ******************************************************************
       2086-CHECK-IF-EMPTY-SCHEDULED.                                   
           PERFORM 2339-IF-WEEK-DAYS-EMPTY                              
           PERFORM 2340-IF-DEPARTURE-TIME-EMPTY                         
           PERFORM 2341-IF-ARRIVAL-TIME-EMPTY                           
             MOVE WS-Z02302-TYPE2 TO WS-TYPE-OF-SEATS                   
             DISPLAY 'WS-TYPE-OF-SEATS: ' WS-TYPE-OF-SEATS              
                                                                        
           IF SO-FLIGHTS-TO THEN                                        
             PERFORM 2342-IF-SCHEDULE-DATES-EMPTY                       
             PERFORM 2343-IF-PLANE-MODEL2-EMPTY                         
             PERFORM 2344-IF-TYPE-OF-SEAYS2-EMPTY                       
             PERFORM 2345-IF-AIRLINE2-EMPTY                             
             PERFORM 2346-IF-AIRPORTS2-EMPTY                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2087-PREP-AND-INSERT-FLIGHTS                      
      * PARAGRAPH WILL CHECK ALL DATES IN RANGE TO FIND   ALL           
      * WEEKDAYS WHERE USER WANTS TO ADD A FLIGHT                       
      ******************************************************************
       2087-PREP-AND-INSERT-FLIGHTS.                                    
           DISPLAY ' 2087-PREP-AND-INSERT-FLIGHTS PERFORMED'            
           PERFORM 2089-PREPARE-WEEK-DAYS                               
           PERFORM 7016-OPEN-CURSOR-C-NAME                              
           PERFORM 7017-FETCH-CURSOR-C-NAME                             
           PERFORM 7018-CLOSE-CURSOR-C-NAME                             
           .                                                            
      ******************************************************************
      *                 2088-CHECK-IF-WEEK-DAY-VALID                    
      ******************************************************************
       2088-CHECK-IF-WEEK-DAY-VALID.                                    
           IF WS-WEEK-DAYS-TABLE(WS-WEEK-DAY:1)  = 'X'                  
           THEN                                                         
               SET SO-VALID-WEEK-DAY TO TRUE                            
           ELSE                                                         
               SET SO-SKIP-THIS-DAY  TO TRUE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2089-PREPARE-WEEK-DAYS                     
      * THIS PARAGRAPH WILL PREPARE DATA FOR THE CURSOR                 
      *                                                                 
      * IN CURSOR'S WHERE CLOUSE WE CAN FIND :                          
      *    DAYOFWEEK(DATE_VALUE) IN (:WS-MON, :WS-TUE, :WS-WED,         
      *                   :WS-THU, :WS-FRI, :WS-SAT, :WS-SUN)           
      *                                                                 
      * VLAUES IN THE IN CLOUSE WILL BE PREPARED HERE                   
      *                                                                 
      * IF WE WANT TO GET SUNDAYS WE WILL PUT 1 TO WS-SUN IF ELSE       
      * WE WILL PUT 0 THERE                                             
      *                                                                 
      * IF WE WANT TO GET MONDAYS WE WILL PUT 2 TO WS-MON IF ELSE       
      * WE WILL PUT 0 THERE                                             
      * (THIS PROCESS CONTINUES FOR THE ALL WEEKDAYS)                   
      *                                                                 
      * THANKS TO THAT LOGIC WE WILL FETCH ONLY VALID WEEK DAYS         
      *                                                                 
      ******************************************************************
       2089-PREPARE-WEEK-DAYS.                                          
           IF WS-Z02302-O-SUN = 'X' THEN                                
                MOVE 1 TO WS-SUN                                        
           ELSE                                                         
                MOVE 0 TO WS-SUN                                        
           END-IF                                                       
           IF WS-Z02302-O-MON = 'X' THEN                                
                MOVE 2  TO WS-MON                                       
           ELSE                                                         
                MOVE 0  TO WS-MON                                       
           END-IF                                                       
           IF WS-Z02302-O-TUE = 'X' THEN                                
                MOVE 3 TO WS-TUE                                        
           ELSE                                                         
                MOVE 0 TO WS-TUE                                        
           END-IF                                                       
           IF WS-Z02302-O-WED = 'X' THEN                                
                MOVE  4   TO WS-WED                                     
           ELSE                                                         
                MOVE  0   TO WS-WED                                     
           END-IF                                                       
           IF WS-Z02302-O-THU = 'X' THEN                                
                MOVE  5   TO WS-THU                                     
           ELSE                                                         
                MOVE  0   TO WS-THU                                     
           END-IF                                                       
           IF WS-Z02302-O-FRI = 'X' THEN                                
                MOVE  6   TO WS-FRI                                     
           ELSE                                                         
                MOVE  0   TO WS-FRI                                     
           END-IF                                                       
           IF WS-Z02302-O-SAT = 'X' THEN                                
                MOVE  7   TO WS-SAT                                     
           ELSE                                                         
                MOVE  0   TO WS-SAT                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2090-CHECK-HOURS                            
      * PROGRAM WILL CHECK IF BOTH DEPARTURE AND ARRIVAL ARE IN THE SAME
      * DAY OR MAYBE THERE IN OTHER DAYS                                
      ******************************************************************
       2090-CHECK-HOURS.                                                
           MOVE WS-Z02302-DEPARTURE-TIME TO WS-TIME-CHECK-VAR           
           MOVE WS-TIME-HOUR TO WS-TEMP-HOUR1                           
           MOVE WS-TIME-MINUTE TO WS-TEMP-MINUTE1                       
           MOVE WS-Z02302-ARRIVAL-TIME TO WS-TIME-CHECK-VAR             
           MOVE WS-TIME-HOUR TO WS-TEMP-HOUR2                           
           MOVE WS-TIME-MINUTE TO WS-TEMP-HOUR2                         
                                                                        
           DISPLAY '290 CZASY '                                         
           DISPLAY 'HOUR1: '   WS-TEMP-HOUR1                            
           DISPLAY 'MINUTE1: ' WS-TEMP-MINUTE2                          
           DISPLAY 'HOUR2: '   WS-TEMP-HOUR2                            
           DISPLAY 'MINUTE2: ' WS-TEMP-MINUTE2                          
           IF WS-TEMP-HOUR1 < WS-TEMP-HOUR2                             
               DISPLAY ' 2090   WS-TEMP-HOUR1 < WS-TEMP-HOUR2 '         
               DISPLAY 'SAME DAY '                                      
               SET SO-ARRIVAL-IS-SAME-DAY TO TRUE                       
           ELSE                                                         
              IF WS-TEMP-HOUR1 =  WS-TEMP-HOUR2                         
                   IF WS-TEMP-MINUTE1 < WS-TEMP-MINUTE2 THEN            
                DISPLAY ' 2090   WS-TEMP-HOUR1 = WS-TEMP-HOUR2 '        
                DISPLAY ' 2090   WS-TEMP-MINUTE1 < WS-TEMP-MINUTE2 '    
                 DISPLAY 'SAME DAY '                                    
                      SET SO-ARRIVAL-IS-SAME-DAY TO TRUE                
                   ELSE                                                 
                       IF WS-TEMP-MINUTE1  > WS-TEMP-MINUTE2 THEN       
                DISPLAY ' 2090   WS-TEMP-MINUTE1 > WS-TEMP-MINUTE2 '    
                    DISPLAY 'NEXT DAY '                                 
                          SET SO-ARRIVAL-IS-NEXT-DAY TO TRUE            
                       ELSE                                             
                    DISPLAY 'NEXT DAY MINUTE = LUB <'                   
                          SET SO-ARRIVAL-IS-NEXT-DAY TO TRUE            
                       END-IF                                           
                   END-IF                                               
              ELSE                                                      
                DISPLAY 'ELSE NEXT DAY '                                
                 SET SO-ARRIVAL-IS-NEXT-DAY  TO TRUE                    
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2091-CONVER-LOCAL-TIME-TO-UTC                 
      * PARAGRAPH WILL MOVE TIMESTAMP IN LOCAL AIRPORTS TIMEZONES       
      * TO UTC                                                          
      ******************************************************************
       2091-CONVER-LOCAL-TIME-TO-UTC.                                   
           DISPLAY ' 2091-PREPARE-TIMESTAMPS  PERFORMED '               
           DISPLAY 'CZAS NA POCZATKU: '                                 
           DISPLAY 'DEPARTURE TIME: ' WS-Z02302-DEPARTURE-TIME          
           DISPLAY 'ARRIVAL   TIME: ' WS-Z02302-ARRIVAL-TIME            
           MOVE WS-CURRENT-DATE TO   WS-DATE                            
                                                                        
           MOVE '.' TO WS-Z02302-DEPARTURE-TIME(3:1)                    
           MOVE WS-Z02302-DEPARTURE-TIME TO WS-TIME                     
           PERFORM 2085-INITIALIZE-TIMESTAMP                            
                                                                        
           MOVE WS-TIMESTAMP-STRUCTURE TO T05-DEPARTURE-TIMESTAMP       
                                                                        
           IF SO-ARRIVAL-IS-NEXT-DAY THEN                               
                                                                        
               MOVE WS-TOMORROW-DATE TO WS-DATE                         
           ELSE                                                         
               MOVE WS-CURRENT-DATE TO WS-DATE                          
           END-IF                                                       
                                                                        
           MOVE '.' TO WS-Z02302-ARRIVAL-TIME (3:1)                     
           MOVE WS-Z02302-ARRIVAL-TIME  TO WS-TIME                      
           PERFORM 2085-INITIALIZE-TIMESTAMP                            
           MOVE WS-TIMESTAMP-STRUCTURE TO T05-ARRIVAL-TIMESTAMP         
           PERFORM 7100-MOVE-TIMESTAMP-TO-UTC                           
           DISPLAY 'T05-DEPARTURE-TIMESTAMP: ' T05-DEPARTURE-TIMESTAMP  
           DISPLAY 'T05-ARRIVAL-TIMESTAMP: ' T05-ARRIVAL-TIMESTAMP      
           DISPLAY ' 2091-PREPARE-TIMESTAMPS  ENDED     '               
           .                                                            
      ******************************************************************
      *                    2092-PREPARE-FLIGHT-DATA                     
      * EVERY FLIGHT WILL HAVE ITS UNIQUE FLIGHT-ID                     
      * BUT EVERY FLIGHT IN THOSE SCHEDULED FLIGHTS WILL HAVE           
      * THE SAME FLIGHT NUMBER                                          
      ******************************************************************
       2092-PREPARE-FLIGHT-DATA.                                        
           PERFORM 2084-GET-FLIGHT-ID                                   
           MOVE ID-NUMBER TO WS-ID-NUMBER-CHAR                          
           MOVE WS-ID-NUMBER-CHAR TO T05-FLIGHT-ID-TEXT                 
           COMPUTE T05-FLIGHT-ID-LEN  =                                 
                FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)                 
                                                                        
           IF SO-FIRST-FLIGHT-NUMBER THEN                               
              SET SO-NOT-FIRST-FLIGHT-NUMBER TO TRUE                    
             MOVE AIRLINE-CODE      TO T05-FLIGHT-NUMBER-TEXT(1:3)      
             MOVE WS-ID-NUMBER-CHAR TO T05-FLIGHT-NUMBER-TEXT(4:12)     
             COMPUTE T05-FLIGHT-NUMBER-LEN  =                           
                  FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)               
             MOVE  T05-FLIGHT-NUMBER-TEXT TO WS-PREV-FLIGHT-NUMBER      
             IF SO-FLIGHTS-TO THEN                                      
               MOVE T05-FLIGHT-NUMBER-TEXT TO WS-Z02302-FLIGHT-NUMBER-TO
             ELSE                                                       
               MOVE T05-FLIGHT-NUMBER-TEXT TO                           
                                            WS-Z02302-FLIGHT-NUMBER-FROM
             END-IF                                                     
           ELSE                                                         
             MOVE WS-PREV-FLIGHT-NUMBER TO T05-FLIGHT-NUMBER-TEXT       
             COMPUTE T05-FLIGHT-NUMBER-LEN  =                           
                  FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)               
           END-IF                                                       
           MOVE AIRLINE-CODE    TO T05-AIRLINE-CODE                     
           MOVE WS-Z02302-AIR-DES(1:3) TO T05-ARRIVAL-AIRPORT-CODE      
           MOVE WS-Z02302-AIR-ORG(1:3) TO T05-DEPARTURE-AIRPORT-CODE    
           MOVE T08-PLANE-ID TO T05-PLANE-ID                            
           MOVE 'CONFIRMED' TO T05-FLIGHT-STATUS-TEXT                   
           MOVE 9 TO T05-FLIGHT-STATUS-LEN                              
                                                                        
           .                                                            
      ******************************************************************
      *                      2093-SEND-PREPARED-MAP                     
      ******************************************************************
       2093-SEND-PREPARED-MAP.                                          
           MOVE LOW-VALUES TO DEPA-TO                                   
           MOVE LOW-VALUES TO ARVA-TO                                   
           MOVE LOW-VALUES TO MONO                                      
           MOVE LOW-VALUES TO TUEO                                      
           MOVE LOW-VALUES TO WEDO                                      
           MOVE LOW-VALUES TO THUO                                      
           MOVE LOW-VALUES TO FRIO                                      
           MOVE LOW-VALUES TO SATO                                      
           MOVE LOW-VALUES TO SUNO                                      
           MOVE AIR-ORG2O TO WS-TEMP-AIRPORT                            
           MOVE AIR-DES2O TO AIR-ORG2O                                  
           MOVE WS-TEMP-AIRPORT TO AIR-DES2O                            
           MOVE DFHBMPRO  TO AIR-DES2A                                  
           MOVE DFHBMPRO  TO AIR-ORG2A                                  
           MOVE DFHBMPRO  TO MODEL2A                                    
           MOVE DFHBMPRO  TO TYPE2A                                     
           MOVE DFHBMPRO  TO AIRLINE2A                                  
           MOVE DFHBMPRO  TO START-DA                                   
           MOVE DFHBMPRO  TO END-DA                                     
           MOVE AIR-DES2O TO WS-Z02302-AIR-DES                          
           MOVE AIR-ORG2O TO WS-Z02302-AIR-ORG                          
           PERFORM 2101-SEND-THE-MAP-SCHED                              
           .                                                            
      ******************************************************************
      *                     2095-READ-THE-QUEUE                         
      ******************************************************************
       2095-READ-THE-QUEUE.                                             
           EXEC CICS                                                    
             READQ TS                                                   
             QUEUE(CT-QUEUE-NAME)                                       
             INTO(WS-QUEUE-STRUCTURE)                                   
             ITEM(WS-WHAT-RECORD-TO-READ)                               
             NOHANDLE                                                   
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
            CONTINUE                                                    
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-DATA TO TRUE                           
           WHEN DFHRESP(QIDERR)                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'QIDERR ' TO WS-Z02141-I-ERROR-MESSAGE(1)             
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      2100-SEND-THE-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
           DISPLAY 'SEND THE MP0229 MAP '                               
           DISPLAY 'VALUE OF AIRLINEO BEFORE SEND:  ' AIRLINEO          
           EXEC CICS                                                    
           SEND MAP('MP0229') MAPSET('MP0229')                          
           FROM(MP0229O)                                                
           ERASE                                                        
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2101-SEND-THE-MAP-SCHED                      
      ******************************************************************
       2101-SEND-THE-MAP-SCHED.                                         
           EXEC CICS                                                    
           SEND MAP('MP0230') MAPSET('MP0230')                          
           FROM(MP0230O)                                                
           ERASE                                                        
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 2102-INITIALIZE-MAP-SCHED                       
      ******************************************************************
       2102-INITIALIZE-MAP-SCHED.                                       
           MOVE LOW-VALUES TO MONA                                      
           MOVE LOW-VALUES TO TUEA                                      
           MOVE LOW-VALUES TO WEDA                                      
           MOVE LOW-VALUES TO THUA                                      
           MOVE LOW-VALUES TO FRIA                                      
           MOVE LOW-VALUES TO SATA                                      
           MOVE LOW-VALUES TO SUNA                                      
           MOVE LOW-VALUES TO DEPA-TA                                   
           MOVE LOW-VALUES TO ARVA-TA                                   
           MOVE LOW-VALUES TO MODEL2A                                   
           MOVE LOW-VALUES TO TYPE2A                                    
           MOVE LOW-VALUES TO AIR-ORG2A                                 
           MOVE LOW-VALUES TO AIR-DES2A                                 
           MOVE LOW-VALUES TO AIRLINE2A                                 
           .                                                            
      ******************************************************************
      *                      2200-CHECK-EIBRESP                         
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
      * CALL DO SE ZEBY WYSWIETLIL WIADOMOSC                            
              DISPLAY 'MAPFAIL'                                         
              SET SO-Z02141-M-WITHOUT TO TRUE                           
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE ' YOU NEED TO PROVIDE  DATA '                        
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
      * CALL DO PROGRAM KTORY WYSYPUJE                                  
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              DISPLAY 'UNKNOWN EIBERSP '                                
              DISPLAY 'EIBRESP VALUE : ' EIBRESP                        
              MOVE EIBRESP TO WS-EIBRESP-TEMP                           
              STRING 'UNKOWN EIBRESP, EIBRESP VALUE ' WS-EIBRESP-TEMP   
              DELIMITED BY SIZE                                         
              INTO WS-Z02141-I-ERROR-MESSAGE(1)                         
              END-STRING                                                
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2300-CALL-ERROR-ROUTINE                       
      *  PROGRAM WILL CALL  TO THE ERROR ROTUINE.                       
      *                                                                 
      *  AFTER BEING CALLED ERROR ROUTINE PROGRAM WILL BE ABLE TO       
      *  RETURN CONTROL TO THIS PROGRAM OR TO THE FIRST PROGRAM         
      * IN THE APPLICATION (Z02131)                                     
      *                                                                 
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
           SET SO-Z02141-M-WITH      TO TRUE                            
           IF SO-GO-TO-FIRST-PROG      THEN                             
             SET SO-NOT-GO-TO-FIRST TO TRUE                             
             MOVE CT-FIRST-PROG-NAME TO WS-Z02141-I-CALLING-PROGRAM     
             SET SO-Z02141-M-NO-DATA TO TRUE                            
           ELSE                                                         
             MOVE CT-THIS-PROGRAM-NAME TO   WS-Z02141-I-CALLING-PROGRAM 
           END-IF                                                       
           SET SO-Z02141-I-FIRST-TIME  TO TRUE                          
           IF NOT SO-FLIGHTS-FROM                                       
             CONTINUE                                                   
           END-IF                                                       
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                                                         
           .       
      ******************************************************************
      *                  2301-SINGLE-FLIGHT-TO-COMMAREA                 
      * PARAGRAPH MOVES DATA OF THE SINGLE FLIGHT THAT WERE PREVIOUSLY  
      * SAVED TO THE SCREEN                                             
      *                                                                 
      * PROGRAM ALSO TRANSLATES AIRLINE IATA CODE TO FULL AIRLINE NAME  
      ******************************************************************
       2301-SINGLE-FLIGHT-TO-COMMAREA.                                  
           DISPLAY '2301 WAS PERFORMED '                                
           MOVE LOW-VALUES TO MP0229O                                   
           MOVE WS-Z02152-I-AIR-ORG          TO AIR-ORGO                
           MOVE WS-Z02152-I-AIR-DES          TO AIR-DESO                
           MOVE WS-Z02152-I-DATE-D           TO DEP-DO                  
           MOVE WS-Z02152-I-TIME-D           TO DEP-TO                  
           DISPLAY '2301   ARRIVAL DATE ' WS-Z02152-I-DATE-R            
           DISPLAY '2301   ARRIVAL TIME ' WS-Z02152-I-TIME-R            
           MOVE WS-Z02152-I-DATE-R           TO ARV-DO                  
           MOVE WS-Z02152-I-TIME-R           TO ARV-TO                  
           DISPLAY '2301   MOCEL NAME:  ' WS-Z02152-I-MODEL-NAME        
           MOVE WS-Z02152-I-MODEL-NAME       TO MODELO                  
           MOVE WS-Z02152-I-TYPE-OF-SEATS    TO WS-TEMP-NUMERIC         
           IF WS-Z02152-I-AIRLINE-CODE = SPACE OR LOW-VALUES THEN       
             MOVE WS-Z02302-AIRLINE-NAME       TO  AIRLINEO             
           ELSE                                                         
                                                                        
              MOVE WS-Z02152-I-AIRLINE-CODE TO AIRLINE-CODE             
              PERFORM 7099-GET-AIRLINE-NAME                             
              MOVE AIRLINE-NAME-TEXT TO AIRLINEO                        
              MOVE LOW-VALUES TO WS-Z02152-I-AIRLINE-CODE               
           END-IF                                                       
           MOVE WS-TEMP-NUMERIC TO TYPEO                                
           PERFORM 2080-CHECK-FOR-PLANE-MODEL                           
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                 2302-SCHEDULE-DATA-TO-COMMAREA                  
      * PARAGRAPH WILL MOVE PREVIOSULY SAVED SCHEDULED DATA TO THE      
      * SCREEN                                                          
      * THIS PARAGRAPH ALSO WILL DISPLAY NAMES THAT WERE PROVIDED       
      * BY Z02162 PROGRAM (IF THERE ARE ANY)                            
      *                                                                 
      * IF NOT PROGRAM WILL DISPLAY DATA THAT WERE SAVED EARLIER        
      ******************************************************************
       2302-SCHEDULE-DATA-TO-COMMAREA.                                  
           MOVE LOW-VALUES TO MP0230O                                   
           MOVE WS-Z02302-O-MON          TO  MONO                       
           MOVE WS-Z02302-O-TUE          TO  TUEO                       
           MOVE WS-Z02302-O-WED          TO  WEDO                       
           MOVE WS-Z02302-O-THU          TO  THUO                       
           MOVE WS-Z02302-O-FRI          TO  FRIO                       
           MOVE WS-Z02302-O-SAT          TO  SATO                       
           MOVE WS-Z02302-O-SUN          TO  SUNO                       
           MOVE WS-Z02302-DEPARTURE-TIME TO  DEPA-TO                    
           MOVE WS-Z02302-ARRIVAL-TIME   TO  ARVA-TO                    
           MOVE WS-Z02302-START-DATE     TO  START-DO                   
           MOVE WS-Z02302-END-DATE       TO  END-DO                     
           MOVE WS-Z02152-I-MODEL-NAME   TO  MODEL2O                    
           MOVE WS-Z02302-TYPE2          TO  TYPE2O                     
                                                                        
           PERFORM 2303-MOVE-AIRLINE-DATA                               
           PERFORM 2304-MOVE-AIRPORTS-DATA                              
                                                                        
           PERFORM 2080-CHECK-FOR-PLANE-MODEL                           
           PERFORM 2102-INITIALIZE-MAP-SCHED                            
                                                                        
           IF SO-FLIGHTS-FROM        THEN                               
                PERFORM 2093-SEND-PREPARED-MAP                          
           ELSE                                                         
                PERFORM 2101-SEND-THE-MAP-SCHED                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2303-MOVE-AIRLINE-DATA                       
      * IF WS-Z02152-I-AIRLINE-CODE IS NOT EMPTY IT MEANS THAT          
      *  IS MEANS THAT PROGRAM WAS CALLED BY Z02162 PROGRAM             
      * AND WE HAVE TO DISPLAY DATA THAT THIS PROGRAM GAVE AS           
      *                                                                 
      * Z02162 PROGRAM ALLOW USER TO CHOOSE SIMILAR NAME TO WHAT        
      * HE PROVIDED                                                     
      ******************************************************************
       2303-MOVE-AIRLINE-DATA.                                          
           IF WS-Z02152-I-AIRLINE-CODE = SPACE OR LOW-VALUES THEN       
             MOVE WS-Z02302-AIRLINE-NAME       TO  AIRLINE2O            
           ELSE                                                         
              MOVE WS-Z02152-I-AIRLINE-CODE TO AIRLINE-CODE             
              PERFORM 7099-GET-AIRLINE-NAME                             
              MOVE AIRLINE-NAME-TEXT TO AIRLINE2O                       
              DISPLAY 'AIRLINE2O : ' AIRLINE2O                          
              MOVE LOW-VALUES TO WS-Z02152-I-AIRLINE-CODE               
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2304-MOVE-AIRPORTS-DATA                    
      *   IF WS-Z02152-I-AIR-ORG AND WS-Z02152-I-AIR-DES IS NOT EMPTY   
      *                                                                 
      *  IT MEANS THAT THIS PROGRAM WAS CALLED BY Z02162 PROGRAM:       
      *                                                                 
      *  IN THIS PROGRAM USER CHOOSE AIRPORT NAME AND PRESSED ENTER     
      * THEN Z02162 CALLED TO THIS PROGRAM AND NOW WE HAVE TO DISPLAY   
      * THIS DATA ON THE SCREEN                                         
      ******************************************************************
       2304-MOVE-AIRPORTS-DATA.                                         
           IF WS-Z02152-I-AIR-ORG = SPACE OR LOW-VALUES THEN            
             MOVE WS-Z02302-AIR-ORG        TO  AIR-ORG2O                
           ELSE                                                         
             MOVE WS-Z02152-I-AIR-ORG      TO AIR-ORG2O                 
           END-IF                                                       
           IF WS-Z02152-I-AIR-DES = SPACE OR LOW-VALUES THEN            
                 MOVE WS-Z02302-AIR-DES        TO  AIR-DES2O            
           ELSE                                                         
                 MOVE WS-Z02152-I-AIR-DES      TO AIR-DES2O             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2305-RECEIVE-SINGLE-FLIGHT-MAP                 
      ******************************************************************
       2305-RECEIVE-SINGLE-FLIGHT-MAP.                                  
           MOVE LOW-VALUES TO MP0229I                                   
           EXEC CICS                                                    
             RECEIVE MAP('MP0229') MAPSET('MP0229')                     
             INTO(MP0229I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2306-RECEIVE-SCHEDULE-MAP                     
      ******************************************************************
       2306-RECEIVE-SCHEDULE-MAP.                                       
           MOVE LOW-VALUES TO MP0230I                                   
           EXEC CICS                                                    
             RECEIVE MAP('MP0230') MAPSET('MP0230')                     
             INTO(MP0230I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2307-VALIDATE-SINGLE-FLIGHT                    
      ******************************************************************
       2307-VALIDATE-SINGLE-FLIGHT.                                     
           PERFORM 2308-VALIDATE-AIRPORTS                               
           PERFORM 2311-IF-AIRPORTS-DIFFERENT                           
      * HERE PROGRAM WILL CHECK IF NAME OF PLANE MODEL PROVIDED BY THE  
      * USER IS VALID OR NOT                                            
           PERFORM 2312-CHECK-MODEL-NAME                                
           PERFORM 2313-CHECK-IF-MODEL-VALID                            
           PERFORM 2315-FLIGHT-DISTANCE                                 
           PERFORM 2082-CHECK-TIME-OF-FLIGHT                            
      * PARAGRAPH WILL CHECK IF THIS FLIGHT CAN FLY IN GIVEN DATES      
      * IF DATE OF DEPARTURE IS BEFORE DATE OF ARRIVAL                  
           PERFORM 7105-CHECK-IF-DATES-POSSIBLE                         
           PERFORM 2316-VALIDATE-AIRLINE-NAME                           
           PERFORM 2367-PREPARE-FLIGHT-DATA                             
      * PARAGRAPH WILL CHECK IF THIS FLIGHT WILL TAKE <= 24 HOURS       
                                                                        
           PERFORM 7106-VALIDATE-TIME-OF-FLIGHT                         
           PERFORM 2083-ADD-A-FLIGHT                                    
           .                                                            
      ******************************************************************
      *                   2308-VALIDATE-AIRPORTS                        
      ******************************************************************
       2308-VALIDATE-AIRPORTS.                                          
           PERFORM 2309-VALIDATE-ORIGIN-AIRPORT                         
           PERFORM 2310-VALIDATE-DEST-AIRPORT                           
           .                                                            
      ******************************************************************
      *                  2309-VALIDATE-ORIGIN-AIRPORT                   
      ******************************************************************
       2309-VALIDATE-ORIGIN-AIRPORT.                                    
           MOVE WS-AIR-ORG TO WS-AIRPORT-VALUE                          
           PERFORM 2077-VALIDATE-AIRPORT-CODE                           
           IF SO-INVALID-IATA THEN                                      
              MOVE WS-AIR-ORG TO WS-SEARCHED-PHRASE-AIRPORT             
              SET SO-CHECK-AIR-OIGIN  TO TRUE                           
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           ELSE                                                         
              MOVE WS-AIRPORT-VALUE TO WS-Z02152-I-AIR-ORG              
              MOVE WS-AIRPORT-FULL-NAME TO WS-Z02152-I-ORG-NAM          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2310-VALIDATE-DEST-AIRPORT                    
      ******************************************************************
       2310-VALIDATE-DEST-AIRPORT.                                      
           INITIALIZE WS-AIRPORT-VALUE                                  
           INITIALIZE WS-AIRPORT-FULL-NAME                              
                                                                        
           MOVE WS-AIR-DES TO WS-AIRPORT-VALUE                          
           PERFORM 2077-VALIDATE-AIRPORT-CODE                           
           IF SO-INVALID-IATA THEN                                      
                MOVE WS-AIR-DES TO WS-SEARCHED-PHRASE-AIRPORT           
                SET SO-CHECK-AIR-DESTINATION TO TRUE                    
                PERFORM 2500-CALL-TO-VALIDATE-NAMES                     
           ELSE                                                         
                MOVE WS-AIRPORT-VALUE TO WS-Z02152-I-AIR-DES            
                MOVE WS-AIRPORT-FULL-NAME TO WS-Z02152-I-DES-NAM        
                DISPLAY ' IN DES CHECK : ' WS-Z02152-I-DES-NAM          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2311-IF-AIRPORTS-DIFFERENT                     
      * PARAGRAPH WILL CHECK IF USER WANTS TO FLY TO THE SAME           
      * AIRPORT                                                         
      ******************************************************************
       2311-IF-AIRPORTS-DIFFERENT.                                      
           IF WS-Z02152-I-AIR-DES = WS-Z02152-I-AIR-ORG THEN            
             MOVE 'YOU CANT FLY TO THE SAME AIRPORT '                   
                            TO WS-Z02141-I-ERROR-MESSAGE(1)             
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2312-CHECK-MODEL-NAME                          
      ******************************************************************
       2312-CHECK-MODEL-NAME.                                           
            MOVE WS-PLANE-MODEL TO T08-PLANE-MODEL-TEXT                 
            COMPUTE T08-PLANE-MODEL-LEN =                               
                FUNCTION LENGTH(T08-PLANE-MODEL-TEXT)                   
           PERFORM 7002-CHECK-MODEL-NAME                                
           IF SO-MODEL-NAME-INVALID THEN                                
              SET SO-CHECK-PLANE-MODEL TO TRUE                          
              MOVE T08-PLANE-MODEL-TEXT TO WS-SEARCHED-PHRASE-MODEL     
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2313-CHECK-IF-MODEL-VALID                       
      ******************************************************************
       2313-CHECK-IF-MODEL-VALID.                                       
           PERFORM 2314-PREPARE-SEAT-TYPE-DATA                          
           PERFORM 7001-CHECK-IF-MODEL-VALID                            
           .                                                            
      ******************************************************************
      *                   2314-PREPARE-SEAT-TYPE-DATA                   
      ******************************************************************
       2314-PREPARE-SEAT-TYPE-DATA.                                     
           DISPLAY '2314 CHECK WS-TYPE-OF-SEATS: '                      
           DISPLAY 'WS-TYPE OF SEATS: ' WS-TYPE-OF-SEATS                
           IF FUNCTION TEST-NUMVAL(WS-TYPE-OF-SEATS) =  0 THEN          
             DISPLAY 'WPADAMY W TEST-NUMVAL 0  '                        
            COMPUTE WS-TYPE-OF-SEATS-NUMERIC  =                         
                      FUNCTION NUMVAL(WS-TYPE-OF-SEATS)                 
             IF WS-TYPE-OF-SEATS-NUMERIC <= 00 THEN                     
                DISPLAY 'INVALID TYPE OF SEATS'                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID TYPE OF SEATS      ' TO                     
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
             END-IF                                                     
                                                                        
           ELSE                                                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID SEAT TYPE          ' TO                     
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           DISPLAY 'MOVE WS-TYPE-OF-SEATS NUMERIC TO T08'               
           MOVE WS-TYPE-OF-SEATS-NUMERIC TO T08-TYPE-OF-SEATS-ID        
           DISPLAY 'AFTER MOVE TO T08'                                  
           .                                                            
      ******************************************************************
      *                     2315-FLIGHT-DISTANCE                        
      * PARAGRAPH WILL CHECK IF MAXIMAL RANGE OF THE PLANE IS ENOUGH    
      * FOR THE DISTANCE BETWEEN AIRPORTS THAT USER CHOOSE              
      ******************************************************************
       2315-FLIGHT-DISTANCE.                                            
           PERFORM 2081-CALCULATE-DISTANCE                              
           MOVE WS-D TO WS-DISPLAY-D                                    
           DISPLAY 'DISTANCE IS : ' WS-DISPLAY-D                        
      * CHECK IF AIRPLANE MAXIMAL RANGE IS EQUAL OR LESS THAN           
      * DISTANCE BETWEEN AIRPORTS                                       
           IF AIRPLANE-RANGE >= WS-D THEN                               
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'AIRPLANE RANGE IS TO SHORT  '                       
                                TO          WS-Z02141-I-ERROR-MESSAGE(1)
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2316-VALIDATE-AIRLINE-NAME                    
      ******************************************************************
       2316-VALIDATE-AIRLINE-NAME.                                      
           MOVE WS-AIRLINE TO AIRLINE-NAME-TEXT                         
           COMPUTE AIRLINE-NAME-LEN =                                   
              FUNCTION LENGTH(AIRLINE-NAME-TEXT)                        
           PERFORM 7011-CHECK-AIRLINE                                   
           .                                                            
      ******************************************************************
      *                  2317-VALIDATE-WEEK-DAYS                        
      ******************************************************************
       2317-VALIDATE-WEEK-DAYS.                                         
           IF WS-Z02302-O-TUE  NOT = 'X'  AND                           
              WS-Z02302-O-WED  NOT = 'X'  AND                           
              WS-Z02302-O-THU  NOT = 'X'  AND                           
              WS-Z02302-O-FRI  NOT = 'X'  AND                           
              WS-Z02302-O-SAT  NOT = 'X'  AND                           
              WS-Z02302-O-SUN  NOT = 'X'  THEN                          
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PLACE X NAXT TO WEEK DAY ' TO                        
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2318-CHECK-SCHEDULE-AIRPORTS                    
      * PARAGRAPH WILL VALIDATE IF ORIGIN AND DEPARTURE AIRPORTS ARE    
      * VALID                                                           
      ******************************************************************
       2318-CHECK-SCHEDULE-AIRPORTS.                                    
           MOVE WS-Z02302-AIR-ORG TO WS-AIRPORT-VALUE                   
           PERFORM 2077-VALIDATE-AIRPORT-CODE                           
           IF SO-INVALID-IATA THEN                                      
                                                                        
              MOVE WS-Z02302-AIR-ORG TO WS-SEARCHED-PHRASE-AIRPORT      
              SET SO-CHECK-AIR-OIGIN  TO TRUE                           
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           ELSE                                                         
              MOVE  WS-Z02302-AIR-ORG TO T05-DEPARTURE-AIRPORT-CODE     
           END-IF                                                       
                                                                        
                                                                        
           INITIALIZE WS-AIRPORT-VALUE                                  
           INITIALIZE WS-AIRPORT-FULL-NAME                              
                                                                        
           MOVE WS-Z02302-AIR-DES TO WS-AIRPORT-VALUE                   
           PERFORM 2077-VALIDATE-AIRPORT-CODE                           
           IF SO-INVALID-IATA THEN                                      
              MOVE WS-Z02302-AIR-DES   TO WS-SEARCHED-PHRASE-AIRPORT    
              SET SO-CHECK-AIR-DESTINATION TO TRUE                      
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           ELSE                                                         
              MOVE WS-Z02302-AIR-DES TO T05-ARRIVAL-AIRPORT-CODE        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2319-VALIDATE-SCHEDULE-DATES                     
      * PARAGRAPH WILL CHECK IF START AND END DATES OF A SCHEDULE       
      * ARE VALID                                                       
      ******************************************************************
       2319-VALIDATE-SCHEDULE-DATES.                                    
           MOVE WS-Z02302-START-DATE  TO ZZEC0243-I-DATE-VALUE          
           SET ZZEC0243-M-10-CHAR  TO TRUE                              
           PERFORM 2350-VALIDATE-DATE                                   
                                                                        
           MOVE WS-Z02302-END-DATE    TO ZZEC0243-I-DATE-VALUE          
           SET ZZEC0243-M-10-CHAR  TO TRUE                              
           PERFORM 2350-VALIDATE-DATE                                   
           .                                                            
      ******************************************************************
      *                  2320-VALIDATE-SCHEDULE-MODEL                   
      ******************************************************************
       2320-VALIDATE-SCHEDULE-MODEL.                                    
           MOVE WS-Z02152-I-MODEL-NAME TO T08-PLANE-MODEL-TEXT          
           COMPUTE T08-PLANE-MODEL-LEN =                                
                FUNCTION LENGTH(T08-PLANE-MODEL-TEXT)                   
           PERFORM 7002-CHECK-MODEL-NAME                                
                                                                        
           IF SO-MODEL-NAME-INVALID THEN                                
              SET SO-CHECK-PLANE-MODEL TO TRUE                          
              MOVE T08-PLANE-MODEL-TEXT TO WS-SEARCHED-PHRASE-MODEL     
              DISPLAY 'OPCJA DRUGA CHECK PLANE MODEL '                  
              DISPLAY 'T08-PLANE MODEL: ' T08-PLANE-MODEL-TEXT          
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           END-IF                                                       
                                                                        
           PERFORM 2074-CHECK-IF-VALID-MODEL                            
           .                                                            
      ******************************************************************
      *                   2321-VALIDATE-USER-TIME                       
      ******************************************************************
       2321-VALIDATE-USER-TIME.                                         
           MOVE WS-Z02302-DEPARTURE-TIME TO WS-TIME-CHECK-VAR           
           PERFORM 2360-CHECK-TIME                                      
           IF NOT SO-TIME-IS-VALID THEN                                 
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'INVALID TIME     ' TO                                
                                        WS-Z02141-I-ERROR-MESSAGE(1)    
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
                                                                        
           MOVE WS-Z02302-ARRIVAL-TIME   TO WS-TIME-CHECK-VAR           
           PERFORM 2360-CHECK-TIME                                      
           IF NOT SO-TIME-IS-VALID THEN                                 
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'INVALID TIME     ' TO                                
                                        WS-Z02141-I-ERROR-MESSAGE(1)    
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2322-VALIDATE-SCHEDULE-AIRLINE                
      ******************************************************************
       2322-VALIDATE-SCHEDULE-AIRLINE.                                  
           MOVE WS-Z02302-AIRLINE-NAME  TO AIRLINE-NAME-TEXT            
           COMPUTE AIRLINE-NAME-LEN =                                   
              FUNCTION LENGTH(AIRLINE-NAME-TEXT)                        
           PERFORM 7011-CHECK-AIRLINE                                   
           .                                                            
      ******************************************************************
      *                 2323-IF-SCHEDULE-AIRPORTS-DIF                   
      ******************************************************************
       2323-IF-SCHEDULE-AIRPORTS-DIF.                                   
           IF WS-Z02302-AIR-DES = WS-Z02302-AIR-ORG  THEN               
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU CANT FLY TO THE SAME AIRPORT '                  
                           TO          WS-Z02141-I-ERROR-MESSAGE(1)     
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2324-CHECK-SCHEDULE-DISTANCE                  
      * PARAGRAPH WILL CHECK IF THE CHOSEN FLIGHT CAN FLY AT THIS       
      * DISTANCE                                                        
      ******************************************************************
       2324-CHECK-SCHEDULE-DISTANCE.                                    
           MOVE WS-Z02302-AIR-DES(1:3) TO WS-AIR-DES                    
           MOVE WS-Z02302-AIR-ORG(1:3) TO WS-AIR-ORG                    
           PERFORM 2081-CALCULATE-DISTANCE                              
           IF AIRPLANE-RANGE >= WS-D THEN                               
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'AIRPLANE RANGE IS TO SHORT  '                       
                             TO          WS-Z02141-I-ERROR-MESSAGE(1)   
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2325-CONTROL-THE-DATA-FLOW                      
      * PARAGRAPH CONTROL LOGIC                                         
      * THANKS TO THAT USER WILL BE ABLE TO FIRST PROVIDE DATA FOR      
      * "TO" FLIGHT AND AFTER PRESSING ENTER FOR A "FROM" FLIGHTS       
      *                                                                 
      *                                                                 
      * PARAGRAPH WORKS BY CHECKING A FLAGS                             
      *   HERE WE HAVE 4 OPTIONS                                        
      *                                                                 
      *   1. USER PROVIDED DATA FOR FLIGHTS "TO" BUT NO FLIGHT DATA     
      * WAS GENREATED                                                   
      *   2. USER PROVIDED DATA FOR FLIGHTS "FROM" BUT NO FLIGHT DATA   
      *  WAS GENERATED                                                  
      *                                                                 
      *   3. USER PROVIDED DATA FOR FLIGHTS "FROM" AND DATA WAS ADDED   
      * TO THE QUEUE                                                    
      *                                                                 
      *   4. OTHER OPTIONS (PROGRAM GOES THERE FOR EXAMPLE WHEN         
      *   USER PROVIDED DATA FOR "TO" FLIGHTS AND DATA WAS GENERATED    
      *   IT IS NOT AN ERROR                                            
      ******************************************************************
       2325-CONTROL-THE-DATA-FLOW.                                      
           EVALUATE TRUE                                                
             WHEN SO-FLIGHTS-TO AND SO-FLIGHTS-TO-NOT-ADDED             
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE '"TO" FLIGHT COULD NOT BE GENERATED(INVALID DATA) ' 
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             WHEN SO-FLIGHTS-FROM AND SO-FLIGHTS-FROM-NOT-ADDED         
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE '"FROM" FLIGHT COULD NOT BE GENERATED(INVALID DATA)'
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             WHEN SO-FLIGHTS-FROM AND SO-FLIGHTS-FROM-WAS-ADDED         
      * HERE PROGRAM WILL INSERT ALL DATA INTO DATABSE                  
               PERFORM 7021-INSERT-ALL-FLIGHTS-DATA                     
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'AMOUNT OF FLIGHTS THAT WERE ADDED:         '       
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               MOVE WS-ITERX TO WS-ITER-FORMAT                          
               MOVE WS-ITER-FORMAT TO WS-Z02141-I-ERROR-MESSAGE(2)      
               SET SO-GO-TO-FIRST-PROG TO TRUE                          
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             WHEN OTHER                                                 
               SET SO-M-NOT-FIRST            TO TRUE                    
               DISPLAY 'FALGA PO ZMIANIE: ' SW-M-WHAT-MODE              
               SET SO-FLIGHTS-FROM           TO TRUE                    
               SET SO-FLIGHTS-FROM-NOT-ADDED TO TRUE                    
               MOVE WS-ZZEC0215 TO DFHCOMMAREA                          
               PERFORM 2093-SEND-PREPARED-MAP                           
               SET  SO-FINAL-WITH-COMMAREA TO TRUE                      
             END-EVALUATE                                               
           .                                                            
      ******************************************************************
      *                   2326-IF-ORIGIN-AIRPORT-EMPTY                  
      ******************************************************************
       2326-IF-ORIGIN-AIRPORT-EMPTY.                                    
           IF AIR-ORGI = SPACE OR LOW-VALUES THEN                       
                                                                        
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROVIDE ORIGIN AIRPORT   ' TO                      
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           ELSE                                                         
               MOVE AIR-ORGI TO WS-AIR-ORG                              
               INSPECT WS-AIR-ORG REPLACING ALL '_' BY ' '              
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2329-IF-DEST-AIRPORT-EMPTY                      
      ******************************************************************
       2329-IF-DEST-AIRPORT-EMPTY.                                      
           IF AIR-DESI = SPACE OR LOW-VALUES THEN                       
              SET SO-AIR-DES-EMPTY TO TRUE                              
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROVIDE DESTINATION AIRPORT ' TO                   
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           ELSE                                                         
              MOVE AIR-DESI TO WS-AIR-DES                               
              INSPECT WS-AIR-DES REPLACING ALL '_' BY ' '               
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2330-IF-DEPARTURE-DATE-EMPTY                      
      ******************************************************************
       2330-IF-DEPARTURE-DATE-EMPTY.                                    
           IF DEP-DI = SPACE OR LOW-VALUES THEN                         
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROVIDE DEPARTURE DATE      ' TO                   
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           ELSE                                                         
              MOVE DEP-DI TO WS-DEPARTURE-DATE                          
               SET ZZEC0243-M-10-CHAR  TO TRUE                          
               MOVE WS-DEPARTURE-DATE TO ZZEC0243-I-DATE-VALUE          
               PERFORM 2350-VALIDATE-DATE                               
               MOVE WS-DEPARTURE-DATE TO WS-Z02152-I-DATE-D             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2331-IF-DEPARTURE-TIME-EMPTY                     
      ******************************************************************
       2331-IF-DEPARTURE-TIME-EMPTY.                                    
           IF DEP-TI = SPACE OR LOW-VALUES THEN                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE DEPARTURE TIME      ' TO                    
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE DEP-TI TO WS-DEPARTURE-TIME                          
              MOVE WS-DEPARTURE-TIME TO WS-TIME-CHECK-VAR               
              PERFORM 2360-CHECK-TIME                                   
              IF SO-TIME-IS-VALID THEN                                  
                CONTINUE                                                
              ELSE                                                      
                PERFORM 2700-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'INVALID TIME  ' TO                                
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
              END-IF                                                    
              MOVE WS-DEPARTURE-TIME TO WS-Z02152-I-TIME-D              
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2332-IF-ARRIVAL-DATE-EMPTY                      
      ******************************************************************
       2332-IF-ARRIVAL-DATE-EMPTY.                                      
                                                     
           IF ARV-DI = SPACE OR LOW-VALUES THEN                         
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROVIDE ARRIVAL DATE      ' TO                     
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           ELSE                                                         
              MOVE ARV-DI TO WS-ARRIVAL-DATE                            
              SET ZZEC0243-M-10-CHAR  TO TRUE                           
              MOVE WS-ARRIVAL-DATE    TO ZZEC0243-I-DATE-VALUE          
              PERFORM 2350-VALIDATE-DATE                                
              MOVE WS-ARRIVAL-DATE    TO WS-Z02152-I-DATE-R             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2333-IF-ARRIVAL-TIME-EMPTY                      
      ******************************************************************
       2333-IF-ARRIVAL-TIME-EMPTY.                                      
           IF ARV-TI = SPACE OR LOW-VALUES THEN                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE ARRIVAL  TIME      ' TO                     
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH     TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE ARV-TI              TO WS-ARRIVAL-TIME               
                                                                        
              MOVE WS-ARRIVAL-TIME     TO WS-TIME-CHECK-VAR             
              PERFORM 2360-CHECK-TIME                                   
              IF SO-TIME-IS-VALID THEN                                  
                CONTINUE                                                
              ELSE                                                      
                PERFORM 2700-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'INVALID TIME  ' TO                                
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
              END-IF                                                    
              MOVE WS-ARRIVAL-TIME TO WS-Z02152-I-TIME-R                
              DISPLAY '2333 MOVE TO TIME-R : '                          
              DISPLAY 'WS-ARRIVAL-TIME: '    WS-ARRIVAL-TIME            
              DISPLAY 'WS-Z02152-I-TIME-R: ' WS-Z02152-I-TIME-R         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2334-IF-PLANE-MODEL-EMPTY                       
      ******************************************************************
       2334-IF-PLANE-MODEL-EMPTY.                                       
           IF MODELI = SPACE OR LOW-VALUES THEN                         
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE PLANE MODEL NAME       ' TO                 
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE MODELI TO WS-PLANE-MODEL                             
              INSPECT WS-PLANE-MODEL    REPLACING ALL '_' BY ' '        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2335-IF-TYPE-OF-SEATS-EMPTY                    
      ******************************************************************
       2335-IF-TYPE-OF-SEATS-EMPTY.                                     
           IF TYPEI = SPACE OR LOW-VALUES THEN                          
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE TYPE OF SEATS        ' TO                   
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE TYPEI TO WS-TYPE-OF-SEATS                            
              INSPECT WS-TYPE-OF-SEATS  REPLACING ALL '_' BY ' '        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2336-IF-AIRLINE-NAME-EMPTY                     
      ******************************************************************
       2336-IF-AIRLINE-NAME-EMPTY.                                      
           IF AIRLINEI = SPACE OR LOW-VALUES THEN                       
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE ARILINE NAME    ' TO                        
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE AIRLINEI TO WS-AIRLINE                               
              MOVE WS-AIRLINE TO WS-Z02302-AIRLINE-NAME                 
              INSPECT WS-AIRLINE           REPLACING ALL '_' BY ' '     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2337-GET-GOGRAFICAL-POSITIONS                  
      ******************************************************************
       2337-GET-GOGRAFICAL-POSITIONS.                                   
           MOVE WS-AIR-DES TO T02-AIRPORT-CODE                          
           PERFORM 7010-GET-GEOGRAF-POS                                 
           MOVE WS-LATITUDE TO WS-DES-LATITUDE                          
           MOVE WS-LONGITUDE TO WS-DES-LONGITUDE                        
           MOVE WS-AIR-ORG TO T02-AIRPORT-CODE                          
           PERFORM 7010-GET-GEOGRAF-POS                                 
           MOVE WS-LATITUDE TO WS-ORG-LATITUDE                          
           MOVE WS-LONGITUDE TO WS-ORG-LONGITUDE                        
           .                                                            
      ******************************************************************
      *                  2338-MOVE-DEGREES-TO-RADIANS                   
      ******************************************************************
       2338-MOVE-DEGREES-TO-RADIANS.                                    
           COMPUTE WS-RAD-DES-LATITUDE =                                
                (CT-PI-VALUE * WS-DES-LATITUDE) / 180                   
           COMPUTE WS-RAD-DES-LONGITUDE =                               
                (CT-PI-VALUE * WS-DES-LONGITUDE) / 180                  
           COMPUTE WS-RAD-ORG-LATITUDE =                                
                (CT-PI-VALUE * WS-ORG-LATITUDE)  / 180                  
           COMPUTE WS-RAD-ORG-LONGITUDE =                               
                (CT-PI-VALUE * WS-ORG-LONGITUDE)  / 180                 
           .                                                            
      ******************************************************************
      *                  2339-IF-WEEK-DAYS-EMPTY                        
      ******************************************************************
       2339-IF-WEEK-DAYS-EMPTY.                                         
           IF MONI NOT  = SPACE OR LOW-VALUES AND                       
               TUEI NOT   = SPACE OR LOW-VALUES AND                     
              WEDI  NOT  = SPACE OR LOW-VALUES AND                      
              THUI NOT   = SPACE OR LOW-VALUES AND                      
              FRII NOT   = SPACE OR LOW-VALUES AND                      
              SATI NOT   = SPACE OR LOW-VALUES AND                      
              SUNI NOT   = SPACE OR LOW-VALUES                          
           THEN                                                         
             MOVE MONI TO  WS-Z02302-O-MON                              
             MOVE TUEI TO  WS-Z02302-O-TUE                              
             MOVE WEDI TO  WS-Z02302-O-WED                              
             MOVE THUI TO  WS-Z02302-O-THU                              
             MOVE FRII TO  WS-Z02302-O-FRI                              
             MOVE SATI TO  WS-Z02302-O-SAT                              
             MOVE SUNI TO  WS-Z02302-O-SUN                              
                                                                        
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE DATE OF THE WEEK ' TO                        
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2340-IF-DEPARTURE-TIME-EMPTY                    
      ******************************************************************
       2340-IF-DEPARTURE-TIME-EMPTY.                                    
           IF DEPA-TI NOT = SPACE OR LOW-VALUES THEN                    
              MOVE DEPA-TI TO WS-Z02302-DEPARTURE-TIME                  
              INSPECT WS-Z02302-DEPARTURE-TIME REPLACING ALL '_' BY ' ' 
                                                                        
              IF SO-FLIGHTS-TO THEN                                     
               MOVE WS-Z02302-DEPARTURE-TIME TO WS-Z02302-FIRST-DEP-TIME
              ELSE                                                      
               MOVE WS-Z02302-DEPARTURE-TIME TO WS-Z02302-SEC-DEP-TIME  
              END-IF                                                    
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE TIME  ' TO                                   
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2341-IF-ARRIVAL-TIME-EMPTY                   
      ******************************************************************
       2341-IF-ARRIVAL-TIME-EMPTY.                                      
           IF ARVA-TI NOT = SPACE OR LOW-VALUES THEN                    
              MOVE ARVA-TI TO WS-Z02302-ARRIVAL-TIME                    
              INSPECT WS-Z02302-ARRIVAL-TIME   REPLACING ALL '_' BY ' ' 
              IF SO-FLIGHTS-TO THEN                                     
                MOVE WS-Z02302-ARRIVAL-TIME  TO WS-Z02302-FIRST-ARV-TIME
              ELSE                                                      
                MOVE WS-Z02302-ARRIVAL-TIME  TO WS-Z02302-SEC-ARV-TIME  
              END-IF                                                    
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE TIME  ' TO                                   
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2342-IF-SCHEDULE-DATES-EMPTY                  
      ******************************************************************
       2342-IF-SCHEDULE-DATES-EMPTY.                                    
           IF START-DI NOT = SPACE OR LOW-VALUES THEN                   
              MOVE START-DI TO WS-Z02302-START-DATE                     
              INSPECT WS-Z02302-START-DATE   REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE START DATE ' TO                              
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           IF END-DI NOT = SPACE OR LOW-VALUES THEN                     
              MOVE END-DI TO WS-Z02302-END-DATE                         
              INSPECT WS-Z02302-END-DATE     REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE END DATE ' TO                                
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2343-IF-PLANE-MODEL2-EMPTY                   
      ******************************************************************
       2343-IF-PLANE-MODEL2-EMPTY.                                      
           IF MODEL2I NOT = SPACE OR LOW-VALUES THEN                    
              MOVE MODEL2I TO WS-Z02152-I-MODEL-NAME                    
              INSPECT WS-Z02152-I-MODEL-NAME REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE END DATE ' TO                                
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2344-IF-TYPE-OF-SEAYS2-EMPTY                
      ******************************************************************
       2344-IF-TYPE-OF-SEAYS2-EMPTY.                                    
           IF TYPE2I NOT = SPACE OR LOW-VALUES THEN                     
              MOVE TYPE2I TO WS-Z02302-TYPE2                            
              DISPLAY 'TYPE2I: ' TYPE2I                                 
              DISPLAY 'TYPE2I TO Z02302TYPE2: '  WS-Z02302-TYPE2        
                                                                        
              MOVE TYPE2I TO WS-TYPE-OF-SEATS                           
              INSPECT WS-TYPE-OF-SEATS       REPLACING ALL '_' BY ' '   
              INSPECT WS-Z02302-TYPE2        REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE END DATE ' TO                                
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2345-IF-AIRLINE2-EMPTY                      
      ******************************************************************
       2345-IF-AIRLINE2-EMPTY.                                          
           IF AIRLINE2I NOT = SPACE OR LOW-VALUES THEN                  
              MOVE AIRLINE2I TO WS-Z02302-AIRLINE-NAME                  
              INSPECT WS-Z02302-AIRLINE-NAME REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE END DATE ' TO                                
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2346-IF-AIRPORTS2-EMPTY                       
      ******************************************************************
       2346-IF-AIRPORTS2-EMPTY.                                         
           IF AIR-ORG2I NOT = SPACE OR LOW-VALUES THEN                  
              MOVE AIR-ORG2I TO WS-Z02302-AIR-ORG                       
              INSPECT WS-Z02302-AIR-ORG      REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE AIRPORT NAME ' TO                            
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           IF AIR-DES2I NOT = SPACE OR LOW-VALUES THEN                  
              MOVE AIR-DES2I TO WS-Z02302-AIR-DES                       
              INSPECT WS-Z02302-AIR-DES      REPLACING ALL '_' BY ' '   
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'PROVIDE AIRPORT NAME ' TO                            
                                          WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2347-MOVE-SCHED-FLIGHT-DATA                    
      * PARAGRAPH WILL MOVE SCHEDULE DATA INTO THE DB2 VARIABLES        
      *                                                                 
      ******************************************************************
       2347-MOVE-SCHED-FLIGHT-DATA.                                     
           MOVE WS-Z02302-START-DATE TO START-SCHEDULE-DATE             
           MOVE WS-Z02302-END-DATE TO END-SCHEDULE-DATE                 
           MOVE T05-PLANE-ID   TO PLANE-ID                              
           MOVE ' ' TO WEEK-DAYS-TEXT                                   
           MOVE 1 TO WEEK-DAYS-LEN                                      
           MOVE WS-Z02302-AIR-ORG(1:3) TO ORIGIN-AIRPORT-CODE           
           MOVE WS-Z02302-AIR-DES(1:3) TO DESTINATION-AIRPORT-CODE      
           MOVE WS-Z02302-FLIGHT-NUMBER-TO TO FLIGHT-NUMBER-TO-TEXT     
           COMPUTE FLIGHT-NUMBER-TO-LEN =                               
                   FUNCTION LENGTH(FLIGHT-NUMBER-TO-TEXT)               
           MOVE WS-Z02302-FLIGHT-NUMBER-FROM                            
                            TO FLIGHT-NUMBER-FROM-TEXT                  
           COMPUTE FLIGHT-NUMBER-FROM-LEN =                             
                   FUNCTION LENGTH(FLIGHT-NUMBER-FROM-TEXT)             
           MOVE WS-Z02302-FIRST-DEP-TIME TO WS-TIME-DB-FORMAT           
           MOVE '.' TO WS-TIME-DB-FILLER1                               
           MOVE '.' TO WS-TIME-DB-FILLER2                               
           MOVE 00 TO WS-TIME-DB-SECOND                                 
           MOVE WS-TIME-DB-FORMAT TO DEPARTURE-TIME-ORIGIN              
                                                                        
           MOVE WS-Z02302-FIRST-ARV-TIME TO WS-TIME-DB-FORMAT           
           MOVE '.' TO WS-TIME-DB-FILLER1                               
           MOVE '.' TO WS-TIME-DB-FILLER2                               
           MOVE 00 TO WS-TIME-DB-SECOND                                 
           MOVE WS-TIME-DB-FORMAT TO ARRIVAL-TIME-DESTINATION           
           MOVE WS-Z02302-SEC-DEP-TIME TO WS-TIME-DB-FORMAT             
           MOVE '.' TO WS-TIME-DB-FILLER1                               
           MOVE '.' TO WS-TIME-DB-FILLER2                               
           MOVE 00 TO WS-TIME-DB-SECOND                                 
           MOVE WS-TIME-DB-FORMAT TO DEPARTURE-TIME-DESTINATION         
                                                                        
                                                                        
           MOVE WS-Z02302-SEC-ARV-TIME TO WS-TIME-DB-FORMAT             
           MOVE '.' TO WS-TIME-DB-FILLER1                               
           MOVE '.' TO WS-TIME-DB-FILLER2                               
           MOVE 00 TO WS-TIME-DB-SECOND                                 
           MOVE WS-TIME-DB-FORMAT TO ARRIVAL-TIME-ORIGIN                
           MOVE CT-SCHEDULED-STATUS TO SCHEDULED-STATUS                 
           .                                                            
      ******************************************************************
      *                  2348-PREPARE-TIME-OFFSET                       
      * PARAGRAPH WILL PREPARE TIME OFFSET                              
      *                                                                 
      * VARIABLE T02-TIME-ZONE STORES DATA IN FORMAT SHH.MM             
      * WHERE S MEANS THE SIGN, HH MEANS AMOUNT OF HOURS, AND           
      * MM MEANS AMOUNT OF MINUTES                                      
      *                                                                 
      * PROGRAM WILL PREPARE THAT DATA TO BE USED IN SQL STATEMNT       
      * IN THIS SQL WE WILL USE TIMESTAMPADD FUNCTION THAT WILL         
      * ALLOW THE PROGRAM TO MOVE TIMESTAMPS FROM LOCAL TIME ZONE       
      * TO UTC TIME ZONE                                                
      *                                                                 
      ******************************************************************
       2348-PREPARE-TIME-OFFSET.                                        
           INITIALIZE WS-HOUR-OFFSET-TEMP                               
           INITIALIZE WS-MINUTE-OFFSET-TEMP                             
           INITIALIZE  WS-HOUR-OFFSET                                   
           INITIALIZE  WS-MINUTE-OFFSET                                 
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
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID DATA FROM DATABASE' TO                      
                                    WS-Z02141-I-ERROR-MESSAGE(1)        
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
                                                                        
           .                                                            
      ******************************************************************
      *                   2300-CALL-ERROR-ROUTINE                       
      ******************************************************************
       2350-VALIDATE-DATE.                                              
                                                                        
           DISPLAY '2350 CALL TO DATE ROUTINE '                         
           CALL CT-DATE-ROUTINE-NAME USING ZZEC0243                     
                                                                        
                                                                        
           IF ZZEC0243-O-RC-NO-ERROR THEN CONTINUE                      
           ELSE                                                         
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'DATE ERROR ' TO WS-Z02141-I-ERROR-MESSAGE(1)         
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2351-SEND-CHOICE-MAP                         
      ******************************************************************
       2351-SEND-CHOICE-MAP.                                            
           EXEC CICS                                                    
             SEND MAP('MP0228') MAPSET('MP0228')                        
             FROM(MP0228O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2352-PROCESS-USER-CHOICE                     
      * PARAGRAPH IS CALLED WHEN USER PRESSED ENTER ON CHOICE MAP       
      *                                                                 
      * NOW WE NEED TO VALIDATE IF HIS CHIOCE IS VALID OR NOT           
      ******************************************************************
       2352-PROCESS-USER-CHOICE.                                        
           PERFORM 2353-RECEIVE-USER-CHOICE                             
           EVALUATE TRUE                                                
             WHEN WS-FIRST-CHOICE = 'X' AND WS-SECOND-CHOICE = 'X'      
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU CANT CHOOSE BOTH OPTIONS '                     
                             TO          WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             WHEN WS-FIRST-CHOICE = 'X'                                 
                 SET SO-ADD-A-SINGLE  TO TRUE                           
                 SET SO-USER-HAS-CHOSEN TO TRUE                         
             WHEN WS-SECOND-CHOICE = 'X'                                
                 SET SO-ADD-A-SCHEDULE TO TRUE                          
                 SET SO-USER-HAS-CHOSEN TO TRUE                         
             WHEN OTHER                                                 
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PLEASE PROVIDE VALID INPUT '                       
                             TO          WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2353-RECEIVE-USER-CHOICE                    
      * PARAGRAPH WILL BE USED TO GET USER CHOICE                       
      *  ON THE SCREEN WHERE HE HAVE TO CHOOSE                          
      *   1. ADD A SINGLE FLIGHT                                        
      *   2. ADD A SCHEUDLE                                             
      *                                                                 
      * THIS PARAGRAPH WILL GET HIS CHOICE AND WILL VALIDATE THAT       
      ******************************************************************
       2353-RECEIVE-USER-CHOICE.                                        
           DISPLAY '2353 PERFORMED RECEIVE THE MAP'                     
           MOVE LOW-VALUES TO MP0228I                                   
           EXEC CICS                                                    
             RECEIVE MAP('MP0228') MAPSET('MP0228')                     
             INTO(MP0228I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           DISPLAY 'AFTER RECEIVE '                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
             DISPLAY 'SUCCESSFULL RECEIVE '                             
             MOVE CHOI1I    TO WS-FIRST-CHOICE                          
             MOVE CHOI2I    TO WS-SECOND-CHOICE                         
           WHEN DFHRESP(MAPFAIL)                                        
             DISPLAY 'MAPFAIL       '                                   
             PERFORM  2700-INITIALIZE-ERROR-MESSAGE                     
             MOVE 'YOU NEED TO CHOOSE SOMETHING '                       
                                 TO  WS-Z02141-I-ERROR-MESSAGE(1)       
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           WHEN OTHER                                                   
             DISPLAY 'OTHER CASE '                                      
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2360-CHECK-TIME                             
      * PARAGRAPH WILL CHECK IF TIME PROVIDED BY USER IS VALID          
      * IF HOUR >= 00 AND < 24                                          
      * IF MINUTE >= 00 AND < 60                                        
      * AND IF TIME DELIMITER IS ':'                                    
      ******************************************************************
       2360-CHECK-TIME.                                                 
                                                                        
                                                                        
           SET SO-TIME-IS-VALID   TO TRUE                               
                                                                        
           IF WS-TIME-HOUR IS NUMERIC THEN                              
            MOVE WS-TIME-HOUR TO WS-TIME-TEMP-NUMERIC                   
             IF WS-TIME-TEMP-NUMERIC >= 00 AND                          
                WS-TIME-TEMP-NUMERIC < 24 THEN                          
                                                                        
                IF WS-TIME-MINUTE IS NUMERIC THEN                       
                 MOVE WS-TIME-MINUTE TO WS-TIME-TEMP-NUMERIC            
                   IF WS-TIME-TEMP-NUMERIC >= 00 AND                    
                      WS-TIME-TEMP-NUMERIC < 60 THEN                    
                     IF WS-TIME-FILLER = ':' THEN                       
                       SET SO-TIME-IS-VALID   TO TRUE                   
                     ELSE                                               
                       SET SO-TIME-IS-INVALID TO TRUE                   
                     END-IF                                             
                   ELSE                                                 
                     SET SO-TIME-IS-INVALID TO TRUE                     
                   END-IF                                               
                ELSE                                                    
                 SET SO-TIME-IS-INVALID TO TRUE                         
                END-IF                                                  
             ELSE                                                       
                SET SO-TIME-IS-INVALID TO TRUE                          
             END-IF                                                     
           ELSE                                                         
             SET SO-TIME-IS-INVALID TO TRUE                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2365-VALIDATE-SCHEDULE-DATA                   
      ******************************************************************
       2365-VALIDATE-SCHEDULE-DATA.                                     
             PERFORM 2317-VALIDATE-WEEK-DAYS                            
             PERFORM 2318-CHECK-SCHEDULE-AIRPORTS                       
             PERFORM 2319-VALIDATE-SCHEDULE-DATES                       
                                                                        
             PERFORM 2320-VALIDATE-SCHEDULE-MODEL                       
      * TIME CHECK (PROGRAM VALIDATES IF TIME VALUES PROVIDED BY USER   
      * ARE VALID                                                       
             PERFORM 2321-VALIDATE-USER-TIME                            
                                                                        
             PERFORM 2322-VALIDATE-SCHEDULE-AIRLINE                     
      * CHECK IF DESTINATION AND ORIGIN AIRPORTS ARE DIFFERENT          
             PERFORM 2323-IF-SCHEDULE-AIRPORTS-DIF                      
      * DISTANCE CHECK                                                  
             PERFORM 2324-CHECK-SCHEDULE-DISTANCE                       
           .                                                            
      ******************************************************************
      *                     2366-SAVE-SINGLE-TO-COMMAREA                
      * PARAGRAPH WILL SAVE ALL DATA PROVIDED BY THE USER TO COMMAREA   
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       2366-SAVE-SINGLE-TO-COMMAREA.                                    
           INSPECT TYPEI REPLACING ALL '_' BY ' '                       
           IF FUNCTION TEST-NUMVAL(TYPEI) = 0 THEN                      
             COMPUTE WS-Z02152-I-TYPE-OF-SEATS =                        
               FUNCTION NUMVAL(TYPEI)                                   
              IF WS-Z02152-I-TYPE-OF-SEATS <= 0 THEN                    
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'TYPE OF SETS IS NUMERIC'                           
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           ELSE                                                         
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'TYPE OF SETS IS NOT VALID NUMERIC'                 
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           MOVE  AIR-ORGI TO WS-Z02152-I-AIR-ORG                        
           MOVE  AIR-DESI TO WS-Z02152-I-AIR-DES                        
           MOVE  DEP-DI   TO WS-Z02152-I-DATE-D                         
           MOVE  ARV-DI   TO WS-Z02152-I-DATE-R                         
           MOVE  MODELI   TO WS-Z02152-I-MODEL-NAME                     
           MOVE  DEP-TI   TO WS-Z02152-I-TIME-D                         
           MOVE  ARV-TI   TO WS-Z02152-I-TIME-R                         
           MOVE  AIRLINEI TO WS-Z02302-AIRLINE-NAME                     
           .                                                            
      ******************************************************************
      *                    2367-PREPARE-FLIGHT-DATA                     
      * PARAGRAPH WILL PREPARE FLIGHT DATA TO BE INSERTED INTO THE      
      * DATABASE                                                        
      *                                                                 
      * MOSTLY IT WILL JUST MOVE DATA FROM PROGRAM VARIABLES TO         
      * DB2 HOST VARIABLES                                              
      * IT WILL ALSO PREPARE TIMESTAMP VALUES                           
      *  IT NEEDS TO CONNECT DATE AND TIME AND ALSO SWITCH ":" SYMBOL   
      * WITH "." SYMBOL (BETWEEN HOUR AND MINUTE)                       
      *                                                                 
      *  ALSO USER CANT SPECIFY SECONDS OR MICROSECONDS SO PROGRAM      
      * HAVE TO SET THEM AS ZERO                                        
      *                                                                 
      ******************************************************************
       2367-PREPARE-FLIGHT-DATA.                                        
           PERFORM 2084-GET-FLIGHT-ID                                   
           MOVE ID-NUMBER TO WS-ID-NUMBER-CHAR                          
           MOVE WS-ID-NUMBER-CHAR TO T05-FLIGHT-ID-TEXT                 
           MOVE AIRLINE-CODE      TO T05-FLIGHT-NUMBER-TEXT(1:3)        
           MOVE WS-ID-NUMBER-CHAR TO T05-FLIGHT-NUMBER-TEXT(4:12)       
           COMPUTE T05-FLIGHT-NUMBER-LEN  =                             
                FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)                 
           COMPUTE T05-FLIGHT-ID-LEN  =                                 
                FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)                 
                                                                        
           MOVE WS-AIR-ORG TO T05-DEPARTURE-AIRPORT-CODE                
           MOVE WS-AIR-DES TO T05-ARRIVAL-AIRPORT-CODE                  
           MOVE AIRLINE-CODE    TO T05-AIRLINE-CODE                     
                                                                        
           MOVE WS-DEPARTURE-DATE TO WS-DATE                            
      * USER PROVIDED TIME IN FORMAT HH:MM BUT DB2 SHOULD               
      * GET HH.MM, TO DO THAT WE HAVE TO MOVE '.' THERE                 
           MOVE '.' TO WS-DEPARTURE-TIME-DELIMITER                      
           MOVE WS-DEPARTURE-TIME TO WS-TIME                            
           PERFORM 2085-INITIALIZE-TIMESTAMP                            
           MOVE WS-TIMESTAMP-STRUCTURE TO                               
                     T05-DEPARTURE-TIMESTAMP                            
                                                                        
           MOVE WS-ARRIVAL-DATE TO WS-DATE                              
      * SAME STORY AS ABOVE                                             
           MOVE '.' TO WS-ARRIVAL-TIME-DELIMITER                        
           MOVE WS-ARRIVAL-TIME TO WS-TIME                              
           PERFORM 2085-INITIALIZE-TIMESTAMP                            
           MOVE WS-TIMESTAMP-STRUCTURE TO                               
                     T05-ARRIVAL-TIMESTAMP                              
           PERFORM 7100-MOVE-TIMESTAMP-TO-UTC                           
           MOVE T08-PLANE-ID TO T05-PLANE-ID                            
           MOVE 'CONFIRMED' TO T05-FLIGHT-STATUS-TEXT                   
           MOVE 9 TO T05-FLIGHT-STATUS-LEN                              
           .                                                            
      ******************************************************************
      *                   2500-CALL-TO-VALIDATE-NAMES                   
      * PROGRAM WILL DISPALY ALL SIMILAR NAMES TO WHAT USER JUST        
      * ENTERED                                                         
      * ON THE SCREEN DSIPALYED BY CALLED PROGRAM USER WILL HAVE TO     
      * CHOOSE WHAT NAME HE WANTS TO USE                                
      ******************************************************************
       2500-CALL-TO-VALIDATE-NAMES.                                     
           IF NOT SO-FLIGHTS-FROM THEN                                  
             CONTINUE                                                   
           END-IF                                                       
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           MOVE 'Z02292  ' TO WS-RETURN-CONTROL-PROGRAM                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM('Z02162') COMMAREA(DFHCOMMAREA)               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *              2700-INITIALIZE-ERROR-MESSAGE                      
      ******************************************************************
       2700-INITIALIZE-ERROR-MESSAGE.                                   
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 10       
             MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER)           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2710-WRITE-FLIGHT-DATA                        
      * HERE WE WILL SAVE ALL DATA INTO THE QUEUE                       
      * LATER PROGRAM WILL USER THIS QUEUE TO ADD ALL 'TO' FLIGHTS      
      * AND ALL 'FROM' FLIGHTS                                          
      ******************************************************************
       2710-WRITE-FLIGHT-DATA.                                          
           MOVE T05-FLIGHT-ID-TEXT         TO QUEUE-FLIGHT-ID           
           MOVE T05-FLIGHT-NUMBER-TEXT     TO QUEUE-FLIGHT-NUMBER       
           MOVE T05-PLANE-ID               TO QUEUE-PLANE-ID            
           MOVE T05-DEPARTURE-AIRPORT-CODE TO QUEUE-DEPARTURE-AIRPORT   
           MOVE T05-DEPARTURE-TIMESTAMP    TO QUEUE-DEPARTURE-TIMESTAMP 
           MOVE T05-ARRIVAL-AIRPORT-CODE   TO QUEUE-ARRIVAL-AIRPORT     
           MOVE T05-ARRIVAL-TIMESTAMP      TO QUEUE-ARRIVAL-TIMESTAMP   
           MOVE T05-FLIGHT-STATUS-TEXT     TO QUEUE-FILGHT-STATUS       
           MOVE T05-AIRLINE-CODE           TO QUEUE-AIRLINE-CODE        
                                                                        
           EXEC CICS                                                    
            WRITEQ TS                                                   
            QUEUE(CT-QUEUE-NAME)                                        
            FROM(WS-QUEUE-STRUCTURE)                                    
           END-EXEC                                                     
           IF SO-FLIGHTS-TO THEN                                        
             IF EIBRESP = DFHRESP(NORMAL) THEN                          
                 DISPLAY 'TO-WAS-ADDED TO TRUE'                         
                 SET SO-FLIGHTS-TO-WAS-ADDED TO TRUE                    
             END-IF                                                     
           ELSE                                                         
             IF EIBRESP = DFHRESP(NORMAL) THEN                          
                 DISPLAY 'FROM-WAS-ADDED TO TRUE'                       
                 SET SO-FLIGHTS-FROM-WAS-ADDED TO TRUE                  
             END-IF                                                     
           END-IF                                                       
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2720-MOVE-QUEUE-TO-DB-VARS                     
      ******************************************************************
       2720-MOVE-QUEUE-TO-DB-VARS.                                      
           MOVE QUEUE-FLIGHT-ID       TO T05-FLIGHT-ID-TEXT             
           MOVE QUEUE-FLIGHT-NUMBER   TO T05-FLIGHT-NUMBER-TEXT         
           MOVE QUEUE-PLANE-ID        TO T05-PLANE-ID                   
           MOVE QUEUE-DEPARTURE-AIRPORT TO T05-DEPARTURE-AIRPORT-CODE   
           MOVE QUEUE-DEPARTURE-TIMESTAMP TO T05-DEPARTURE-TIMESTAMP    
           MOVE QUEUE-ARRIVAL-AIRPORT TO T05-ARRIVAL-AIRPORT-CODE       
           MOVE QUEUE-ARRIVAL-TIMESTAMP TO T05-ARRIVAL-TIMESTAMP        
           MOVE QUEUE-FILGHT-STATUS     TO T05-FLIGHT-STATUS-TEXT       
           MOVE 9                       TO T05-FLIGHT-STATUS-LEN        
           MOVE QUEUE-AIRLINE-CODE     TO T05-AIRLINE-CODE              
           COMPUTE T05-FLIGHT-ID-LEN =                                  
            FUNCTION LENGTH(T05-FLIGHT-ID-TEXT)                         
           COMPUTE T05-FLIGHT-NUMBER-LEN =                              
            FUNCTION LENGTH(T05-FLIGHT-NUMBER-TEXT)                     
           .                                                            
      ******************************************************************
      *              2800-CALL-TO-FLIGHT-OPTION                         
      ******************************************************************
       2800-CALL-TO-FLIGHT-OPTION.                                      
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-FLIGHT-PROGRAM-NAME)                       
                  COMMAREA(DFHCOMMAREA)                                 
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                          3000-FINAL                             
      ******************************************************************
       3000-FINAL.                                                      
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EVALUATE TRUE                                                
           WHEN SO-GO-TO-PREVIOUS-PROGRAM                               
              PERFORM 3001-GOBACK-TO-PREVIOUS-PROG                      
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3002-RETURN-WITH-TRANSID                          
           WHEN OTHER                                                   
              PERFORM 3003-SEND-INVALID-MODE-MSG                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   3001-GOBACK-TO-PREVIOUS-PROG                  
      ******************************************************************
       3001-GOBACK-TO-PREVIOUS-PROG.                                    
           EXEC CICS                                                    
            XCTL PROGRAM(CT-NAME-OF-PROG-BEFORE)                        
            COMMAREA(DFHCOMMAREA) LENGTH(0)                             
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   3002-RETURN-WITH-TRANSID                      
      ******************************************************************
       3002-RETURN-WITH-TRANSID.                                        
           EXEC CICS                                                    
            RETURN TRANSID('0224') COMMAREA(DFHCOMMAREA)                
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   3003-SEND-INVALID-MODE-MSG                    
      ******************************************************************
       3003-SEND-INVALID-MODE-MSG.                                      
           PERFORM  2700-INITIALIZE-ERROR-MESSAGE                       
           MOVE 'INVALID 3000 FINAL MODE ' TO                           
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   7001-CHECK-IF-MODEL-VALID                     
      ******************************************************************
       7001-CHECK-IF-MODEL-VALID.                                       
           EXEC SQL                                                     
            SELECT PLANE_ID,                                            
                   AIRPLANE_RANGE,                                      
                   AIRPLANE_SPEED                                       
           INTO                                                         
            :T08-PLANE-ID,                                              
            :AIRPLANE-RANGE,                                            
            :AIRPLANE-SPEED                                             
           FROM                                                         
            T08_TABLE_PLANE_TABLE T08                                   
           INNER JOIN T07_PLANE_MODEL_TABLE T07                         
            ON  T07.PLANE_MODEL = T08.PLANE_MODEL                       
           WHERE                                                        
            T08.PLANE_MODEL = :T08-PLANE-MODEL                          
                    AND                                                 
            T08.TYPE_OF_SEATS_ID = :T08-TYPE-OF-SEATS-ID                
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               SET SO-PLANE-VALID TO TRUE                               
               DISPLAY 'PLANE VALID '                                   
               MOVE T08-PLANE-ID TO WS-Z02152-I-PLANE-ID                
               MOVE T08-PLANE-MODEL-TEXT TO                             
                          WS-Z02152-I-MODEL-NAME                        
               MOVE T08-PLANE-MODEL-TEXT TO WS-Z02152-I-MODEL-NAME      
                                                                        
           WHEN SO-SQLCODE-NOT-FOUND                                    
               DISPLAY 'PLANE IS INVALID '                              
               SET SO-PLANE-INVALID TO TRUE                             
               PERFORM 2700-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'WE CANT USE THAT PLANE '                           
                              TO          WS-Z02141-I-ERROR-MESSAGE(1)  
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           WHEN OTHER                                                   
               SET SO-7001-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7100-VALIDATE-AIRPORT-IATA                   
      ******************************************************************
       7100-VALIDATE-AIRPORT-IATA.                                      
           MOVE WS-AIRPORT-VALUE TO T02-AIRPORT-CODE                    
           INITIALIZE   T02-AIRPORT-FULL-NAME                           
           EXEC SQL                                                     
              SELECT AIRPORT_CODE, AIRPORT_FULL_NAME                    
              INTO :T02-AIRPORT-CODE, :T02-AIRPORT-FULL-NAME            
              FROM T02_AIRPORT_TABLE                                    
              WHERE AIRPORT_CODE = :T02-AIRPORT-CODE                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               SET SO-VALID-IATA   TO TRUE                              
           WHEN SO-SQLCODE-NOT-FOUND                                    
               SET SO-INVALID-IATA TO TRUE                              
           WHEN OTHER                                                   
               SET SO-7100-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  7002-CHECK-MODEL-NAME                          
      ******************************************************************
       7002-CHECK-MODEL-NAME.                                           
           DISPLAY '7002: T08-PLANE-MODEL-TEXT: ' T08-PLANE-MODEL-TEXT  
           EXEC SQL                                                     
            SELECT "A"                                                  
            INTO :WS-DUMMY                                              
            FROM T08_TABLE_PLANE_TABLE                                  
            WHERE PLANE_MODEL = :T08-PLANE-MODEL                        
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
                SET SO-MODEL-NAME-VALID     TO TRUE                     
           WHEN SO-SQLCODE-NOT-FOUND                                    
                SET SO-MODEL-NAME-INVALID   TO TRUE                     
           WHEN OTHER                                                   
                SET SO-7002-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7009-TRANSLATE-PLANE-ID                      
      ******************************************************************
       7009-TRANSLATE-PLANE-ID.                                         
           INITIALIZE T08-PLANE-MODEL                                   
           EXEC SQL                                                     
             SELECT PLANE_MODEL                                         
            INTO                                                        
             :T08-PLANE-MODEL                                           
            FROM                                                        
             T08_TABLE_PLANE_TABLE                                      
            WHERE                                                       
             PLANE_ID = :T08-PLANE-ID                                   
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
                SET SO-TRANSLATION-SUCCESS TO TRUE                      
           WHEN SO-SQLCODE-NOT-FOUND                                    
                SET SO-TRANSLATION-FALIED  TO TRUE                      
           WHEN OTHER                                                   
                SET SO-7009-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7010-GET-GEOGRAF-POS                         
      ******************************************************************
       7010-GET-GEOGRAF-POS.                                            
           DISPLAY '7010 TO2 AIRPORT CODE : '  T02-AIRPORT-CODE         
           EXEC SQL                                                     
            SELECT                                                      
            LATITUDE,                                                   
            LONGITUDE                                                   
            INTO                                                        
             :WS-LATITUDE,                                              
             :WS-LONGITUDE                                              
            FROM T02_AIRPORT_TABLE                                      
            WHERE                                                       
            AIRPORT_CODE = :T02-AIRPORT-CODE                            
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
      *                    7011-CHECK-AIRLINE                           
      ******************************************************************
       7011-CHECK-AIRLINE.                                              
           EXEC SQL                                                     
            SELECT AIRLINE_CODE                                         
            INTO :AIRLINE-CODE                                          
            FROM T01_AIRLINE_NAMES_TABLE                                
            WHERE AIRLINE_NAME = :AIRLINE-NAME                          
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
      * PROGRAM Z02162 WILL BE CALLED AND THEN USER WILL BE ABLE        
                                                                        
      * TO CHOOSE SIMILAR NAMES TO WHAT HE JUST ENTERED                 
              MOVE AIRLINE-NAME-TEXT TO  WS-SEARCHED-PHRASE-AIRLINE     
              SET SO-CHECK-AIRLINE-NAME TO TRUE                         
              DISPLAY 'SEARCH AIRLINE '                                 
              DISPLAY ' PRZED CALL WS-SEARCHED : '                      
                            WS-SEARCHED-PHRASE-AIRLINE                  
              PERFORM 2500-CALL-TO-VALIDATE-NAMES                       
           WHEN OTHER                                                   
              SET SO-7011-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7014-INSERT-THE-FLIGHT                       
      * PARAGRAPH INSERT FLIGHT DATA INTO THE DATABASE                  
      ******************************************************************
       7014-INSERT-THE-FLIGHT.                                          
           EXEC SQL                                                     
             INSERT INTO T05_FLIGHT_TABLE(FLIGHT_ID,                    
                                          FLIGHT_NUMBER,                
                                          PLANE_ID,                     
                                          DEPARTURE_AIRPORT_CODE,       
                                          DEPARTURE_TIMESTAMP,          
                                          ARRIVAL_AIRPORT_CODE,         
                                          ARRIVAL_TIMESTAMP,            
                                          FLIGHT_STATUS,                
                                          AIRLINE_CODE)                 
                                  VALUES(:T05-FLIGHT-ID,                
                                         :T05-FLIGHT-NUMBER,            
                                         :T05-PLANE-ID,                 
                                         :T05-DEPARTURE-AIRPORT-CODE,   
                                         :T05-DEPARTURE-TIMESTAMP,      
                                         :T05-ARRIVAL-AIRPORT-CODE,     
                                         :T05-ARRIVAL-TIMESTAMP,        
                                         :T05-FLIGHT-STATUS,            
                                         :T05-AIRLINE-CODE)             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7014-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           ELSE                                                         
            ADD 1 TO WS-ITERX                                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7012-INSERT-NEW-VALUE                       
      * THIS PARAGRAPH IS USED TO ADD SOME VALUE TO T20_TEMP_TABLE      
      *                                                                 
      * DB2 WILL CREATE ANOTHER IDENTIFICATION NUMBER FOR THIS          
      * VALUE, IN THE PARAGRAPH BELOW WE WILL TAKE THIS VALUE           
      * AND USE IT AS FLIGHT ID AND ALSO IN FLIGHT NUMBER               
      *                                                                 
      *                                                                 
      ******************************************************************
       7012-INSERT-NEW-VALUE.                                           
           MOVE 'A' TO TEXT-VALUE                                       
           EXEC SQL                                                     
              INSERT INTO T20_TEMP_TABLE(TEXT_VALUE)                    
              VALUES(:TEXT-VALUE)                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7012-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7013-GET-THIS-VALUE                          
      * APRAGRAPH WILL RECEIVE NUMBER THAT WAS PREVIOUSLY ASSIGNED      
      * BY THE PARAGRAPH ABOVE                                          
      * WE WILL USE THAT NUMBER IN ORDER TO CREATE FLIGHT NUMBER        
      * AND FLIGHT ID                                                   
      ******************************************************************
       7013-GET-THIS-VALUE.                                             
           EXEC SQL                                                     
            SELECT ID_NUMBER                                            
            INTO :ID-NUMBER                                             
            FROM T20_TEMP_TABLE                                         
            ORDER BY ID_NUMBER DESC                                     
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
               SET SO-7013-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7016-OPEN-CURSOR-C-NAME                     
      ******************************************************************
       7016-OPEN-CURSOR-C-NAME.                                         
           DISPLAY 'OTWIERANIA CURSORA: '                               
           DISPLAY 'DATA START:  ' WS-Z02302-START-DATE                 
           DISPLAY 'DATA END:  '    WS-Z02302-END-DATE                  
           DISPLAY 'WARUNEK: ' WS-USER-DAYS                             
           EXEC SQL                                                     
             OPEN C-NAME                                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7016-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7017-FETCH-CURSOR-C-NAME                     
      * PROGRAM WILL INSERT PREPARED FLIGTH INTO THE QUEUE              
      ******************************************************************
       7017-FETCH-CURSOR-C-NAME.                                        
           PERFORM 2090-CHECK-HOURS                                     
           PERFORM 7019-FETCH-DATE-CURSOR                               
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
             PERFORM 2091-CONVER-LOCAL-TIME-TO-UTC                      
             PERFORM 2092-PREPARE-FLIGHT-DATA                           
             PERFORM 2710-WRITE-FLIGHT-DATA                             
             PERFORM 7019-FETCH-DATE-CURSOR                             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     7016-OPEN-CURSOR-C-NAME                     
      ******************************************************************
       7018-CLOSE-CURSOR-C-NAME.                                        
           EXEC SQL                                                     
             CLOSE   C-NAME                                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7016-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                       7019-FETCH-DATE-CURSOR                    
      ******************************************************************
       7019-FETCH-DATE-CURSOR.                                          
           DISPLAY '7019 FETCH C-NAME '                                 
           SET SO-NOT-END-OF-CURSOR-DATA TO TRUE                        
           EXEC SQL                                                     
             FETCH C-NAME INTO                                          
             :WS-CURRENT-DATE,                                          
             :WS-TOMORROW-DATE                                          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7019 FETCH NORMAL '                              
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY '7019 FETCH END    '                              
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
                                                                        
           WHEN OTHER                                                   
              DISPLAY '7019 FETCH ERROR  '                              
              SET SO-7019-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 7021-INSERT-ALL-FLIGHTS-DATA                    
      * PARAGRAPH WILL ADD INTO THE DATABASE ALL FLIGHTS                
      * THAT WERE PREVIOUSLY GENERATED AND PUT IN THE QUEUE             
      ******************************************************************
       7021-INSERT-ALL-FLIGHTS-DATA.                                    
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           MOVE 0 TO WS-ITERX                                           
           SET SO-NOT-END-OF-QUEUE-DATA TO TRUE                         
           PERFORM 2095-READ-THE-QUEUE                                  
           PERFORM UNTIL SO-END-OF-QUEUE-DATA                           
             PERFORM 2720-MOVE-QUEUE-TO-DB-VARS                         
                                                                        
              PERFORM 7014-INSERT-THE-FLIGHT                            
              ADD 1 TO WS-WHAT-RECORD-TO-READ                           
              PERFORM 2095-READ-THE-QUEUE                               
           END-PERFORM                                                  
           IF WS-ITERX > 0 THEN                                         
              PERFORM 7022-INSERT-SCHEDULED-FLIGHT                      
           ELSE                                                         
              PERFORM  2700-INITIALIZE-ERROR-MESSAGE                    
              MOVE 'NO FLIGHTS ADDED' TO                                
                                      WS-Z02141-I-ERROR-MESSAGE(1)      
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7022-INSERT-SCHEDULED-FLIGHT                    
      * THIS PARAGRAPH WILL PREPARE DATA TO BE INSERTED INTO            
      * T10-SCHEDULED-FLIGHTS-TABLE TABLE THAT WILL STORE INFORMATION   
      * ABOUT SCHEDULED FLIGHTS                                         
      ******************************************************************
       7022-INSERT-SCHEDULED-FLIGHT.                                    
               PERFORM 2347-MOVE-SCHED-FLIGHT-DATA                      
               EXEC SQL                                                 
               INSERT INTO T10_SCHEDULED_FLIGHTS_TABLE(FLIGHT_NUMBER_TO,
                                                FLIGHT_NUMBER_FROM,     
                                                DEPARTURE_TIME_ORIGIN,  
                                                ORIGIN_AIRPORT_CODE,    
                                               ARRIVAL_TIME_DESTINATION,
                                               DESTINATION_AIRPORT_CODE,
                                             DEPARTURE_TIME_DESTINATION,
                                                ARRIVAL_TIME_ORIGIN,    
                                                WEEK_DAYS,              
                                                PLANE_ID,               
                                                START_SCHEDULE_DATE,    
                                                END_SCHEDULE_DATE,      
                                               SCHEDULED_STATUS)        
                                                                        
                                       VALUES(:FLIGHT-NUMBER-TO,        
                                              :FLIGHT-NUMBER-FROM,      
                                              :DEPARTURE-TIME-ORIGIN,   
                                              :ORIGIN-AIRPORT-CODE,     
                                              :ARRIVAL-TIME-DESTINATION,
                                              :DESTINATION-AIRPORT-CODE,
                                            :DEPARTURE-TIME-DESTINATION,
                                              :ARRIVAL-TIME-ORIGIN,     
                                              :WEEK-DAYS,               
                                              :PLANE-ID,                
                                              :START-SCHEDULE-DATE,     
                                              :END-SCHEDULE-DATE,       
                                              :SCHEDULED-STATUS)        
              END-EXEC                                                  
              MOVE SQLCODE TO SW-SQLCODE                                
              IF NOT SO-SQLCODE-NORMAL THEN                             
                SET  SO-7022-PARA TO TRUE                               
                PERFORM 9000-DB2-ERROR                                  
              END-IF                                                    
           .                                                            
      ******************************************************************
      *                     7099-GET-AIRLINE-NAME                       
      ******************************************************************
       7099-GET-AIRLINE-NAME.                                           
           INITIALIZE AIRLINE-NAME                                      
           DISPLAY '7099 PERFORMED  AIRLINE-CODE: ' AIRLINE-CODE        
           EXEC SQL                                                     
             SELECT AIRLINE_NAME                                        
             INTO  :AIRLINE-NAME                                        
             FROM T01_AIRLINE_NAMES_TABLE                               
             WHERE AIRLINE_CODE = :AIRLINE-CODE                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
                DISPLAY '7099 SUCCESSFULL FETCH '                       
           WHEN SO-SQLCODE-NOT-FOUND                                    
      * THIS SHOULD NOT HAPPEN BUT WHEN IT DOES WE WILL NOT DROP THE    
      * PROGRAM ( USER WILL BE ABLE TO PROVIDE AIRLINE NAME ONCE AGAIN) 
                DISPLAY '7099 END OF DATA FETCH 100 '                   
           WHEN OTHER                                                   
               SET SO-7099-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7100-MOVE-TIMESTAMP-TO-UTC                   
      * PARAGRAPH HAVE TO TAKE TIMEZONE FROM DATABSE                    
      * AND MOVE PROVIDED TIME STAMP BY -TIMEZONE                       
      * BECAUSE IF USER PROVIDE DATE AND TIME IN LOCAL AIRPORT TIME     
      * FOR EXAMPLE UTC -2 THEN PROGRAM NEED TO MOVE THIS TIME BY +2    
      * HOURS TO MAKE IT TO UTC                                         
      ******************************************************************
       7100-MOVE-TIMESTAMP-TO-UTC.                                      
           DISPLAY '71000 STARTED '                                     
           DISPLAY 'T05-DEP ' T05-DEPARTURE-AIRPORT-CODE                
           DISPLAY 'T05-ARV ' T05-ARRIVAL-AIRPORT-CODE                  
           IF SO-FLIGHTS-FROM THEN                                      
              DISPLAY 'SO-FLIGHT FROM ARE TRUE'                         
              MOVE WS-Z02302-AIR-DES(1:3) TO T05-ARRIVAL-AIRPORT-CODE   
              MOVE WS-Z02302-AIR-ORG(1:3) TO T05-DEPARTURE-AIRPORT-CODE 
           END-IF                                                       
           DISPLAY '7100 PO DZWNYM IFIE '                               
           MOVE    T05-DEPARTURE-AIRPORT-CODE TO T02-AIRPORT-CODE       
           PERFORM 7101-FETCH-TIMEZONE                                  
           MOVE T05-DEPARTURE-TIMESTAMP TO WS-MODIFIED-TIMESTAMP        
           PERFORM 7102-CONVERT-TIMESTAMP-TO-UTC                        
           MOVE WS-MODIFIED-TIMESTAMP-OUT TO T05-DEPARTURE-TIMESTAMP    
                                                                        
           MOVE    T05-ARRIVAL-AIRPORT-CODE TO T02-AIRPORT-CODE         
           PERFORM 7101-FETCH-TIMEZONE                                  
           MOVE T05-ARRIVAL-TIMESTAMP TO WS-MODIFIED-TIMESTAMP          
           PERFORM 7102-CONVERT-TIMESTAMP-TO-UTC                        
           MOVE WS-MODIFIED-TIMESTAMP-OUT TO T05-ARRIVAL-TIMESTAMP      
           DISPLAY  ' AFTER MODIFING TIMESTAMP: '                       
           DISPLAY 'DEPARTURE: ' T05-DEPARTURE-TIMESTAMP                
           DISPLAY 'ARRIVAL:  ' T05-ARRIVAL-TIMESTAMP                   
           .                                                            
      ******************************************************************
      *                 7101-FETCH-TIMEZONE                             
      ******************************************************************
       7101-FETCH-TIMEZONE.                                             
           INITIALIZE T02-TIME-ZONE2                                    
           DISPLAY '7101 AIRPORT CODE: ' T02-AIRPORT-CODE               
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
             DISPLAY 'STREFA TO: '   T02-TIME-ZONE2                     
                                                                        
           WHEN OTHER                                                   
                                                                        
             SET SO-7101-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7102-CONVERT-TIMESTAMP-TO-UTC                
      * PREVIOUS PROGRAMS WERE CONVERTING TIMESTAMPS FROM UTC TO        
      * LOCAL TIMEZONE BUT THIS TIME WE NEED TO DO THIS IN A            
      * OPPOSITE WAY                                                    
      * TO DO THAT WE HAVE TO SWICH TIMEZONE SIGN ON THE OPPOISTE       
      * (FROM + TO - ) (FROM - TO +)                                    
      ******************************************************************
       7102-CONVERT-TIMESTAMP-TO-UTC.                                   
      * SIGN  NEEDS TO BE REVERSED                                      
           MOVE T02-TIME-ZONE2      TO WS-TEMP-TIMEZONE                 
           IF WS-TIMEZONE-SIGN = '-' THEN                               
              MOVE '+' TO WS-TIMEZONE-SIGN                              
           ELSE                                                         
              MOVE '-' TO WS-TIMEZONE-SIGN                              
           END-IF                                                       
           PERFORM 2348-PREPARE-TIME-OFFSET                             
                                                                        
           INITIALIZE WS-MODIFIED-TIMESTAMP-OUT                         
           PERFORM 7203-INSERT-TIMESTMAP                                
           PERFORM 7204-CONVERT-LOCAL-TO-UTC                            
           PERFORM 7104-DELETE-TIMESTMAP                                
           .                                                            
      ******************************************************************
      *                    7203-INSERT-TIMESTMAP                        
      * THIS TABLE WILL BE USED ONLY BECAUSE DB2 REQUIRES THAT          
      * THAT DATA IN A SUBQUERY NEEDS TO BE TAKEN FROM A TABLE          
      * SO WE WILL INSERT DATA TO THIS TABLE , THEN WE WILL SELECT      
      * THIS DATA AND AT THE END THIS DATA WILL BE DELETED              
      ******************************************************************
       7203-INSERT-TIMESTMAP.                                           
           MOVE WS-MODIFIED-TIMESTAMP TO TEMP-TIMESTAMP                 
           EXEC SQL                                                     
             INSERT INTO T22_TEMP_TIMESTAMP_TABLE(TEMP_TIMESTAMP)       
                      VALUES(:TEMP-TIMESTAMP)                           
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7103-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7204-CONVERT-LOCAL-TO-UTC                     
      * THIS PROGRAM WILL TAKE ALREADY PREPARED TIMESTAMPS AND          
      * WILL MOVE THEIR TIMEZONE FROM LOCAL TMIEZONE FOR THE AIRPORT    
      * (THIS IS THE TIME IN WHAT USER PROVIDES THIS VALUE)             
      *                                                                 
      * TO DO THAT PROGRAM WILL USE TIMESTAMPADD FUNCTION               
      *                                                                 
      * IN THE QUERY BELOW WE WILL TAKE DATA THAT PROGRAM PUT IN A      
      * SQL DATABASE AND WE WILL ADD TO THIS TIMESTAMP AMOUNT OF HOURS  
      * (THIS VALUE WAS PREPARED ERLIER AND REPRESENT AMOUNT OF         
      * HOURS WE HAVE TO ADD TO GET TIMESTAMP IN UTC TIMEZONE)          
      *                                                                 
      * THEN WE WILL TAKE THAT MODIFIED TIMESTAMP AND WE WILL DO THE    
      * SAME WITH MINUTE OFFSET                                         
      *                                                                 
      ******************************************************************
       7204-CONVERT-LOCAL-TO-UTC.                                       
           EXEC SQL                                                     
           SELECT                                                       
               CHAR(TIMESTAMPADD(4,:WS-MINUTE-OFFSET,                   
                ZMIENNA))                                               
           INTO :WS-MODIFIED-TIMESTAMP-OUT                              
           FROM                                                         
           (SELECT TIMESTAMPADD(8,:WS-HOUR-OFFSET,                      
            TEMP_TIMESTAMP) AS ZMIENNA                                  
           FROM T22_TEMP_TIMESTAMP_TABLE                                
           WHERE TEMP_TIMESTAMP = :WS-MODIFIED-TIMESTAMP                
           FETCH FIRST ROW ONLY )                                       
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
                SET SO-7102-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7104-DELETE-TIMESTMAP                        
      * PROGRAM USED THIS TABLE TO STORE THERE TIMESTAMP VALUE (        
      *                                                                 
      * ONLY NEED FOR THIS WAS BECAUSE DB2 NEEDS TO SELECT THIS VALUE   
      * FROM THE DATABASE (IT CANNOT BE PROVIDED BY HOST VARIALBE)      
      *        
      * BECAUSE WE DON'T NEED THAT DATA ANYMORE, WE WILL DELETE THAT    
      * ROW                                                             
      ******************************************************************
       7104-DELETE-TIMESTMAP.                                           
           MOVE WS-MODIFIED-TIMESTAMP TO TEMP-TIMESTAMP                 
           EXEC SQL                                                     
             DELETE FROM T22_TEMP_TIMESTAMP_TABLE                       
                 WHERE TEMP_TIMESTAMP =  :TEMP-TIMESTAMP                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7104-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7105-CHECK-IF-DATES-POSSIBLE                   
      * PARAGRAPH WILL CHECK IF DEPARUTRE DATE IS BEFORE AN ARRIVAL     
      * DATE                                                            
      *                                                                 
      * IF IT IS NOT THEN USER WILL BE NOTIFIED WITH PROPER MESSAGE     
      ******************************************************************
       7105-CHECK-IF-DATES-POSSIBLE.                                    
           EXEC SQL                                                     
              SELECT "A"                                                
              INTO :WS-DUMMY                                            
              FROM T05_FLIGHT_TABLE                                     
              WHERE                                                     
               DATE(:WS-Z02152-I-DATE-D) <= DATE(:WS-Z02152-I-DATE-R)   
              FETCH FIRST ROW ONLY                                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
              PERFORM  2700-INITIALIZE-ERROR-MESSAGE                    
              MOVE 'ARRIVAL DATE IS BEFORE DEPARTURE  ' TO              
                                      WS-Z02141-I-ERROR-MESSAGE(1)      
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
              SET SO-7105-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7106-VALIDATE-TIME-OF-FLIGHT                  
      * PARAGRAPH WILL CHECK IF TIME OF THE FLIGHT IS LESS OR EQUAL TO  
      * 24 HOURS                                                        
      * WE WILL ALSO CHECK IF FLIGHT IS LONGER THAN 1 MINUTE            
      ******************************************************************
       7106-VALIDATE-TIME-OF-FLIGHT.                                    
           EXEC SQL                                                     
             SELECT "A"                                                 
            INTO                                                        
               :WS-DUMMY                                                
            FROM                                                        
               T05_FLIGHT_TABLE                                         
            WHERE                                                       
              TIMESTAMPDIFF(4,CHAR( TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) - 
            TIMESTAMP(:T05-DEPARTURE-TIMESTAMP) ) ) BETWEEN             
                       :CT-ONE-MINUTE AND :CT-24-HOURS                  
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
            WHEN SO-SQLCODE-NORMAL                                      
              CONTINUE                                                  
            WHEN SO-SQLCODE-NOT-FOUND                                   
              PERFORM  2700-INITIALIZE-ERROR-MESSAGE                    
              MOVE 'THIS FLIGHT TAKES TOO MUCH TIME '                   
                            TO        WS-Z02141-I-ERROR-MESSAGE(1)      
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
            WHEN OTHER                                                  
              SET SO-7106-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      9000-DB2-ERROR                             
      ******************************************************************
       9000-DB2-ERROR.                                                  
           MOVE SQLCODE TO SQLCODE-FORMAT                               
           MOVE SQLERRMC TO WS-SQLERRMC                                 
           PERFORM 2700-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'DB2 ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)           
                                                                        
           STRING 'IN SATATEMENT: ' SW-STATEMENT-ID                     
             DELIMITED BY SIZE                                          
             INTO WS-Z02141-I-ERROR-MESSAGE(2)                          
           END-STRING                                                   
                                                                        
           STRING 'SQLCODE: ' SQLCODE-FORMAT                            
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
      *                        9100-ROLLBACK                            
      ******************************************************************
       9100-ROLLBACK.                                                   
           EXEC CICS                                                    
            SYNCPOINT ROLLBACK                                          
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                                                                                                    
                                                         