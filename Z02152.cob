       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02152.                                              
      ******************************************************************
      *                                                                 
      *  PROGRAM Z02152        TRANSACTION 0209                         
      *                                                                 
      *  PROGRAM IS CALLED WHEN USER CHOOSE OPTION '1' ON Z02131 PROGRAM
      *    OPTION '1' = "BOOK A FLIGHT "                                
      *                                                                 
      *  PROGRAM WILL ALLOW USER TO PROVIDE DATA ABOUT THE FLIGHT HE    
      *  WANTS TO MAKE RESERVATION ON.                                  
      *                                                                 
      *  AFTER GATHERING DATA AND SUCCESSFULL VALIDATION PROGRAM WILL   
      *  CALL TO Z02172 PROGRAM                                         
      *                                                                 
      * PROGRAM CAN ALSO CALL TO Z02162 PROGRAM                         
      *     (ONLY WHEN USER PROVIDED INVALID AIRPORT NAME)              
      *                                                                 
      * IF USER WILL PRESS F3 KEY THEN PROGRAM RETURNS CONTROL TO       
      * CALLING PROGRAM (Z02131)                                        
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0215.                                               
           COPY ZZEC0215.                                               
           COPY ZZEC0243.                                               
                                                                        
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
                                                                        
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
                                                                        
       01 CT-CONSTANTS.                                                 
           05 CT-THIS-PROGRAM-NAME           PIC X(8) VALUE 'Z02152  '. 
           05 CT-NAME-OF-PROG-BEFORE         PIC X(8) VALUE 'Z02131  '. 
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
      * PROGRAM THAT IS BEFORE OURS IN THE LOGIC                        
       01 SW-SWITCHES.                                                  
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
           05 SW-IF-VALID-FULL-NAME          PIC X.                     
               88 SO-VALID-NAME              VALUE 'Y'.                 
               88 SO-INVALID-NAME            VALUE 'N'.                 
      * PROGRAM VARIABLES                                               
       01 WS-VARIABLES.                                                 
           05 WS-TICKET-NUMBER-TEMP          PIC S9(4) COMP VALUE 0.    
           05 WS-AIRPORT-IATA-CODE           PIC X(3) VALUE SPACE.      
           05 WS-EIBRESP-TEMP                PIC X(10) VALUE SPACE.     
           05 WS-LENGTH-OF-STRING            PIC S9(4) COMP.            
           05 WS-AIRPORT-NAME-TEMP           PIC X(50) VALUE SPACE.     
           05 WS-AIRPORT-NAME-FROM-USER      PIC X(50) VALUE SPACE.     
           05 WS-AIRPORT-ORIGIN              PIC X(50) VALUE SPACE.     
           05 WS-AIRPORT-DESTINATION         PIC X(50) VALUE SPACE.     
           05 WS-DEPARTURE-DATE              PIC X(10) VALUE SPACE.     
           05 WS-RETURN-DATE                 PIC X(10) VALUE SPACE.     
           05 WS-TICKET-NUMBER               PIC X(2)  VALUE SPACE.     
           05 WS-IF-ONE-WAY-FLAG             PIC X(1).                  
               88 SO-ONE-WAY-FLAG-VALID      VALUE 'X'.                 
           05 WS-IF-DIRECT-FLAG              PIC X(1)  VALUE SPACE.     
               88 SO-DIRECT-FLAG-VALID       VALUE 'X'.                 
           05 WS-AIRPORT-VALUE               PIC X(50) VALUE SPACE.     
           05 WS-ITER                        PIC S9(4) COMP VALUE 0.    
           05 WS-AIRPORT-FULL-NAME           PIC X(59) VALUE SPACE.     
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA  PIC X(17294).                                    
      ******************************************************************
      *                    PROCEDURE DIVISION                           
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02152-----------------START-------------'          
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02152--------------B--FINAL-------------'          
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                           1000-INIT                             
      ******************************************************************
       1000-INIT.                                                       
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           PERFORM 1005-IGNORE-IF-FIRST-TIME                            
           .                                                            
      ******************************************************************
      *                     1005-IGNORE-IF-FIRST-TIME                   
      * THIS PARAGRAPH WILL ISSUE IGNORE CONDITION STATEMENT            
      * IF THIS IS FIRST TIME PROGRAM RUNDS                             
      ******************************************************************
       1005-IGNORE-IF-FIRST-TIME.                                       
           DISPLAY 'Z02152 FLAGA: '  SW-M-WHAT-MODE                     
           IF SO-M-FIRST-WITHOUT THEN                                   
             PERFORM 1010-IGNORE-CONDITION                              
           END-IF                                                       
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
      *                        2000-PROCESS                  
      * PROGRAM WILL CALL PARAGRAPH BASED ON WHAT IS THE CURRECT        
      * MODE OF PROGRAM                                                 
      *                                                                 
      *   1. FIRST TIME WITHOUT DATA    (SO-M-FIRST-WITHOUT)            
      *      CALLED WHEN THIS IS THE FIRST TIME PROGRAM RUNS            
      *       (WE DON'T HAVE ANY DATA PROVIDED BY THE USER)             
      *   2. FIRST TIME WITH DATA     (SO-M-FIRST-WITH)                 
      *       CALLED FOR EXAMPLE:                                       
      *       WHEN USER PROVIDED SOME DATA PRESSED ENTER BUT            
      *       THERE WAS SOMETHING WRONG WITH THOSE DATA                 
      *       PROGRAM CALLED TO Z02141 PROGRAM AND AFTER                
      *       PRESSING F3 AND RETURNING TO THIS PROGRAM                 
      *       THIS PARAGRAPH WILL BE CALLED                             
      *       AND DATA THAT USER PROVIDED ERLIER WILL BE DISPLEYED      
      *   3. NOT FIRST TIME (CALLED WHEN USER PRESSED A KEY)            
      *    (SO-M-NOT-FIRST)                                             
      ******************************************************************
       2000-PROCESS.                                                    
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           EVALUATE TRUE                                                
           WHEN  SO-M-FIRST-WITHOUT                                     
             PERFORM 2001-PROCESS-FIRST-WITHOUT                         
             SET SO-M-NOT-FIRST TO TRUE                                 
           WHEN SO-M-FIRST-WITH                                         
             PERFORM 2002-PROCESS-FIRST-TIME-WITH                       
             SET SO-M-NOT-FIRST TO TRUE                                 
           WHEN SO-M-NOT-FIRST                                          
             PERFORM 2003-PROCESS-NOT-FIRST-TIME                        
           WHEN OTHER                                                   
      * THIS SHOULD NOT HAPPEN                                          
             PERFORM 2318-SEND-INVALID-CALL-MSG                         
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2001-PROCESS-FIRST-WITHOUT                     
      ******************************************************************
       2001-PROCESS-FIRST-WITHOUT.                                      
           PERFORM 2050-SEND-FRESH-MAP                                  
           .                                                            
      ******************************************************************
      *                  2002-PROCESS-FIRST-TIME-WITH                   
      * PARAGRAPH WILL BE CALLED WHEN USER FOR EXAMPLE:                 
      * PROVIDED SOME DATA BUT IN TIME OF VALIDATION SOMETHING WENT     
      * WRONG AND PROGRAM Z02141 WAS CALLED, AFTER RETURNING TO THIS    
      * PROGRAM ( BY PRESSING F3 ) PROGRAM ALREADY HAS SOME OF THE      
      * DATA PROVIDED BY THE USER.                                      
      * TO AVOID SITUTATION WHEN USER HAS TO PROVIDE SAME DATA ONCE     
      * AGAIN, THIS PROGRAM WILL DISPLAY THIS  PREVIOUSLY GATHERED DATA 
      *                                                                 
      ******************************************************************
       2002-PROCESS-FIRST-TIME-WITH.                                    
           PERFORM 2010-DFHCOMMAREA-TO-SCREEN                           
           .                                                            
      ******************************************************************
      *                  2003-PROCESS-NOT-FIRST-TIME                    
      * PROGRAM RUNS NOT FIRST TIME (PROGRAM WAS STARTED                
      * WHEN USER PRESSED ATTENTION KEY)                                
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           PERFORM 2060-PROCESS-INPUT                                   
           .                                                            
      ******************************************************************
      *                      2010-DFHCOMMAREA-TO-SCREEN                 
      * THIS "MOVE" PARAGRAPHS WILL DISPLAY DATA THAT USER PROVIDED     
      * IF THERE IS NO PROVIDED DATA PROGRAM WILL DISPLAY EMPTY LINE    
      ******************************************************************
       2010-DFHCOMMAREA-TO-SCREEN.                                      
           MOVE LOW-VALUES TO MP0215O                                   
           PERFORM 2011-MOVE-AIRPORT-ORIGIN-NAME                        
           PERFORM 2012-MOVE-AIRPORT-DEST-NAME                          
           PERFORM 2113-MOVE-AIRPORT-ORIGIN-CODE                        
           PERFORM 2114-MOVE-AIRPORT-DEST-CODE                          
           PERFORM 2015-MOVE-DEPARTURE-DATE                             
           PERFORM 2016-MOVE-RETURN-DATE                                
           PERFORM 2017-MOVE-TICKET-NUMBER                              
           PERFORM 2018-MOVE-ONE-WAY-FLAG                               
           PERFORM 2019-MOVE-DIRECT-FLAG                                
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                     2011-MOVE-AIRPORT-ORIGIN-NAME               
      ******************************************************************
       2011-MOVE-AIRPORT-ORIGIN-NAME.                                   
           IF WS-Z02152-I-ORG-NAM = SPACE OR LOW-VALUES THEN            
             CONTINUE                                                   
           ELSE                                                         
             MOVE  WS-Z02152-I-ORG-NAM TO ORG-NAMO                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2012-MOVE-AIRPORT-DEST-NAME                 
      ******************************************************************
       2012-MOVE-AIRPORT-DEST-NAME.                                     
           IF WS-Z02152-I-DES-NAM = SPACE OR LOW-VALUES THEN            
             CONTINUE                                                   
           ELSE                                                         
             MOVE WS-Z02152-I-DES-NAM  TO DES-NAMO                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2113-MOVE-AIRPORT-ORIGIN-CODE                
      ******************************************************************
       2113-MOVE-AIRPORT-ORIGIN-CODE.                                   
           IF WS-Z02152-I-AIR-ORG = SPACE OR LOW-VALUES THEN            
            MOVE CT-EMPTY-AIR-ORG TO AIR-ORGO                           
           ELSE                                                         
            MOVE WS-Z02152-I-AIR-ORG TO AIR-ORGO                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2114-MOVE-AIRPORT-DEST-CODE                
      ******************************************************************
       2114-MOVE-AIRPORT-DEST-CODE.                                     
           IF WS-Z02152-I-AIR-DES = SPACE OR LOW-VALUES THEN            
            MOVE CT-EMPTY-AIR-DES TO AIR-DESO                           
           ELSE                                                         
            MOVE WS-Z02152-I-AIR-DES TO AIR-DESO                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2015-MOVE-DEPARTURE-DATE                   
      ******************************************************************
       2015-MOVE-DEPARTURE-DATE.                                        
           IF WS-Z02152-I-DATE-D = SPACE OR LOW-VALUES THEN             
            MOVE CT-EMPTY-DATE-D TO DATE-DO                             
           ELSE                                                         
            MOVE WS-Z02152-I-DATE-D TO DATE-DO                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2016-MOVE-RETURN-DATE                      
      ******************************************************************
       2016-MOVE-RETURN-DATE.                                           
           IF WS-Z02152-I-DATE-R = SPACE OR LOW-VALUES THEN             
            MOVE CT-EMPTY-DATE-R TO DATE-RO                             
           ELSE                                                         
            MOVE WS-Z02152-I-DATE-R TO DATE-RO                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2017-MOVE-TICKET-NUMBER                    
      ******************************************************************
       2017-MOVE-TICKET-NUMBER.                                         
           IF WS-Z02152-I-TIC-NUM = SPACE OR LOW-VALUES THEN            
            MOVE CT-EMPTY-TIC-NUM TO TIC-NUMO                           
           ELSE                                                         
            MOVE WS-Z02152-I-TIC-NUM TO TIC-NUMO                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2018-MOVE-ONE-WAY-FLAG                       
      ******************************************************************
       2018-MOVE-ONE-WAY-FLAG.                                          
           IF WS-Z02152-I-ONE-WAY = SPACE OR LOW-VALUES THEN            
             MOVE CT-EMPTY-ONE-WAY TO ONE-WAYO                          
           ELSE                                                         
             MOVE WS-Z02152-I-ONE-WAY TO ONE-WAYO                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2019-MOVE-DIRECT-FLAG                        
      ******************************************************************
       2019-MOVE-DIRECT-FLAG.                                           
           IF WS-Z02152-I-DIRECT = SPACE OR LOW-VALUES THEN             
            MOVE CT-EMPTY-DIRECT TO DIRECTO                             
           ELSE                                                         
            MOVE WS-Z02152-I-DIRECT TO DIRECTO                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2050-SEND-FRESH-MAP                        
      ******************************************************************
       2050-SEND-FRESH-MAP.                                             
           MOVE LOW-VALUES TO MP0215O                                   
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                   2060-PROCESS-INPUT                            
      * PROGRAM WILL PERFORM PARAGRAPH DEPENDING ON WHAT KEY USER       
      * PRESSED                                                         
      *                                                                 
      * IF HE PRESSED ENTER THEN PROGRAM WILL GET ALL DATA HE PROVIDED  
      * AND WILL VALIDATE THAT ( LATER PROGRAM Z02172 CAN BE CALLED )   
      *                                                                 
      * IF USER PRESSED F3 THEN PROGRAM WILL GO BACK TO PREVIOUS PROGRAM
      *                                                                 
      * PRESSING ANY OTHER KEY WILL RESULT WITH DISPLAYIN PROPER        
      * MESSAGE ON THE SCREEN ( BY CALLING Z02141 PROGRAM)              
      ******************************************************************
       2060-PROCESS-INPUT.                                              
           EVALUATE EIBAID                                              
           WHEN DFHENTER                                                
               PERFORM 2070-GET-AND-PROCESS-DATA                        
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
      * PARAGRAPH WILL PROCESS USER INPUT                               
      * IF EVERYTHING WILL BE VALID THEN PROGRAM Z02172 WILL BE CALLED  
      *                                                                 
      * PROGRAM CAN ALSO CALL TO Z02162 PROGRAM (WHEN USER PROVIDED     
      * INVALID AIRPORT NAME)                                           
      * OR TO DATE VALIDATION PROGRAM IN CASE OF DATES                  
      ******************************************************************
       2070-GET-AND-PROCESS-DATA.                                       
           PERFORM 2071-GET-DATA-FROM-USER                              
           PERFORM 2072-CHECK-IF-DATA-IS-VALID                          
           PERFORM 2610-PREPARE-DATA                                    
           PERFORM 2800-CALL-TO-FLIGHT-OPTION                           
           .                                                            
      ******************************************************************
      *                   2071-GET-DATA-FROM-USER                       
      ******************************************************************
       2071-GET-DATA-FROM-USER.                                         
           MOVE LOW-VALUES TO MP0215I                                   
           EXEC CICS                                                    
           RECEIVE MAP('MP0215') MAPSET('MP0215')                       
           INTO(MP0215I)                                                
           NOHANDLE                                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
                                                                        
           .                                                            
      ******************************************************************
      *                   2072-CHECK-IF-DATA-IS-VALID                   
      *                                                                 
      * USER WILL HAVE TO PROVIDE :                                     
      *            ORIGIN AIRPORT (ALWAYS),                             
      *            DESTINATION AIRPOT (ALWAYS),                         
      *            DEPARTURE DATE (ALWAYS),                             
      *            RETURN DATE( ONLY WITH RETURN FLIGHTS),              
      *            TICKET NUMBER (ALWAYS),                              
      *            ONE-WAY FLAG ( NOT ALWAYS),                          
      *            DIRECT FLAG  ( NOT ALWAYS)                           
      *                                                                 
      * PARAGRAPH WILL CHECK IF ANY OF THAT VALUE IS EMPTY              
      *                                                                 
      * AND IF THOSE VALUES ARET EMPTY PROGRAM WILL VALIDATE  THEM      
                                                                        
      *                                                                 
      ******************************************************************
       2072-CHECK-IF-DATA-IS-VALID.                                     
           PERFORM 2073-CHECK-WHAT-IS-EMPTY                             
      * PARAGRAPH WILL CHECK IF ALL NEEDED DATA ARE NOT EMPTY           
           PERFORM 2308-CHECK-NEEDED-DATA-EMPTY                         
           PERFORM 2074-VALIDATE-TICKET-NUMBER                          
           PERFORM 2309-VALIDATE-ONE-WAY-FLAG                           
           PERFORM 2310-VALIDATE-DIRECT-FLAG                            
           PERFORM 2311-VALIDATE-AIRPORT-ORIGIN                         
           PERFORM 2312-VALIDATE-AIRPORT-DEST                           
           PERFORM 2313-VALIDATE-DEP-DATE                               
           PERFORM 2314-VALIDATE-RET-DATE                               
      * PARAGRAPH WILL CHECK IF WE DONT FLY TO THE SAME AIRPORT         
           PERFORM 2315-CHECK-IF-ARIRPORT-DIF                           
           .                                                            
      ******************************************************************
      *                   2073-CHECK-WHAT-IS-EMPTY                      
      * PROGRAM WILL CHECK WHAT VALUES ARE EMPTY AND BASED ON THAT      
      * PROGRAM WILL SET FLAGS                                          
      *                                                                 
      * IF VALUE IS PROVIDED PROGRAM WILL SAVE IT TO CORRECT PROGRAM    
      * VARIABLES                                                       
      *                                                                 
      * IF DATA IS PROVIDED PROGRAM WILL REPLACE IT'S '_' SIGN WITH     
      * SPACE, THAT IS BECAUSE ALL FIELDS ON THE MAP ARE FILLED WITH    
      * '_' SYMBOL AND AFTER USER PROVIDES DATA WE WILL GET             
      * SOMETHING LIKE THAT "VALUE_____"                                
      * SO WE NEED TO GET RID OF THOSE UNDERSCORES                      
      *                                                                 
      ******************************************************************
       2073-CHECK-WHAT-IS-EMPTY.                                        
           PERFORM 2301-CHECK-AIRPORT-ORIGIN                            
           PERFORM 2302-CHECK-AIRPORT-DESTINATION                       
           PERFORM 2303-CHECK-DEPARTURE-DATE                            
           PERFORM 2304-CHECK-RETURN-DATE                               
           PERFORM 2305-CHECK-TICKET-NUMBER                             
           PERFORM 2306-CHECK-ONE-WAY-FLAG                              
           PERFORM 2307-CHECK-DIRECT-FLAG                               
           .                                                            
      ******************************************************************
      *                   2074-VALIDATE-TICKET-NUMBER                   
      * IF USER PROVIDED INVALID TICKET NUMBER THEN PROGRAM WILL        
      * DROP ERROR                                                      
      ******************************************************************
       2074-VALIDATE-TICKET-NUMBER.                                     
           IF FUNCTION TEST-NUMVAL(WS-TICKET-NUMBER) = 0 THEN           
              SET SO-TICKET-NUMBER-VALID  TO TRUE                       
              COMPUTE WS-TICKET-NUMBER-TEMP =                           
                FUNCTION NUMVAL(WS-TICKET-NUMBER)                       
              IF WS-TICKET-NUMBER-TEMP <= 00 THEN                       
                                                                        
                PERFORM 2700-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'TICKET NUMBER IS INVALID ' TO                     
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
              END-IF                                                    
           ELSE                                                         
                PERFORM 2700-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'TICKET NUMBER IS INVALID ' TO                     
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2077-VALIDATE-AIRPORT-NAME                     
      * WS-AIRPORT-VALUE IS VALUE PROVIDED BY THE USER                  
      * WE NEED TO CHECK IF MAYBE THIS IS VALID IATA CODE               
      * OR MAYBE VALID AIRPORT FULL NAME                                
                                                                        
      ******************************************************************
       2077-VALIDATE-AIRPORT-NAME.                                      
           MOVE WS-AIRPORT-VALUE TO  T02-AIRPORT-CODE                   
           MOVE WS-AIRPORT-VALUE TO  T02-AIRPORT-FULL-NAME              
           PERFORM 7100-VALIDATE-AIRPORT-IATA                           
           IF SO-INVALID-IATA THEN                                      
              PERFORM 7001-VALIDATE-AIRPORT-NAME                        
              IF SO-VALID-NAME THEN                                     
               MOVE T02-AIRPORT-CODE           TO WS-AIRPORT-IATA-CODE  
               MOVE T02-AIRPORT-FULL-NAME-TEXT TO WS-AIRPORT-FULL-NAME  
               SET SO-VALID-IATA TO TRUE                                
              END-IF                                                    
           ELSE                                                         
      * VALID IATA                                                      
             MOVE T02-AIRPORT-FULL-NAME-TEXT TO WS-AIRPORT-FULL-NAME    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      2100-SEND-THE-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
           SEND MAP('MP0215') MAPSET('MP0215')                          
           FROM(MP0215O)                                                
           ERASE                                                        
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                      2200-CHECK-EIBRESP                         
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
              PERFORM 2316-SEND-MAPFAIL-MSG                             
           WHEN OTHER                                                   
              PERFORM 2317-SEND-OTHER-EIBRESP-MSG                       
           END-EVALUATE                                                 
           .                
      ******************************************************************
      *                   2300-CALL-ERROR-ROUTINE                       
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
           MOVE CT-THIS-PROGRAM-NAME TO   WS-Z02141-I-CALLING-PROGRAM   
           SET SO-Z02141-M-WITH      TO TRUE                            
           SET SO-Z02141-I-FIRST-TIME  TO TRUE                          
           PERFORM 2600-SAVE-DATA-TO-COMMAREA                           
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                2301-CHECK-AIRPORT-ORIGIN                        
      ******************************************************************
       2301-CHECK-AIRPORT-ORIGIN.                                       
           IF AIR-ORGI = SPACE OR LOW-VALUES THEN                       
              SET SO-AIR-ORG-EMPTY TO TRUE                              
           ELSE                                                         
              MOVE AIR-ORGI TO WS-AIRPORT-ORIGIN                        
              INSPECT WS-AIRPORT-ORIGIN REPLACING ALL '_' BY ' '        
              SET SO-AIR-ORG-NOT-EMPTY TO TRUE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2302-CHECK-AIRPORT-DESTINATION                   
      ******************************************************************
       2302-CHECK-AIRPORT-DESTINATION.                                  
           IF AIR-DESI = SPACE OR LOW-VALUES THEN                       
              SET SO-AIR-DES-EMPTY TO TRUE                              
           ELSE                                                         
              DISPLAY 'Z02152, AIR DES, PRZED: ' AIR-DESI               
              SET SO-AIR-DES-NOT-EMPTY TO TRUE                          
              MOVE AIR-DESI TO WS-AIRPORT-DESTINATION                   
              INSPECT WS-AIRPORT-DESTINATION REPLACING ALL '_' BY ' '   
              DISPLAY 'Z02152, AIR DES, PO: ' WS-AIRPORT-DESTINATION    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2303-CHECK-DEPARTURE-DATE                      
      ******************************************************************
       2303-CHECK-DEPARTURE-DATE.                                       
           IF DATE-DI = SPACE OR LOW-VALUES THEN                        
              SET SO-DATE-D-EMPTY  TO TRUE                              
           ELSE                                                         
              SET SO-DATE-D-NOT-EMPTY TO TRUE                           
              MOVE DATE-DI TO WS-DEPARTURE-DATE                         
              INSPECT WS-DEPARTURE-DATE REPLACING ALL '_' BY ' '        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2304-CHECK-RETURN-DATE                         
      ******************************************************************
       2304-CHECK-RETURN-DATE.                                          
           IF DATE-RI = SPACE OR LOW-VALUES THEN                        
              SET SO-DATE-R-EMPTY  TO TRUE                              
           ELSE                                                         
              SET SO-DATE-R-NOT-EMPTY TO TRUE                           
              MOVE DATE-RI TO WS-RETURN-DATE                            
              INSPECT WS-RETURN-DATE REPLACING ALL '_' BY ' '           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2305-CHECK-TICKET-NUMBER                      
      ******************************************************************
       2305-CHECK-TICKET-NUMBER.                                        
                                                                        
           IF TIC-NUMI = SPACE OR LOW-VALUES THEN                       
              SET SO-TIC-NUM-EMPTY  TO TRUE                             
           ELSE                                                         
              SET SO-TIC-NUM-NOT-EMPTY TO TRUE
              MOVE TIC-NUMI TO WS-TICKET-NUMBER                         
              INSPECT WS-TICKET-NUMBER REPLACING ALL '_' BY ' '         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2306-CHECK-ONE-WAY-FLAG                      
      ******************************************************************
       2306-CHECK-ONE-WAY-FLAG.                                         
           IF ONE-WAYI = SPACE OR LOW-VALUES OR '_' THEN                
              SET SO-ONE-WAY-EMPTY  TO TRUE                             
              SET SO-NOT-ONE-WAY-FLIGHT  TO TRUE                        
           ELSE                                                         
              SET SO-ONE-WAY-NOT-EMPTY TO TRUE                          
              MOVE ONE-WAYI TO WS-IF-ONE-WAY-FLAG                       
              SET SO-ONE-WAY-FLIGHT TO TRUE                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2307-CHECK-DIRECT-FLAG                        
      ******************************************************************
       2307-CHECK-DIRECT-FLAG.                                          
           IF DIRECTI = SPACE OR LOW-VALUES OR '_' THEN                 
              SET SO-DIRECT-EMPTY  TO TRUE                              
              SET SO-NOT-DIRECT-FLIGHT   TO TRUE                        
           ELSE                                                         
              SET SO-DIRECT-NOT-EMPTY TO TRUE                           
              SET SO-DIRECT-FLIGHT  TO TRUE                             
              MOVE DIRECTI TO WS-IF-DIRECT-FLAG                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2308-CHECK-NEEDED-DATA-EMPTY                   
      ******************************************************************
       2308-CHECK-NEEDED-DATA-EMPTY.                                    
           IF SO-DATE-D-EMPTY  OR SO-AIR-ORG-EMPTY OR  SO-AIR-DES-EMPTY 
              OR SO-TIC-NUM-EMPTY THEN                                  
              PERFORM 2700-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO SPECIFIY INFORMATIONS BELOW: '          
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              MOVE 'DATE OF DEPARTUERE ' TO WS-Z02141-I-ERROR-MESSAGE(2)
              MOVE 'ORIGIN AIRPORT  ' TO WS-Z02141-I-ERROR-MESSAGE(3)   
              MOVE 'DESTINATION AIRPORT  '                              
                                         TO WS-Z02141-I-ERROR-MESSAGE(4)
              MOVE 'NUMBER OF TICKECTS ' TO WS-Z02141-I-ERROR-MESSAGE(5)
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2309-VALIDATE-ONE-WAY-FLAG                   
      ******************************************************************
       2309-VALIDATE-ONE-WAY-FLAG.                                      
           IF SO-ONE-WAY-NOT-EMPTY THEN                                 
               IF NOT SO-ONE-WAY-FLAG-VALID THEN                        
                 PERFORM 2700-INITIALIZE-ERROR-MESSAGE                  
                  MOVE 'ONE-WAY FLAG SHOULD BE  "X" ' TO                
                                           WS-Z02141-I-ERROR-MESSAGE(1) 
                  SET SO-Z02141-M-WITH TO TRUE                          
                  PERFORM 2300-CALL-ERROR-ROUTINE                       
               END-IF                                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2310-VALIDATE-DIRECT-FLAG                   
      ******************************************************************
       2310-VALIDATE-DIRECT-FLAG.                                       
           IF SO-DIRECT-NOT-EMPTY THEN                                  
               IF NOT SO-DIRECT-FLAG-VALID THEN                         
                 PERFORM 2700-INITIALIZE-ERROR-MESSAGE                  
                  MOVE 'DIRECT FLAG SHOULD BE "X" ' TO                  
                                            WS-Z02141-I-ERROR-MESSAGE(1)
                  SET SO-Z02141-M-WITH TO TRUE                          
                  PERFORM 2300-CALL-ERROR-ROUTINE                       
               END-IF                                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2311-VALIDATE-AIRPORT-ORIGIN                 
      ******************************************************************
       2311-VALIDATE-AIRPORT-ORIGIN.                                    
      * CHECK IF GIVEN ORIGIN AIRPORT IS CORRECT                        
           MOVE WS-AIRPORT-ORIGIN TO WS-AIRPORT-VALUE                   
                                                                        
           PERFORM 2077-VALIDATE-AIRPORT-NAME                           
           IF SO-INVALID-IATA THEN                                      
              MOVE WS-AIRPORT-ORIGIN TO WS-SEARCHED-PHRASE-AIRPORT      
              SET SO-CHECK-AIR-OIGIN  TO TRUE                           
              PERFORM 2500-VALIDATE-AIRPORT-NAME                        
           ELSE                                                         
      * WS-AIRPORT-VALUE STORES CODE OF THE AIRPORT HERE                
              MOVE WS-AIRPORT-IATA-CODE TO WS-Z02152-I-AIR-ORG          
              MOVE WS-AIRPORT-FULL-NAME TO WS-Z02152-I-ORG-NAM          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2312-VALIDATE-AIRPORT-DEST                     
      ******************************************************************
       2312-VALIDATE-AIRPORT-DEST.                                      
      * CHECK IF GIVEN AIRPORT DESTINATION IS VALID                     
           INITIALIZE WS-AIRPORT-VALUE                                  
           INITIALIZE WS-AIRPORT-FULL-NAME                              
                                                                        
           MOVE WS-AIRPORT-DESTINATION     TO WS-AIRPORT-VALUE          
           PERFORM 2077-VALIDATE-AIRPORT-NAME                           
           IF SO-INVALID-IATA THEN                                      
              MOVE WS-AIRPORT-DESTINATION  TO WS-SEARCHED-PHRASE-AIRPORT
                                                                        
              SET SO-CHECK-AIR-DESTINATION TO TRUE     
              PERFORM 2500-VALIDATE-AIRPORT-NAME                        
           ELSE                                                         
      * WS-AIRPORT-VALUE STORES CODE OF THE AIRPORT HERE                
              MOVE WS-AIRPORT-IATA-CODE    TO WS-Z02152-I-AIR-DES       
              MOVE WS-AIRPORT-FULL-NAME    TO WS-Z02152-I-DES-NAM       
              DISPLAY ' IN DES CHECK : ' WS-Z02152-I-DES-NAM            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2313-VALIDATE-DEP-DATE                        
      ******************************************************************
       2313-VALIDATE-DEP-DATE.                                          
           DISPLAY '2313 VALIDATE DEP DATE'                             
           SET ZZEC0243-M-10-CHAR  TO TRUE                              
           MOVE WS-DEPARTURE-DATE TO ZZEC0243-I-DATE-VALUE              
           PERFORM 2350-VALIDATE-DATE-VALUE                             
           .                                                            
      ******************************************************************
      *                   2314-VALIDATE-RET-DATE                        
      ******************************************************************
       2314-VALIDATE-RET-DATE.                                          
           DISPLAY '2314 VALIDATE ARV DATE'                             
           IF SO-ONE-WAY-FLAG-VALID THEN                                
              CONTINUE                                                  
           ELSE                                                         
              SET ZZEC0243-M-10-CHAR  TO TRUE                           
              MOVE WS-RETURN-DATE TO ZZEC0243-I-DATE-VALUE              
              PERFORM 2350-VALIDATE-DATE-VALUE                          
            END-IF                                                      
           .                                                            
      ******************************************************************
      *                   2315-CHECK-IF-ARIRPORT-DIF                    
      ******************************************************************
       2315-CHECK-IF-ARIRPORT-DIF.                                      
                                                                        
           DISPLAY '2315 CHECK: '  
           IF WS-AIRPORT-DESTINATION = WS-AIRPORT-ORIGIN  THEN          
             MOVE 'YOU CANT FLY TO THE SAME AIRPORT '                   
                              TO WS-Z02141-I-ERROR-MESSAGE(1)           
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2316-SEND-MAPFAIL-MSG                        
      ******************************************************************
       2316-SEND-MAPFAIL-MSG.                                           
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           PERFORM 2700-INITIALIZE-ERROR-MESSAGE                        
           MOVE ' YOU NEED TO PROVIDE  DATA '                           
           TO WS-Z02141-I-ERROR-MESSAGE(1)                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2317-SEND-OTHER-EIBRESP-MSG                   
      ******************************************************************
       2317-SEND-OTHER-EIBRESP-MSG.                                     
           PERFORM 2700-INITIALIZE-ERROR-MESSAGE                        
           DISPLAY 'UNKNOWN EIBERSP '                                   
           DISPLAY 'EIBRESP VALUE : ' EIBRESP                           
           MOVE EIBRESP TO WS-EIBRESP-TEMP                              
           STRING 'OTHER  EIBRESP, EIBRESP VALUE ' WS-EIBRESP-TEMP      
            DELIMITED BY SIZE                                           
            INTO WS-Z02141-I-ERROR-MESSAGE(1)                           
           END-STRING                                                   
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2318-SEND-INVALID-CALL-MSG                    
      * PARAGRAPH WILL BE PERFORMED ONLY WHEN PROGRAM HAS INVALID       
      * MODE                                                            
      ******************************************************************
       2318-SEND-INVALID-CALL-MSG.                                      
           PERFORM 2700-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'INVALID CALL ' TO WS-Z02141-I-ERROR-MESSAGE(1)         
           SET SO-M-FIRST-WITH TO  TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2350-VALIDATE-DATE-VALUE                      
      ******************************************************************
       2350-VALIDATE-DATE-VALUE.                                        
                                                                        
           CALL CT-DATE-ROUTINE-NAME USING ZZEC0243                     
                                                                        
           IF ZZEC0243-O-RC-NO-ERROR THEN                               
              DISPLAY 'DATE ROUTINE NO ERROR   '                        
           ELSE                                                         
             DISPLAY 'DATE ROUTINE ERROR '                              
             PERFORM 2700-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'DATE ERROR ' TO WS-Z02141-I-ERROR-MESSAGE(1)         
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2500-VALIDATE-AIRPORT-NAME                    
      ******************************************************************
       2500-VALIDATE-AIRPORT-NAME.                                      
           PERFORM 2600-SAVE-DATA-TO-COMMAREA                           
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           MOVE 'Z02152  ' TO WS-RETURN-CONTROL-PROGRAM                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM('Z02162') COMMAREA(DFHCOMMAREA)               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .     
      ******************************************************************
      *              2600-SAVE-DATA-TO-COMMAREA                         
      ******************************************************************
       2600-SAVE-DATA-TO-COMMAREA.                                      
           MOVE WS-AIRPORT-ORIGIN TO WS-Z02152-I-AIR-ORG                
           MOVE WS-AIRPORT-DESTINATION TO WS-Z02152-I-AIR-DES           
           MOVE WS-DEPARTURE-DATE TO WS-Z02152-I-DATE-D                 
           MOVE WS-RETURN-DATE TO WS-Z02152-I-DATE-R                    
           MOVE WS-TICKET-NUMBER TO WS-Z02152-I-TIC-NUM                 
           MOVE WS-IF-ONE-WAY-FLAG TO WS-Z02152-I-ONE-WAY               
           MOVE WS-IF-DIRECT-FLAG TO WS-Z02152-I-DIRECT                 
           .                                                            
      ******************************************************************
      *              2601-MOVE-DATA-TO-COMMAREA                         
      ******************************************************************
       2601-MOVE-DATA-TO-COMMAREA.                                      
           MOVE WS-Z02152-I-AIR-ORG TO  WS-Z02172-ORIGIN-AIRPORT-IATA   
           MOVE WS-Z02152-I-AIR-DES TO  WS-Z02172-DEST-AIRPORT-IATA     
           MOVE WS-Z02152-I-DATE-D  TO  WS-Z02172-DEPARTURE-DATE        
           MOVE WS-Z02152-I-DATE-R  TO  WS-Z02172-RETURN-DATE           
           MOVE WS-TICKET-NUMBER    TO  WS-Z02172-TICKET-NUMBER         
           .                                                            
      ******************************************************************
      *                       2610-PREPARE-DATA                         
      * PARAGRAPH WILL MOVE DATA FROM THIS PROGRAM TO CORRECT           
      * PLACES IN COMMAREA                                              
      ******************************************************************
       2610-PREPARE-DATA.                                               
           PERFORM 2600-SAVE-DATA-TO-COMMAREA                           
           PERFORM 2601-MOVE-DATA-TO-COMMAREA                           
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
      *              2700-INITIALIZE-ERROR-MESSAGE                      
      * PARAGRAPH WILL BE PERFORMED IF USER PROVIDED VALID INPUT        
      * AND PRESSED ENTER                                               
      *                                                                 
      * PROGRAM CALLED HERE WILL DISPLAY ALL FLIGHTS THAT MEETS         
      * CRITERIA PROVIDED BY THE USER                                   
      * IF NO FLIGHT COULD BE FOUND THEN USER CAN GO BACK TO THIS       
      * PROGRAM BY PRESSING F3 BUTTON                                   
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
      * PROGRAM WILL RETURN PROGRAM (OR CALL TO ERROR ROUTINE)          
      * DEPENDING ON THE FLAGS SET BY THE PROGRAM BEFORE                
      * PROGRAM LOGIC CAN BE RETURNED TO CALLING PROGRAM OR             
      * WE WILL USE RETURN WITH TRANSID IN ORDER TO CREATE PSEUDO       
      * CONVERSATIONAL PROGRAM                                          
      ******************************************************************
       3000-FINAL.                                                      
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EVALUATE TRUE                                                
           WHEN SO-GO-TO-PREVIOUS-PROGRAM                               
              PERFORM 3001-CALL-PREVIOUS-PROGRAM                        
           WHEN SO-FINAL-WITH-COMMAREA                                  
              SET SO-M-NOT-FIRST TO TRUE                                
              PERFORM 3002-RETURN-WITH-TRANSID                          
           WHEN OTHER                                                   
              PERFORM 3003-SEND-INVALID-OPTION-MSG                      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     3001-CALL-PREVIOUS-PROGRAM                  
      * PREVIOUS PROGRAM (Z02131) HAVE TO BE CALLED WITHOUT COMMAREA    
      * (EIBCALEN NEEDS TO BE 0)                                        
      * XCTL STATEMENT BELOW HAS TO BE USED TO MOVE ROGRAM LOGIC BACK   
      * TO THIS PROGRAM                                                 
      ******************************************************************
       3001-CALL-PREVIOUS-PROGRAM.                                      
           EXEC CICS                                                    
            XCTL PROGRAM(CT-NAME-OF-PROG-BEFORE)                        
            COMMAREA(DFHCOMMAREA) LENGTH(0)                             
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                     3002-RETURN-WITH-TRANSIDAM                  
      * PROGRAM WILL END THIS TRANSACTION WITH OPTION TO RETTRIGER      
      * AFTER USER WILL PRESS AN ATTENTION KEY                          
      ******************************************************************
       3002-RETURN-WITH-TRANSID.                                        
           EXEC CICS                                                    
             RETURN TRANSID('0209') COMMAREA(DFHCOMMAREA)               
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    3003-SEND-INVALID-OPTION-MSG                 
      * PARAGRAPH WILL BE PERFORMED IF IN 3000'S EVALUATE STATMENT      
      * THERE WILL BE "OTHER OPTION"                                    
      ******************************************************************
       3003-SEND-INVALID-OPTION-MSG.                                    
           PERFORM  2700-INITIALIZE-ERROR-MESSAGE                       
           MOVE 'INVALID 3000 FINAL MODE ' TO                           
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                  7100-VALIDATE-AIRPORT-IATA                     
      * PARAGRAPH WILL CHECK IF THERE IS AN AIRPORT NAME                
      * THAT HAS SAME AIRPORT IATA CODE LIKE THE ONE PROVIDED BY THE    
      * USER                                                            
      * IF THIS PRARAGRAPH WONT FIND A MATHING POW THEN                 
      * FLAG SO-INVALID-IATA WILL BE SET TO TRUE                        
      *                                                                 
      * LATER OTHER PARAGRAPH WILL CHECK IF MAYBE USER PROVIDED         
      * VALID FULL AIRPORT-NAME NOT ONLY A IATA CODE                    
      *                                                                 
      * IF THIS WILL ALSO FAIL THE PROGRAM WILL CALL TO Z02162 PROGRAM  
      * AND THIS PROGRAM WILL DISPLAY ALL SIMILAR AIRPORT NAMES TO      
      * WHAT USER PROVIDED (IF THIS IS POSSIBLE)                        
      ******************************************************************
       7100-VALIDATE-AIRPORT-IATA.                                      
           MOVE WS-AIRPORT-VALUE TO T02-AIRPORT-CODE                    
           INITIALIZE   T02-AIRPORT-FULL-NAME                           
           EXEC SQL                                                     
              SELECT                                                    
                AIRPORT_CODE                                            
                ,AIRPORT_FULL_NAME                                      
              INTO                                                      
                :T02-AIRPORT-CODE                                       
                ,:T02-AIRPORT-FULL-NAME                                 
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
      *               7001-VALIDATE-AIRPORT-NAME                        
      ******************************************************************
       7001-VALIDATE-AIRPORT-NAME.                                      
           INITIALIZE   T02-AIRPORT-FULL-NAME                           
           EXEC SQL                                                     
              SELECT                                                    
                  AIRPORT_CODE                                          
                 ,AIRPORT_FULL_NAME                                     
              INTO                                                      
                   :T02-AIRPORT-CODE                                    
                  ,:T02-AIRPORT-FULL-NAME                               
              FROM T02_AIRPORT_TABLE                                    
              WHERE AIRPORT_FULL_NAME = :T02-AIRPORT-FULL-NAME          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               SET SO-VALID-NAME   TO TRUE                              
           WHEN SO-SQLCODE-NOT-FOUND                                    
               SET SO-INVALID-NAME TO TRUE                              
           WHEN OTHER                                                   
               SET SO-7100-PARA TO TRUE                                 
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
                                                       
                                     
                 
                          
                                            

           

      
