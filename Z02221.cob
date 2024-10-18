       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02221.                                              
      ***************************************************************** 
      *                   Z02221 (0217)                                 
      *                                                                 
      *  PROGRAM CAN BE CALLED FROM 2 PLACES IN THE CODE                
      *  1. CAN BE CALLED BY Z02131 PROGRAM WHEN USER CHOSE '2' OPTION  
      * "FIND A FLIGHT"                                                 
      *                                                                 
      *  2. CAN BE CALLED BY Z02321 PROGRAM                             
      *                                                                 
      *                                                                 
      * THIS PROGRAM WILL ALLOW USER TO PROVIDE DATA ABOUT THE FLIGHT   
      *                                                                 
      * WHEN PROGRAM IS CALLED BY Z02131 THEN USER WILL BE ALLOWED TO   
      * PROVIDE MORE DATA                                               
      *                                                                 
      * WHEN PROGRAM IS CALELD BYZ Z02321 USER WILL WE ALLOWED TO       
      *  PROVIDE FLITHT NUMBER ONLY                                     
      *                                                                 
      * IN BOTH SCENARIOS IF USER WONT SPECIFY ANYTHING THEN            
      *  NEXT PROGRAM (Z02232) WILL DISPLAY ALL POSSIBLE FLIGHTS        
      *                                                                 
      *                                                                 
      *  DEPENDING ON THE WHAT PROGRAM WAS THE CALLING ONE              
      * THE NEXT PROGRAM LOGIC (Z02232) WILL BE CHANGED                 
      *                                                                 
      * IF THIS PROGRAM (Z02221)  IS CALLED BY Z02131 THEN USER CAN     
      * SEE GRAPHICAL REPRESENTATION OF THE SEATS OR LIST OF THE        
      * PASSENGERS. THIS CHOICE WILL BE MADE BY PLACING '1' OR '2' NEXT 
      * TO THE FLIGHT NAME                                              
      *                                                                 
      *                                                                 
      *  IF THIS PROGRAM WAS CALLED BY Z02321 THEN USER WILL BE ABLE    
      * TO DELETE A FLIGHT BY PLACING 'X' NEXT TO IT AND PRESSEING ENTER
      * (IN Z02232 PROGRAM)                                             
      *                                                               
      ****************************************************************
       DATA DIVISION.                                                 
       WORKING-STORAGE SECTION.                                       
           COPY DFHAID.                                               
           COPY ZZEC0215.                                             
           COPY ZZMP0222.                                             
           COPY ZZEC0243.                                             
           COPY DFHBMSCA.                                             
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
       01 CT-CONSTANTS.                                               
            05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02132  '.     
            05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02221  '.     
            05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.     
            05 CT-FIRST-PROG-NAME      PIC X(8) VALUE 'Z02131  '.     
            05 CT-DATE-ROUTINE-NAME    PIC X(8) VALUE 'Z02043  '.     
            05 CT-DISPLAY-PROG-NAME    PIC X(8) VALUE 'Z02232  '.     
       01 SW-SWITCHES.                                                
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME             PIC X.        
               88 SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'.  
               88 SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'.  
               88 SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'.  
           05 SW-WHAT-TYPE-OF-END                           PIC X.      
               88 SO-FINAL-WITH-COMMAREA                    VALUE '1'.  
               88 SO-FINAL-TERMINATION                      VALUE '2'.  
           05 SW-IF-INPUT-EMPTY                             PIC X.      
               88 SO-EMPTY-INPUT                            VALUE '1'.  
               88 SO-NOT-EMPTY-INPUT                        VALUE '2'.  
           05 SW-IF-FLIGHT-NUMBER-EMPTY                     PIC X.      
               88 SO-FLIGHT-NUMBER-EMPTY                    VALUE '1'.  
               88 SO-FLIGHT-NUMER-NOT-EMPTY                 VALUE '2'.  
                                                                        
           05 SW-IF-ORIGIN-AIRPORT-EMPTY                    PIC X.      
               88 SO-ORIGIN-AIRPORT-EMPTY                   VALUE '1'.  
               88 SO-ORIGIN-AIRPORT-NOT-EMPTY               VALUE '2'.  
                                                                        
           05 SW-IF-DEST-AIRPORT-EMPTY                      PIC X.      
               88 SO-DEST-AIRPORT-EMPTY                     VALUE '1'.  
               88 SO-DEST-AIRPORT-NOT-EMPTY                 VALUE '2'.  
                                                                        
           05 SW-IF-ARRIVAL-DATE-EMPTY                      PIC X.      
               88 SO-ARRIVAL-DATE-EMPTY                     VALUE '1'.  
               88 SO-ARRIVAL-DATE-NOT-EMPTY                 VALUE '2'.  
           05 SW-IF-DEPARTURE-DATE-EMPTY                    PIC X.      
               88 SO-DEPARTURE-DATE-EMPTY                   VALUE '1'.  
               88 SO-DEPARTURE-DATE-NOT-EMPTY               VALUE '2'.  
                                                                        
       01 WS-VARIABLES.                                                 
           05  WS-ARRIVAL-DATE                   PIC X(10).             
           05  WS-DEPARTURE-DATE                 PIC X(10).             
           05  WS-DEST-AIRPORT                   PIC X(3).              
           05  WS-ORIGIN-AIRPORT                 PIC X(3).              
           05  WS-FLIGHT-NUMBER                  PIC X(15).             
           05  WS-ITER1                          PIC S9(4) COMP VALUE 0.
           05  WS-ITER2                          PIC S9(4) COMP VALUE 0.
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                           1000-INIT                             
      ******************************************************************
       1000-INIT.                                                       
           PERFORM  1005-CHECK-IF-FIRST-TIME                            
           PERFORM  1006-INITIALIZE-COMMAREA                            
           .                                                            
      ***************************************************************** 
      *                 1005-CHECK-IF-FIRST-TIME                        
      * PROGRAM CAN HAVE 3 MODES                                        
      * 1.  SO-M-FIRST-WITHOUT PROGRAM IS RUN FOR THE FIRST TIME        
      *    (NO DATA WAS PROVIDED BY THE USER)                           
      * 2.  SO-M-FIRST-WITH    PROGRAM IS RUN FOR THE FIRST TIME        
      *    (BUT USER ALREADY PROVIDED SOME DATA AND WE JUST HAVE        
      *   TO DISPLAY THEM)                                              
      * 3.   SO-M-NOT-FIRST PROGRAM RUNS NOT FOR THE FIRST TIME         
      *    (PROGRAM RUNS BECAUSE USER PRESSED ATTENTION KEY)            
      *                                                                 
      ***************************************************************** 
       1005-CHECK-IF-FIRST-TIME.                                        
           INITIALIZE WS-ZZEC0215                                       
                                                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           DISPLAY 'MODE PROGRAMU: ' SW-M-WHAT-MODE                     
                                                                        
           EVALUATE TRUE                                                
             WHEN SO-M-FIRST-WITHOUT                                    
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1015-SET-START-FLAGS                             
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
      *                   1006-INITIALIZE-COMMAREA                      
      ******************************************************************
       1006-INITIALIZE-COMMAREA.                                        
           INITIALIZE  Z02232-ARRIVAL-DATE                              
           INITIALIZE  Z02232-DEPARTURE-DATE                            
           INITIALIZE  Z02232-DEST-AIRPORT                              
           INITIALIZE  Z02232-ORIGIN-AIRPORT                            
           INITIALIZE  Z02232-FLIGHT-NUMBER                             
           SET         SO-CONFIRMED-ST-EMPTY   TO TRUE                  
           SET         SO-CANCELED-ST-EMPTY    TO TRUE                  
           SET         SO-BOARDING-ST-EMPTY    TO TRUE                  
           SET         SO-DEPARTED-ST-EMPTY    TO TRUE                  
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
           CONTINUE                                                     
           .                                                            
      ****************************************************************  
      *                          2000-PROCESS                           
      ****************************************************************  
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
               MOVE 'SERIOUS ERROR IN Z02212' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   2001-PROCESS-FIRST-TIME                       
      ****************************************************************  
       2001-PROCESS-FIRST-TIME.                                         
           MOVE LOW-VALUES TO MP0222O                                   
           PERFORM 2105-PREPARE-THE-MAP                                 
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                   2002-PROCESS-WITH-DATA                        
      ****************************************************************  
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2105-PREPARE-THE-MAP                                 
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                   2003-PROCESS-NOT-FIRST-TIME                   
      * PROGRAM WILL CHECK WHAT KEY WAS PRESSED BY THE USER             
      * IF USER PRESSED 'F3' THEN PROGRAM CONTROL WILL GO BACK          
      * TO CALLING PROGRAM                                              
      *                                                                 
      * IF USER PRESSED ENTER THEN PROGRAM WILL MAKE ACTIONS DEPENING   
      * ON WHAT HE PROVIDED ON THE SCREEN                               
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           DISPLAY '2003 PERFORMED '                                    
           PERFORM 2004-INITIALIZE-COMMAREA                             
           EVALUATE EIBAID                                              
           WHEN DFHPF3                                                  
               SET SO-FINAL-TERMINATION TO TRUE                         
           WHEN DFHENTER                                                
               DISPLAY 'ENTER PRESSED'                                  
               PERFORM 2101-PROCESS-INPUT                               
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID KEY            ' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   2004-INITIALIZE-COMMAREA                      
      ****************************************************************  
       2004-INITIALIZE-COMMAREA.                                        
           PERFORM 1006-INITIALIZE-COMMAREA                             
           .                                                            
      ****************************************************************  
      *                    2100-SEND-THE-MAP                            
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0222') MAPSET('MP0222')                        
             FROM(MP0222O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                     2101-PROCESS-INPUT                          
      ****************************************************************  
       2101-PROCESS-INPUT.                                              
           PERFORM 2102-RECEIVE-MAP                                     
           IF SO-EMPTY-INPUT THEN                                       
             DISPLAY 'SO EMPTY INNPUT '                                 
             SET SO-Z02232-M-EMPTY-INPUT      TO TRUE                   
           ELSE                                                         
             DISPLAY 'SO NOT EMPTY INNPUT '                             
             PERFORM 2110-PREPARE-DATA                                  
             SET SO-Z02232-M-NOT-EMPTY-INPUT  TO TRUE                   
           END-IF                                                       
           PERFORM 2600-CALL-DISPLAY-PROGRAM                            
           .                                                            
      ****************************************************************  
      *                     2102-RECEIVE-MAP                            
      ****************************************************************  
       2102-RECEIVE-MAP.                                                
           SET SO-NOT-EMPTY-INPUT TO TRUE                               
           MOVE LOW-VALUES TO MP0222I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0222') MAPSET('MP0222')                      
            INTO(MP0222I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
                PERFORM 2104-CHECK-WHAT-IS-EMPTY                        
           WHEN DFHRESP(MAPFAIL)                                        
                SET SO-EMPTY-INPUT TO TRUE                              
           WHEN OTHER                                                   
                PERFORM 2200-CHECK-EIBRESP                              
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *            F   2104-CHECK-WHAT-IS-EMPTY                         
      ****************************************************************  
       2104-CHECK-WHAT-IS-EMPTY.                                        
                                                                        
           INITIALIZE  WS-ARRIVAL-DATE                                  
           INITIALIZE  WS-DEPARTURE-DATE                                
           INITIALIZE  WS-DEST-AIRPORT                                  
           INITIALIZE  WS-ORIGIN-AIRPORT                                
           INITIALIZE  WS-FLIGHT-NUMBER                                 
           PERFORM 2301-INITIALIZE-THE-FLAGS                            
           PERFORM 2302-CHECK-FLIGHT-NUMBER-EMPTY                       
      * IF THIS PROGRAM IS CALLED BY Z02131                             
      *                                                                 
      * IF THIS PROGRAM IS CALLED BY Z02321 THEN THIS FIELDS WILL BE    
      * HIDED FROM THE USER                                             
           IF SO-PROVIDE-ALL-DATA THEN                                  
             PERFORM 2303-IF-ORIGIN-AIRPORT-EMPTY                       
             PERFORM 2304-IF-DEST-AIRPORT-EMPTY                         
             PERFORM 2305-IF-DEPARTURE-DATE-EMPTY                       
             PERFORM 2306-IF-ARRIVAL-DATE-EMPTY                         
             PERFORM 2307-CHECK-FLIGHT-STATUSES                         
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                      2105-PREPARE-THE-MAP                       
      * IF PROGRAM WAS CALLED BY Z02131 PROGRAM THEN WHOLE MAP          
      * WILL BE SENT ( WE WILL JUST FORMAT THE FIELDS HERE)             
      *                                                                 
      * IF PROGRAM WAS CALLED BY Z02321 THEN WE WILL ONLY DISPLAY       
      * 1 FIELDS WHEN USER WILL BE ABLE TO PROVIDE FLIGHT NUMBER        
      ****************************************************************  
       2105-PREPARE-THE-MAP.                                            
           IF SO-PROVIDE-ALL-DATA THEN                                  
             MOVE LOW-VALUES TO FLG-NA                                  
             MOVE LOW-VALUES TO AIR1A                                   
             MOVE LOW-VALUES TO AIR-ORGA                                
             MOVE LOW-VALUES TO AIR2A                                   
             MOVE LOW-VALUES TO AIR-DESA                                
             MOVE LOW-VALUES TO AIR3A                                   
             MOVE LOW-VALUES TO DEP-DA                                  
             MOVE LOW-VALUES TO AIR4A                                   
             MOVE LOW-VALUES TO ARV-DA                                  
             MOVE LOW-VALUES TO AIR5A                                   
             MOVE LOW-VALUES TO AIR6A                                   
             MOVE LOW-VALUES TO S-CONFA                                 
             MOVE LOW-VALUES TO AIR7A                                   
             MOVE LOW-VALUES TO S-CANCA                                 
             MOVE LOW-VALUES TO AIR8A                                   
             MOVE LOW-VALUES TO S-BORDA                                 
             MOVE LOW-VALUES TO AIR9A                                   
             MOVE LOW-VALUES TO S-DEPAA                                 
           ELSE                                                         
             MOVE LOW-VALUES TO FLG-NA                                  
             MOVE DFHBMDAR   TO AIR1A                                   
             MOVE DFHBMDAR   TO AIR-ORGA                                
             MOVE DFHBMDAR   TO AIR2A                                   
             MOVE DFHBMDAR   TO AIR-DESA                                
             MOVE DFHBMDAR   TO AIR3A                                   
             MOVE DFHBMDAR   TO DEP-DA                                  
             MOVE DFHBMDAR   TO AIR4A                                   
             MOVE DFHBMDAR   TO ARV-DA                                  
             MOVE DFHBMDAR   TO AIR5A                                   
             MOVE DFHBMDAR   TO AIR6A                                   
             MOVE DFHBMDAR   TO S-CONFA                                 
             MOVE DFHBMDAR   TO AIR7A                                   
             MOVE DFHBMDAR   TO S-CANCA                                 
             MOVE DFHBMDAR   TO AIR8A                                   
             MOVE DFHBMDAR   TO S-BORDA                                 
             MOVE DFHBMDAR   TO AIR9A                                   
             MOVE DFHBMDAR   TO S-DEPAA                                 
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                      2110-PREPARE-DATA                          
      * PARAGRAPH JUST MOVES DATA TO VALID COMMAREA FIELDS              
      ****************************************************************  
       2110-PREPARE-DATA.                                               
           DISPLAY '21110 PERPARE DATA '                                
           MOVE WS-ARRIVAL-DATE   TO Z02232-ARRIVAL-DATE                
           MOVE WS-DEPARTURE-DATE TO Z02232-DEPARTURE-DATE              
                                                                        
           MOVE WS-DEST-AIRPORT   TO Z02232-DEST-AIRPORT                
           MOVE WS-ORIGIN-AIRPORT TO Z02232-ORIGIN-AIRPORT              
           MOVE WS-FLIGHT-NUMBER  TO Z02232-FLIGHT-NUMBER               
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
           SET  SO-Z02141-I-FIRST-TIME TO TRUE                          
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    2301-INITIALIZE-THE-FLAGS                    
      ******************************************************************
       2301-INITIALIZE-THE-FLAGS.                                       
           SET  SO-FLIGHT-NUMER-NOT-EMPTY  TO TRUE                      
           SET  SO-ORIGIN-AIRPORT-NOT-EMPTY TO TRUE                     
           SET  SO-DEST-AIRPORT-NOT-EMPTY  TO TRUE                      
                                                                        
           SET  SO-ARRIVAL-DATE-NOT-EMPTY  TO TRUE                      
           SET  SO-CONFIRMED-ST-NOT-EMPTY  TO TRUE                      
                                                                        
           SET  SO-CANCELED-ST-NOT-EMPTY   TO TRUE                      
           SET  SO-BOARDING-ST-NOT-EMPTY   TO TRUE                      
           SET  SO-DEPARTED-ST-NOT-EMPTY   TO TRUE                      
           .                                                            
      ******************************************************************
      *                  2302-CHECK-FLIGHT-NUMBER-EMPTY                 
      ******************************************************************
       2302-CHECK-FLIGHT-NUMBER-EMPTY.                                  
           DISPLAY '2302 PERFORMED FLG-NI: ' FLG-NI                     
           IF FLG-NI = SPACE OR LOW-VALUES THEN                         
             DISPLAY 'FLG-NI EMPTY'                                     
             SET SO-FLIGHT-NUMBER-EMPTY TO TRUE                         
           ELSE                                                         
             DISPLAY 'FLG-NI NOT EMPTY'                                 
             MOVE FLG-NI TO WS-FLIGHT-NUMBER                            
             INSPECT WS-FLIGHT-NUMBER REPLACING ALL '_' BY ' '          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2303-IF-ORIGIN-AIRPORT-EMPTY                    
      ******************************************************************
       2303-IF-ORIGIN-AIRPORT-EMPTY.                                    
           IF AIR-ORGI = SPACE OR LOW-VALUES THEN                       
              SET SO-ORIGIN-AIRPORT-EMPTY TO TRUE                       
           ELSE                                                         
              MOVE AIR-ORGI TO WS-ORIGIN-AIRPORT                        
              INSPECT WS-ORIGIN-AIRPORT REPLACING ALL '_' BY ' '        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2304-IF-DEST-AIRPORT-EMPTY                     
      ******************************************************************
       2304-IF-DEST-AIRPORT-EMPTY.                                      
           IF AIR-DESI = SPACE OR LOW-VALUES THEN                       
              SET SO-DEST-AIRPORT-EMPTY TO TRUE                         
           ELSE                                                         
              MOVE AIR-DESI TO WS-DEST-AIRPORT                          
              INSPECT WS-DEST-AIRPORT REPLACING ALL '_' BY ' '          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2305-IF-DEPARTURE-DATE-EMPTY                    
      ******************************************************************
       2305-IF-DEPARTURE-DATE-EMPTY.                                    
           IF DEP-DI = SPACE OR LOW-VALUES THEN                         
              SET SO-DEPARTURE-DATE-EMPTY TO TRUE                       
           ELSE                                                         
                                                                        
              MOVE DEP-DI TO WS-DEPARTURE-DATE                          
              MOVE WS-DEPARTURE-DATE TO ZZEC0243-I-DATE-VALUE           
              SET ZZEC0243-M-10-CHAR TO TRUE                            
              PERFORM 2350-VALIDATE-DATE-VALUE                          
              INSPECT WS-DEPARTURE-DATE REPLACING ALL '_' BY ' '        
           END-IF                                                       
           .   
      ******************************************************************
      *                 2306-IF-ARRIVAL-DATE-EMPTY                      
      ******************************************************************
       2306-IF-ARRIVAL-DATE-EMPTY.                                      
           IF ARV-DI = SPACE OR LOW-VALUES THEN                         
              SET SO-ARRIVAL-DATE-EMPTY TO TRUE                         
           ELSE                                                         
              MOVE ARV-DI TO WS-ARRIVAL-DATE                            
              MOVE WS-ARRIVAL-DATE TO ZZEC0243-I-DATE-VALUE             
              SET ZZEC0243-M-10-CHAR TO TRUE                            
              PERFORM 2350-VALIDATE-DATE-VALUE                          
              INSPECT WS-ARRIVAL-DATE REPLACING ALL '_' BY ' '          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2307-CHECK-FLIGHT-STATUSES                      
      ******************************************************************
       2307-CHECK-FLIGHT-STATUSES.                                      
           PERFORM 2308-CHECK-CONFIRMED-STATUS                          
           PERFORM 2309-CHECK-CANCELED-STATUS                           
           PERFORM 2310-CHECK-BOARDING-STATUS                           
           PERFORM 2311-CHECK-DEPARTED-STATUS                           
           .                                                            
      ******************************************************************
      *                 2308-CHECK-CONFIRMED-STATUS                     
      ******************************************************************
       2308-CHECK-CONFIRMED-STATUS.                                     
           IF S-CONFI = SPACE OR LOW-VALUES THEN                        
              DISPLAY 'SCONFI EMPTY'                                    
                                                                        
              SET SO-CONFIRMED-ST-EMPTY TO TRUE                         
           ELSE                                                         
              IF S-CONFI NOT = 'X' THEN                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID STATUS ' TO WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2309-CHECK-CANCELED-STATUS                      
      ******************************************************************
       2309-CHECK-CANCELED-STATUS.                                      
           IF S-CANCI = SPACE OR LOW-VALUES THEN                        
              DISPLAY 'SCANCI EMPTY'                                    
              SET SO-CANCELED-ST-EMPTY TO TRUE                          
           ELSE                                                         
              IF S-CANCI NOT = 'X' THEN                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID STATUS ' TO WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2310-CHECK-BOARDING-STATUS                       
      ******************************************************************
       2310-CHECK-BOARDING-STATUS.                                      
           IF S-BORDI = SPACE OR LOW-VALUES THEN                        
              DISPLAY 'SBORDI EMPTY'                                    
              SET SO-BOARDING-ST-EMPTY TO TRUE                          
           ELSE                                                         
              IF S-BORDI NOT = 'X' THEN                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID STATUS ' TO WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           END-IF                                                       
           .            
      ******************************************************************
      *                 2311-CHECK-DEPARTED-STATUS                      
      ******************************************************************
       2311-CHECK-DEPARTED-STATUS.                                      
           IF S-DEPAI = SPACE OR LOW-VALUES THEN                        
              SET SO-DEPARTED-ST-EMPTY TO TRUE                          
           ELSE                                                         
              IF S-DEPAI NOT = 'X' THEN                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID STATUS ' TO WS-Z02141-I-ERROR-MESSAGE(1)   
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2350-VALIDATE-DATE-VALUE                       
      * PARAGRAPH WILL MAKE A CALL TO DATE ROUTINE THAT WILL VALIDATE   
      * IF DATE PROVIDED BY THE USER IS VALID OR NOT                    
      ******************************************************************
       2350-VALIDATE-DATE-VALUE.                                        
           CALL CT-DATE-ROUTINE-NAME USING ZZEC0243                     
                                                                        
                                                                        
           IF ZZEC0243-O-RC-NO-ERROR THEN CONTINUE                      
           ELSE                                                         
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'DATE ERROR ' TO WS-Z02141-I-ERROR-MESSAGE(1)         
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
      *                   2600-CALL-DISPLAY-PROGRAM                     
      * PROGRAM WILL CALL TO Z02232 PROGRAM                             
      * AND THIS PROGRAM WILL DISPLAY ALL THE FLIGHTS THAT MEETS        
      * USER CRITERIA                                                   
      ******************************************************************
       2600-CALL-DISPLAY-PROGRAM.                                       
           DISPLAY '2600 DISPLAY STATUSOW: '                            
           SET  SO-M-FIRST-WITHOUT TO TRUE                              
           IF SO-PROVIDE-ONLY-FLIGHT-N THEN                             
              SET SO-SEARCH-ONLY-FLG-ID TO TRUE                         
           ELSE                                                         
              SET SO-SEARCH-ALL-DATA    TO TRUE                         
           END-IF                                                       
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-DISPLAY-PROG-NAME) COMMAREA(DFHCOMMAREA)    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ***************************************************************** 
      *                          3000-FINAL                             
      ***************************************************************** 
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3001-RETURN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
              PERFORM 3002-RETURN-TO-FIRST-PROG                         
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO   WS-Z02141-I-ERROR-MESSAGE(1) 
              SET     SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *               3001-RETURN-WITH-TRANSID.                         
      * PARAGRAPH WILL END THIS PROGRAM WITH OPTION TO RETRRIGER        
      * IF USER WILL PRESS ANY ATTENTION KEY                            
      ******************************************************************
       3001-RETURN-WITH-TRANSID.                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           DISPLAY 'RETURN WITH 0217'                                   
           EXEC CICS                                                    
            RETURN TRANSID('0217') COMMAREA(DFHCOMMAREA)                
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  3002-RETURN-TO-FIRST-PROG                      
      * PROGRAM WILL RETURN CONTROL TO FIRST PROGRAM                    
      * Z02131  (TO CALL THAT PROGRAM CORRECTLY WE NEED TO PROVIDE      
      * LENGTH(0) IN XCTL STATEMENTS                                    
      ******************************************************************
       3002-RETURN-TO-FIRST-PROG.                                       
           SET SO-M-FIRST-WITH   TO TRUE                                
           EXEC CICS                                                    
             XCTL PROGRAM(CT-FIRST-PROG-NAME)                           
             COMMAREA(DFHCOMMAREA) LENGTH(0)                            
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .  
                              
                                                
                                                         
                                                                        
