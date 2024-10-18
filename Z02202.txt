       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02202.                                              
      ******************************************************************
      *                Z02202  (0215)                                   
      *                                                                 
      * PROGRAM IS PERFORMED BY Z02192 PROGRAM IN ORDER TO ALLOW        
      * USER TO PROVIDE PASSENGERS DATA,                                
      *                                                                 
      * USER WILL HAVE TO PROVIDE DATA FOR ALL  PASSENGERS              
      *                                                                 
      * IF USER WILL MAKE MISTAKE WHILE PROVIDING DATA FOR PASSENGER    
      * HE WILL HAVE TO PROVIDE THAT DATA AGIAN                         
      *                                                                 
      * IF ALL DATA WILL FOR ALL PASSENGER WILL BE VALID PROGRAM        
      * Z02212 WILL BE CALLED                                           
      *                                                                 
      * WHILE PROVIDING DATA FOR COUNTRY PROGRAM CAN CALL TO            
      * Z02162 PROGRAM THAT WILL DISPLAY SIMILAR NAMES TO WHAT USER JUST
      * PROVIDED ( THEN USER CAN CHOOSE THIS COUNTRY NAME AND CONTROL   
      * WILL GO BACK TO THIS PROGRAM)                                   
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY ZZEC0215.                                               
           COPY DFHAID.                                                 
           COPY ZZMP0220.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T06TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL INCLUDE T03TAB END-EXEC.                            
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
       01 CT-CONSTANTS.                                                 
           05 CT-CALLING-PROGRAM-NAME PIC X(8) VALUE 'Z02192  '.        
           05 CT-THIS-PROGRAM-NAME    PIC X(8) VALUE 'Z02202  '.        
           05 CT-ERROR-ROUTINE-NAME   PIC X(8) VALUE 'Z02141  '.        
           05 CT-SIMILAR-NAMES-PROG   PIC X(8) VALUE 'Z02162  '.        
           05 CT-RESERVATION-PROGRAM  PIC X(8) VALUE 'Z02212  '.        
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME              PIC X.         
               88  SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'. 
               88  SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'. 
               88  SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'.
           05 SW-WHAT-TYPE-OF-END                            PIC X.     
               88 SO-FINAL-WITH-COMMAREA                     VALUE '1'. 
               88 SO-FINAL-TERMINATION                       VALUE '2'. 
           05 SW-IF-INVALID-NAME                             PIC X.     
               88 SO-NOT-INVALID-NAME                        VALUE '1'. 
               88 SO-INVALID-NAME                            VALUE '2'. 
           05 SW-WHERE-TO-GO                                 PIC X.     
               88 SO-GO-BACK-TO-THIS                         VALUE '1'. 
               88 SO-GO-BACK-TO-Z02152                       VALUE '2'. 
           05 SW-WHAT-DOCUMENT-TYPE                          PIC X.     
               88 SO-PASSPORT                                VALUE '2'. 
               88 SO-NATIONAL-ID                             VALUE '1'. 
           05 SW-IF-COUNTRY-IN-SCHENGEN                      PIC X.     
               88 SO-NOT-IN-SCHENGEN                         VALUE '2'. 
               88 SO-IN-SCHENGEN                             VALUE '1'. 
           05 SW-IF-FLIGHTS-IN-ONE-COUNTRY                   PIC X.     
               88 SO-IN-ONE-COUNTRY                          VALUE '2'. 
               88 SO-NOT-IN-ONE-COUNTRY                      VALUE '1'. 
           05 SW-IF-RECORD-FOUND                             PIC X.     
               88 SO-PASSENGER-FOUND                         VALUE '2'. 
               88 SO-PASSENGER-NOT-FOUND                     VALUE '1'. 
           05 SW-IF-PASSENGER-EXISTS                         PIC X.     
               88 SO-PASSANGER-IN-BASE                       VALUE '1'. 
               88 SO-NEW-PASSENGER                           VALUE '2'. 
           05 SW-IF-COUNTRY-IATA-VALID                       PIC X.     
               88 SO-COUNTRY-IATA-VALID                      VALUE '1'. 
               88 SO-COUNTRY-IATA-INVALID                    VALUE '2'. 
           05 SW-IF-LAST-NAME-VALID                          PIC X.     
               88 SO-LAST-NAME-VALID                         VALUE '1'. 
               88 SO-LAST-NAME-INVALID                       VALUE '2'. 
           05 SW-IF-FIRST-NAME-VALID                         PIC X.     
               88 SO-FIRST-NAME-INVALID                      VALUE '1'. 
               88 SO-FIRST-NAME-VALID                        VALUE '2'. 
           05 SW-IF-COUNTRY-WAS-FOUND                        PIC X.     
               88 SO-COUNTRY-FOUND                           VALUE '1'. 
               88 SO-COUNTRY-NOT-FOUND                       VALUE '2'. 
           05 SW-IF-PASSENGER-CAN-FLY                        PIC X.     
               88 SO-PASSENGER-CAN-FLY                       VALUE 'Y'. 
           05 SW-IF-LETTER-VALID                 PIC X.                 
              88 SO-LETTER-VALID                 VALUE ' ' '-' '!' '?'. 
       01 WS-VARIABLES.                                                 
           05 WS-ITER                            PIC S9(4) COMP VALUE 0.
           05 WS-ITER1                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER2                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER3                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER4                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER5                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER6                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER7                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER8                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER9                           PIC S9(4) COMP VALUE 0.
           05 WS-ITER10                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER11                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER12                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER13                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER14                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER15                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER16                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER17                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER18                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER19                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER20                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER21                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER22                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER23                          PIC S9(4) COMP VALUE 0.
           05 WS-ITER24                          PIC S9(4) COMP VALUE 0.
           05 WS-NAME.                                                  
               10 WS-FIRST-NAME-FIRST-LETTER     PIC X(1)  VALUE SPACE. 
               10 WS-FIRST-NAME-REST             PIC X(49) VALUE SPACE. 
           05 WS-LAST-NAME.                                             
               10 WS-LAST-NAME-FIRST-LETTER      PIC X(1)  VALUE SPACE. 
               10 WS-LAST-NAME-REST              PIC X(49) VALUE SPACE. 
           05 WS-NAME-CHECK                      PIC X(50).            
                                                                       
           05 WS-TYPE-OF-DOCUMENT                PIC X     VALUE SPACE.
           05 WS-NATIONALITY                     PIC X(50) VALUE SPACE.
           05 WS-TEMP-AIRPORT-CODE1              PIC X(3)  VALUE SPACE.
           05 WS-TEMP-AIRPORT-CODE2              PIC X(3)  VALUE SPACE.
           05 WS-COUNTRY-CODE                    PIC X(3)  VALUE SPACE.
           05 WS-ID-NUMBER                       PIC X(12) VALUE SPACE.
           05 WS-SEAT-COUNTER-FORMAT             PIC 99   VALUE 0.     
           05 WS-TEMP-ID-NUMBER                  PIC X(10) VALUE SPACE.
       LINKAGE SECTION.                                                
       01 DFHCOMMAREA PIC X(17294).                                    
       PROCEDURE DIVISION USING DFHCOMMAREA.                           
           PERFORM 1000-INIT                                           
           PERFORM 2000-PROCESS                                        
           PERFORM 3000-FINAL                                          
           .                                                           
      *****************************************************************
      *                         1000-INIT                              
      *****************************************************************
       1000-INIT.                                                      
           PERFORM  1005-CHECK-IF-FIRST-TIME                           
           .                                                           
      *****************************************************************
      *                                                                
      *                 1005-CHECK-IF-FIRST-TIME                       
      *                                                                
      *****************************************************************
       1005-CHECK-IF-FIRST-TIME.                                       
           INITIALIZE WS-ZZEC0215                                      
                                                                       
           MOVE DFHCOMMAREA TO WS-ZZEC0215                             
           EVALUATE TRUE                                               
             WHEN SO-M-FIRST-WITHOUT                                   
               MOVE 1 TO Z02202-SEAT-COUNTER                           
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
      *                       1010-CICS-IGNORE                          
      ******************************************************************
       1010-CICS-IGNORE.                                                
           EXEC CICS                                                    
             IGNORE CONDITION ERROR                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                      1015-SET-START-FLAGS                       
      ******************************************************************
       1015-SET-START-FLAGS.                                            
           SET SO-GO-BACK-TO-THIS TO TRUE                               
           .                                                            
      ******************************************************************
      *                       2000-PROCESS                              
      * THERE ARE 3 MODES THAT THIS PROGRAM CAN RUN IN                  
      *           
      *  1. SO-PROGRAM-RUNS-FIRST-TIME IT MEANS THAT PROGRAM            
      * LITERARY WAS PERFORMED FIRST TIME AND IT DOESN'T HAVE ANY       
      * OLD DATA                                                        
      *  2. SO-PROGRAM-RUNS-WITH-DATA                                   
      *     MEANS THAT USER PROVIDED SOME DATA BEFORE AND NOW           
      *  PROGRAM HAVE TO DISPLAY THOSE DATA                             
      *  3.  SO-PROGRAM-RUNS-NOT-FIRST-TIME IT MEANS                    
      *    TAHT THIS PROGRAM IS RUNNING BECAUSE OF THE FACT THAT        
      *   USER PRESSED ATTENTION KEY                                    
      *                                                                 
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
               PERFORM 2301-SEND-INVALID-CALL-MSG                       
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                     2001-PROCESS-FIRST-TIME                     
      * PROGRAM WILL DISPLAY WHAT IS THE NUMBER OF THIS PASSENGER       
      * IN SEQUENCE  ( IT WILL JUST DISPLAY '1' ) BECAUSE THIS IS       
      *  THE FIRST TIME PROGRAM RUNS                                    
                                                                        
      ****************************************************************  
       2001-PROCESS-FIRST-TIME.                                         
           MOVE LOW-VALUES TO MP0220O                                   
           MOVE Z02202-SEAT-COUNTER TO WS-SEAT-COUNTER-FORMAT
           MOVE WS-SEAT-COUNTER-FORMAT TO PAS-NO                      
           PERFORM 2100-SEND-THE-MAP                                  
           .                                                          
      ****************************************************************
      *                     2002-PROCESS-WITH-DATA                    
      ****************************************************************
       2002-PROCESS-WITH-DATA.                                        
      * IF THAT 'IF' ISN'T TRUE THAT MEANS THAT USER PROVIDED WRONG   
      * COUNTRY NAME AND PROGRAM CALLED TO Z02162 ,                   
      * USER CHOOSE SOME COUNTRY NAME AND RETURNED TO THIS PROGRAM    
      *                                                               
      * IF THAT IS THE CASE WE HAVE TO MOVE 'WS-Z02152-I-COUNTRY-IATA'
      * VARIABLE THAT GET FROM Z02162 PROGRAM AND SENT IT TO THE USER 
      *                                                               
      * IN CASE THAT THIS VALUE THAT WE GET FROM Z02162 PROGRAM       
      * IS INVALID THAT MEANS THAT USER MADE A MISTAKE                
      *                                                               
      * FOR EXAMPLE HE DIDN'T CHOOSE ANY NAME AND JUST PRESSED F3     
      * IF THAT IS THE CASE HE WILL GET PROPER ERRROR MESSAGE         
           DISPLAY '2002 PROCESS WITH DATA  '                         
           DISPLAY '2002 WS-Z02152-I-COUNTRY-IATA: '                  
                   WS-Z02152-I-COUNTRY-IATA                           
           IF WS-Z02152-I-COUNTRY-IATA = SPACE OR LOW-VALUES THEN     
               CONTINUE                                               
           ELSE                                                       
              DISPLAY 'WS-Z02152-I-COUNTRY-IATA NOT EMPTY '           
              MOVE LOW-VALUES TO MP0220O                              
              MOVE WS-Z02152-I-COUNTRY-IATA TO T03-COUNTRY-CODE       
              PERFORM 7012-VALIDATE-COUNTRY-IATA                      
              MOVE T03-COUNTRY-NAME-TEXT TO NATIONO                   
           END-IF                                                     
      *                                                               
      * THOSE MOVE STATMENTS MOVES DATA THAT WERE PREVIOSULY SAVED    
      * TO THE SCREEN                                                 
      *                                                               
           MOVE Z02202-SEAT-COUNTER TO WS-SEAT-COUNTER-FORMAT         
           MOVE WS-SEAT-COUNTER-FORMAT TO PAS-NO                        
           DISPLAY '2002 PAS-NO ' PAS-NO                                
           MOVE Z02202-PASS-NAME      TO NAMEO                          
           MOVE Z02202-PASS-LAST-NAME TO LNAMEO                         
           MOVE Z02202-DOCUMENT-TYPE  TO TYPEO                          
           MOVE Z02202-ID-NUMBER      TO WS-TEMP-ID-NUMBER              
           MOVE WS-TEMP-ID-NUMBER     TO PESELI                         
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                    2003-PROCESS-NOT-FIRST-TIME                  
      * THIS OPTION WILL PROCESS USER INPUT                             
      * (WILL CHECK IF EVERYTHING IS VALID AND BASED ON THAT            
      * PASSENGER CAN BE SAVED TO THE DATABASE OR ERROR                 
      * WILL BE DISPLAYED )                                             
      *                                                                 
      * PARAGRAPH (2111) IS USED TO CHECK IF PROGRAM SHOULD             
      * DISPLAY PLACE FOR THE NEXT PASSENGER ( IF USER WANTS TO         
      * RESERVATE 10 PLACES THEN PROGRAM SHOULD DIPSLAY EMPTY MAP 10    
      * TIMES.                                                          
      *                                                                 
      * IN CASE THAT USER ENTERED ALL DATA ( AND ALL DATA ARE VALID)    
      * PROGRAM WILL CALL FOR Z02211 THAT WILL DISPLAY RESERVATION      
      * DETAILS                                                         
      ****************************************************************  
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHENTER                                                
               PERFORM 2102-PROCESS-INPUT-DATA                          
               PERFORM 2112-SAVE-PASSENGER-DATA                         
               PERFORM 2111-DISPLAY-NEXT                                
           WHEN DFHPF3                                                  
               SET SO-FINAL-TERMINATION TO TRUE                         
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'NO-ACTION KEY          ' TO        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                    2100-SEND-THE-MAP                            
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0220') MAPSET('MP0220')                        
             FROM(MP0220O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                   2102-PROCESS-INPUT-DATA                       
      ****************************************************************  
       2102-PROCESS-INPUT-DATA.                                         
           PERFORM 2103-RECEIVE-USER-INPUT                              
           PERFORM 2104-CHECK-IF-INPUT-EMPTY                            
           PERFORM 2105-CHECK-IF-INPUT-VALID                            
           .                                                            
      ************************************************************      
      *                  2103-RECEIVE-USER-INPUT                        
      ****************************************************************  
       2103-RECEIVE-USER-INPUT.                                         
           MOVE LOW-VALUES TO MP0220I                                   
           EXEC CICS                                                    
             RECEIVE MAP('MP0220') MAPSET('MP0220')                     
             INTO(MP0220I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL) 
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PLEASE PROVIDE DATA !' TO                           
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
              PERFORM 2200-CHECK-EIBRESP                                
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   2104-CHECK-IF-INPUT-EMPTY                     
      * PARAGRAPH WILL DISPLAY ERROR MESSAGE IF NOT ALL DATA WAS        
      * PROVIDED BY THE USER                                            
      *                                                                 
      * IF DATA WAS PROVIDED PROGRAM WILL CHANGE ITS '_' SYMBOL TO      
      * SPACE, THIS IS BECAUSE ON THE SCREEN THOSE FIELDS ARE FILLED    
      * WITH UNDERSCORES AND WE NEED TO GET RID OF THEM                 
      ****************************************************************  
       2104-CHECK-IF-INPUT-EMPTY.                                       
           PERFORM 2302-IF-USER-NAME-EMPTY                              
           PERFORM 2303-IF-USER-LAST-NAME-EMPTY                         
           PERFORM 2304-IF-DOCUMENT-TYPE-EMPTY                          
           PERFORM 2305-IF-USER-ID-NUMBER-EMPTY                         
           PERFORM 2306-IF-USER-NATIONALITY-EMPTY                       
           .                                                            
      ****************************************************************  
      *                  2105-CHECK-IF-INPUT-VALID                      
      * PARAGRAPH WILL CHECK IF DATA PROVIDED BY THE USER IS CORRECT    
      *                                                                 
      * 1. CHECK IF ID-NUMBER IS IN A TABLE OR NOT                      
      *   IF NOT  WE WILL ADD THIS PASSENGER TO A TABLE                 
      *                                                                 
      *                                                                 
      *   IF PASSENGER IS IN THE TABLE WE WILL CHECK IF HE IS           
      *    ALLOWED TO FLY (IF ISNT' BANNED BY AIRLINE)                  
      *                                                                 
      * 2 IN CASE THAT THIS IS NEW PASSENGER WE NEED TO CHECK           
      * IF NATIONALITY HE PROVIDED EXISTS ( IF WE HAVE THIS VALUE IN    
      * DATABASE)                                                       
      *                                                                 
      * 3. PARAGRAPH WILL CHECK IF USER CAN FLY BY HIS NATIONAL ID      
      *  OR HE HAVE TO HAVE A PASSPORT                                  
      *                                                                 
      ****************************************************************  
       2105-CHECK-IF-INPUT-VALID.                                       
           MOVE WS-ID-NUMBER TO  IDENTIFICATION-NUMBER                  
           COMPUTE  IDENTIFICATION-NUMBER = FUNCTION                    
           NUMVAL(WS-ID-NUMBER)                                         
           PERFORM 7001-IF-PASSENGER-IN-BASE                            
           EVALUATE TRUE                                                
           WHEN  SO-PASSENGER-FOUND                                     
      * PASSENGER IS ALREADY IN THE DATABASE                            
      * HE WAS FLIGHING BEFORE                                          
              DISPLAY '2105 PASSENGER FOUND '                           
                                                                        
              SET SO-PASSANGER-IN-BASE TO TRUE                          
              PERFORM 2106-VALIDATE-OLD-PASSENGER                       
           WHEN  SO-PASSENGER-NOT-FOUND                                 
      * PASSENGER IS NEW ( WE DON'T HAVE INFORMATIONS ABOUT HIM IN A    
      * DATABASE )                                                      
              DISPLAY '2105 NEW PASSENGER   '                           
              SET SO-NEW-PASSENGER     TO TRUE                          
              PERFORM 2110-VALIDATE-NEW-PASSENGER                       
           WHEN OTHER                                                   
      * THIS ERROR SHOULDN'T HAPPEN                                     
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'OTHER ERROR IN 2105 (Z02202) ' TO                   
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE   
           END-EVALUATE                                               
           .                                                          
      ****************************************************************
      *                    2106-VALIDATE-OLD-PASSENGER                
      * OLD PASSENGER = PASSENGER WE ALREADY HAVE IN THE BASE         
      ****************************************************************
       2106-VALIDATE-OLD-PASSENGER.                                   
           DISPLAY '2106 PERFORMED  '                                 
           DISPLAY 'IF CAN FLY: ' IF-PASSENGER-CAN-FLY                
           MOVE IF-PASSENGER-CAN-FLY TO SW-IF-PASSENGER-CAN-FLY       
                                                                      
           IF SO-PASSENGER-CAN-FLY  THEN                              
              CONTINUE                                                
           ELSE                                                       
              PERFORM 2307-SEND-PASSENGER-BAN-MSG                     
           END-IF                                                     
           PERFORM 2355-CHECK-IF-VALID-DOCUMENT                       
           .                                                          
      ****************************************************************
      *                       2110-VALIDATE-NEW-PASSENGER             
      * USER PROVIDED DATA OF PASSENGER THAT WASN'T FLYING BEFORE.    
      * WE WILL CHECK IF HE HAS VALID NATIONALITY AND                 
      * IF HE CAN FLY ON THIS FLIGHT ( IF HE HAS VALID DOCUMENT)      
      *                                                               
      * IF ALL PROVIDED DATA IS VALID THEN THIS PASSENGER DATA        
      * WILL BE INSERTED INTO THE DATABASE                            
      ****************************************************************
       2110-VALIDATE-NEW-PASSENGER.                                   
           PERFORM 2310-VALIDATE-FIRST-LAST-NAME                      
           PERFORM 7002-CHECK-IF-COUNTRY-VALID                        
           PERFORM 2355-CHECK-IF-VALID-DOCUMENT                       
           PERFORM 7007-INSERT-PASSENGER-DATA                         
           .                                                          
      ****************************************************************
      *                       2111-DISPLAY-NEXT                       
      * IF USER PROVIDED DATA FOR ALL PASSENGERS THEN PROGRAM WILL    
      * CALL TO NEXT PROGRAM                                           
      * IF NOT THEN PROGRAM WILL DISPLAY ANOTHER EMPTY MAP AND ALLOW   
      * USER TO PROVIDE INFO                                           
      **************************************************************** 
       2111-DISPLAY-NEXT.                                              
           IF Z02202-SEAT-COUNTER  < Z02192-TICKET-NUMBER(1)  THEN     
            ADD 1 TO Z02202-SEAT-COUNTER                               
            MOVE LOW-VALUES TO MP0220O                                 
            MOVE Z02202-SEAT-COUNTER TO WS-SEAT-COUNTER-FORMAT         
            MOVE WS-SEAT-COUNTER-FORMAT TO PAS-NO                      
            DISPLAY '2111 PAS-NO ' PAS-NO                              
            PERFORM 2100-SEND-THE-MAP                                  
           ELSE                                                        
            PERFORM 2610-CALL-TO-NEXT-PROGRAM                          
           END-IF                                                      
           .                                                           
      **************************************************************** 
      *                  2112-SAVE-PASSENGER-DATA                      
      **************************************************************** 
       2112-SAVE-PASSENGER-DATA.                                       
           DISPLAY '2112 SEAT COUNTER: ' Z02202-SEAT-COUNTER           
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL                  
                                WS-ITER3 > Z02192-NUMBER-OF-FLIGHTS    
               MOVE PASSENGER-ID TO                                    
                 Z02192-PASSENGER-ID(WS-ITER3,Z02202-SEAT-COUNTER)     
           END-PERFORM                                                 
           DISPLAY '2112 PASSENGER ID: '  PASSENGER-ID                 
           .                                                           
      **************************************************************** 
      *                  2199-CHECK-IF-NAME-VALID                      
      **************************************************************** 
       2199-CHECK-IF-NAME-VALID.                                       
           SET SO-NOT-INVALID-NAME TO TRUE                             
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER >         
                       LENGTH OF WS-NAME-CHECK OR SO-INVALID-NAME      
              IF WS-NAME-CHECK(WS-ITER:1) IS ALPHABETIC THEN     
               CONTINUE                                               
              ELSE                                                    
                 MOVE WS-NAME-CHECK(WS-ITER:1) TO  SW-IF-LETTER-VALID 
                                                                      
                 IF SO-LETTER-VALID                                   
                 THEN                                                 
                    CONTINUE                                          
                 ELSE                                                 
                    SET SO-INVALID-NAME TO TRUE                       
                    PERFORM 2400-INITIALIZE-ERROR-MESSAGE             
                    MOVE 'INVALID FIRST OR LAST NAME '                
                             TO  WS-Z02141-I-ERROR-MESSAGE(1)         
                    SET SO-Z02141-M-WITH TO TRUE                      
                    PERFORM 2300-CALL-ERROR-ROUTINE                   
                 END-IF                                               
              END-IF                                                  
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
                                                                        
           IF SO-GO-BACK-TO-Z02152 THEN                                 
             MOVE CT-CALLING-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM
             SET SO-GO-BACK-TO-THIS TO TRUE                             
           ELSE                                                         
             MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM   
           END-IF                                                       
           SET SO-Z02141-M-WITH TO TRUE                                 
           SET  SO-Z02141-I-FIRST-TIME TO TRUE                          
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   2301-SEND-INVALID-CALL-MSG                    
      ******************************************************************
       2301-SEND-INVALID-CALL-MSG.                                      
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'INVALID CALL  IN Z02182' TO                            
                               WS-Z02141-I-ERROR-MESSAGE(1)             
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                  2302-IF-USER-NAME-EMPTY                        
      ******************************************************************
       2302-IF-USER-NAME-EMPTY.                                         
           IF NAMEI = SPACE OR LOW-VALUES THEN                          
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE PASSENGER NAME ' TO                         
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE NAMEI TO WS-NAME                                     
              INSPECT WS-NAME REPLACING ALL '_' BY ' '                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2303-IF-USER-LAST-NAME-EMPTY                  
      ******************************************************************
       2303-IF-USER-LAST-NAME-EMPTY.                                    
           IF LNAMEI = SPACE OR LOW-VALUES THEN                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE PASSENGER LAST NAME ' TO                    
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE LNAMEI TO WS-LAST-NAME                               
              INSPECT WS-LAST-NAME REPLACING ALL '_' BY ' '             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2304-IF-DOCUMENT-TYPE-EMPTY                   
      ******************************************************************
       2304-IF-DOCUMENT-TYPE-EMPTY.                                     
           IF TYPEI = SPACE OR LOW-VALUES THEN                          
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE TYPE OF DOCUMENT ' TO                       
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE TYPEI TO WS-TYPE-OF-DOCUMENT                         
              INSPECT WS-TYPE-OF-DOCUMENT REPLACING ALL '_' BY ' '      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2305-IF-USER-ID-NUMBER-EMPTY                  
      ******************************************************************
       2305-IF-USER-ID-NUMBER-EMPTY.                                    
           IF PESELI = SPACE OR LOW-VALUES THEN                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE IDENTIFICATION NUMBER ' TO                  
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
                                                                        
              MOVE PESELI TO WS-ID-NUMBER                               
              INSPECT WS-ID-NUMBER REPLACING ALL '_' BY ' '             
              IF FUNCTION TEST-NUMVAL(WS-ID-NUMBER) = 0 THEN            
               CONTINUE                                                 
              ELSE                                                      
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID ID NUMBER  ' TO                            
                                  WS-Z02141-I-ERROR-MESSAGE(1)    
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2306-IF-USER-NATIONALITY-EMPTY                
      ******************************************************************
       2306-IF-USER-NATIONALITY-EMPTY.                                  
           IF NATIONI = SPACE OR LOW-VALUES THEN                        
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PROVIDE NATIONALITY  ' TO                           
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           ELSE                                                         
              MOVE NATIONI TO WS-NATIONALITY                            
              INSPECT WS-NATIONALITY REPLACING ALL '_' BY ' '           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2307-SEND-PASSENGER-BAN-MSG                   
      ******************************************************************
       2307-SEND-PASSENGER-BAN-MSG.                                     
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'PASSENGER IS BANNED FROM FLYING  ' TO                  
                               WS-Z02141-I-ERROR-MESSAGE(1)             
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2308-INITIALIZE-THE-MAP                       
      ******************************************************************
       2308-INITIALIZE-THE-MAP.                                         
           MOVE LOW-VALUES TO  Z02202-PASS-NAME                         
           MOVE LOW-VALUES TO  Z02202-PASS-LAST-NAME 
           MOVE LOW-VALUES TO  Z02202-DOCUMENT-TYPE                     
           MOVE 0          TO  Z02202-ID-NUMBER                         
           MOVE LOW-VALUES TO WS-Z02152-I-COUNTRY-IATA                  
           .                                                            
      ******************************************************************
      *                   2309-IF-CAN-FLY-W-NATIONAL-ID                 
      ******************************************************************
       2309-IF-CAN-FLY-W-NATIONAL-ID.                                   
           IF SO-NEW-PASSENGER THEN                                     
             MOVE WS-COUNTRY-CODE  TO NATIONALITY                       
           END-IF                                                       
           PERFORM 7008-IF-PASSENGER-IN-SCHENGEN                        
                                                                        
           EVALUATE TRUE                                                
            WHEN SO-IN-SCHENGEN                                         
      * IF USER NATIONALITY IS IN SHCENGEN ZONE WE NEED TO CHECK        
      * IF ALL FLIGHTS ARE IN SCHENGEN ZONE                             
               PERFORM 7003-IF-FLIGHT-IN-SCHENGEN                       
            WHEN SO-NOT-IN-SCHENGEN                                     
               PERFORM 7005-IF-FLIGHTS-IN-COUNTRY                       
            WHEN OTHER                                                  
      * THIS SHOULDN'T HAPPEN                                           
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID NATIONALITY '                              
                           TO  WS-Z02141-I-ERROR-MESSAGE(1)             
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2310-VALIDATE-FIRST-LAST-NAME                  
      ******************************************************************
       2310-VALIDATE-FIRST-LAST-NAME.                                   
           MOVE WS-NAME TO WS-NAME-CHECK                                
           PERFORM 2199-CHECK-IF-NAME-VALID      
           MOVE WS-LAST-NAME TO WS-NAME-CHECK                          
           PERFORM 2199-CHECK-IF-NAME-VALID                            
           .                                                           
      **************************************************************** 
      *                     2355-CHECK-IF-VALID-DOCUMENT               
      **************************************************************** 
       2355-CHECK-IF-VALID-DOCUMENT.                                   
           MOVE WS-TYPE-OF-DOCUMENT  TO SW-WHAT-DOCUMENT-TYPE          
           EVALUATE TRUE                                               
           WHEN SO-PASSPORT                                            
             CONTINUE                                                  
           WHEN SO-NATIONAL-ID                                         
              PERFORM 2309-IF-CAN-FLY-W-NATIONAL-ID                    
           WHEN OTHER                                                  
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
             MOVE 'PLEASE PROVIDE VALID DOCUMENT TYPE '                
                           TO  WS-Z02141-I-ERROR-MESSAGE(1)            
             SET SO-Z02141-M-WITH TO TRUE                              
             PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                
           .                                                           
      *****************************************************************
      *                   2400-INITIALIZE-ERROR-MESSAGE                
      *****************************************************************
       2400-INITIALIZE-ERROR-MESSAGE.                                  
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 10    
             MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER2)         
           END-PERFORM                                                 
           .                                                           
      *****************************************************************
      *                    6610-CALL-TO-NEXT-PROGRAM                   
      *****************************************************************
       2610-CALL-TO-NEXT-PROGRAM.                                      
           SET SO-M-FIRST-WITHOUT TO TRUE                              
           SET SO-DISPLAY-RESERVATION TO TRUE      
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-RESERVATION-PROGRAM) COMMAREA(DFHCOMMAREA)  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ***************************************************************** 
      *                   2640-CALL-TO-CHECK-NATIONALITY                
      ***************************************************************** 
       2640-CALL-TO-CHECK-NATIONALITY.                                  
           MOVE WS-NATIONALITY TO WS-SEARCHED-PHRASE-COUNTRY            
           SET SO-M-FIRST-WITHOUT TO TRUE                               
           SET SO-CHECK-COUNTRY TO TRUE                                 
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
             XCTL PROGRAM(CT-SIMILAR-NAMES-PROG) COMMAREA(DFHCOMMAREA)  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ***************************************************************** 
      *                          3000-FINAL                             
      ***************************************************************** 
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              MOVE WS-ZZEC0215 TO DFHCOMMAREA                           
              DISPLAY 'RETURN WITH 0215'                                
              EXEC CICS                                                 
               RETURN TRANSID('0215') COMMAREA(DFHCOMMAREA)             
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN SO-FINAL-TERMINATION                                    
              SET SO-M-FIRST-WITH   TO TRUE                             
              EXEC CICS                                                 
                XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                   
                 COMMAREA(DFHCOMMAREA)            
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO   WS-Z02141-I-ERROR-MESSAGE(1)   
              SET     SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       7001-IF-PASSENGER-IN-BASE                 
      ******************************************************************
       7001-IF-PASSENGER-IN-BASE.                                       
           DISPLAY '7001: IDENTIFICATION-NUMBER ' IDENTIFICATION-NUMBER 
           INITIALIZE NATIONALITY                                       
           INITIALIZE DOCUMENT-TYPE                                     
           INITIALIZE PASSENGER-ID                                      
           INITIALIZE IF-PASSENGER-CAN-FLY                              
           INITIALIZE PASSENGER-NAME                                    
           INITIALIZE PASSENGER-LAST-NAME                               
           EXEC SQL                                                     
             SELECT NATIONALITY,                                        
                    DOCUMENT_TYPE,                                      
                    PASSENGER_ID,                                       
                    IF_PASSENGER_CAN_FLY,                               
                    PASSENGER_NAME,                                     
                    PASSENGER_LAST_NAME                                 
             INTO                                                       
                    :NATIONALITY,                                       
                    :DOCUMENT-TYPE,                                     
                    :PASSENGER-ID,                                      
                    :IF-PASSENGER-CAN-FLY,                              
                    :PASSENGER-NAME,                                    
                    :PASSENGER-LAST-NAME                                
             FROM                                                       
                    T06_PASSENGERS_TABLE
             WHERE IDENTIFICATION_NUMBER = :IDENTIFICATION-NUMBER       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7001 PASSENGER ID: '  PASSENGER-ID               
              PERFORM 2120-CHECK-PASS-DATA                              
              SET SO-PASSENGER-FOUND  TO TRUE                           
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-PASSENGER-NOT-FOUND TO TRUE                        
           WHEN OTHER                                                   
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                2120-CHECK-PASS-DATA                             
      * PARAGRAPH IS CALLED ONLY WHEN WE SUCCESSFULLY RETRIVED          
      * PASSENGER DATA FROM THE DATABASE ( PASSENGER WAS ALREADY THERE) 
      * NOW WE WILL CHECK IF NAME AND LAST NAME IS THE SAME AS          
      * DATA THAT USER PROVIDED                                         
      ****************************************************************  
       2120-CHECK-PASS-DATA.                                            
           IF PASSENGER-LAST-NAME-TEXT = WS-LAST-NAME AND               
              PASSENGER-NAME-TEXT      = WS-NAME THEN                   
            CONTINUE                                                    
           ELSE                                                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PASSENGER DATA IN TABLE IN DIFFERENT THAT THIS'     
              TO WS-Z02141-I-ERROR-MESSAGE(1)                           
              SET     SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                7002-CHECK-IF-COUNTRY-VALID         
      ****************************************************************  
       7002-CHECK-IF-COUNTRY-VALID.                                     
           DISPLAY '7002 CHECK COUNTRY VALID: '                         
           DISPLAY 'WS-NATIONALITY: ' WS-NATIONALITY                    
           MOVE WS-NATIONALITY TO T03-COUNTRY-NAME-TEXT                 
                                                                        
           COMPUTE T03-COUNTRY-NAME-LEN =                               
            FUNCTION LENGTH(WS-NATIONALITY)                             
           EXEC SQL                                                     
             SELECT                                                     
              COUNTRY_CODE                                              
             INTO                                                       
              :WS-COUNTRY-CODE                                          
             FROM                                                       
              T03_COUNTRY_TABLE                                         
             WHERE                                                      
              COUNTRY_NAME = :T03-COUNTRY-NAME                          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE W 7002: ' WS-SQLCODE-FORMAT                 
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             SET SO-COUNTRY-FOUND TO TRUE                               
           WHEN SO-SQLCODE-NOT-FOUND                                    
      * IT MEANS THAT USER NATIONALITY IS INVALID                       
             SET SO-COUNTRY-NOT-FOUND TO TRUE                           
             MOVE WS-NAME             TO Z02202-PASS-NAME               
             MOVE WS-LAST-NAME        TO Z02202-PASS-LAST-NAME          
             MOVE WS-TYPE-OF-DOCUMENT TO Z02202-DOCUMENT-TYPE           
             MOVE WS-ID-NUMBER        TO Z02202-ID-NUMBER               
              MOVE LOW-VALUES TO MP0220O                                
             PERFORM 2640-CALL-TO-CHECK-NATIONALITY                     
           WHEN OTHER                                                   
             SET SO-7002-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR      
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                7003-IF-FLIGHT-IN-SCHENGEN                       
      * PARAGRAPH WILL CHECK IF ALL FLIGHTS THAT USER CHOOSE            
      * ARE IN SCHENGEN ZONE                                            
      ****************************************************************  
       7003-IF-FLIGHT-IN-SCHENGEN.                                      
           MOVE 1 TO WS-ITER1                                           
           SET SO-IN-SCHENGEN TO TRUE                                   
           DISPLAY 'START 7003 '                                        
           DISPLAY 'Z02192-NUMBER-OF-FLIGHTS ' Z02192-NUMBER-OF-FLIGHTS 
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 >        
                      Z02192-NUMBER-OF-FLIGHTS OR SO-NOT-IN-SCHENGEN    
             MOVE Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1) TO                
                    T05-FLIGHT-ID-TEXT                                  
             COMPUTE  T05-FLIGHT-ID-LEN =                               
                    FUNCTION LENGTH(Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1))
             DISPLAY 'INSIDE 7003 FLIGHT ID: ' T05-FLIGHT-ID            
      * T022 = SECOND T02_AIRPORT TABLE                                 
      * T112 = SECOND T11 TABLE                                         
             EXEC SQL                                                   
             SELECT                                                     
                  T02.AIRPORT_CODE,                                     
                  T022.AIRPORT_CODE                                     
             INTO                                                       
                  :WS-TEMP-AIRPORT-CODE1,                               
                  :WS-TEMP-AIRPORT-CODE2                                
             FROM                                                       
                  T02_AIRPORT_TABLE T02                                 
             INNER JOIN                                                 
                  T05_FLIGHT_TABLE T05 ON                               
                  T05.DEPARTURE_AIRPORT_CODE = T02.AIRPORT_CODE         
             INNER JOIN                                                 
                  T02_AIRPORT_TABLE T022 ON                             
                  T05.ARRIVAL_AIRPORT_CODE = T022.AIRPORT_CODE   
             INNER JOIN                                                 
                  T11_SCHENGEN_COUNTRIES_TABLE T11                      
             ON T022.COUNTRY_CODE = T11.COUNTRY_CODE                    
             INNER JOIN                                                 
                  T11_SCHENGEN_COUNTRIES_TABLE T112                     
             ON T02.COUNTRY_CODE = T112.COUNTRY_CODE                    
             WHERE                                                      
              T05.FLIGHT_ID = :T05-FLIGHT-ID                            
             END-EXEC                                                   
             MOVE SQLCODE TO SW-SQLCODE                                 
             EVALUATE TRUE                                              
             WHEN SO-SQLCODE-NORMAL                                     
               DISPLAY '7003 IN SCHENGEN '                              
               DISPLAY '7003 WS-TEMP1: ' WS-TEMP-AIRPORT-CODE1          
               DISPLAY '7003 WS-TEMP2: 'WS-TEMP-AIRPORT-CODE2           
             WHEN SO-SQLCODE-NOT-FOUND                                  
               DISPLAY '7003 NOT IN SCHENGEN '                          
               DISPLAY 'NOT IN SCHENGEN '                               
               SET SO-NOT-IN-SCHENGEN TO TRUE                           
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'THIS PASSENGER CANT FLY WITH NATIONAL ID'          
                          TO  WS-Z02141-I-ERROR-MESSAGE(1)              
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             WHEN OTHER                                                 
               SET SO-7003-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
             END-EVALUATE                                               
           END-PERFORM                                                  
           DISPLAY 'END OF 7003 '                                       
           .                                                            
      ****************************************************************  
      *            7004-CHECK-IF-PASS-IN-SCHEN                          
      ****************************************************************  
       7004-CHECK-IF-PASS-IN-SCHEN.                                     
           EXEC SQL           
            SELECT                                                      
                   T11.COUNTRY_CODE                                     
            INTO   :WS-COUNTRY-CODE                                     
            FROM T11_SCHENGEN_COUNTRIES_TABLE T11                       
            INNER JOIN                                                  
            T03_COUNTRY_TABLE T03 ON                                    
            T03.COUNTRY_CODE = T11.COUNTRY_CODE                         
            WHERE T03.COUNTRY_NAME = :T03-COUNTRY-NAME                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              SET SO-IN-SCHENGEN     TO TRUE                            
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-NOT-IN-SCHENGEN TO TRUE                            
           WHEN OTHER                                                   
              SET SO-7004-PARA       TO TRUE                            
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                 7005-IF-FLIGHTS-IN-COUNTRY                      
      * PARAGRAPH WILL CHECK IF ALL FLIGHTS ARE IN THE SAME             
      * COUNTRY  ( SAME AS PASSENGER NATIONALITY)                       
      ****************************************************************  
       7005-IF-FLIGHTS-IN-COUNTRY.                                      
           IF SO-PASSANGER-IN-BASE THEN                                 
             PERFORM 7006-GET-USER-COUNTRY-CODE                         
           END-IF                                                       
           MOVE 1 TO WS-ITER1                                           
           SET SO-IN-ONE-COUNTRY TO TRUE                                
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 >        
                      Z02192-NUMBER-OF-FLIGHTS OR SO-NOT-IN-ONE-COUNTRY 
                                                                        
               PERFORM 7015-CHECK-AIPORT-COUNTRIES    
           END-PERFORM                                                 
           .                                                           
      **************************************************************** 
      *               7006-GET-USER-COUNTRY-CODE                       
      **************************************************************** 
       7006-GET-USER-COUNTRY-CODE.                                     
           EXEC SQL                                                    
             SELECT COUNTRY_CODE                                       
             INTO :WS-COUNTRY-CODE                                     
             FROM T03_COUNTRY_TABLE                                    
             WHERE COUNTRY_CODE = :NATIONALITY                         
           END-EXEC                                                    
      * USER NATIONALITY WAS SAVED IN DATABASE SO IT SHOULD BE         
      * VALID                                                          
      * IF IT IS NOT PROGRAM WILL TERMINATE                            
                                                                       
           MOVE SQLCODE TO SW-SQLCODE                                  
           EVALUATE TRUE                                               
           WHEN SO-SQLCODE-NORMAL                                      
              CONTINUE                                                 
           WHEN OTHER                                                  
              SET SO-7006-PARA TO TRUE                                 
              PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                
           .                                                           
      **************************************************************** 
      *               7007-INSERT-PASSENGER-DATA                       
      * THIS PARAGRAPH WILL BE CALLED IF WE DON'T HAVE THIS PASSENGER  
      * IN THE DATABASE ALREADY                                        
      **************************************************************** 
       7007-INSERT-PASSENGER-DATA.                                     
           MOVE WS-NAME TO PASSENGER-NAME-TEXT                         
           COMPUTE PASSENGER-NAME-LEN = FUNCTION LENGTH(WS-NAME)       
           MOVE WS-LAST-NAME TO PASSENGER-LAST-NAME-TEXT               
           COMPUTE PASSENGER-LAST-NAME-LEN =                           
           FUNCTION LENGTH(WS-LAST-NAME)       
           MOVE WS-TYPE-OF-DOCUMENT TO DOCUMENT-TYPE                    
           MOVE 'Y' TO IF-PASSENGER-CAN-FLY                             
           MOVE WS-ID-NUMBER TO IDENTIFICATION-NUMBER                   
           MOVE WS-ID-NUMBER TO PASSENGER-ID                            
           MOVE WS-COUNTRY-CODE TO NATIONALITY                          
           DISPLAY '7007 ALL DATA START '                               
           DISPLAY 'PASS ID '                PASSENGER-ID               
           DISPLAY 'PASS NAME '              PASSENGER-NAME             
           DISPLAY 'PASS LNAME '             PASSENGER-LAST-NAME        
           DISPLAY 'PASS TYPE '              DOCUMENT-TYPE              
           DISPLAY 'NATION    '              NATIONALITY                
           DISPLAY 'IF CAN FLY '             IF-PASSENGER-CAN-FLY       
           DISPLAY 'ID NUM    '              IDENTIFICATION-NUMBER      
           DISPLAY '7007 ALL DATA END '                                 
           EXEC SQL                                                     
           INSERT INTO T06_PASSENGERS_TABLE(                            
                                                                        
                       PASSENGER_NAME,                                  
                       PASSENGER_LAST_NAME,                             
                       DOCUMENT_TYPE,                                   
                       NATIONALITY,                                     
                       IF_PASSENGER_CAN_FLY,                            
                       IDENTIFICATION_NUMBER)                           
                  VALUES(                                               
                                                                        
                       :PASSENGER-NAME,                                 
                       :PASSENGER-LAST-NAME,                            
                       :DOCUMENT-TYPE,                                  
                       :NATIONALITY,                                    
                       :IF-PASSENGER-CAN-FLY,                           
                       :IDENTIFICATION-NUMBER)                          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'Z02202 INSERT SQLCODE : ' WS-SQLCODE-FORMAT         
           EVALUATE TRUE                      
           WHEN SO-SQLCODE-NORMAL                                       
                                                                        
              DISPLAY 'INSERT NORMAL      '                             
           WHEN OTHER                                                   
              DISPLAY 'INSERT ERRROR      '                             
             SET SO-7007-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           PERFORM 7010-DB2-COMMIT                                      
           PERFORM 7011-GET-PASSENGER-ID                                
           .                                                            
      ******************************************************************
      *                   7008-IF-PASSENGER-IN-SCHENGEN                 
      ******************************************************************
       7008-IF-PASSENGER-IN-SCHENGEN.                                   
           EXEC SQL                                                     
            SELECT                                                      
                   T11.COUNTRY_CODE                                     
            INTO   :WS-COUNTRY-CODE                                     
            FROM T11_SCHENGEN_COUNTRIES_TABLE T11                       
            INNER JOIN                                                  
            T03_COUNTRY_TABLE T03 ON                                    
            T03.COUNTRY_CODE = T11.COUNTRY_CODE                         
            WHERE T03.COUNTRY_CODE = :NATIONALITY                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               DISPLAY '7008 IN SCHENGEN '                              
               SET SO-IN-SCHENGEN TO TRUE                               
           WHEN SO-SQLCODE-NOT-FOUND                                    
               DISPLAY '7008 NOT IN SCHENGEN '                          
               SET SO-NOT-IN-SCHENGEN TO TRUE                           
           WHEN OTHER                                                   
               SET SO-7008-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7010-DB2-COMMIT                            
      ******************************************************************
       7010-DB2-COMMIT.                                                 
           EXEC CICS                                                    
             SYNCPOINT                                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                      2011-GET-PASSENGER-ID                      
      ******************************************************************
       7011-GET-PASSENGER-ID.                                           
           EXEC SQL                                                     
             SELECT PASSENGER_ID                                        
             INTO :PASSENGER-ID                                         
             FROM T06_PASSENGERS_TABLE                                  
             ORDER BY PASSENGER_ID DESC                                 
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN OTHER                                                   
              SET SO-7011-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *              7012-VALIDATE-COUNTRY-IATA                         
      ****************************************************************  
       7012-VALIDATE-COUNTRY-IATA.                                      
           DISPLAY '7012 PERFORMED : '      
           INITIALIZE T03-COUNTRY-NAME                                  
           EXEC SQL                                                     
             SELECT COUNTRY_NAME                                        
             INTO :T03-COUNTRY-NAME                                     
             FROM T03_COUNTRY_TABLE                                     
             WHERE COUNTRY_CODE = :T03-COUNTRY-CODE                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               DISPLAY 'VALID COUNTRY IATA'                             
           WHEN SO-SQLCODE-NOT-FOUND                                    
               DISPLAY 'INVALID COUNTRY IATA'                           
               INITIALIZE WS-Z02152-I-COUNTRY-IATA                      
               DISPLAY  'WE GOT INVALID COUNTY NAME '                   
               DISPLAY 'T03-COUNTRY-NAME: ' T03-COUNTRY-NAME-TEXT       
               DISPLAY 'T03-COUNTRY-CODE: ' T03-COUNTRY-CODE            
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID COUNTRY NAME!' TO                          
                                  WS-Z02141-I-ERROR-MESSAGE(1)          
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           WHEN OTHER                                                   
               SET SO-7012-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  7015-CHECK-AIPORT-COUNTRIES                    
      * PARAGRAPH WILL RETURN THE RECORD IF AIRPORTS ARE IN THE SAME    
      * COUNTRY , PARAGRAPH WILL BE PERFORMED FOR ALL FLIGHTS           
      * SO IF THIS QUEUE WONT RETURN A ROW IT MEANS THAT                
      * THIS FLIGHT IS IN A DIFFERENT COUNTRY                           
      ******************************************************************
       7015-CHECK-AIPORT-COUNTRIES.                                     
           MOVE Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1) TO                  
                  T05-FLIGHT-ID-TEXT                                  
           COMPUTE  T05-FLIGHT-ID-LEN =                               
                  FUNCTION LENGTH(Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1))
                                                                      
           EXEC SQL                                                   
                SELECT                                                
                 T02.COUNTRY_CODE,                                    
                 T022.COUNTRY_CODE                                    
                INTO                                                  
                    :WS-TEMP-AIRPORT-CODE1,                           
                    :WS-TEMP-AIRPORT-CODE2                            
                FROM T02_AIRPORT_TABLE T02                            
                INNER JOIN                                            
                  T05_FLIGHT_TABLE T05 ON                             
                  T05.DEPARTURE_AIRPORT_CODE = T02.AIRPORT_CODE       
                INNER JOIN                                            
                  T02_AIRPORT_TABLE T022 ON                           
      * T022  = SECOND T02_AIRPORT_TABLE                              
                                                                      
                  T05.ARRIVAL_AIRPORT_CODE = T022.AIRPORT_CODE        
                INNER JOIN                                            
                  T03_COUNTRY_TABLE T03 ON                            
                   T03.COUNTRY_CODE = T02.COUNTRY_CODE AND            
                   T03.COUNTRY_CODE = T022.COUNTRY_CODE               
                WHERE                                                 
                   T05.FLIGHT_ID = :T05-FLIGHT-ID    AND              
                   T03.COUNTRY_CODE = :WS-COUNTRY-CODE                
           END-EXEC                                                   
           MOVE SQLCODE TO SW-SQLCODE                                 
           EVALUATE TRUE                                              
            WHEN SO-SQLCODE-NORMAL                                    
                DISPLAY '7015 NORMAL '                                
                DISPLAY '7015 TEMP1: ' WS-TEMP-AIRPORT-CODE1          
                DISPLAY '7015 TEMP2: ' WS-TEMP-AIRPORT-CODE2          
                                                                      
            WHEN SO-SQLCODE-NOT-FOUND                 
                DISPLAY '7015 NOT FOUND '                               
               SET SO-NOT-IN-ONE-COUNTRY TO TRUE                        
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'THIS PASSENGER CANT FLY WITH NATIONAL ID'         
                           TO  WS-Z02141-I-ERROR-MESSAGE(1)             
                SET SO-Z02141-M-WITH TO TRUE                            
                PERFORM 2300-CALL-ERROR-ROUTINE                         
            WHEN OTHER                                                  
                SET SO-7005-PARA TO TRUE                                
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
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                                                                       
                
                            
                                   
                          
                        
                  
                                                                        
                                          
       
                               
            
                                
                      
                    
                       
                                                                        
                   
      

                    
      
                        

                                        
                
           
                                                      
                            

 
                             
