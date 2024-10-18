       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02182.                                              
      ******************************************************************
      *                       Z02182  (0212)                            
      *                                                                 
      * PROGRAM IS CALLED IN ORDER TO SEE DETAILS ABOUT CHOSEN          
      * FLIGHTS                                                         
      *                                                                 
      * USER CAN BROWSE RESULT SET BY PRESSING 'F7' AND 'F8' KEYS       
      *                                                                 
      * BY PRESSING 'F3' CONTROL WILL BE RETURNED TO CALLING PROGRAM    
      *  (Z02172)                                                       
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
      *                        CHANGE LOG                               
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0218.                                               
           COPY ZZEC0215.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T13TAB END-EXEC.                            
           EXEC SQL INCLUDE T08TAB END-EXEC.                            
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T01TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL                               
           DECLARE C-NAME CURSOR FOR                                    
           SELECT * FROM T05_FLIGHT_TABLE                               
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
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
           05 CT-ERROR-ROUTINE-NAME           PIC X(8) VALUE 'Z02141  '.
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02182  '.
           05 CT-CALLING-PROGRAM-NAME         PIC X(8) VALUE 'Z02172  '.
           05 CT-DETAILS-QUEUE                PIC X(8) VALUE '02X6    '.
       01 SW-SWITHCES.          
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME              PIC X.         
               88  SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'. 
               88  SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'. 
               88  SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'. 
           05 SW-WHAT-TYPE-OF-END                            PIC X.     
               88 SO-FINAL-WITH-COMMAREA                     VALUE '1'. 
               88 SO-FINAL-TERMINATION                       VALUE '2'. 
           05 SW-WHERE-TO-GO                                 PIC X.     
               88 SO-GO-BACK-TO-THIS                         VALUE '1'. 
               88 SO-GO-BACK-TO-Z02152                       VALUE '2'. 
           05 IF-END-OF-QUEUE-1                              PIC X.     
               88 SO-END-OF-QUEUE-1                          VALUE 'Y'. 
               88 SO-NOT-END-OF-QUEUE-1                      VALUE 'N'. 
       01 WS-VARIABLES.                                                 
           05 WS-TEMP-TIMEZONE.                                         
              10 WS-TIMEZONE-HOUR-AND-SIGN.                             
               15 WS-TIMEZONE-SIGN                    PIC X.            
               15 WS-TIMEZONE-HOUR                    PIC X(2).         
              10 WS-TIMEZONE-FILLER                  PIC X.             
              10 WS-TIMEZONE-MINUTE                  PIC X(2).          
           05 WS-ITER                        PIC S9(4) COMP VALUE 0.    
           05 WS-ITER2                       PIC S9(4) COMP VALUE 0.    
           05 WS-ITER3                       PIC S9(4) COMP VALUE 0.    
           05 WS-ITER4                       PIC S9(4) COMP VALUE 0.    
           05 WS-SUBFLIGHT-COUNT             PIC S9(4) COMP VALUE 0.    
           05 WS-ITER6                       PIC S9(4) COMP VALUE 0.    
           05 WS-ITER7                       PIC S9(4) COMP VALUE 0.    
           05 WS-ITER8                       PIC S9(4) COMP VALUE 0.    
           05 WS-ITER9                       PIC S9(4) COMP VALUE 0.    
           05 WS-AMOUNT-OF-MIDDLE-FLIGHTS    PIC S9(4) COMP VALUE 0.    
           05 WS-TEMP-STRING                 PIC X(15) VALUE SPACE.     
           05 WS-HOUR-OFFSET               PIC S9(4) COMP VALUE 0.      
           05 WS-MINUTE-OFFSET             PIC S9(4) COMP VALUE 0.      
           05 WS-HOUR-OFFSET-TEMP          PIC X(10) VALUE SPACE.       
           05 WS-MINUTE-OFFSET-TEMP        PIC X(10) VALUE SPACE.       
           05 WS-DEST-AIRPORT-NAME.     
              49 WS-DEST-AIRPORT-NAME-LEN    PIC S9(4) COMP.            
              49 WS-DEST-AIRPORT-NAME-TEXT   PIC X(100).                
           05 WS-ORIGIN-AIRPORT-NAME.                                   
              49 WS-ORIGIN-AIRPORT-NAME-LEN  PIC S9(4) COMP.            
              49 WS-ORIGIN-AIRPORT-NAME-TEXT PIC X(100).                
           05 WS-FIRST-AIRPORT-NAME.                                    
              49 WS-FIRST-AIRPORT-NAME-LEN  PIC S9(4) COMP.             
              49 WS-FIRST-AIRPORT-NAME-TEXT PIC X(100).                 
           05 WS-SECOND-AIRPORT-NAME.                                   
              49 WS-SECOND-AIRPORT-NAME-LEN  PIC S9(4) COMP.            
              49 WS-SECOND-AIRPORT-NAME-TEXT PIC X(100).                
           05 WS-WHAT-RECORD-TO-READ         PIC S9(4) COMP VALUE 0.    
           05 WS-MODIFIED-TIMESTAMP          PIC X(26).                 
           05 WS-FIRST-AIRPORT-CODE          PIC X(3).                  
           05 WS-SECOND-AIRPORT-CODE         PIC X(3).                  
           05 WS-WORKING-TIMESTAMP.                                     
             10 WORK-TS-DATE.                                           
               15 QUEUE-1-DEP-YEAR   PIC 9(4).                          
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-1-DEP-MONTH    PIC 9(2).                        
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-1-DEP-DAY      PIC 9(2).                        
             10 FILLER       PIC X VALUE '-'.                           
             10 WORK-TS-TIME.                                           
               15 QUEUE-1-DEP-HOUR     PIC 9(2).                        
               15 FILLER      PIC X VALUE '.'.                          
               15 QUEUE-1-DEP-MINUTE   PIC 9(2).                        
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-1-DEP-SECOND   PIC 9(2).                          
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-1-DEP-MICROSEC PIC 9(6).                          
       01 WS-QUEUE-1-STRUCTURE.                                         
           05 QUEUE-1-FLIGHT-NUMBER          PIC X(15).                 
           05 QUEUE-1-DEPARTURE-DATE         PIC X(10).                 
           05 QUEUE-1-DEPARTURE-TIME         PIC X(5).                  
           05 QUEUE-1-ARRIVAL-DATE           PIC X(10).    
           05 QUEUE-1-ARRIVAL-TIME           PIC X(5).                  
           05 QUEUE-1-AIRPORT-ORIGIN-NAME    PIC X(100).                
           05 QUEUE-1-AIRPORT-DES-NAME       PIC X(100).                
           05 QUEUE-1-AMOUNT-OF-FREE-SETS    PIC 999.                   
           05 QUEUE-1-AIRLINE-NAME           PIC X(20).                 
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02182----------START-------------------'           
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02182-----------END--------------------'           
           PERFORM 3000-FINAL                                           
           .                                                            
      ***************************************************************** 
      *                         1000-INIT                               
      ***************************************************************** 
       1000-INIT.                                                       
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           DISPLAY 'Z02182 START Z02182-FLIGHT-ID: ' Z02182-FLIGHT-ID   
           PERFORM  1005-CHECK-IF-FIRST-TIME                            
           .                                                            
      ***************************************************************** 
      *                                                                 
      *                 1005-CHECK-IF-FIRST-TIME                        
      *                                                                 
      * PROGRAM CAN HAVE 3 MODES                                        
      *  FIRST ONE: SO-M-FIRST-WITHOUT WILL HAPPEN ONLY WHEN THIS       
      * IS THE FIRST TIME THIS PROGRAM RUNS AND USER DIDN'T PROVIDE     
      * ANY DATA BEFORE                                                 
      *  SECOND ONE: SO-M-FIRST-WITH WILL HAPPEN IF FOR EXAMPLE         
      * PROGRAM GENERATED SOME DATA BUT USER MADE A MISTAKE (           
      * PRESSED INVALID KEY AND PROGRAM CONTROL WENT ZO Z02141 PROGRAM) 
      * AFTER THEN IF USER PRESSED F3 ANDWE WILL GO BACK TO THIS PROGRAM
      * THEN SOME DATA WAS ALREADY GENERATED, NORMALLY WE WOULD         
      * DISPLAY THIS GENERAED DATA BUT BECAUSE PROCESS OF CREATION   
      * OF THIS DATA IS VERY SIMPLE WE WILL  ALWAYS BEHEVE LIKE IT WAS  
      * A FIRST OPTION ( SO-MS-FIRST-WITHOUT)                           
      *                                                                 
      *                                                                 
      *  THIRD ONE : SO-M-NOT-FIRST IT MEANS THAT PROGRAM               
      *     RETRIGERED AFTER THE USER PRESSED ATTENTION KEY             
      *  IF THIS IS THE CASE PROGRAM NEEDS TO CHECK WHAT USER PRESSED   
      * AND MAKE SOME ACTIONS ACCORDINGLY                               
      ***************************************************************** 
       1005-CHECK-IF-FIRST-TIME.                                        
           DISPLAY 'MODE W Z02182: ' SW-M-WHAT-MODE                     
           EVALUATE TRUE                                                
             WHEN SO-M-FIRST-WITHOUT                                    
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1020-DELETE-QUEUES                               
               SET SO-M-NOT-FIRST TO TRUE                               
               SET SO-PROGRAM-RUNS-FIRST-TIME TO TRUE                   
                                                                        
             WHEN SO-M-FIRST-WITH                                       
               SET SO-PROGRAM-RUNS-FIRST-TIME TO TRUE                   
               SET SO-M-NOT-FIRST TO TRUE                               
                                                                        
             WHEN SO-M-NOT-FIRST                                        
               SET SO-PROGRAM-RUNS-NOT-FIRST-TIME  TO TRUE              
             WHEN OTHER                                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID CALL' TO WS-Z02141-I-ERROR-MESSAGE(1)      
               SET SO-Z02141-M-WITHOUT TO TRUE                          
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
      *                     1020-DELETE-QUEUES                          
      ******************************************************************
       1020-DELETE-QUEUES.                                              
           EXEC CICS                                                    
            DELETEQ TS                                                  
            QUEUE(CT-DETAILS-QUEUE)                                     
            NOHANDLE                                                    
           END-EXEC                                                     
           IF EIBRESP  = DFHRESP(QIDERR) THEN                           
             CONTINUE                                                   
           ELSE                                                         
             PERFORM 2200-CHECK-EIBRESP                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                          2000-PROCESS                           
      * PARAGRAPH WILL MOVE PROGRAM LOGIC TO PARAGRAPHS DEPENDING ON    
      * WHAT MODE PROGRAM IS CURRENTLY IN                               
      *                                                                 
      * IN THIS PROGRA THERE ARE 2 MODES PROGRAM CAN BE IN              
      *  1. PROGRAM RUN FOR THE FIRST TIME                              
      *  2. PROGRAM WAS RETRIGERED BECAUSE USER PRESSED ATENTION KEY    
      *                                                                 
      * ANY OTHER ERROR MEANS THAT THIS IS AN ERROR                     
      *                                                                 
      *                                                                 
      *  FIRST OPTION WILL PROCESS INPUT AND WILL CREATE A QUEUE THAT   
      * WILL DISPLAY DETAILS OF ALL FLIGHTS                             
      *                                                                 
      * SECOND OPTION WILL CHECK WHAT KEY WAS PRESSED BY THE USER       
      * IF IT WAS A 'F3' KEY THEN CONTROL WILL RETURN TO CALLING PROGRAM
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
               MOVE 'SERIOUS ERROR IN Z02182' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       2001-PROCESS-FIRST-TIME                   
      * THIS PROGRAP WILL BE DISPLAYING DETAILS FOR ALL TYPES OF FLIGHTS
      * BUT HERE WE WILL HAVE TO CHOOSE ONE OUT OF 2 OPTIONS            
      *                                                                 
      *                                                                 
      * DUE TO THE FACT THAT BOTH  ONE WAY AND 2 WAY NON DIRECT FLIGHTS 
      * USES THE SAME PLACE IN COMMAREA THIS TWO OPTIONS WILL BE        
      * HANDLED BY ONE PARAGRAPH                                        
      *                                                                 
      * SECOND PARAGRAPH WILL HANDLE SCENARIOS WHEN USER SEARCHES       
      * FOR DIRECT 2WAY FLIGHT                                          
      ******************************************************************
       2001-PROCESS-FIRST-TIME.                                         
           EVALUATE TRUE                                                
           WHEN SO-Z02182-M-ONE-WAY                                     
           WHEN SO-Z02182-M-2-WAY-N-DIRECT                              
              PERFORM 2005-DISPLAY-ONE-WAY-FLIGHT                       
           WHEN SO-Z02182-M-2-WAY                                       
              PERFORM 2006-DISPLAY-2WAY-FLIGHT                          
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID PROGRAM MODE ' TO                          
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2002-PROCESS-WITH-DATA                        
      ******************************************************************
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2001-PROCESS-FIRST-TIME                              
           .                                                            
      ******************************************************************
      *                   2003-PROCESS-NOT-FIRST-TIME                   
      * PARAGRAPH IS CALLED WHEN PROGRAM WAS RETTRIGERED BECAUSE OF     
      * THE FACT THAT USER PRESSED ATTENTION KEY,                       
      * NOW WE NEED TO FIND OUT IF THIS ATTENTION KEY HAS SOME ACTION   
      * ASSIGNED AND IF SO THEN PROGRAM SHOULD PERFORM THOSE ACTIONS    
      *                                                                 
      * IF USER PRESSED "NO ACTION KEY" THEN HE WILL GET PROPER MESSAGE 
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           DISPLAY 'W Z02182 2003 PERFORMED'                            
           EVALUATE EIBAID                                              
      * AFTER PRESSING F8 USER WILL SEE NEXT 4 ROWS OF DATA             
           WHEN DFHPF8                                                  
               PERFORM 2015-DISPLAY-NEXT-4                              
      * AFTER PRESSING F7 USER WILL SEE PREVIOUS 4 ROWS OF DATA         
           WHEN DFHPF7                                                  
               PERFORM 2017-DISPLAY-PREV-4                              
      * AFTER PRESSING F3 PROGRAM CONTROL WILL RETURN TO CALLING PROGRAM
           WHEN DFHPF3       
               SET SO-FINAL-TERMINATION TO TRUE                         
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID KEY       ' TO                             
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2005-DISPLAY-ONE-WAY-FLIGHT                   
      * DATA SHOULD BE ALREADY IN THE RECORD PROVIDED BY THE            
      * Z02172 PROGRAM                                                  
      *                                                                 
      * HERE PARAGRAPH LOGIC CHANGES DEPENDING ON THE FACT IF           
      * THIS IS DIRECT FLIGHT OR NOT                                    
      *                                                                 
      * IF THIS IS SINGLE DIRECT FLIGHT LOGIC IS THE SIMPLEST           
      *  PROGRAM WILL GET ALL DATA AND DISPLAY RESULT                   
      *                                                                 
      * IF FLIGHTS HAS 1 TRANSFER FLIGHT THEN PROGRAM WILL HAVE TO USER 
      * 2 OTHER PARAGRAPHS (PREPARE FIRST FLIGHT AND PREPARE LAST FLIGHT
      *                                                                 
      * IF THERE IS MORE TRANSFERS PARAGRAPH WILL USE 3 PARAGRAPHS      
      * (PREAPRE FIRST FLIGHT, PREPERE MIDDLE FLIGHTS AND PREPARE LAST  
      * FLIGHT)                                                         
      ******************************************************************
       2005-DISPLAY-ONE-WAY-FLIGHT.                                     
                                                                        
           IF Z02182-TRANSFER-NUMBER = 00 THEN                          
      * WRITES NON TRANSFER FLIGHT                                      
             PERFORM 2301-WRITE-NON-TRANSFER-F                          
           ELSE                                                         
      * IF WE GOT EXACLY ONE TRANSFER (SO WE GOT 2 FLIGHTS)             
      * WE HAVE TO PREAPRE THE FIRST AND THE LAST FLIGHT (THIS 2)
               PERFORM 2302-WRITE-1-TRANSFER-F                          
                                                                        
             ELSE                                                       
      * WS-AMOUNT-OF-MIDDLE-FLIGHTS  IS A NUMBER OF FLIGHTS AFTER       
      * THE FIRST ONE AND BEFORE THE LAST ONE                           
      * IF WE GOT 5 FLIGHTS THEN THIS NUMBER WILL BE 3                  
                                                                        
      * A SUBFLIGHT IS A FLIGHT THAT IS NOT FIRST IN A SEQENCE          
      * THERE IS ONE FIRST MAIN FLIGHT AND ALL FLIGHTS                  
      * ARE CALLED SUBFLIGHTS                                           
               PERFORM 2007-PREPARE-FIRST-FLIGHT                        
               COMPUTE WS-AMOUNT-OF-MIDDLE-FLIGHTS =                    
                                             Z02182-TRANSFER-NUMBER  - 1
               MOVE 1 TO WS-SUBFLIGHT-COUNT                             
               PERFORM WS-AMOUNT-OF-MIDDLE-FLIGHTS  TIMES               
                  PERFORM 2009-PREPARE-MIDDLE-FLIGHTS                   
                  ADD 1 TO WS-SUBFLIGHT-COUNT                           
               END-PERFORM                                              
                                                                        
               PERFORM 2008-PREPARE-LAST-FLIGHT                         
             END-IF                                                     
           END-IF                                                       
           PERFORM 2010-DISPLAY-DATA                                    
           .                                                            
      ****************************************************************  
      *                  2006-DISPLAY-2WAY-FLIGHT                       
      *                                                                 
      * PARAGRAPH WILL PREPARE DATA FOR 2 WAY FLIGHTS                   
      *                                                                 
      ****************************************************************  
       2006-DISPLAY-2WAY-FLIGHT.                                        
           PERFORM 2306-MOVE-2WAY-TO-TO-QUEUE                           
           PERFORM 7009-GET-BOTH-AIRPORTS-NAMES                         
           MOVE WS-FIRST-AIRPORT-NAME-TEXT TO                           
              QUEUE-1-AIRPORT-ORIGIN-NAME                               
           MOVE WS-SECOND-AIRPORT-NAME-TEXT TO                          
                          QUEUE-1-AIRPORT-DES-NAME                      
                                                                        
           MOVE  Z02182-2WAY-TO-FLIGHT-AIRLINE  TO AIRLINE-CODE         
           PERFORM 2308-GET-AIRLINE-NAME                                
                                                                        
           PERFORM 2307-MOVE-2WAY-FROM-TO-QUEUE                         
                                                                        
           MOVE  Z02182-2WAY-FROM-FL-AIRLINE  TO AIRLINE-CODE           
           PERFORM 2308-GET-AIRLINE-NAME                                
                                                                        
           PERFORM 2010-DISPLAY-DATA                                    
           .                                                            
      ****************************************************************  
      *                 2007-PREPARE-FIRST-FLIGHT                       
      * PARAGRAPH WILL GET ALL DETAILS ABOUT FIRST TRANSFER FLIGHT      
      *                                                                 
      ****************************************************************  
       2007-PREPARE-FIRST-FLIGHT.                                       
           MOVE Z02182-DEPARTURE-DATE TO QUEUE-1-DEPARTURE-DATE         
           MOVE Z02182-DEPARTURE-TIME TO QUEUE-1-DEPARTURE-TIME         
           MOVE Z02182-FLIGHT-NUMBER  TO QUEUE-1-FLIGHT-NUMBER          
           MOVE Z02182-AMOUNT-OF-FREE-SETS-M TO                         
                                             QUEUE-1-AMOUNT-OF-FREE-SETS
           MOVE Z02182-FLIGHT-ID     TO T05-FLIGHT-ID-TEXT              
           PERFORM 2222-CALCULATE-FLIGHT-ID-LEN                         
           PERFORM 7002-GET-DEST-AIR-NAME                               
           MOVE T02-AIRPORT-FULL-NAME-TEXT TO QUEUE-1-AIRPORT-DES-NAME  
                                                                        
           MOVE Z02182-AIRPORT-ORIGIN-CODE TO T02-AIRPORT-CODE          
           PERFORM 7003-TRANSLATE-AIRPORT-IATA                          
           MOVE T02-AIRPORT-FULL-NAME-TEXT TO                           
                                             QUEUE-1-AIRPORT-ORIGIN-NAME
                                                                        
      *    MOVE Z02182-AIRLINE-CODE-M TO AIRLINE-CODE                   
      *    PERFORM 7004-TRANSLATE-AIRLINE-IATA                          
      *    MOVE AIRLINE-NAME-TEXT TO QUEUE-1-AIRLINE-NAME  
                                                                        
           PERFORM 7005-GET-ARRIVAL-TIME                                
           MOVE WS-MODIFIED-TIMESTAMP TO WS-WORKING-TIMESTAMP           
           MOVE WORK-TS-TIME TO QUEUE-1-ARRIVAL-TIME                    
           MOVE WORK-TS-DATE TO QUEUE-1-ARRIVAL-DATE                    
                                                                        
           PERFORM 2102-WRITE-TO-QUEUE                                  
           .                                                            
      ****************************************************************  
      *                   2008-PREPARE-LAST-FLIGHT                      
      * THIS PARAGRAPH WILL WRITE ALL DATA ABOUT LAST FLIGHT TO QUEUE   
      ****************************************************************  
       2008-PREPARE-LAST-FLIGHT.                                        
                                                                        
           MOVE Z02182-TR-FLIGHT-ID(WS-SUBFLIGHT-COUNT)                 
                                                TO QUEUE-1-FLIGHT-NUMBER
           MOVE Z02182-TR-FLIGHT-SEATS(WS-SUBFLIGHT-COUNT) TO           
                                             QUEUE-1-AMOUNT-OF-FREE-SETS
           MOVE Z02182-ARRIVAL-DATE TO QUEUE-1-ARRIVAL-DATE             
           MOVE Z02182-ARRIVAL-TIME TO QUEUE-1-ARRIVAL-TIME             
           MOVE Z02182-TR-FLIGHT-ID(WS-SUBFLIGHT-COUNT)                 
                                                   TO T05-FLIGHT-ID-TEXT
           PERFORM 2222-CALCULATE-FLIGHT-ID-LEN                         
           PERFORM 7006-GET-AIRLINE-NAME                                
           MOVE T05-FLIGHT-NUMBER-TEXT TO QUEUE-1-FLIGHT-NUMBER         
           MOVE AIRLINE-NAME-TEXT TO QUEUE-1-AIRLINE-NAME               
                                                                        
      * IN CASE WE ARE SEARCHIGN FOR N-DIRECT RETURN FLIGHTS            
      * THEN OUR ORIGIN AIRPORT WILL BE ALSO OUR DEPARTURE AIRPORT      
      *                                                                 
      * IN OTHER CASE WE WILL USE DEPARTURE AIRPORT                     
           IF SO-Z02182-M-2-WAY-N-DIRECT THEN                           
             MOVE Z02182-AIRPORT-ORIGIN-CODE TO T02-AIRPORT-CODE        
           ELSE                                                         
             MOVE Z02182-AIRPORT-DES-CODE TO T02-AIRPORT-CODE           
           END-IF    
           PERFORM 7003-TRANSLATE-AIRPORT-IATA                          
           MOVE T02-AIRPORT-FULL-NAME-TEXT TO QUEUE-1-AIRPORT-DES-NAME  
                                                                        
           PERFORM 7007-GET-DEP-AIR-NAME                                
           MOVE T02-AIRPORT-FULL-NAME-TEXT TO                           
                                             QUEUE-1-AIRPORT-ORIGIN-NAME
                                                                        
           PERFORM 7008-GET-DEPATURE-TIME                               
                                                                        
           MOVE WS-MODIFIED-TIMESTAMP TO WS-WORKING-TIMESTAMP           
           MOVE WORK-TS-TIME TO QUEUE-1-DEPARTURE-TIME                  
           MOVE WORK-TS-DATE TO QUEUE-1-DEPARTURE-DATE                  
                                                                        
                                                                        
           PERFORM 2102-WRITE-TO-QUEUE                                  
           .                                                            
      ****************************************************************  
      *                 2009-PREPARE-MIDDLE-FLIGHTS                     
      *                                                                 
      * PARAGRAPH WILL PREPARE MIDDLE FLIGHTS (NOT FIRST OR LAST)       
      * AFTER PREPARATION MIDDLE FLIGHTS WILL BE WRITTEN INTO THE       
      * QUEUE ( WE WILL USE THAT QUEUE TO DISPLAY FLIGHTS ON THE        
      * SCREEN AND THANKS TO THAT QUEUE WE WILL CREATE PAGING LOGIC)    
      ****************************************************************  
       2009-PREPARE-MIDDLE-FLIGHTS.                                     
           MOVE Z02182-TR-FLIGHT-SEATS(WS-SUBFLIGHT-COUNT)              
                                          TO QUEUE-1-AMOUNT-OF-FREE-SETS
           MOVE Z02182-TR-FLIGHT-ID(WS-SUBFLIGHT-COUNT)                 
                                             TO  QUEUE-1-FLIGHT-NUMBER  
           MOVE Z02182-TR-FLIGHT-ID(WS-SUBFLIGHT-COUNT) TO              
                                      T05-FLIGHT-ID-TEXT                
           PERFORM 2222-CALCULATE-FLIGHT-ID-LEN                         
           PERFORM 7009-GET-BOTH-AIRPORTS-NAMES                         
           MOVE T05-FLIGHT-NUMBER-TEXT TO QUEUE-1-FLIGHT-NUMBER         
           MOVE WS-FIRST-AIRPORT-NAME-TEXT TO                           
                                             QUEUE-1-AIRPORT-ORIGIN-NAME
           MOVE WS-SECOND-AIRPORT-NAME-TEXT TO                        
                                           QUEUE-1-AIRPORT-DES-NAME   
           PERFORM 7006-GET-AIRLINE-NAME                              
           MOVE AIRLINE-NAME-TEXT TO QUEUE-1-AIRLINE-NAME             
                                                                      
           MOVE WS-FIRST-AIRPORT-CODE TO T02-AIRPORT-CODE             
           PERFORM 7008-GET-DEPATURE-TIME                             
           MOVE WS-MODIFIED-TIMESTAMP TO WS-WORKING-TIMESTAMP         
           MOVE WORK-TS-TIME TO QUEUE-1-DEPARTURE-TIME                
           MOVE WORK-TS-DATE TO QUEUE-1-DEPARTURE-DATE                
                                                                      
                                                                      
           MOVE WS-SECOND-AIRPORT-CODE TO T02-AIRPORT-CODE            
           PERFORM 7005-GET-ARRIVAL-TIME                              
           MOVE WS-MODIFIED-TIMESTAMP TO WS-WORKING-TIMESTAMP         
           MOVE WORK-TS-TIME TO QUEUE-1-ARRIVAL-TIME                  
           MOVE WORK-TS-DATE TO QUEUE-1-ARRIVAL-DATE                  
           PERFORM 2102-WRITE-TO-QUEUE                                
           .                                                          
      ****************************************************************
      *                     2010-DISPLAY-DATA                         
      ****************************************************************
       2010-DISPLAY-DATA.                                             
           MOVE 1 TO Z02182-Q1-LAST-REC-ID                            
           PERFORM 2015-DISPLAY-NEXT-4                                
           .                                                          
      ****************************************************************
      *                  2011-INITIALIZE-MAP-DATA                     
      ****************************************************************
       2011-INITIALIZE-MAP-DATA.                                      
           MOVE LOW-VALUES TO MP0218O                                 
           PERFORM VARYING WS-ITER6 FROM 1 BY 1 UNTIL WS-ITER6 > 4    
              MOVE LOW-VALUES TO FLIGHT-NUMBER-A(WS-ITER6)            
              MOVE LOW-VALUES TO DEPARTURE-DATE-A(WS-ITER6)           
              MOVE LOW-VALUES TO DEPARTURE-TIME-A(WS-ITER6)           
              MOVE LOW-VALUES TO ARRIVAL-DATE-A(WS-ITER6)   
              MOVE LOW-VALUES TO ARRIVAL-TIME-A(WS-ITER6)               
              MOVE LOW-VALUES TO ORIGIN-AIRPORT-A(WS-ITER6)             
              MOVE LOW-VALUES TO ARRIVAL-AIRPORT-A(WS-ITER6)            
              MOVE LOW-VALUES TO FREE-SEATS-A(WS-ITER6)                 
              MOVE LOW-VALUES TO AIRLINE-A(WS-ITER6)                    
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                    2012-READ-QUEUE-1                            
      ****************************************************************  
       2012-READ-QUEUE-1.                                               
           SET SO-NOT-END-OF-QUEUE-1  TO TRUE                           
           INITIALIZE WS-QUEUE-1-STRUCTURE                              
           EXEC CICS                                                    
            READQ TS                                                    
            QUEUE(CT-DETAILS-QUEUE)                                     
            ITEM(WS-WHAT-RECORD-TO-READ)                                
            INTO(WS-QUEUE-1-STRUCTURE)                                  
            NOHANDLE                                                    
           END-EXEC                                                     
                                                                        
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-1 TO TRUE                              
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                2013-CHECK-FOR-QIDEROR                           
      ****************************************************************  
       2013-CHECK-FOR-QIDEROR.                                          
           IF EIBRESP  = DFHRESP(QIDERR) THEN                           
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE   
              MOVE 'QIDEROR 2013 PARA ' TO WS-Z02141-I-ERROR-MESSAGE(1) 
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *               2014-MOVE-QUEUE-TO-SCREEN                         
      ****************************************************************  
       2014-MOVE-QUEUE-TO-SCREEN.                                       
           MOVE SPACE TO  FLIGHT-NUMBER-O(WS-ITER3)                     
           MOVE QUEUE-1-FLIGHT-NUMBER  TO FLIGHT-NUMBER-O(WS-ITER3)     
           MOVE QUEUE-1-DEPARTURE-DATE TO DEPARTURE-DATE-O(WS-ITER3)    
           MOVE QUEUE-1-DEPARTURE-TIME TO DEPARTURE-TIME-O(WS-ITER3)    
           MOVE QUEUE-1-ARRIVAL-DATE   TO ARRIVAL-DATE-O(WS-ITER3)      
           MOVE QUEUE-1-ARRIVAL-TIME   TO ARRIVAL-TIME-O(WS-ITER3)      
           MOVE QUEUE-1-AIRPORT-ORIGIN-NAME TO                          
                                              ORIGIN-AIRPORT-O(WS-ITER3)
           MOVE QUEUE-1-AIRPORT-DES-NAME TO ARRIVAL-AIRPORT-O(WS-ITER3) 
                                                                        
           DISPLAY 'QUEUE1 -ORIGIN-NAME: ' QUEUE-1-AIRPORT-ORIGIN-NAME  
           DISPLAY 'QUEUE1 -DEST-NAME: ' QUEUE-1-AIRPORT-DES-NAME       
           DISPLAY 'SCREEN -ORIGIN-NAME: ' ORIGIN-AIRPORT-O(WS-ITER3)   
           DISPLAY 'SCREEN -DEST-NAME: '  ARRIVAL-AIRPORT-O(WS-ITER3)   
                                                                        
           MOVE QUEUE-1-AMOUNT-OF-FREE-SETS  TO FREE-SEATS-O(WS-ITER3)  
           MOVE QUEUE-1-AIRLINE-NAME    TO AIRLINE-O(WS-ITER3)          
           .                                                            
      ****************************************************************  
      *                        2015-DISPLAY-NEXT-4                      
      ****************************************************************  
       2015-DISPLAY-NEXT-4.                                             
           MOVE  Z02182-Q1-LAST-REC-ID TO WS-WHAT-RECORD-TO-READ        
           PERFORM 2011-INITIALIZE-MAP-DATA                             
           PERFORM 2012-READ-QUEUE-1                                    
           PERFORM 2013-CHECK-FOR-QIDEROR                               
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 4      
                                           OR SO-END-OF-QUEUE-1         
               IF WS-ITER3 = 1 THEN                                     
                 MOVE WS-WHAT-RECORD-TO-READ TO Z02182-Q1-FIRST-REC-ID  
               END-IF                                                   
               PERFORM 2014-MOVE-QUEUE-TO-SCREEN                        
               MOVE WS-WHAT-RECORD-TO-READ TO Z02182-Q1-LAST-REC-ID     
               ADD 1 TO WS-WHAT-RECORD-TO-READ                          
               PERFORM 2012-READ-QUEUE-1                                
           END-PERFORM                                                  
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                        2015-DISPLAY-NEXT-4                      
      ****************************************************************  
       2017-DISPLAY-PREV-4.                                             
           IF  Z02182-Q1-FIRST-REC-ID - 4 >= 1 THEN                     
               SUBTRACT 4 FROM Z02182-Q1-FIRST-REC-ID                   
           ELSE                                                         
               MOVE 1 TO Z02182-Q1-FIRST-REC-ID                         
           END-IF                                                       
           MOVE Z02182-Q1-FIRST-REC-ID TO Z02182-Q1-LAST-REC-ID         
           PERFORM   2015-DISPLAY-NEXT-4                                
           .                                                            
      ****************************************************************  
      *                     2100-SEND-THE-MAP                           
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
             SEND MAP('MP0218') MAPSET('MP0218')                        
             FROM(MP0218O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                     2102-WRITE-TO-QUEUE     
      ****************************************************************  
       2102-WRITE-TO-QUEUE.                                             
           EXEC CICS                                                    
            WRITEQ TS                                                   
            QUEUE(CT-DETAILS-QUEUE)                                     
            FROM(WS-QUEUE-1-STRUCTURE)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
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
      *                  2222-CALCULATE-FLIGHT-ID-LEN                   
      ******************************************************************
       2222-CALCULATE-FLIGHT-ID-LEN.                                    
           DISPLAY 'LENGTH OF ID-TEXT: ' LENGTH OF T05-FLIGHT-ID-TEXT   
           MOVE FUNCTION REVERSE(T05-FLIGHT-ID-TEXT) TO                 
                   WS-TEMP-STRING                                       
           MOVE 0 TO WS-ITER9                                           
           INSPECT WS-TEMP-STRING TALLYING WS-ITER9 FOR LEADING SPACES  
           DISPLAY 'LEADING SPACES: ' WS-ITER9                          
                                                                        
           COMPUTE T05-FLIGHT-ID-LEN = LENGTH OF T05-FLIGHT-ID-TEXT -   
                                   WS-ITER9                             
           INITIALIZE WS-TEMP-STRING                                    
           MOVE T05-FLIGHT-ID-TEXT TO WS-TEMP-STRING                    
           INSPECT WS-TEMP-STRING REPLACING ALL ' ' BY '&'              
           DISPLAY 'ZAMIENIONE: ' WS-TEMP-STRING                        
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
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           SET  SO-Z02141-I-FIRST-TIME TO TRUE     
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
                                                                        
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   2301-WRITE-NON-TRANSFER-F                     
      * PROGRAM WILL MOVE DATA FROM COMMAREA TO QUEUE DATA AND AFTER    
      * PREPARATION THIS DATA WILL BE SAVED TO QUEUE                    
      ******************************************************************
       2301-WRITE-NON-TRANSFER-F.                                       
           MOVE Z02182-FLIGHT-ID TO T05-FLIGHT-ID-TEXT                  
           PERFORM 2222-CALCULATE-FLIGHT-ID-LEN                         
           PERFORM 7009-GET-BOTH-AIRPORTS-NAMES                         
           MOVE WS-FIRST-AIRPORT-NAME-TEXT                              
                                         TO  WS-ORIGIN-AIRPORT-NAME-TEXT
           MOVE WS-SECOND-AIRPORT-NAME-TEXT                             
                                            TO WS-DEST-AIRPORT-NAME-TEXT
           DISPLAY 'ORIGIN AIRPORT: '      WS-ORIGIN-AIRPORT-NAME-TEXT  
           DISPLAY 'DESTINATION AIRPORT: ' WS-DEST-AIRPORT-NAME-TEXT    
           MOVE LOW-VALUES TO MP0218O                                   
           MOVE Z02182-AIRLINE-CODE-M         TO AIRLINE-CODE           
                                                                        
           PERFORM 7004-TRANSLATE-AIRLINE-IATA                          
           INITIALIZE WS-QUEUE-1-STRUCTURE                              
           MOVE AIRLINE-NAME-TEXT             TO QUEUE-1-AIRLINE-NAME   
                                                                        
           MOVE Z02182-FLIGHT-NUMBER          TO QUEUE-1-FLIGHT-NUMBER  
           DISPLAY '2005 FLIGHT NUMBER:  '  Z02182-FLIGHT-NUMBER        
           MOVE Z02182-DEPARTURE-DATE         TO QUEUE-1-DEPARTURE-DATE 
           MOVE Z02182-DEPARTURE-TIME         TO QUEUE-1-DEPARTURE-TIME 
           MOVE Z02182-ARRIVAL-DATE           TO QUEUE-1-ARRIVAL-DATE   
           MOVE Z02182-ARRIVAL-TIME           TO QUEUE-1-ARRIVAL-TIME   
           DISPLAY 'WS-ORIGIN AIPORT-NAME-TEXT '                        
                          WS-ORIGIN-AIRPORT-NAME-TEXT          
           MOVE WS-ORIGIN-AIRPORT-NAME-TEXT   TO                        
                              QUEUE-1-AIRPORT-ORIGIN-NAME               
           DISPLAY 'WS-DEST-AIRPORT-NAME-TEXT PONIZEJ: '                
           DISPLAY WS-DEST-AIRPORT-NAME-TEXT                            
           MOVE WS-DEST-AIRPORT-NAME-TEXT     TO                        
                             QUEUE-1-AIRPORT-DES-NAME                   
           MOVE Z02182-AMOUNT-OF-FREE-SETS-M  TO                        
                            QUEUE-1-AMOUNT-OF-FREE-SETS                 
           DISPLAY 'QUEUE  RECORDS BEFORE WRITE:  '                     
           DISPLAY 'ORIGIN: '  QUEUE-1-AIRPORT-ORIGIN-NAME              
           DISPLAY 'DEST:   '  QUEUE-1-AIRPORT-DES-NAME                 
                                                                        
           PERFORM 2102-WRITE-TO-QUEUE                                  
           .                                                            
      ******************************************************************
      *                   2302-WRITE-1-TRANSFER-F                       
      * PARAGRAPH WILL PREPARE BOTH FLIGHTS                             
      * THE FIRST AND THE LAST ( WE GOT ONLY 2 FLIGHTS HERE)            
      *                                                                 
      * DATA AFTER PREPARATION WILL BE SAVED TO QUEUE                   
      ******************************************************************
       2302-WRITE-1-TRANSFER-F.                                         
           MOVE 1 TO WS-SUBFLIGHT-COUNT                                 
           PERFORM 2007-PREPARE-FIRST-FLIGHT                            
           PERFORM 2008-PREPARE-LAST-FLIGHT                             
           .                                                            
      ******************************************************************
      *                   2305-PREPARE-TIME-OFFSET                      
      * TIME ZONE IS STORES AS SHH.MM WHERE  S IS A SIGN                
      * HH IS AMOUNT OF HOURS AND MM IS AMOUNT OF MINUTES               
      * WE NEED ALL OF THAT INFO TO CORRECTLY MOVE UTC TIMESTAMP TO     
      * LOCAL AIRPORT TIMESTAMP                                         
      *                                                                 
      * PARAGRAPH WILL ALSO VALIDATE IF THIS TIMEZONE IS VALID          
      *                                                                 
      ******************************************************************
       2305-PREPARE-TIME-OFFSET.                                        
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
               MOVE 'DATABASE ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)  
               SET    SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           INITIALIZE WS-MODIFIED-TIMESTAMP                             
           .                                                            
      ******************************************************************
      *                    2306-MOVE-2WAY-TO-TO-QUEUE                   
      * PARAGRAPH WILL MOVE 2 WAY DIRECT FLIGHTS DATA TO QUEUE          
      * PARAGRAPH WILL ONLY HANDLE FLIGHT "TO"                          
      ******************************************************************
       2306-MOVE-2WAY-TO-TO-QUEUE.                                      
           MOVE  Z02182-2WAY-FLIGHT-TO-NUMBER TO                        
                         QUEUE-1-FLIGHT-NUMBER                          
           MOVE Z02182-2WAY-FLIGHT-TO-DEP-DATE TO                       
                         QUEUE-1-DEPARTURE-DATE                         
           MOVE Z02182-2WAY-FLIGHT-TO-DEP-TIME TO                       
                        QUEUE-1-DEPARTURE-TIME 
           MOVE Z02182-2WAY-FLIGHT-TO-ARV-DATE TO                       
                          QUEUE-1-ARRIVAL-DATE                          
           MOVE  Z02182-2WAY-FLIGHT-TO-ARV-TIME TO                      
                          QUEUE-1-ARRIVAL-TIME                          
           MOVE Z02182-2WAY-TO-FLIGHT-SEATS     TO                      
                        QUEUE-1-AMOUNT-OF-FREE-SETS                     
           MOVE Z02182-2WAY-FLIGHT-TO-ID     TO T05-FLIGHT-ID-TEXT      
           PERFORM 2222-CALCULATE-FLIGHT-ID-LEN                         
           .                                                            
      ******************************************************************
      *                   2307-MOVE-2WAY-FROM-TO-QUEUE                  
      * PARAGRAPH WILL BE CALLED WHEN PROGRAM HANDLES 2WAY              
      * DIRECT FLIGHTS                                                  
      * THIS PRARGRAPH WILL MOVE "FROM" FLIGHT DATA TO QUEUE            
      ******************************************************************
       2307-MOVE-2WAY-FROM-TO-QUEUE.                                    
           MOVE  Z02182-2WAY-FLIGHT-FROM-NUMBER TO                      
                         QUEUE-1-FLIGHT-NUMBER                          
           MOVE Z02182-2WAY-FL-FROM-DEP-DATE TO                         
                         QUEUE-1-DEPARTURE-DATE                         
           MOVE Z02182-2WAY-FL-FROM-DEP-TIME TO                         
                        QUEUE-1-DEPARTURE-TIME                          
           MOVE Z02182-2WAY-FL-FROM-ARV-DATE TO                         
                          QUEUE-1-ARRIVAL-DATE                          
           MOVE  Z02182-2WAY-FL-FROM-ARV-TIME TO                        
                          QUEUE-1-ARRIVAL-TIME                          
           MOVE Z02182-2WAY-FROM-FLIGHT-SEATS     TO                    
                        QUEUE-1-AMOUNT-OF-FREE-SETS                     
                                                                        
           MOVE WS-FIRST-AIRPORT-NAME-TEXT TO                           
                          QUEUE-1-AIRPORT-DES-NAME                      
           MOVE WS-SECOND-AIRPORT-NAME-TEXT TO                          
                         QUEUE-1-AIRPORT-ORIGIN-NAME                    
           .                                                            
      ******************************************************************
      *                      2308-GET-AIRLINE-NAME                      
      * PARAGRAPH WILL TRANSALTE IATA CODE TO FULL AIRLINE NAME         
      ******************************************************************
       2308-GET-AIRLINE-NAME.                                           
           PERFORM 7004-TRANSLATE-AIRLINE-IATA                          
           MOVE AIRLINE-NAME-TEXT TO QUEUE-1-AIRLINE-NAME               
           PERFORM 2102-WRITE-TO-QUEUE                                  
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
              PERFORM 3002-RETURN-TO-CALLING-PROG                       
           WHEN OTHER                                                   
              PERFORM 3003-SEND-ERROR-MESSAGE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  3001-RETURN-WITH-TRANSID                       
      ******************************************************************
       3001-RETURN-WITH-TRANSID.                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            RETURN TRANSID('0212') COMMAREA(DFHCOMMAREA)                
           END-EXEC   
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   3002-RETURN-TO-CALLING-PROG                   
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
      *                   3003-SEND-ERROR-MESSAGE                       
      ******************************************************************
       3003-SEND-ERROR-MESSAGE.                                         
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)       
           SET    SO-Z02141-M-WITH TO TRUE                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   7002-GET-DEST-AIR-NAME                        
      ******************************************************************
       7002-GET-DEST-AIR-NAME.                                          
           INITIALIZE  T02-AIRPORT-FULL-NAME                            
           INITIALIZE  T02-AIRPORT-CODE                                 
           DISPLAY 'PRZED 7002: '                                       
           DISPLAY 'T05-FLIGHT-ID-TEXT: ' T05-FLIGHT-ID-TEXT            
           DISPLAY 'T05-FLIGHT-ID-LEN: ' T05-FLIGHT-ID-LEN              
           EXEC SQL                                                     
           SELECT AIRPORT_FULL_NAME,                                    
                  AIRPORT_CODE                                          
             INTO :T02-AIRPORT-FULL-NAME,  
                  :T02-AIRPORT-CODE                                     
                                                                        
           FROM T02_AIRPORT_TABLE                                       
           INNER JOIN T05_FLIGHT_TABLE ON                               
           ARRIVAL_AIRPORT_CODE = AIRPORT_CODE                          
           WHERE FLIGHT_ID = :T05-FLIGHT-ID                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             DISPLAY '7002 SQLCODE NOT NORMAL '                         
             SET SO-7002-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           DISPLAY '7002 SQLCODE  NORMAL '                              
           .                                                            
      ******************************************************************
      *                   7003-TRANSLATE-AIRPORT-IATA                   
      ******************************************************************
       7003-TRANSLATE-AIRPORT-IATA.                                     
           DISPLAY '7003-TRANSLATE-AIRPORT-IATA PERFORMED'              
           INITIALIZE  T02-AIRPORT-FULL-NAME                            
           EXEC SQL                                                     
            SELECT AIRPORT_FULL_NAME                                    
            INTO :T02-AIRPORT-FULL-NAME                                 
            FROM T02_AIRPORT_TABLE                                      
            WHERE AIRPORT_CODE = :T02-AIRPORT-CODE                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
                                                                        
           DISPLAY '7003-TRANSLATE-AIRPORT-IATA SQLCODE NOT NOMRAL '    
              SET SO-7003-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           DISPLAY '7003-TRANSLATE-AIRPORT-IATA SQLCODE  NOMRAL '       
           .                 
      ******************************************************************
      *                   7004-TRANSLATE-AIRLINE-IATA                   
      ******************************************************************
       7004-TRANSLATE-AIRLINE-IATA.                                     
           INITIALIZE AIRLINE-NAME                                      
           EXEC SQL                                                     
            SELECT AIRLINE_NAME                                         
            INTO   :AIRLINE-NAME                                        
            FROM T01_AIRLINE_NAMES_TABLE                                
            WHERE AIRLINE_CODE = :AIRLINE-CODE                          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7004-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                   7005-GET-ARRIVAL-TIME                         
      * PARAGRAPH HAS TO RETURN TIME OF ARRIVAL AND DATE OF ARRIVAL     
      * BASED OF T05-FLIGHT-ID FLIGHT                                   
      * AND THIS TIME NEED TO BE MOVED INTO LOCAL AIRPORT TIME ZONE     
      ****************************************************************  
       7005-GET-ARRIVAL-TIME.                                           
           PERFORM 7010-GET-TIMEZONE                                    
           PERFORM 2305-PREPARE-TIME-OFFSET                             
           PERFORM 7011-ARV-TIME-TO-LOCAL-ZONE                          
           .                                                            
      ******************************************************************
      *                    7006-GET-AIRLINE-NAME                        
      ******************************************************************
       7006-GET-AIRLINE-NAME.                                           
           INITIALIZE AIRLINE-NAME                                      
           EXEC SQL                                                     
             SELECT AIRLINE_NAME,                                       
                    FLIGHT_NUMBER  
             INTO :AIRLINE-NAME,                                        
                  :T05-FLIGHT-NUMBER                                    
             FROM T01_AIRLINE_NAMES_TABLE                               
             INNER JOIN T05_FLIGHT_TABLE ON                             
             T01_AIRLINE_NAMES_TABLE.AIRLINE_CODE   =                   
             T05_FLIGHT_TABLE.AIRLINE_CODE                              
             WHERE FLIGHT_ID = :T05-FLIGHT-ID                           
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7006-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7007-GET-DEP-AIR-NAME                         
      ******************************************************************
       7007-GET-DEP-AIR-NAME.                                           
           INITIALIZE T02-AIRPORT-FULL-NAME,                            
           EXEC SQL                                                     
           SELECT AIRPORT_FULL_NAME,                                    
                  AIRPORT_CODE                                          
             INTO :T02-AIRPORT-FULL-NAME,                               
                  :T02-AIRPORT-CODE                                     
           FROM T02_AIRPORT_TABLE                                       
           INNER JOIN T05_FLIGHT_TABLE ON                               
           DEPARTURE_AIRPORT_CODE = AIRPORT_CODE                        
           WHERE FLIGHT_ID = :T05-FLIGHT-ID                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7002-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .          
      ******************************************************************
      *                     7008-GET-DEPATURE-TIME                      
      ******************************************************************
       7008-GET-DEPATURE-TIME.                                          
           PERFORM 7010-GET-TIMEZONE                                    
                                                                        
           PERFORM 2305-PREPARE-TIME-OFFSET                             
           PERFORM 7012-DEPARTURE-TIME-TO-UTC                           
           .                                                            
      ******************************************************************
      *                  7009-GET-BOTH-AIRPORTS-NAMES                   
      ******************************************************************
       7009-GET-BOTH-AIRPORTS-NAMES.                                    
           DISPLAY '7009 W Z02182 T05-FLIGHT-ID: ' T05-FLIGHT-ID-TEXT   
           DISPLAY 'LEN OF FLIGHT ID: ' T05-FLIGHT-ID-LEN               
           INITIALIZE WS-FIRST-AIRPORT-NAME                             
           INITIALIZE WS-SECOND-AIRPORT-NAME                            
           EXEC SQL                                                     
             SELECT                                                     
               F1.AIRPORT_FULL_NAME,                                    
               F1.AIRPORT_CODE,                                         
               F2.AIRPORT_FULL_NAME,                                    
               F2.AIRPORT_CODE,                                         
               FLIGHT_NUMBER                                            
             INTO                                                       
               :WS-FIRST-AIRPORT-NAME,                                  
               :WS-FIRST-AIRPORT-CODE,                                  
               :WS-SECOND-AIRPORT-NAME,                                 
               :WS-SECOND-AIRPORT-CODE,                                 
               :T05-FLIGHT-NUMBER                                       
              FROM T02_AIRPORT_TABLE F1                                 
               INNER JOIN T05_FLIGHT_TABLE ON                           
               DEPARTURE_AIRPORT_CODE = F1.AIRPORT_CODE                 
               INNER JOIN T02_AIRPORT_TABLE F2 ON                       
                ARRIVAL_AIRPORT_CODE = F2.AIRPORT_CODE                  
              WHERE           
               FLIGHT_ID = :T05-FLIGHT-ID                               
           END-EXEC                                                     
           DISPLAY 'F1 AIRPORT FULL NAME: ' WS-FIRST-AIRPORT-NAME-TEXT  
           DISPLAY 'F2 AIRPORT FULL NAME: ' WS-SECOND-AIRPORT-NAME-TEXT 
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
           DISPLAY 'SQLCODE NOT NORMAL '                                
               SET SO-7009-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-IF                                                       
           DISPLAY 'SQLCODE NORMAL '                                    
           .                                                            
      ******************************************************************
      *                     7010-GET-TIMEZONE                           
      ******************************************************************
       7010-GET-TIMEZONE.                                               
           EXEC SQL                                                     
            SELECT TIME_ZONE2                                           
             INTO :T02-TIME-ZONE2                                       
            FROM T02_AIRPORT_TABLE WHERE                                
              AIRPORT_CODE = :T02-AIRPORT-CODE                          
              FETCH FIRST ROW ONLY                                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7010-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7011-ARV-TIME-TO-LOCAL-ZONE                     
      ******************************************************************
       7011-ARV-TIME-TO-LOCAL-ZONE.                                     
           EXEC SQL                                                     
            SELECT CHAR(TIMESTAMPADD(4,:WS-MINUTE-OFFSET,               
            ZMIENNA))       
            INTO :WS-MODIFIED-TIMESTAMP                                 
            FROM                                                        
            (SELECT TIMESTAMPADD(8,:WS-HOUR-OFFSET,                     
            ARRIVAL_TIMESTAMP)                                          
              AS  ZMIENNA                                               
            FROM  T05_FLIGHT_TABLE                                      
            WHERE FLIGHT_ID = :T05-FLIGHT-ID )                          
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7011-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7012-DEPARTURE-TIME-TO-UTC                   
      * PROGRAM WILL MOVE GIVEN DEPARTURE TIMESTAMP FROM UTC TO         
      * LOCAL AIRPORT TIME ZONE                                         
      ******************************************************************
       7012-DEPARTURE-TIME-TO-UTC.                                      
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
      *                       9000-DB2-ERROR                            
      ******************************************************************
       9100-ROLLBACK.                        
           EXEC CICS                         
              SYNCPOINT ROLLBACK             
           END-EXEC                          
           PERFORM 2200-CHECK-EIBRESP        
           .                                 
                                                   
                                            
                                          
                                                  
                                     
                                           
                             
                                                  
                         

         
                     
                        
                    
                  
          

                                                   
             
       
                                           
                                            
   
             
                                
                                        
                      
