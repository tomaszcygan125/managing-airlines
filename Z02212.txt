       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02212.                                              
      ******************************************************************
      *                Z02212  (0216)                                   
      *                                                                 
      * PROGRAM CAN BE CALLED FROM TWO PLACES IN CODE                   
      *                                                                 
      *  1. FROM PROGRAM Z02202 ( THERE USER HAD TO PROVIDE PASSENGERS  
      *  DATA) -> THIS PROGRAM (Z02212) WILL INSERT RESERVATION DATA    
      * INTO THE THE TABLES AND WILL DISPLAY THEM TO THE USER           
      * USER WILL HAVE TWO CHOICES 1. PRESS F3 AND DROP THE RESERVATION 
      *                       2. PRESS ENTER AND CONFIRM THE RESERVATION
      * AFTER PRESSING F3 OR ENTER PROGRAM WILL END                     
      *                                                                 
      *                                                                 
      * 2. FROM PROGRAM Z02312( THERE USER HAVE TO CHOOSE WHAT          
      * SCHEDULE HE WANTS TO DELETE) IF USER WILL PLACE 'D' NEXT TO     
      * SCHEDULE NAME AND WILL PRESS ENTER THEN CONTROL WILL BE         
      * PASSED TO THIS PROGRAM.                                         
      * PROGRAM WILL ALSO GET ID OF THIS SCHEDULE, BY USING THIS        
      * VALUE PROGRAM WILL GET DETAILS ABOUT THIS SCHEDULE AND WILL     
      * DISPLAY THAT ON THE SCREEN, USER WILL BE ONLY ALLOWED TO        
      * PRESS F3 AND RETURN TO CALING PROGRAM(Z02312)                   
      *                                                                 
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
           COPY ZZMP0221.                                               
           COPY ZZEC0215.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                           
           EXEC SQL INCLUDE T05TAB END-EXEC.                           
           EXEC SQL INCLUDE T06TAB END-EXEC.                           
           EXEC SQL INCLUDE T09TAB END-EXEC.                           
           EXEC SQL INCLUDE T10TAB END-EXEC.                           
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
       01 CT-CONSTANTS.                                                
            05 CT-CALLING-PROGRAM-NAME     PIC X(8) VALUE 'Z02202  '.  
            05 CT-THIS-PROGRAM-NAME        PIC X(8) VALUE 'Z02212  '.  
            05 CT-ERROR-ROUTINE-NAME       PIC X(8) VALUE 'Z02141  '.  
            05 CT-FIRST-PROG-NAME          PIC X(8) VALUE 'Z02131  '.  
            05 CT-SCHEDULE-PROGRAM         PIC X(8) VALUE 'Z02312  '.  
            05 CT-CHOICE-PROG              PIC X(8) VALUE 'Z02152  '.  
            05 CT-CONFIRMED-STATUS.                                    
               49 CT-CONFIRMED-STATUS-LEN  PIC S9(4) COMP VALUE 9.     
               49 CT-CONFIRMED-STATUS-TEXT PIC X(15) VALUE 'CONFIRMED'.
            05 CT-MAXIMAL-NUMBER-OF-ROWS   PIC S9(4) COMP VALUE 20.     
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME             PIC X.          
               88 SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'.  
               88 SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'.  
               88 SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'.  
           05 SW-WHAT-TYPE-OF-END                           PIC X.      
               88 SO-FINAL-WITH-COMMAREA                    VALUE '1'.  
               88 SO-FINAL-TERMINATION                      VALUE '2'.  
           05 SW-WHERE-TO-GO                                PIC X.      
               88 SO-GO-BACK-TO-THIS                        VALUE '1'.  
               88 SO-GO-BACK-TO-Z02152                      VALUE '2'.  
               88 SO-GO-BACK-TO-BEGINING                    VALUE '3'.  
       01 WS-VARIABLES.                                                 
           05 WS-ITER1                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER2                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER3                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER4                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER5                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER6                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER7                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER8                          PIC S9(4) COMP VALUE 0. 
           05 WS-ITER9                          PIC S9(4) COMP VALUE 0. 
           05 WS-RESERVATION-FORMAT             PIC X(10).              
           05 WS-TEMP-STRING                    PIC X(79) VALUE SPACE.  
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02212-------------START'                           
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02212-------------END  '                           
           PERFORM 3000-FINAL                                           
           .                                                            
      ***************************************************************** 
      *                          1000-INIT         
      ***************************************************************** 
       1000-INIT.                                                       
           SET SO-GO-BACK-TO-THIS TO TRUE                               
           PERFORM  1005-CHECK-IF-FIRST-TIME                            
           .                                                            
      ***************************************************************** 
      *                 1005-CHECK-IF-FIRST-TIME                        
      * PROGRAM CAN HAVE 1 OUT OF 3 MODES                               
      *                                                                 
      *  1.  SO-M-FIRST-WITHOUT PROGRAM WAS CALLED FOR THE FIRST TIME   
      *  ( NO DATA WAS PREVIOUSLY GENERATED BY THE PROGRAM)             
      *                                                                 
      *  2.  SO-M-FIRST-WITH -  PROGRAM IS CALLED FOR THE FIRST TIME    
      * BUT IT ALREADY GENERATED SOME DATA BEFORE (IT WAS RUNNING       
      * WAS STOPPED AND IS RUNNING AGAIN)                               
      *  IN THIS MODE WE WILL JUST DISPLAY DATA THAT WERE PREVIOUSLY    
      * GENERATED                                                       
      *                                                                 
      *  3.  SO-M-NOT-FIRST - PROGRAM WAS RETRIGGERED WHEN USER PRESSED 
      * AN ATTENTION KEY                                                
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
           SET SO-GO-BACK-TO-THIS TO TRUE                               
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
      ******************************************************************
      *                     2001-PROCESS-FIRST-TIME                     
      * IN CASE PROGRA RUNS IN ORDER TO DISPLAY RESERVATION DETAILS:    
      *   PARAGRAPH WILL GET MAIN PASENGER LAST NAME FROM THE DATABASE  
      *  THEN IT WILL ADD IT TO T09 T09_RESERVATION_MAIN_PASSENGER_TABLE
      *   THEN IT WITH GET RESERVATION ID ( NUMBER WILL BE CREATED WHILE
      *   INSERTING USER LAST NAME)                                     
      *   PARAGRAPH WILL ALSO INSERT DATA INTO T04_FLIGHT_SEATS AND     
      *   T12_RESERVATION_PASSENGERS                                    
      *                                                                 
      *   THIS 3 TABLES (T04,T09,T12) WILL STORE ALL INFORMATIONS ABOUT 
      *   THE RESERVATION                                               
      *                                                                 
      *   AFTER ALL OF THIS DATA WILL BE INSERTED INTO THE DATABASE     
      *   USER WILL HAVE TO CHOOSE IF HE CONFIRM THIS RSERVATION OR IF  
      *   HE WANTS TO DELETE THIS                                       
      *                                                                 
      *   IF HE WANTS TO DELETE (DROP) THE RESERVATION WE WILL REMOVE   
      *   PREVIOUSLY INSERTED DATA FROM THE DATABASE                    
      *                                                                 
      *   IF HE WILL CONFIRM THIS RESERVATION WE WILL ISSUE SYNCPOINT   
      *   COMMIT STATEMENT                                              
      * IN CASE PROGRAM RUNS IN ORDER TO DISPLAY SCHEDULE DETAILS:      
      *   PROGRAM WILL ISSUE SELECT SQL STATEMENT THAT WILL GET         
      *   ALL IMPORTANT INFORMATION ABOUT THE SCHEDULE THIS INFO WILL   
      *   BE THEN PUT IN SCREEN VARIABLES AND WILL BE DISPLAYED ON      
      *   THE SCREEN                                                    
      *                                                                 
      ******************************************************************
       2001-PROCESS-FIRST-TIME.                                         
           PERFORM 2306-INITIALIZE-SCREEN-DATA                          
           IF SO-DISPLAY-RESERVATION  THEN                              
      * ID OF PASSENGER BELOW IS ID OF A MAIN PASSENGER OF THIS         
      * RESERVATION PROGRAM WILL USE THAT ID IN ORDER TO GET HIS        
      * LAST NAME                                                       
             MOVE Z02192-PASSENGER-ID(1, 1) TO                          
                    PASSENGER-ID                                        
             DISPLAY '2001 PASSENGER ID: ' PASSENGER-ID                 
             PERFORM 7001-GET-PASSENGER-LAST-NAME                       
             PERFORM 7002-INSERT-RESERV-LAST-NAME                       
             PERFORM 7003-GET-RESERVATION-NUMBER                        
      * THIS PARAGRAPH WILL INSERT INTO T04_FLIGHT_TABLE                
      * SEAT ROW AND LETTER, RESERVATION ID AND PASSENGER ID            
             PERFORM 7004-INSERT-PASSENGER-DATA                         
      * THIS PARAGRAPH                                                  
      * WILL ADD PASSENGERS ID TO THE RESERVATION ID                    
             PERFORM 7006-INSERT-INTO-T12-TABLE                         
             PERFORM 2101-PREPARE-SCREEN-VARIABLES                      
             PERFORM 2100-SEND-THE-MAP                                  
           ELSE                                                         
      * IF PROGRAM LOGIC IS HERE IT MEANS THAT WE SHOULD                
      * DISPLAY RESERVATION DETAILS                                     
             PERFORM 2102-INIT-SCREEN-VARIABLES                         
             PERFORM 7011-GET-SCHEDULE-DETAILS                          
             PERFORM 2302-DISPLAY-SCHEDULE-DETAILS                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                       2002-PROCESS-WITH-DATA                    
      * PROGRAM IS CALLED ONLY WHEN PROGRAM ALREADY GENEREDTED SOME     
      * DATA                                                            
      * NOW WE HAVE TO JUST DIPSLAY THIS DATA                           
      * THEN USER WILL BE ABLE TO CHOOSE WHAT HE WANTS TO DO            
      * F3 - TO DROP RESERVATION                                        
      * ENTER - TO CONFIRM THE RESERVATION                              
      ******************************************************************
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2301-MOVE-DATA-TO-SCREEN                             
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                       2003-PROCESS-NOT-FIRST-TIME               
      * HERE WE WILL CHECK WHAT USER HAVE PRESSED                       
      *                                                                 
      *   IF HE PRESSED ENTER THEN IT MEANS HE CONFIRMS THIS RESERVATION
      *    SYNCPOINT COMMAND WILL BE PERFORMED                          
      *                                                                 
      *   IF PRESSES F3 BUTTON IT MEANS THAT RESERVATION DATA           
      *  SHOULD BE REMOVED FROM THE DATABASE                            
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           DISPLAY '2003 PERFORMED   '                                  
           EVALUATE EIBAID                                              
           WHEN DFHENTER                                                
              IF SO-DISPLAY-RESERVATION THEN                            
                SET SO-FINAL-TERMINATION TO TRUE                        
                SET SO-GO-BACK-TO-Z02152 TO TRUE                        
                PERFORM 2105-FINAL-AND-COMMIT                           
              ELSE                                                      
      * IN CASE THAT PROGRAM WAS CALLED IN ORDER TO DISPLAY SCHEDULE    
      * DETAILS AND USER WILL PRESS ENTER HE WILL BE NOTIFIED THAT      
      * THERE IS NO ACTION ASSIGNED TO THAT KEY                         
      *                                                                 
                PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
                MOVE 'NO ACTION KEY  ' TO   WS-Z02141-I-ERROR-MESSAGE(1)
                SET     SO-Z02141-M-WITH TO TRUE                        
                PERFORM 2300-CALL-ERROR-ROUTINE                         
              END-IF                                                    
           WHEN DFHPF3                                                  
              SET SO-FINAL-TERMINATION TO TRUE                          
              IF SO-DISPLAY-RESERVATION THEN                            
                SET SO-GO-BACK-TO-Z02152 TO TRUE                        
                PERFORM 2104-FINAL-AND-ROLLBACK                         
              ELSE                                                      
      * IN CASE THAT THIS PROGRAM WAS CALLED IN ORDER TO DISPLAY        
      * RESERVATION DETAILS, USER CAN GO BACK TO CALLING PROGRAM        
      * BY PRESSING F3 ( WE DON'T HAVE TO CODE NOTHING MORE PROGRAM     
      * WILL ISSUE 3000-FINAL PARAGRAPH AND WILL END THIS PROGRAM)      
                CONTINUE                                                
              END-IF                                                    
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID KYE   ' TO   WS-Z02141-I-ERROR-MESSAGE(1)   
              SET     SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      2100-SEND-THE-MAP                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
            SEND MAP('MP0221') MAPSET('MP0221')                         
            FROM(MP0221O)                                               
            ERASE                                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2101-PREPARE-SCREEN-VARIABLES                 
      * PARARGRAPH WILL DISPLAY ALL NEEDED DATA TO THE USER             
      *                                                                 
      * ALSO ALL DATA WILL BE SAVED INTO THE COMMAREA                   
      * SO IN CASE THAT USER HAVE TO COME BACK TO THIS PROGRAM          
      * WE WON'T HAVE TO PREPARE THOSE DATA ONCE AGAIN                  
      ******************************************************************
       2101-PREPARE-SCREEN-VARIABLES.                                   
           PERFORM 2102-INIT-SCREEN-VARIABLES                           
           MOVE WS-OUR-RESERVATION-ID TO WS-RESERVATION-FORMAT          
                                                                        
           STRING                                                       
              'YOUR RESERVATION NUMBER '                                
              WS-RESERVATION-FORMAT                                     
              DELIMITED BY SIZE                                         
              INTO POLEO(1)                                             
           END-STRING                                                   
                                                                        
           MOVE 'MAIN PASSENGER LAST NAME: ' TO POLEO(2)                
           MOVE T09-MAIN-P-LAST-NAME-TEXT TO POLEO(3)                   
           MOVE 'FLIGHTS IN RESERVATION: ' TO POLEO(4)                  
           MOVE 5 TO WS-ITER4                                           
                                                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 >        
                              Z02192-NUMBER-OF-FLIGHTS                  
                MOVE Z02192-SEAT-ACT-FLG-NUMBER(WS-ITER5) TO            
                     POLEO(WS-ITER4)                                    
                ADD 1 TO WS-ITER4                                       
           END-PERFORM                                                  
           PERFORM 2304-SAVE-SCREEN-TO-COMMAREA                         
           .                                                            
      ******************************************************************
      *                 2102-INIT-SCREEN-VARIABLES                      
      ******************************************************************
       2102-INIT-SCREEN-VARIABLES.                                      
           MOVE LOW-VALUES TO MP0221O                                   
           PERFORM VARYING WS-ITER6 FROM 1 BY 1 UNTIL WS-ITER6 > 20     
               MOVE LOW-VALUES TO POLEA(WS-ITER6)   
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2104-FINAL-AND-ROLLBACK.                     
      * DATA WAS ALREADY INSERTED INTO DATABASE                         
      * SO NOW WE NEED TO USER 'DELETE' SQL STATEMENT TO GET RID OF IT  
      ******************************************************************
       2104-FINAL-AND-ROLLBACK.                                         
           PERFORM 2106-DELETE-FROM-TABLES                              
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'RESERVATION  DROPPED '  TO                             
                      WS-Z02141-I-ERROR-MESSAGE(1)                      
           SET    SO-Z02141-M-WITH TO TRUE                              
           SET    SO-GO-BACK-TO-Z02152   TO TRUE                        
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ****************************************************************  
      *                     2105-FINAL-AND-COMMIT                       
      ****************************************************************  
       2105-FINAL-AND-COMMIT.                                           
           PERFORM 9200-COMMIT                                          
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'RESERVATION  SAVED '  TO                               
                      WS-Z02141-I-ERROR-MESSAGE(1)                      
           SET    SO-Z02141-M-WITH TO TRUE                              
           SET    SO-GO-BACK-TO-Z02152   TO TRUE                        
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ****************************************************************  
      *               2106-DELETE-FROM-TABLES                           
      ****************************************************************  
       2106-DELETE-FROM-TABLES.                                         
           PERFORM 7008-DELETE-FROM-T09-TAB                             
           PERFORM 7009-DELETE-FROM-T12-TAB                             
           PERFORM 7010-DELETE-FROM-T04-TAB                             
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
             MOVE CT-CHOICE-PROG TO WS-Z02141-I-CALLING-PROGRAM         
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
      *                   2301-MOVE-DATA-TO-SCREEN                      
      * PROGRAM MOVES DATA THAT WERE SAVED TO COMMAREA TO SCREEN        
      * VARIABLES                                                       
      * THEN PROGRAM CAN DISPLAY THOSE DATA TO THE USER                 
      ******************************************************************
       2301-MOVE-DATA-TO-SCREEN.                                        
           PERFORM VARYING WS-ITER4 FROM 1 BY 1 UNTIL WS-ITER4 >        
                                      CT-MAXIMAL-NUMBER-OF-ROWS         
              MOVE WS-Z02212-SCREEN-DATA(WS-ITER4) TO                   
                       POLEO(WS-ITER4)                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2302-DISPLAY-SCHEDULE-DETAILS                 
      * PARAGRAPH WILL CONCATENATE USER MASSGE WITH SCHEDULE DATA       
      * IN A SCREEN VARIABLE                                            
      *                                                                 
      * THIS VARIABLES WILL BE USED IN ORDER TO DISPLAY DATA ON THE     
      * SCREEN                                                          
      *                                                                 
      ******************************************************************
       2302-DISPLAY-SCHEDULE-DETAILS.   
           PERFORM 2303-DATA-TO-SCREEN-VARIABLES                        
           PERFORM 2304-SAVE-SCREEN-TO-COMMAREA                         
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                   2303-DATA-TO-SCREEN-VARIABLES                 
      * PARAGRAPH WILL CONCATENATE MESSAGE FOR USER WITH USER DATA      
      ******************************************************************
       2303-DATA-TO-SCREEN-VARIABLES.                                   
           PERFORM 2307-STRING-FLIGHT-TO-NUMBER                         
           PERFORM 2308-STRING-FLIGHT-FROM-NUMBER                       
           PERFORM 2309-STRING-AIRPORT-ORIGIN                           
           PERFORM 2310-STRING-AIRPORT-DESTIN                           
           PERFORM 2311-STRING-DEPARTURE-TIME-ORG                       
           PERFORM 2312-STRING-ARRIVAL-TIME-DEST                        
           PERFORM 2313-STRING-DEPART-TIME-DEST                         
           PERFORM 2314-STRING-ARRIVAL-TIME-ORG                         
           PERFORM 2315-STRING-START-SCHED-DATE                         
           PERFORM 2316-STRING-END-SCHEDULE-DATE                        
           PERFORM 2317-STRING-SCHEDULE-STATUS                          
           .                                                            
      ******************************************************************
      *                   2304-SAVE-SCREEN-TO-COMMAREA                  
      * THIS PARAGRAPH WILL SAVE DATA THAT WERE DISPLAYED ON THE SCREEN 
      * TO COMMAREA AND THANKS TO THAT IT WILL BE POSSIBLE TO GET THAT  
      * VALUES LATER                                                    
      *                                                                 
      * THIS VALUE WILL BE NEEDED IN ORDER NO TO PREPARE THIS DATA      
      * EVERYTIME                                                       
      * IF THIS IS POSSIBLE WE WILL GENERATE THOSE DATA ONE TIME NAD    
      * EVERY TIME WE WILL NEED TO DISPLAY THAT WE WILL USE             
      * DATA FROM COMMAREA                                              
      ******************************************************************
       2304-SAVE-SCREEN-TO-COMMAREA.                                    
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 > 20     
               MOVE POLEO(WS-ITER1) TO WS-Z02212-SCREEN-DATA(WS-ITER1)  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2306-INITIALIZE-SCREEN-DATA                   
      * PARAGRAPH WILL INITIALIZE SCEEN DATA (MAP DATA)                 
      * AND COMMAREA VARIABLES TAHT WILL LATER STORE THIS DATA          
      *                                                                 
      ******************************************************************
       2306-INITIALIZE-SCREEN-DATA.                                     
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 > 20     
               MOVE SPACE TO POLEO(WS-ITER1)                            
               MOVE SPACE TO WS-Z02212-SCREEN-DATA(WS-ITER1)            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2307-STRING-FLIGHT-TO-NUMBER                   
      ******************************************************************
       2307-STRING-FLIGHT-TO-NUMBER.                                    
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'FLIGHT NUMBER "TO":           ',                     
                   FLIGHT-NUMBER-TO-TEXT                                
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(1)                              
           .                                                            
      ******************************************************************
      *                  2308-STRING-FLIGHT-FROM-NUMBER                 
      ******************************************************************
       2308-STRING-FLIGHT-FROM-NUMBER.                                  
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'FLIGHT NUMBER "FROM":         ',                     
                   FLIGHT-NUMBER-FROM-TEXT                              
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING            
           MOVE WS-TEMP-STRING TO POLEO(2)                              
           .                                                            
      ******************************************************************
      *                  2309-STRING-AIRPORT-ORIGIN                     
      ******************************************************************
       2309-STRING-AIRPORT-ORIGIN.                                      
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'ORIGIN AIRPORT:               ',                     
                   ORIGIN-AIRPORT-CODE                                  
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(3)                              
           .                                                            
      ******************************************************************
      *                  2310-STRING-AIRPORT-DESTIN                     
      ******************************************************************
       2310-STRING-AIRPORT-DESTIN.                                      
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'DESTINATION AIRPORT:          '                      
                   DESTINATION-AIRPORT-CODE                             
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(4)                              
           .                                                            
      ******************************************************************
      *                  2311-STRING-DEPARTURE-TIME-ORG                 
      ******************************************************************
       2311-STRING-DEPARTURE-TIME-ORG.                                  
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'TIME OF DEPARTURE UTC:        '                      
                   DEPARTURE-TIME-ORIGIN                                
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING    
           MOVE WS-TEMP-STRING TO POLEO(5)                              
           .                                                            
      ******************************************************************
      *                  2312-STRING-ARRIVAL-TIME-DEST                  
      ******************************************************************
       2312-STRING-ARRIVAL-TIME-DEST.                                   
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'TIME OF ARRIVAL   UTC:        '                      
                   ARRIVAL-TIME-DESTINATION                             
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(6)                              
           .                                                            
      ******************************************************************
      *                  2313-STRING-DEPART-TIME-DEST                   
      * PARAGRAPH WILL STRING TIME OF DEPARTURE FROM DESTINATION        
      * AIRPORT                                                         
      * FOR EXAMPEL                                                     
      *          WAW  ---- > KRK                                        
      *          KRK  ---->  WAW                                        
      *                                                                 
      * TIME OF DEPARTURE FROM KRAKOW WILL BE OUR DEPARTURE FROM        
      * DESTINATION                                                     
      ******************************************************************
       2313-STRING-DEPART-TIME-DEST.                                    
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'TIME OF SECOND DEPARTURE UTC: '                      
                   DEPARTURE-TIME-DESTINATION                           
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(7)                              
           .                                                            
      ******************************************************************
      *                  2314-STRING-ARRIVAL-TIME-ORG                   
      ******************************************************************
       2314-STRING-ARRIVAL-TIME-ORG.                                    
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'TIME OF SECOND ARRIVAL UTC:   '                      
                   ARRIVAL-TIME-ORIGIN                                  
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(8)                              
           .                                                            
      ******************************************************************
      *                  2315-STRING-START-SCHED-DATE                   
      * PARAGRAPH WILL PREAPARE DATE OF START OF THE SCHEDULE           
      ******************************************************************
       2315-STRING-START-SCHED-DATE.                                    
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'SCHEDULE STARTS:              '                      
                   START-SCHEDULE-DATE                                  
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(9)                              
           .                                                            
      ******************************************************************
      *                  2316-STRING-END-SCHEDULE-DATE                  
      ******************************************************************
       2316-STRING-END-SCHEDULE-DATE.                                   
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'SCHEDULE ENDS:                '                      
                   END-SCHEDULE-DATE                                    
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(10)                             
           .                                                            
      ******************************************************************
      *                  2317-STRING-SCHEDULE-STATUS                    
      ******************************************************************
       2317-STRING-SCHEDULE-STATUS.                                     
           MOVE SPACE TO WS-TEMP-STRING                                 
           STRING 'SCHEDULE STATUS:              '                      
                   SCHEDULED-STATUS-TEXT                                
           DELIMITED BY SIZE                                            
           INTO WS-TEMP-STRING                                          
           END-STRING                                                   
           MOVE WS-TEMP-STRING TO POLEO(11)                             
           .                                                            
      ******************************************************************
      *                   2400-INITIALIZE-ERROR-MESSAGE                 
      ******************************************************************
       2400-INITIALIZE-ERROR-MESSAGE.                                   
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 10     
             MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER2)          
           END-PERFORM                                                  
           .                                                            
      ***************************************************************** 
      *                          3000-FINAL                             
      ***************************************************************** 
       3000-FINAL.                                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3001-RETURN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
              IF SO-DISPLAY-RESERVATION THEN                            
                PERFORM 3002-GOBACK-TO-FIRST-PROG                       
              ELSE                                                      
      * WE WILL BE HERE IF PROGRAM DISPLAYED SCHEDULE DATA              
                PERFORM 3003-GOBACK-TO-SCHEDULE-PROG                    
              END-IF                                                    
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO   WS-Z02141-I-ERROR-MESSAGE(1)  
              SET     SO-Z02141-M-WITH TO TRUE                          
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  3001-RETURN-WITH-TRANSID                       
      ******************************************************************
       3001-RETURN-WITH-TRANSID.                                        
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           DISPLAY 'RETURN WITH 0216'                                   
           EXEC CICS                                                    
            RETURN TRANSID('0216') COMMAREA(DFHCOMMAREA)                
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   3002-GOBACK-TO-FIRST-PROG                     
      *  THIS PARAGRAPH WILL RETURN CONTROL TO CALLING PROGRAM          
      * NOT LIKE OTHER PROGRAMS THIS PROGRAM (THE FIRST ONE) DOESN'T    
      * USE COMMAREA FLAGS TO DETERMINE IF IT WAS CALLED FOR THE FIRST  
      * TIME OR NOT                                                     
      *                                                                 
      * HERE WE WILL HAVE TO CALL THAT PROGRAM WITH LENGTH(0) AND       
      * THANKS TO THAT PROGRAM WILL RUN CORRECTLY                       
      ******************************************************************
       3002-GOBACK-TO-FIRST-PROG.                                       
           SET SO-M-FIRST-WITH   TO TRUE                                
           EXEC CICS                                                    
            XCTL PROGRAM(CT-FIRST-PROG-NAME)                            
            COMMAREA(DFHCOMMAREA) LENGTH(0)                             
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 3003-GOBACK-TO-SCHEDULE-PROG                    
      * PARAGRAPH WILL RETURN CONTROL TO CALLING PROGRAM Z02312         
      *  ( THIS IS THE PROGRAM THAT ALLOWS USER TO SEE  ALL SCHEDULES   
      * IN ORDER TO DELETE ONE OF THEM )                                
      ******************************************************************
       3003-GOBACK-TO-SCHEDULE-PROG.                                    
           SET SO-M-FIRST-WITHOUT   TO TRUE                             
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-SCHEDULE-PROGRAM)                           
            COMMAREA(DFHCOMMAREA)                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 7001-GET-PASSENGER-LAST-NAME                    
      ******************************************************************
       7001-GET-PASSENGER-LAST-NAME.                                    
           EXEC SQL                                                     
           SELECT                                                       
              PASSENGER_LAST_NAME                                       
           INTO                                                         
              :PASSENGER-LAST-NAME                                      
           FROM T06_PASSENGERS_TABLE                                    
           WHERE PASSENGER_ID = :PASSENGER-ID                           
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'PO 7001 SQLCODE: ' WS-SQLCODE-FORMAT                
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN OTHER                                                   
      * SQLCODE OTHER THAN 000 MEANS THAT WE HAVE SERIOUS ERROR         
      * AND TRANSACTION NEED TO BE STOPPED                              
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *               7002-INSERT-RESERV-LAST-NAME                      
      ******************************************************************
       7002-INSERT-RESERV-LAST-NAME.                                    
           MOVE PASSENGER-LAST-NAME TO T09-MAIN-PASSENGER-LAST-NAME     
                                                                        
           EXEC SQL                                                     
           INSERT INTO T09_RESERVATION_MAIN_PASSENGER_TABLE(            
                                         MAIN_PASSENGER_LAST_NAME)      
                  VALUES(:T09-MAIN-PASSENGER-LAST-NAME)                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN OTHER                                                   
              SET SO-7002-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 7003-GET-RESERVATION-NUMBER                     
      * THIS QUERY WILL RETURN TO THE PROGRAM ID OF TRANSACTION THAT    
      * WAS ADDED A MOMENT BEFORE, THERE WILL BE NO OTHER OPTION        
      * TO ADD A RESERVATION SA THE LARGEST NUMBER WILL BE OUR          
      * PREVIOUSLY ADDED                                                
      ******************************************************************
       7003-GET-RESERVATION-NUMBER.                                     
           EXEC SQL                                                     
            SELECT RESERVATION_ID                                       
            INTO                                                        
             :WS-OUR-RESERVATION-ID                                     
            FROM                                                        
             T09_RESERVATION_MAIN_PASSENGER_TABLE      
            ORDER BY RESERVATION_ID DESC                                
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               CONTINUE                                                 
           WHEN OTHER                                                   
               SET SO-7003-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                7004-INSERT-PASSENGER-DATA                       
      * PARAGRAPH WILL PREPARE DATA THAT WILL BE LATER INSERTED INTO    
      * T04_FLIGHT_SEATS TABLE                                          
      ******************************************************************
       7004-INSERT-PASSENGER-DATA.                                      
           MOVE WS-OUR-RESERVATION-ID TO T04-RESERVATION-ID             
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL                   
                       WS-ITER1 > Z02192-NUMBER-OF-FLIGHTS              
             MOVE Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1) TO                
                        T04-FLIGHT-ID-TEXT                              
             COMPUTE T04-FLIGHT-ID-LEN =                                
              FUNCTION LENGTH(Z02192-SEAT-FLIGHT-NUMBER(WS-ITER1))      
                                                                        
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL                 
                       WS-ITER2 > Z02192-TICKET-NUMBER(WS-ITER1)        
               MOVE Z02192-PASSENGER-ID(WS-ITER1, WS-ITER2) TO          
                  T04-PASSENGER-ID                                      
               MOVE Z02192-SEAT-ROW-POSITION(WS-ITER1, WS-ITER2)        
                    TO T04-ROW-NUMBER                                   
               MOVE Z02192-SEAT-LETTER(WS-ITER1, WS-ITER2)              
                    TO T04-SEAT-LETTER                                  
                                                                        
               PERFORM 7005-INSERT-TO-T04  
             END-PERFORM                                                
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                       7005-INSERT-TO-T04                        
      * PARAGRAPH WILL INSERT DATA INTO                                 
      *  T04_FLIGHT_SEATS TABLE                                         
      ******************************************************************
       7005-INSERT-TO-T04.                                              
           EXEC SQL                                                     
             INSERT INTO                                                
             T04_FLIGHT_SEATS(FLIGHT_ID,                                
                              RESERVATION_ID,                           
                              PASSENGER_ID,                             
                              ROW_NUMBER,                               
                              SEAT_LETTER,                              
                              RESERVATION_STATUS)                       
                      VALUES(:T04-FLIGHT-ID,                            
                             :T04-RESERVATION-ID,                       
                             :T04-PASSENGER-ID,                         
                             :T04-ROW-NUMBER,                           
                             :T04-SEAT-LETTER,                          
                             :CT-CONFIRMED-STATUS)                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               CONTINUE                                                 
           WHEN OTHER                                                   
               SET SO-7005-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  7006-INSERT-INTO-T12-TABLE                     
      ******************************************************************
       7006-INSERT-INTO-T12-TABLE.                                      
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 >        
                           Z02192-TICKET-NUMBER(1)                      
               MOVE Z02192-PASSENGER-ID(1, WS-ITER3)                    
                     TO T12-PASSENGER-ID                                
               MOVE  WS-OUR-RESERVATION-ID TO T12-RESERVATION-ID        
               PERFORM 7007-INSERT-INTO-T12                             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      7007-INSERT-INTO-T12                       
      ******************************************************************
       7007-INSERT-INTO-T12.                                            
           EXEC SQL                                                     
            INSERT INTO                                                 
             T12_RESERVATION_PASSENGERS(RESERVATION_ID,                 
                                        PASSENGER_ID)                   
                                 VALUES(:T12-RESERVATION-ID,            
                                        :T12-PASSENGER-ID)              
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN OTHER                                                   
              SET SO-7007-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7008-DELETE-FROM-T09-TAB                   
      *                                                                 
      ******************************************************************
       7008-DELETE-FROM-T09-TAB.                                        
           EXEC SQL                                                     
            DELETE FROM T09_RESERVATION_MAIN_PASSENGER_TABLE         
            WHERE                                                       
               RESERVATION_ID = :WS-OUR-RESERVATION-ID                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
              SET SO-7008-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7009-DELETE-FROM-T12-TAB                   
      ******************************************************************
       7009-DELETE-FROM-T12-TAB.                                        
           EXEC SQL                                                     
            DELETE FROM T12_RESERVATION_PASSENGERS                      
            WHERE                                                       
               RESERVATION_ID = :WS-OUR-RESERVATION-ID                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
              SET SO-7009-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7010-DELETE-FROM-T04-TAB                   
      ******************************************************************
       7010-DELETE-FROM-T04-TAB.                                        
           EXEC SQL                                                     
            DELETE FROM T04_FLIGHT_SEATS                                
            WHERE                                                       
               RESERVATION_ID = :WS-OUR-RESERVATION-ID                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL                                     
              SET SO-7010-PARA TO TRUE     
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7011-GET-SCHEDULE-DETAILS                      
      * PARAGRAPH WILL BE USED TO GET DETAILS OF THE SCHEDULE           
      ******************************************************************
       7011-GET-SCHEDULE-DETAILS.                                       
           MOVE WS-Z02312-SCHEDULE-ID TO SCHEDULED-FLIGHT-ID            
                                                                        
           EXEC SQL                                                     
            SELECT                                                      
                   FLIGHT_NUMBER_TO                                     
                  ,FLIGHT_NUMBER_FROM                                   
                  ,DEPARTURE_TIME_ORIGIN                                
                  ,ORIGIN_AIRPORT_CODE                                  
                  ,ARRIVAL_TIME_DESTINATION                             
                  ,DESTINATION_AIRPORT_CODE                             
                  ,DEPARTURE_TIME_DESTINATION                           
                  ,ARRIVAL_TIME_ORIGIN                                  
                  ,WEEK_DAYS                                            
                  ,PLANE_ID                                             
                  ,START_SCHEDULE_DATE                                  
                  ,END_SCHEDULE_DATE                                    
                  ,SCHEDULED_STATUS                                     
           INTO                                                         
                   :FLIGHT-NUMBER-TO                                    
                  ,:FLIGHT-NUMBER-FROM                                  
                  ,:DEPARTURE-TIME-ORIGIN                               
                  ,:ORIGIN-AIRPORT-CODE                                 
                  ,:ARRIVAL-TIME-DESTINATION                            
                  ,:DESTINATION-AIRPORT-CODE                            
                  ,:DEPARTURE-TIME-DESTINATION                          
                  ,:ARRIVAL-TIME-ORIGIN                                 
                  ,:WEEK-DAYS                                           
                  ,:PLANE-ID      
                  ,:START-SCHEDULE-DATE                                 
                  ,:END-SCHEDULE-DATE                                   
                  ,:SCHEDULED-STATUS                                    
           FROM                                                         
                T10_SCHEDULED_FLIGHTS_TABLE                             
           WHERE                                                        
               SCHEDULED_FLIGHT_ID = :SCHEDULED-FLIGHT-ID               
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              CONTINUE                                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'DATA ERROR' TO WS-Z02141-I-ERROR-MESSAGE(1)         
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
              SET SO-7011-PARA TO TRUE                                  
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
      ******************************************************************
      *                       9200-COMMIT                               
      ******************************************************************
       9200-COMMIT.                                                     
           EXEC CICS                                                    
             SYNCPOINT                                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP      
           .                             
                                      
                             
   
                             
                 
                               

 


                                               
                                       

                                
                                     

                    


                         
                    
                     

