       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02192.                                              
      ******************************************************************
      *                 Z02192  (0213)                                  
      *                                                                 
      * PROGRAM WILL DISPLAY GRAFICAL SEATS REPRESENTATION FOR          
      *  ALL FLIGHTS CHOSEN BY THE USER ONE BY ONE TILL END OF          
      *  FLIGHTS. USER WILL HAVE TO CHOOSE AS MANY SEATS ON EACH OF THIS
      * REPRESENATIONS AS THE NUMBER HE SPECIFIED IN Z02152 PROGRAM     
      *                                                                 
      * IF USER WILL SUCCESSFULLY CHOOSE ALL THAT SEATS                 
      *  PROGRAM WILL CALL TO Z02202 PROGRAM                            
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
      *                          CHANGE LOG                             
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0219.                                               
           COPY ZZEC0215.                                               
           COPY DFHBMSCA.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T13TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T08TAB END-EXEC.                            
                                                                        
      * THIS CURSOR WILL FETCH ONE BY ONE ALL TAKEN SEATS IN THAT THE   
      * GIVEN FLIGHT                  
           EXEC SQL                                                     
            DECLARE C-TAKEN-SEATS-CURSOR CURSOR                         
            FOR                                                         
            SELECT ROW_NUMBER,                                          
                   SEAT_LETTER,                                         
                   RESERVATION_ID                                       
            FROM                                                        
               T04_FLIGHT_SEATS                                         
            WHERE                                                       
               FLIGHT_ID = :T05-FLIGHT-ID                               
           END-EXEC                                                     
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
           05 CT-SEAT-IN-RESERVATION       PIC X VALUE 'R'.             
           05 CT-TAKEN-SEAT                PIC X VALUE '|'.             
           05 CT-MAXIMAL-NUMBER-OF-ROWS    PIC S9(4) COMP VALUE 20.     
           05 CT-CALLING-PROGRAM-NAME      PIC X(8) VALUE 'Z02172  '.   
           05 CT-THIS-PROGRAM-NAME         PIC X(8) VALUE 'Z02192  '.   
           05 CT-ERROR-ROUTINE-NAME        PIC X(8) VALUE 'Z02141  '.   
           05 CT-FIND-FLIGHT-PROGRAM       PIC X(8) VALUE 'Z02232  '.   
           05 CT-FIND-BOOKING-PROGRAM      PIC X(8) VALUE 'Z02261  '.   
           05 CT-PASSENGER-DATA            PIC X(8) VALUE 'Z02202  '.   
           05 CT-MAXIMAL-WIDTH-OF-REPR     PIC S9(4) COMP VALUE 79.     
           05 CT-MAXIMAL-HEIGHT-OF-REPR    PIC S9(4) COMP VALUE 20.     
       01 SW-SWITCHES.                                                  
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
           05 SW-IF-END-OF-CURSOR                            PIC X.     
               88 SO-END-OF-CURSOR-DATA                      VALUE '1'. 
               88 SO-NOT-END-OF-CURSOR-DATA                  VALUE '2'. 
           05 SW-IF-SEAT-IS-TAKEN                            PIC X.     
               88 SO-SEAT-IS-TAKEN                           VALUE '1'. 
               88 SO-SEAT-IS-NOT-TAKEN                       VALUE '2'. 
           05 SW-HOW-MANY-DIGITS-SEAT                        PIC X.     
               88 SO-2-DIGIT-NUMBER                          VALUE '2'. 
               88 SO-1-DIGIT-NUMBER                          VALUE '1'. 
           05 SW-WCHICH-FLIGHT                               PIC 9.     
               88 SO-1-FLIGHT                                VALUE 1.   
               88 SO-2-FLIGHT                                VALUE 2.   
               88 SO-3-FLIGHT                                VALUE 3.  
               88 SO-4-FLIGHT                                VALUE 4.  
               88 SO-5-FLIGHT                                VALUE 5.  
               88 SO-6-FLIGHT                                VALUE 6.  
               88 SO-7-FLIGHT                                VALUE 7.  
               88 SO-8-FLIGHT                                VALUE 8.  
       01 WS-VARIABLES.                                                
           05 WS-RESERVATION-ID                PIC S9(9) COMP VALUE 0. 
           05 WS-ITER1                         PIC S9(4) COMP VALUE 0. 
           05 WS-ITER2                         PIC S9(4) COMP VALUE 0. 
           05 WS-ITER3                         PIC S9(4) COMP VALUE 0. 
           05 WS-ITER4                         PIC S9(4) COMP VALUE 0. 
           05 WS-ITER10                        PIC S9(4) COMP VALUE 0. 
           05 WS-TEMP-VARIABLE                 PIC S9(4) COMP VALUE 0. 
           05 WS-SEAT-NUMBER                   PIC 99.                 
           05 WS-A                             PIC S9(9) COMP VALUE 0. 
           05 WS-B                             PIC X.                  
           05 WS-COUNT-OF-SEATS                PIC S9(4) COMP VALUE 0. 
           05 WS-ROW-NUMBER                    PIC 99.                 
           05 WS-TEMP-SEAT-LETTER              PIC S9(9) COMP.         
           05 WS-TEMP-SEAT-LETTER-A            PIC X.                  
           05 WS-TEMP-SEAT-ROW                PIC S9(9) COMP.          
           05 WS-SEATS-REPRESENTATION.                                 
              10 WS-SEATS-REPR-TABLE OCCURS 20 TIMES.                  
                 15 WS-SEATS-LINE PIC X(79).                           
           05 WS-USER-SEATS-DATA.                                      
              10 WS-SEATS-POSITIONS OCCURS 99 TIMES.                   
                 15 WS-SEAT-ROW-POS          PIC S9(4) COMP.           
                 15 WS-SEAT-LETTER-POS       PIC S9(4) COMP.           
           05 WS-TEMP-NUM     PIC 9(4) VALUE 0.                        
           05 WS-TEMP-STRING  PIC X(15) VALUE SPACE.                   
           05 WS-TEMP-STRING1 PIC X(4) VALUE SPACE.                    
           05 WS-ROW-NUMBER-ALPHA PIC X(2) VALUE SPACE.                
       LINKAGE SECTION.                                                
       01 DFHCOMMAREA PIC X(17294).                                    
       PROCEDURE DIVISION USING DFHCOMMAREA.    
           DISPLAY 'Z02192-----------------START'                       
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02192-----------------END'                         
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
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1015-SET-START-FLAGS                             
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
           SET SO-1-FLIGHT TO TRUE                                      
           SET SO-NOT-END-OF-CURSOR-DATA TO TRUE                        
           .                                                            
      ******************************************************************
      *                          2000-PROCESS                           
      * HERE PROGRAM CAN HAVE ONLY 2 VALID MODES                        
      * 1. THIS IS FIRST TIME PORGRAM RUNS                              
      * 2. THIS IS NOT FIRST TIME ( USER PRESSED KEY AND ACCORDINGLY    
      * TO THAT PROGRAM TAKES ACTION)                                   
      *                                                                 
      * IF THIS IS FIRST TIME PROGRAM RUNS PROGRAM WILL                 
      * DISPLAY GRAPHICAL REPRESENTATION OF THE SEATS ON THE PLANE      
      * AND WILL ALLOW USER TO MARK  SEATS OF HIS CHIOCE                
      * IF USER WILL PRESS ENTER THEN 2 SCENARIOS CAN BE TRUE:          
      *   1. THIS WAS ONLY FLIGHT  USER MAKES RESERVATION ON            
      *    IF THAT IS TRUE THEN CONTROL WILL GO TO THE NEXT PROGRAM     
      *     (Z02202)                                                    
      *   2. THERE IS MORE FLIGHTS USER MAKES RESRVATION ON             
      *                                                                 
      *    IF THIS IS TRUE THEN PROGRAM WILL DISPLAY GRAPHICAL          
      *    REPRESENTATION OF THE NEXT FLIGHT                            
      *    THIS PROCESS WILL CONTINUE TILL THE END OF THE FLIGHTS       
      *    IN THIS RESRVATION                                           
      ******************************************************************
       2000-PROCESS.                                                    
           EVALUATE TRUE                                                
           WHEN SO-PROGRAM-RUNS-FIRST-TIME                              
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2001-PROCESS-FIRST-TIME                          
                                                                        
           WHEN SO-PROGRAM-RUNS-NOT-FIRST-TIME                          
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               PERFORM 2003-PROCESS-NOT-FIRST-TIME                      
                                                                        
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'SERIOUS ERROR IN Z02182' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       2001-PROCESS-FIRST-TIME                   
      * HERE PROGRAM WILL VALIDATE IF THIS PROGRAM WAS CALLED WITH      
      * CORRECT FLAGS, IF NOT WE WILL DROP AN ERROR                     
      *                                                                 
      *   PROGRAM MODES ARE:                                            
      *   1.  SO-Z02192-M-ONE-WAY  -> ONE WAY FLIGHT                    
      *   2.  SO-Z02192-M-2-WAY    -> 2WAY FLIGHT                       
      *   3.  SO-ONLY-DISPLAY     -> IT MEANS PROGRAM WAS CALLED        
      *  BY  Z02232 PROGRAM AND NOW WE HAVE TO DISPLAY GRAPHICAL        
      *  REPRESENTATION AND TAKEN SEATS FOR THIS FLIGHT  
      * BUT USER WON'T BE ABLE TO MODIFY THIS MAP                       
      *                                                                 
      *   4. SO-ONLY-DISPLAY-RESERV -> IT MEANS PROGRAM WAS CALLED      
      *    BY Z02261 PROGRAM AND WE NEED TO DISPLAY GRAPHICAL           
      *    REPRESENTATION OF THE SEATS ALONG WITH ALL TAKEN SEATS       
      *   BUT TAKEN SEATS IN A GIVEN RESERVATION WILL BE MARKED LIKE 'R'
      *    AS ABOVE USER WON'T BE ALLOWED TO MAKE CHANGES ON THIS MAP   
      *                                                                 
      *   IF ANY OTHER PROGRAM MODE WAS SPECIFIED THEN PROPER MESSAGE   
      *  WILL BE DISPLAYED FOR THE USER                                 
      *                                                                 
      ******************************************************************
       2001-PROCESS-FIRST-TIME.                                         
           EVALUATE TRUE                                                
           WHEN SO-Z02192-M-ONE-WAY                                     
           WHEN SO-Z02192-M-2-WAY                                       
           WHEN SO-ONLY-DISPLAY                                         
           WHEN SO-ONLY-DISPLAY-RESERV                                  
              PERFORM 2005-DISPLAY-THE-SEATS                            
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID PROGRAM MODE ' TO                          
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       2002-PROCESS-WITH-DATA                    
      ******************************************************************
       2002-PROCESS-WITH-DATA.                                          
           PERFORM 2001-PROCESS-FIRST-TIME                              
           .                                                            
      ******************************************************************
      *                      2003-PROCESS-NOT-FIRST-TIME                
      * PROGRAM CAN BE USED TO ALLOW USER TO CHOOSE SEATS OR            
      * TO JUST DISPLAY TO HIS REPRESENATION OF THE SEATS               
      * IF SECOND OPTION IS TRUE THEN ENTER KEY WONT BY ACTIVE          
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHENTER                                                
               IF SO-ONLY-DISPLAY OR SO-ONLY-DISPLAY-RESERV  THEN       
                 PERFORM 2400-INITIALIZE-ERROR-MESSAGE                  
                 MOVE 'YOU HAVE PRESSED NO ACTION KEY ' TO              
                                     WS-Z02141-I-ERROR-MESSAGE(1)       
                 SET SO-Z02141-M-WITH TO TRUE                           
                 PERFORM 2300-CALL-ERROR-ROUTINE                        
               ELSE                                                     
                 PERFORM 2106-PROCES-USER-INPUT                         
               END-IF                                                   
           WHEN DFHPF3                                                  
               SET SO-FINAL-TERMINATION  TO TRUE                        
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU HAVE PRESSED NO ACTION KEY ' TO                
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   2005-DISPLAY-THE-SEATS                        
      * AT THE BEGINIG PROGRAM WILL PREPARE DATA FOR THE CURSR          
      * WHEN ALL OF THAT IS READY THEN PROGRAM                          
      * WILL GET EMPTY GRAFICAL REPRESENTATION AND LATER                
      * CURSOR WILL BE OPENED AND DATA WILL BE FETCHED                  
      * (SEATS WILL BE MARKED                                           
      ****************************************************************  
       2005-DISPLAY-THE-SEATS.                                          
           PERFORM 2102-INITIALIZE-SCREEN                               
      * PROGRAM HAVE TO VALIDATE DATA NEEDED BY THE PROGRAM             
      * WHEN WE HAVE TO DISPLAY SEATS FOR A RESERVATION WE NEED         
      * TO VALIDATE RESERVATION ID                                      
      *                                                                 
      * SO-ONLY-DISPLAY-RESERV MEANS THAT WE HAVE TO DISPLAY            
      * TAKEN SEATS FOR A GIVEN RESERVATION                             
           IF SO-ONLY-DISPLAY-RESERV  THEN                              
             PERFORM 2301-VALIDATE-RESERVATION-ID                       
             PERFORM 2302-PREPARE-FLIGHT-ID-RESERV                      
           ELSE                                                         
             PERFORM 2303-PREPARE-FLIGHT-ID-NORMAL                      
           END-IF                                                       
                                                                        
      * GET EMPTY GRAFICAL REPRESENTATION OF THE SEATS                  
           PERFORM 7001-GET-GRAFICAL-REPR                               
           MOVE T13-GRAFICAL-REPRES-TEXT  TO WS-SEATS-REPRESENTATION    
                                                                        
      *                                                                 
      * GRAPHICAL REPRESERNTATION STORES INFO ABOUT WHERE ARE           
      * SEATS ETC.                                                      
      * ON THAT GRAPHICAL REPRESENTATION WE WILL MARK TAKEN SEATS AS    
      * '|' AND IN CASE THAT WE ARE SEARCHING FOR SEATS IN A            
      * GIVEN RESERVATION WE WILL MARK TAKEN SEATS IN A RESERVATION AS  
      * 'R'                                                             
                                                                        
           PERFORM 2103-GET-TAKEN-SEATS                                 
           PERFORM 2101-MOVE-DATA-TO-SCREEN                             
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ****************************************************************  
      *                  2100-SEND-THE-MAP                              
      ****************************************************************  
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
            SEND MAP('MP0219') MAPSET('MP0219')                         
            FROM(MP0219O)                                               
            ERASE           
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ****************************************************************  
      *                  2101-MOVE-DATA-TO-SCREEN                       
      ****************************************************************  
       2101-MOVE-DATA-TO-SCREEN.                                        
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 > 20     
              DISPLAY 'WS-ITER1 = ' WS-ITER1                            
              MOVE WS-SEATS-LINE(WS-ITER1) TO POLEO(WS-ITER1)           
              DISPLAY POLEO(WS-ITER1)                                   
           END-PERFORM                                                  
           .                                                            
      ****************************************************************  
      *                 2102-INITIALIZE-SCREEN                          
      * PROGRAM CAN BY USED IN 2 SCENARIOS                              
      * 1. USER WILL CHOOSE SEATS HE WANTS TO RESERVATE AND HE WILL     
      * GO FURTHER                                                      
      * 2. USER JUST WANT TO  SEE  WHAT SEATS ARE TAKEN                 
      *                                                                 
      * IF THE SECOND OPTION IS TRUE THEN WE NEED TO BLOCK THE MAP      
      * THANKS TO THAT USER WONT BE ABLE TO MODIFY ANYTHING             
      *                                                                 
      * IF FIRST OPTION IS TRUE THEN WE WILL JUST RESET A MAP           
      ****************************************************************  
       2102-INITIALIZE-SCREEN.                                          
           MOVE LOW-VALUES TO MP0219O                                   
           IF SO-ONLY-DISPLAY  OR SO-ONLY-DISPLAY-RESERV  THEN          
               MOVE  DFHBMDAR          TO    HEADA                      
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 20   
               MOVE  DFHBMPRO          TO POLEA(WS-ITER2)               
             END-PERFORM                                                
           ELSE                                                         
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 20   
               MOVE LOW-VALUES TO POLEA(WS-ITER2)                       
             END-PERFORM    
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                   2103-GET-TAKEN-SEATS                          
      * THIS PARAGRAPH WILL FETCH ALL DATA ABOUT TAKEN SEATS FROM       
      * T04_FLIGHT_SEATS TABLE, AND WILL DISPLAY THEM FOR THE USER      
      ****************************************************************  
       2103-GET-TAKEN-SEATS.                                            
           PERFORM 7002-OPEN-CURSOR                                     
           PERFORM 7003-FETCH-CURSOSR-DATA                              
           PERFORM 7004-CLOSE-CURSOR                                    
           .                                                            
      ****************************************************************  
      *                   2105-CHECK-THIS-SEAT                          
      * SCREEN POSITION OF THE SEAT WAS ALREADY CALCULATED              
      * THIS PARAGRAPH WILL ONLY PUT THERE '|' IF THIS IS TAKEN SEAT    
      * OR 'R' IF THIS IS TAKEN SEAT IN CURRECT RESERVATION             
      *  CT-SEAT-IN-RESERVATION = 'R'                                   
      *  CT-TAKEN-SEAT          = '|'                                   
      ****************************************************************  
       2105-CHECK-THIS-SEAT.                                            
           IF T04-RESERVATION-ID = WS-RESERVATION-ID THEN               
             MOVE CT-SEAT-IN-RESERVATION TO                             
                WS-SEATS-LINE(WS-SEAT-NUMBER)(WS-ROW-NUMBER:1)          
           ELSE                                                         
             MOVE CT-TAKEN-SEAT          TO                             
                WS-SEATS-LINE(WS-SEAT-NUMBER)(WS-ROW-NUMBER:1)          
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                   2106-PROCES-USER-INPUT                        
      * IF USER DIDN'T CHECK SEATS FOR ALL FLIGHTS                      
      * PROGRAM WILL SEND TO HIM GRAFICAL REPRESENTATION OF NEXT        
      * FLIGHT                                                          
      *                                                                 
      * IF HE CHECKED EVERYTHING PROGRAM LOGIC GOES TO THE NEXT PROGRAM 
      * (Z02202)                                                        
      ****************************************************************  
       2106-PROCES-USER-INPUT.                                          
           PERFORM 2107-RECEIVE-DATA                                    
           PERFORM 2108-CHECK-THIS-DATA                                 
           DISPLAY 'WS-FLIGHT-COUNTER: '  WS-FLIGHT-COUNTER             
           DISPLAY 'Z02192-ONE-WAY-FLIGHT-AMOUNT '                      
                            Z02192-ONE-WAY-FLIGHT-AMOUNT                
           IF WS-FLIGHT-COUNTER < Z02192-ONE-WAY-FLIGHT-AMOUNT          
           THEN                                                         
              ADD 1 TO WS-FLIGHT-COUNTER                                
              PERFORM 2005-DISPLAY-THE-SEATS                            
           ELSE                                                         
      * HERE WE KNOW THAT ALL SEATS FOR ALL FLIGHTS WERE SAVED          
      * CORRECTLY                                                       
                                                                        
             PERFORM 2600-CALL-TO-PASSENGERS-PROG                       
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                   2107-RECEIVE-DATA                             
      ****************************************************************  
       2107-RECEIVE-DATA.                                               
           EXEC CICS                                                    
            RECEIVE MAP('MP0219') MAPSET('MP0219')                      
            INTO(MP0219I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(MAPFAIL)                                        
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED CHOOSE SOMETHING ' TO                      
                         WS-Z02141-I-ERROR-MESSAGE(1)                   
              SET    SO-Z02141-M-WITH TO TRUE  
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                   2108-CHECK-THIS-DATA                          
      *   PARAGRAPH WILL GET POSITIONS WHERE USER PLACED ;X;            
      *                                                                 
      *   PARAGRAPH WILL VALIDATE THEM AND PREPARE  THEM TO BE USED     
      *                                                                 
      *   AT THE END OF THIS PARAGRAPH DATA WILL BE SAVED INTO THE      
      * COMMAREA                                                        
      * IN A VALID FORMAT FOR EXMAPLE ( 10 A )                          
      ****************************************************************  
       2108-CHECK-THIS-DATA.                                            
           PERFORM 2305-GET-USER-SEATS                                  
           PERFORM 2307-IF-VALID-SEAT-NUMBER                            
      * NOW PROGRAM WILL CHECK IF THIS SEATS EXEISTS ON THAT POSITIONS  
      * (IF THEY ARE REPRESENTED BY '-' IN EMPTY REPRESENTATION         
           PERFORM 2308-VALIDATE-USER-SEAT                              
      * IN THAT PLACE IN PROCEDURE WE KNOW THAT USER CHECHED            
      * 1. VALID NUMBER OF SEATS                                        
      * 2. THOSE SEATS ARE VALID  ( EXISTS IN THAT PLACE)               
      * 3. THOSE SEATS ARE NOT TAKEN                                    
      *                                                                 
      * NOW PROGRAM NEED TO WRITE INFORMATIONS ABOUT THIS               
      * TAKEN SEATS INTO COMMAREA                                       
                                                                        
           PERFORM 2312-MOVE-DATA-TO-COMMAREA                           
      * AFTER THIS PARAGRAPH ENDS WE KNOW THAT USER PROVIDED            
      * CHOOSE VALID NUMBER OF SEATS AND ALL OF THIS SEATS ARE VALID    
      * AND NOT TAKEN                                                   
           .                                                            
      ****************************************************************  
      *                 2140-GET-EXACT-POSITIONS          
      * EARLIER PROGRAM GET TAKEN SEAT DATA FROM THE DATABASE           
      * BUT THIS IS A "VALID" DATA FOR EXAMPLE 10 A                     
      * FIELDS ON THE SCREEN ARE STORED AS NUMBERS SO WE NEED           
      * TO FIND "REAL" POSITIONS OF THE SEAT ON THE SCREEN              
      *                                                                 
      ****************************************************************  
       2140-GET-EXACT-POSITIONS.                                        
      * THIS LOOP WILL FIND A ROW WHERE FIRST LETTER OF THAT ROW        
      * IS THE SAME AS SEAT LETTER FROM THE DATABASE                    
      *                                                                 
      * ON GRAFICAL REPRESENTATION THERE ALWAYS SHOULD BE LETTERS       
      * REPRESENTING ROWS ON THE LEFT SIDE OF THE REPRESENTATION        
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 >        
                                           CT-MAXIMAL-NUMBER-OF-ROWS    
             IF T04-SEAT-LETTER = WS-SEATS-LINE(WS-ITER1)(1:1)          
             THEN                                                       
      * IF WE HAVE FOUND ROW NUMBER THIS VALUE WILL BE SAVED            
                 MOVE WS-ITER1 TO WS-SEAT-NUMBER                        
             END-IF                                                     
           END-PERFORM                                                  
           PERFORM 2306-PREPARE-ROW-NUMBER                              
      * WE WILL FIND AT WHICH POSITION THERE IS SUCH NUMBER             
      * ON THE FIRST LINE OF GRAPHICAL REPRESENTATION                   
      *                                                                 
      * FIRST LINE OF REPRESENTATION STORES DATA LIKE 1 2 3 4 5         
      * ETC. WE JUST NEED TO FIND AT WHICH POSITION THERE IS            
      * OUR ROW NUMBER                                                  
      *                                                                 
           INSPECT WS-SEATS-LINE(1) TALLYING WS-ROW-NUMBER              
             FOR CHARACTERS  BEFORE INITIAL WS-ROW-NUMBER-ALPHA         
      * IF NUMBER OF ROW IS 2 DIGIT WE WILL ADD 1                       
      * IF ELSE WE WILL ADD 2 TO THIS NUMBER                            
      * THIS WILL ALLOW PROGRAM TO MARK VALID POSITIONS                 
           IF WS-TEMP-NUM > 9 THEN                                      
              ADD 1 TO WS-ROW-NUMBER                                    
           ELSE             
              ADD 2 TO WS-ROW-NUMBER                                   
           END-IF                                                      
      * NOW WE GOT WS-SEAT-NUMBER AND WS-ROW-NUMBER                    
      * THOSE VARIABLES REPRESENTS EXECT POSITIONS ON THE MAP          
      * !THEY ARE NOT STORING DATA FROM THE DATABASE FOR EXAMPLE       
      * 13 B , ONLY VALUES LIKE 173 AND 25 WCHICH REPESENTS            
      * POSITION ON THE MAP                                            
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
      *                  2301-VALIDATE-RESERVATION-ID                   
      * PARAGRAPH SIMPLY VALIDATES IF A GIVEN RESERVATION ID IS A       
      * VALID NUMBER                                                    
      ******************************************************************
       2301-VALIDATE-RESERVATION-ID.                                    
           IF FUNCTION TEST-NUMVAL(Z02242-I-RESERVATION-ID) = 0 THEN    
             COMPUTE WS-RESERVATION-ID = FUNCTION NUMVAL(               
                                    Z02242-I-RESERVATION-ID)            
           ELSE                                                         
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'INVALID RESERVATION NUMBER'                          
                                       TO WS-Z02141-I-ERROR-MESSAGE(1)  
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2302-PREPARE-FLIGHT-ID-RESERV                 
      * PARAGRAPH WILL BE PERFORMED WHEN PRORAM IS CALLED IN ORDER      
      * TO DISPLAY GRAPHINCAL REPRESENTAITON OF THE SEATS               
      * WHERE SEATS IN A GIVEN RESERVATION HAS TO BE MARKED AS 'R'      
      *                                                                 
      * THIS PARAGRAPH PREPARES FLIGHT ID TO BE USED LATER              
      *                                                                 
      * IT ALSO COUNT HOW LONG THE FLIGHT NUMBER IS  ( IN COBOL         
      * WE HAVE TO COUNT LEADING SIGNS SO IT HAD TO BE REVERSED         
      * - JUST IN ORDER TO COUNT VARIABLES AT THE END OF A STRING       
      * WE NEED TO REVERSE A STRING)                                    
      ******************************************************************
       2302-PREPARE-FLIGHT-ID-RESERV.                                   
           MOVE Z02242-FLIGHT-NUMBER TO T05-FLIGHT-ID-TEXT              
           MOVE FUNCTION REVERSE(Z02242-FLIGHT-NUMBER) TO               
                             WS-TEMP-STRING                             
           INSPECT WS-TEMP-STRING TALLYING WS-ITER10 FOR                
                                         LEADING X'00'                  
           COMPUTE T05-FLIGHT-ID-LEN = LENGTH OF T05-FLIGHT-ID-TEXT -   
                                                WS-ITER10               
           .                                                            
      ******************************************************************
      *                  2303-PREPARE-FLIGHT-ID-NORMAL                  
      * PARAGRAPH WILL PREPARE FLIGHT ID WHEN PROGRAM IS IN ANY OTHER   
      * MODE THAT "DISPLAY SEATS FOR RESERVATION "                      
      *                                                                 
      * IT ALSO COUNT HOW LONG THE FLIGHT NUMBER IS  ( IN COBOL         
      * WE HAVE TO COUNT LEADING SIGNS SO IT HAD TO BE REVERSED         
      * - JUST IN ORDER TO COUNT VARIABLES AT THE END OF A STRING       
      * WE NEED TO REVERSE A STRING)                                    
      ******************************************************************
       2303-PREPARE-FLIGHT-ID-NORMAL.                                   
           MOVE Z02192-ONE-WAY-FL-ID(WS-FLIGHT-COUNTER)  TO             
                            T05-FLIGHT-ID-TEXT                          
           MOVE FUNCTION REVERSE(Z02192-ONE-WAY-FL-ID(WS-FLIGHT-COUNTER)
                    ) TO WS-TEMP-STRING                                 
           INSPECT WS-TEMP-STRING TALLYING WS-ITER10 FOR LEADING X'00'  
                                                                        
           COMPUTE T05-FLIGHT-ID-LEN = LENGTH OF T05-FLIGHT-ID-TEXT -   
                                               WS-ITER10                
           .                                                            
      ******************************************************************
      *                    2305-GET-USER-SEATS                          
      * THIS LOOP WILL GO THROUGH WHOLE GRAPHICAL REPRESENTATION OF     
      * THE SEATS                                                       
      * IF THIS LOPP WILL FIND AND 'X' SYMBOL (IT REPRESENTS THAT USER  
      * CHOOSE THIS SEAT) IT SAVES POSITION OF THIS X TO THE            
      * ARRAYS                                                          
      *    BOTH WS-SEAT-ROW-POS    AND                                  
      *         WS-SEAT-LETTER-POS                                      
      *   WILL BE STORED AS NUMBER (LETER WE WILL CALCULATE WHAT        
      * LETTER IT SHOULD BE )                                           
      ******************************************************************
       2305-GET-USER-SEATS.                                             
           MOVE 1 TO WS-ITER1                                           
           MOVE 1 TO WS-ITER2                                           
           MOVE 1 TO WS-ITER3                                           
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 >        
                                    CT-MAXIMAL-WIDTH-OF-REPR            
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 >      
                                    CT-MAXIMAL-HEIGHT-OF-REPR           
               IF POLEI(WS-ITER2)(WS-ITER1:1) = 'X'                     
                 ADD 1 TO WS-COUNT-OF-SEATS                             
                   MOVE WS-ITER1 TO WS-SEAT-ROW-POS(WS-ITER3)           
                   MOVE WS-ITER2 TO WS-SEAT-LETTER-POS(WS-ITER3)        
                 ADD 1 TO WS-ITER3                                      
               END-IF                                                   
             END-PERFORM                                                
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2306-PREPARE-ROW-NUMBER                        
      * THIS PARAGRAPH WILL EXTRACT NUMBER FROM THE VARIABLE            
      * NUMERIC VARIABLE CAN LOOK LIKE THIS 0005 AT THE END OF THIS     
      * PARAGRAPH WE WILL GET  JUST '5'                                 
      * WE NEED TO DO THAT IN ORDER TO FIND CORRECT SPOT ON THE MAP     
      ******************************************************************
       2306-PREPARE-ROW-NUMBER.                                         
           MOVE T04-ROW-NUMBER TO WS-TEMP-NUM                           
           MOVE WS-TEMP-NUM TO WS-TEMP-STRING1                          
           MOVE WS-TEMP-STRING1(3:2) TO WS-ROW-NUMBER-ALPHA             
           INSPECT WS-ROW-NUMBER-ALPHA REPLACING LEADING '0' BY ' '     
           MOVE 0 TO WS-ROW-NUMBER                                      
           .                                                            
      ******************************************************************
      *                   2307-IF-VALID-SEAT-NUMBER                     
      * THIS PARAGRAPH WILL CHECK IF USER CHOOSE VALID NUMBER OF SEATS  
      *  NOT LESS OR MORE THAT HE DECLARED                              
      ******************************************************************
       2307-IF-VALID-SEAT-NUMBER.                                       
           IF WS-COUNT-OF-SEATS > Z02192-ONE-WAY-TICKET-NUMBER THEN     
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU WANT  TO MANY SEATS ' TO                       
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           IF WS-COUNT-OF-SEATS < Z02192-ONE-WAY-TICKET-NUMBER THEN     
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU CHECKED WRONG NUMBER OF SEATS  '  TO           
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2308-VALIDATE-USER-SEAT                      
      * PARAGRAPH WILL GET GRAPHICAL REPRESENTATION OF THE SEATS        
      *                                                                 
      * AND WILL CHECK IF:                                              
      *  1. SEATS EXIST                                                 
      *  2. IF SEAT IS NOT TAKEN                                        
      *                                                                 
      * IF THERE WILL BE AN ERROR PROCESS WILL BE STOPPED AND USER      
      * WILL GET PROPER MESSAGE                                         
      ******************************************************************
       2308-VALIDATE-USER-SEAT.                                         
           MOVE Z02192-ONE-WAY-FL-ID(WS-FLIGHT-COUNTER) TO              
                          T05-FLIGHT-ID-TEXT                            
           COMPUTE T05-FLIGHT-ID-LEN =                                  
                FUNCTION LENGTH(Z02192-ONE-WAY-FL-ID(WS-FLIGHT-COUNTER))
                                                                        
           PERFORM 7001-GET-GRAFICAL-REPR                               
           PERFORM  2313-UNSTRING-REPRESENTATION                        
                                                                        
      * PERFORM AS MANY TIME AS MUCH SEATS USER WANTS TO RESERVATE      
           MOVE 1 TO WS-ITER3                                           
           PERFORM WS-COUNT-OF-SEATS TIMES                              
      * WE WILL MOVE ARRAY DATA TO NORMAL VARIABLES TO MAKE IT MORE     
      * CLREAR                                                          
            COMPUTE WS-TEMP-SEAT-ROW = WS-SEAT-ROW-POS(WS-ITER3)        
            COMPUTE WS-TEMP-SEAT-LETTER =                               
                                        WS-SEAT-LETTER-POS(WS-ITER3)    
                                                                        
             IF WS-SEATS-LINE(WS-TEMP-SEAT-LETTER)                      
                                          (WS-TEMP-SEAT-ROW:1)          
              NOT  = '-' THEN                                           
               PERFORM 2309-SEND-INVALID-SEAT-MSG                       
             ELSE     
      * MOVE STATEMENT BELOW WILL GET US LETTER OF A SEAT               
      * IT IS A FIRST SYMBOL ON THE LEFT SIDE                           
                                                                        
               MOVE WS-SEATS-LINE(WS-TEMP-SEAT-LETTER)(1:1) TO          
                       WS-TEMP-SEAT-LETTER-A                            
               PERFORM 2310-VALIDATE-ROW-NUMER                          
                                                                        
      * NOW THOSE 2 VARIABLES STORES ACTUAL VALUES OF THE SEAT          
      * THEY ARE NOT THE SCREEN POSITION ONLY THE VALID DATABASE        
      * POSITIONS                                                       
               MOVE WS-TEMP-SEAT-LETTER TO WS-SEAT-LETTER-POS(WS-ITER3) 
               MOVE WS-TEMP-SEAT-ROW    TO  WS-SEAT-ROW-POS(WS-ITER3)   
               PERFORM 7006-CHECK-IF-TAKEN                              
                 IF SO-SEAT-IS-TAKEN THEN                               
                   PERFORM 2311-SEND-TAKEN-SEAT-MSG                     
                 END-IF                                                 
             END-IF                                                     
             ADD 1 TO WS-ITER3                                          
           END-PERFORM                                                  
      * AFTER THIS PARAGRAPH IS COMPLETED                               
      * ARRAYS: WS-SEAT-LETTER-POS AND WS-SEAT-ROW-POS                  
      * STORES ACTUAL DATA DESCRIBING THE SEAT ( SEAT LETTER IS STORED  
      * AS A NUMBER )                                                   
           .                                                            
      ******************************************************************
      *                   2309-SEND-INVALID-SEAT-MSG                    
      ******************************************************************
       2309-SEND-INVALID-SEAT-MSG.                                      
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'THAT IS NOT A SEAT       '  TO                         
                               WS-Z02141-I-ERROR-MESSAGE(1)             
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                     2310-VALIDATE-ROW-NUMER    
      * THIS IS EXAMPLE OF A GRAPHICAL REPRESENTATION                   
      *             1  2  3  4  5  6  7  8  9                           
      *           A -  -  -  -  -  -  -  -  -                           
      *                                                                 
      *           B -  -  -  -  -  -  -  -  -                           
      *                                                                 
      *           C -  -  -  -  -  -  -  -  -                           
      *                                                                 
      *           D -  -  -  -  -  X  -  -  -                           
      *                                                                 
      *           E -  -  -  -  -  -  -  -  -                           
      * THIS PARAGRAPH WILL ALREADY HAVE PHYSICAL POSITIONS             
      * WHERE USER PLACED AN 'X' NOW PROGRAM WILL CHECK IF              
      * NUMBER ABOVE THIS POSITION IS CORRECT                           
      *                                                                 
      *  IN CASE THAT USER'S SCREEN LOOKS LIKE THIS ONE ABOVE           
      * PROGRAM WILL CHECK IF  '6 ' IS NUMERIC OR NOT                   
      * IF IT IS THEN  IT WILL BE TREATED AS A ROW NUMBER               
      *                                                                 
      * IF THIS VALUE WILL BE CORRECT THEN THIS ROW NUMBER WILL         
      * BE STORED IN   WS-TEMP-SEAT-ROW VARIABLE                        
      ******************************************************************
       2310-VALIDATE-ROW-NUMER.                                         
      * CHECK IF NUMBER IDETIFYING ROW NUMBER IS NUMERIR                
           IF FUNCTION TEST-NUMVAL(                                     
                WS-SEATS-LINE(1)(WS-TEMP-SEAT-ROW:2))       = 0         
           THEN                                                         
               COMPUTE WS-TEMP-SEAT-ROW = FUNCTION NUMVAL(              
                 WS-SEATS-LINE(1)(WS-TEMP-SEAT-ROW:2))                  
           ELSE                                                         
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID REPRESENTATION       ' TO                   
                                    WS-Z02141-I-ERROR-MESSAGE(1)        
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF    
           .                                                            
      ******************************************************************
      *                   2311-SEND-TAKEN-SEAT-MSG                      
      ******************************************************************
       2311-SEND-TAKEN-SEAT-MSG.                                        
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'THAT SEAT IS TAKEN ' TO                                
                            WS-Z02141-I-ERROR-MESSAGE(1)                
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                   2312-MOVE-DATA-TO-COMMAREA                    
      * PARAGRAPH WILL MVOE SEAT DATA FROM PROGRAM VARIABLES TO         
      * COMMAREA                                                        
      ******************************************************************
       2312-MOVE-DATA-TO-COMMAREA.                                      
           MOVE T05-FLIGHT-ID-TEXT TO                                   
                         Z02192-SEAT-FLIGHT-NUMBER(WS-FLIGHT-COUNTER)   
           MOVE T05-FLIGHT-NUMBER-TEXT TO                               
                         Z02192-SEAT-ACT-FLG-NUMBER(WS-FLIGHT-COUNTER)  
           MOVE Z02192-ONE-WAY-TICKET-NUMBER TO                         
                         Z02192-TICKET-NUMBER(WS-FLIGHT-COUNTER)        
           PERFORM VARYING WS-ITER4 FROM 1 BY 1 UNTIL WS-ITER4 >        
                                            Z02192-ONE-WAY-TICKET-NUMBER
                                                                        
               COMPUTE WS-TEMP-SEAT-ROW = WS-SEAT-ROW-POS(WS-ITER4)     
               COMPUTE WS-TEMP-SEAT-LETTER =                            
                                        WS-SEAT-LETTER-POS(WS-ITER4)    
               MOVE WS-SEATS-LINE(WS-TEMP-SEAT-LETTER)(1:1) TO          
                       WS-TEMP-SEAT-LETTER-A                            
               MOVE WS-TEMP-SEAT-LETTER-A TO                            
                   Z02192-SEAT-LETTER(WS-FLIGHT-COUNTER,WS-ITER4)       
               MOVE WS-TEMP-SEAT-ROW      TO                            
                   Z02192-SEAT-ROW-POSITION(WS-FLIGHT-COUNTER,WS-ITER4) 
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2313-UNSTRING-REPRESENTATION                 
      * THIS PARAGRAPH WILL MOVE REPRESENTATION FRO DCLGEN VARIABLE     
      * TO THE PROGRAM VARIABLES                                        
      ******************************************************************
       2313-UNSTRING-REPRESENTATION.                                    
           UNSTRING T13-GRAFICAL-REPRES-TEXT                            
             INTO WS-SEATS-LINE(1)                                      
                  WS-SEATS-LINE(2)                                      
                  WS-SEATS-LINE(3)                                      
                  WS-SEATS-LINE(4)                                      
                  WS-SEATS-LINE(5)                                      
                  WS-SEATS-LINE(6)                                      
                  WS-SEATS-LINE(7)                                      
                  WS-SEATS-LINE(8)                                      
                  WS-SEATS-LINE(9)                                      
                  WS-SEATS-LINE(10)                                     
                  WS-SEATS-LINE(11)                                     
                  WS-SEATS-LINE(12)                                     
                  WS-SEATS-LINE(13)                                     
                  WS-SEATS-LINE(14)                                     
                  WS-SEATS-LINE(15)                                     
                  WS-SEATS-LINE(16)                                     
                  WS-SEATS-LINE(17)                                     
                  WS-SEATS-LINE(18)                                     
                  WS-SEATS-LINE(19)                                     
                  WS-SEATS-LINE(20)                                     
           END-UNSTRING                                                 
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
      *                     2600-CALL-TO-PASSENGERS-PROG                
      ******************************************************************
       2600-CALL-TO-PASSENGERS-PROG.                                    
           SET SO-M-FIRST-WITHOUT  TO TRUE                              
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-PASSENGER-DATA) COMMAREA(DFHCOMMAREA)       
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
              DISPLAY 'RETURN WITH 0212'                                
              EXEC CICS                                                 
               RETURN TRANSID('0213') COMMAREA(DFHCOMMAREA)             
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN SO-FINAL-TERMINATION                                    
              EVALUATE TRUE                                             
              WHEN SO-ONLY-DISPLAY                                      
                                                                        
                SET SO-M-FIRST-WITH  TO TRUE                            
                MOVE WS-ZZEC0215 TO DFHCOMMAREA                         
                EXEC CICS                                               
                 XCTL PROGRAM(CT-FIND-FLIGHT-PROGRAM)                   
                 COMMAREA(DFHCOMMAREA)                                  
                END-EXEC      
              WHEN SO-ONLY-DISPLAY-RESERV                               
                SET SO-M-FIRST-WITH  TO TRUE                            
                MOVE WS-ZZEC0215 TO DFHCOMMAREA                         
                EXEC CICS                                               
                 XCTL PROGRAM(CT-FIND-BOOKING-PROGRAM)                  
                 COMMAREA(DFHCOMMAREA)                                  
                END-EXEC                                                
              WHEN OTHER                                                
                SET SO-M-FIRST-WITH  TO TRUE                            
                MOVE WS-ZZEC0215 TO DFHCOMMAREA                         
                EXEC CICS                                               
                  XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                 
                   COMMAREA(DFHCOMMAREA)                                
                END-EXEC                                                
              END-EVALUATE                                              
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)    
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ****************************************************************  
      *                  7001-GET-GRAFICAL-REPR                         
      * PARAGRAPH WILL GET GRAPHICAL REPRESENTATION OF THE SEATS        
      *                                                                 
      * GRAPHICAL REPRESENTATION LOOKS LIKE THIS:                       
      *                                                                 
      *          1  2  3  4  5  6  7  8  9  10                          
      *       A  -  -  -  -  -  -  -  -  -  -                           
      *                                                                 
      *       B  -  -  -  -  -  -  -  -  -  -                           
      *                                                                 
      *       C  -  -  -  -  -  -  -  -  -  -                           
      *          
      *       D  -  -  -  -  -  -  -  -  -  -                           
      *                                                                 
      * WHERE '-' IS A FREE SEAT                                        
      *                                                                 
      * THIS REPRESENTATION IS SAVED IN A DATABASE                      
      *                                                                 
      * THIS PROGRAM WILL GET THAT REPRESENTATION, LATER IT WILL        
      * ADD A '|' SYMBOL EVERYWHERE THERE IS AN TAKEN SEAT              
      * OR 'R' WHERE THIS IS A SEAT IN A GIVER RESERVATION              
      *                                                                 
      *                                                                 
      ****************************************************************  
       7001-GET-GRAFICAL-REPR.                                          
           INITIALIZE T13-GRAFICAL-REPRESENTATION                       
           INITIALIZE T13-HOW-MANY-ROWS-OF-SEATS                        
           EXEC SQL                                                     
           SELECT                                                       
              GRAFICAL_REPRESENTATION,                                  
              HOW_MANY_ROWS_OF_SEATS,                                   
              FLIGHT_NUMBER                                             
           INTO                                                         
              :T13-GRAFICAL-REPRESENTATION,                             
              :T13-HOW-MANY-ROWS-OF-SEATS,                              
              :T05-FLIGHT-NUMBER                                        
           FROM                                                         
              T13_TYPE_OF_SEATS_TABLE                                   
           INNER JOIN                                                   
              T08_TABLE_PLANE_TABLE                                     
           ON                                                           
              T08_TABLE_PLANE_TABLE.TYPE_OF_SEATS_ID =                  
              T13_TYPE_OF_SEATS_TABLE.TYPE_OF_SEATS_ID                  
           INNER JOIN                                                   
              T05_FLIGHT_TABLE                                          
           ON                                                           
              T05_FLIGHT_TABLE.PLANE_ID =                               
              T08_TABLE_PLANE_TABLE.PLANE_ID     
           WHERE                                                        
              T05_FLIGHT_TABLE.FLIGHT_ID = :T05-FLIGHT-ID               
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                    7002-OPEN-CURSOR                             
      ****************************************************************  
       7002-OPEN-CURSOR.                                                
           EXEC SQL                                                     
             OPEN C-TAKEN-SEATS-CURSOR                                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7002-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                 7003-FETCH-CURSOSR-DATA                         
      * FETCH WILL GET ROW NUMBER AND SEAT LETTER                       
      * PROGRAM NEED TO CHANGE SEATS LETTER TO CORRESPONDING NUMBER     
      * AND PROGRAM WILL CHECK THAT SEAT ON GRAFICAL REPRESENTATION     
      ****************************************************************  
       7003-FETCH-CURSOSR-DATA.                                         
           PERFORM 7005-FETCH-TAKEN-SEAT                                
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
                PERFORM 2140-GET-EXACT-POSITIONS                        
                PERFORM 2105-CHECK-THIS-SEAT                            
               PERFORM 7005-FETCH-TAKEN-SEAT                            
           END-PERFORM 
           .                                                            
      ****************************************************************  
      *                 7004-CLOSE-CURSOR                               
      ****************************************************************  
       7004-CLOSE-CURSOR.                                               
           EXEC SQL                                                     
             CLOSE C-TAKEN-SEATS-CURSOR                                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7002-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ****************************************************************  
      *                    7005-FETCH-TAKEN-SEAT                        
      ****************************************************************  
       7005-FETCH-TAKEN-SEAT.                                           
           SET SO-NOT-END-OF-CURSOR-DATA TO TRUE                        
           INITIALIZE T04-ROW-NUMBER                                    
           INITIALIZE T04-SEAT-LETTER                                   
           EXEC SQL                                                     
           FETCH C-TAKEN-SEATS-CURSOR                                   
           INTO                                                         
               :T04-ROW-NUMBER,                                         
               :T04-SEAT-LETTER,                                        
               :T04-RESERVATION-ID                                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             CONTINUE                                                   
           WHEN SO-SQLCODE-NOT-FOUND                                    
             SET SO-END-OF-CURSOR-DATA  TO TRUE                         
           WHEN OTHER                                                   
             SET SO-7005-PARA TO TRUE    
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7006-CHECK-IF-TAKEN                         
      * WE WILL CHECH IF SEAT THAT USER WANTS TO RESERVATE IS EMPTY     
      * OR ALREADY TAKEN                                                
      ******************************************************************
       7006-CHECK-IF-TAKEN.                                             
                                                                        
           MOVE WS-TEMP-SEAT-ROW             TO  T04-ROW-NUMBER         
           MOVE WS-TEMP-SEAT-LETTER-A        TO  T04-SEAT-LETTER        
                                                                        
                                                                        
           EXEC SQL                                                     
            SELECT ROW_NUMBER                                           
            INTO :WS-TEMP-VARIABLE                                      
            FROM T04_FLIGHT_SEATS                                       
            WHERE                                                       
             ROW_NUMBER =  :T04-ROW-NUMBER                              
            AND                                                         
             SEAT_LETTER = :T04-SEAT-LETTER                             
            AND                                                         
             FLIGHT_ID = :T05-FLIGHT-ID                                 
            FETCH FIRST ROW ONLY                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               SET SO-SEAT-IS-TAKEN TO TRUE                             
           WHEN SO-SQLCODE-NOT-FOUND                                    
               SET SO-SEAT-IS-NOT-TAKEN TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7006-PARA TO TRUE                                  
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
      *                       9000-DB2-ERROR                            
      ******************************************************************
       9100-ROLLBACK.                             
           EXEC CICS                              
             SYNCPOINT ROLLBACK                   
           END-EXEC                               
           PERFORM 2200-CHECK-EIBRESP             
           .                                      
                                              
                               
                                                 
                       
                                                       
                                          
   
                                                                        
                                                   
                 
                                                  
                    


              
                                            
              
                         
                                            
                                            
               
                 
                       
                                  
