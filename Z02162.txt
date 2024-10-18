       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02162.                                              
      ******************************************************************
      *            Z02162  TRANSACTION 0210                             
      *                                                                 
      *  PROGRAM WILL BE CALLED WHEN USER PROVIDE INVALID OR            
      * NOT WHOLE:                                                      
      *              1. AIRPORT NAME                                    
      *              2. PLANE MODEL NAME                                
      *              3. COUNTRY NAME                                    
      *              4. AIRLINE NAME                                    
      *  ROGRAM WILL USE SQL'S LIKE STATEMENT TO GET ALL SIMILAR NAMES  
      * TO WHAR USER PROVIDED                                           
      *                                                                 
      * PROGRAM CAN BE CALLED BY :                                      
      *      1. Z02152                                                  
      *      2. Z02202                                                  
      *      3. Z02292                                                  
      *                                                                 
      *                                                                 
      *   CALLING PROGRAM WILL HAVE TO MODIFY PROPER MAPS THAT WILL     
      * DETERMINE FOR WHAT SIMILAR NAMES PROGRAM SHOULD SEARCH FOR      
      *                                                                 
      *                                                                 
      * PROGRAM LOGIC IS TO FIND SIMILAR NAMES (TO WHAT USER PROVIDED)  
      * PUT THAT IN THE QUEUES AND DISPLAY TO THE USER AND USER WILL    
      * BE ABLE TO CHOOSE ONE OF THIS NAMES (BY PLACING 'X' NEXT TO IT) 
      * AND THAT VALUE WILL BE RETURNED TO CALLING PROGRAM              
      *                                                                 
      * SEARCHING PROCESS WILL BE MADE BY USIND SQL "LIKE" CLOUSE       
      *                                                                 
      * COUNTRIES WILL BE STORED IN      CT-QUEUE-COUNTRY-NAME          
      * PLANE MODEL WILL BE STORED IN    CT-QUEUE-MODEL-NAME            
      * AIRPORT NAEMS WILL BE STORED IN  CT-QUEUE-AIRPORT-NAME          
      * AIRLINES WILL BE STORED IN       CT-QUEUE-AIRLINE-NAME          
      *                                                                 
      * AFTER BEING STORED DATA WILL BE DISPLAYED FOR THE USER THAT     
      * WILL HAVE FUNCTIONALITY TO PRESS F7 AND F8 KEYS TO BROWSE       
      * GENERATED DATA                                                  
      ******************************************************************
      *                       CHANGE LOG                                
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY ZZEC0215.                                               
           COPY DFHAID.                                                 
           COPY ZZMP0216.                                               
           EXEC SQL INCLUDE SQLCA  END-EXEC.                            
           EXEC SQL INCLUDE T01TAB END-EXEC.                            
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T03TAB END-EXEC.                            
           EXEC SQL INCLUDE T08TAB END-EXEC.                            
                                                                        
      * CURSOR DECLARATION                                              
      * CURSOR WILL RETURN ALL SIMILAR AIRPORT NAMES ( TO WHAT USER     
      * PROVIDED)                                                       
           EXEC SQL DECLARE C-AIRPORT-CURSOR CURSOR                     
           FOR                                                          
           SELECT AIRPORT_CODE, AIRPORT_FULL_NAME                       
            FROM T02_AIRPORT_TABLE                                      
           WHERE AIRPORT_FULL_NAME  LIKE :T02-AIRPORT-FULL-NAME         
           END-EXEC                                                     
                                                                        
      * DECLARATION OF SECOND CURSOR                                    
      * THIS ONE WILL RETURN SIMILAR COUTNRY NAMES                      
           EXEC SQL                                                     
           DECLARE C-COUNTRY-CURSOR CURSOR                              
           FOR                                                          
           SELECT      
               COUNTRY_CODE, COUNTRY_NAME                               
           FROM T03_COUNTRY_TABLE                                       
               WHERE COUNTRY_NAME LIKE :T03-COUNTRY-NAME                
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
                                                                        
      * DECLARATION OF THIRD  CURSOR                                    
      * THIS ONE WILL RETURN SIMILAR PLANE MODEL   NAMES                
           EXEC SQL                                                     
           DECLARE C-PLANE-MODEL-CURSOR CURSOR                          
           FOR                                                          
           SELECT                                                       
               PLANE_ID, PLANE_MODEL                                    
           FROM T08_TABLE_PLANE_TABLE                                   
               WHERE PLANE_MODEL  LIKE :T08-PLANE-MODEL                 
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
      * DECLARATION OF FORTH  CURSOR                                    
      * THIS ONE WILL RETURNS SIMILAR ARILINE NAMES                     
           EXEC SQL                                                     
           DECLARE C-AIRLINE-NAME-CURSOR CURSOR                         
           FOR                                                          
           SELECT                                                       
               AIRLINE_CODE, AIRLINE_NAME                               
           FROM T01_AIRLINE_NAMES_TABLE                                 
               WHERE AIRLINE_NAME LIKE :AIRLINE-NAME                    
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
      * DB2 HANDLING VARIABLES                                          
       01 WS-DB2-ERROR.                                                 
           10 SW-SQLCODE                    PIC S9(5).                  
               88 SO-SQLCODE-OK             VALUE  000   100.           
               88 SO-SQLCODE-NORMAL         VALUE 000.                  
               88 SO-SQLCODE-NOT-FOUND      VALUE 100.                  
           10 WS-SQLERRMC                   PIC X(70).                  
           10 WS-SQLCODE-FORMAT             PIC -(5).                   
           10 SW-STATEMENT-ID               PIC X(4).                   
               88 SO-7100-PARA              VALUE '7100'.               
               88 SO-7200-PARA              VALUE '7200'.               
               88 SO-7300-PARA              VALUE '7300'.               
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
                                                                        
       01 CT-CONSTANTS.                                                 
           05 CT-QUEUE-AIRPORT-NAME           PIC X(8) VALUE '02XX    '.
           05 CT-QUEUE-COUNTRY-NAME           PIC X(8) VALUE '02X11   '.
           05 CT-QUEUE-MODEL-NAME             PIC X(8) VALUE '02X12   '.
           05 CT-QUEUE-AIRLINE-NAME           PIC X(8) VALUE '02X13   '.
           05 CT-CALLING-PROGRAM-NAME         PIC X(8) VALUE 'Z02152  '.
           05 CT-Z02152-PROG-NAME             PIC X(8) VALUE 'Z02152  '.
           05 CT-THIS-PROGRAM-NAME            PIC X(8) VALUE 'Z02162  '.
           05 CT-ERROR-ROUTINE-NAME           PIC X(8) VALUE 'Z02141  '.
           05 CT-RESERVATION-PROGRAM          PIC X(8) VALUE 'Z02202  '.
       01 WS-VARIABLES.                                                 
           05 WS-SEARCH-PHRASE                PIC X(100) VALUE SPACE.   
           05 WS-FORMATED-SEARCH-PHRASE-LEN   PIC S9(4)  COMP VALUE 0.  
           05 WS-SELECTED-PLANE-ID            PIC S9(9)  COMP VALUE 0.  
           05 WS-NUMBER-OF-QUEUE-RECORDS      PIC S9(4)  COMP VALUE 0.  
           05 WS-TEMP-STRING                  PIC X(5)   VALUE SPACE.   
           05 WS-ITER                         PIC S9(4)  COMP VALUE 0.  
           05 WS-RECORD-ITEM-ID               PIC S9(4)  COMP VALUE 0.  
           05 WS-SELECTED-IATA                PIC X(3)   VALUE SPACE.   
           05 WS-FORMATED-SEARCH-PHRASE       PIC X(50)  VALUE SPACE.   
           05 WS-X-COUNTER                    PIC S9(4)  COMP VALUE 0.  
           05 WS-POSITION-OF-X                PIC S9(4)  COMP VALUE 0.  
           05 WS-LICZNIK                      PIC S9(4)  COMP VALUE 0.  
           05 WS-ITER2                        PIC S9(4)  COMP VALUE 0.  
       01 WS-QUEUE-STRUCTURE.                                           
           05 QUEUE-AIRPORT-CODE              PIC X(3).                 
           05 QUEUE-AIRPORT-FULL-NAME         PIC X(100).               
       01 WS-QUEUE-2-STRUCTURE.                                         
           05 QUEUE-2-COUNTRY-CODE            PIC X(3).                 
           05 QUEUE-2-COUNTRY-NAME            PIC X(50).                
       01 WS-QUEUE-3-STRUCTURE.                                         
           05 QUEUE-3-PLANE-ID                PIC S9(9) COMP.           
           05 QUEUE-3-PLANE-MODEL             PIC X(50).                
       01 WS-QUEUE-4-STRUCTURE.                                         
           05 QUEUE-4-AIRLINE-CODE            PIC X(3).                 
           05 QUEUE-4-AIRLINE-NAME            PIC X(100).               
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME              PIC X.         
               88  SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'. 
               88  SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'. 
               88  SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'. 
           05 SW-IF-END-IF-INPUT-QUEUE                       PIC X.     
               88 SO-END-OF-QUEUE-DATA                       VALUE 'Y'. 
               88 SO-NOT-END-OF-QUEUE-DATA                   VALUE 'N'. 
           05 SW-IF-END-OF-TABLE-DATA                        PIC X.     
               88 SO-END-OF-TABLE-DATA                       VALUE 'Y'. 
               88 SO-NOT-END-OF-TABLE-DATA                   VALUE 'N'. 
           05 SW-WHAT-TYPE-OF-END                            PIC X.     
               88 SO-FINAL-WITH-COMMAREA                     VALUE '1'. 
               88 SO-FINAL-TERMINATION                       VALUE '2'. 
           05 SW-IF-QUEUE-HAS-DATA                           PIC X.     
               88 SO-QUEUE-HAS-DATA                          VALUE 'Y'. 
               88 SO-QUEUE-DONT-HAVE-DATA                    VALUE 'N'. 
           05 SW-WHERE-SHOULD-CONTROL-GO                     PIC X.     
               88 SO-GO-BACK-TO-THIS-PROG                    VALUE '1'. 
               88 SO-GO-BACK-TO-Z02152                       VALUE '2'. 
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
      ******************************************************************
      *                      PROCEDURE DIVISION                         
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02162---------------START'                         
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02162----------AFTER-PROCESS'                      
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                          1000-INIT                              
      ******************************************************************
       1000-INIT.                                                       
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           PERFORM 1005-CHECK-IF-FIRST-TIME                             
           .                                                            
      ******************************************************************
      *                   1005-CHECK-IF-FIRST-TIME                      
      * PROGRAM WILL SET FLAGS DEPENDING ON WHAT IS THE PROGRAM MODE    
      * RIGHT NOW                                                       
      *                                                                 
      * PROGRAM HAS 3 MAIN MODES:                                       
      * 1. NOT FIRST TIME -> MEANS THAT TRANSACTION WAS STOPPED AND     
      * WAS RETTRIGERED BY USER PRESSING AN ATTENTION KEY               
      *                                                                 
      * 2. FIRST TIME WITHOUT DATA -> PRARAGRAPH WILL BE CALED          
      * ONLY IF PROGRAM IS CALLED FOR THE FIRST TIME  ( PROGRAM DIDIN'T 
      * GENERATE ANY DATA AND USER DIDN'T PROVIDE ANY DATA NEITHER)     
      *                                                                 
      * 3. FIRST TIME WITH DATA -> FOR EXAMPLE PROGRAM DISPLAYED SOME   
      * DATA THEN USER MAKE MISTAKE AND Z02141 PROGRAM WAS CALLED       
      * (ERORR ROUTINE) AND CONTROL RETURNED TO THIS PROGRAM            
      * BECAUSE WE ALREADY HAD SOME DATA THEN THIS DATA WILL BE         
      * DISPLAYED TO THE USER (PROGRAM WON'T LOOK FOR THEM ONCE AGAIN)  
      *                                                                 
      *                                                                 
      * ANY OTHER OPTION WILL BE INVALID - PROPER MESSAGE WILL          
      * BE DISPLAYED TO THE USER                                        
      ******************************************************************
       1005-CHECK-IF-FIRST-TIME.                                        
           DISPLAY 'Z02162 MODE: ' SW-M-WHAT-MODE                       
           PERFORM 1006-SET-STARTING-FLAGS                              
           EVALUATE TRUE                                                
           WHEN SO-M-FIRST-WITHOUT                                      
              PERFORM 1010-IGNORE-CICS                                  
              PERFORM 1011-DELETE-QUEUE                                 
              PERFORM 1016-SET-FLAGS                                    
           WHEN SO-M-FIRST-WITH                                         
              PERFORM 1007-SET-FIRT-WITH-FLAGS                          
           WHEN SO-M-NOT-FIRST                                          
              SET SO-PROGRAM-RUNS-NOT-FIRST-TIME  TO TRUE               
           WHEN OTHER                                                   
              PERFORM 2301-SEND-INVALID-CALL-MSG                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 1006-SET-STARTING-FLAGS                         
      * PARAGRAPH WILL SET FLAGS EVERYTIME WHEN PROGRAM IS CALLED       
      * FOR THE FIRST TIME OR NOT                                       
      ******************************************************************
       1006-SET-STARTING-FLAGS.                                         
           SET SO-NOT-END-OF-QUEUE-DATA TO TRUE                         
           SET SO-NOT-END-OF-TABLE-DATA  TO TRUE                        
           .                                                            
      ******************************************************************
      *                   1007-SET-FIRT-WITH-FLAGS                      
      * PARAGRAPH WILL SET FLAGS WHEN PROGRAM MODE IS:                  
      * FIRST TIME WITH DATA                                            
      *                                                                 
      * IT WILL BE CALLED IN SCENARIO LIKE THIS BELOW:                  
      * LET'S SAY THAT DATA WAS DISPLAYED FOR THE USER - > THAT MEANS   
      * THAT SIMILAR NAMES WAS ALREADY FOUND                            
      * THEN USER MADE MISTAKE (FOR EXAMPLE CHOOSE 2 NAMES NOT 1 )      
      * THEN PROGRAM Z02141 WILL BE CALLED AND AFTER USER WILL PRESS    
      * F3 CONTROL WILL RETURN TO THIS PROGRAM                          
      * BUT THIS TIME WE ALREADY HAVE DATA SO WE DON'T HAVE TO SEARCH   
      * FOR IT ONCE AGAIN, PROGRAM CAN JUST DISPLAY THAT FORM THE QUEUE 
      * AND THIS IS WHAT THIS OPTIONS DOES                              
      *                                                                 
      ******************************************************************
       1007-SET-FIRT-WITH-FLAGS.                                        
           SET SO-FINAL-WITH-COMMAREA    TO TRUE                        
           SET SO-PROGRAM-RUNS-WITH-DATA TO TRUE                        
           SET SO-M-NOT-FIRST            TO TRUE                        
           .                                                            
      ******************************************************************
      *                      1010-IGNORE-CICS                           
      ******************************************************************
       1010-IGNORE-CICS.                                                
           EXEC CICS                                                    
            IGNORE CONDITION ERROR                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     1011-DELETE-QUEUE                           
      * PRAGRAPH WILL DELETE ALL QUEUS THAT PROGRAM USES                
      * THIS PROCESS WILL TAKE PLACE AT THE BEGINING ON THE TRANSACTION 
      ******************************************************************
       1011-DELETE-QUEUE.                                               
           PERFORM 1012-DELETE-AIRPORT-QUEUE                            
           PERFORM 1013-DELETE-COUNTRY-QUEUE                            
           PERFORM 1014-DELETE-MODEL-QUEUE                              
           PERFORM 1015-DELETE-AIRLINE-QUEUE                            
           .                                                            
      ******************************************************************
      *                     1012-DELETE-AIRPORT-QUEUE                   
      ******************************************************************
       1012-DELETE-AIRPORT-QUEUE.                                       
           EXEC CICS                                                    
            DELETEQ TS QUEUE(CT-QUEUE-AIRPORT-NAME)                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR)                                 
           THEN                                                         
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     1013-DELETE-COUNTRY-QUEUE                   
      ******************************************************************
       1013-DELETE-COUNTRY-QUEUE.                                       
           EXEC CICS                                                    
            DELETEQ TS QUEUE(CT-QUEUE-COUNTRY-NAME)                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR)                                 
           THEN                                                         
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     1014-DELETE-MODEL-QUEUE                     
      ******************************************************************
       1014-DELETE-MODEL-QUEUE.                                         
           EXEC CICS                                                    
            DELETEQ TS QUEUE(CT-QUEUE-MODEL-NAME)                       
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR)                                 
           THEN                                                         
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    1015-DELETE-AIRLINE-QUEUE                    
      ******************************************************************
       1015-DELETE-AIRLINE-QUEUE.                                       
           EXEC CICS                                                    
            DELETEQ TS QUEUE(CT-QUEUE-AIRLINE-NAME)                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR)                                 
           THEN                                                         
              CONTINUE                                                  
           ELSE                                                         
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                         1016-SET-FLAGS                          
      ******************************************************************
       1016-SET-FLAGS.                                                  
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           SET SO-PROGRAM-RUNS-FIRST-TIME  TO TRUE                      
           SET SO-M-NOT-FIRST  TO TRUE                                  
           .                                                            
      ******************************************************************
      *                           2000-PROCESS                          
      * PARAGRAPH WILL DISTRIBUTE PROGRAM LOGIC TO PARAGRAPHS           
      * DEPENDING ON THE FLAGS THAT WERE SET IN PREVIOUS PARAGRAPH      
      * (1005)   WIDER DESCRIPTION OF EACH OPTION CAN BE FOUND THERE    
      ******************************************************************
       2000-PROCESS.                                                    
           EVALUATE TRUE                                                
           WHEN SO-PROGRAM-RUNS-FIRST-TIME                              
              PERFORM 2001-PROCESS-WHEN-FIRST-TIME                      
           WHEN SO-PROGRAM-RUNS-NOT-FIRST-TIME                          
              PERFORM 2002-PROCESS-NOT-FIRST-TIME                       
           WHEN SO-PROGRAM-RUNS-WITH-DATA                               
              PERFORM 2003-PROCCES-START-WITH-DATA                      
           WHEN OTHER                                                   
              PERFORM 2301-SEND-INVALID-CALL-MSG                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2001-PROCESS-WHEN-FIRST-TIME                   
      * THIS PARAGRAPH WILL START PROCESS THAT WILL                     
      * FIND ALL WRITE INTO THE QUEUS                                   
      *  SIMILAR NAMES TO WHAT USER PROVIDED ON THE SCREEN              
      * PROGRAM WILL GENERATE ONE OUT OF 4 :                            
      *        1. SIMILAR AIRPORT NAMES                                 
      *        2. SIMILAR PLANE MODEL NAMES                             
      *        3. SIMILAR COUNTRY NAMES                                 
      *        4. SIMILAR AIRLINE NAMES                                 
      *                                                                 
      * AFTER WRTITING ALL SIMILAR NAMES TO THE QUEUES                  
      * PROGRAM WILL DISPLAY FIRST 15 OF THEM                           
      *                                                                 
      * PROGRAM WILL ALSO ALLOW USER TO PAGE (BROWSE) GENERATED DATA    
      ******************************************************************
       2001-PROCESS-WHEN-FIRST-TIME.                                    
           PERFORM 2005-PREPARE-THE-QUEUE                               
           MOVE 1 TO WS-LAST-RECORD-ID                                  
           PERFORM 2012-DISPLAY-NEXT-15                                 
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           .                                                            
      ******************************************************************
      *                  2002-PROCESS-NOT-FIRST-TIME                    
      * PARAGRAPH EVALUATES THROUGH USER CHOICE                         
      ******************************************************************
       2002-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHPF3                                                  
               SET SO-FINAL-TERMINATION TO TRUE                         
           WHEN DFHPF7                                                  
      * PARAGRAPH WILL DISPLAY PREVIOUS 15 RECORDS WITH SIMILAR NAMES   
      * ON THE SCREEN                                                   
               PERFORM 2013-DISPLAY-PREVIOUS-15                         
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
           WHEN DFHPF8                                                  
      * DISPLAYS NEXT 15 SIMILAR NAMES                                  
               PERFORM 2012-DISPLAY-NEXT-15                             
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
           WHEN DFHENTER                                                
      * USER PRESSED ENTER                                              
      * WE NEED TO CHECK WHERE HE PLACED AN 'X'                         
      * AND ACCORDING TO THAT MAKE  ACTIONS                             
               DISPLAY 'ENTER PRESSED'                                  
               PERFORM 2021-PROCESS-INPUT-DATA                          
               SET SO-M-FIRST-WITH TO TRUE                              
               SET SO-FINAL-TERMINATION TO TRUE                         
                                                                        
           WHEN OTHER                                                   
      * PARAGRAPH WILL SEND TO THE USER MESSAGE INDICATING THAT         
      * THE KEY HE PRESSED IS INVALID (DON'T HAVE ANY ACTION ASSIGNED ) 
               PERFORM 2333-SEND-INVALID-KEY-MSG                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 2003-PROCCES-WHEN-START-WITH-DATA               
      * THIS PARAGRAPH WILL BE PERFORMED FOR EXAMPLE WHEN               
      * PROGRAM IS CALLED BY ERROR ROUTINE AND WE ALREADY HAVE DATA     
      * IN OUR QUEUE                                                    
      * WE DON'T HVAE TO SEARCH FOR THIS DATA ONCE AGAIN,               
      * THIS PRARAGRAPH WILL JUST DISPLAY THAT                          
      *                                                                 
      *                                                                 
      ******************************************************************
       2003-PROCCES-START-WITH-DATA.                                    
           MOVE 1 TO WS-LAST-RECORD-ID                                  
           PERFORM 2012-DISPLAY-NEXT-15                                 
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           .                                                            
      ******************************************************************
      *                     2005-PREPARE-THE-QUEUE                      
      * PARAGRAPH WILL CALL TO PARAGRAPH WHAT WILL PREPARE SEARCH PHRASE
      *                                                                 
      * LATER PROGRAM WILL OPEN/FETCH/CLOSE CURSOR TO FETCH ALL SIMILAR 
      * NAMES  DEPENDING ON WHAT WE ARE SEARCHING FOR                   
      * DIFFERENET CURSOR WILL BE USED                                  
      *                                                                 
      * RECORDS WILL BE FETCHED FROM CURSOR TO QUEUE                    
      * AND LATER THIS QUEUE WILL BE USED TO CREATE PAGING LOGIC        
      *                                                                 
      * AFTER THIS PROCESS IS COMPLETED , QUEUES WILL BE DISPLAYED      
      * FOR THE USER                                                    
      ******************************************************************
       2005-PREPARE-THE-QUEUE.                                          
           DISPLAY '2005 PREPARE THE QUEUE PERFORMED'                   
           PERFORM 2011-PREPARE-SEARCH-PHRASE                           
           EVALUATE TRUE                                                
           WHEN SO-CHECK-COUNTRY                                        
                                                                        
             PERFORM 7001-OPEN-COUNTRY-CURSOR                           
             PERFORM 2331-WRITE-COUNTRY-TO-QUEUE                        
             PERFORM 7003-CLOSE-COUNTRY-CURSOR                          
           WHEN SO-CHECK-AIR-OIGIN                                      
           WHEN SO-CHECK-AIR-DESTINATION                                
             PERFORM 7100-OPEN-AIRPORT-CURSOR                           
             PERFORM 2010-MOVE-AIRPORTS-TO-QUEUE                        
             PERFORM 7200-CLOSE-AIRPORT-CURSOR                          
           WHEN SO-CHECK-PLANE-MODEL                                    
             PERFORM 7005-OPEN-MODEL-CURSOR                             
             PERFORM 2303-MOVE-MODELS-TO-QUEUE                          
             PERFORM 7007-CLOSE-MODEL-CURSOR                            
                                                                        
           WHEN SO-CHECK-AIRLINE-NAME                                   
             PERFORM 7010-OPEN-AIRLINE-CURSOR                           
             PERFORM 2305-AIRLINE-CURSOR-TO-Q                           
             PERFORM 7012-CLOSE-AIRLINE-CURSOR                          
           WHEN OTHER                                                   
               PERFORM 2301-SEND-INVALID-CALL-MSG                       
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID CALL  ' TO WS-Z02141-I-ERROR-MESSAGE(1)    
               SET  SO-Z02141-M-WITHOUT TO TRUE                         
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                        2008-READ-THE-QUEUE                      
      * PARAGRAPH WILL READ CORRECT QUEU DEPENDING ON WHAT              
      * WE ARE SEARCHING FOR RIGHT NOW                                  
      * PARAGRAPH WILL CALL TO VALID PARAGRAPHS THAT WILL READ          
      * VALID QUEUES ( IN THAT SITUATION )                              
      ******************************************************************
       2008-READ-THE-QUEUE.                                             
           SET SO-NOT-END-OF-QUEUE-DATA TO TRUE                         
           EVALUATE TRUE                                                
           WHEN SO-CHECK-COUNTRY                                        
             PERFORM 2307-READ-COUNTRY-QUEUE                            
           WHEN SO-CHECK-AIR-OIGIN                                      
           WHEN SO-CHECK-AIR-DESTINATION                                
             PERFORM 2308-READ-AIRPORT-QUEUE                            
           WHEN SO-CHECK-PLANE-MODEL                                    
             PERFORM 2309-READ-MODEL-QUEUE                              
           WHEN SO-CHECK-AIRLINE-NAME                                   
             PERFORM 2310-READ-AIRLINE-QUEUE                            
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'ERROR IN READ QUEUE '                              
                                TO WS-Z02141-I-ERROR-MESSAGE(1)         
               SET  SO-Z02141-M-WITHOUT TO TRUE                         
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                2010-MOVE-AIRPORTS-TO-QUEUE                      
      * THIS PRARGRAPH WILL FETCH AIRPORRT NAMES ONE BY ONE INTO THE    
      * QUEUE                                                           
      ******************************************************************
       2010-MOVE-AIRPORTS-TO-QUEUE.                                     
                                                                        
           PERFORM 7300-FETCH-AIRPORT-RECORD                            
                                                                        
           PERFORM UNTIL SO-END-OF-TABLE-DATA                           
            MOVE T02-AIRPORT-CODE           TO QUEUE-AIRPORT-CODE       
            MOVE T02-AIRPORT-FULL-NAME-TEXT TO QUEUE-AIRPORT-FULL-NAME  
            PERFORM 2302-WRITE-AIRPORT-QUEUE                            
            PERFORM 7300-FETCH-AIRPORT-RECORD                           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2011-PREPARE-SEARCH-PHRASE                    
      * PARAGRAPH WILL PREPARE SEARCH PHRASES                           
      *  IT WILL JUST ADD '%' SIGNS AT BEGNING AND END OF PHRASE        
      * ALSO IT WILL CALCULATE LENGTH OF THIS STRING                    
      *                                                                 
      * IF PROVIDED SEARCH PHRASE IS "BO" LENGTH 2 PROGRAM WILL CREATE  
      * "%BO%" WITH LENGTH = 4                                          
      ******************************************************************
       2011-PREPARE-SEARCH-PHRASE.                                      
           EVALUATE TRUE                                                
           WHEN SO-CHECK-COUNTRY                                        
                                                                        
             MOVE WS-SEARCHED-PHRASE-COUNTRY TO WS-SEARCH-PHRASE        
             PERFORM 2320-SEARCH-PHRASE-FORMAT                          
             MOVE WS-FORMATED-SEARCH-PHRASE-LEN  TO T03-COUNTRY-NAME-LEN
             MOVE WS-FORMATED-SEARCH-PHRASE TO T03-COUNTRY-NAME-TEXT    
                                                                        
           WHEN SO-CHECK-AIR-OIGIN                                      
           WHEN SO-CHECK-AIR-DESTINATION                                
                                                                        
             MOVE WS-SEARCHED-PHRASE-AIRPORT TO WS-SEARCH-PHRASE        
             PERFORM 2320-SEARCH-PHRASE-FORMAT                          
             MOVE WS-FORMATED-SEARCH-PHRASE-LEN                         
                                            TO T02-AIRPORT-FULL-NAME-LEN
             MOVE WS-FORMATED-SEARCH-PHRASE TO                          
                                              T02-AIRPORT-FULL-NAME-TEXT
                                                                        
           WHEN SO-CHECK-PLANE-MODEL                                    
                                                                        
             MOVE WS-SEARCHED-PHRASE-MODEL  TO WS-SEARCH-PHRASE         
             PERFORM 2320-SEARCH-PHRASE-FORMAT                          
             MOVE WS-FORMATED-SEARCH-PHRASE-LEN                         
                                            TO  T08-PLANE-MODEL-LEN     
             MOVE WS-FORMATED-SEARCH-PHRASE TO                          
                                              T08-PLANE-MODEL-TEXT      
           WHEN SO-CHECK-AIRLINE-NAME                                   
                                                                        
             MOVE WS-SEARCHED-PHRASE-AIRLINE  TO WS-SEARCH-PHRASE       
             PERFORM 2320-SEARCH-PHRASE-FORMAT                          
             MOVE WS-FORMATED-SEARCH-PHRASE-LEN                         
                                            TO  AIRLINE-NAME-LEN        
             MOVE WS-FORMATED-SEARCH-PHRASE TO                          
                                              AIRLINE-NAME-TEXT         
           WHEN OTHER                                                   
               PERFORM 2301-SEND-INVALID-CALL-MSG                       
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2012-DISPLAY-NEXT-15                          
      * PARAGRAPH WILL READ DATA FROM THE QUEUE AND WILL PLACE THEM     
      * ON THE SCREEN                                                   
      ******************************************************************
       2012-DISPLAY-NEXT-15.                                            
           MOVE WS-LAST-RECORD-ID TO  WS-RECORD-ITEM-ID                 
           MOVE WS-LAST-RECORD-ID TO WS-FIRST-RECORD-ID                 
                                                                        
           PERFORM 2500-INITIALIZE-MAP-DATA                             
                                                                        
           PERFORM 2600-INITIALIZE-IATA-POSITIONS                       
                                                                        
           PERFORM 2008-READ-THE-QUEUE                                  
                                                                        
           PERFORM 2210-CHECK-FOR-QIDERR                                
                                                                        
           PERFORM 2220-MOVE-QUEUE-TO-SCREEN                            
                                                                        
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                  2013-DISPLAY-PREVIOUS-15                       
      ******************************************************************
       2013-DISPLAY-PREVIOUS-15.                                        
           IF WS-FIRST-RECORD-ID - 15 >= 1 THEN                         
             SUBTRACT 15 FROM WS-FIRST-RECORD-ID                        
           ELSE                                                         
             MOVE 1 TO WS-FIRST-RECORD-ID                               
           END-IF                                                       
           MOVE WS-FIRST-RECORD-ID TO WS-RECORD-ITEM-ID                 
           PERFORM 2500-INITIALIZE-MAP-DATA                             
           PERFORM 2600-INITIALIZE-IATA-POSITIONS                       
           PERFORM 2008-READ-THE-QUEUE                                  
           PERFORM 2210-CHECK-FOR-QIDERR                                
                                                                        
                                                                        
           PERFORM 2220-MOVE-QUEUE-TO-SCREEN                            
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                  2020-CHECK-QUEUE-HAS-DATA                      
      ******************************************************************
       2020-CHECK-QUEUE-HAS-DATA.                                       
           MOVE 1 TO WS-ITER                                            
           EXEC CICS                                                    
           READQ TS                                                     
           QUEUE(CT-QUEUE-AIRPORT-NAME)                                 
           INTO(WS-QUEUE-STRUCTURE)                                     
           ITEM(WS-ITER)                                                
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(ITEMERR)                                        
              SET SO-QUEUE-DONT-HAVE-DATA TO TRUE                       
           WHEN OTHER                                                   
              PERFORM 2200-CHECK-EIBRESP                                
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2021-PROCESS-INPUT-DATA                        
      * PARAGRAPH BELLOW WILL:                                          
      * 1. RECEIVE DATA PROVIDED BY THE USER                            
      * 2. VALIDATE IF USER PROVIDED VALID INPUT                        
      * 3. GET VALUE THAT REPRESENTS NAME CHOSEN BY THE USER            
      * 4. MOVE THIS VALUE TO COMMAREA VARIABLES TO MAKE IT POSSIBLE    
      * FOR CALLING PROGRAM TO GET THIS VALUE                           
      *                                                                 
      ******************************************************************
       2021-PROCESS-INPUT-DATA.                                         
           PERFORM 2022-RECEIVE-THE-MAP                                 
           PERFORM 2023-GET-POSITION-OF-X                               
           PERFORM 2024-CHECK-IATA-AT-X-POSITION                        
           DISPLAY 'IATA AT X POSITION: '                               
                       WS-WHERE-IS-IATA(WS-POSITION-OF-X)               
      * IF EVERYTHING WENT RIGHT THEN PORGRAM WILL SAVE                 
      * FOUNDED IATA OR PLANE ID TO COMMAREA VARIABLES AND PROGRAM      
      * LOGIC WILL GO BACK TO CALLING PROGRAM                           
           EVALUATE TRUE                                                
           WHEN SO-CHECK-AIR-OIGIN                                      
               MOVE WS-SELECTED-IATA TO WS-Z02152-I-AIR-ORG             
                                                                        
           WHEN SO-CHECK-AIR-DESTINATION                                
               MOVE WS-SELECTED-IATA TO WS-Z02152-I-AIR-DES             
           WHEN SO-CHECK-COUNTRY                                        
               DISPLAY 'MOVE SELECTED IATA TO COUNTRY IATA '            
               MOVE WS-SELECTED-IATA TO WS-Z02152-I-COUNTRY-IATA        
           WHEN SO-CHECK-AIRLINE-NAME                                   
               MOVE WS-SELECTED-IATA TO WS-Z02152-I-AIRLINE-CODE        
      * IN CASE THAT WE ARE SEARCHING FOR PLANE MODEL                   
      * WE HAVE TO USE OTHER VARIABLE BECAUSE                           
      * PLANE ID IS STORED AS INT NOT AT 3 LETTER WORD LIKE             
      * STUFF ABOVE                                                     
           WHEN SO-CHECK-PLANE-MODEL                                    
               MOVE WS-SELECTED-PLANE-ID TO WS-Z02152-I-PLANE-ID        
                                                                        
      * WHEN OTHER WILL BE EXECUTED ONLY IF PROGRAM HAS INVALID DATA    
      * IT SHOULDNT HAPPEN HERE BUT JUST IN CASE THIS WHEN OTHER        
      * CLOUSE WILL BE HERE                                             
           WHEN OTHER                                                   
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INVALID CALL TO THIS PROGRAM  Z02162 '             
               TO WS-Z02141-I-ERROR-MESSAGE(1)                          
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      ******************************************************************
      *                     2022-RECEIVE-THE-MAP                        
      ******************************************************************
       2022-RECEIVE-THE-MAP.                                            
           MOVE LOW-VALUES TO MP0216I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0216') MAPSET('MP0216')                      
            INTO(MP0216I)                                               
           END-EXEC                                                     
                                                                        
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2023-GET-POSITION-OF-X                         
      * VARIABLE CHOICEI IS SAVED AS ARRAY SO WE CAN GET A PRECISE      
      * POSITION (WHERE USER PLACED AN "X" ) BY EVALUATING THROUGH ALL  
      * OF THIS ARRAY                                                   
      *                                                                 
      *                                                                 
      * PARAGRAPH WILL ALSO COUNT FOR AMOUNT OF 'X' THAT USER PLACED    
      * IF THIS NUMBER IS LESS OR MORE THAN 1 THEN PROPER MESSAGE       
      * WILL BE DISPLAYED FOR THE USER                                  
      *                                                                 
      * POSITION OF 'X' WILL BE SAVED IN WS-POSITION-OF-X VARIABLE      
      *                                                                 
      * LOW-VALUES , SPACE AND '_' ARE CONSIDERED AS EMPTY              
      *                                                                 
      * ONLY VALID SYMBOL IS 'X'                                        
      *                                                                 
      * IF USER WILL PLACE IN CHOICE FIELD SOMETHING OTHER THANT        
      * LOW-VALUES,SPACE,'X','_' THEN PROGRAM WILL DISPLAY ERROR        
      * MESSAGE                                                         
      ******************************************************************
       2023-GET-POSITION-OF-X.                                          
           MOVE 0 TO WS-X-COUNTER                                       
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 15       
             EVALUATE CHOICEI(WS-ITER)                                  
            WHEN 'X'                                                   
               MOVE WS-ITER TO WS-POSITION-OF-X                        
               ADD 1 TO WS-X-COUNTER                                   
            WHEN LOW-VALUES                                            
            WHEN SPACE                                                 
            WHEN '_'                                                   
               CONTINUE                                                
            WHEN OTHER                                                 
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                   
               MOVE 'PLEASE PLACE "X" NEXT TO CHOSEN NAME '            
               TO WS-Z02141-I-ERROR-MESSAGE(1)                         
               SET SO-Z02141-M-WITH TO TRUE                            
               PERFORM 2300-CALL-ERROR-ROUTINE                         
            END-EVALUATE                                               
          END-PERFORM                                                  
     * USER PLACED MORE 'X' THAN 1                                     
          IF WS-X-COUNTER > 1 THEN                                     
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
             MOVE 'YOU CAN NOT PLACE AN X IN MORE THAN 1 PLACE '       
              TO WS-Z02141-I-ERROR-MESSAGE(1)                          
             SET SO-Z02141-M-WITH TO TRUE                              
             PERFORM 2300-CALL-ERROR-ROUTINE                           
          END-IF                                                       
     * USER DIDN'T PALECE ANY 'X' AT ALL                               
          IF WS-X-COUNTER = 0 THEN                                     
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
             MOVE 'YOU NEED TO PLACE AN X SOMEWHERE '                  
                  TO WS-Z02141-I-ERROR-MESSAGE(1)                      
             MOVE 'IF YOU DONT SEE ANY RECORDS THEN PLEASE PRESS F3'   
                  TO WS-Z02141-I-ERROR-MESSAGE(2)                      
             MOVE ' AND CHOOSE DIFFERENT FLIGHT OPTIONS '              
                  TO WS-Z02141-I-ERROR-MESSAGE(3)                      
             SET SO-Z02141-M-WITH TO TRUE                              
             PERFORM 2300-CALL-ERROR-ROUTINE                           
          END-IF                                                       
          .                                                            
      ******************************************************************
      *                 2024-CHECK-IATA-AT-X-POSITION                   
      * PREVIOUSLY WE GET POSITION WHERE USER PLACED AN 'X'             
      * HERE WE WILL CHECK IF THIS IS VALID PLACE (NAME)                
      *                                                                 
      *                                                                 
      * WHEN PROGRAM SEARCHES FOR PLANE MODELS WE NEED TO TREAT THIS    
      * DIFFERENTLY.                                                    
      * THAT IS BECAUSE PLANE ID IS SAVED AS INT S9(9) COMP.            
      * NOT LIKE THE 3 LETTER CODE LIKE ALL OTHER STUFF HERE            
      *                                                                 
      *                                                                 
      * IF IATA ON THIS POSITION ISNT EQUAL TO 'XXX' OR IN CASE OF      
      * PLANE MODEL TO 0 THEN THIS VARIABLE WILL BE SENT TO THE USER    
      ******************************************************************
       2024-CHECK-IATA-AT-X-POSITION.                                   
           EVALUATE TRUE                                                
            WHEN SO-CHECK-AIR-OIGIN                                     
            WHEN SO-CHECK-AIR-DESTINATION                               
            WHEN SO-CHECK-COUNTRY                                       
            WHEN SO-CHECK-AIRLINE-NAME                                  
              IF WS-WHERE-IS-IATA(WS-POSITION-OF-X) = 'XXX' THEN        
                 PERFORM 2334-SEND-EMPTY-LINE-MESSAGE                   
              END-IF                                                    
      * WHEN USER PLACED AN X NEXT TO VALID AIRPORT NAME THEN           
              MOVE WS-WHERE-IS-IATA(WS-POSITION-OF-X) TO                
                                            WS-SELECTED-IATA            
            WHEN SO-CHECK-PLANE-MODEL                                   
              IF WS-WHERE-IS-PLANE-ID(WS-POSITION-OF-X) = 0 THEN        
                 PERFORM 2334-SEND-EMPTY-LINE-MESSAGE                   
              END-IF                                                    
      * WHEN USER PLACED AN X NEXT TO VALID AIRPORT NAME THEN           
              MOVE WS-WHERE-IS-PLANE-ID(WS-POSITION-OF-X) TO            
                                            WS-SELECTED-PLANE-ID        
            WHEN OTHER                                                  
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'ERROR IN 2024-CHECK IATA PARAGRAPH   '             
                                  TO WS-Z02141-I-ERROR-MESSAGE(1)       
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .                                                            
                                                                        
                                                                        
      ******************************************************************
      *                     2100-SEND-THE-MAP                           
      * WHEN PROGRAM MOVED SIMILAR NAMES TO THE SCREEN                  
      * PROGRAM WILL DISPLAY MAP (WITH THIS NAMES ) TO THE USER         
      ******************************************************************
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
           SEND MAP('MP0216') MAPSET('MP0216')                          
           FROM(MP0216O)                                                
           ERASE                                                        
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     2200-CHECK-EIBRESP                          
      * PROGRAM WILL CHECK IF ANY OF CICS'S STATEMENTS ENDED WITH       
      *ERORR EIBRESP NOT = DFHRESP(NORMAL)                              
      ******************************************************************
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
      *                   2210-CHECK-FOR-QIDERR                         
      * PARAGRAPH SIMPLY TELLS IF READQ STATEMENT ENDED WITH            
      * QIDERR OR NOT                                                   
      * QIDERR ERROR WILL HAPPEN ONLY WHEN PROGRAM TRIES TO READ        
      * QUEUE THAT DON'T EXIST                                          
      *                                                                 
      * DON'T EXIST  = WERENT CREATED BECAUSE THERE WAS NO SIMILAR NAMES
      * TO WHAT USER PROVIDED                                           
      *                                                                 
      * PROPER ERROR MESSAGE WILL BE SEND TO THE USER DEPENDING ON      
      * WHAT WE ARE SEARCHING RIGHT KNOW                                
      *                                                                 
      * IN CASE THAT ANY READ STATEMENT ENDED WITH QIDERR THEN          
      * PROPER MESSAGE WILL BE DISPLAYED BUT AFTER PRESSING 'F3' IN     
      * Z02141 PROGRAM USER WONT GO BACK TO THIS PROGRAM                
      * IT WILL GO BACK TO CALLING PROGRAM                              
      ******************************************************************
       2210-CHECK-FOR-QIDERR.                                           
           IF EIBRESP = DFHRESP(QIDERR) THEN                            
            PERFORM 2400-INITIALIZE-ERROR-MESSAGE                       
            EVALUATE TRUE                                               
            WHEN SO-CHECK-COUNTRY                                       
                                                                        
              MOVE 'TRY OTHER COUNTRY NAME '                            
                               TO WS-Z02141-I-ERROR-MESSAGE(1)          
            WHEN SO-CHECK-AIR-OIGIN                                     
            WHEN SO-CHECK-AIR-DESTINATION                               
              MOVE 'TRY OTHER AIRPORT NAME '                            
                               TO WS-Z02141-I-ERROR-MESSAGE(1)          
            WHEN  SO-CHECK-PLANE-MODEL                                  
              MOVE 'TRY OTHER PLANE MODEL NAME '                        
                               TO WS-Z02141-I-ERROR-MESSAGE(1)          
            WHEN SO-CHECK-AIRLINE-NAME                                  
              MOVE 'TRY OTHER AIRLINE NAME  '                           
                               TO WS-Z02141-I-ERROR-MESSAGE(1)          
            WHEN OTHER                                                  
              MOVE 'IN Z02162 PARAGRAPH 2210 ERROR(OTHER) '             
                               TO WS-Z02141-I-ERROR-MESSAGE(1)          
            END-EVALUATE                                                
            SET SO-Z02141-M-WITH TO TRUE                                
            SET SO-GO-BACK-TO-Z02152  TO TRUE                           
            PERFORM 2300-CALL-ERROR-ROUTINE                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2220-MOVE-QUEUE-TO-SCREEN                     
      * PARAGRAPH WILL READ QUEUE ONE ROW AFTER ANOTHER TILL THE        
      * END OF QUEUE OR MAXIMUM 15 TIMES                                
      * AFTER DOING SO                                                  
      * THIS READED RECORD WILL BE DISPLAYED ON THE SCREEN              
      ******************************************************************
       2220-MOVE-QUEUE-TO-SCREEN.                                       
           DISPLAY '2200 MOVE QUEU TO SCREEN '                          
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 15 OR  
                   SO-END-OF-QUEUE-DATA                                 
              IF WS-ITER2 = 1 THEN                                      
                MOVE WS-RECORD-ITEM-ID TO WS-FIRST-RECORD-ID            
              END-IF                                                    
                                                                        
              EVALUATE TRUE                                             
              WHEN  SO-CHECK-COUNTRY                                    
                 MOVE QUEUE-2-COUNTRY-NAME TO POLEO(WS-ITER2)           
                 MOVE QUEUE-2-COUNTRY-CODE TO WS-WHERE-IS-IATA(WS-ITER2)
              WHEN SO-CHECK-AIR-OIGIN                                   
              WHEN SO-CHECK-AIR-DESTINATION                             
                MOVE QUEUE-AIRPORT-FULL-NAME(1:70) TO   POLEO(WS-ITER2) 
                MOVE QUEUE-AIRPORT-CODE TO WS-WHERE-IS-IATA(WS-ITER2)   
              WHEN SO-CHECK-PLANE-MODEL                                 
                 MOVE QUEUE-3-PLANE-MODEL  TO POLEO(WS-ITER2)           
                 MOVE QUEUE-3-PLANE-ID     TO WS-WHERE-IS-PLANE-ID      
                                                          (WS-ITER2)    
                                                                        
              WHEN SO-CHECK-AIRLINE-NAME                                
                 MOVE QUEUE-4-AIRLINE-NAME  TO POLEO(WS-ITER2)          
                 MOVE QUEUE-4-AIRLINE-CODE TO WS-WHERE-IS-IATA(WS-ITER2)
              WHEN OTHER                                                
                 PERFORM 2400-INITIALIZE-ERROR-MESSAGE                  
                 MOVE 'OTHER ERROR IN 2220 PROG Z02162 '                
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
                 SET SO-Z02141-M-WITH TO TRUE                           
                 PERFORM 2300-CALL-ERROR-ROUTINE                        
              END-EVALUATE                                              
              MOVE  WS-RECORD-ITEM-ID TO WS-LAST-RECORD-ID              
              ADD 1 TO WS-RECORD-ITEM-ID                                
                                                                        
              INITIALIZE WS-QUEUE-STRUCTURE                             
              INITIALIZE WS-QUEUE-2-STRUCTURE                           
              INITIALIZE WS-QUEUE-3-STRUCTURE                           
              INITIALIZE WS-QUEUE-4-STRUCTURE                           
                                                                        
              PERFORM 2008-READ-THE-QUEUE                               
                                                                        
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2300-CALL-ERROR-ROUTINE                       
      * THIS PARAGRAPH WILL BE CALLED WHEN USER HAS TO                  
      * CALL FOR ERROR ROUTINE                                          
      *                                                                 
      * THIS PROGRAM WILL ALSO CHOOSE WHERE LOGIC OF THE PROGRAM        
      * WHOULD RETURN (THIS PROGRAM OR CALLING PROGRAM)                 
      *                                                                 
      *                                                                 
      ******************************************************************
       2300-CALL-ERROR-ROUTINE.                                         
           IF SO-GO-BACK-TO-Z02152 THEN                                 
             MOVE CT-Z02152-PROG-NAME TO WS-Z02141-I-CALLING-PROGRAM    
             SET SO-GO-BACK-TO-THIS-PROG TO TRUE                        
              IF WS-RETURN-CONTROL-PROGRAM NOT = SPACE OR LOW-VALUES    
              THEN                                                      
                 MOVE WS-RETURN-CONTROL-PROGRAM TO                      
                                             WS-Z02141-I-CALLING-PROGRAM
              END-IF                                                    
              IF SO-CHECK-COUNTRY THEN                                  
                MOVE CT-RESERVATION-PROGRAM TO                          
                         WS-Z02141-I-CALLING-PROGRAM                    
              END-IF                                                    
           ELSE                                                         
             MOVE CT-THIS-PROGRAM-NAME TO WS-Z02141-I-CALLING-PROGRAM   
           END-IF                                                       
                                                                        
           DISPLAY 'PRZED CALL DO ERROR ROUTINE: '                      
                                  WS-Z02141-I-CALLING-PROGRAM           
           SET  SO-Z02141-I-FIRST-TIME TO TRUE   
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-ERROR-ROUTINE-NAME) COMMAREA(DFHCOMMAREA)   
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                        2301-SEND-INVALID-CALL-MSG               
      ******************************************************************
       2301-SEND-INVALID-CALL-MSG.                                      
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE ' INVALID CALL TO Z02162 PROGRAM       '                
                                  TO WS-Z02141-I-ERROR-MESSAGE(1)       
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                    2302-WRITE-AIRPORT-QUEUE                     
      ******************************************************************
       2302-WRITE-AIRPORT-QUEUE.                                        
           EXEC CICS                                                    
           WRITEQ TS                                                    
            QUEUE(CT-QUEUE-AIRPORT-NAME)                                
            FROM(WS-QUEUE-STRUCTURE)                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                      2303-MOVE-MODELS-TO-QUEUE                  
      * PARAGRAPH WILL FETCH MODELS ONE BY ONE FROM DATABASE (          
      * ONLY THOSE THAT MEETS CRITERIA ) AND PUTS IT INTO THE           
      * MODEL QUEUE                                                     
      ******************************************************************
       2303-MOVE-MODELS-TO-QUEUE.                                       
                                                                        
           PERFORM 7008-FETCH-MODEL-RECORD                              
                                                                        
           PERFORM UNTIL SO-END-OF-TABLE-DATA                           
            MOVE T08-PLANE-ID          TO QUEUE-3-PLANE-ID              
            MOVE T08-PLANE-MODEL-TEXT  TO QUEUE-3-PLANE-MODEL           
            PERFORM 2304-WRITE-MODEL-QUEUE                              
            PERFORM 7008-FETCH-MODEL-RECORD                             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      2304-WRITE-MODEL-QUEUE                     
      ******************************************************************
       2304-WRITE-MODEL-QUEUE.                                          
           EXEC CICS                                                    
           WRITEQ TS                                                    
            QUEUE(CT-QUEUE-MODEL-NAME)                                  
            FROM(WS-QUEUE-3-STRUCTURE)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2305-AIRLINE-CURSOR-TO-Q                      
      * PARAGRAPH WILL FETCH ALL AIRLINE NAMES THAT MEETS CRITERIA      
      * FROM DATA BASE AND WILL PUT IT INTO THE AIRLINE QUEUE           
      ******************************************************************
       2305-AIRLINE-CURSOR-TO-Q.                                        
           PERFORM 7013-FETCH-AIRLINE-NAME                              
                                                                        
           PERFORM UNTIL SO-END-OF-TABLE-DATA                           
            MOVE AIRLINE-CODE          TO QUEUE-4-AIRLINE-CODE          
            MOVE AIRLINE-NAME-TEXT     TO QUEUE-4-AIRLINE-NAME          
            PERFORM 2306-WRITE-AIRLINE-QUEUE                            
            PERFORM 7013-FETCH-AIRLINE-NAME                             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2306-WRITE-AIRLINE-QUEUE                     
      ******************************************************************
       2306-WRITE-AIRLINE-QUEUE.                                        
           EXEC CICS                                                    
           WRITEQ TS                                                    
            QUEUE(CT-QUEUE-AIRLINE-NAME)                                
            FROM(WS-QUEUE-4-STRUCTURE)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2307-READ-COUNTRY-QUEUE                        
      ******************************************************************
       2307-READ-COUNTRY-QUEUE.                                         
           EXEC CICS                                                    
           READQ TS                                                     
           QUEUE(CT-QUEUE-COUNTRY-NAME)                                 
           INTO(WS-QUEUE-2-STRUCTURE)                                   
           ITEM(WS-RECORD-ITEM-ID)                                      
           NOHANDLE                                                     
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-DATA TO TRUE                           
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2308-READ-AIRPORT-QUEUE                       
      ******************************************************************
       2308-READ-AIRPORT-QUEUE.                                         
           EXEC CICS                                                    
           READQ TS                                                     
           QUEUE(CT-QUEUE-AIRPORT-NAME)                                 
           INTO(WS-QUEUE-STRUCTURE)                                     
           ITEM(WS-RECORD-ITEM-ID)                                      
           NOHANDLE                                                     
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-DATA TO TRUE                           
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                2309-READ-MODEL-QUEUE                            
      ******************************************************************
       2309-READ-MODEL-QUEUE.                                           
           EXEC CICS                                                    
           READQ TS                                                     
            QUEUE(CT-QUEUE-MODEL-NAME)                                  
            INTO(WS-QUEUE-3-STRUCTURE)                                  
            ITEM(WS-RECORD-ITEM-ID)                                     
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-DATA TO TRUE                           
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2310-READ-AIRLINE-QUEUE                     
      ******************************************************************
       2310-READ-AIRLINE-QUEUE.                                         
           EXEC CICS                                                    
           READQ TS                                                     
            QUEUE(CT-QUEUE-AIRLINE-NAME)                                
            INTO(WS-QUEUE-4-STRUCTURE)                                  
            ITEM(WS-RECORD-ITEM-ID)                                     
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(ITEMERR)                                        
             SET SO-END-OF-QUEUE-DATA TO TRUE                           
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2320-SEARCH-PHRASE-FORMAT                   
      ******************************************************************
       2320-SEARCH-PHRASE-FORMAT.                                       
           MOVE 0 TO WS-ITER                                            
           INSPECT WS-SEARCH-PHRASE TALLYING WS-ITER                    
           FOR CHARACTERS BEFORE INITIAL SPACE                          
           DISPLAY 'Z02162 WS-ITER: ' WS-ITER                           
           STRING '%'  WS-SEARCH-PHRASE(1:WS-ITER) '%'                  
           DELIMITED BY SIZE                                            
           INTO WS-FORMATED-SEARCH-PHRASE                               
           END-STRING                                                   
           ADD 2 TO WS-ITER                                             
           MOVE WS-ITER TO WS-FORMATED-SEARCH-PHRASE-LEN                
           .                                                            
      ******************************************************************
      *                    2331-WRITE-COUNTRY-TO-QUEUE                  
      ******************************************************************
       2331-WRITE-COUNTRY-TO-QUEUE.                                     
                                                                        
           PERFORM 7004-FETCH-COUNTRY-RECORD                            
                                                                        
           PERFORM UNTIL SO-END-OF-TABLE-DATA                           
            MOVE T03-COUNTRY-CODE      TO QUEUE-2-COUNTRY-CODE          
            MOVE T03-COUNTRY-NAME-TEXT TO QUEUE-2-COUNTRY-NAME          
            PERFORM 2332-WRITE-COUNTRY-QUEUE                            
            PERFORM 7004-FETCH-COUNTRY-RECORD                           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2332-WRITE-COUNTRY-QUEUE                    
      ******************************************************************
       2332-WRITE-COUNTRY-QUEUE.                                        
           EXEC CICS                                                    
           WRITEQ TS                                                    
            QUEUE(CT-QUEUE-COUNTRY-NAME)                                
            FROM(WS-QUEUE-2-STRUCTURE)                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2333-SEND-INVALID-KEY-MSG                     
      ******************************************************************
       2333-SEND-INVALID-KEY-MSG.                                       
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'NO-ACTION KEY ' TO WS-Z02141-I-ERROR-MESSAGE(1)        
           SET  SO-Z02141-M-WITH TO TRUE                                
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                     2334-SEND-EMPTY-LINE-MESSAGE                
      * PARAGRAPH WILL DISPLAY MESSAGE INDICATING THAT USER             
      * PLACED 'X' ENXT TO EMPTY LINE                                   
      ******************************************************************
       2334-SEND-EMPTY-LINE-MESSAGE.                                    
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'PLACE AN "X" NEXT TO NON EMPTY LINE  '                 
                              TO WS-Z02141-I-ERROR-MESSAGE(1)           
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                2300-CALL-ERROR-ROUTINE                          
      ******************************************************************
       2500-INITIALIZE-MAP-DATA.                                        
           INITIALIZE MP0216O                                           
                                                                        
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 15       
              MOVE '_' TO CHOICEO(WS-ITER)                              
              MOVE LOW-VALUES TO CHOICEA(WS-ITER)                       
              MOVE LOW-VALUES TO POLEA(WS-ITER)                         
              MOVE SPACE TO POLEO(WS-ITER)                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *             2400-INITIALIZE-ERROR-MESSAGE                       
      ******************************************************************
       2400-INITIALIZE-ERROR-MESSAGE.                                   
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 10       
             MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER)           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2600-INITIALIZE-IATA-POSITIONS                  
      ******************************************************************
       2600-INITIALIZE-IATA-POSITIONS.                                  
           EVALUATE TRUE                                                
           WHEN SO-CHECK-PLANE-MODEL                                    
             PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 15     
                  MOVE 0     TO WS-WHERE-IS-PLANE-ID(WS-ITER)           
             END-PERFORM     
           WHEN OTHER                                                   
             PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 15     
                  MOVE 'XXX' TO WS-WHERE-IS-IATA(WS-ITER)               
             END-PERFORM                                                
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                         3000-FINAL                              
      * PROGRAM WILL ALLOW PROGRAM TO RETURN TO CALLING PROGRAM         
      * AND WILL ALLOW FOR PSEUDO-CONVERSATIONAL PROCESSING             
      *****************************************************************I
       3000-FINAL.                                                      
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3001-RETURN-WITH-TRANSID                          
                                                                        
           WHEN SO-FINAL-TERMINATION                                    
              IF SO-CHECK-COUNTRY THEN                                  
                PERFORM 3002-CALL-RESERVATION-PROG                      
              ELSE                                                      
                PERFORM 3003-CALL-TO-CALLING-PROG                       
              END-IF                                                    
                                                                        
           WHEN OTHER                                                   
              PERFORM 3004-SEND-ERROR-MESSAGE                           
                                                                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     3001-RETURN-WITH-TRANSID                    
      * EXECUTING THIS CICS'S STATEMENT WILL RESULT IN TERMINATION      
      * OF THIS TRANSACTION (WITH OPTION TO RETRIGER AFTER USER WILL    
      * PRESS ANTENTION KEY)                                            
      ******************************************************************
       3001-RETURN-WITH-TRANSID.     
           EXEC CICS                                                    
             RETURN TRANSID('0210') COMMAREA(DFHCOMMAREA)               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     3002-CALL-RESERVATION-PROG                  
      * PROGRAM GOES BACK TO RESERVATION PROGRAM (Z02202)               
      *                                                                 
      * THIS IS THE PROGRAM WHEN USER HAS TO PROVIDE DATA ABOUT         
      * PASSENGERS                                                      
      ******************************************************************
       3002-CALL-RESERVATION-PROG.                                      
           DISPLAY '3002 - CALL TO RESERVATION PROGRAM '                
           DISPLAY 'CT-RESERV-PROG: ' CT-RESERVATION-PROGRAM            
           SET SO-M-FIRST-WITH TO TRUE                                  
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(CT-RESERVATION-PROGRAM)                        
              COMMAREA(DFHCOMMAREA)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                      3003-CALL-TO-CALLING-PROG                  
      * PARAGRAPH WILL RETURN CONTROL TO CALLING PROGRAM                
      *                                                                 
      ******************************************************************
       3003-CALL-TO-CALLING-PROG.                                       
           SET SO-M-FIRST-WITH TO TRUE                                  
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EXEC CICS                                                    
            XCTL PROGRAM(WS-RETURN-CONTROL-PROGRAM)                     
              COMMAREA(DFHCOMMAREA)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP
           .                                                            
      ******************************************************************
      *                      3004-SEND-ERROR-MESSAGE                    
      * PROGRAM SENDS ERROR MESSAGE IN CASE THAT                        
      * THERE IS (OTHER) OPTION IN 3000 PARAGRAPH                       
      ******************************************************************
       3004-SEND-ERROR-MESSAGE.                                         
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'SERIOUS ERROR IN ROUTINE ' TO                          
                              WS-Z02141-I-ERROR-MESSAGE(1)              
           SET SO-Z02141-M-WITH   TO TRUE                               
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                      7001-OPEN-COUNTRY-CURSOR                   
      ******************************************************************
       7001-OPEN-COUNTRY-CURSOR.                                        
           DISPLAY '7001 OTWARCIE C-COUNTRY-CURSOR'                     
           EXEC SQL                                                     
            OPEN  C-COUNTRY-CURSOR                                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7003-CLOSE-COUNTRY-CURSOR                  
      ******************************************************************
       7003-CLOSE-COUNTRY-CURSOR.                                       
           EXEC SQL                                                     
            CLOSE  C-COUNTRY-CURSOR                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN   
              SET SO-7003-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7004-FETCH-COUNTRY-RECORD                       
      ******************************************************************
       7004-FETCH-COUNTRY-RECORD.                                       
           SET SO-NOT-END-OF-TABLE-DATA  TO TRUE                        
           INITIALIZE T03-COUNTRY-CODE                                  
           INITIALIZE T03-COUNTRY-NAME                                  
           EXEC SQL                                                     
            FETCH C-COUNTRY-CURSOR                                      
            INTO                                                        
            :T03-COUNTRY-CODE,                                          
            :T03-COUNTRY-NAME                                           
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               DISPLAY 'Z02162, 7004 SUCCESSFULL FETCH '                
           WHEN SO-SQLCODE-NOT-FOUND                                    
               DISPLAY 'END OF INPUT'                                   
               SET SO-END-OF-TABLE-DATA   TO TRUE                       
           WHEN OTHER                                                   
               SET SO-7004-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7005-OPEN-MODEL-CURSOR                      
      ******************************************************************
       7005-OPEN-MODEL-CURSOR.                                          
           EXEC SQL                                                     
            OPEN   C-PLANE-MODEL-CURSOR                                 
           END-EXEC                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7005-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7007-CLOSE-MODEL-CURSOR                    
      ******************************************************************
       7007-CLOSE-MODEL-CURSOR.                                         
           EXEC SQL                                                     
            CLOSE  C-PLANE-MODEL-CURSOR                                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7007-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7008-FETCH-MODEL-RECORD                    
      ******************************************************************
       7008-FETCH-MODEL-RECORD.                                         
           INITIALIZE T08-PLANE-ID                                      
           INITIALIZE T08-PLANE-MODEL                                   
           EXEC SQL                                                     
            FETCH C-PLANE-MODEL-CURSOR                                  
            INTO                                                        
            :T08-PLANE-ID,                                              
            :T08-PLANE-MODEL                                            
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7008-SUCCESSFULL-FETCH '                         
              CONTINUE               
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-TABLE-DATA TO TRUE                          
           WHEN OTHER                                                   
              SET SO-7008-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7010-OPEN-AIRLINE-CURSOR                    
      ******************************************************************
       7010-OPEN-AIRLINE-CURSOR.                                        
           EXEC SQL                                                     
            OPEN C-AIRLINE-NAME-CURSOR                                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7010-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7012-CLOSE-AIRLINE-CURSOR                    
      ******************************************************************
       7012-CLOSE-AIRLINE-CURSOR.                                       
           EXEC SQL                                                     
            CLOSE C-AIRLINE-NAME-CURSOR                                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7012-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7013-FETCH-AIRLINE-NAME                         
      ******************************************************************
       7013-FETCH-AIRLINE-NAME.                                         
           SET SO-NOT-END-OF-TABLE-DATA TO TRUE                         
           INITIALIZE AIRLINE-CODE                                      
           INITIALIZE AIRLINE-NAME                                      
           EXEC SQL                                                     
            FETCH C-AIRLINE-NAME-CURSOR                                 
            INTO                                                        
            :AIRLINE-CODE,                                              
            :AIRLINE-NAME                                               
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7013-SUCCESSFULL-FETCH '                         
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY '7013 - END OF FETCH DATA 100'                    
              SET SO-END-OF-TABLE-DATA TO TRUE                          
           WHEN OTHER                                                   
              SET SO-7013-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7100-OPEN-AIRPORT-CURSOR                   
      ******************************************************************
       7100-OPEN-AIRPORT-CURSOR.                                        
           EXEC SQL                                                     
            OPEN C-AIRPORT-CURSOR                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7100-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7200-CLOSE-AIRPORT-CURSOR                   
      ******************************************************************
       7200-CLOSE-AIRPORT-CURSOR.                                       
           EXEC SQL                                                     
            CLOSE  C-AIRPORT-CURSOR                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7200-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7300-FETCH-AIRPORT-RECORD                   
      ******************************************************************
       7300-FETCH-AIRPORT-RECORD.                                       
           EXEC SQL                                                     
            FETCH C-AIRPORT-CURSOR                                      
            INTO                                                        
            :T02-AIRPORT-CODE,                                          
            :T02-AIRPORT-FULL-NAME                                      
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
               DISPLAY 'Z02162, 7300 SUCCESSFULL FETCH '                
      *        CONTINUE                                                 
           WHEN SO-SQLCODE-NOT-FOUND                                    
               DISPLAY 'END OF INPUT'                                   
               SET SO-END-OF-TABLE-DATA   TO TRUE                       
           WHEN OTHER                                                   
               SET SO-7300-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      9000-DB2-ERROR                             
      * PARAGRAPH WILL CREATE DB2 ERROR MESSAGE AND WILL CALL TO        
      * ERROR ROUTINE (Z02141 PROGRAM)                                  
      *                                                                 
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
      *                        9100-ROLLBACK                 
      ******************************************************************
       9100-ROLLBACK.                                                   
           EXEC CICS                                                    
            SYNCPOINT ROLLBACK                                          
           END-EXEC                                                     
           .                                                                       
                                   
                                
                                 
                                   
                                   
                                           
                                                                        
                       
                                                                        

                                                 
