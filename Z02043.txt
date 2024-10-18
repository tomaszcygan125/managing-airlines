       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02043.                                              
       AUTHOR.        TOMASZ CYGAN                                      
       DATE-WRITTEN.  06/06/2024.                                       
       DATE-COMPILED. 06/06/2024.                                       
                                                                        
      ***************************************************************** 
      *                      Z02043                                     
      * PROGRAM VALIDATES IF GIVEN DATE IS CORRECT                      
      * USER CAN GET ERROR CODES AS BELOW, ALSO USER WILL GET ERROR     
      * MESSASGE AND STATEMENT IDENTIFICATOR                            
      *                                                                 
      *        ERROR CODES                                              
      *    A.  DAY MONTH OR YEAR NOT NUMERIC                            
      *    B.  DELIMITED IS NOT '-'                                     
      *    C.  EMPTY INPUT                                              
      *    D.  VALUE OF MONTH OR DAY IS INCORECT                        
      *    E.  INCORECT TYPE OF DATE                                    
      *    F.  OTHER ERROR                                              
      ***************************************************************** 
      *                 CHANGE LOG                                      
      *                                                                 
      * CHANGE NAME    YY-MM-DD DESCRIPTION                             
      * ------ ------  -------- --------------------------------------- 
      *        TSOUS02 24-06-10 SEC VERSION                             
      *                                                                 
      *                                                                 
      ***************************************************************** 
      ***************************************************************   
      *         DATA DIVISION                                           
      ***************************************************************   
       DATA DIVISION.                                                   
      ***************************************************************   
      *             WORKING-STORAGE SECTION                             
      ***************************************************************   
       WORKING-STORAGE SECTION.       
                                                                        
       01 CT-CONSTANTS.                                                 
           05 CT-GO-BACK-TO-THIS               PIC X(8) VALUE 'Z02152 '.
           05 CT-ERROR-ROUTINE-NAME            PIC X(8) VALUE 'Z02141 '.
       01 WS-VARIABLES.                                                 
                                                                        
           05 WS-DATE-VALUE.                                            
               10 WS-YEAR           PIC 9999.                           
               10 WS-1ST-DELIMITER  PIC X.                              
               10 WS-MONTH          PIC 99.                             
               10 WS-2ND-DELIMITER  PIC X.                              
               10 WS-DAY            PIC 99.                             
                                                                        
           05 WS-DATE-VALUE-8-CHAR.                                     
               10 WS-8-CHAR-YEAR    PIC 9999.                           
               10 WS-8-CHAR-MONTH   PIC 99.                             
               10 WS-8-CHAR-DAY     PIC 99.                             
                                                                        
       01 SW-SWITCHES.                                                  
           05 SW-IF-LEAP-YEAR       PIC X.                              
              88 SO-LEAP-YEAR       VALUE 'Y'.                          
              88 SO-NOT-LEAP-YEAR   VALUE 'N'.                          
           05 SW-WHAT-MONTH         PIC 99.                             
              88 SO-MONTH-WITH-MAX-31-DAYS VALUE  01 03 05 07 09 11.    
              88 SO-MONTH-FEBUARY          VALUE  02.                   
              88 SO-MONTH-WITH-MAX-30-DAYS VALUE  04 06 08 10 12.       
                                                                        
      ***************************************************************   
      *                  LINKAGE SECTION                                
      ***************************************************************   
       LINKAGE SECTION.                                                 
          COPY  ZZEC0243.                                               
      ***************************************************************   
      *                   PROCEDURE DIVISION                            
      ***************************************************************   
       PROCEDURE DIVISION USING ZZEC0243.                               
                                                                        
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           PERFORM 3000-FINAL.                                          
                                                                        
      ***************************************************************   
      *                        1000-INIT                                
      * INITIALIZATION OF PROGRAM VARIABLES                             
      *                                                                 
      ***************************************************************   
       1000-INIT.                                                       
                                                                        
      * SETTING THAT RETURN CODE  IS UNKNOWN                            
      * IN CASE THAT PROGRAM WILL NOT END WITH SUCCESS AND NOT          
      * WITH ANY KNOWN ERROR                                            
                                                                        
           MOVE 'UNKNOWN ERROR ' TO ZZEC0243-O-ERROR-MSG                
           SET ZZEC0243-O-RC-UNKNOWN TO TRUE                            
           INITIALIZE WS-VARIABLES                                      
           .                                                            
      **************************************************************    
      *                      2000-PROCESS                               
      **************************************************************    
       2000-PROCESS.                                                    
      * CHECK IF INPUT IS EMPTY OR NOT                                  
           PERFORM 2200-CHECK-IF-EMPTY                                  
      * MAKE DECISION BASED ON WHAT TYPE OF DATE WE ARE DEALING WITH    
                                                                        
           EVALUATE TRUE                                                
             WHEN ZZEC0243-M-10-CHAR                                    
                                                                        
                 PERFORM 2005-CHECK-10-CHAR-DATE                        
                                                                        
             WHEN ZZEC0243-M-8-CHAR                                     
                                                                        
                 PERFORM 2006-CHECK-8-CHAR-DATE                         
                                                                        
             WHEN OTHER                                                 
                 PERFORM 2007-SET-ERROR-WRONG-TYPE                      
           END-EVALUATE                                                 
      *                                                                 
           SET ZZEC0243-SO-NONE-PARA        TO TRUE                     
           SET ZZEC0243-O-RC-NO-ERROR       TO TRUE                     
           MOVE SPACE                       TO ZZEC0243-O-ERROR-MSG     
           .                                                            
      ******************************************************************
      *                  2005-CHECK-10-CHAR-DATE                        
      *                                                                 
      * PARAGRAPHS WILL CHECK                                           
      * 1: IF YEAR MONTH AND DAY VALUE IS NUMERIC                       
      * 2: IF DATE DELIMITER = '-'                                      
      * 3: IF VALUE OF MONTH > 0 <= 12 AND IF DAY IS IN CORECT RANGE    
      * DEPENDING ON YEAR AND MONTH                                     
      *                                                                 
      ******************************************************************
       2005-CHECK-10-CHAR-DATE.                                         
                                                                        
               PERFORM 2010-FORMAT-10-CHAR-DATE                         
                                                                        
               PERFORM 2110-CHECK-IF-NUMERIC                            
                                                                        
               PERFORM 2120-CHECK-DELIMITER                             
                                                                        
               PERFORM 2130-CHECK-Y-M-D-VALUE                           
                                                                        
           .                                                            
      ******************************************************************
      *                  2006-CHECK-8-CHAR-DATE                         
      *                                                                 
      * PARAGRAPHS WILL CHECK                                           
      * 1: IF YEAR MONTH AND DAY VALUE IS NUMERIC                       
      * 2: IF VALUE OF MONTH > 0 <= 12 AND IF DAY IS IN CORECT RANGE    
      * DEPENDING ON YEAR AND MONTH                                     
      *                                                                 
      ******************************************************************
       2006-CHECK-8-CHAR-DATE.                                          
               PERFORM 2020-FORMAT-CHAR-8-DATE                          
                                                                        
               PERFORM 2110-CHECK-IF-NUMERIC                            
                                                                        
               PERFORM 2130-CHECK-Y-M-D-VALUE                           
           .                                                            
      ******************************************************************
      *                 2007-SET-ERROR-WRONG-TYPE                       
      *                                                                 
      * SETTING ERROR OF WRONG TYPE OF DATE                             
      * AND RETURNING CONTROL TO CALLING PROGRAM                        
      ******************************************************************
       2007-SET-ERROR-WRONG-TYPE.                                       
           SET ZZEC0243-O-RC-WRONG-TYPE TO TRUE                         
           MOVE 'WRONG TYPE OF DATE '   TO ZZEC0243-O-ERROR-MSG         
           SET ZZEC0243-SO-2000-PARA    TO TRUE                         
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                  2010-FORMAT-10-CHAR-DATE                       
      *                                                                 
      * MOVING DATE PARTS TO PROGRAM VARIABLES                          
      ******************************************************************
       2010-FORMAT-10-CHAR-DATE.                                        
           MOVE ZZEC0243-I-DATE-VALUE TO WS-DATE-VALUE                  
           .                                                            
                                                                        
      ******************************************************************
      *                  2020-FORMAT-CHAR-8-DATE                        
      *                                                                 
      * DATA FROM COMMAREA IS PUT IN GROUP VARIABLE                     
      * AND VARIABLES FROM THAT GROUP ARE PUT IN VARIABLES THAT ARE     
      * SAME FOR BOTH TYPES OF DATE                                     
      ******************************************************************
       2020-FORMAT-CHAR-8-DATE.                                         
           MOVE ZZEC0243-I-DATE-VALUE      TO WS-DATE-VALUE-8-CHAR      
                                                                        
           MOVE WS-8-CHAR-YEAR             TO WS-YEAR                   
           MOVE WS-8-CHAR-MONTH            TO WS-MONTH                  
           MOVE WS-8-CHAR-DAY               TO WS-DAY                   
           .                                                            
      ******************************************************************
      *                     2110-CHECK-IF-NUMERIC                       
      * CHECKS IF YEAR , MONTH, AND DAY VALUE ARE NUMBERS               
      ******************************************************************
       2110-CHECK-IF-NUMERIC.                                           
                                                                        
           IF WS-MONTH IS NUMERIC                                       
              AND                                                       
              WS-YEAR IS NUMERIC                                        
              AND                                                       
              WS-DAY IS NUMERIC THEN                                    
            CONTINUE                                                    
           ELSE                                                         
              SET ZZEC0243-O-RC-NOT-NUMERIC  TO TRUE                    
              MOVE 'YEAR OR MONTH OR DAY NOT NUMERIC' TO                
                                                   ZZEC0243-O-ERROR-MSG 
              SET ZZEC0243-SO-2110-PARA      TO TRUE                    
              PERFORM 3000-FINAL                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2120-CHECK-DELIMITER                        
      * CHECKS IF DATE DELIMITER IS EQUAL TO '-'                        
      * PARAGRAPH TAKES  PLACE ONLY WHEN DATE TYPE IS YYYY-MM-DD        
      * AND BECAUSE OF THAT WE USE REFERENCE MODIFICATON                
      ******************************************************************
       2120-CHECK-DELIMITER.       
           IF WS-1ST-DELIMITER = '-' AND                                
              WS-2ND-DELIMITER = '-' THEN                               
             CONTINUE                                                   
           ELSE                                                         
                                                                        
             SET  ZZEC0243-O-RC-WRONG-DELIMITER  TO TRUE                
             MOVE ' DELIMITER WAS NOT EQUAL TO - ' TO                   
                                                    ZZEC0243-O-ERROR-MSG
             SET ZZEC0243-SO-2120-PARA  TO TRUE                         
             PERFORM 3000-FINAL                                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2130-CHECK-Y-M-D-VALUE                      
      * CHECK IF MONTH >  0 AND <= 12                                   
      * CHECK IF DAY IS > 0 AND LESS THAT 31 OR 30 OR 28 OR 29          
      * DEPENDING ON VALUE OF THE MONTH  AND YEAR                       
      *                                                                 
      ******************************************************************
       2130-CHECK-Y-M-D-VALUE.                                          
                                                                        
      * END ROUTINE IN CASE THAT USER SPECIFIED DAY 00                  
           IF WS-DAY = 00 THEN                                          
              PERFORM  2400-SET-WRRONG-VALUE-ERROR                      
           END-IF                                                       
                                                                        
                                                                        
           MOVE WS-MONTH    TO SW-WHAT-MONTH                            
                                                                        
           EVALUATE TRUE                                                
             WHEN SO-MONTH-WITH-MAX-31-DAYS                             
                                                                        
               IF WS-DAY > 31  THEN                                     
                  PERFORM 2400-SET-WRRONG-VALUE-ERROR                   
               END-IF      
             WHEN SO-MONTH-FEBUARY                                      
                                                                        
               PERFORM 2300-CHECK-IF-LEAP-YEAR                          
                                                                        
               IF SO-LEAP-YEAR THEN                                     
      * MAX VALUE FOR FEBUARY IN THE LEAP YEAR IS 29                    
                                                                        
                  IF WS-DAY >  29 THEN                                  
                    PERFORM 2400-SET-WRRONG-VALUE-ERROR                 
                  END-IF                                                
                                                                        
               ELSE                                                     
      * IN NOMRAL YEAR FOR FEBUARY WE GET MAX OF 28                     
                                                                        
                  IF WS-DAY > 28  THEN                                  
                    PERFORM 2400-SET-WRRONG-VALUE-ERROR                 
                  END-IF                                                
               END-IF                                                   
                                                                        
             WHEN SO-MONTH-WITH-MAX-30-DAYS                             
      * MAX DAY VALUE IS 30 HERE                                        
                                                                        
               IF WS-DAY > 30 THEN                                      
                    PERFORM 2400-SET-WRRONG-VALUE-ERROR                 
               END-IF                                                   
                                                                        
             WHEN OTHER                                                 
      * WRONG MONTH                                                     
               PERFORM 2400-SET-WRRONG-VALUE-ERROR                      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2200-CHECK-IF-EMPTY                          
      *                                                                 
      * CHECK IF INPUT DATE IS EMPTY OR NOT                             
      ******************************************************************
       2200-CHECK-IF-EMPTY.                                             
           IF ZZEC0243-I-DATE-VALUE = SPACES                            
                             OR                                         
              ZZEC0243-I-DATE-VALUE = LOW-VALUES                        
           THEN                                                         
              SET ZZEC0243-O-RC-EMPTY-INPUT TO TRUE                     
              MOVE 'EMPTY INPUT '           TO ZZEC0243-O-ERROR-MSG     
              SET ZZEC0243-SO-2200-PARA     TO TRUE                     
              PERFORM 3000-FINAL                                        
           END-IF                                                       
           .                                                            
      ***************************************************************** 
      *                    2300-CHECK-IF-LEAP-YEAR                      
      * PARAGRAPH CHECKS IF GIVEN YEAR IS LEAP OR NOT                   
      ***************************************************************** 
       2300-CHECK-IF-LEAP-YEAR.                                         
           IF FUNCTION MOD(WS-YEAR, 400) = 0                            
              OR (                                                      
              FUNCTION MOD(WS-YEAR,  4) = 0                             
              AND   FUNCTION MOD(WS-YEAR, 100) NOT = 0  )               
           THEN                                                         
              SET SO-LEAP-YEAR     TO TRUE                              
           ELSE                                                         
              SET SO-NOT-LEAP-YEAR TO TRUE                              
           END-IF                                                       
                                                                        
           .                                                            
      ***************************************************************** 
      *                     2350-CALL-ERROR-ROUTINE                     
      ***************************************************************** 
      *2350-CALL-ERROR-ROUTINE.                                         
      *    MOVE CT-GO-BACK-TO-THIS TO WS-Z02141-I-CALLING-PROGRAM       
      *    SET SO-Z02141-I-FIRST-TIME TO TRUE                           
      *    SET SO-Z02141-M-WITH  TO TRUE                                
      *    MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
      *    EXEC CICS 
      *     XCTL PROGRAM(CT-ERROR-ROUTINE-NAME)                         
      *     COMMAREA(DFHCOMMAREA)                                       
      *    END-EXEC                                                     
      *    .                                                            
      ***************************************************************** 
      *                  2400-SET-WRRONG-VALUE-ERROR                    
      *                                                                 
      * MODIFING ERROR MSG AND RETURN CODE FROM ROUTINE                 
      ***************************************************************** 
       2400-SET-WRRONG-VALUE-ERROR.                                     
           MOVE 'INVALID MONTH OR DAY VALUE ' TO ZZEC0243-O-ERROR-MSG   
           SET ZZEC0243-O-RC-WRONG-VALUE      TO TRUE                   
           SET ZZEC0243-SO-2130-PARA          TO TRUE                   
           PERFORM 3000-FINAL                                           
           .                                                            
      ***************************************************************** 
      *                          3000-FINAL                             
      *  END OF THE PROGRAM                                             
      ***************************************************************** 
       3000-FINAL.                                                      
      * GO BACK TO TRANSACTION                                          
           DISPLAY 'GOBACK '                                            
           GOBACK                                                       
           .                                                            
                                                   

                                             
                                                                        
                                     



                                  