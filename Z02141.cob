       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID Z02141.                                               
      ******************************************************************
      *                  Z02141 - 0207                                  
      *PROGRAM IS CALLED IF ANY ERROR HAPPEND IN ANY APPLICATION PROGRAM
      *                                                                 
      *  PROGRAM WILL DISPLAY ERROR MASSEGO ON  THIS PROGRAM'S MAP      
      *                                                                 
      *                                                                 
      *                                                                 
      *  MAP CONTAINS OF 10 ROWS OF USER MESSAGES AND STATIC MESSAGE    
      *  FOR USER INDICATING THAT AFTER PRESSING 'F3' HE CAN GO BACK    
      * TO CALLING PROGRAM  ( THE SCREEN WHERE ERROR OCCURED )          
      *                                                                 
      * ALL THIS 10 LINES DESCIBED ABOVE WILL BE PROVIDED BY CALLING    
      * PROGRAM                                                         
      *                                                                 
      * THE TASK OF THIS PROGRAM IS ONLY TO DISPLAY THOSE MESSAGES      
      * AND ALLOW USER TO GO BACK                                       
      ******************************************************************
      *                      CHANGE LOG                                 
      *                                                                 
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY ZZMP0214.                                               
           COPY DFHAID.                                                 
      * PROGRAM COMMAREA                                                
           COPY DFHBMSCA.                                               
      * TEN DUZY WSPOLNY COPYBOOK                                       
           COPY ZZEC0215.                                               
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME               PIC X.        
               88 SO-FIRST-TIME-PROGRAM-RUNS              VALUE 'Y'.    
               88 SO-NOT-FIRST-TIME-PROGRAM-RUNS          VALUE 'N'.    
           05 SW-WHAT-TYPE-SEND                           PIC X.        
               88 SO-SEND-WHOLE-MAP                       VALUE 'M'.    
               88 SO-SEND-ONLY-DATA                       VALUE 'D'.    
           05 SW-WHAT-TYPE-OF-END                         PIC X.        
               88 SO-FINAL-TERMINATION                    VALUE '1'.    
               88 SO-FINAL-WITH-COMMAREA                  VALUE '2'.    
               88 SO-FINAL-ERROR                          VALUE '3'.    
               88 SO-FINAL-ERROR-TERMINATION              VALUE '4'.    
       01 CT-ERROR-MESSAGE PIC X(78) VALUE 'END DUE TO ERROR '.         
       01 CT-SEND-MAP-ERROR-MSG PIC X(78) VALUE 'ERROR WHIEL SENDING'.  
       01 WS-ITER         PIC S9(4) COMP VALUE 0.                       
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA    PIC X(17294).                                  
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02141---------START'                               
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02141---------END'                                 
           PERFORM 3000-FINAL                                           
           .                                                            
      ***************************************************************** 
      *                       1000-INIT                                 
      ***************************************************************** 
       1000-INIT.                                                       
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           PERFORM 1010-CHECK-IF-FIRST-TIME                             
           .                                                            
      ******************************************************************
      *                1010-CHECK-IF-FIRST-TIME                         
      * PROGRAM WILL CHECK IF THIS IS THE FIRST TIME PROGRAM RUNS       
      * OR NOT                                                          
      * FLAG SO-Z02141-I-FIRST-TIME HAS TO BE SET TO TRUE BY CALLING    
      * PROGRAM                                                         
      ******************************************************************
       1010-CHECK-IF-FIRST-TIME.                                        
           IF SO-Z02141-I-FIRST-TIME   THEN                             
              SET SO-Z02141-I-NOT-FIRST-TIME TO TRUE                    
              SET SO-FIRST-TIME-PROGRAM-RUNS TO TRUE                    
           ELSE                                                         
              SET SO-NOT-FIRST-TIME-PROGRAM-RUNS TO TRUE                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2000-PROCESS                                  
      ******************************************************************
       2000-PROCESS.                                                    
           IF SO-FIRST-TIME-PROGRAM-RUNS  THEN                          
              PERFORM 2001-FIRST-TIME-RUN                               
           ELSE                                                         
              PERFORM 2002-NOT-FIRST-TIME-RUN                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2001-FIRST-TIME-RUN                             
      * IF THIS IS THE FIRST TIME PROGRAM RUNS THEN PROGRAM             
      * WILL DISPLAY ERROR MESSAGE ON THE SCREEN                        
      ******************************************************************
       2001-FIRST-TIME-RUN.                                             
           INITIALIZE MPSEO                                             
           PERFORM 2010-SEND-INPUT-TO-SCREEN                            
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           .                                                            
      ******************************************************************
      *              2002-NOT-FIRST-TIME-RUN                            
      * THIS PARAGRAPH WILL CHECK WHAT KEY USER HAVE PRESSED            
      * IF USER PRESSED F3 THEN PROGRAM WILL RETURN CONTROL TO          
      * CALLING PROGRAM                                                 
      * IF USER PRESSED ANYTHING ELSE THEN HE WILL GET MESSAGE          
      * INDICATING THAT THIS IS INVALID KEY                             
      ******************************************************************
       2002-NOT-FIRST-TIME-RUN.                                         
           EVALUATE EIBAID                                              
             WHEN DFHPF3                                                
               SET SO-FINAL-TERMINATION   TO TRUE                       
             WHEN OTHER                                                 
               MOVE 'INVALID KEY '        TO MSGO                       
               PERFORM 2100-SEND-THE-MAP                                
               SET SO-FINAL-WITH-COMMAREA     TO TRUE                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *              2010-SEND-INPUT-TO-SCREEN                          
      * PARAGRAPH WILL MOVE ERROR MESSAGES TO THE SCREEN VARIABLES      
      * ATTRIBUTES OF SCREEN FIELDS WILL BE SET TO "BRT" BRIGHT         
      ******************************************************************
       2010-SEND-INPUT-TO-SCREEN.                                       
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 10       
             MOVE WS-Z02141-I-ERROR-MESSAGE(WS-ITER)  TO POLEO(WS-ITER) 
             MOVE DFHBMBRY                           TO POLEA(WS-ITER)  
           END-PERFORM                                                  
                                                                        
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *              2010-SEND-INPUT-TO-SCREEN                          
      ******************************************************************
       2100-SEND-THE-MAP.                                               
           EXEC CICS                                                    
           SEND MAP('MPSE') MAPSET('MPSE')                              
           FROM(MPSEO)                                                  
           ERASE                                                        
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2200-CHECK-EIBRESP                      
      * PARAGRAPH WILL CHECH IF CICS OPERATION RISED ANY ERROR          
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
           DISPLAY 'PRZED EVALUATE EIBRESP '                            
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
               DISPLAY ' EIBRESP NORMAL   '                             
           WHEN OTHER                                                   
               DISPLAY ' EIBRESP OTHER    '                             
               SET SO-FINAL-ERROR         TO TRUE                       
               PERFORM 3000-FINAL                                       
           END-EVALUATE                                                 
           DISPLAY 'PO EVALUATE EIBRESP '                               
           .                                                            
      ******************************************************************
      *                        3000-FINAL                               
      * PARAGRAPH WILL RETURN CONTROL TO CALLING PROGRAM                
      * OR WILL END THIS TRANSATION ( DUE TO AN ERROR )                 
      *                                                                 
      * SPECIAL FLAGS WILL BE SET  HERE :                               
      *               SO-M-FIRST-WITHOUT                                
      *          OR  SO-M-FIRST-WITH                                    
      *                                                                 
      * THOSE 2 FLAGS INDICATING HOW CALLED PROGRAM SHOULD BEHAVE       
      * FIRST FLAG WILL FORCE THIS PROGRAM TO BEHAVE LIKE IT IS CALLED  
      * FOR THE FIRST TIME                                              
      *                                                                 
      * SECOND FLAG WILL FORCE PROGRAM TO BEHAVE LIKE THERE IS ALREADY  
      * SOME DATA  ( PROVIDED BY THE USER SAVED BY THE PROGRAM)         
      *                                                                 
      *                                                                 
      ******************************************************************
       3000-FINAL.                                                      
           MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
           WHEN SO-FINAL-WITH-COMMAREA                                  
              PERFORM 3004-RETURN-WITH-TRANSID                          
           WHEN SO-FINAL-TERMINATION                                    
               EVALUATE TRUE                                            
                 WHEN SO-Z02141-M-WITHOUT                               
                      SET SO-M-FIRST-WITHOUT TO TRUE                    
                 WHEN SO-Z02141-M-WITH                                  
                      SET SO-M-FIRST-WITH    TO TRUE                    
                 WHEN SO-Z02141-M-NO-DATA                               
                    MOVE WS-ZZEC0215 TO DFHCOMMAREA                     
                    PERFORM 3002-RETURN-WITHOUT-COMMAREA                
               END-EVALUATE                                             
                                                                        
               MOVE WS-ZZEC0215 TO DFHCOMMAREA                          
               PERFORM 3003-RETURN-WITH-COMMAREA                        
           WHEN SO-FINAL-ERROR                                          
               PERFORM 3001-SEND-ERROR-MESSAGE                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      3001-SEND-ERROR-MESSAGE                    
      * PARAGRAPH WILL BE PREFORMED ONLY IF ANY ERROR HAPPEN INSIDE     
      * THIS PROGRAM                                                    
      * THEN USER WILL GET PROPER ERORR MESSAGE                         
      * AND TRANSACTION WILL BE TERMINATED                              
      * (CONTROL WONT BE RETURNED TO CALLING PROGRAM)                   
      ******************************************************************
       3001-SEND-ERROR-MESSAGE.                                         
              EXEC CICS                                                 
               SEND TEXT                                                
               FROM(CT-ERROR-MESSAGE)                                   
              END-EXEC                                                  
                                                                        
              EXEC CICS                                                 
               RETURN                                                   
              END-EXEC                                                  
           .          
      ******************************************************************
      *                    3002-RETURN-WITHOUT-COMMAREA                 
      * PARAGRAPH WILL BE CALLED WHEN PROGRAM HAS TO TRANSFER CONTROL   
      * TO PROGRAM WITHOUT COMMAREA                                     
      *                                                                 
      * PHYSICLY WE HAVE TO PLACE COMMAREA IN THIS XCTL STATEMENT       
      * BY DUE TO USE OF LENGTH(0) PARAMETER PROGRAM THAT WILL BE CALLED
      * WILL BEHAVE LIKE THERE IS NO COMMAREA ( EIBCALEN = 0 )          
      ******************************************************************
       3002-RETURN-WITHOUT-COMMAREA.                                    
           EXEC CICS                                                    
            XCTL PROGRAM(WS-Z02141-I-CALLING-PROGRAM)                   
            COMMAREA(DFHCOMMAREA) LENGTH(0)                             
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    3003-RETURN-WITH-COMMAREA                    
      * PARAGRAPH WILL BE CALLED WHEN PROGRAM HAS TO RETURN CONTROL     
      * TO CALLING PROGRAM "NORMALLY" WITH COMMAREA                     
      ******************************************************************
       3003-RETURN-WITH-COMMAREA.                                       
           EXEC CICS                                                    
            XCTL PROGRAM(WS-Z02141-I-CALLING-PROGRAM)                   
            COMMAREA(DFHCOMMAREA)                                       
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    3004-RETURN-WITH-TRANSID                     
      * PARAGRAPH WILL BE CALLED IN ORDER TO CREATE                     
      * PSEUDO-CONVERSATIONAL PROGRAM                                   
      * THIS CICS STATEMENT WILL END THIS PROGRAM BUT THERE WILL BE     
      * OPTION TO RETRINGGER ( USER HAS TO PRESS ATENTION KEY )         
      ******************************************************************
       3004-RETURN-WITH-TRANSID.                                        
           EXEC CICS                                                    
            RETURN TRANSID('0207') COMMAREA(DFHCOMMAREA)      
            RETURN TRANSID('0207') COMMAREA(DFHCOMMAREA)           
           END-EXEC                                                
           .                                                                 
                                                  
      



