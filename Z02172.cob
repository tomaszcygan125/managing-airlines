       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02172.                                              
      ******************************************************************
      *               Z02172 (TRANS 0211)                               
      *                                                                 
      * PROGRAM IS RESPONSIBLE FOR DISPLAYING FLIGHTS THAT MEETS        
      * USER'S CRITERIA                                                 
      *                                                                 
      *  WHEN FLIGHT IS ONE-WAY PROGRAM WILL USE MAP MP0217             
      *  WHEN FLIGHT HAVE 2 PARTS ( TO AND FROM) IT WILL USER M10217 MAP
      *                                                                 
      *                                                                 
      *   AFTER BEING CALLED THIS PROGRAM WILL FIND ALL FLIGHTS THAT    
      *  MEETS CRITERIA AND PUT THEM INTO THE QUEUE                     
      *                                                                 
      *  AFTER THIS PROCESS IN COMPELTED PROGRAM WILL DISPLAY PART OF IT
      *  ON THE SCRREN. LATER USER WILL BE ABLE TO MOVE THIS RESULT SET 
      *  BY USING F7 AND F8 KEYS                                        
      *                                                                 
      *  USER ALSO WILL BE ALBE TO CHOOSE THIS FLIGHT                   
      *   OR TO DISPLAY MORE DETAILS ABOUT THIS FLIGHT                  
      * TO CHOOSE THE FLIIGHT MEANS THAT USER WILL CHOOSE THE SEAT      
      * WILL PROVIDE DATA FOR PASSENGERS AND WILL CREATE A RESERVATION  
      *                                                                 
      *                                                                 
      *                                                                 
      * THERE ARE 4 MAIN TYPES OF FLIGHTS                               
      * 1. DIRECT AND ONE-WAY                                           
      * 2. NOT DIRECT AND ONE-WAY                                       
      * 3. DIRECT AND 2WAY                                              
      * 4. NOT DIRECT AND 2 WAY                                         
      *                                                                 
      ******************************************************************
      *                         CHANGE LOG                              
      *                                                                 
      ******************************************************************
      ******************************************************************
      *                         DATA DIVISION                           
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
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
               88 SO-7019-PARA              VALUE '7019'.               
               88 SO-7020-PARA              VALUE '7020'.               
               88 SO-7021-PARA              VALUE '7021'.               
               88 SO-7022-PARA              VALUE '7022'.               
               88 SO-7023-PARA              VALUE '7023'.    
               88 SO-7024-PARA              VALUE '7024'.      
               88 SO-7025-PARA              VALUE '7025'.      
               88 SO-7026-PARA              VALUE '7026'.      
               88 SO-7027-PARA              VALUE '7027'.      
               88 SO-7028-PARA              VALUE '7028'.      
               88 SO-7029-PARA              VALUE '7029'.      
               88 SO-7030-PARA              VALUE '7030'.      
               88 SO-7031-PARA              VALUE '7031'.      
               88 SO-7032-PARA              VALUE '7032'.      
               88 SO-7033-PARA              VALUE '7033'.      
               88 SO-7034-PARA              VALUE '7034'.      
               88 SO-7035-PARA              VALUE '7035'.      
               88 SO-7036-PARA              VALUE '7036'.      
               88 SO-7037-PARA              VALUE '7037'.      
               88 SO-7038-PARA              VALUE '7038'.      
               88 SO-7039-PARA              VALUE '7039'.      
               88 SO-7040-PARA              VALUE '7040'.      
               88 SO-7041-PARA              VALUE '7041'.      
               88 SO-7042-PARA              VALUE '7042'.      
               88 SO-7043-PARA              VALUE '7043'.      
               88 SO-7044-PARA              VALUE '7044'.      
               88 SO-7045-PARA              VALUE '7045'.      
               88 SO-7046-PARA              VALUE '7046'.      
               88 SO-7047-PARA              VALUE '7047'.      
               88 SO-7048-PARA              VALUE '7048'.      
               88 SO-7049-PARA              VALUE '7049'.      
               88 SO-7050-PARA              VALUE '7050'.      
               88 SO-7051-PARA              VALUE '7051'.      
               88 SO-7052-PARA              VALUE '7052'.      
               88 SO-7053-PARA              VALUE '7053'.      
               88 SO-7054-PARA              VALUE '7054'.      
               88 SO-7055-PARA              VALUE '7055'.      
               88 SO-7056-PARA              VALUE '7056'.      
               88 SO-7201-PARA              VALUE '7201'.      
               88 SO-7202-PARA              VALUE '7202'.      
               88 SO-7203-PARA              VALUE '7203'.    
               88 SO-7204-PARA              VALUE '7204'.               
               88 SO-7205-PARA              VALUE '7205'.               
               88 SO-7206-PARA              VALUE '7206'.               
               88 SO-7207-PARA              VALUE '7207'.               
               88 SO-7208-PARA              VALUE '7208'.               
               88 SO-7209-PARA              VALUE '7209'.               
               88 SO-7210-PARA              VALUE '7210'.               
               88 SO-7211-PARA              VALUE '7211'.               
               88 SO-7212-PARA              VALUE '7212'.               
               88 SO-7213-PARA              VALUE '7213'.               
               88 SO-7214-PARA              VALUE '7214'.               
               88 SO-7215-PARA              VALUE '7215'.               
               88 SO-7216-PARA              VALUE '7216'.               
               88 SO-7217-PARA              VALUE '7217'.               
               88 SO-7218-PARA              VALUE '7218'.               
               88 SO-7219-PARA              VALUE '7219'.               
               88 SO-7220-PARA              VALUE '7220'.               
               88 SO-7221-PARA              VALUE '7221'.               
               88 SO-7222-PARA              VALUE '7222'.               
               88 SO-7223-PARA              VALUE '7223'.               
               88 SO-7224-PARA              VALUE '7224'.               
               88 SO-7225-PARA              VALUE '7225'.               
               88 SO-7226-PARA              VALUE '7226'.               
               88 SO-7227-PARA              VALUE '7227'.               
               88 SO-7228-PARA              VALUE '7228'.               
               88 SO-7229-PARA              VALUE '7229'.               
               88 SO-7230-PARA              VALUE '7230'.               
               88 SO-7231-PARA              VALUE '7231'.               
               88 SO-7301-PARA              VALUE '7301'.               
       01 CT-CONSTANTS.                                                 
           05 CT-MAXIMAL-AMOUNT-OF-2WAY          PIC S9(4) COMP VALUE 4.
           05 CT-PI-VALUE          PIC 9(1)V9(6) COMP VALUE 3.141592.   
           05 CT-THIS-PROGRAM-NAME         PIC X(8) VALUE 'Z02172  '.   
           05 CT-ERROR-ROUTINE-NAME        PIC X(8) VALUE 'Z02141  '.   
           05 CT-CALLING-PROGRAM-NAME      PIC X(8) VALUE 'Z02152  '.   
           05 CT-DETAIL-PROGRAM-NAME       PIC X(8) VALUE 'Z02182  '.   
           05 CT-SEATS-PROGRAM-NAME        PIC X(8) VALUE 'Z02192  '.   
           05 CT-ONEWAY-QUEUE       PIC X(8) VALUE '02X1    '.          
           05 CT-FLIGHT-TO-QUEUE           PIC X(8) VALUE 'FIRSTQUE'.   
           05 CT-FLIGHT-FROM-QUEUE         PIC X(8) VALUE 'SECQUEUE'.   
           05 CT-TO-AND-FROM-QUEUE         PIC X(8) VALUE 'THRQUEUE'.   
           05 CT-RETURN-DIRECT-QUEUE       PIC X(8) VALUE '02X5    '.   
           05  CT-NO-FINISHED              PIC X(50)                    
                             VALUE 'TA OPCJA NIE JEST SKONCZONA'.       
           05 CT-EMPTY-FIELD   PIC X(15) VALUE 'XXXXXXXXXXXXXX'.        
           05 CT-1-HOUR        PIC S9(9) COMP VALUE 3600.               
           05 CT-15-HOURS      PIC S9(9) COMP VALUE 54000.              
           05 CT-DELETED-STATUS.                                        
               49 CT-DELETED-STATUS-LEN PIC S9(4) COMP VALUE 7.         
               49 CT-DELETED-STATUS-TEXT PIC X(15) VALUE 'DELETED'.     
       01 WS-FIRST-DEPARTURE-TIMESTAMP.                                 
             10 WS-FIRST-DEP-DATE.                                      
               15 FIRST-DEP-YEAR   PIC 9(4).                            
               15 FILLER       PIC X VALUE '-'.                         
               15 FIRST-DEP-MONTH    PIC 9(2).                          
               15 FILLER       PIC X VALUE '-'.                         
               15 FIRST-DEP-DAY      PIC 9(2).                          
             10 FILLER       PIC X VALUE '-'.                           
             10 WS-FIRST-DEP-TIME.                                      
               15 FIRST-DEP-HOUR     PIC 9(2).                          
               15 FILLER      PIC X VALUE '.'.                          
               15 FIRST-DEP-MINUTE   PIC 9(2).                          
               10 FILLER       PIC X VALUE '.'.                         
               10 FIRST-DEP-SECOND   PIC 9(2).                          
               10 FILLER       PIC X VALUE '.'.                         
               10 FIRST-DEP-MICROSEC PIC 9(6).                          
       01 WS-SEC-ARRIVAL-TIMESTAMP.                                     
             10 WS-SECOND-ARRIVAL-DATE.                                 
               15 SECOND-ARV-YEAR   PIC 9(4).                           
               15 FILLER       PIC X VALUE '-'.                         
               15 SECOND-ARV-MONTH    PIC 9(2).                         
               15 FILLER       PIC X VALUE '-'.
               15 SECOND-ARV-DAY      PIC 9(2).                 
             10 FILLER       PIC X VALUE '-'.                   
             10 WS-SECOND-ARRIVAL-TIME.                         
               15 SECOND-ARV-HOUR     PIC 9(2).                 
               15 FILLER      PIC X VALUE '.'.                  
               15 SECOND-ARV-MINUTE   PIC 9(2).                 
               10 FILLER       PIC X VALUE '.'.                 
               10 SECOND-ARV-SECOND   PIC 9(2).                 
               10 FILLER       PIC X VALUE '.'.                 
               10 SECOND-ARV-MICROSEC PIC 9(6).                 
       01 WS-FIRST-ARRIVAL-TIMESTAMP.                           
             10 WS-FIRST-ARRIVAL-DATE.                          
               15 FIRST-ARV-YEAR   PIC 9(4).                    
               15 FILLER       PIC X VALUE '-'.                 
               15 FIRST-ARV-MONTH    PIC 9(2).                  
               15 FILLER       PIC X VALUE '-'.                 
               15 FIRST-ARV-DAY      PIC 9(2).                  
             10 FILLER       PIC X VALUE '-'.                   
             10 WS-FIRST-ARRIVAL-TIME.                          
               15 FIRST-ARV-HOUR     PIC 9(2).                  
               15 FILLER      PIC X VALUE '.'.                  
               15 FIRST-ARV-MINUTE   PIC 9(2).                  
               10 FILLER       PIC X VALUE '.'.                 
               10 FIRST-ARV-SECOND   PIC 9(2).                  
               10 FILLER       PIC X VALUE '.'.                 
               10 FIRST-ARV-MICROSEC PIC 9(6).                  
       01 WS-SEC-DEARTURE-TIMESTAMP.                            
             10 WS-SECOND-DEPARTURE-DATE.                       
               15 SECOND-DEP-YEAR   PIC 9(4).                   
               15 FILLER       PIC X VALUE '-'.                 
               15 SECOND-DEP-MONTH    PIC 9(2).                 
               15 FILLER       PIC X VALUE '-'.                 
               15 SECOND-DEP-DAY      PIC 9(2).                 
             10 FILLER       PIC X VALUE '-'.                   
             10 WS-SECOND-DEPARTURE-TIME.                       
               15 SECOND-DEP-HOUR     PIC 9(2).  
               15 FILLER      PIC X VALUE '.'.                         
               15 SECOND-DEP-MINUTE   PIC 9(2).                        
               10 FILLER       PIC X VALUE '.'.                        
               10 SECOND-DEP-SECOND   PIC 9(2).                        
               10 FILLER       PIC X VALUE '.'.                        
               10 SECOND-DEP-MICROSEC PIC 9(6).                        
      * BEGINING OF QUEUE1                                             
       01 WS-ONE-WAY-Q-STRUCTURE.                                      
                                                                       
           05 QUEUE-FLIGHT-NUMBER          PIC X(15).                  
           05 QUEUE-FLIGHT-ID              PIC X(15).                  
           05 QUEUE-DEPARTURE-AIRPORT-CODE PIC X(3).                   
           05 QUEUE-DEPARTURE-TIMESTAMP.                               
             10 ONE-WAY-Q-DATE-DEP.                                    
               15 ONE-WAY-Q-DEP-YEAR PIC 9(4).                         
               15 FILLER       PIC X VALUE '-'.                        
               15 ONE-WAY-Q-DEP-MONTH  PIC 9(2).                       
               15 FILLER       PIC X VALUE '-'.                        
               15 ONE-WAY-Q-DEP-DAY    PIC 9(2).                       
             10 FILLER       PIC X VALUE '-'.                          
             10 ONE-WAY-Q-TIME-DEP.                                    
               15 ONE-WAY-Q-DEP-HOUR   PIC 9(2).                       
               15 FILLER      PIC X VALUE '.'.                         
               15 ONE-WAY-Q-DEP-MINUTE PIC 9(2).                       
             10 FILLER       PIC X VALUE '.'.                          
             10 ONE-WAY-Q-DEP-SECOND PIC 9(2).                         
             10 FILLER       PIC X VALUE '.'.                          
             10 ONE-WAY-Q-DEP-MICROSEC PIC 9(6).                       
           05 QUEUE-ARRIVAL-AIRPORT-CODE   PIC X(3).                   
           05 QUEUE-ARRIVAL-TIMESTAMP.                                 
             10 ONE-WAY-Q-ARV-DATE.                                    
               15 ONE-WAY-Q-ARV-YEAR PIC 9(4).                         
               15 FILLER       PIC X VALUE '-'.                        
               15 ONE-WAY-Q-ARV-MONTH  PIC 9(2).                       
               15 FILLER       PIC X VALUE '-'.                        
               15 ONE-WAY-Q-ARV-DAY    PIC 9(2).    
             10 FILLER       PIC X VALUE '-'.                          
             10 ONE-WAY-Q-ARV-TIME.                                    
               15 ONE-WAY-Q-ARV-HOUR   PIC 9(2).                       
               15 FILLER      PIC X VALUE '.'.                         
               15 ONE-WAY-Q-ARV-MINUTE PIC 9(2).                       
             10 FILLER       PIC X VALUE '.'.                          
             10 ONE-WAY-Q-ARV-SECOND PIC 9(2).                         
             10 FILLER       PIC X VALUE '.'.                          
             10 ONE-WAY-Q-ARV-MICROSEC PIC 9(6).                       
           05 QUEUE-AIRLINE-CODE           PIC X(3).                   
           05 QUEUE-TRANSFER-NUMBER        PIC X(2).                   
      * THIS VARIABLE STORES INFORMATION ABOUT AMOUNT OF FREE SEATS    
      * FOR MAIN (FIRST FLIGHT)                                        
           05 ONE-WAY-Q-FREE-SEATS         PIC S9(4) COMP.             
      * THIS TABLE STORES INFORMATIONS ABOUT TRANSFER FLIGHTS          
                                                                       
           05 ONE-WAY-Q-ADDITIONAL-FLIGHTS OCCURS 5 TIMES.             
              10 ONE-WAY-Q-FLIGHT-ID       PIC X(15).                  
              10 ONE-WAY-Q-FREE-SEATS-T    PIC S9(4) COMP.             
      * END OF QUEUE1                                                  
       01 WS-FIRST-QUEUE-STRUCTURE.                                    
                                                                       
           05 QUEUE-F-FLIGHT-NUMBER          PIC X(15).                
           05 QUEUE-F-FIRST-FLIGHT-ID        PIC X(15).                
           05 QUEUE-F-DEPARTURE-AIRPORT-CODE PIC X(3).                 
           05 QUEUE-F-DEPARTURE-TIMESTAMP.                             
             10 QUEUE-F-DATE-DEP.                                      
               15 QUEUE-F-DEP-YEAR   PIC 9(4).                         
               15 FILLER       PIC X VALUE '-'.                        
               15 QUEUE-F-DEP-MONTH    PIC 9(2).                       
               15 FILLER       PIC X VALUE '-'.                        
               15 QUEUE-F-DEP-DAY      PIC 9(2).                       
             10 FILLER       PIC X VALUE '-'.                          
             10 QUEUE-F-TIME-DEP.                                      
               15 QUEUE-F-DEP-HOUR     PIC 9(2).                       
               15 FILLER      PIC X VALUE '.'.      
               15 QUEUE-F-DEP-MINUTE   PIC 9(2).                        
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-F-DEP-SECOND   PIC 9(2).                          
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-F-DEP-MICROSEC PIC 9(6).                          
           05 QUEUE-F-RRIVAL-AIRPORT-CODE   PIC X(3).                   
           05 QUEUE-F-ARRIVAL-TIMESTAMP.                                
             10 QUEUE-F-ARV-DATE.                                       
               15 QUEUE-F-ARV-YEAR   PIC 9(4).                          
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-F-ARV-MONTH    PIC 9(2).                        
               15 FILLER       PIC X VALUE '-'.                         
               15 QUEUE-F-ARV-DAY      PIC 9(2).                        
             10 FILLER       PIC X VALUE '-'.                           
             10 QUEUE-F-ARV-TIME.                                       
               15 QUEUE-F-ARV-HOUR     PIC 9(2).                        
               15 FILLER      PIC X VALUE '.'.                          
               15 QUEUE-F-ARV-MINUTE   PIC 9(2).                        
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-F-ARV-SECOND   PIC 9(2).                          
             10 FILLER       PIC X VALUE '.'.                           
             10 QUEUE-F-ARV-MICROSEC PIC 9(6).                          
           05 QUEUE-F-AIRLINE-CODE           PIC X(3).                  
           05 QUEUE-F-TRANSFER-NUMBER        PIC X(2).                  
      * THIS VARIABLE STORES INFORMATION ABOUT AMOUNT OF FREE SEATS     
      * FOR MAIN (FIRST FLIGHT)                                         
           05 QUEUE-F-FREE-SEATS           PIC S9(4) COMP.              
      * THIS TABLE STORES INFORMATIONS ABOUT TRANSFER FLIGHTS           
                                                                        
           05 QUEUE-F-ADDITIONAL-FLIGHTS  OCCURS 5 TIMES.               
              10 QUEUE-F-FLIGHT-ID         PIC X(15).                   
              10 QUEUE-F-FREE-SEATS-T      PIC S9(4) COMP.              
      * END OF QUEUE1                                                   
       01 WS-SECOND-QUEUE-STRUCTURE.                                    
                                                                        
           05 QUEUE-S-FLIGHT-NUMBER          PIC X(15).         
           05 QUEUE-S-FIRST-FLIGHT-ID        PIC X(15).               
           05 QUEUE-S-DEPARTURE-AIRPORT-CODE PIC X(3).                
           05 QUEUE-S-DEPARTURE-TIMESTAMP.                            
             10 QUEUE-S-DATE-DEP.                                     
               15 QUEUE-S-DEP-YEAR   PIC 9(4).                        
               15 FILLER       PIC X VALUE '-'.                       
               15 QUEUE-S-DEP-MONTH    PIC 9(2).                      
               15 FILLER       PIC X VALUE '-'.                       
               15 QUEUE-S-DEP-DAY      PIC 9(2).                      
             10 FILLER       PIC X VALUE '-'.                         
             10 QUEUE-S-TIME-DEP.                                     
               15 QUEUE-S-DEP-HOUR     PIC 9(2).                      
               15 FILLER      PIC X VALUE '.'.                        
               15 QUEUE-S-DEP-MINUTE   PIC 9(2).                      
             10 FILLER       PIC X VALUE '.'.                         
             10 QUEUE-S-DEP-SECOND   PIC 9(2).                        
             10 FILLER       PIC X VALUE '.'.                         
             10 QUEUE-S-DEP-MICROSEC PIC 9(6).                        
           05 QUEUE-S-RRIVAL-AIRPORT-CODE   PIC X(3).                 
           05 QUEUE-S-ARRIVAL-TIMESTAMP.                              
             10 QUEUE-S-ARV-DATE.                                     
               15 QUEUE-S-ARV-YEAR   PIC 9(4).                        
               15 FILLER       PIC X VALUE '-'.                       
               15 QUEUE-S-ARV-MONTH    PIC 9(2).                      
               15 FILLER       PIC X VALUE '-'.                       
               15 QUEUE-S-ARV-DAY      PIC 9(2).                      
             10 FILLER       PIC X VALUE '-'.                         
             10 QUEUE-S-ARV-TIME.                                     
               15 QUEUE-S-ARV-HOUR     PIC 9(2).                      
               15 FILLER      PIC X VALUE '.'.                        
               15 QUEUE-S-ARV-MINUTE   PIC 9(2).                      
             10 FILLER       PIC X VALUE '.'.                         
             10 QUEUE-S-ARV-SECOND   PIC 9(2).                        
             10 FILLER       PIC X VALUE '.'.                         
             10 QUEUE-S-ARV-MICROSEC PIC 9(6).                        
           05 QUEUE-S-AIRLINE-CODE           PIC X(3).   
           05 QUEUE-S-TRANSFER-NUMBER        PIC X(2).                  
      * THIS VARIABLE STORES INFORMATION ABOUT AMOUNT OF FREE SEATS     
      * FOR MAIN (FIRST FLIGHT)                                         
           05 QUEUE-S-FREE-SEATS           PIC S9(4) COMP.              
      * THIS TABLE STORES INFORMATIONS ABOUT TRANSFER FLIGHTS           
                                                                        
           05 QUEUE-S-ADDITIONAL-FLIGHTS  OCCURS 5 TIMES.               
              10 QUEUE-S-FLIGHT-ID         PIC X(15).                   
              10 QUEUE-S-FREE-SEATS-T      PIC S9(4) COMP.              
      * END OF QUEUE1                                                   
       01 WS-QUEUE-2-STRUCTURE.                                         
           05 QUEUE-2-FIRST-FLIGHT-ID      PIC X(15).                   
           05 QUEUE-2-TRANSFER-NUMBER      PIC 99.                      
           05 QUEUE-2-TRAN-FLIGHT-ID       PIC X(15) OCCURS 5 TIMES.    
                                                                        
       01 WS-QUEUE-3-STRUCTURE.                                         
           05 QUEUE-3-FLIGHT-ID            PIC X(15).                   
           05 QUEUE-3-FLIGHT-NUMBER        PIC X(15).                   
           05 QUEUE-3-TIME-DEP             PIC X(5).                    
           05 QUEUE-3-TIME-ARV             PIC X(5).                    
       01 WS-QUEUE-4-STRUCTURE.                                         
           05 QUEUE-4-FLIGHT-ID            PIC X(15).                   
           05 QUEUE-4-FLIGHT-NUMBER        PIC X(15).                   
           05 QUEUE-4-TIME-DEP             PIC X(5).                    
           05 QUEUE-4-TIME-ARV             PIC X(5).                    
                                                                        
       01 WS-RETURN-DIRECT-QUEUE-ST.                                    
           05 2WAY-DIR-FLIGHT-TO-NUMBER        PIC X(15).               
           05 2WAY-DIR-FLIGHT-TO-ID            PIC X(15).               
           05 2WAY-DIR-FLIGHT-TO-DEP-DATE      PIC X(10).               
           05 2WAY-DIR-FLIGHT-TO-DEP-TIME      PIC X(5).                
           05 2WAY-DIR-FLIGHT-TO-ARV-DATE      PIC X(10).               
           05 2WAY-DIR-FLIGHT-TO-ARV-TIME      PIC X(10).               
           05 2WAY-DIR-FLIGHT-FROM-NUMBER      PIC X(15).               
           05 2WAY-DIR-FLIGHT-FROM-ID          PIC X(15).               
           05 2WAY-DIR-FLIGHT-FROM-DEP-DATE    PIC X(10).  
           05 2WAY-DIR-FLIGHT-FROM-DEP-TIME    PIC X(5).               
           05 2WAY-DIR-FLIGHT-FROM-ARV-DATE    PIC X(10).              
           05 2WAY-DIR-FLIGHT-FROM-ARV-TIME    PIC X(10).              
           05 2WAY-DIR-DEPARTURE-AIRPORT       PIC X(3).               
           05 2WAY-DIR-ARRIVAL-AIRPORT         PIC X(3).               
           05 2WAY-DIR-TO-FLIGHT-SEATS         PIC S9(4) COMP.         
           05 2WAY-DIR-FROM-FLIGHT-SEATS       PIC S9(4) COMP.         
           05 2WAY-DIR-TO-FLIGHT-AIRLINE       PIC X(3).               
           05 2WAY-DIR-FROM-FLIGHT-AIRLINE     PIC X(3).               
           05 2WAY-DIR-NUMBER-OF-TRANSFERS     PIC 9(2).               
       01 WS-TO-AND-FROM-Q-STRUCTURE.                                  
           05 QUEUE-6-FIRST-TO-NUMBER          PIC X(15).              
           05 QUEUE-6-FIRST-FROM-NUMBER        PIC X(15).              
           05 QUEUE-6-FLIGHT-TO-DEP-DATE       PIC X(10).              
           05 QUEUE-6-FLIGHT-TO-DEP-TIME       PIC X(5).               
           05 QUEUE-6-FLIGHT-TO-ARV-DATE       PIC X(10).              
           05 QUEUE-6-FLIGHT-TO-ARV-TIME       PIC X(10).              
           05 QUEUE-6-FLIGHT-FROM-DEP-DATE     PIC X(10).              
           05 QUEUE-6-FLIGHT-FROM-DEP-TIME     PIC X(5).               
           05 QUEUE-6-FLIGHT-FROM-ARV-DATE     PIC X(10).              
           05 QUEUE-6-FLIGHT-FROM-ARV-TIME     PIC X(10).              
           05 QUEUE-6-DEPARTURE-AIRPORT        PIC X(3).               
           05 QUEUE-6-ARRIVAL-AIRPORT          PIC X(3).               
           05 QUEUE-6-NUMBER-OF-TO-TRANSFERS   PIC 9(2).               
           05 QUEUE-6-NUMBER-OF-FR-TRANSFERS PIC 9(2).                 
           05 QUEUE-6-SUBFLIGHT-TO-TABLE OCCURS 6 TIMES.               
              10 QUEUE-6-TO-FLIGHT-ID          PIC X(15).              
              10 QUEUE-6-TO-FREE-SEATS         PIC S9(4) COMP.         
           05 QUEUE-6-SUBFLIGHT-FROM-TABLE OCCURS 6 TIMES.             
              10 QUEUE-6-FROM-FLIGHT-ID        PIC X(15).              
              10 QUEUE-6-FROM-FREE-SEATS       PIC S9(4) COMP.         
       01 WS-DB2-DATA.                                                 
           05 WS-TO-FLIGHT-ID.                                         
               49 WS-TO-FLIGHT-ID-LEN          PIC S9(4) COMP.         
               49 WS-TO-FLIGHT-ID-TEXT         PIC X(15).              
           05 WS-TO-FLIGHT-NUMBER.   
               49 WS-TO-FLIGHT-NUMBER-LEN          PIC S9(4) COMP.      
               49 WS-TO-FLIGHT-NUMBER-TEXT         PIC X(15).           
           05 WS-FROM-FLIGHT-ID.                                        
               49 WS-FROM-FLIGHT-ID-LEN          PIC S9(4) COMP.        
               49 WS-FROM-FLIGHT-ID-TEXT         PIC X(15).             
           05 WS-FROM-FLIGHT-NUMBER.                                    
               49 WS-FROM-FLIGHT-NUMBER-LEN          PIC S9(4) COMP.    
               49 WS-FROM-FLIGHT-NUMBER-TEXT         PIC X(15).         
                                                                        
       01 WS-VARIABLES.                                                 
           05 WS-TEMP-TIMEZONE.                                         
              10 WS-TIMEZONE-HOUR-AND-SIGN.                             
               15 WS-TIMEZONE-SIGN                    PIC X.            
               15 WS-TIMEZONE-HOUR                    PIC X(2).         
              10 WS-TIMEZONE-FILLER                  PIC X.             
              10 WS-TIMEZONE-MINUTE                  PIC X(2).          
                                                                        
           05 WS-EARTCH-RADIOUS               PIC S9(4) COMP VALUE 6371.
           05 WS-TRANSFER-NUMBER                     PIC S9(4) COMP.    
           05 WS-COMMON-TRANSFER-NUMBER              PIC S9(4) COMP.    
           05 WS-RANDOM-VALUE                        PIC X.             
           05 WS-FIRST-FROM-FLIGHT-TIMESTAMP         PIC X(26).         
           05 WS-LAST-TO-FLIGHT-TIMESTAMP            PIC X(26).         
           05 WS-FIRST-QUEUE-ITERATOR                PIC S9(4) COMP.    
           05 WS-SECOND-QUEUE-ITERATOR               PIC S9(4) COMP.    
           05 WS-TEMP-AIRPORT-CODE                   PIC X(3).          
           05 WS-WHICH-FLIGHT-IN-TRANSFERS           PIC S9(4) COMP.    
           05 WS-SUM-OF-DISTANCES                    COMP-2.            
           05 WS-LAST-DESTINATION-AIRPORT            PIC X(3).          
           05 WS-CALCULATED-DISTANCE                 COMP-2.            
           05 WS-DESTINATION-AIRPORT                 PIC X(3).          
           05 WS-ORIGIN-AIRPORT                      PIC X(3).          
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
                                                                     
           05 WS-MAXIMAL-DISTANCE                    COMP-2.         
           05 WS-DISTANCE-TABLE OCCURS 6 TIMES.                      
               10 WS-DISTANCE                       COMP-2.          
           05 WS-TEMP-TIMESTAMP.                                     
                                                                     
             10 WS-TEMP-DATE.                                        
               15 WS-TEMPYEAR        PIC 9(4).                       
               15 FILLER       PIC X VALUE '-'.                      
               15 WS-TEMPMONTH         PIC 9(2).                     
               15 FILLER       PIC X VALUE '-'.                      
               15 WS-TEMPDAY           PIC 9(2).                     
             10 FILLER       PIC X VALUE '-'.                        
             10 WS-TEMP-TIME.                                        
               15 WS-TEMPHOUR          PIC 9(2).                     
               15 FILLER      PIC X VALUE '.'.                       
               15 WS-TEMPMINUTE      PIC 9(2).                       
             10 FILLER       PIC X VALUE '.'.                        
             10 WS-TEMPSECOND        PIC 9(2).                       
             10 FILLER       PIC X VALUE '.'.                        
             10 WS-TEMPMICROSECOND   PIC 9(6).                       
           05 WS-WHICH-TRANSFER            PIC 99 VALUE 0.           
           05 WS-TEMP-TICKET-NUMBER        PIC 99 VALUE 0.           
           05 WS-NUM-OF-FREE-SEATS         PIC S9(9) COMP VALUE 0.   
           05 WS-TO-FLIGHT-AIRLINE-CODE    PIC X(3).         
           05 WS-FROM-FLIGHT-AIRLINE-CODE  PIC X(3).                    
           05 WS-ITER                      PIC S9(4) COMP VALUE 0.      
           05 WS-ITER2                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER3                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER4                     PIC S9(4) COMP VALUE 0.      
           05 WS-CHOICE-POSITION           PIC S9(4) COMP VALUE 0.      
           05 WS-ITER5                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER6                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER7                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER8                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER9                     PIC S9(4) COMP VALUE 0.      
           05 WS-ITER10                    PIC S9(4) COMP VALUE 0.      
           05 WS-ITER11                    PIC S9(4) COMP VALUE 0.      
           05 WS-ITER12                    PIC S9(4) COMP VALUE 0.      
           05 WS-ITER15                    PIC S9(4) COMP VALUE 0.      
           05 WS-TRANSFER-ITER             PIC S9(4) COMP VALUE 0.      
           05 WS-NULL-IND                  PIC S9(4) COMP VALUE 0.      
           05 WS-AMOUNT-OF-FREE-SEATS      PIC S9(4) COMP VALUE 0.      
           05 WS-TEMP-NUMERIC              PIC S9(4) COMP VALUE 0.      
           05 WS-TEMP-NUMERIC2             PIC S9(4) COMP VALUE 0.      
           05 WS-COUNT-OF-TAKEN-SEATS      PIC S9(4) COMP VALUE 0.      
           05 WS-TO-FLIGHT-FREE-SEATS      PIC S9(4) COMP VALUE 0.      
           05 WS-FROM-FLIGHT-FREE-SEATS    PIC S9(4) COMP VALUE 0.      
           05 WS-WHAT-RECORD-TO-READ       PIC S9(4) COMP VALUE 0.      
           05 WS-USER-CHOICE-POSITION      PIC S9(4) COMP VALUE 0.      
           05 WS-USER-CHOICE               PIC X.                       
           05 WS-COUNT-USER-CHOICE         PIC S9(4) COMP VALUE 0.      
           05 WS-CHOICE-COUNTER            PIC S9(4) COMP VALUE 0.      
           05 WS-DEPARTURE-DATE            PIC X(10) VALUE SPACE.       
           05 WS-LICZNIK-ODRZ              PIC S9(4) COMP VALUE 0.      
           05 WS-ARRIVAL-DATE              PIC X(10) VALUE SPACE.       
           05 WS-HOUR-OFFSET               PIC S9(4) COMP VALUE 0.      
           05 WS-MINUTE-OFFSET             PIC S9(4) COMP VALUE 0.      
           05 WS-HOUR-OFFSET-TEMP          PIC X(10) VALUE SPACE.       
           05 WS-MINUTE-OFFSET-TEMP        PIC X(10) VALUE SPACE.       
           05 WS-MODIFIED-TIMESTAMP        PIC X(26).    
           05 WS-MAP2-USER-CHOICE          PIC X.                     
           05 WS-MDL-LONGITUDE             USAGE COMP-2.              
           05 WS-MDL-LATITUDE              USAGE COMP-2.              
           05 WS-TR1-LATITUDE              USAGE COMP-2.              
           05 WS-TR1-LONGITUDE             USAGE COMP-2.              
           05 WS-TR2-LATITUDE              USAGE COMP-2.              
           05 WS-TR2-LONGITUDE             USAGE COMP-2.              
                                                                      
           05 WS-DISTANCE-X                USAGE COMP-2.              
           05 WS-DISTANCE-1                USAGE COMP-2.              
           05 WS-DISTANCE-2                USAGE COMP-2.              
           05 WS-DISTANCE-3                USAGE COMP-2.              
           05 WS-DISTANCE-4                USAGE COMP-2.              
           05 WS-DISTANCE-5                USAGE COMP-2.              
           05 WS-DISTANCE-6                USAGE COMP-2.              
                                                                      
           05 WS-TRANSFER-FLIGHT-ID-1      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-FLIGHT-ID-2      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-FLIGHT-ID-3      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-FLIGHT-ID-4      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-FLIGHT-ID-5      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-FLIGHT-ID-6      PIC X(15) VALUE SPACE.     
           05 WS-TRANSFER-TIME-1           PIC S9(9) COMP VALUE 0.    
           05 WS-TRANSFER-TIME-2           PIC S9(9) COMP VALUE 0.    
           05 WS-TRANSFER-TIME-3           PIC S9(9) COMP VALUE 0.    
           05 WS-TRANSFER-TIME-4           PIC S9(9) COMP VALUE 0.    
           05 WS-TRANSFER-TIME-5           PIC S9(9) COMP VALUE 0.    
           05 WS-TRANSFER-TIME-6           PIC S9(9) COMP VALUE 0.    
                                                                      
           05 WS-TRANSFER-MAX-SEAT-1       PIC S9(9) USAGE COMP.      
           05 WS-TRANSFER-MAX-SEAT-2       PIC S9(9) USAGE COMP.      
           05 WS-TRANSFER-MAX-SEAT-3       PIC S9(9) USAGE COMP.      
           05 WS-TRANSFER-MAX-SEAT-4       PIC S9(9) USAGE COMP.      
           05 WS-TRANSFER-MAX-SEAT-5       PIC S9(9) USAGE COMP.      
           05 WS-TRANSFER-MAX-SEAT-6       PIC S9(9) USAGE COMP.    
           05 WS-TRANSFER-AIRPORT-2        PIC X(3) VALUE SPACE.        
           05 WS-TRANSFER-AIRPORT-3        PIC X(3) VALUE SPACE.        
           05 WS-TRANSFER-AIRPORT-4        PIC X(3) VALUE SPACE.        
           05 WS-TRANSFER-AIRPORT-5        PIC X(3) VALUE SPACE.        
           05 WS-FIRST-DEP-AIRPORT-CODE    PIC X(3) VALUE SPACE.        
           05 WS-LAST-ARRIVAL-AIRPORT-CODE PIC X(3) VALUE SPACE.        
           05 WS-TRANSFER-ARV-AIRPORT-CODE PIC X(3) VALUE SPACE.        
           05 WS-USER-CHOICE-LEFT-POS      PIC S9(4) COMP VALUE 0.      
           05 WS-USER-CHOICE-RIGHT-POS     PIC S9(4) COMP VALUE 0.      
           05 WS-USER-CHOICE-COUNT-LEFT    PIC S9(4) COMP VALUE 0.      
           05 WS-USER-CHOICE-COUNT-RIGHT   PIC S9(4) COMP VALUE 0.      
       01 SW-SWITCHES.                                                  
           05 SW-IF-PROGRAM-RUNS-FIRST-TIME              PIC X.         
               88  SO-PROGRAM-RUNS-FIRST-TIME                VALUE 'Y'. 
               88  SO-PROGRAM-RUNS-WITH-DATA                 VALUE 'C'. 
               88  SO-PROGRAM-RUNS-NOT-FIRST-TIME            VALUE 'N'. 
           05 SW-IF-END-OF-ONE-WAY-Q-DATA                    PIC X.     
               88 SO-END-OF-QUEUE                          VALUE 'Y'.   
               88 SO-NOT-END-OF-QUEUE                      VALUE 'N'.   
           05 SW-IF-END-OF-QUEUE-3-DATA                      PIC X.     
               88 SO-NOT-END-OF-FINAL-QUEUE                  VALUE 'Y'. 
               88 SO-END-OF-FINAL-QUEUE                      VALUE 'N'. 
           05 SW-IF-END-OF-CURSOR-1-DATA                     PIC X.     
               88 SO-END-OF-CURSOR-DATA                      VALUE 'Y'. 
               88 SO-NOT-END-OF-CURSOR-DATA                  VALUE 'N'. 
           05 SW-WHAT-TYPE-OF-END                            PIC X.     
               88 SO-FINAL-WITH-COMMAREA                     VALUE '1'. 
               88 SO-FINAL-TERMINATION                       VALUE '2'. 
           05 SW-IF-FLIGHTS-ARE-POSSIBLE                     PIC X.     
               88 SO-POSSIBLE-FLIGHTS                        VALUE '1'. 
               88 SO-IMPOSSIBLE-FLIGHTS                      VALUE '2'. 
           05 SW-IF-THERE-ARE-SEATS                          PIC X.     
               88 SO-THERE-ARE-FREE-SEATS                    VALUE 'Y'. 
               88 SO-THERE-IS-NO-FREE-SEAT                   VALUE 'N'. 
           05 SW-IF-END-OF-FIRST-QUEUE                       PIC X.     
               88 SO-NOT-END-OF-TO-QUEUE                  VALUE 'Y'.    
               88 SO-END-OF-TO-QUEUE                         VALUE 'N'. 
           05 SW-IF-END-OF-SECOND-QUEUE                      PIC X.     
               88 SO-NOT-END-OF-FROM-QUEUE                    VALUE 'Y'.
               88 SO-END-OF-FROM-QUEUE                        VALUE 'N'.
           05 SW-USER-CHOICE                                 PIC X.     
               88 SO-FLIGHT-WAS-CHOSEN                       VALUE '1'. 
               88 SO-DISPLAY-MORE-DETAILS                    VALUE '2'. 
           05 SW-WHERE-TO-GO                                 PIC X.     
               88 SO-GO-BACK-TO-THIS                         VALUE '1'. 
               88 SO-GO-BACK-TO-Z02152                       VALUE '2'. 
                                                                        
                                                                        
                                                                        
           05 SW-IF-GET-SUCCESSFULL                          PIC X.     
               88 SO-GET-WAS-UNSUCCESSFULL                   VALUE 'N'. 
               88 SO-GET-WAS-SUCCESSFULL                     VALUE 'Y'. 
           05 SW-IF-TIME-IS-ACCURATE                         PIC X.     
               88 SO-TIME-IS-ACCURATE                        VALUE 'Y'. 
               88 SO-TIME-IS-NOT-ACCURATE                    VALUE 'N'. 
           05 SW-IF-DISTANCE-OK                              PIC X.     
               88 SO-DISANCE-TO-LONG                         VALUE 'Y'. 
               88 SO-DISTANCE-OK                             VALUE 'N'. 
           05 SW-IF-CONTINUE-WITH-ROW                        PIC X.     
               88 SO-SKIP-THAT-ROW                           VALUE '1'. 
               88 SO-CONTINUE-WITH-ROW                       VALUE '2'. 
           05 SW-IF-SEARH-NEXT-TRANSFER                      PIC X.     
               88 SO-SEARCH-NEXT-TRANSFER                    VALUE '1'. 
               88 SO-DONT-SEARCH-NEXT-TRANSFER               VALUE '2'. 
                                                                        
           05 SW-IF-TO-CHECK-FURTHER                         PIC X.     
               88 SO-DONT-CHECK-FURTHER                      VALUE 'N'. 
               88 SO-CHECK-FURTHER                           VALUE 'Y'. 
           05 SW-WHAT-TYPE-FLIGHT                            PIC X.     
               88 SO-FLIGHTS-TO                              VALUE '1'. 
               88 SO-FLIGHTS-FROM                            VALUE '2'. 
           05 SW-WHAT-SIDE-TO-PAGE                           PIC X.     
               88 SO-PAGE-RIGHT-SIDE                         VALUE '1'. 
               88 SO-PAGE-LEFT-SIDE                          VALUE '2'. 
           05 SW-WHAT-TO-DO-WITH-LOGIC                       PIC X.     
               88 SO-CHOOSE-THIS-FLIGHTS                      VALUE '1'.
               88 SO-DISPLAY-DETAILS                          VALUE '2'.
           05 SW-IF-DATE-IS-CORRECT                           PIC X.    
               88 SO-DATE-IS-CORRECT                          VALUE 'Y'.
               88 SO-DATE-IS-INVALID                          VALUE 'N'.
           05 SW-IF-SUB-FLIGHTS-VALID                         PIC X.    
               88 SO-SUB-FLIGHTS-CORRECT                      VALUE 'Y'.
               88 SO-SUB-FLIGHTS-INVALID                      VALUE 'N'.
           05 SW-IF-RECORD-FOUND                              PIC X.    
               88 SO-RECORD-NOT-FOUND                         VALUE 'Y'.
               88 SO-RECORD-FOUND                             VALUE 'N'.
           05 SW-IF-DEST-FINAL                                PIC X.    
               88 SO-THIS-IS-FINAL-FLIGHT                     VALUE 'Y'.
               88 SO-THIS-IS-NOT-FINAL-FLIGHT                 VALUE 'N'.
           05 SW-IF-END-OF-CNAME-1                            PIC X.    
               88 SO-END-OF-C-NAME1                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-1                      VALUE 'N'.
           05 SW-IF-END-OF-CNAME-2                            PIC X.    
               88 SO-END-OF-C-NAME2                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-2                      VALUE 'N'.
           05 SW-IF-END-OF-CNAME-3                            PIC X.    
               88 SO-END-OF-C-NAME3                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-3                      VALUE 'N'.
           05 SW-IF-END-OF-CNAME-4                            PIC X.    
               88 SO-END-OF-C-NAME4                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-4                      VALUE 'N'.
           05 SW-IF-END-OF-CNAME-5                            PIC X.    
               88 SO-END-OF-C-NAME5                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-5                      VALUE 'N'.
           05 SW-IF-END-OF-CNAME-6                            PIC X.    
               88 SO-END-OF-C-NAME6                           VALUE 'Y'.
               88 SO-NOT-END-OF-C-NAME-6                      VALUE 'N'.
           05 SW-IF-DEP-DATE-VALID                            PIC X.    
               88 SO-DEP-DATE-VALID                           VALUE 'Y'.
               88 SO-DEP-DATE-INVALID                         VALUE 'N'.
           05 SW-IF-ARV-DATE-VALID                            PIC X.    
               88 SO-ARV-DATE-VALID                           VALUE 'Y'.
               88 SO-ARV-DATE-INVALID                         VALUE 'N'.
                                                                        
                                                                        
           COPY DFHAID.                                                 
           COPY ZZEC0215.                                               
           COPY ZZMP0217.                                               
           COPY ZZMM0217.                                               
           COPY ZZM10217.                                               
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
           EXEC SQL INCLUDE T02TAB END-EXEC.                            
           EXEC SQL INCLUDE T05TAB END-EXEC.                            
           EXEC SQL INCLUDE T04TAB END-EXEC.                            
           EXEC SQL INCLUDE T08TAB END-EXEC.                            
           EXEC SQL INCLUDE T13TAB END-EXEC.                            
      * ONLY DIRECT FLIGHTS WITH ALL DATA PROVIDED                      
           EXEC SQL DECLARE C-DIRECT-ONE-WAY-CURSOR   CURSOR            
           FOR                                                          
           SELECT                                                       
           T05.FLIGHT_ID,                                               
           T05.FLIGHT_NUMBER,                                           
           T05.DEPARTURE_AIRPORT_CODE,                                  
           T05.ARRIVAL_AIRPORT_CODE,                                    
           T05.AIRLINE_CODE,                                            
           T13.MAXIMUAL_AMOUNT_OF_SEATS -                               
                  COALESCE(COUNT(T04.FLIGHT_ID), 0)                     
           FROM T05_FLIGHT_TABLE T05                                    
           INNER JOIN T08_TABLE_PLANE_TABLE T08                         
           ON T05.PLANE_ID = T08.PLANE_ID                               
           INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON                    
             T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID                
           LEFT JOIN T04_FLIGHT_SEATS T04 ON                            
              T05.FLIGHT_ID = T04.FLIGHT_ID       
                                                                        
           WHERE DEPARTURE_AIRPORT_CODE =    :T05-DEPARTURE-AIRPORT-CODE
                                    AND                                 
                 ARRIVAL_AIRPORT_CODE   =    :T05-ARRIVAL-AIRPORT-CODE  
                                    AND                                 
                 DATE(DEPARTURE_TIMESTAMP) = :WS-DEPARTURE-DATE         
                                                                        
                                                                        
                                    AND                                 
                 FLIGHT_STATUS <> :CT-DELETED-STATUS                    
           GROUP BY                                                     
              T05.FLIGHT_ID,                                            
              T05.FLIGHT_NUMBER,                                        
              T05.DEPARTURE_AIRPORT_CODE,                               
              T05.ARRIVAL_AIRPORT_CODE,                                 
              T05.AIRLINE_CODE,                                         
              T13.MAXIMUAL_AMOUNT_OF_SEATS                              
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
      * FLIGHTS WITH 1 TRANSFER WITH DESTINATION AIRPORT                
      * THIS CURSOR WILL GET CODE OF THE AIRPORT WHERE TRANSFER WILL    
      * BE MADE, AND ALSO WILL RETURN ID OF FIRST AND SECOND            
      * FLIGHT                                                          
           EXEC SQL                                                     
           DECLARE C-NOT-DIRECT-1-TRANSFER-ONEWAY CURSOR                
           FOR                                                          
           SELECT                                                       
           DISTINCT                                                     
           F1.ARRIVAL_AIRPORT_CODE,                                     
           F1.FLIGHT_ID,                                                
           F2.FLIGHT_ID,                                                
           TIMESTAMPDIFF(2, CHAR(F2.DEPARTURE_TIMESTAMP -               
           F1.ARRIVAL_TIMESTAMP)),                                      
           F1.AIRLINE_CODE                                              
      * QUERY WILL SEARCH FOR FLIGHT THAT HAVE EXACLY 1 TRANSFER        
      * BETWEEN DEPARTURE AIRPORT AND ARRIVAL AIRPORT   
                                                                        
             FROM T05_FLIGHT_TABLE F1                                   
                                                                        
               JOIN T05_FLIGHT_TABLE F2 ON F1.ARRIVAL_AIRPORT_CODE =    
                                                                        
                                               F2.DEPARTURE_AIRPORT_CODE
                                                                        
             WHERE F1.DEPARTURE_AIRPORT_CODE =                          
                                        :WS-FIRST-DEP-AIRPORT-CODE      
                                                                        
             AND F2.ARRIVAL_AIRPORT_CODE = :WS-LAST-ARRIVAL-AIRPORT-CODE
                                                                        
                                                                        
      * DATE THAT USER PROVIDED (WHEN HE WANTS TO DEPARTURE)            
             AND DATE(F1.DEPARTURE_TIMESTAMP) = :WS-DEPARTURE-DATE      
                                                                        
      * DATE TAHT USER PROVIDED ( WHEN HE WANTS TO ARRIVE )             
             AND DATE(F2.ARRIVAL_TIMESTAMP ) = :WS-ARRIVAL-DATE         
             AND                                                        
                F1.FLIGHT_STATUS <> :CT-DELETED-STATUS                  
             AND                                                        
                F2.FLIGHT_STATUS <> :CT-DELETED-STATUS                  
           FOR FETCH ONLY                                               
           END-EXEC.                                                    
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
      * CURSOR WILL RETRIVE ALL FLIGHTS WITH 2 TRANSFERS                
           EXEC SQL                                                     
           DECLARE C-NOT-DIRECT-2-TRANSFER-ONEWAY CURSOR FOR            
           SELECT  DISTINCT F1.ARRIVAL_AIRPORT_CODE,                    
                            F2.ARRIVAL_AIRPORT_CODE,                    
                            F1.FLIGHT_ID,                               
                            F2.FLIGHT_ID,       
                            F3.FLIGHT_ID,                               
      * TIME BETWEEN LANDING ON SECOND AIRPORT AND DEPARTUING FORM THERE
                   TIMESTAMPDIFF(2, CHAR(F2.DEPARTURE_TIMESTAMP -       
                         F1.ARRIVAL_TIMESTAMP)),                        
      * TIME BETWEEN LANDING ON THIRD AIRPORT AND DEPARTUING FORM THERE 
                  TIMESTAMPDIFF(2, CHAR(F3.DEPARTURE_TIMESTAMP -        
                         F2.ARRIVAL_TIMESTAMP)),                        
                   F1.AIRLINE_CODE                                      
           FROM T05_FLIGHT_TABLE F1                                     
           INNER JOIN T05_FLIGHT_TABLE F2 ON F1.ARRIVAL_AIRPORT_CODE =  
           F2.DEPARTURE_AIRPORT_CODE                                    
           INNER JOIN T05_FLIGHT_TABLE F3 ON F2.ARRIVAL_AIRPORT_CODE =  
           F3.DEPARTURE_AIRPORT_CODE                                    
           WHERE F1.DEPARTURE_AIRPORT_CODE = :WS-FIRST-DEP-AIRPORT-CODE 
           AND F3.ARRIVAL_AIRPORT_CODE = :WS-LAST-ARRIVAL-AIRPORT-CODE  
           AND DATE(F1.DEPARTURE_TIMESTAMP) = :WS-DEPARTURE-DATE        
           AND DATE(F3.ARRIVAL_TIMESTAMP ) = :WS-ARRIVAL-DATE           
           END-EXEC.                                                    
                                                                        
                                                                        
                                                                        
      *-----------------------------------------------------------------
                                                                        
                                                                        
      * CURSORS BELOW WILL BE USED TO GET DATA RELATED TO RETURN FLIGHTS
           EXEC SQL                                                     
            DECLARE C-RETURN-FLIGHT-WITH-ALL CURSOR FOR                 
             SELECT                                                     
             T05.FLIGHT_ID,                                             
             T05.FLIGHT_NUMBER,                                         
             MAXIMUAL_AMOUNT_OF_SEATS -                                 
             COALESCE(COUNT(T04.FLIGHT_ID), 0)                          
                                                                        
                  FROM                                                  
                       T05_FLIGHT_TABLE T05                             
              INNER JOIN T08_TABLE_PLANE_TABLE  T08    
                     ON T05.PLANE_ID =  T08.PLANE_ID                    
                                                                        
            INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON                   
            T08.TYPE_OF_SEATS_ID   = T13.TYPE_OF_SEATS_ID               
                                                                        
            LEFT JOIN                                                   
            T04_FLIGHT_SEATS T04 ON T05.FLIGHT_ID =  T04.FLIGHT_ID      
                                                                        
              WHERE                                                     
             T05.DEPARTURE_AIRPORT_CODE = :T05-DEPARTURE-AIRPORT-CODE   
                                                                        
             AND T05.ARRIVAL_AIRPORT_CODE = :T05-ARRIVAL-AIRPORT-CODE   
                                                                        
             AND DATE(T05.DEPARTURE_TIMESTAMP) = :WS-DEPARTURE-DATE     
                                                                        
             AND DATE(T05.ARRIVAL_TIMESTAMP) = :WS-ARRIVAL-DATE         
             AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS                
              GROUP BY                                                  
                T05.FLIGHT_ID,                                          
                T05.FLIGHT_NUMBER,                                      
                MAXIMUAL_AMOUNT_OF_SEATS                                
                                                                        
           END-EXEC.                                                    
                                                                        
      * CURSOR WILL SEARCH FOR 2WAY DIRECT FLIGHTS                      
           EXEC SQL                                                     
             DECLARE  C-DIRECT-2WAY-CURSOR    CURSOR FOR                
            SELECT                                                      
            FIRST.FLIGHT_ID                                             
            ,FIRST.FLIGHT_NUMBER                                        
            ,SECOND.FLIGHT_ID                                           
            ,SECOND.FLIGHT_NUMBER                                       
            ,FIRSTT13.MAXIMUAL_AMOUNT_OF_SEATS -                        
                 COALESCE(COUNT(FIRSTT04.FLIGHT_ID), 0)                 
                                                                        
            ,SECONDT13.MAXIMUAL_AMOUNT_OF_SEATS -  
                 COALESCE(COUNT(FIRSTT04.FLIGHT_ID), 0)                 
            ,FIRST.AIRLINE_CODE                                         
            ,SECOND.AIRLINE_CODE                                        
            FROM T05_FLIGHT_TABLE FIRST                                 
            INNER JOIN                                                  
            T05_FLIGHT_TABLE SECOND                                     
            ON FIRST.ARRIVAL_AIRPORT_CODE =                             
            SECOND.DEPARTURE_AIRPORT_CODE                               
            AND                                                         
            SECOND.ARRIVAL_AIRPORT_CODE =                               
            FIRST.DEPARTURE_AIRPORT_CODE                                
            INNER JOIN                                                  
                  T08_TABLE_PLANE_TABLE FIRSTT08 ON                     
             FIRST.PLANE_ID = FIRSTT08.PLANE_ID                         
            INNER JOIN                                                  
            T13_TYPE_OF_SEATS_TABLE FIRSTT13                            
             ON FIRSTT08.TYPE_OF_SEATS_ID =                             
                FIRSTT13.TYPE_OF_SEATS_ID                               
            LEFT JOIN T04_FLIGHT_SEATS FIRSTT04 ON                      
             FIRSTT04.FLIGHT_ID = FIRST.FLIGHT_ID                       
             INNER JOIN                                                 
                   T08_TABLE_PLANE_TABLE SECONDT08 ON                   
              SECOND.PLANE_ID = SECONDT08.PLANE_ID                      
             INNER JOIN                                                 
             T13_TYPE_OF_SEATS_TABLE SECONDT13                          
              ON SECONDT08.TYPE_OF_SEATS_ID =                           
                 SECONDT13.TYPE_OF_SEATS_ID                             
              LEFT JOIN T04_FLIGHT_SEATS SECONDT04 ON                   
                SECONDT04.FLIGHT_ID = SECOND.FLIGHT_ID                  
              WHERE                                                     
                  DATE(FIRST.DEPARTURE_TIMESTAMP) =                     
               :WS-DEPARTURE-DATE                                       
              AND                                                       
                  DATE(SECOND.ARRIVAL_TIMESTAMP)  =  :WS-ARRIVAL-DATE   
              AND  (FIRST.ARRIVAL_AIRPORT_CODE =                        
                               :T05-ARRIVAL-AIRPORT-CODE      
              AND FIRST.DEPARTURE_AIRPORT_CODE =                        
                          :T05-DEPARTURE-AIRPORT-CODE )                 
               AND FIRST.ARRIVAL_TIMESTAMP < SECOND.DEPARTURE_TIMESTAMP 
               AND   FIRST.FLIGHT_STATUS <> :CT-DELETED-STATUS          
               AND   SECOND.FLIGHT_STATUS <> :CT-DELETED-STATUS         
               AND (FIRSTT04.RESERVATION_STATUS <> :CT-DELETED-STATUS   
                 OR  FIRSTT04.RESERVATION_STATUS  IS NULL )             
               AND (SECONDT04.RESERVATION_STATUS <> :CT-DELETED-STATUS  
                 OR SECONDT04.RESERVATION_STATUS  IS NULL )             
                                                                        
              GROUP BY                                                  
              FIRST.FLIGHT_ID,                                          
              FIRST.FLIGHT_NUMBER,                                      
              SECOND.FLIGHT_ID,                                         
              SECOND.FLIGHT_NUMBER ,                                    
              FIRSTT13.MAXIMUAL_AMOUNT_OF_SEATS,                        
              SECONDT13.MAXIMUAL_AMOUNT_OF_SEATS                        
              ,FIRST.AIRLINE_CODE                                       
              ,SECOND.AIRLINE_CODE                                      
             END-EXEC                                                   
      * THIS CURSOR WILL RETURN ALL FLIGHTS THAT COMES FROM             
      * AIRPORT PROVIDED BY THE USER                                    
           EXEC SQL                                                     
            DECLARE C-FIND-0-TRANSFER CURSOR FOR                        
             SELECT T05.FLIGHT_ID,                                      
                      T05.FLIGHT_NUMBER,                                
                      T05.DEPARTURE_TIMESTAMP,                          
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.DEPARTURE_AIRPORT_CODE,                       
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      T05.AIRLINE_CODE,                                 
                      (T13.MAXIMUAL_AMOUNT_OF_SEATS -                   
                      COALESCE(COUNT(T04.FLIGHT_ID), 0) )               
               FROM T05_FLIGHT_TABLE T05                                
               LEFT  JOIN T04_FLIGHT_SEATS T04 ON                       
                 T05.FLIGHT_ID = T04.FLIGHT_ID   
              INNER JOIN T08_TABLE_PLANE_TABLE T08 ON              
                T08.PLANE_ID = T05.PLANE_ID                        
              INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON            
                T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID        
               WHERE T05.DEPARTURE_AIRPORT_CODE =                  
                             :WS-Z02172-ORIGIN-AIRPORT-IATA        
               AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS         
              GROUP BY                                             
                     T05.FLIGHT_ID,                                
                     T05.FLIGHT_NUMBER,                            
                     T05.DEPARTURE_TIMESTAMP,                      
                     T13.MAXIMUAL_AMOUNT_OF_SEATS,                 
                     T05.ARRIVAL_AIRPORT_CODE,                     
                     T05.DEPARTURE_AIRPORT_CODE,                   
                     T05.ARRIVAL_TIMESTAMP,                        
                     T05.AIRLINE_CODE                              
          END-EXEC                                                 
     * THIS CURSOR WILL FETCH ALL DATA OBOUT FIRST TRANSFER FLIGHT 
     * CUROSR SEARCH FLIGHTS THAT HAVE DEPARTURE AIRPORT           
     * SAME AS ARRIVAL AIRPORT FROM PREVOIUS FLIGHT                
     * AND IT'S WAITING TIME ( TIME BETWEEEN ARRIVAL AND DEPARTERE)
     * IS BETWWEN 1 HOUR AND 15 HOURS                              
                                                                   
          EXEC SQL                                                 
           DECLARE C-FIND-1-TRANSFER CURSOR FOR                    
            SELECT   T05.FLIGHT_ID,                                
                     T05.ARRIVAL_TIMESTAMP,                        
                     T05.ARRIVAL_AIRPORT_CODE,                     
                     (T13.MAXIMUAL_AMOUNT_OF_SEATS -               
                     COALESCE(COUNT(T04.FLIGHT_ID), 0) ),          
                     T05.DEPARTURE_AIRPORT_CODE                    
              FROM T05_FLIGHT_TABLE T05                            
              LEFT  JOIN T04_FLIGHT_SEATS T04 ON                   
                T05.FLIGHT_ID = T04.FLIGHT_ID                      
              INNER JOIN T08_TABLE_PLANE_TABLE T08 ON              
                T08.PLANE_ID = T05.PLANE_ID     
               INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON                
                 T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID            
                WHERE T05.DEPARTURE_AIRPORT_CODE =                      
                              :T05-ARRIVAL-AIRPORT-CODE                 
                AND   T05.DEPARTURE_TIMESTAMP >                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 1 HOUR AND           
                      T05.DEPARTURE_TIMESTAMP <                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 15 HOURS             
                AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS             
               GROUP BY                                                 
                      T05.FLIGHT_ID,                                    
                      T13.MAXIMUAL_AMOUNT_OF_SEATS,                     
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.DEPARTURE_AIRPORT_CODE                        
           END-EXEC.                                                    
      * THIS CURSOR WILL FETCH ALL DATA OBOUT SECOND TRANSFER FLIGHT    
      * CUROSR SEARCH FLIGHTS THAT HAVE DEPARTURE AIRPORT               
      * SAME AS ARRIVAL AIRPORT FROM PREVOIUS FLIGHT                    
      * AND IT'S WAITING TIME ( TIME BETWEEEN ARRIVAL AND DEPARTERE)    
      * IS BETWWEN 1 HOUR AND 15 HOURS                                  
           EXEC SQL                                                     
            DECLARE C-FIND-2-TRANSFER   CURSOR FOR                      
             SELECT   T05.FLIGHT_ID,                                    
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      (T13.MAXIMUAL_AMOUNT_OF_SEATS -                   
                      COALESCE(COUNT(T04.FLIGHT_ID), 0) ),              
                      T05.DEPARTURE_AIRPORT_CODE                        
               FROM T05_FLIGHT_TABLE T05                                
               LEFT  JOIN T04_FLIGHT_SEATS T04 ON                       
                 T05.FLIGHT_ID = T04.FLIGHT_ID                          
               INNER JOIN T08_TABLE_PLANE_TABLE T08 ON                  
                 T08.PLANE_ID = T05.PLANE_ID                            
               INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON                
                 T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID
                WHERE T05.DEPARTURE_AIRPORT_CODE =                      
                              :T05-ARRIVAL-AIRPORT-CODE                 
                AND   T05.DEPARTURE_TIMESTAMP >                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 1 HOUR AND           
                      T05.DEPARTURE_TIMESTAMP <                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 15 HOURS             
                AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS             
               GROUP BY                                                 
                      T05.FLIGHT_ID,                                    
                      T13.MAXIMUAL_AMOUNT_OF_SEATS,                     
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.DEPARTURE_AIRPORT_CODE                        
           END-EXEC.                                                    
      * THIS CURSOR WILL FETCH ALL DATA OBOUT THIRD  TRANSFER FLIGHT    
      * CUROSR SEARCH FLIGHTS THAT HAVE DEPARTURE AIRPORT               
      * SAME AS ARRIVAL AIRPORT FROM PREVOIUS FLIGHT                    
      * AND IT'S WAITING TIME ( TIME BETWEEEN ARRIVAL AND DEPARTERE)    
      * IS BETWWEN 1 HOUR AND 15 HOURS                                  
           EXEC SQL                                                     
            DECLARE C-FIND-3-TRANSFER CURSOR FOR                        
             SELECT   T05.FLIGHT_ID,                                    
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      (T13.MAXIMUAL_AMOUNT_OF_SEATS -                   
                      COALESCE(COUNT(T04.FLIGHT_ID), 0) ),              
                      T05.DEPARTURE_AIRPORT_CODE                        
               FROM T05_FLIGHT_TABLE T05                                
               LEFT  JOIN T04_FLIGHT_SEATS T04 ON                       
                 T05.FLIGHT_ID = T04.FLIGHT_ID                          
               INNER JOIN T08_TABLE_PLANE_TABLE T08 ON                  
                 T08.PLANE_ID = T05.PLANE_ID                            
               INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON                
                 T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID            
                WHERE T05.DEPARTURE_AIRPORT_CODE =                      
                              :T05-ARRIVAL-AIRPORT-CODE
                AND   T05.DEPARTURE_TIMESTAMP >                       
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 1 HOUR AND         
                      T05.DEPARTURE_TIMESTAMP <                       
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 15 HOURS           
                AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS           
               GROUP BY                                               
                      T05.FLIGHT_ID,                                  
                      T13.MAXIMUAL_AMOUNT_OF_SEATS,                   
                      T05.ARRIVAL_AIRPORT_CODE,                       
                      T05.ARRIVAL_TIMESTAMP,                          
                      T05.DEPARTURE_AIRPORT_CODE                      
           END-EXEC.                                                  
      * THIS CURSOR WILL FETCH ALL DATA OBOUT FORTH  TRANSFER FLIGHT  
      * CUROSR SEARCH FLIGHTS THAT HAVE DEPARTURE AIRPORT             
      * SAME AS ARRIVAL AIRPORT FROM PREVOIUS FLIGHT                  
      * AND IT'S WAITING TIME ( TIME BETWEEEN ARRIVAL AND DEPARTERE)  
      * IS BETWWEN 1 HOUR AND 15 HOURS                                
           EXEC SQL                                                   
            DECLARE C-FIND-4-TRANSFER CURSOR FOR                      
             SELECT   T05.FLIGHT_ID,                                  
                      T05.ARRIVAL_TIMESTAMP,                          
                      T05.ARRIVAL_AIRPORT_CODE,                       
                      (T13.MAXIMUAL_AMOUNT_OF_SEATS -                 
                      COALESCE(COUNT(T04.FLIGHT_ID), 0) ),            
                      T05.DEPARTURE_AIRPORT_CODE                      
               FROM T05_FLIGHT_TABLE T05                              
               LEFT  JOIN T04_FLIGHT_SEATS T04 ON                     
                 T05.FLIGHT_ID = T04.FLIGHT_ID                        
               INNER JOIN T08_TABLE_PLANE_TABLE T08 ON                
                 T08.PLANE_ID = T05.PLANE_ID                          
               INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON              
                 T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID          
                WHERE T05.DEPARTURE_AIRPORT_CODE =                    
                              :T05-ARRIVAL-AIRPORT-CODE               
                AND   T05.DEPARTURE_TIMESTAMP >                       
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 1 HOUR AND  
                      T05.DEPARTURE_TIMESTAMP <                       
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 15 HOURS           
                AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS           
               GROUP BY                                               
                      T05.FLIGHT_ID,                                  
                      T13.MAXIMUAL_AMOUNT_OF_SEATS,                   
                      T05.ARRIVAL_AIRPORT_CODE,                       
                      T05.ARRIVAL_TIMESTAMP,                          
                      T05.DEPARTURE_AIRPORT_CODE                      
           END-EXEC.                                                  
      * THIS CURSOR WILL FETCH ALL DATA OBOUT FIFTH  TRANSFER FLIGHT  
      * CUROSR SEARCH FLIGHTS THAT HAVE DEPARTURE AIRPORT             
      * SAME AS ARRIVAL AIRPORT FROM PREVOIUS FLIGHT                  
      * AND IT'S WAITING TIME ( TIME BETWEEEN ARRIVAL AND DEPARTERE)  
      * IS BETWWEN 1 HOUR AND 15 HOURS                                
      * THIS CURSOR NOT LIKE THE PREVIOUS ONES HAVE SEARCH CONDITION  
      * PLACED ON DESTINATION AIRPORT -> BECAUSE IT IS THE LAST       
      * POSSIBLE TRANSFER, SO IT NOT MAKE SENS TO SEARCH ALL FLIGHTS  
      * WE ARE GONNA GET ONLY VALID FLIGHTS AT THIS POINT             
           EXEC SQL                                                   
            DECLARE C-FIND-5-TRANSFER  CURSOR FOR                     
             SELECT   T05.FLIGHT_ID,                                  
                      T05.ARRIVAL_TIMESTAMP,                          
                      T05.ARRIVAL_AIRPORT_CODE,                       
                      (T13.MAXIMUAL_AMOUNT_OF_SEATS -                 
                      COALESCE(COUNT(T04.FLIGHT_ID), 0) ),            
                      T05.DEPARTURE_AIRPORT_CODE                      
               FROM T05_FLIGHT_TABLE T05                              
               LEFT  JOIN T04_FLIGHT_SEATS T04 ON                     
                 T05.FLIGHT_ID = T04.FLIGHT_ID                        
               INNER JOIN T08_TABLE_PLANE_TABLE T08 ON                
                 T08.PLANE_ID = T05.PLANE_ID                          
               INNER JOIN T13_TYPE_OF_SEATS_TABLE T13 ON              
                 T13.TYPE_OF_SEATS_ID = T08.TYPE_OF_SEATS_ID          
                WHERE T05.DEPARTURE_AIRPORT_CODE =                    
                              :T05-ARRIVAL-AIRPORT-CODE         
                AND   T05.DEPARTURE_TIMESTAMP >                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 1 HOUR AND           
                      T05.DEPARTURE_TIMESTAMP <                         
               TIMESTAMP(:T05-ARRIVAL-TIMESTAMP) + 15 HOURS             
               AND T05.ARRIVAL_AIRPORT_CODE =                           
                                   :WS-Z02172-DEST-AIRPORT-IATA         
                                                                        
                AND T05.FLIGHT_STATUS <> :CT-DELETED-STATUS             
               GROUP BY                                                 
                      T05.FLIGHT_ID,                                    
                      T13.MAXIMUAL_AMOUNT_OF_SEATS,                     
                      T05.ARRIVAL_AIRPORT_CODE,                         
                      T05.ARRIVAL_TIMESTAMP,                            
                      T05.DEPARTURE_AIRPORT_CODE                        
           END-EXEC.                                                    
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(17294).                                     
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY 'Z02172------------START---------------'             
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           DISPLAY 'Z02172------------END---------------'               
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                  1000-INIT                                      
      ******************************************************************
       1000-INIT.                                                       
           PERFORM 1005-CHECK-IF-FIRST-TIME                             
           .                                                            
      ******************************************************************
      *                 1005-CHECK-IF-FIRST-TIME                        
      * DEPENDING ON PROGRAM MODE, PARAGRAPH WILL SET PROGRAM FLAGS     
      *                                                                 
      *                                                                 
      * PROGRAM CAN HAVE 3 MAIN MODES             
      * 1. NOT FIRST TIME (SO-M-NOT-FIRST) -> PROGRAM WAS RUNNING BEFORE
      * AND NOW IS RETRIGGERED BY THE USER (USER PRESSED ATTENTION KEY) 
      *                                                                 
      * 2. FIRST TIME WITHOUT DATA(SO-M-FIRST-WITHOUT) ->               
      * FIRST TIME PROGRAM RUNS (USER DIDN'T PROVIDE ANY DATA NOR       
      * PROGRAM FOUND ANY DATA )                                        
      *                                                                 
      * 3. FIRST TIME WITH DATA (SO-M-FIRST-WITH) WILL BE USED IN       
      * SCENARIO LIKE THIS:                                             
      *    - PROGRAM WAS STARTED AND FOUND SOME FLIGHTS                 
      *    - USER MADE SOME ERROR (FOR EXMAPLE HE CHOOSE 2 FLIGHTS)     
      *    - PROGRAM Z02141 WAS CALLED                                  
      *    - USER PRESSED F3 TO GOBACK TO THIS PROGRAM                  
      *    - THIS PROGRAM STARTS BUT IT ALREADY FOUNDED FLIGHTS         
      *  THAT MEETS CRITERIA, SO THIS PROGRAM HAS TO JUST DISPLAY       
      * THOSE DATA FROM THE QUEUE                                       
      ******************************************************************
       1005-CHECK-IF-FIRST-TIME.                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0215                              
           DISPLAY 'Z02172 MODE: ' SW-M-WHAT-MODE                       
           EVALUATE TRUE                                                
      * CALLED FIRST TIME                                               
           WHEN SO-M-FIRST-WITHOUT                                      
               PERFORM 1010-CICS-IGNORE                                 
               PERFORM 1011-DELETE-THE-QUEUES                           
               PERFORM 1015-SET-START-FLAGS                             
      * CALLED FIRST TIME BUT WE HAVE ALREADY SOME DATA                 
           WHEN SO-M-FIRST-WITH                                         
               SET SO-PROGRAM-RUNS-WITH-DATA  TO TRUE                   
               SET SO-M-NOT-FIRST TO TRUE                               
      * PROGRAM WAS CALLED BY THE USER PRESSING ATTENTION KEY           
           WHEN SO-M-NOT-FIRST                                          
               SET SO-PROGRAM-RUNS-NOT-FIRST-TIME  TO TRUE              
      * WHEN OTHER CLOUSE WILL BE TRUE ONLY IF THIS WAS INVALID CALL    
      * INVALID CALL MEANS THAT PROGRAM WAS CALLED WITH INVALID DATA    
           WHEN OTHER   
               PERFORM 3001-SEND-INVALID-CALL-MSG                       
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      1010-CICS-IGNORE                           
      ******************************************************************
       1010-CICS-IGNORE.                                                
           EXEC CICS                                                    
            IGNORE CONDITION ERROR                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                     1011-DELETE-THE-QUEUES                      
      * PARAGRAPH WILL DELETE ALL QUEUES THAT CAN BE USED BY            
      * THE PROGRAM ( QUEUES WILL BE AUTOMATICLY CREATED WHEN           
      * PROGRAM WILL TRY TO WRITE TO IT)                                
      ******************************************************************
       1011-DELETE-THE-QUEUES.                                          
           PERFORM 1012-DELETE-ONE-WAY-Q                                
           PERFORM 1013-DELETE-RETURN-DIRECT-Q                          
           PERFORM 1014-DELETE-FLIGHT-TO-QUEUE                          
           PERFORM 1016-DELETE-FLIGHT-FROM-QUEUE                        
           PERFORM 1017-DELETE-TO-AND-FROM-QUEUE                        
           .                                                            
      ******************************************************************
      *                    1012-DELETE-ONE-WAY-Q                        
      * THIS QUEUE IS USED TO STORE INFORMATIONS ABOUT                  
      * 1. DIRECT ONE WAY FLIGHT                                        
      * 2. NON DIRECT ONE-WAY-FLIGHT                                    
      ******************************************************************
       1012-DELETE-ONE-WAY-Q.                                           
           EXEC CICS                                                    
           DELETEQ TS                                                   
           QUEUE(CT-ONEWAY-QUEUE)                                       
           NOHANDLE      
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) THEN                            
            CONTINUE                                                    
           ELSE                                                         
            PERFORM 2200-CHECK-EIBRESP                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    1013-DELETE-RETURN-DIRECT-Q                  
      ******************************************************************
       1013-DELETE-RETURN-DIRECT-Q.                                     
           EXEC CICS                                                    
           DELETEQ TS                                                   
           QUEUE(CT-RETURN-DIRECT-QUEUE)                                
           NOHANDLE                                                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) OR DFHRESP(NORMAL) THEN         
            CONTINUE                                                    
           ELSE                                                         
            PERFORM 2200-CHECK-EIBRESP                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     1014-DELETE-FLIGHT-TO-QUEUE                 
      * THIS QUEUE WILL STORE INFORMATIONS ABOUT FLIGHT "TO" FOR        
      * NON DIRECT RETURN FLIGHTS                                       
      ******************************************************************
       1014-DELETE-FLIGHT-TO-QUEUE.                                     
           EXEC CICS                                                    
           DELETEQ TS                                                   
           QUEUE(CT-FLIGHT-TO-QUEUE)                                    
           NOHANDLE                                                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) OR DFHRESP(NORMAL) THEN         
            DISPLAY 'USUSNIETO FIRST QUEUE '                            
           ELSE     
            PERFORM 2200-CHECK-EIBRESP                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    1016-DELETE-FLIGHT-FROM-QUEUE                
      * THIS IS TEMPORARY QUEUE THAT WILL STORE INFORMATIONS            
      * ABOUT FLIGHTS "FROM" FOR RETURN NON DIRECT FLIGHTS              
      ******************************************************************
       1016-DELETE-FLIGHT-FROM-QUEUE.                                   
           EXEC CICS                                                    
           DELETEQ TS                                                   
           QUEUE(CT-FLIGHT-FROM-QUEUE)                                  
           NOHANDLE                                                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) OR DFHRESP(NORMAL)  THEN        
            DISPLAY 'USUSNIETO SEC   QUEUE '                            
           ELSE                                                         
            PERFORM 2200-CHECK-EIBRESP                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 1017-DELETE-TO-AND-FROM-QUEUE                   
      * THIS QUEUE WILL STORE DATA ABOUT NON DIRECT                     
      * RETURN FLIGHTS                                                  
      ******************************************************************
       1017-DELETE-TO-AND-FROM-QUEUE.                                   
           EXEC CICS                                                    
           DELETEQ TS                                                   
           QUEUE(CT-TO-AND-FROM-QUEUE)                                  
           NOHANDLE                                                     
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(QIDERR) OR DFHRESP(NORMAL)  THEN        
            DISPLAY 'USUSNIETO THIRD QUEUE '                            
           ELSE                                                         
            PERFORM 2200-CHECK-EIBRESP                                  
           END-IF     
           .                                                            
      ******************************************************************
      *                    1015-SET-START-FLAGS                         
      ******************************************************************
       1015-SET-START-FLAGS.                                            
           SET SO-NOT-END-OF-QUEUE      TO TRUE                         
           SET SO-NOT-END-OF-CURSOR-DATA  TO TRUE                       
           SET SO-M-NOT-FIRST             TO TRUE                       
           SET SO-PROGRAM-RUNS-FIRST-TIME TO TRUE                       
           .                                                            
      ******************************************************************
      *                          2000-PROCESS                           
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
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'SERIOUS ERROR IN Z02172' TO                        
                                   WS-Z02141-I-ERROR-MESSAGE(1)         
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-EVALUATE                                                 
           .      
      ******************************************************************
      *                    2001-PROCESS-FIRST-TIME                      
      * BASED ON THE FLAGS SET BY THE USER IN Z02152 PROGRAM            
      * THIS PARAGRAPH WILL DISTRIBUTE PROGRAM LOGIC TO VALID           
      * PARAGRAPHS                                                      
      *                                                                 
      * WE GOT 4 MAIN TYPES OF FLIGHT SEARCHING:                        
      *                                                                 
      *     1. DIRECT AND ONE-WAY                                       
      *     2. NOT DIRECT AND ONE-WAY                                   
      *     3. DIRECT AND 2WAY                                          
      *     4. NOT DIRECT AND 2 WAY                                     
      ******************************************************************
       2001-PROCESS-FIRST-TIME.                                         
           EVALUATE TRUE                                                
            WHEN SO-DIRECT-FLIGHT                                       
             EVALUATE TRUE                                              
              WHEN SO-ONE-WAY-FLIGHT                                    
                                                                        
                 PERFORM 2101-DIRECT-ONEWAY-WITH-ALL                    
                                                                        
              WHEN SO-NOT-ONE-WAY-FLIGHT                                
                                                                        
                 PERFORM 2108-FIND-DIRECT-2-WAY-FLIGHT                  
                                                                        
             END-EVALUATE                                               
            WHEN SO-NOT-DIRECT-FLIGHT                                   
                                                                        
             EVALUATE TRUE                                              
             WHEN SO-ONE-WAY-FLIGHT                                     
                                                                        
                SET SO-SEARCH-FOR-N-DIRECT-ONEWAY TO TRUE               
                PERFORM 2105-NOT-DIRECT-ONEWAY                          
                PERFORM 2021-DISPLAY-FIRST-15-FLIGHTS                   
                                                                        
             WHEN SO-NOT-ONE-WAY-FLIGHT                                 
                                                                        
                PERFORM 2109-NON-DIRECT-RETURN-FLIGHTS                  
                                                                        
             END-EVALUATE                                               
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2002-PROCESS-WITH-DATA                      
      * HERE PROGRAM NEEDS TO DISPLAY ALL FLIGHTS FROM QUEUE            
      * BECAUSE IT WAS ALREAY SAVED                                     
      *                                                                 
      * PARAGRAPH CAN                                                   
      * 1 DISPLAY ONE-WAY FLIGHT                                        
      * 2. DISPLAY RETURN FLIGHTS                                       
      *                                                                 
      * PARAGRAPH WILL DISPLAY THIS PAGE OF DATA THAT WAS THE LAST      
      * PAGE THAT USER HAD SEEN                                         
      *                                                                 
      * THAT LOGIC WILL BE CREATED BY TWO VARIABLES:                    
      *  WS-Z02172-FIRST-REC-ID AND WS-Z02172-LAST-REC-ID               
      * WHERE "FIRST" STORES NUMBER OF FIRST RECORD ON THE PAGE         
      * AND "LAST" STORES NUMBER OF LAST RECORD ON THE PAGE             
      *                                                                 
      ******************************************************************
       2002-PROCESS-WITH-DATA.                                          
           IF SO-ONE-WAY-FLIGHT THEN                                    
              MOVE WS-Z02172-FIRST-REC-ID TO WS-Z02172-LAST-REC-ID      
              PERFORM 2021-DISPLAY-FIRST-15-FLIGHTS                     
              SET SO-FINAL-WITH-COMMAREA TO TRUE                        
           ELSE                                                         
              IF  SO-SEARCH-FOR-N-DIRECT-RETURN  THEN                   
                 MOVE WS-Z02172-FIRST-Q5-REC-ID TO                      
                   WS-Z02172-LAST-Q5-REC-ID                             
                 PERFORM 2117-DISPLAY-NEXT-4-2WAY-N-DIR                 
              ELSE                                                      
                 MOVE WS-Z02172-FIRST-Q5-REC-ID TO
                   WS-Z02172-LAST-Q5-REC-ID                             
                PERFORM 2083-DISPLAY-NEXT-4-2WAY-DIR                    
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2003-PROCESS-NOT-FIRST-TIME                       
      * THIS PARAGRAPH WILL BE PERFORMED WHEN PROGRAM ISN'T CALLED FOR  
      * THE FIRST TIME. USER HAD TO PRESS SOME KEY TO START THIS PROCESS
      * HERE PROGRAM WILL EVALUATE THROUGH USER CHOICES                 
      ******************************************************************
       2003-PROCESS-NOT-FIRST-TIME.                                     
           EVALUATE EIBAID                                              
           WHEN DFHPF3                                                  
              SET SO-FINAL-TERMINATION TO TRUE                          
           WHEN DFHENTER                                                
      * IF USER PRESSED ENTER PROGRAM HAS TO VALIDATE USER INPUT        
             IF SO-ONE-WAY-FLIGHT THEN                                  
                PERFORM 2030-PROCESS-INPUT-ONE-WAY                      
                                                                        
             ELSE                                                       
                PERFORM 2075-PROCESS-INPUT-2-WAY                        
             END-IF                                                     
           WHEN DFHPF8                                                  
      * PROGRAM WILL DO PAGINING LOGIC UPWARDS                          
             IF SO-ONE-WAY-FLIGHT THEN                                  
      * DISPLAY NEXT 15 ONE WAY FLIGHTS (DIRECT OR NOT DIRECT)          
              PERFORM 2050-DISPLAY-NEXT-15-ONE-WAY                      
             ELSE                                                       
              PERFORM 2331-DISPLAY-NEXT-2WAY                            
             END-IF                                                     
           WHEN DFHPF7                                                  
      * PROGRAM WILL DO PAGINING LOGIC BACKWARDS                        
             IF SO-ONE-WAY-FLIGHT THEN                                  
              PERFORM 2051-DISPLAY-PREV-15-ONE-WAY                      
             ELSE     
              PERFORM 2330-DISPLAY-PREVIOUS-2WAY                        
             END-IF                                                     
           WHEN OTHER                                                   
              PERFORM 2336-SEND-INVALID-KEY-MSG                         
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 2020-WRITE-DIRECT-ONE-WAY                       
      * THIS PARAGRAPH WILL SAVE TO THE QUEUE DATA ABOUT                
      * DIRECT ONE WAY FLIGHTS                                          
      *                                                                 
      * WE WILL SET NUMBER OF TRANSFERS TO 0 BECAUSE HERE WON'T BE      
      * ANY                                                             
      ******************************************************************
       2020-WRITE-DIRECT-ONE-WAY.                                       
           MOVE T05-FLIGHT-NUMBER-TEXT     TO QUEUE-FLIGHT-NUMBER       
           MOVE T05-FLIGHT-ID-TEXT         TO QUEUE-FLIGHT-ID           
           MOVE T05-DEPARTURE-AIRPORT-CODE TO                           
                                            QUEUE-DEPARTURE-AIRPORT-CODE
           MOVE T05-ARRIVAL-AIRPORT-CODE   TO QUEUE-ARRIVAL-AIRPORT-CODE
           MOVE T05-AIRLINE-CODE           TO QUEUE-AIRLINE-CODE        
           MOVE '00'                       TO QUEUE-TRANSFER-NUMBER     
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-ONEWAY-QUEUE)                                     
             FROM(WS-ONE-WAY-Q-STRUCTURE)                               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *               2021-DISPLAY-FIRST-15-FLIGHTS                     
      * THIS PARAGRAPH WILL DISPLAY FIRST-15-FLIGHTS                    
      *                                                                 
      * IT JUST SETS ITEM COUNTER ( ITEM COUNTER WILL STORE INFORMATION 
      * ABOUT NUMBER OF ROW IN THE QUEU) TO  1 NAD THANKS TO THAT       
      * WE WILL START READING THE QUEU FROM THE FIRST ROW     
      ******************************************************************
       2021-DISPLAY-FIRST-15-FLIGHTS.                                   
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           MOVE 1 TO WS-Z02172-LAST-REC-ID                              
           PERFORM 2050-DISPLAY-NEXT-15-ONE-WAY                         
           .                                                            
      ******************************************************************
      *                     2022-READ-DIRECT-ONEWAY-QUEUE               
      ******************************************************************
       2022-READ-DIRECT-ONEWAY-QUEUE.                                   
           EXEC CICS                                                    
           READQ TS                                                     
           QUEUE(CT-ONEWAY-QUEUE)                                       
           INTO(WS-ONE-WAY-Q-STRUCTURE)                                 
           ITEM(WS-WHAT-RECORD-TO-READ)                                 
           NOHANDLE                                                     
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(QIDERR)                                         
             CONTINUE                                                   
           WHEN DFHRESP(ITEMERR)                                        
             DISPLAY 'ITERM ERR LAST IS: '   WS-Z02172-LAST-REC-ID      
             SET SO-END-OF-QUEUE TO TRUE                                
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *               2023-ONE-WAY-QUEUE-TO-SCREEN                      
      * THIS PARAGRAPH CAN BE CALLED WHEN PROGRAM PREPARES              
      * DIRECT ONE WAY FLIGHT OR NOT DIRECT ONEWAY FLIGHT               
      *                                                                 
      * FIRST PART OF THIS PARAGRAPH IS IMPORANT FOR BOTH               
      * DIRECT OR NON DIRECT FLIGHTS                                    
      *                                                                 
      * SECOND PART (AFTER *************)
      * IS IMPORANT ONLY FOR NON DIRECT FLIGHTS                         
      ******************************************************************
       2023-ONE-WAY-QUEUE-TO-SCREEN.                                    
           MOVE QUEUE-FLIGHT-NUMBER          TO FLIGHT-NUMBERO(WS-ITER3)
           MOVE QUEUE-DEPARTURE-AIRPORT-CODE TO  AIR-ORGO(WS-ITER3)     
           MOVE ONE-WAY-Q-DATE-DEP          TO DEPARTURE-DATEO(WS-ITER3)
           MOVE ONE-WAY-Q-TIME-DEP          TO DEPARTURE-TIMEO(WS-ITER3)
           MOVE QUEUE-ARRIVAL-AIRPORT-CODE   TO  AIR-DESO(WS-ITER3)     
           MOVE ONE-WAY-Q-ARV-DATE           TO  ARRIVAL-DATEO(WS-ITER3)
           MOVE ONE-WAY-Q-ARV-TIME           TO  ARRIVAL-TIMEO(WS-ITER3)
           MOVE QUEUE-AIRLINE-CODE           TO  AIRLINEO(WS-ITER3)     
           MOVE QUEUE-TRANSFER-NUMBER   TO TRANSFER-NUMBERO(WS-ITER3)   
      * HERE PROGRAM WILL SAVE FLIGHT NUMBER FOR EACH POSITION ON THE   
      * SCREEN, THANKS TO THAT LOGIC PROGRAM WILL HAVE TO RETRIVE       
      * PALCES WHEN USER PLACED AN X, NOT ALL THE FIELDS ON THE MAP     
           MOVE QUEUE-FLIGHT-ID     TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3)  
           MOVE QUEUE-TRANSFER-NUMBER TO WS-TRANSFER-NUMBER             
           MOVE WS-TRANSFER-NUMBER TO  WS-WHAT-TRANSFER-NUMBER(WS-ITER3)
                                                                        
      ******************************************************************
                                                                        
                                                                        
      * THIS LOOP WONT DO ANYTHING IF THIS IS NOT A TRANSFER FLIGHT     
      * IF IT IS A TRANFSER FLIGHT IT WILL GO MANY TIMES AS MANY        
      * TRANSFER WE GOT                                                 
                                                                        
      * THIS DATA WILL BE LATER USED TO IDENTIFY ANY OF THE FLIGHTS     
      * DISPLAED ON THE SCREEN                                          
      * EVERY NON DIRECT FLIGHTS SET HAS A MAIN FLIGHT                  
      * AND A SUBFLIGHT                                                 
      * A MAIN ONE IS A FIRST IN ORDER ALL OTHER FLIGHTS THAT COMES     
      * AFTER WILL BE CALLED A SUBFLIGHT                                
      *                                                                 
      * THE LOOP BELOW WILL PUT A  FLIGHT ID TO AN ARRAY THAT           
      * WILL BE USED IN CASE THAT USER SEARCHES FOR NON DIRECT          
      * ONE WAY FLIGHT IN ORDER TO DETERMINE IF THE FLIGHT CHOSEN       
      * BY THE USER IS A VALID ONE                                      
           PERFORM 2024-CLEAR-SUB-FLIGHT-TABLE                          
           PERFORM VARYING WS-TRANSFER-ITER FROM 1 BY 1 UNTIL           
                     WS-TRANSFER-ITER >  WS-TRANSFER-NUMBER             
                                                                        
              MOVE ONE-WAY-Q-FLIGHT-ID(WS-TRANSFER-ITER) TO             
                       WS-WHAT-SUB-FLIGHT(WS-ITER3,WS-TRANSFER-ITER)    
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *              2024-CLEAR-SUB-FLIGHT-TABLE                        
      * PARAGRAPH WILL CLEAR ALL SUBLIGHTS FOR THIS FLIGHT              
      *                                                                 
      * WHAT THAT MEAN:                                                 
      * THE FIRST FLIGHT IN TRANSFER FLIGHT IS A MAIN ONE               
      * ALL FLIGHTS THAT COMES AFTER IT WILL BE CALLED A SUBLIGHT       
      *                                                                 
      * IF WE GOT FLIGHT THAT GOES ; MAD - BER - KRK - KTW - WAW ;      
      * THEN FLIGHT THAT GOES MAD- BER WILL THE MAIN ONE AND ALL        
      * FLIGHT THAT COMES AFTER : BER - KRK ,KRK -KTW ,KTW -WAW         
      * WILL BE A SUBLIGHT                                              
      ******************************************************************
       2024-CLEAR-SUB-FLIGHT-TABLE.                                     
           PERFORM VARYING WS-ITER7 FROM 1 BY 1 UNTIL WS-ITER7 > 5      
              MOVE CT-EMPTY-FIELD TO                                    
                                  WS-WHAT-SUB-FLIGHT(WS-ITER3,WS-ITER7) 
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2030-PROCESS-INPUT-ONE-WAY                     
      * IN ONE CHOICE FIELD ON THE MAP USER CAN:                        
      * 1. SPECIFY '1' -> IT IS INTERPRETED AS (I CHOOSE THIS FLIGHT)   
      * 2. SPECIFY '2' -> IT IS INTERPRETED AS (I WANT MORE DETAILS)    
      ******************************************************************
       2030-PROCESS-INPUT-ONE-WAY.                                      
           PERFORM 2031-RECEIVE-USER-INPUT                              
           PERFORM 2032-PROCESS-USER-CHOICE                             
           .                                                            
      ******************************************************************
      *                  2032-PROCESS-USER-CHOICE                       
      ******************************************************************
       2031-RECEIVE-USER-INPUT.                                         
           MOVE LOW-VALUES TO MP0217I                                   
           EXEC CICS                                                    
            RECEIVE MAP('MP0217') MAPSET('MP0217')                      
            INTO(MP0217I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2032-PROCESS-USER-CHOICE                       
      *                                                                 
      * PROGRAM WILL FIND A ROW WHERE USER PLACED HIS CHOICE            
      * IT WILL CHECK IF THIS CHOICE IS VALID AND IF PLACE WHERE        
      * USER PLACED HIS CHOICE IS VALID                                 
      *                                                                 
      * IF EVERYTHING WILL BE CORRECT THEN                              
      * PROGRAM WILL MAKE CALL TO PROGRAM THAT WILL DISPLAY FLIHGT      
      * DETAILS OR TO PROGRAM THAT WILL ALLOW USER TO RESERVATE A SEATS 
      *                                                                 
      ******************************************************************
       2032-PROCESS-USER-CHOICE.                                        
           INITIALIZE WS-CHOICE-COUNTER                                 
           INITIALIZE WS-USER-CHOICE-POSITION                           
                                                                        
           PERFORM 2333-GET-CHOICE-POSITION                             
      * PARAGRAPH WILL CHECK IF USER PLACED HIC CHOICE NEXT TO          
      * ONLY ONE FLIGHT (OR SET OF FLIGHTS)                             
                                                                        
           PERFORM 2334-CHECK-CHOICE-NUMBER                             
      * CHECK IF USER PLACED HIS CHOICE NEXT TO NON EMPTY LINE          
           PERFORM 2329-CHECK-IF-LINE-EMPTY                             
                                                                        
      * HERE PROGRAM WILL EVALUATE THROUGH USER CHOICE                  
      * USER COULD PLACE :                                              
      * '1' -> THEN PROGRAM Z02192 WILL BE CALLED                       
      * '2' -> THEN PROGRAM Z02182 WILL BE CALLED                       
      * OTHER -> THEN USER WILL GET PROPER MESSAGE AND HE WILL HAVE     
      * TO PROVIDE INPUT ONCE AGAIN                                     
           EVALUATE TRUE                                                
           WHEN  SO-FLIGHT-WAS-CHOSEN                                   
              DISPLAY 'SO FLIGHT-WAS CHOSEN '                           
              PERFORM 2210-PREPARE-FLIGHT-DATA                          
              SET SO-Z02192-M-ONE-WAY  TO TRUE                          
              PERFORM 2620-CALL-FOR-CHOSEN-FLIGHT                       
                                                                        
           WHEN  SO-DISPLAY-MORE-DETAILS                                
              DISPLAY 'SO FLIGHT-WAS DETAILS'                           
              SET SO-Z02182-M-ONE-WAY  TO TRUE                          
              PERFORM 2210-PREPARE-FLIGHT-DATA                          
              PERFORM 2610-CALL-FOR-MORE-DETAILS                        
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'INVALID CHOICE          '                           
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              MOVE '1 FOR CHOOSE THE FLIGHT '                           
                           TO WS-Z02141-I-ERROR-MESSAGE(2)              
              MOVE '2 FOR DISPLAYING MORE DETAILS'                      
                           TO WS-Z02141-I-ERROR-MESSAGE(3)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2040-COMPUTE-SIZE-ERROR.                     
      ******************************************************************
       2040-COMPUTE-SIZE-ERROR.                                         
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'COMPUTE SIZE ERROR ' TO WS-Z02141-I-ERROR-MESSAGE(1)   
           SET  SO-Z02141-M-WITH  TO TRUE                               
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *               2046-WRITE-RETURN-DIRECT-FLG                      
      * PARAGRAPH MOVES PROGRAM DATA TO QUEUE VARIABLES                 
      * AND CICS'S WRITEQ STATEMENNT IS EXECUTED                        
      *                                                                 
      ******************************************************************
       2046-WRITE-RETURN-DIRECT-FLG.                                    
           PERFORM 2308-MOVE-DATA-TO-RETURN-Q                           
           EXEC CICS                                                    
            WRITEQ TS                                                   
            QUEUE(CT-RETURN-DIRECT-QUEUE)                               
            FROM(WS-RETURN-DIRECT-QUEUE-ST)                             
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *             2047-DISPLAY-RETURN-FLIGHTS                         
      * PARAGRAPH MOVES 1 TO WS-Z02172-LAST-Q5-REC-ID VARIABLES         
      * THANKS TO THAT THE NEXT CALLED PARAGRAPH (2083) WILL            
      * START DO DISPLAYING ROWS FROM THE QUEUE FROM THE FIRST ROW      
      ******************************************************************
       2047-DISPLAY-RETURN-FLIGHTS.                                     
           MOVE 1 TO WS-Z02172-LAST-Q5-REC-ID                           
           PERFORM 2083-DISPLAY-NEXT-4-2WAY-DIR                         
           .                                                            
      ******************************************************************
      *                    2050-DISPLAY-NEXT-15-ONE-WAY                 
      * PARAGRAPH WILL INITIALIZE MAP AND                               
      ******************************************************************
       2050-DISPLAY-NEXT-15-ONE-WAY.             
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2102-INITIALIZE-MAP-1                                
           MOVE WS-Z02172-LAST-REC-ID TO WS-WHAT-RECORD-TO-READ         
                                                                        
           PERFORM 2022-READ-DIRECT-ONEWAY-QUEUE                        
           PERFORM 2103-INITIALIZE-FLIGHT-ARRAY                         
           PERFORM 2052-CHECK-FOR-QIDERR                                
                                                                        
           PERFORM 2053-MOVE-QUEUE-TO-SCRREN                            
           PERFORM 2100-SEND-THE-MAP-1                                  
           .                                                            
      ******************************************************************
      *                  2051-DISPLAY-PREV-15-ONE-WAY                   
      ******************************************************************
       2051-DISPLAY-PREV-15-ONE-WAY.                                    
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           PERFORM 2102-INITIALIZE-MAP-1                                
                                                                        
            IF WS-Z02172-FIRST-REC-ID - 15  >= 1 THEN                   
              SUBTRACT 15 FROM WS-Z02172-FIRST-REC-ID                   
            ELSE                                                        
              MOVE 1 TO WS-Z02172-FIRST-REC-ID                          
            END-IF                                                      
           MOVE WS-Z02172-FIRST-REC-ID TO WS-WHAT-RECORD-TO-READ        
           PERFORM 2103-INITIALIZE-FLIGHT-ARRAY                         
           PERFORM 2022-READ-DIRECT-ONEWAY-QUEUE                        
           PERFORM 2052-CHECK-FOR-QIDERR                                
      * IF THERE ARE RECORD IN THE QUEUE AND QUEUE EXIST THEN           
      * WE WILL START READING THIS QUEU IN THE LOOP                     
                                                                        
           PERFORM 2053-MOVE-QUEUE-TO-SCRREN                            
           PERFORM 2100-SEND-THE-MAP-1                                  
           .                                                            
      ******************************************************************
      *                   2052-CHECK-FOR-QIDERR                         
      ******************************************************************
       2052-CHECK-FOR-QIDERR.                                           
           IF EIBRESP = DFHRESP(QIDERR) THEN                            
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'THERE IS NO FLIGHTS MEETING THOOSE CRITERIA'         
             TO WS-Z02141-I-ERROR-MESSAGE(1)                            
             SET SO-Z02141-M-WITH TO TRUE                               
             SET SO-GO-BACK-TO-Z02152 TO TRUE                           
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2053-MOVE-QUEUE-TO-SCRREN                      
      * PARAGRAPH WILL MOVE DATA FOR THE QUEU TO THE SCREEN VARIABLES   
      * LOOP WILL GO 15 TIMES OR  END OF QUEU WILL BE REACHED           
      *                                                                 
      * WHEN LOOP ITERATOR IS EQUAL TO 1, WE WILL SAVE THAT POSITION    
      * (WE WILL USE THAT INFO IN CASE OF BROWSING RESULT SET           
      * BACKWARDS)                                                      
      ******************************************************************
       2053-MOVE-QUEUE-TO-SCRREN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
                                                OR   SO-END-OF-QUEUE    
             IF WS-ITER3 = 1 THEN                                       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-QUEUE-1-FIRST-ID       
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-REC-ID    
             END-IF                                                     
                                                                        
               PERFORM 2023-ONE-WAY-QUEUE-TO-SCREEN                     
               MOVE WS-WHAT-RECORD-TO-READ TO WS-QUEUE-1-LAST-ID        
               MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-REC-ID     
               ADD 1 TO WS-WHAT-RECORD-TO-READ                          
               PERFORM 2022-READ-DIRECT-ONEWAY-QUEUE                    
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2156-DISPLAY-MAP-2           
      ******************************************************************
       2156-DISPLAY-MAP-2.                                              
           EXEC CICS                                                    
            SEND MAP('MM0217') MAPSET('MM0217')                         
            FROM(MM0217O)                                               
            ERASE                                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2058-INITIALIZE-RETURN-MAP                     
      * PARAGRAPH WILL PREPARE (M102170) MAP TO BEING DISPLAYED         
      * ALL ATTRIBITU FIELDS WILL BE RESET, THAT WILL ALLOW US          
      * TO ALWAYS DISPLAY CORRECT MAP                                   
      ******************************************************************
       2058-INITIALIZE-RETURN-MAP.                                      
           MOVE LOW-VALUES TO M10217O                                   
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 4      
             MOVE LOW-VALUES TO TRANSA(WS-ITER2)                        
             MOVE LOW-VALUES TO LFLIGHT-NUMBER-A(WS-ITER5)              
             MOVE LOW-VALUES TO RFLIGHT-NUMBER-A(WS-ITER5)              
             MOVE LOW-VALUES TO LDEPARTURE-DATE-A(WS-ITER5)             
             MOVE LOW-VALUES TO RDEPARTURE-DATE-A(WS-ITER5)             
             MOVE LOW-VALUES TO LDEPARTURE-TIME-A(WS-ITER5)             
             MOVE LOW-VALUES TO RDEPARTURE-TIME-A(WS-ITER5)             
             MOVE LOW-VALUES TO LARRIVAL-DATE-A(WS-ITER5)               
             MOVE LOW-VALUES TO RARRIVAL-DATE-A(WS-ITER5)               
             MOVE LOW-VALUES TO LARRIVAL-TIME-A(WS-ITER5)               
             MOVE LOW-VALUES TO RARRIVAL-TIME-A(WS-ITER5)               
             MOVE LOW-VALUES TO LORIGIN-AIRPORT-A(WS-ITER5)             
             MOVE LOW-VALUES TO RORIGIN-AIRPORT-A(WS-ITER5)             
             MOVE LOW-VALUES TO LDEST-AIRPORT-A(WS-ITER5)               
             MOVE LOW-VALUES TO RDEST-AIRPORT-A(WS-ITER5)               
             MOVE LOW-VALUES TO BCHOICE-A(WS-ITER5)                     
           END-PERFORM                                                  
           .                  
      ******************************************************************
      *                     2059-READ-DIRECT-2WAY                       
      ******************************************************************
       2059-READ-DIRECT-2WAY.                                           
           EXEC CICS                                                    
            READQ TS                                                    
            QUEUE(CT-RETURN-DIRECT-QUEUE)                               
            INTO(WS-RETURN-DIRECT-QUEUE-ST)                             
            ITEM(WS-WHAT-RECORD-TO-READ)                                
            NOHANDLE                                                    
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(QIDERR)                                         
              CONTINUE                                                  
           WHEN DFHRESP(ITEMERR)                                        
              SET SO-END-OF-QUEUE TO TRUE                               
           WHEN OTHER                                                   
              PERFORM 2200-CHECK-EIBRESP                                
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2100-SEND-THE-MAP-1                         
      ******************************************************************
       2100-SEND-THE-MAP-1.                                             
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 15     
              DISPLAY FLIGHT-NUMBERO(WS-ITER5)                          
           END-PERFORM                                                  
           EXEC CICS                                                    
            SEND MAP('MP0217') MAPSET('MP0217')                         
            FROM(MP0217O)                                               
            ERASE                                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 2075-PROCESS-INPUT-2-WAY       
      * PARAGRAPH WILL HAVE TO GET INFO ABOUT WHERE USER PLACED HIS     
      * CHOICE,                                                         
      * THEN IT NEEDS TO CHECK IF THIS CHOICE IS VALID AND IF USER      
      * PLACED THIS CHOICE IN VALID PLACE                               
      * IF ALL OF THAT IS VALID WE WILL PREPARE DATA TO BE USED         
      * BY Z02182 PROGRAM AND BY Z02192 PROGRAM                         
      *                                                                 
      ******************************************************************
       2075-PROCESS-INPUT-2-WAY.                                        
           DISPLAY '2075-PROCESS-INPUT-2-WAY PERFORMED '                
                                                                        
           PERFORM 2076-RECEIVE-MAP-3                                   
      * PROGRAM WILL CHECK IF USER PLACED AN 'X' NEXT TO VALID          
      * DATA                                                            
                                                                        
           PERFORM 2079-CHECK-WHAT-USER-CHOOSE                          
                                                                        
           EVALUATE TRUE                                                
           WHEN SO-CHOOSE-THIS-FLIGHTS                                  
             PERFORM 2305-PREPARE-CHOOSE-2WAY                           
           WHEN SO-DISPLAY-DETAILS                                      
             PERFORM 2306-PREPARE-DETAILS-2WAY                          
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      2076-RECEIVE-MAP-2                         
      ******************************************************************
       2076-RECEIVE-MAP-3.                                              
           MOVE LOW-VALUES TO M10217I                                   
           EXEC CICS                                                    
            RECEIVE MAP('M10217') MAPSET('M10217')                      
            INTO(M10217I)                                               
            NOHANDLE                                                    
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(MAPFAIL)  THEN                          
             CONTINUE        
          ELSE                                                         
                                                                       
            PERFORM 2200-CHECK-EIBRESP                                 
          END-IF                                                       
          .                                                            
     ******************************************************************
     *              2079-CHECK-WHAT-USER-CHOOSE                        
     * THIS PARAGRAPH WILL GET POSITION OF USER CHOICE (WHERE USER     
     * PLACE A SYMBOL)                                                 
     *                                                                 
     * IF USER PLACED THERE SOMETHING OTHER THAN '1' OR '2' THEN       
     * PROGRAM WILL DISPLAY PROPER ERROR                               
     *                                                                 
     * IF USER HAVE CHECKED MORE THAN 1 FLIGHT (OR 1 TRANSFER FLIGHT)  
     * THEN PROGRAM WILL DISPLAY PROPER ERROR                          
     *                                                                 
     ******************************************************************
      2079-CHECK-WHAT-USER-CHOOSE.                                     
                                                                       
     * PARAGRAPH WILL GET POSITION WHERE USER PLACED HIS CHOICE        
     * WE WILL ALSO GET NUMBER OF CHOICES USER HAS MADE                
     *                                                                 
     * LATER WE WILL VALIDATE IF USER CHOOSE ONLY 1 OPTTION (VALID)    
     * OR MORE (INVALID )                                              
                                                                       
          PERFORM 2301-GET-CHOICE-POSITION-2WAY                        
          PERFORM 2302-CHECK-CHOICE-NUMBER                             
                                                                       
     * IF WE ARE SEARCHING FOR NON DIRECT 2WAY FLIGHT                  
          IF SO-SEARCH-FOR-N-DIRECT-RETURN THEN                        
            PERFORM 2303-VALIDATE-N-DIRECT-2WAY                        
          ELSE                                                         
     * IF WE ARE SEARCHING FOR DIRECT 2 WAY FLIGHT                     
            PERFORM 2304-VALIDATE-DIRECT-2WAY                          
          END-IF 
      * IF USER PLACED '1' NEXT TO FLIGHT IT MEANS HE WANTS TO          
      * CHOOSE IT                                                       
      *                                                                 
      * IF USER PLACED '2' NEXT TO FLIGHT IT MEANS HE WANTS TO GET      
      * MORE DETAILS ABOUT THIS FLIGHT                                  
           EVALUATE WS-USER-CHOICE                                      
           WHEN '1' SET SO-CHOOSE-THIS-FLIGHTS TO TRUE                  
           WHEN '2' SET SO-DISPLAY-DETAILS     TO TRUE                  
           WHEN OTHER                                                   
                 PERFORM 2400-INITIALIZE-ERROR-MESSAGE                  
                 MOVE 'PLACE 1 OR 2 NEXT TO VALID FLIGHT ' TO           
                                 WS-Z02141-I-ERROR-MESSAGE(1)           
                 SET SO-Z02141-M-WITH TO TRUE                           
                 PERFORM 2300-CALL-ERROR-ROUTINE                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2080-MOVE-2WAY-Q-TO-SCREEN                    
      * THIS PARAGRAPH WILL MOVE VARIABLES FROM THE QUEUE               
      * TO THE SCREEN VARIABLES                                         
      ******************************************************************
       2080-MOVE-2WAY-Q-TO-SCREEN.                                      
           MOVE 2WAY-DIR-FLIGHT-TO-NUMBER              TO               
               LFLIGHT-NUMBER-O(WS-ITER3)                               
           MOVE 2WAY-DIR-FLIGHT-TO-DEP-DATE            TO               
              LDEPARTURE-DATE-O(WS-ITER3)                               
           MOVE 2WAY-DIR-FLIGHT-TO-DEP-TIME            TO               
              LDEPARTURE-TIME-O(WS-ITER3)                               
           MOVE 2WAY-DIR-FLIGHT-TO-ARV-DATE            TO               
              LARRIVAL-DATE-O(WS-ITER3)                                 
           MOVE 2WAY-DIR-FLIGHT-TO-ARV-TIME            TO               
              LARRIVAL-TIME-O(WS-ITER3)                                 
           MOVE 2WAY-DIR-FLIGHT-FROM-NUMBER            TO               
               RFLIGHT-NUMBER-O(WS-ITER3)                               
           MOVE 2WAY-DIR-FLIGHT-FROM-DEP-DATE          TO 
             RDEPARTURE-DATE-O(WS-ITER3)                               
          MOVE 2WAY-DIR-FLIGHT-FROM-DEP-TIME          TO               
             RDEPARTURE-TIME-O(WS-ITER3)                               
          MOVE 2WAY-DIR-FLIGHT-FROM-ARV-DATE          TO               
             RARRIVAL-DATE-O(WS-ITER3)                                 
          MOVE 2WAY-DIR-FLIGHT-FROM-ARV-TIME          TO               
             RARRIVAL-TIME-O(WS-ITER3)                                 
                                                                       
          MOVE 2WAY-DIR-DEPARTURE-AIRPORT             TO               
             LORIGIN-AIRPORT-O(WS-ITER3)                               
          MOVE 2WAY-DIR-DEPARTURE-AIRPORT             TO               
             RDEST-AIRPORT-O(WS-ITER3)                                 
          MOVE 2WAY-DIR-ARRIVAL-AIRPORT               TO               
             LDEST-AIRPORT-O(WS-ITER3)                                 
          MOVE 2WAY-DIR-ARRIVAL-AIRPORT               TO               
             RORIGIN-AIRPORT-O(WS-ITER3)                               
          MOVE 2WAY-DIR-FLIGHT-TO-NUMBER TO                            
              WS-ARRAY-FLIGHT-TO(WS-ITER3)                             
          MOVE 2WAY-DIR-FLIGHT-FROM-NUMBER TO                          
              WS-ARRAY-FLIGHT-FROM(WS-ITER3)                           
                                                                       
          MOVE 2WAY-DIR-FLIGHT-TO-NUMBER                               
          TO WS-FLIGHT-ARRAY-TO(WS-ITER3)                              
          MOVE 2WAY-DIR-FLIGHT-FROM-NUMBER TO                          
                       WS-FLIGHT-ARRAY-FROM(WS-ITER3)                  
          MOVE 2WAY-DIR-NUMBER-OF-TRANSFERS TO TRANSO(WS-ITER3)        
          MOVE 2WAY-DIR-NUMBER-OF-TRANSFERS TO TRAN2O(WS-ITER3)        
                                                                       
          .                                                            
     ******************************************************************
     *                  2081-SEND-2WAY-FLIGHT-MAP                      
     * PARAGRAPH SEND MAPS WHEN PROGRAM SEARCHES FOR RETURN FLIGHTS    
     ******************************************************************
      2081-SEND-2WAY-FLIGHT-MAP.                                       
          EXEC CICS                                                    
           SEND MAP('M10217') MAPSET('M10217')    
            FROM(M10217O)                                               
            ERASE                                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2082-MOVE-QUEUE-6-TO-SCREEN                    
      ******************************************************************
       2082-MOVE-QUEUE-6-TO-SCREEN.                                
           MOVE QUEUE-6-FIRST-TO-NUMBER                TO          
               LFLIGHT-NUMBER-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-TO-DEP-DATE             TO          
              LDEPARTURE-DATE-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-TO-DEP-TIME             TO          
              LDEPARTURE-TIME-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-TO-ARV-DATE             TO          
              LARRIVAL-DATE-O(WS-ITER3)                            
           MOVE QUEUE-6-FLIGHT-TO-ARV-TIME             TO          
              LARRIVAL-TIME-O(WS-ITER3)                            
           MOVE QUEUE-6-FIRST-FROM-NUMBER              TO          
               RFLIGHT-NUMBER-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-FROM-DEP-DATE           TO          
              RDEPARTURE-DATE-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-FROM-DEP-TIME           TO          
              RDEPARTURE-TIME-O(WS-ITER3)                          
           MOVE QUEUE-6-FLIGHT-FROM-ARV-DATE           TO          
              RARRIVAL-DATE-O(WS-ITER3)                            
           MOVE QUEUE-6-FLIGHT-FROM-ARV-TIME           TO          
              RARRIVAL-TIME-O(WS-ITER3)                            
                                                                   
           MOVE QUEUE-6-DEPARTURE-AIRPORT              TO          
              LORIGIN-AIRPORT-O(WS-ITER3)                          
           MOVE QUEUE-6-ARRIVAL-AIRPORT                TO          
              LDEST-AIRPORT-O(WS-ITER3)                            
                                                                   
           MOVE QUEUE-6-ARRIVAL-AIRPORT              TO            
              RORIGIN-AIRPORT-O(WS-ITER3)                          
           MOVE QUEUE-6-DEPARTURE-AIRPORT              TO          
              RDEST-AIRPORT-O(WS-ITER3)                            
                                                                   
           MOVE QUEUE-6-FIRST-TO-NUMBER  TO                        
               WS-ARRAY-FLIGHT-TO(WS-ITER3)                        
           MOVE QUEUE-6-FIRST-FROM-NUMBER  TO                      
               WS-ARRAY-FLIGHT-FROM(WS-ITER3)       
                                                                        
           MOVE QUEUE-6-FIRST-TO-NUMBER  TO WS-FLIGHT-ARRAY-TO(WS-ITER3)
           MOVE QUEUE-6-FIRST-FROM-NUMBER  TO                           
                        WS-FLIGHT-ARRAY-FROM(WS-ITER3)                  
           MOVE QUEUE-6-NUMBER-OF-TO-TRANSFERS TO TRANSO(WS-ITER3)      
           MOVE QUEUE-6-NUMBER-OF-FR-TRANSFERS TO TRAN2O(WS-ITER3)      
                                                                        
           .                                                            
      ******************************************************************
      *                2101-DIRECT-ONEWAY-WITH-ALL                      
      * ONE OF THE SIMPLIEST SCENARIOS                                  
      * PROGRAM WILL USE CURSOR TO FIND ALL DIRECT ONE-WAY FLIGHTS      
      * WITH GIVEN DEPARTUE DATE AND GIVEN ORIGIN AND DEPARTUER AIRPORTS
      *                                                                 
      *                                                                 
      * THIS PARAGRAPH WILL MOVE COMMAREA DATA TO CUSOR CONDITION       
      * VARIABLES                                                       
      * AND THEN CURSOR WILL BE OPENED AND ALL FLIGHTS THAT MEETS       
      * CRITERIA WILL BE FETCHED                                        
      *                                                                 
      * AFTER THIS PROCESS IS COMPLETED FIRST 15 FLIGHTS WILL BE        
      * DISPLYED AND USER WILL BE ABLE TO MOVE RESULT SET BY USING      
      * F7 AND F8 KEYS                                                  
      *                                                                 
      ******************************************************************
       2101-DIRECT-ONEWAY-WITH-ALL.                                     
                                                                        
           MOVE WS-Z02172-DEPARTURE-DATE    TO   WS-DEPARTURE-DATE      
           MOVE WS-Z02172-RETURN-DATE       TO   WS-ARRIVAL-DATE        
           MOVE WS-Z02172-DEST-AIRPORT-IATA TO  T05-ARRIVAL-AIRPORT-CODE
           MOVE WS-Z02172-ORIGIN-AIRPORT-IATA TO                        
                                              T05-DEPARTURE-AIRPORT-CODE
                                                                        
           PERFORM 7001-OPEN-DIR-ONEWAY-CURSOR                          
      * PARAGRAPH WILL FETCH DIRECT ONEWAY FLIGHTS TO QUEUE             
           PERFORM 7002-FETCH-FLIGHTS-TO-QUEUE    
           PERFORM 7003-CLOSE-DIR-ONEWAY-CURSOR                         
           PERFORM 2021-DISPLAY-FIRST-15-FLIGHTS                        
           .                                                            
      ******************************************************************
      *                  2082-MOVE-2WAY-TO-SCREEN                       
      * THIS PARAGRAPH WILL READ QUEUE 4 TIMES OR TO THE END OF THE     
      * RECORDS                                                         
      *                                                                 
      * QUEUE REOCRD WILL BE DISPLAYED ON THE SCREEN                    
      ******************************************************************
       2082-MOVE-2WAY-TO-SCREEN.                                        
           MOVE WS-Z02172-LAST-Q5-REC-ID TO WS-WHAT-RECORD-TO-READ      
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 >        
                    CT-MAXIMAL-AMOUNT-OF-2WAY OR SO-END-OF-QUEUE        
                                                                        
              IF WS-ITER3 = 1 THEN                                      
                MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-Q5-REC-ID
              END-IF                                                    
                                                                        
              PERFORM 2080-MOVE-2WAY-Q-TO-SCREEN                        
              MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-Q5-REC-ID   
              ADD 1 TO WS-WHAT-RECORD-TO-READ                           
              PERFORM 2059-READ-DIRECT-2WAY                             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2102-INITIALIZE-MAP-1                       
      ******************************************************************
       2102-INITIALIZE-MAP-1.                                           
           MOVE LOW-VALUES TO MP0217O                                   
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
             MOVE LOW-VALUES TO CHOICEA(WS-ITER3)                       
             MOVE LOW-VALUES TO FLIGHT-NUMBERA(WS-ITER3)                
             MOVE LOW-VALUES TO AIR-ORGA(WS-ITER3)                      
             MOVE LOW-VALUES TO DEPARTURE-DATEA(WS-ITER3)               
             MOVE LOW-VALUES TO DEPARTURE-TIMEA(WS-ITER3) 
             MOVE LOW-VALUES TO AIR-DESA(WS-ITER3)                      
             MOVE LOW-VALUES TO ARRIVAL-DATEA(WS-ITER3)                 
             MOVE LOW-VALUES TO ARRIVAL-TIMEA(WS-ITER3)                 
             MOVE LOW-VALUES TO AIRLINEA(WS-ITER3)                      
             MOVE LOW-VALUES TO TRANSFER-NUMBERA(WS-ITER3)              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2083-DISPLAY-NEXT-4-2WAY-DIR                  
      ******************************************************************
       2083-DISPLAY-NEXT-4-2WAY-DIR.                                    
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           MOVE WS-Z02172-LAST-Q5-REC-ID TO WS-WHAT-RECORD-TO-READ      
           PERFORM 2058-INITIALIZE-RETURN-MAP                           
           PERFORM 2085-INITIALIZE-2WAY-FL-ARRAY                        
           PERFORM 2059-READ-DIRECT-2WAY                                
           PERFORM 2052-CHECK-FOR-QIDERR                                
                                                                        
      * AFTER CHECKING THAT QUEUE EXISTS PROGRAM WILL PERFORM PARAGRAPH 
      * THAT WILL READ QUEUE IN LOOP AND WILL PLACE RECORDS ON THE      
      * SCREEN                                                          
           PERFORM 2082-MOVE-2WAY-TO-SCREEN                             
           PERFORM 2081-SEND-2WAY-FLIGHT-MAP                            
           .                                                            
      ******************************************************************
      *                     2084-DISPLAY-PREV-4-2WAY-DIR                
      * PARAGRAPH WILL CALCULATE FROM THAT ROW TO START READING         
      * AND AFTER CALCULATION IS MADE WE WILL CALL TO                   
      *  2083-DISPLAY-NEXT-4-2WAY-DIR PARAGRAPH THAT WILL               
      * START READING FROM THAT RECORD                                  
      *                                                                 
      * IF WE CAN SUBTRACT 3 FROM RECORD COUNTER THEN WE WILL DO THAT   
      * IN OTHER CASE WE WILL START READING FROM THE TOP ( FROM         
      * THE FIRST RECORD)                                               
      *                                                                 
      ******************************************************************
       2084-DISPLAY-PREV-4-2WAY-DIR.                                    
           IF WS-Z02172-FIRST-Q5-REC-ID - CT-MAXIMAL-AMOUNT-OF-2WAY  - 1
                               >= 1 THEN                                
             COMPUTE WS-TEMP-NUMERIC = CT-MAXIMAL-AMOUNT-OF-2WAY - 1    
             SUBTRACT WS-TEMP-NUMERIC  FROM WS-Z02172-FIRST-Q5-REC-ID   
                                                                        
           ELSE                                                         
             MOVE 1 TO WS-Z02172-FIRST-Q5-REC-ID                        
           END-IF                                                       
           MOVE WS-Z02172-FIRST-Q5-REC-ID TO WS-Z02172-LAST-Q5-REC-ID   
           PERFORM 2083-DISPLAY-NEXT-4-2WAY-DIR                         
           .                                                            
      ******************************************************************
      *                   2085-INITIALIZE-2WAY-FL-ARRAY                 
      * THIS PARAGRAPH WILL INITIALIZE 2 ARRAYS THAT WILL BE USED IN    
      * ORDER TO VALIDATE THAT USER CHECKED VALID FLIGHT ON THE SCREEN  
      ******************************************************************
       2085-INITIALIZE-2WAY-FL-ARRAY.                                   
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 4      
              MOVE CT-EMPTY-FIELD TO WS-ARRAY-FLIGHT-TO(WS-ITER5)       
              MOVE CT-EMPTY-FIELD TO WS-ARRAY-FLIGHT-FROM(WS-ITER5)     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2089-VALIDATE-THE-FLIGHT                    
      * THIS PARAGRAPH WILL CALUCALATE DISTANCE BETWEEN                 
      * TWO AIRPORTS AND IF SUM OF THOSE DISTANCES FOR ALL              
      * TRANSFER FLIGHTS IS LESS THAN MAXIMAL DISTANCE POSSIBLE THEN    
      * THIS PARAGRAPH WILL ALLOW PROGRAM TO SAVE THIS FLIGHT           
      *                                                                 
      * IN OTHER CASE PROGRAM WILL ALLOW PROGRAM TO SEARCH DEEPER       
      * (FOR FLIGTH WITH BIGGER NUMBER OF TRANSFERS                     
      ******************************************************************
       2089-VALIDATE-THE-FLIGHT.                                        
              SET SO-DONT-SEARCH-NEXT-TRANSFER TO TRUE                  
      * IF NUMBER OF SEAT IS VALID       
              IF SO-CONTINUE-WITH-ROW THEN                              
                MOVE T05-ARRIVAL-AIRPORT-CODE TO WS-DESTINATION-AIRPORT 
                MOVE T05-DEPARTURE-AIRPORT-CODE TO WS-ORIGIN-AIRPORT    
                PERFORM 2208-CALCULATE-DISTANCE                         
                MOVE WS-CALCULATED-DISTANCE TO                          
                WS-DISTANCE(WS-WHICH-FLIGHT-IN-TRANSFERS)               
                PERFORM 2202-CHECK-IF-DEST-FINAL                        
                  IF SO-THIS-IS-FINAL-FLIGHT THEN                       
                                                                        
                     PERFORM 7230-PREPARE-ARV-TIME                      
      * PARAGRAPH WILL CALCULATE WHAT IS CURRENT SUM OF ALL             
      * DISTANCES FOR ALL TRANSFER FLIGHTS                              
      * IF THIS SUM IS GREATER THAN MAXIMAL DISTANCE THEN               
      * WE WILL SKIP THAT FLIGHT ( WE WILL NOT USE IT )                 
                      IF SO-SEARCH-FOR-N-DIRECT-ONEWAY THEN             
                       PERFORM 2099-CALCULATE-SUM-OF-DIST               
                         IF WS-SUM-OF-DISTANCES >                       
                            WS-MAXIMAL-DISTANCE THEN                    
                            CONTINUE                                    
                         ELSE                                           
                           PERFORM 2207-INITIALIZE-SUBFLIGHTS           
                           PERFORM 2203-WRITE-ONE-WAY-QUEUE             
                         END-IF                                         
                      ELSE                                              
      * IF WE ARE SEARCHING FOR RETURN FLIGHTS WE NEED TO CHECK         
      * IF THEIR ARRIVAL DATE IS AS USER SPECIFIED                      
                        IF SO-SEARCH-FOR-N-DIRECT-ONEWAY THEN           
                         PERFORM 2099-CALCULATE-SUM-OF-DIST             
                           IF WS-SUM-OF-DISTANCES >                     
                              WS-MAXIMAL-DISTANCE OR                    
                              ONE-WAY-Q-ARV-DATE NOT = WS-ARRIVAL-DATE  
                              THEN                                      
                             DISPLAY 'RETURN FLIGHT DROP '              
                             DISPLAY 'DUE TO INVALID DISTANCE OR '      
                             DISPLAY 'INVALID DATE '                    
                           ELSE     
                             PERFORM 2207-INITIALIZE-SUBFLIGHTS         
                             PERFORM 2203-WRITE-ONE-WAY-QUEUE           
                           END-IF                                       
                      END-IF                                            
                  ELSE                                                  
                      SET SO-SEARCH-NEXT-TRANSFER TO TRUE               
                  END-IF                                                
              END-IF                                                    
           .                                                            
      ******************************************************************
      *                     2099-CALCULATE-SUM-OF-DIST                  
      * PARAGRAPH WILL CALCULATE ALL DISTANCES THAT ALL TRANSFER        
      * FLIGHTS ARE GONNA TO MAKE                                       
      *                                                                 
      *  WE GOT WS-WHICH-FLIGHT-IN-TRANSFERS VARIABLE                   
      * WHICH WILL TELL US WHICH TO WHICH FLIGHT WE ARE ADDING THOSE    
      * DISTANCES                                                       
      *                                                                 
      * FOR EXMAPLE WHEN THIS PARAGRAPH WAS CALLED WHILE PROGRAM WAS    
      * SEARCHING FOR 2 TRANSFER FLIGHT ( WHAT MEANS WE WERE CALLED     
      * WHILE SEARCHING FOR THIRD FLIGHT )                              
      * THEN WE WILL ADD HERE DISTANCE( FIRST AIRPORT,                  
      * SECOND AIRPORT)  + DISTANCE( SECOND AIRPORT, THIRD AIRPORT ) +  
      * DISTANCE(TRHID AIRPORT, FORTH AIRPORT)                          
      *                                                                 
      * LATER PROGRAM WILL CHECK IF THIS SUM IS LESS THAN OR EQUAL      
      * TO MAXIMAL DISTANCE                                             
      *                                                                 
      * MAXIMAL DISTANCE WILL BE 250% OF DISTANCE (FIRST AIRPORT, FORTH 
      * AIRPORT)                                                        
      *                                                                 
      ******************************************************************
       2099-CALCULATE-SUM-OF-DIST.                                      
           MOVE 0 TO WS-SUM-OF-DISTANCES                                
           PERFORM VARYING WS-ITER15 FROM 1 BY 1 UNTIL WS-ITER15 >      
                                     WS-WHICH-FLIGHT-IN-TRANSFERS    
              ADD WS-DISTANCE(WS-WHICH-FLIGHT-IN-TRANSFERS) TO          
                   WS-SUM-OF-DISTANCES                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2103-INITIALIZE-FLIGHT-ARRAY.                   
      * THIS ARRAY WILL BE USED LATER BY PROGRAM TO DETERMINE WHAT      
      * IS THE FLIGHT NUMBER USER WANTS TO USE                          
      ******************************************************************
       2103-INITIALIZE-FLIGHT-ARRAY.                                    
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 15     
              MOVE CT-EMPTY-FIELD   TO WS-WHAT-FLIGHT-NUMBER(WS-ITER3)  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2105-NOT-DIRECT-ONEWAY                       
      * THIS PARAGRAPHS WILL FIND ALL (UP TO 5 TRANSFER) FLIGHTS        
      *                                                                 
      * AT THE BEGINING PARAGRAPH WILL CALCULATE MAXIMAL DISTANCE       
      * THAT TRANSFER FLIGHT CAN HAVE,                                  
      * MAXIMAL DISTANCE = 250 % OF THE SHORTEST POSSIBLE ROUTE BETWWEN 
      * 2 POINTS ON THE GLOBE ( POINTS ARE AIRPORTS OFCOURSE )          
      *                                                                 
      * THE PROGRAM WILL OPEN THE FIRST CURSOR AND FETCH DATA FROM IT,  
      *  CHECKING IF THE DESTINATION AIRPORT MATCHES THE ONE THE USER   
      * WANTS                                                           
      * . IF IT DOES, THE FLIGHT WILL BE ADDED TO THE QUEUE. IF NOT,    
      * THE PROGRAM WILL OPEN THE SECOND CURSOR, AND THE LOGIC WILL     
      * REPEAT.                                                         
      *  THIS PROCESS CONTINUES UP TO THE SIXTH CURSOR, WHICH ENDS THE  
      * F LIGHT   SEARCH PROCESS.                                       
      *                                                                 
      *                                                                 
      *  THIS PARAGRAPH, AND PARAGRAPHS CALLED BY THIS PARAGRAPH        
      * USES SPECIAL NAMES OF CURSORS AND PARAGRAPHS THAT USES THOSE    
      * CURSORS    
      *                                                                 
      *  WHEN YOU SEE "4-TRANSFER"  SOMEWHERE IN THE NAME OF PARAGRAPH  
      * OR CURSOR THIS MEANS THAT THIS CURSOR SEARCHES FOR FLIGHTS      
      * THAT HAS 4 TRANSFERS SO THERE ARE SUM OF 5 FLIGHTS IN THIS      
      * WHEN YOU SEE 0-TRANSFER THAT MEANS THAT THIS PARAGRAPH SEARCHES 
      * FOR DIRECT FLIGHTS                                              
      ******************************************************************
       2105-NOT-DIRECT-ONEWAY.                                          
           DISPLAY 'IN 2105  NOT DIRECT NEW '                           
           MOVE WS-Z02172-DEST-AIRPORT-IATA   TO WS-DESTINATION-AIRPORT 
           MOVE WS-Z02172-ORIGIN-AIRPORT-IATA TO WS-ORIGIN-AIRPORT      
           PERFORM 2208-CALCULATE-DISTANCE                              
           COMPUTE WS-MAXIMAL-DISTANCE = 2.5 * WS-CALCULATED-DISTANCE   
                                                                        
           PERFORM 7201-OPEN-C-0-TRANSFER                               
           PERFORM 7202-FETCH-C-0-TRANSFER                              
           PERFORM 7203-CLOSE-C-0-TRANSFER                              
                                                                        
           DISPLAY 'ALL SHOULD BE GOOD '                                
           .                                                            
      ******************************************************************
      *                  2108-FIND-DIRECT-2-WAY-FLIGHT                  
      * PROGRAM WILL USE SINGLE CURSOR TO FIND                          
      * ALL POSSIBLE RETURN FLIGHTS WITH GIVEN CRITERIA FROM Z02152     
      * PROGRAM                                                         
      * PROGRAM WILL FETCH DATA INTO THE QUEUE AND LATER WILL DISPLAY   
      * THAT INTO THE SCREEN                                            
      *                                                                 
      * HERE PROGRAM WILL USE OTHER MAP THAN THE ONE USED FOR           
      * ONEWAY FLIGHTS    (M10217)                                      
      *                                                                 
      *                                                                 
      * PROGRAM WILL :                                                  
      * 1. OPEN CUROSR.                                                 
      * 2. FETCH DATA FROM THE CURSOR ONE BY ONE INTO THE QUEU          
      * 3. CLOSE THE CURSOR      
      * 4. DISPLAY FLIGHT                                               
      ******************************************************************
       2108-FIND-DIRECT-2-WAY-FLIGHT.                                   
           SET SO-SEARCH-FOR-N-DIRECT-ONEWAY TO TRUE                    
           MOVE WS-Z02172-DEPARTURE-DATE    TO   WS-DEPARTURE-DATE      
           MOVE WS-Z02172-RETURN-DATE     TO   WS-ARRIVAL-DATE          
           MOVE WS-Z02172-DEST-AIRPORT-IATA TO T05-ARRIVAL-AIRPORT-CODE 
           MOVE WS-Z02172-ORIGIN-AIRPORT-IATA TO                        
                                              T05-DEPARTURE-AIRPORT-CODE
                                                                        
           PERFORM 7045-OPEN-DIRECT-2WAY-CURSOR                         
           PERFORM 7046-DIRECT-2WAY-TO-QUEUE                            
           PERFORM 7047-CLOSE-DIRECT-2WAY-CUR                           
                                                                        
           PERFORM 2047-DISPLAY-RETURN-FLIGHTS                          
           .                                                            
      ******************************************************************
      *                2109-NON-DIRECT-RETURN-FLIGHTS                   
      * PRAGRAPH WILL:                                                  
      * FIRST IT WILL START PROCESS OF SEARCHING NON DIRECT ONE-WAY     
      * FLIGHTS                                                         
      * THOSE FLIGHT WILL BE SAVE INTO THE QUEUE                        
      *                                                                 
      * AFTER THIS PROCESS IS COMPLEATED PARAGRAPH WILL START THIS      
      * PROCESS ONCE AGAIN BUT THIS TIME WE WILL SWITCH PLACES OF       
      * AIRPORTS (WE WILL SEARCH FLIGHTS FROM "DESTINATION" AIRPORT TO  
      * "ORIGIN" AIRPORT )                                              
      *                                                                 
      * THIS TIME WE WILL CHANGE NAME OF QUEUE WHERE FLIGHTS WILL BE    
      * SAVED                                                           
      * ALSO BY MODIFIING FLAG: SO-SEARCH-FOR-N-DIRECT-RETURN           
      * SEARCHING LOGIC CHANGES A LITTLE, NOW WE WILL VALIDATE          
      * IF ARRIVAL DATE IS VALID OR NOT                                 
      *                                                                 
      * AT THE END OF THOSE 2 PROCESSES WE WILL HAVE TWO QUEUES         
      * 1. FIRST WILL STORE INFO ABOUT "TO" FLIGHTS 
      * 2. SECOND WILL STORE INFO ABOUT "FROM" FLIGHTS                  
      *                                                                 
      * THEN WE WILL CREATE THIRD QUEU THAT WILL STORE                  
      * ALL VALID COMBINATIONS OF FLIGHT (TO) AND (FROM)                
      *                                                                 
      ******************************************************************
       2109-NON-DIRECT-RETURN-FLIGHTS.                                  
           SET SO-SEARCH-FOR-N-DIRECT-ONEWAY TO TRUE                    
           MOVE CT-FLIGHT-TO-QUEUE TO   CT-ONEWAY-QUEUE                 
           PERFORM 2105-NOT-DIRECT-ONEWAY                               
      * HERE WE WILL CHANGE QUEUE NAME AND                              
      * WE WILL SWICH PLACES OF DESTINATION AND ORIGIN AIRPORT          
                                                                        
           MOVE WS-Z02172-DEST-AIRPORT-IATA TO WS-TEMP-AIRPORT-CODE     
           MOVE WS-Z02172-ORIGIN-AIRPORT-IATA TO                        
                 WS-Z02172-DEST-AIRPORT-IATA                            
           MOVE WS-TEMP-AIRPORT-CODE TO  WS-Z02172-ORIGIN-AIRPORT-IATA  
                                                                        
      *    SET SO-SEARCH-FOR-N-DIRECT-RETURN TO TRUE                    
           MOVE CT-FLIGHT-FROM-QUEUE TO CT-ONEWAY-QUEUE                 
           MOVE WS-Z02172-RETURN-DATE  TO WS-Z02172-DEPARTURE-DATE      
           PERFORM 2105-NOT-DIRECT-ONEWAY                               
                                                                        
           SET SO-SEARCH-FOR-N-DIRECT-RETURN TO TRUE                    
      * RIGHT NOW WE WILL HAVE TWO QUEUES                               
      * 1 WITH FLIGHTS "TO"                                             
      * 2 WITH FLIGHTS "FROM"                                           
      * HERE WE WILL CHECK IF MAYBE ONE OR BOTH OF THEM IS EMPTY        
      * MAYBE THERE ARE NO FLIGHT MEETING CRITERIA                      
                                                                        
           PERFORM 2110-CHECK-BOTH-QUEUES                               
                                                                        
      * THIS PARAGRAPH WILL FIND AND MATCH VALID FLIGHTS                
      * AND WILL PUT THEM AS ONE RECORD IN A NEW QUEUE                  
           DISPLAY 'EVERYTHING SHOULD BE GOOD HERE'                     
           PERFORM 2111-MOVE-BOTH-QUEUES-TO-ONE                         
           PERFORM 2116-DISPLAY-RETURN-N-DIRECT                         
           .                                                            
      ******************************************************************
      *                   2110-CHECK-BOTH-QUEUES                        
      * THIS PARAGRAPH WILL CHECK IF ONE OF BOTH QUEUES THAT SHOULD     
      * STORE "TO" AND "FROM" FLIGHTS ARE EMPTY OR DON'T EXIST          
      * IF THIS IS TRUE THEN TRANSACTION WILL BE TERMINATED             
      ******************************************************************
       2110-CHECK-BOTH-QUEUES.                                          
                                                                        
           MOVE CT-FLIGHT-TO-QUEUE TO CT-ONEWAY-QUEUE                   
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           PERFORM   2022-READ-DIRECT-ONEWAY-QUEUE                      
           PERFORM 2052-CHECK-FOR-QIDERR                                
           IF EIBRESP = DFHRESP(ITEMERR) THEN                           
             DISPLAY 'THERE IS NO RECORDS IN QUEUE '                    
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROGRAM COULD NOT FIND ANY FLIGHTS "TO" ' TO       
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           MOVE CT-FLIGHT-FROM-QUEUE TO CT-ONEWAY-QUEUE                 
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           PERFORM   2022-READ-DIRECT-ONEWAY-QUEUE                      
           PERFORM 2052-CHECK-FOR-QIDERR                                
           IF EIBRESP = DFHRESP(ITEMERR) THEN                           
             DISPLAY 'THERE IS NO RECORDS IN QUEUE '                    
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'PROGRAM COULD NOT FIND ANY FLIGHTS "FROM" ' TO     
                                            WS-Z02141-I-ERROR-MESSAGE(1)
               SET SO-Z02141-M-WITHOUT TO TRUE                          
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2111-MOVE-BOTH-QUEUES-TO-ONE                      
      * FIRST QUEUE STORES FLIGHTS "TO"                                 
      * SECOND QUEUE STORES FLIGHTS "FROM"                              
      * THIRD QUEUE WILL STORE INFO ABOUT BOTH FLIGHTS                  
      *                                                                 
      * PARAGRAPH WILL CHECK IF THIS FLIGHTS STORED IN THIS 2 QUEUE     
      * CAN BE ADDED ( IF TIME OF DEPARTURE IS GREATER THAT TIME OF     
      * ARRIVAL)                                                        
      ******************************************************************
       2111-MOVE-BOTH-QUEUES-TO-ONE.                                    
           DISPLAY 'ZACZYNAMY 2111 PARAGRAPH '                          
                                                                        
           MOVE 1 TO WS-FIRST-QUEUE-ITERATOR                            
           MOVE 1 TO WS-SECOND-QUEUE-ITERATOR                           
           PERFORM 2112-READ-FLIGHT-TO-QUEUE                            
           PERFORM 2113-READ-FLIGHT-FROM-QUEUE                          
                                                                        
                                                                        
           PERFORM UNTIL SO-END-OF-TO-QUEUE                             
             PERFORM UNTIL SO-END-OF-FROM-QUEUE                         
                 PERFORM 7301-VALIDATE-TWO-FLIGHTS                      
                   IF SO-POSSIBLE-FLIGHTS  THEN                         
                      PERFORM 2114-ADD-THOSE-QUEUES                     
                      PERFORM 2115-WRITE-FINAL-QUEUE                    
                   END-IF                                               
                 ADD 1 TO WS-SECOND-QUEUE-ITERATOR                      
                 PERFORM 2113-READ-FLIGHT-FROM-QUEUE                    
                                                                        
             END-PERFORM                                                
                                                                        
             MOVE 1 TO WS-SECOND-QUEUE-ITERATOR                         
             PERFORM 2113-READ-FLIGHT-FROM-QUEUE                        
             ADD 1 TO WS-FIRST-QUEUE-ITERATOR                           
             PERFORM 2112-READ-FLIGHT-TO-QUEUE                          
           END-PERFORM                                                  
           .             
      ******************************************************************
      *                   2112-READ-FLIGHT-TO-QUEUE                     
      ******************************************************************
       2112-READ-FLIGHT-TO-QUEUE.                                       
           SET SO-NOT-END-OF-TO-QUEUE    TO TRUE                        
                                                                        
           EXEC CICS                                                    
            READQ TS                                                    
            QUEUE(CT-FLIGHT-TO-QUEUE)                                   
            INTO(WS-FIRST-QUEUE-STRUCTURE)                              
            ITEM(WS-FIRST-QUEUE-ITERATOR)                               
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(ITEMERR)                                        
              SET SO-END-OF-TO-QUEUE     TO TRUE                        
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2113-READ-FLIGHT-FROM-QUEUE                 
      ******************************************************************
       2113-READ-FLIGHT-FROM-QUEUE.                                     
           SET SO-NOT-END-OF-FROM-QUEUE  TO TRUE                        
           EXEC CICS                                                    
             READQ TS                                                   
             QUEUE(CT-FLIGHT-FROM-QUEUE)                                
             INTO(WS-SECOND-QUEUE-STRUCTURE)                            
             ITEM(WS-SECOND-QUEUE-ITERATOR)                             
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              CONTINUE                                                  
           WHEN DFHRESP(ITEMERR)  
              SET SO-END-OF-FROM-QUEUE TO TRUE                          
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     2114-ADD-THOSE-QUEUES                       
      * THIS PARAGRAPH WILL CONCATENATE 2 QUEUS INTO ONE                
      *                                                                 
      *                                                                 
      ******************************************************************
       2114-ADD-THOSE-QUEUES.                                           
           MOVE QUEUE-F-FLIGHT-NUMBER  TO                               
                 QUEUE-6-FIRST-TO-NUMBER                                
           MOVE QUEUE-S-FLIGHT-NUMBER  TO                               
                 QUEUE-6-FIRST-FROM-NUMBER                              
           MOVE QUEUE-F-DEPARTURE-TIMESTAMP  TO  WS-TEMP-TIMESTAMP      
              MOVE WS-TEMP-DATE TO  QUEUE-6-FLIGHT-TO-DEP-DATE          
              MOVE WS-TEMP-TIME TO QUEUE-6-FLIGHT-TO-DEP-TIME           
           MOVE QUEUE-F-ARRIVAL-TIMESTAMP    TO  WS-TEMP-TIMESTAMP      
              MOVE WS-TEMP-DATE TO  QUEUE-6-FLIGHT-TO-ARV-DATE          
              MOVE WS-TEMP-TIME TO QUEUE-6-FLIGHT-TO-ARV-TIME           
           MOVE QUEUE-S-DEPARTURE-TIMESTAMP  TO  WS-TEMP-TIMESTAMP      
              MOVE WS-TEMP-DATE TO  QUEUE-6-FLIGHT-FROM-DEP-DATE        
              MOVE WS-TEMP-TIME TO QUEUE-6-FLIGHT-FROM-DEP-TIME         
           MOVE QUEUE-S-ARRIVAL-TIMESTAMP    TO  WS-TEMP-TIMESTAMP      
              MOVE WS-TEMP-DATE TO  QUEUE-6-FLIGHT-FROM-ARV-DATE        
              MOVE WS-TEMP-TIME TO QUEUE-6-FLIGHT-FROM-ARV-TIME         
           MOVE QUEUE-F-DEPARTURE-AIRPORT-CODE   TO                     
                           QUEUE-6-DEPARTURE-AIRPORT                    
           MOVE QUEUE-S-DEPARTURE-AIRPORT-CODE  TO                      
                           QUEUE-6-ARRIVAL-AIRPORT                      
           DISPLAY '2114 DEPARTUER AIRPORT: ' QUEUE-6-DEPARTURE-AIRPORT 
           DISPLAY '2114 ARRIVAL AIRPORT: '  QUEUE-6-ARRIVAL-AIRPORT    
           MOVE QUEUE-F-TRANSFER-NUMBER  TO                             
                         QUEUE-6-NUMBER-OF-TO-TRANSFERS     
           MOVE QUEUE-S-TRANSFER-NUMBER  TO                             
                         QUEUE-6-NUMBER-OF-FR-TRANSFERS                 
           MOVE QUEUE-F-FIRST-FLIGHT-ID TO QUEUE-6-TO-FLIGHT-ID(1)      
           MOVE QUEUE-F-FREE-SEATS      TO QUEUE-6-TO-FREE-SEATS(1)     
           MOVE 2 TO WS-ITER9                                           
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL                   
                     WS-ITER2 > QUEUE-6-NUMBER-OF-TO-TRANSFERS          
              MOVE     QUEUE-F-FLIGHT-ID(WS-ITER2) TO                   
                        QUEUE-6-TO-FLIGHT-ID(WS-ITER9)                  
              MOVE QUEUE-F-FREE-SEATS-T(WS-ITER2) TO                    
                       QUEUE-6-TO-FREE-SEATS(WS-ITER9)                  
              ADD 1 TO WS-ITER9                                         
           END-PERFORM                                                  
                                                                        
           MOVE QUEUE-S-FIRST-FLIGHT-ID TO QUEUE-6-FROM-FLIGHT-ID(1)    
           MOVE QUEUE-S-FREE-SEATS      TO QUEUE-6-FROM-FREE-SEATS(1)   
           MOVE 2 TO WS-ITER9                                           
                                                                        
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL                   
                     WS-ITER2 > QUEUE-6-NUMBER-OF-FR-TRANSFERS          
              MOVE     QUEUE-S-FLIGHT-ID(WS-ITER2) TO                   
                        QUEUE-6-FROM-FLIGHT-ID(WS-ITER9)                
              MOVE QUEUE-S-FREE-SEATS-T(WS-ITER2) TO                    
                       QUEUE-6-FROM-FREE-SEATS(WS-ITER9)                
              ADD 1 TO WS-ITER9                                         
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      2115-WRITE-FINAL-QUEUE                     
      * THIS QUEUE WILL STORE INFORMATIONS ABOUT RETURN NON DIRECT      
      *  FLIGHST                                                        
      ******************************************************************
       2115-WRITE-FINAL-QUEUE.                                          
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-TO-AND-FROM-QUEUE)    
             FROM(WS-TO-AND-FROM-Q-STRUCTURE)                           
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                2116-DISPLAY-RETURN-N-DIRECT                     
      * PARAGRAPH WILL DISPLAY FIRST 4 RETURN NON DIRECT FLIGHTS        
      * STARTING FROM 1                                                 
      ******************************************************************
       2116-DISPLAY-RETURN-N-DIRECT.                                    
           MOVE 1 TO  WS-Z02172-LAST-Q5-REC-ID                          
           PERFORM 2117-DISPLAY-NEXT-4-2WAY-N-DIR                       
           .                                                            
      ******************************************************************
      *                2117-DISPLAY-NEXT-4-2WAY-N-DIR                   
      * PROGRAM WILL DISPALY NEXT 4 RETURN FLIGHT THAT ARE NON          
      * DIRECT                                                          
      ******************************************************************
       2117-DISPLAY-NEXT-4-2WAY-N-DIR.                                  
           SET SO-NOT-END-OF-FINAL-QUEUE TO TRUE                        
           MOVE WS-Z02172-LAST-Q5-REC-ID TO WS-WHAT-RECORD-TO-READ      
           PERFORM 2058-INITIALIZE-RETURN-MAP                           
           PERFORM 2131-INITIALIZE-RETURN-ARRAY                         
           PERFORM 2118-READ-TO-AND-FROM-Q                              
           PERFORM 2052-CHECK-FOR-QIDERR                                
           PERFORM 2119-MOVE-QUEUE-TO-SCREEN                            
           PERFORM 2081-SEND-2WAY-FLIGHT-MAP                            
           .                                                            
      ******************************************************************
      *                       2118-READ-TO-AND-FROM-Q                   
      * TO-AND-FROM QUEUE IS A QUEUE CREATED FROM 2 OTHER AND NOW       
      * STORES INFORMATIONS ABOUT RETURN NOT DIRECT FLIGHTS             
      ******************************************************************
       2118-READ-TO-AND-FROM-Q.                                         
           EXEC CICS                                                    
             READQ TS   
             QUEUE(CT-TO-AND-FROM-QUEUE)                                
             INTO(WS-TO-AND-FROM-Q-STRUCTURE)                           
             ITEM(WS-WHAT-RECORD-TO-READ)                               
             NOHANDLE                                                   
           END-EXEC                                                     
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
           WHEN DFHRESP(QIDERR)                                         
              CONTINUE                                                  
           WHEN DFHRESP(ITEMERR)                                        
              SET SO-END-OF-FINAL-QUEUE TO TRUE                         
           WHEN OTHER                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                2119-MOVE-QUEUE-TO-SCREEN                        
      ******************************************************************
       2119-MOVE-QUEUE-TO-SCREEN.                                       
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 4      
                                              OR SO-END-OF-FINAL-QUEUE  
              IF WS-ITER3 = 1 THEN                                      
                MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-FIRST-Q5-REC-ID
              END-IF                                                    
              PERFORM 2082-MOVE-QUEUE-6-TO-SCREEN                       
              MOVE WS-WHAT-RECORD-TO-READ TO WS-Z02172-LAST-Q5-REC-ID   
              MOVE WS-WHAT-RECORD-TO-READ TO                            
                         WS-RETURN-N-DIRECT-ARRAY(WS-ITER3)             
              ADD 1 TO WS-WHAT-RECORD-TO-READ                           
              PERFORM 2118-READ-TO-AND-FROM-Q                           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                2120-PREPARE-TO-FLIGHTS-ARRAY                    
      ******************************************************************
       2120-PREPARE-TO-FLIGHTS-ARRAY.   
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
           MOVE CT-EMPTY-FIELD TO                                       
                                 WS-FLIGHT-ARRAY-TO(WS-ITER5)           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                2121-PREPARE-FROM-FLIGHTS                        
      ******************************************************************
       2121-PREPARE-FROM-FLIGHTS.                                       
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
           MOVE CT-EMPTY-FIELD TO                                       
                            WS-FLIGHT-ARRAY-FROM(WS-ITER5)              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                2125-DISPLAY-PREV-4-2WAY-N-DIR                   
      ******************************************************************
       2125-DISPLAY-PREV-4-2WAY-N-DIR.                                  
           IF WS-Z02172-FIRST-Q5-REC-ID - 3 >= 1 THEN                   
             SUBTRACT 3 FROM WS-Z02172-FIRST-Q5-REC-ID                  
           ELSE                                                         
             MOVE 1 TO WS-Z02172-FIRST-Q5-REC-ID                        
           END-IF                                                       
           MOVE WS-Z02172-FIRST-Q5-REC-ID TO WS-Z02172-LAST-Q5-REC-ID   
           PERFORM 2117-DISPLAY-NEXT-4-2WAY-N-DIR                       
           .                                                            
      ******************************************************************
      *                 2130-PROCESS-RETURN-2WAY                        
      ******************************************************************
       2130-PROCESS-RETURN-2WAY.                                        
           PERFORM 2076-RECEIVE-MAP-3                                   
           .                                                            
      ******************************************************************
      *                 2131-INITIALIZE-RETURN-ARRAY                    
      * PARAGRAPH WILL INITIALIZE ARRAY THAT WILL STORE INFORMATION     
      * ABOUT WHERE ON THE SCREEN WE GOT WHAT QUEUE ITERM   
      *                                                                 
      * LATER WHEN USER WILL CHOOSE A FLIGHT                            
      * PROGRAM WILL KNOW WHICH POSITION ON THE SCREEN = TO             
      * WHAT QUEUE ITEM NUMBER (NUMBER OF ROW                           
      ******************************************************************
       2131-INITIALIZE-RETURN-ARRAY.                                    
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 4      
              MOVE 0 TO WS-RETURN-N-DIRECT-ARRAY(WS-ITER2)              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2132-PREPARE-2WAY-N-DIR                      
      * THIS PARAGRAPH WILL BE CALLED WHEN PROGRAM SEARCHES FOR         
      * RETURN NOT DIRECT FLIGHTS                                       
      * AND USER CHOOSE ONE OF THOSE FOUNDED FLIGHTS                    
      * HERE WE WILL PREPARE DATA FOR BOTH POSSIBLE OPTIONS             
      * '1' - > CHOOSE THIS FLIGHT (Z02192 PROGRAM CALLED )             
      * '2' -> DISPLAY DETAILS (Z02182 PROGRAM CALLED )                 
      *                                                                 
      * PARAGRAPH LOGIC IS ONLY TO MOVE DATA TO VALID PARAGRAPHS        
      * THERE IS NO DEEPER LOGIC HERE                                   
      ******************************************************************
       2132-PREPARE-2WAY-N-DIR.                                         
                                                                        
           MOVE QUEUE-6-TO-FLIGHT-ID(1) TO Z02182-FLIGHT-ID             
           DISPLAY 'Z02172 2132 Z02182-FLIGHT-ID: ' Z02182-FLIGHT-ID    
           MOVE QUEUE-6-FIRST-TO-NUMBER TO Z02182-FLIGHT-NUMBER         
           MOVE QUEUE-6-FLIGHT-TO-DEP-DATE TO Z02182-DEPARTURE-DATE     
           MOVE QUEUE-6-FLIGHT-TO-DEP-TIME TO Z02182-DEPARTURE-TIME     
           MOVE QUEUE-6-FLIGHT-TO-ARV-DATE TO Z02182-ARRIVAL-DATE       
           MOVE QUEUE-6-FLIGHT-TO-ARV-TIME TO Z02182-ARRIVAL-TIME       
           MOVE QUEUE-6-DEPARTURE-AIRPORT TO                            
                                Z02182-AIRPORT-ORIGIN-CODE              
           MOVE QUEUE-6-ARRIVAL-AIRPORT   TO                            
                                 Z02182-AIRPORT-DES-CODE                
           MOVE QUEUE-6-TO-FREE-SEATS(1) TO      
                             Z02182-AMOUNT-OF-FREE-SETS-M               
           COMPUTE WS-COMMON-TRANSFER-NUMBER =                          
                        QUEUE-6-NUMBER-OF-TO-TRANSFERS +                
                           QUEUE-6-NUMBER-OF-FR-TRANSFERS  + 1          
           MOVE WS-COMMON-TRANSFER-NUMBER      TO                       
                                 Z02182-TRANSFER-NUMBER                 
           PERFORM 2133-MOVE-TABLES                                     
                                                                        
           MOVE WS-COMMON-TRANSFER-NUMBER TO WS-TEMP-NUMERIC            
           ADD 1 TO WS-TEMP-NUMERIC                                     
           MOVE WS-TEMP-NUMERIC TO Z02192-ONE-WAY-FLIGHT-AMOUNT         
           MOVE Z02192-ONE-WAY-FLIGHT-AMOUNT TO                         
                      Z02192-NUMBER-OF-FLIGHTS                          
           MOVE  WS-Z02172-TICKET-NUMBER TO                             
                                   Z02192-ONE-WAY-TICKET-NUMBER         
                                                                        
      * AT FIRST WE NEED TO MOVE ALL "TO" FLIGHTS                       
      * THEN WE WILL MOVE ALL "FROM" FLIGHTS TO VALID ARRAY             
           MOVE 1 TO WS-ITER9                                           
           MOVE QUEUE-6-NUMBER-OF-TO-TRANSFERS TO WS-ITER11             
           ADD 1 TO WS-ITER11                                           
           PERFORM VARYING WS-ITER10 FROM 1 BY 1 UNTIL WS-ITER10 >      
                                   WS-ITER11                            
                 MOVE QUEUE-6-TO-FLIGHT-ID(WS-ITER10) TO                
                           Z02192-ONE-WAY-FL-ID(WS-ITER9)               
                 ADD 1 TO WS-ITER9                                      
           END-PERFORM                                                  
                                                                        
           MOVE QUEUE-6-NUMBER-OF-FR-TRANSFERS TO WS-ITER11             
           ADD 1 TO WS-ITER11                                           
           PERFORM VARYING WS-ITER10 FROM 1 BY 1 UNTIL WS-ITER10 >      
                                   WS-ITER11                            
                                                                        
                 DISPLAY 'QUEUE-6-FROM-FLIGHT-ID(WS-ITER10) '           
                               QUEUE-6-FROM-FLIGHT-ID(WS-ITER10)        
                 MOVE QUEUE-6-FROM-FLIGHT-ID(WS-ITER10) TO    
                           Z02192-ONE-WAY-FL-ID(WS-ITER9)               
                                                                        
                 ADD 1 TO WS-ITER9                                      
           END-PERFORM                                                  
           DISPLAY 'DATA WERE MOVED '                                   
           DISPLAY 'RETURN NON DIRECT FLIGHTS'                          
           .                                                            
      ******************************************************************
      *                      2133-MOVE-TABLES                           
      * THIS PARAGRAPH WILL MOVE DATA FROM "THIRD" QUEUE                
      * QUEUE THAT STORES INFO ABOUT RETURN NON DIRECT FLIGHTS          
      *                                                                 
      * TO COMMON COMMAREA THAT WILL BE USED  TO PASS THIS DATA         
      * TO Z02182 PROGRAM                                               
      ******************************************************************
       2133-MOVE-TABLES.                                                
           DISPLAY '2133-MOVE-TABLEDATADISPLAE'                         
           MOVE 1 TO WS-ITER5                                           
           MOVE QUEUE-6-NUMBER-OF-TO-TRANSFERS TO WS-ITER11             
           ADD 1 TO WS-ITER11                                           
           PERFORM VARYING WS-ITER2 FROM 2 BY 1 UNTIL WS-ITER2 >        
                                   WS-ITER11                            
                 INITIALIZE Z02182-TR-FLIGHT-SEATS(WS-ITER5)            
                 INITIALIZE Z02182-TR-FLIGHT-ID(WS-ITER5)               
                                                                        
                 MOVE QUEUE-6-TO-FLIGHT-ID(WS-ITER2)  TO                
                                Z02182-TR-FLIGHT-ID(WS-ITER5)           
                 MOVE QUEUE-6-TO-FREE-SEATS(WS-ITER2) TO                
                         Z02182-TR-FLIGHT-SEATS(WS-ITER5)               
                 DISPLAY 'TR FLIGHT ID: ' Z02182-TR-FLIGHT-ID(WS-ITER5) 
                 DISPLAY 'TR FLIGHT SEATS: '                            
                           Z02182-TR-FLIGHT-SEATS(WS-ITER5)             
                 ADD 1 TO WS-ITER5                                      
           END-PERFORM                                                  
                                                                        
           MOVE QUEUE-6-NUMBER-OF-FR-TRANSFERS TO WS-ITER11
                                                                        
           ADD 1 TO WS-ITER11                                           
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 >        
                                              WS-ITER11                 
                 INITIALIZE Z02182-TR-FLIGHT-SEATS(WS-ITER5)            
                 INITIALIZE Z02182-TR-FLIGHT-ID(WS-ITER5)               
                                                                        
                 MOVE QUEUE-6-FROM-FLIGHT-ID(WS-ITER2)  TO              
                                Z02182-TR-FLIGHT-ID(WS-ITER5)           
                 MOVE QUEUE-6-FROM-FREE-SEATS(WS-ITER2) TO              
                         Z02182-TR-FLIGHT-SEATS(WS-ITER5)               
                 ADD 1 TO WS-ITER5                                      
                 DISPLAY 'TR FLIGHT ID: ' Z02182-TR-FLIGHT-ID(WS-ITER5) 
                 DISPLAY 'TR FLIGHT SEATS: '                            
                           Z02182-TR-FLIGHT-SEATS(WS-ITER5)             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2200-CHECK-EIBRESP                          
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
      *                    2210-PREPARE-FLIGHT-DATA                     
      * PROGRAM WILL SEARCH THOUGH THE QUEUE IN ORDER TO FIND           
      * RECORD THAT HAS THE SAME FLIGHT ID AS IN WS-WHAT-FLIGHT-NUMBER  
      * ARRAY                                                           
      * ( FLIGHT ID WAS PUT THERE WHILE DISPLAYING RECORD ON THE SCREEN)
      *                                                                 
      * AFTER FINDING THIS RECORD PROGRAM WILL CHECK IF ALL SUB FLIGHTS 
      * ARE ALSO CORRECT                                                
      *                                                                 
      * THIS PARAGRAPH CAN BE USED IN TWO SCENARIOS :                   
      *       1. USER CHOOSE OPTION '1'                                 
      *       2. USER CHOOSE OPTION '2'                                 
      * THIS TWO OPTIONS CALLS TWO DIFFERENT PROGRAMS                   
      * BUT THIS PARAGRAPH IS COMMON FOR BOTH                           
      ******************************************************************
       2210-PREPARE-FLIGHT-DATA.                                        
           DISPLAY '2210 WAS PERFORMED'                                 
      * WE NEED TO FIND QUEUE RECORD WHERE QUEUE-FLIGHT-ID     IS       
      * EQUAL TO  WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)        
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           MOVE 1 TO WS-WHAT-RECORD-TO-READ                             
           PERFORM UNTIL SO-END-OF-QUEUE                                
             PERFORM 2022-READ-DIRECT-ONEWAY-QUEUE                      
              DISPLAY '2210 FLIGHT ID: '  QUEUE-FLIGHT-ID               
              DISPLAY '2210 WS-WHAT-FLIGHT-NUMBER: '                    
                     WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)     
              IF QUEUE-FLIGHT-ID     =                                  
                    WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)      
              THEN                                                      
      * NOW WE NEED TO CHECK IF ALL SUB FLIGHTS ARE CORRECT             
      * (IF THERE IS ANY)                                               
                 DISPLAY 'XYZSYZZYSY'                                   
                 DISPLAY 'LOTY SIE ROWNAJA '                            
                 MOVE QUEUE-TRANSFER-NUMBER TO WS-TEMP-NUMERIC          
                 MOVE                                                   
                     WS-WHAT-TRANSFER-NUMBER(WS-USER-CHOICE-POSITION)   
                       TO WS-TEMP-NUMERIC2                              
                 DISPLAY 'WS-WHAT-TRANSFER-NUMBER: ' WS-TEMP-NUMERIC2   
                                                                        
                 DISPLAY 'WS-TEMP-NUMERIC: ' WS-TEMP-NUMERIC            
                 IF WS-WHAT-TRANSFER-NUMBER(WS-USER-CHOICE-POSITION)    
                  = WS-TEMP-NUMERIC                                     
                    DISPLAY 'WS-HWAT-TRANSFER-NUMBER  =     '           
                              ' WS-TEMP NUMERIC '                       
                    PERFORM 2212-CHECK-SUB-FLIGHTS                      
                     IF SO-SUB-FLIGHTS-CORRECT THEN                     
                       SET SO-END-OF-QUEUE TO TRUE                      
                     END-IF                                             
                 END-IF                                                 
              END-IF                                                    
             ADD 1 TO WS-WHAT-RECORD-TO-READ                            
           END-PERFORM                                                  
                                                                        
      * IT MEANS THAT WE DIDN'T FIND ANY RECORD IN THE QUEUE            
      * THAT MEETS CRITERIA, THIS SHOULDN'T HAPPEN                      
           IF NOT SO-SUB-FLIGHTS-CORRECT                                
                                                                        
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE         
             MOVE 'Z02172, THERE IS NO SUCH FLIGHT- ERROR!! '           
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
                                                                        
           END-IF                                                       
           MOVE QUEUE-FLIGHT-ID       TO Z02182-FLIGHT-ID               
           MOVE QUEUE-FLIGHT-NUMBER   TO  Z02182-FLIGHT-NUMBER          
           MOVE ONE-WAY-Q-DATE-DEP  TO     Z02182-DEPARTURE-DATE        
           MOVE ONE-WAY-Q-TIME-DEP  TO Z02182-DEPARTURE-TIME            
           MOVE ONE-WAY-Q-ARV-DATE TO Z02182-ARRIVAL-DATE               
           MOVE ONE-WAY-Q-ARV-TIME TO Z02182-ARRIVAL-TIME               
           MOVE QUEUE-DEPARTURE-AIRPORT-CODE TO                         
                                   Z02182-AIRPORT-ORIGIN-CODE           
           MOVE QUEUE-ARRIVAL-AIRPORT-CODE TO                           
                               Z02182-AIRPORT-DES-CODE                  
           DISPLAY 'KRZYSZTOFCYGAN'                                     
           DISPLAY 'DEPARTURE: ' Z02182-AIRPORT-ORIGIN-CODE             
           DISPLAY 'ARRIVAL: ' Z02182-AIRPORT-DES-CODE                  
           MOVE ONE-WAY-Q-FREE-SEATS TO Z02182-AMOUNT-OF-FREE-SETS-M    
           PERFORM 2211-MOVE-TABLES                                     
           MOVE QUEUE-TRANSFER-NUMBER TO                                
                             Z02182-TRANSFER-NUMBER                     
           MOVE QUEUE-AIRLINE-CODE TO Z02182-AIRLINE-CODE-M             
                                                                        
                                                                        
                                                                        
           MOVE WS-Z02172-TICKET-NUMBER TO                              
                                            Z02192-ONE-WAY-TICKET-NUMBER
           DISPLAY 'PRZED CALL DO Z02192 QUEUE TRANSFER NUMBER: '       
                            QUEUE-TRANSFER-NUMBER                       
           MOVE  QUEUE-TRANSFER-NUMBER TO WS-TEMP-NUMERIC               
           ADD 1 TO WS-TEMP-NUMERIC                                     
           MOVE WS-TEMP-NUMERIC TO Z02192-ONE-WAY-FLIGHT-AMOUNT         
                                                                        
      * PROGRAM USES 2 DIFFERENT ITERATORS BECAUSE OF THE FACT          
      * THAT ONE-WAY-Q STORES FIRST FLIGHT DIFFERENTLY THATN            
      * Z02192 COMMAREA                                                 
                                                                        
      * PROGRAM HAVE TO MOVE FIRST FLIGHT MANUALY                       
      * NEXT FLIGHTS WILL BE MOVED BY USING THE LOOP                    
           MOVE Z02192-ONE-WAY-FLIGHT-AMOUNT TO                         
                     Z02192-NUMBER-OF-FLIGHTS                           
           MOVE QUEUE-FLIGHT-ID    TO  Z02192-ONE-WAY-FL-ID(1)          
           MOVE 1 TO WS-ITER10                                          
           PERFORM VARYING WS-ITER9 FROM 2 BY 1 UNTIL                   
                               WS-ITER9 >  Z02192-ONE-WAY-FLIGHT-AMOUNT 
               MOVE  ONE-WAY-Q-FLIGHT-ID(WS-ITER10) TO                  
                                          Z02192-ONE-WAY-FL-ID(WS-ITER9)
                ADD 1 TO WS-ITER10                                      
           END-PERFORM                                                  
      * AFTER THIS PARAGRAPH ENDS WITHOUT ERROR                         
      * WE GOT PREPARED DATA FOR CALLING Z02182 PROGRAM                 
      * AND FOR CALLING Z02192 PROGRAM                                  
           .                                                            
      ******************************************************************
      *                     2211-MOVE-TABLES                            
      * PARAGRAPH WILL MOVE DATA FROM THE QUEUE TO COMMAREA             
      ******************************************************************
       2211-MOVE-TABLES.                                                
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 5      
             MOVE ONE-WAY-Q-FLIGHT-ID(WS-ITER5) TO                      
                                           Z02182-TR-FLIGHT-ID(WS-ITER5)
             MOVE ONE-WAY-Q-FREE-SEATS-T(WS-ITER5) TO                   
                                        Z02182-TR-FLIGHT-SEATS(WS-ITER5)
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2212-CHECK-SUB-FLIGHTS                       
      * MAIN FLIGHT IS A FIRST FLIGHT IN THE SEQUENCE OF TRANSFER       
      * FLIGHTS                                                         
      * A SUB FLIGHT IS A FLIGHT THAT IS NOT FIRST IN SEQUENCE        
      *                                                                 
      * THIS PARAGRAPH WILL CHECK IF MAIN FLIGHT HAS ALL OF HIS         
      * SUBFLIGHT SAME AS IN THE MOMENT OF DISPLAYING THAT ON THE SCREEN
      *                                                                 
      * THERE COULD BE MANY FLIGHTS WITH THE SAME MAIN FLIGHT           
      * SO WE HAVE TO FIND OUT IF ALL OF THEIRS SUBFLIGHTS ARE CORRECT  
      ******************************************************************
       2212-CHECK-SUB-FLIGHTS.                                          
           DISPLAY '2212  PERFORMED '                                   
                                                                        
           SET SO-SUB-FLIGHTS-CORRECT  TO TRUE                          
           PERFORM VARYING WS-ITER8 FROM 1 BY 1 UNTIL WS-ITER8 >        
                      WS-TEMP-NUMERIC    OR SO-SUB-FLIGHTS-INVALID      
             DISPLAY '2212 IN LOOP: '                                   
             DISPLAY 'WS-WHAT-SUB-FLIGHT:   '                           
                    WS-WHAT-SUB-FLIGHT(WS-USER-CHOICE-POSITION,WS-ITER8)
             DISPLAY 'ONE-WAY-Q-FLIGHT-ID: '                            
                                           ONE-WAY-Q-FLIGHT-ID(WS-ITER8)
             IF WS-WHAT-SUB-FLIGHT(WS-USER-CHOICE-POSITION,WS-ITER8)    
               = ONE-WAY-Q-FLIGHT-ID(WS-ITER8)                          
             THEN                                                       
                CONTINUE                                                
             ELSE                                                       
                SET SO-SUB-FLIGHTS-INVALID  TO TRUE                     
             END-IF                                                     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2213-PREPARE-2WAY-DIRECTS                    
      * THIS PARAGRAPH NEED TO FIND PARTICULAR RECORD IN 2WAY-DIR       
      * 2WAY-DIR-RECORD WILL BE THEN SENDED TO Z02182 PROGRAM           
      ******************************************************************
       2213-PREPARE-2WAY-DIRECT.                                        
           SET SO-NOT-END-OF-QUEUE TO TRUE                              
           SET SO-RECORD-NOT-FOUND   TO TRUE                            
           MOVE 1 TO WS-WHAT-RECORD-TO-READ     
           PERFORM 2059-READ-DIRECT-2WAY                                
           PERFORM 2052-CHECK-FOR-QIDERR                                
                                                                        
           PERFORM UNTIL SO-END-OF-QUEUE OR SO-RECORD-FOUND             
                                                                        
              IF 2WAY-DIR-FLIGHT-TO-NUMBER =                            
                       WS-FLIGHT-ARRAY-TO(WS-CHOICE-POSITION) AND       
                2WAY-DIR-FLIGHT-FROM-NUMBER =                           
                       WS-FLIGHT-ARRAY-FROM(WS-CHOICE-POSITION) THEN    
                                                                        
                 SET SO-RECORD-FOUND TO TRUE                            
              ELSE                                                      
                 ADD 1 TO WS-WHAT-RECORD-TO-READ                        
              END-IF                                                    
              PERFORM 2059-READ-DIRECT-2WAY                             
           END-PERFORM                                                  
                                                                        
           IF SO-RECORD-NOT-FOUND THEN                                  
             PERFORM 2400-INITIALIZE-ERROR-MESSAGE                      
             MOVE 'Z02172 2WAY, NIE MA LOTU W ONE-WAY-Q  - BLAD!!!! '   
                                         TO WS-Z02141-I-ERROR-MESSAGE(1)
             SET SO-Z02141-M-WITH TO TRUE                               
             PERFORM 2300-CALL-ERROR-ROUTINE                            
           END-IF                                                       
                                                                        
           MOVE 2WAY-DIR-FLIGHT-TO-ID          TO                       
                       Z02182-2WAY-FLIGHT-TO-ID                         
           MOVE 2WAY-DIR-FLIGHT-FROM-ID          TO                     
                       Z02182-2WAY-FLIGHT-FROM-ID                       
           MOVE 2WAY-DIR-FLIGHT-TO-NUMBER      TO                       
                       Z02182-2WAY-FLIGHT-TO-NUMBER                     
           MOVE 2WAY-DIR-FLIGHT-TO-DEP-DATE    TO                       
                       Z02182-2WAY-FLIGHT-TO-DEP-DATE                   
           MOVE 2WAY-DIR-FLIGHT-TO-DEP-TIME    TO                       
                       Z02182-2WAY-FLIGHT-TO-DEP-TIME                   
           MOVE 2WAY-DIR-FLIGHT-TO-ARV-DATE    TO  
                       Z02182-2WAY-FLIGHT-TO-ARV-DATE                   
           MOVE 2WAY-DIR-FLIGHT-TO-ARV-TIME    TO                       
                        Z02182-2WAY-FLIGHT-TO-ARV-TIME                  
           MOVE 2WAY-DIR-FLIGHT-FROM-NUMBER    TO                       
                        Z02182-2WAY-FLIGHT-FROM-NUMBER                  
           MOVE 2WAY-DIR-FLIGHT-FROM-DEP-DATE  TO                       
                        Z02182-2WAY-FL-FROM-DEP-DATE                    
           MOVE 2WAY-DIR-FLIGHT-FROM-DEP-TIME  TO                       
                        Z02182-2WAY-FL-FROM-DEP-TIME                    
           MOVE 2WAY-DIR-FLIGHT-FROM-ARV-DATE  TO                       
                        Z02182-2WAY-FL-FROM-ARV-DATE                    
           MOVE 2WAY-DIR-FLIGHT-FROM-ARV-TIME  TO                       
                        Z02182-2WAY-FL-FROM-ARV-TIME                    
           MOVE 2WAY-DIR-DEPARTURE-AIRPORT     TO                       
                        Z02182-2WAY-DEPARTURE-AIRPORT                   
           MOVE 2WAY-DIR-ARRIVAL-AIRPORT       TO                       
                         Z02182-2WAY-ARRIVAL-AIRPORT                    
           MOVE 2WAY-DIR-TO-FLIGHT-SEATS       TO                       
                         Z02182-2WAY-TO-FLIGHT-SEATS                    
           MOVE 2WAY-DIR-FROM-FLIGHT-SEATS     TO                       
                         Z02182-2WAY-FROM-FLIGHT-SEATS                  
           MOVE 2WAY-DIR-TO-FLIGHT-AIRLINE     TO                       
                         Z02182-2WAY-TO-FLIGHT-AIRLINE                  
           MOVE 2WAY-DIR-FROM-FLIGHT-AIRLINE   TO                       
                         Z02182-2WAY-FROM-FL-AIRLINE                    
                                                                        
      * MOVE STATEMENTS BELOW WILL PREPARE Z02192 DATA                  
           MOVE WS-Z02172-TICKET-NUMBER TO                              
                                            Z02192-ONE-WAY-TICKET-NUMBER
           MOVE 2 TO Z02192-ONE-WAY-FLIGHT-AMOUNT                       
           MOVE Z02192-ONE-WAY-FLIGHT-AMOUNT TO                         
                     Z02192-NUMBER-OF-FLIGHTS                           
           MOVE Z02182-2WAY-FLIGHT-TO-ID TO                             
                         Z02192-ONE-WAY-FL-ID(1)                        
           MOVE Z02182-2WAY-FLIGHT-FROM-ID TO                           
                         Z02192-ONE-WAY-FL-ID(2)      
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
      *                    2301-GET-CHOICE-POSITION-2WAY                
      *                                                                 
      ******************************************************************
       2301-GET-CHOICE-POSITION-2WAY.                                   
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 4      
                IF BCHOICE-I(WS-ITER3) = SPACE OR LOW-VALUES THEN       
                  CONTINUE                                              
                ELSE                                                    
                  IF BCHOICE-I(WS-ITER3)  = '1' OR '2' THEN             
                                                                        
                     MOVE BCHOICE-I(WS-ITER3) TO WS-USER-CHOICE         
                     MOVE WS-ITER3 TO WS-CHOICE-POSITION                
                     ADD 1 TO WS-COUNT-USER-CHOICE                      
                  ELSE                                                  
      * IF USER PLACED SOMETHING OTHER THAN '1' OR '2' THEN WE WILL    
      * DISPLAY AN ERROR                                                
                    PERFORM 2400-INITIALIZE-ERROR-MESSAGE               
                     MOVE 'INVALID CHOICE, CHOOSE 1 OR 2' TO            
                                     WS-Z02141-I-ERROR-MESSAGE(1)       
                       SET SO-Z02141-M-WITH TO TRUE                     
                     PERFORM 2300-CALL-ERROR-ROUTINE                    
                  END-IF                                                
                END-IF                                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2302-CHECK-CHOICE-NUMBER                     
      * PARAGRAPH WILL CHECK IF USER PLACED HIS CHOICE ONLY             
      * FOR ONE ROW OF DATA (ONE FLIGHT OR ONE SET OF FLIGHTS)          
      ******************************************************************
       2302-CHECK-CHOICE-NUMBER.                                        
      * IF USER CHECKED MORE THAN 1 ROW (1 FLIGHT)                      
      * THEN WE WILL DISPLAY ERRROR                                     
           IF WS-COUNT-USER-CHOICE > 1 THEN                             
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU CAN CHOOSE ONLY 1 FLIGHT' TO                   
                               WS-Z02141-I-ERROR-MESSAGE(1)             
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
      * IF USER DIDN'T CHECK ANY ROW                                    
      * THEN WE WILL DISPLAY ERRROR                                     
           IF WS-COUNT-USER-CHOICE = 0 THEN                             
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU HAVE TO CHOOSE SOMETHING' TO                   
                               WS-Z02141-I-ERROR-MESSAGE(1)             
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2303-VALIDATE-N-DIRECT-2WAY                  
      * PARAGRAPH IS CALLED TO DETERMINE IF USER PLACED HIS CHOICE      
      * IN VALID POSITION ON THE SCREEN                                 
      ******************************************************************
       2303-VALIDATE-N-DIRECT-2WAY.                                     
           DISPLAY 'WARTOSC WYBRANA PRZEZ USERA :  '                    
                          WS-RETURN-N-DIRECT-ARRAY(WS-CHOICE-POSITION)  
      * THIS ARRAY WAS SET TO 0 BEFORE DATA WERE DISPLAYED FOR THE      
      * USER (IF THIS POSITION IS EQUAL TO 0 THEN IT MEANS THAT NO      
      * DATA WAS PUT THERE )                                            
                                                                        
           IF WS-RETURN-N-DIRECT-ARRAY(WS-CHOICE-POSITION)  =  0        
                                                                        
                 PERFORM 2400-INITIALIZE-ERROR-MESSAGE                  
                 MOVE 'YOU HAVE TO CHOOSE NOT EMPTY FLIGHT ' TO         
                                 WS-Z02141-I-ERROR-MESSAGE(1)           
                 SET SO-Z02141-M-WITH TO TRUE                           
                 PERFORM 2300-CALL-ERROR-ROUTINE                        
           ELSE                                                         
      * IF THIS NUMBER IS NOT EQUAL TO ZERO IT MEANS WE HAVE            
      * NUMBER OF THE ITEM STORED THERE                                 
      * SO WE WILL READ THAT ITEM OF THE QUEUE (THAT RECORD )           
                                                                        
               MOVE  WS-RETURN-N-DIRECT-ARRAY(WS-CHOICE-POSITION)       
                                               TO WS-WHAT-RECORD-TO-READ
               PERFORM 2118-READ-TO-AND-FROM-Q                          
      * TEST                                                            
               DISPLAY 'ALAMAKOTAFILEMONACZARNEGO '                     
               DISPLAY 'PO PRZECZYTANIU  REKORD:   '                    
               DISPLAY QUEUE-6-FIRST-TO-NUMBER                          
               DISPLAY QUEUE-6-FIRST-FROM-NUMBER                        
               DISPLAY QUEUE-6-FLIGHT-TO-DEP-DATE                       
               DISPLAY QUEUE-6-FLIGHT-TO-DEP-TIME                       
               DISPLAY QUEUE-6-FLIGHT-TO-ARV-DATE                       
               DISPLAY QUEUE-6-FLIGHT-TO-ARV-TIME                       
               DISPLAY QUEUE-6-FLIGHT-FROM-DEP-DATE        
               DISPLAY QUEUE-6-FLIGHT-FROM-DEP-TIME                     
               DISPLAY QUEUE-6-FLIGHT-FROM-ARV-DATE                     
               DISPLAY QUEUE-6-FLIGHT-FROM-ARV-TIME                     
               DISPLAY QUEUE-6-DEPARTURE-AIRPORT                        
               DISPLAY QUEUE-6-ARRIVAL-AIRPORT                          
               DISPLAY QUEUE-6-NUMBER-OF-TO-TRANSFERS                   
               DISPLAY QUEUE-6-NUMBER-OF-FR-TRANSFERS                   
               DISPLAY QUEUE-6-TO-FLIGHT-ID(1)                          
               DISPLAY QUEUE-6-TO-FLIGHT-ID(2)                          
               DISPLAY QUEUE-6-TO-FLIGHT-ID(3)                          
               DISPLAY QUEUE-6-TO-FLIGHT-ID(4)                          
               DISPLAY QUEUE-6-TO-FLIGHT-ID(5)                          
               DISPLAY QUEUE-6-TO-FLIGHT-ID(6)                          
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(1)                        
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(2)                        
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(3)                        
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(4)                        
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(5)                        
               DISPLAY QUEUE-6-FROM-FLIGHT-ID(6)                        
      * /TEST                                                           
                                                                        
      * IF WE COULDNT READ THIS RECORD IT IS A STRANGE SITUATION (      
      * IT SHOULDNT HAPPEN  )                                           
             IF EIBRESP = DFHRESP(QIDERR) OR DFHRESP(ITEMERR) THEN      
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'INNER ERROR (NOT A USER MISTAKE ) '  TO            
                                 WS-Z02141-I-ERROR-MESSAGE(1)           
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2304-VALIDATE-DIRECT-2WAY                    
      ******************************************************************
       2304-VALIDATE-DIRECT-2WAY.                                       
           IF WS-ARRAY-FLIGHT-TO(WS-CHOICE-POSITION)  =                 
              CT-EMPTY-FIELD                                            
               PERFORM 2400-INITIALIZE-ERROR-MESSAGE                    
               MOVE 'YOU HAVE TO CHOOSE NOT EMPTY FLIGHT ' TO           
                               WS-Z02141-I-ERROR-MESSAGE(1)             
               SET SO-Z02141-M-WITH TO TRUE                             
               PERFORM 2300-CALL-ERROR-ROUTINE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2305-PREPARE-CHOOSE-2WAY                     
      * PARARGRAPH WILL BE CALLED TO PREAPRE DATA OF 2WAY FLIGHT        
      * TO BEING SENT TO Z02192 PROGRAM                                 
      ******************************************************************
       2305-PREPARE-CHOOSE-2WAY.                                        
           SET SO-Z02192-M-2-WAY TO TRUE                                
           IF SO-SEARCH-FOR-N-DIRECT-RETURN THEN                        
                                                                        
              SET SO-Z02182-M-ONE-WAY TO TRUE                           
              PERFORM 2132-PREPARE-2WAY-N-DIR                           
           ELSE                                                         
              PERFORM 2213-PREPARE-2WAY-DIRECT                          
           END-IF                                                       
           PERFORM 2620-CALL-FOR-CHOSEN-FLIGHT                          
           .                                                            
      ******************************************************************
      *                    2306-PREPARE-DETAILS-2WAY                    
      * PARAGRAPH WILL PREAPRE 2WAY FLIGHT DATA TO BE SENT TO           
      * Z02182 PROGRAM                                                  
      ******************************************************************
       2306-PREPARE-DETAILS-2WAY.                                       
           SET SO-Z02182-M-2-WAY TO TRUE                                
           IF SO-SEARCH-FOR-N-DIRECT-RETURN THEN                        
              SET SO-Z02182-M-2-WAY-N-DIRECT TO TRUE                    
              PERFORM 2132-PREPARE-2WAY-N-DIR                           
           ELSE       
              PERFORM 2213-PREPARE-2WAY-DIRECT                          
           END-IF                                                       
           PERFORM 2610-CALL-FOR-MORE-DETAILS                           
           .                                                            
      ******************************************************************
      *                    2329-CHECK-IF-LINE-EMPTY                     
      ******************************************************************
       2329-CHECK-IF-LINE-EMPTY.                                        
           DISPLAY '2329 PERFORMED (EMPTY LINE)  '                      
      * HERE WE WILL CHECK IF USER PLACED HIS CHOICE NEXT TO EMPTY      
      * OR VALID LINE  (FLIGHT)                                         
           IF WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION) =          
                        CT-EMPTY-FIELD                                  
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'PLEASE CHOOSE NOT EMPTY LINE '                      
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2307-PREPARE-OFFSET                          
      * PARAGRAPH PREPARES TIME OFFSET                                  
      * THIS VALUE WILL BE USER TO MOVE UTC TIMEZONE TO LOCAL AIRPORT   
      * TIME ZONE                                                       
      *                                                                 
      * TIME ZONE IS STORED IN FORMAT SHH.MM WHERE S IS A SIGN          
      * HH IS AMOUNT OF HOURS AND MM IS AMOUNT OF MINUTES THAT          
      * WE NEED TO MOVE TO GET LOCAL TIME ZONE (FROM UTC)               
      *                                                                 
      * PARAGRAPH ALSO NEEDS TO CHECK IF THIS TIMEZONE IS VALID NUMERIC 
      * DATA, IN CASE IT ISN'T USER WILL GET MESSAGE SAYING THAT        
      * THERE WAS DATABASE ERROR                                        
      ******************************************************************
       2307-PREPARE-OFFSET.                                             
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
              MOVE 'INVALID DATA FROM DATABASE' TO                      
                                    WS-Z02141-I-ERROR-MESSAGE(1)        
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2308-MOVE-DATA-TO-RETURN-Q                   
      * PARAGRAPH IS CALLED IN ORDER TO MOVE DATA TO QUEUE THAT WILL    
      * STORE INFORMATIONS ABOUT DIRECT 2WAY(RETURN FLIGHTS)            
      *                                                                 
      * THAT IS WHY WE AER STOREING HERE AN '00' AS TRANSFER NUMBER     
      *                                                                 
      * THIS FLIGHTS WILL ALWAYS HAVE 0 TRANSFERS                       
      ******************************************************************
       2308-MOVE-DATA-TO-RETURN-Q.                                      
           MOVE WS-TO-FLIGHT-ID-TEXT    TO 2WAY-DIR-FLIGHT-TO-ID        
           MOVE WS-FROM-FLIGHT-ID-TEXT  TO 2WAY-DIR-FLIGHT-FROM-ID      
           MOVE WS-TO-FLIGHT-NUMBER-TEXT TO 2WAY-DIR-FLIGHT-TO-NUMBER   
           MOVE WS-FROM-FLIGHT-NUMBER-TEXT TO                           
                                           2WAY-DIR-FLIGHT-FROM-NUMBER  
           MOVE T05-ARRIVAL-AIRPORT-CODE TO 2WAY-DIR-ARRIVAL-AIRPORT    
           MOVE T05-DEPARTURE-AIRPORT-CODE TO 2WAY-DIR-DEPARTURE-AIRPORT
           MOVE WS-FIRST-DEP-DATE TO 2WAY-DIR-FLIGHT-TO-DEP-DATE        
           MOVE WS-FIRST-DEP-TIME TO 2WAY-DIR-FLIGHT-TO-DEP-TIME        
           MOVE WS-FIRST-ARRIVAL-DATE TO 2WAY-DIR-FLIGHT-TO-ARV-DATE    
           MOVE WS-FIRST-ARRIVAL-TIME TO 2WAY-DIR-FLIGHT-TO-ARV-TIME    
                                                                        
           MOVE WS-SECOND-DEPARTURE-DATE TO                             
                                           2WAY-DIR-FLIGHT-FROM-DEP-DATE
           MOVE WS-SECOND-DEPARTURE-TIME TO                             
                                           2WAY-DIR-FLIGHT-FROM-DEP-TIME
           MOVE WS-SECOND-ARRIVAL-DATE TO 2WAY-DIR-FLIGHT-FROM-ARV-DATE 
           MOVE WS-SECOND-ARRIVAL-TIME TO 2WAY-DIR-FLIGHT-FROM-ARV-TIME 
           MOVE 0 TO 2WAY-DIR-NUMBER-OF-TRANSFERS                       
           .                                                            
      ******************************************************************
      *                    2330-DISPLAY-PREVIOUS-2WAY                   
      ******************************************************************
       2330-DISPLAY-PREVIOUS-2WAY.                                      
           IF  SO-SEARCH-FOR-N-DIRECT-RETURN THEN                       
      * DISPLAY PREVIOUS FOUR NON DIRECT RETURN FLIGHTS                 
             PERFORM 2125-DISPLAY-PREV-4-2WAY-N-DIR                     
           ELSE                                                         
      * DISPLAYE PREVIOUS FOUR DIRECT RETURN FLIGHTS                    
             PERFORM 2084-DISPLAY-PREV-4-2WAY-DIR                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2331-DISPLAY-NEXT-2WAY                       
      ******************************************************************
       2331-DISPLAY-NEXT-2WAY.                                          
              IF SO-SEARCH-FOR-N-DIRECT-RETURN THEN                     
      * DISPLAY NEXT 4 NOT DIRECT RETURN FLIGHTS                        
                 PERFORM 2117-DISPLAY-NEXT-4-2WAY-N-DIR                 
              ELSE                                                      
      * DISPLAY NEXT 4  DIRECT RETURN FLIGHTS                           
                 PERFORM 2083-DISPLAY-NEXT-4-2WAY-DIR                   
              END-IF                                                    
           .          
      ******************************************************************
      *                   2333-GET-CHOICE-POSITION                      
      * PARAGRAPH WILL GET POSITION WHERE USER PLACED HIS CHOICE,       
      * PARAGRAPH WILL ALSO COUNT AMOUNT OF CHOICES THAT USER HAS       
      * MADE                                                            
      * VALID NUMBER OF CHOICES IS ONLY (1) OTHER NUMBER WILL RESULT    
      * IN ERROR                                                        
      ******************************************************************
       2333-GET-CHOICE-POSITION.                                        
           DISPLAY '2333 PERFORMED : '                                  
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 15     
                IF CHOICEI(WS-ITER5) = SPACE OR LOW-VALUES THEN         
                  CONTINUE                                              
                ELSE                                                    
                  ADD 1 TO WS-CHOICE-COUNTER                            
                  MOVE CHOICEI(WS-ITER5) TO SW-USER-CHOICE              
                  MOVE WS-ITER5 TO WS-USER-CHOICE-POSITION              
                  DISPLAY 'WS-USER-CHOICE POSITION: '                   
                       WS-USER-CHOICE-POSITION                          
                END-IF                                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2334-CHECK-CHOICE-NUMBER                     
      ******************************************************************
       2334-CHECK-CHOICE-NUMBER.                                        
           DISPLAY '2334  CHECK NUMBER OF CHOICES '                     
           IF WS-CHOICE-COUNTER = 0 THEN                                
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU NEED TO SPECIFY SOMETHING , 1 OR 2'             
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
      * USER CHECKED MORE THAN 1 ROW                                    
           IF WS-CHOICE-COUNTER > 1 THEN   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'YOU CAN ONLY CHOOSE 1 FLIGHT '                      
                           TO WS-Z02141-I-ERROR-MESSAGE(1)              
              SET SO-Z02141-M-WITH TO TRUE                              
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-IF                                                       
           DISPLAY 'PONIZEJ WARTOSC TEGO WYBRANEGO: '                   
           DISPLAY  WS-WHAT-FLIGHT-NUMBER(WS-USER-CHOICE-POSITION)      
           .                                                            
      ******************************************************************
      *                    2335-MOVE-DATA-TO-QUEUE                      
      * PARAGRAPH IS CALLED TO MOVE DATA ABOUT DIRECT 2WAY FLIGHT       
      * INTO THE QUEUE                                                  
      ******************************************************************
       2335-MOVE-DATA-TO-QUEUE.                                         
           MOVE WS-TO-FLIGHT-FREE-SEATS     TO                          
                                             2WAY-DIR-TO-FLIGHT-SEATS   
           MOVE WS-FROM-FLIGHT-FREE-SEATS   TO                          
                                             2WAY-DIR-FROM-FLIGHT-SEATS 
           MOVE WS-TO-FLIGHT-AIRLINE-CODE   TO                          
                                             2WAY-DIR-TO-FLIGHT-AIRLINE 
           MOVE WS-FROM-FLIGHT-AIRLINE-CODE TO                          
                                          2WAY-DIR-FROM-FLIGHT-AIRLINE  
           .                                                            
      ******************************************************************
      *                   2336-SEND-INVALID-KEY-MSG                     
      ******************************************************************
       2336-SEND-INVALID-KEY-MSG.                                       
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'NO ACTION KEY PRESSED ' TO                             
                                        WS-Z02141-I-ERROR-MESSAGE(1)    
           SET SO-Z02141-M-WITH TO TRUE                                 
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                    2400-INITIALIZE-ERROR-MESSAGE   
       ******************************************************************
        2400-INITIALIZE-ERROR-MESSAGE.                                   
            PERFORM VARYING WS-ITER4 FROM 1 BY 1 UNTIL WS-ITER4 > 10     
              MOVE SPACE TO WS-Z02141-I-ERROR-MESSAGE(WS-ITER4)          
            END-PERFORM                                                  
            .                                                            
       ******************************************************************
       *                  2610-CALL-FOR-MORE-DETAILS                     
       * PARAGRAPH WILL CALL PROGRAM THAT WILL DISPLAY MORE DETAILS      
       * ABOUT THE CHOSEN FLIGHT OR FLIGHTS                              
       ******************************************************************
        2610-CALL-FOR-MORE-DETAILS.                                      
            SET SO-M-FIRST-WITHOUT  TO TRUE                              
            DISPLAY 'Z02172 Z610 Z02182-FLIGHT-ID: '                     
                           Z02182-FLIGHT-ID                              
            MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
            EXEC CICS                                                    
              XCTL PROGRAM(CT-DETAIL-PROGRAM-NAME) COMMAREA(DFHCOMMAREA) 
            END-EXEC                                                     
            PERFORM 2200-CHECK-EIBRESP                                   
            .                                                            
       ******************************************************************
       *                   2620-CALL-FOR-CHOSEN-FLIGHT                   
       * PROGRAM WILL CALL TO THE PROGRAM THAT WILL ALLOW USER TO        
       * CHOOSE SEATS ON THIS FLIGHT                                     
       *                                                                 
       ******************************************************************
        2620-CALL-FOR-CHOSEN-FLIGHT.                                     
            SET SO-M-FIRST-WITHOUT    TO TRUE                            
            MOVE 1 TO WS-FLIGHT-COUNTER                                  
            MOVE WS-ZZEC0215 TO DFHCOMMAREA                              
            EXEC CICS                                                    
               XCTL PROGRAM(CT-SEATS-PROGRAM-NAME) COMMAREA(DFHCOMMAREA) 
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
              EXEC CICS                                                 
               RETURN TRANSID('0211') COMMAREA(DFHCOMMAREA)             
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN SO-FINAL-TERMINATION                                    
              SET SO-M-FIRST-WITHOUT TO TRUE                            
              MOVE WS-ZZEC0215 TO DFHCOMMAREA                           
              EXEC CICS                                                 
                XCTL PROGRAM(CT-CALLING-PROGRAM-NAME)                   
                 COMMAREA(DFHCOMMAREA)                                  
              END-EXEC                                                  
              PERFORM 2200-CHECK-EIBRESP                                
           WHEN OTHER                                                   
              PERFORM 2400-INITIALIZE-ERROR-MESSAGE                     
              MOVE 'SERIOUS ERROR ' TO  WS-Z02141-I-ERROR-MESSAGE(1)    
              SET    SO-Z02141-M-WITH TO TRUE                           
              PERFORM 2300-CALL-ERROR-ROUTINE                           
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     3001-SEND-INVALID-CALL-MSG                  
      ******************************************************************
       3001-SEND-INVALID-CALL-MSG.                                      
           PERFORM 2400-INITIALIZE-ERROR-MESSAGE                        
           MOVE 'INVALID CALL' TO WS-Z02141-I-ERROR-MESSAGE(1)          
           SET SO-Z02141-M-WITHOUT TO TRUE                              
           PERFORM 2300-CALL-ERROR-ROUTINE                              
           .                                                            
      ******************************************************************
      *                     7001-OPEN-DIR-ONEWAY-CURSOR                 
      ******************************************************************
       7001-OPEN-DIR-ONEWAY-CURSOR.                                     
           DISPLAY 'BEFORE OPEN 2101 '                                  
           DISPLAY 'DEP ARIRPOT: ' T05-DEPARTURE-AIRPORT-CODE           
           DISPLAY 'ARV AIRPORT: ' T05-ARRIVAL-AIRPORT-CODE             
           DISPLAY 'DEP DATE: ' WS-DEPARTURE-DATE                       
           EXEC SQL                                                     
            OPEN C-DIRECT-ONE-WAY-CURSOR                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7001-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7002-FETCH-FLIGHTS-TO-QUEUE                   
      * PARAGRAPH FETCHES DIRECT ONEWAY FLIGHTS                         
      *                                                                 
      * PARAGRAPH WILL FETCH FLIGHT DATA, THEN IT WILL                  
      * MOVE THEIR DEPARTURE AND ARRIVAL TIMESTAMP TO LOCAL TIMEZONE    
      * OF THE AIRPORT (IN DATABASE THOSE TIMESTAMP ARE STORED IN       
      * UTC TIMEZONE )                                                  
      * AND AFTER THAT THIS MODIFIED RECORD WILL BE PUT IN TO THE QUEUE 
      *                                                                 
      *  IN 7006 PARAGRAPH THERE IS LOGIC THAT WILL DETERMINE I F       
      * THERE IS ENOUGH SEATS FOR USER NEEDS                            
      *                                                                 
      * (USER PROVIDED DATA ABOUT HOW MANY SEATS HE NEEDS IN            
      * Z02152 PROGRAM)                                                 
      ******************************************************************
       7002-FETCH-FLIGHTS-TO-QUEUE.                                     
           PERFORM 7006-FETCH-DIRECT-CURSOR-1WAY                        
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
               IF SO-THERE-ARE-FREE-SEATS THEN   
                 PERFORM 7004-PREPARE-TIMES                             
                                                                        
                 PERFORM 2020-WRITE-DIRECT-ONE-WAY                      
               END-IF                                                   
               PERFORM 7006-FETCH-DIRECT-CURSOR-1WAY                    
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     7003-CLOSE-DIR-ONEWAY-CURSOR                
      ******************************************************************
       7003-CLOSE-DIR-ONEWAY-CURSOR.                                    
           EXEC SQL                                                     
            CLOSE C-DIRECT-ONE-WAY-CURSOR                               
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7003-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                        7004-PREPARE-TIMES                       
      * THIS PARAGRAPH WILL MOVE TIMESTAMPS TAKEN FROM DATABASE         
      * FROM UTC TIMEZONE TO LOCAL TIME ZONE FOR THE AIRPORT            
      *                                                                 
      * TIME-ZONE2 IS IN FORMAT SHH.MM                                  
      ******************************************************************
       7004-PREPARE-TIMES.                                              
           DISPLAY '7004--------------------START-----------'           
           DISPLAY 'STREFA DLA KRAJU   : '  T05-DEPARTURE-AIRPORT-CODE  
           MOVE    T05-DEPARTURE-AIRPORT-CODE TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7005-PREPARE-THE-ORG-TIME                            
           MOVE    WS-MODIFIED-TIMESTAMP    TO QUEUE-DEPARTURE-TIMESTAMP
           DISPLAY 'DEP TIMESTAMP PO: '  QUEUE-DEPARTURE-TIMESTAMP      
                                                                        
           DISPLAY 'MOD TIMESTAMP  DEPARTURE '                          
                                         WS-MODIFIED-TIMESTAMP          
                                                                        
           MOVE    T05-ARRIVAL-AIRPORT-CODE   TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7012-PREPARE-THE-DES-TIME                            
           MOVE    WS-MODIFIED-TIMESTAMP      TO QUEUE-ARRIVAL-TIMESTAMP
           DISPLAY 'MOD TIMESTAMP  ARIVAL    '                          
                                         WS-MODIFIED-TIMESTAMP          
                                                                        
           DISPLAY '7004--------------------END-----------'             
           .                                                            
      ******************************************************************
      *                    7005-PREPARE-THE-ORG-TIME.                   
      * TIME-ZONE2 IS ALWAYS IN FORMAT SHH.MM                           
      * FOR EXAMPLE -02.15   - 2 HOURS AND 15 MINUTES                   
      *  OR +04.55  -> PLUS 4 HOURS AND 55 MINUTES                      
      ******************************************************************
       7005-PREPARE-THE-ORG-TIME.                                       
           INITIALIZE WS-HOUR-OFFSET-TEMP                               
           INITIALIZE WS-MINUTE-OFFSET-TEMP                             
           PERFORM 2307-PREPARE-OFFSET                                  
           INITIALIZE WS-MODIFIED-TIMESTAMP                             
                                                                        
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
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7005-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7012-PREPARE-THE-DES-TIME                    
      ******************************************************************
       7012-PREPARE-THE-DES-TIME.                                       
           INITIALIZE WS-HOUR-OFFSET-TEMP                               
           INITIALIZE WS-MINUTE-OFFSET-TEMP                             
           PERFORM 2307-PREPARE-OFFSET                                  
           INITIALIZE WS-MODIFIED-TIMESTAMP                             
                                                                        
           EXEC SQL                                                     
            SELECT CHAR(TIMESTAMPADD(4,:WS-MINUTE-OFFSET,ZMIENNA))      
            INTO :WS-MODIFIED-TIMESTAMP                                 
            FROM                                                        
            (SELECT TIMESTAMPADD(8,:WS-HOUR-OFFSET,                     
              ARRIVAL_TIMESTAMP)                                        
              AS  ZMIENNA                                               
            FROM  T05_FLIGHT_TABLE                                      
            WHERE FLIGHT_ID = :T05-FLIGHT-ID)                           
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
              SET SO-7012-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7006-FETCH-DIRECT-CURSOR-1WAY                 
      * PARAGRAPH WILL FETCH DATA FOR THE CURSOR                        
      * AND WILL VALIDATE IF THERE IS ENOUGH SEATS ON THE FLIGHT    
      ******************************************************************
       7006-FETCH-DIRECT-CURSOR-1WAY.                                   
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-FLIGHT-NUMBER                                 
           INITIALIZE T05-DEPARTURE-AIRPORT-CODE                        
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE T05-AIRLINE-CODE                                  
           INITIALIZE WS-AMOUNT-OF-FREE-SEATS                           
           EXEC SQL                                                     
             FETCH C-DIRECT-ONE-WAY-CURSOR                              
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-FLIGHT-NUMBER,                                        
             :T05-DEPARTURE-AIRPORT-CODE,                               
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :T05-AIRLINE-CODE,                                         
             :WS-AMOUNT-OF-FREE-SEATS                                   
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
      * HERE PROGRAM WILL CHECK IF THERE IS ENOUGH SEATS ON THIS PLANE  
              PERFORM 7013-CHECK-FOR-SEATS                              
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-CURSOR-DATA TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7006-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                7010-FETCH-TIMEZONE                              
      ******************************************************************
       7010-FETCH-TIMEZONE.                                             
           INITIALIZE T02-TIME-ZONE2                                    
           DISPLAY '7010 -> FETCH TIMEZONE'                             
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
                                                                        
             SET SO-7010-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7013-CHECK-FOR-SEATS                         
      ******************************************************************
       7013-CHECK-FOR-SEATS.                                            
           MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-NUMERIC              
                                                                        
           IF WS-AMOUNT-OF-FREE-SEATS >= WS-TEMP-NUMERIC                
           THEN                                                         
               MOVE WS-AMOUNT-OF-FREE-SEATS TO ONE-WAY-Q-FREE-SEATS     
                                                                        
               SET SO-THERE-ARE-FREE-SEATS  TO TRUE                     
           ELSE                                                         
               SET SO-THERE-IS-NO-FREE-SEAT TO TRUE                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7045-OPEN-DIRECT-2WAY-CURSOR   
      * PARAGRAPH WILL OPEN CURSOR USED TO FIND ALL DIRECT              
      * 2 WAY FLIGHT THAT MEETS CRITERIA                                
      ******************************************************************
       7045-OPEN-DIRECT-2WAY-CURSOR.                                    
           EXEC SQL                                                     
            OPEN C-DIRECT-2WAY-CURSOR                                   
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7045-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7046-DIRECT-2WAY-TO-QUEUE                      
      ******************************************************************
       7046-DIRECT-2WAY-TO-QUEUE.                                       
           PERFORM 7048-FETCH-DIR-2WAY-CURSOR                           
                                                                        
           PERFORM UNTIL SO-END-OF-CURSOR-DATA                          
               IF SO-THERE-ARE-FREE-SEATS THEN                          
                 PERFORM 7050-PREPARE-2WAY-TIMESTAMPS                   
                 PERFORM 7049-VALIDATE-PREPARED-DATES                   
                 IF SO-DATE-IS-CORRECT THEN                             
                   PERFORM 2046-WRITE-RETURN-DIRECT-FLG                 
                 END-IF                                                 
               END-IF                                                   
                 PERFORM 7048-FETCH-DIR-2WAY-CURSOR                     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     7047-CLOSE-DIRECT-2WAY-CUR                  
      * PARAGRAPH WILL CLOSE CURSOR USED TO FIND ALL DIRECT             
      * 2 WAY FLIGHT THAT MEETS CRITERIA                                
      ******************************************************************
       7047-CLOSE-DIRECT-2WAY-CUR.        
           EXEC SQL                                                     
            CLOSE C-DIRECT-2WAY-CURSOR                                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7047-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7048-FETCH-DIR-2WAY-CURSOR                      
      * PARAGRAPH WILL FETCH DATA ABOUT DIRECT 2 WAY FLIGHT             
      *                                                                 
      * PARAGRAPH WILL ALSO CHECK IF ON THE GIVEN FLIGHT THERE IS       
      * ENOUGH SEATS FOR THE USER                                       
      ******************************************************************
       7048-FETCH-DIR-2WAY-CURSOR.                                      
           SET SO-NOT-END-OF-CURSOR-DATA TO TRUE                        
           INITIALIZE WS-AMOUNT-OF-FREE-SEATS                           
           INITIALIZE  T05-FLIGHT-ID                                    
           INITIALIZE  T05-FLIGHT-NUMBER                                
           INITIALIZE WS-TO-FLIGHT-ID                                   
           INITIALIZE WS-TO-FLIGHT-NUMBER                               
           INITIALIZE WS-FROM-FLIGHT-ID                                 
           INITIALIZE WS-FROM-FLIGHT-NUMBER                             
           INITIALIZE WS-TO-FLIGHT-FREE-SEATS                           
           INITIALIZE WS-FROM-FLIGHT-FREE-SEATS                         
           EXEC SQL                                                     
           FETCH C-DIRECT-2WAY-CURSOR                                   
           INTO                                                         
              :WS-TO-FLIGHT-ID,                                         
              :WS-TO-FLIGHT-NUMBER,                                     
              :WS-FROM-FLIGHT-ID,                                       
              :WS-FROM-FLIGHT-NUMBER,                                   
              :WS-TO-FLIGHT-FREE-SEATS,                                 
              :WS-FROM-FLIGHT-FREE-SEATS,    
              :WS-TO-FLIGHT-AIRLINE-CODE,                               
              :WS-FROM-FLIGHT-AIRLINE-CODE                              
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
      * IN CASE THAT WE GET VALUES SUCCESSFULLY                         
      * PROGRAM WILL CHECK IF THERE IS ENOUGH PLACES ON THE BOTH        
      * FLIGHTS                                                         
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-NUMERIC           
                                                                        
              IF WS-TO-FLIGHT-FREE-SEATS >= WS-TEMP-NUMERIC             
              THEN                                                      
                 SET SO-THERE-ARE-FREE-SEATS TO TRUE                    
                 IF WS-FROM-FLIGHT-FREE-SEATS >= WS-TEMP-NUMERIC        
                 THEN                                                   
                    PERFORM 2335-MOVE-DATA-TO-QUEUE                     
                    SET SO-THERE-ARE-FREE-SEATS TO TRUE                 
                 ELSE                                                   
                    SET SO-THERE-IS-NO-FREE-SEAT TO TRUE                
                 END-IF                                                 
              ELSE                                                      
                 SET SO-THERE-IS-NO-FREE-SEAT TO TRUE                   
              END-IF                                                    
                                                                        
           WHEN SO-SQLCODE-NOT-FOUND                                    
               SET SO-END-OF-CURSOR-DATA TO TRUE                        
           WHEN OTHER                                                   
               SET SO-7048-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       7049-VALIDATE-PREPARED-DATES              
      * THIS PARAGRAPH WILL CHECK IF DATE OF DEPARTURE AND DATE OF      
      * ARRIVAL OF 2WAY FLIGHT IS THE SAME AS THE DATES PROVIDED BY     
      * THE USER                                                        
      *                                                                 
      * THIS CHECK IS MADE HERE BECAUSE WE HAD TO MOVE TIMES(TIMESTAMPS)
      * OF DEPARTURE AND ARRIVAL TO LOCAL AIRPORT TIMES                 
      * AND NOW WE CAN CHECK IF THOSE VALUES ARE THE SAME AS            
      * THE ONES PROVIDED BY THE USER                                   
      *                                                                 
      * IF THIS CHECK WOULD BE BEFORE THEN WE WOULD HAVE INVALID DATA   
      ******************************************************************
       7049-VALIDATE-PREPARED-DATES.                                    
           IF WS-FIRST-DEP-DATE = WS-DEPARTURE-DATE AND                 
              WS-SECOND-ARRIVAL-DATE = WS-ARRIVAL-DATE  THEN            
              SET SO-DATE-IS-CORRECT TO TRUE                            
           ELSE                                                         
              SET SO-DATE-IS-INVALID TO TRUE                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7050-PREPARE-2WAY-TIMESTAMPS                   
      * PARAGRAPH IS CALLED ONLY WHEN PROGRAM NEEDS TO PREPARE          
      * TIMESTAMP FOR THE 2WAY FLIGHTS                                  
      *                                                                 
      * PROGRAM WILL MOVE UTC TIME TO LOCAL AIRPORTS TIME ZONE          
      * WE HAVE 4 TIMESTAMPS TO PREPARE HERE                            
      * 1. TIME OF FIRST  DEPARTURE                                     
      * 2. TIME OF FIRST  ARRIVAL                                       
      * 3. TIME OF SECOND DEPARTURE                                     
      * 4. TIME OF SECOND ARRIVAL                                       
      *                                                                 
      * AT THE END OF THIS PARAGRAPH WE WILL HAVE 4 MODIFIED TIMESTAMPS 
      * IN PROGRAM VARIABLES                                            
      ******************************************************************
       7050-PREPARE-2WAY-TIMESTAMPS.                                    
           MOVE    T05-DEPARTURE-AIRPORT-CODE TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                   
                                                                        
      * HERE WE WILL GET TIME OF THE FIRST DEPARTURE                    
           MOVE WS-TO-FLIGHT-ID-TEXT TO T05-FLIGHT-ID-TEXT              
           COMPUTE T05-FLIGHT-ID-LEN =                                  
                 FUNCTION LENGTH(WS-TO-FLIGHT-ID-TEXT)                  
                                                                        
           PERFORM 7005-PREPARE-THE-ORG-TIME                            
           MOVE  WS-MODIFIED-TIMESTAMP TO WS-FIRST-DEPARTURE-TIMESTAMP  
                                                                        
      * HERE WE WILL GET MODIFIED TIMESTAMP OF THE SECOND ARRIVAL       
      * IT MEANS ( THE TIME WE WILL ARIVE TO ORIGIN AIRPORT )           
      * TIME WILL BE GIVEN IN THE LOCAL AIRPOT TIME                     
           MOVE WS-FROM-FLIGHT-ID-TEXT TO T05-FLIGHT-ID-TEXT            
           COMPUTE T05-FLIGHT-ID-LEN =                                  
                 FUNCTION LENGTH(WS-FROM-FLIGHT-ID-TEXT)                
                                                                        
           PERFORM 7012-PREPARE-THE-DES-TIME                            
           MOVE  WS-MODIFIED-TIMESTAMP TO WS-SEC-ARRIVAL-TIMESTAMP      
                                                                        
                                                                        
      * HERE WE WILL GET FIRST ARRIVAL TIME                             
           MOVE    T05-ARRIVAL-AIRPORT-CODE TO T02-AIRPORT-CODE         
           PERFORM 7010-FETCH-TIMEZONE                                  
                                                                        
           MOVE WS-TO-FLIGHT-ID-TEXT TO T05-FLIGHT-ID-TEXT              
           COMPUTE T05-FLIGHT-ID-LEN =                                  
                 FUNCTION LENGTH(WS-TO-FLIGHT-ID-TEXT)                  
           PERFORM 7012-PREPARE-THE-DES-TIME                            
           MOVE  WS-MODIFIED-TIMESTAMP TO WS-FIRST-ARRIVAL-TIMESTAMP    
                                                                        
      * AND FINALLY HERE WE WILL GET SECOND DEPARTURE TIME              
      * IT MEANS THE TIME WE WILL DEPARTURE FROM DESTINATION AIRPORT    
                                                                        
           MOVE WS-FROM-FLIGHT-ID-TEXT TO T05-FLIGHT-ID-TEXT            
           COMPUTE T05-FLIGHT-ID-LEN =                                  
                 FUNCTION LENGTH(WS-FROM-FLIGHT-ID-TEXT)        
           PERFORM 7005-PREPARE-THE-ORG-TIME                            
           MOVE  WS-MODIFIED-TIMESTAMP TO WS-SEC-DEARTURE-TIMESTAMP     
           .                                                            
      ******************************************************************
      *                       7051-CHECK-TIMES                          
      ******************************************************************
       7051-CHECK-TIMES.                                                
           IF WS-Z02172-RETURN-DATE =  ONE-WAY-Q-ARV-DATE               
           AND WS-Z02172-DEPARTURE-DATE =  ONE-WAY-Q-DATE-DEP           
           THEN                                                         
               SET SO-DATE-IS-CORRECT TO TRUE                           
           ELSE                                                         
               SET SO-DATE-IS-INVALID TO TRUE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7201-OPEN-C-0-TRANSFER                     
      ******************************************************************
       7201-OPEN-C-0-TRANSFER.                                          
           EXEC SQL                                                     
             OPEN C-FIND-0-TRANSFER                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7201-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7202-FETCH-C-0-TRANSFER                     
      *                                                                 
      * HERE PROGRAM WILL BE SEARCHING FOR DIRECT FLIGHTS               
      * 0 TRANSFERS -> IF THERE ARE SUCH FLIGHTS THEN                   
      * WE WILL WRITE THEM INTO THE QUEU                                
      * IF NOW PROGRAM WILL LOOK FOR FLIGHTS WITH 1 TRANSFER            
      *                                                           
      *                                                                 
      * VARIABLE WS-WHICH-FLIGHT-IN-TRANSFERS STORES INFORMATION        
      * ABOUT HOW MANY FLIGHTS THERE ARE IN THIS NUMBER OF TRANSFERS    
      * NUMBER OF TRANSFERS + 1                                         
      *                                                                 
      *                                                                 
      * IN CASE FLIGHT WILL BE FOUND HERE                               
      * WE WON'T CHECK IF DISTANCE IS OK                                
      * BECAUSE THIS IS DIRECT FLIGHT                                   
      * IN CASE THIS FLIGHT ISN'T DIRECT                                
      * WE WILL CALCULATE FIRST DISTANCE                                
      ******************************************************************
       7202-FETCH-C-0-TRANSFER.                                         
           PERFORM 7204-FETCH-C-0-TRANSFER                              
           PERFORM UNTIL SO-END-OF-C-NAME1                              
           MOVE 1 TO WS-WHICH-FLIGHT-IN-TRANSFERS                       
      * IF AMOUNT OF SEATS IS CORRECT                                   
              IF SO-CONTINUE-WITH-ROW THEN                              
                PERFORM 2202-CHECK-IF-DEST-FINAL                        
                                                                        
                  IF SO-THIS-IS-FINAL-FLIGHT THEN                       
      * WE HAVE FOUND THE DIRECT FLIGHT                                 
      *  RECORD WILL BE WRITED TO QUEUE                                 
                     PERFORM 7004-PREPARE-TIMES                         
                     IF SO-SEARCH-FOR-N-DIRECT-ONEWAY THEN              
                       PERFORM 2205-CHECK-DEPARTURE-DATE                
                       IF SO-DEP-DATE-VALID  THEN                       
                         PERFORM 2207-INITIALIZE-SUBFLIGHTS             
                         PERFORM 2203-WRITE-ONE-WAY-QUEUE               
                       END-IF                                           
                     ELSE                                               
                       PERFORM 2206-CHECK-ARRIVAL-DATE                  
                       IF SO-ARV-DATE-VALID  THEN                       
                         PERFORM 2207-INITIALIZE-SUBFLIGHTS             
                         PERFORM 2203-WRITE-ONE-WAY-QUEUE               
                       END-IF     
                     END-IF                                             
                  ELSE                                                  
      * IF WE DIDN'T FIND THE DIRECT FLIGHT WE ARE GONNA                
      * SEARCH FOR TRANSFER FLIGHTS                                     
                      PERFORM 7004-PREPARE-TIMES                        
                      PERFORM 2205-CHECK-DEPARTURE-DATE                 
                      IF  SO-DEP-DATE-VALID THEN                        
                      MOVE T05-ARRIVAL-AIRPORT-CODE TO WS-ORIGIN-AIRPORT
                        MOVE T05-DEPARTURE-AIRPORT-CODE  TO             
                                     WS-DESTINATION-AIRPORT             
                        PERFORM 2208-CALCULATE-DISTANCE                 
                        MOVE WS-CALCULATED-DISTANCE TO WS-DISTANCE(     
                                WS-WHICH-FLIGHT-IN-TRANSFERS)           
                        PERFORM 7205-OPEN-C-1-TRANSFER                  
                        PERFORM 7206-1-TRANSFER-TO-QUEUE                
                        PERFORM 7207-CLOSE-C-1-TRANSFER                 
                      END-IF                                            
                  END-IF                                                
              END-IF                                                    
              PERFORM 7204-FETCH-C-0-TRANSFER                           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      7203-CLOSE-C-0-TRANSFER                    
      ******************************************************************
       7203-CLOSE-C-0-TRANSFER.                                         
           EXEC SQL                                                     
             CLOSE C-FIND-0-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7201-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7204-FETCH-C-0-TRANSFER                     
      ******************************************************************
       7204-FETCH-C-0-TRANSFER.                                         
           DISPLAY 'FETCH C-NAME 1'                                     
           MOVE 0 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-1  TO TRUE                          
           INITIALIZE WS-ONE-WAY-Q-STRUCTURE                            
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-FLIGHT-NUMBER                                 
           INITIALIZE T05-DEPARTURE-TIMESTAMP                           
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-DEPARTURE-AIRPORT-CODE                        
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE T05-AIRLINE-CODE                                  
           INITIALIZE WS-NUM-OF-FREE-SEATS                              
           EXEC SQL                                                     
            FETCH C-FIND-0-TRANSFER                                     
             INTO                                                       
              :T05-FLIGHT-ID,                                           
              :T05-FLIGHT-NUMBER,                                       
              :T05-DEPARTURE-TIMESTAMP,                                 
              :T05-ARRIVAL-TIMESTAMP,                                   
              :T05-DEPARTURE-AIRPORT-CODE,                              
              :T05-ARRIVAL-AIRPORT-CODE,                                
              :T05-AIRLINE-CODE,                                        
              :WS-NUM-OF-FREE-SEATS                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           MOVE SQLCODE TO WS-SQLCODE-FORMAT                            
           DISPLAY 'SQLCODE: ' WS-SQLCODE-FORMAT                        
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY 'FETCH NORMAL '                                   
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  >= WS-TEMP-TICKET-NUMBER         
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO   
                           WS-LAST-DESTINATION-AIRPORT                  
                 SET SO-CONTINUE-WITH-ROW TO TRUE                       
                 PERFORM 2201-MOVE-FETCHED-TO-QEUEU                     
              ELSE                                                      
                 DISPLAY 'SO SKIP THE ROW     '                         
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY 'FETCH END'                                       
              SET SO-END-OF-C-NAME1 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7204-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     7205-OPEN-C-1-TRANSFER                      
      ******************************************************************
       7205-OPEN-C-1-TRANSFER.                                          
           MOVE T05-ARRIVAL-AIRPORT-CODE TO  WS-LAST-DESTINATION-AIRPORT
           EXEC SQL                                                     
             OPEN C-FIND-1-TRANSFER                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7205-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7206-1-TRANSFER-TO-QUEUE                     
      * WS-WHICH-FLIGHT-IN-TRANSFERS STORES NUMBER OF FLIGHTS           
      * IN TRANSFER FLIGHT                                              
      *                                                                 
      ******************************************************************
       7206-1-TRANSFER-TO-QUEUE.                                        
           PERFORM 7208-FETCH-1-TRANSFER                                
           PERFORM UNTIL SO-END-OF-C-NAME2                              
              MOVE 2 TO WS-WHICH-FLIGHT-IN-TRANSFERS                    
              PERFORM 2089-VALIDATE-THE-FLIGHT                          
              IF SO-SEARCH-NEXT-TRANSFER THEN                           
               PERFORM 7209-OPEN-C-2-TRANSFER                           
               PERFORM 7210-C-2-TRANSFER-TO-QUEUE                       
               PERFORM 7211-CLOSE-C-2-TRANSFER                          
              END-IF                                                    
                                                                        
             PERFORM 7208-FETCH-1-TRANSFER                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     7207-CLOSE-C-1-TRANSFER                     
      ******************************************************************
       7207-CLOSE-C-1-TRANSFER.                                         
           EXEC SQL                                                     
             CLOSE C-FIND-1-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7207-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7208-FETCH-1-TRANSFER                      
      ******************************************************************
       7208-FETCH-1-TRANSFER.                                           
           MOVE 1 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-2 TO TRUE                           
           INITIALIZE  T05-FLIGHT-ID                                    
           INITIALIZE  T05-ARRIVAL-TIMESTAMP                            
           INITIALIZE  T05-ARRIVAL-AIRPORT-CODE                         
           INITIALIZE  WS-NUM-OF-FREE-SEATS  
           EXEC SQL                                                     
             FETCH C-FIND-1-TRANSFER                                    
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-ARRIVAL-TIMESTAMP,                                    
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :WS-NUM-OF-FREE-SEATS,                                     
             :T05-DEPARTURE-AIRPORT-CODE                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  > WS-TEMP-TICKET-NUMBER          
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO                       
                           WS-LAST-DESTINATION-AIRPORT                  
                SET SO-CONTINUE-WITH-ROW TO TRUE                        
                PERFORM 2204-MOVE-FETCHED-TO-QEUEU                      
              ELSE                                                      
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-C-NAME2 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7208-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7209-OPEN-C-2-TRANSFER                     
      ******************************************************************
       7209-OPEN-C-2-TRANSFER.                                          
           MOVE T05-ARRIVAL-AIRPORT-CODE TO  WS-LAST-DESTINATION-AIRPORT
           EXEC SQL                                                     
             OPEN C-FIND-2-TRANSFER                                     
           END-EXEC  
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7209-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                       7210-C-2-TRANSFER-TO-QUEUE                
      ******************************************************************
       7210-C-2-TRANSFER-TO-QUEUE.                                      
           PERFORM 7212-FETCH-C-2-TRANSFER                              
           PERFORM UNTIL SO-END-OF-C-NAME3                              
              MOVE 3 TO WS-WHICH-FLIGHT-IN-TRANSFERS                    
              PERFORM 2089-VALIDATE-THE-FLIGHT                          
              IF SO-SEARCH-NEXT-TRANSFER THEN                           
                   PERFORM 7213-OPEN-C-3-TRANSFER                       
                   PERFORM 7214-3-TRANSFER-TO-QUEUE                     
                   PERFORM 7215-CLOSE-C-3-TRANSFER                      
              END-IF                                                    
             PERFORM 7212-FETCH-C-2-TRANSFER                            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     7211-CLOSE-C-2-TRANSFER                     
      ******************************************************************
       7211-CLOSE-C-2-TRANSFER.                                         
           EXEC SQL                                                     
             CLOSE C-FIND-2-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7211-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7212-FETCH-C-2-TRANSFER                       
      ******************************************************************
       7212-FETCH-C-2-TRANSFER.                                         
           MOVE 2 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-3 TO TRUE                           
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE WS-NUM-OF-FREE-SEATS                              
           EXEC SQL                                                     
             FETCH C-FIND-2-TRANSFER                                    
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-ARRIVAL-TIMESTAMP,                                    
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :WS-NUM-OF-FREE-SEATS,                                     
             :T05-DEPARTURE-AIRPORT-CODE                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  > WS-TEMP-TICKET-NUMBER          
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO                       
                           WS-LAST-DESTINATION-AIRPORT                  
                SET SO-CONTINUE-WITH-ROW TO TRUE                        
                PERFORM 2204-MOVE-FETCHED-TO-QEUEU                      
              ELSE                                                      
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-C-NAME3 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7212-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE 
           .                                                            
      ******************************************************************
      *                      7213-OPEN-C-3-TRANSFER                     
      ******************************************************************
       7213-OPEN-C-3-TRANSFER.                                          
           MOVE T05-ARRIVAL-AIRPORT-CODE TO  WS-LAST-DESTINATION-AIRPORT
           EXEC SQL                                                     
             OPEN C-FIND-3-TRANSFER                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7213-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                          7214-3-TRANSFER-TO-QUEUE               
      ******************************************************************
       7214-3-TRANSFER-TO-QUEUE.                                        
           PERFORM 7216-FETCH-C-3-TRANSFER                              
           PERFORM UNTIL SO-END-OF-C-NAME4                              
              MOVE 4 TO WS-WHICH-FLIGHT-IN-TRANSFERS                    
              PERFORM 2089-VALIDATE-THE-FLIGHT                          
              IF SO-SEARCH-NEXT-TRANSFER THEN                           
                  PERFORM 7217-OPEN-C-4-TRANSFER                        
                  PERFORM 7218-4-TRANSFER-TO-QUEUE                      
                  PERFORM 7219-CLOSE-C-4-TRANSFER                       
              END-IF                                                    
             PERFORM 7216-FETCH-C-3-TRANSFER                            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      7215-CLOSE-C-3-TRANSFER                    
      ******************************************************************
       7215-CLOSE-C-3-TRANSFER.                                         
           EXEC SQL            
             CLOSE C-FIND-3-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7215-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7216-FETCH-C-3-TRANSFER                       
      ******************************************************************
       7216-FETCH-C-3-TRANSFER.                                         
           MOVE 3 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-4 TO TRUE                           
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE WS-NUM-OF-FREE-SEATS                              
           EXEC SQL                                                     
             FETCH C-FIND-3-TRANSFER                                    
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-ARRIVAL-TIMESTAMP,                                    
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :WS-NUM-OF-FREE-SEATS,                                     
             :T05-DEPARTURE-AIRPORT-CODE                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  > WS-TEMP-TICKET-NUMBER          
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO                       
                           WS-LAST-DESTINATION-AIRPORT                  
                SET SO-CONTINUE-WITH-ROW TO TRUE                        
                PERFORM 2204-MOVE-FETCHED-TO-QEUEU                      
              ELSE                                                      
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-C-NAME4 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7216-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7217-OPEN-C-4-TRANSFER                     
      ******************************************************************
       7217-OPEN-C-4-TRANSFER.                                          
           MOVE T05-ARRIVAL-AIRPORT-CODE TO  WS-LAST-DESTINATION-AIRPORT
           EXEC SQL                                                     
             OPEN C-FIND-4-TRANSFER                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7217-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  7218-4-TRANSFER-TO-QUEUE                       
      * PARAGRAPH HANDLES LOGIC OF FINDING AND VALIDATING 4 TRAANSFER   
      * FLIGHT                                                          
      ******************************************************************
       7218-4-TRANSFER-TO-QUEUE.                                        
           PERFORM 7220-FETCH-C-4-TRANSFER                              
           PERFORM UNTIL SO-END-OF-C-NAME5                              
              MOVE 5 TO WS-WHICH-FLIGHT-IN-TRANSFERS                    
              PERFORM 2089-VALIDATE-THE-FLIGHT                          
              IF SO-SEARCH-NEXT-TRANSFER THEN                           
                  PERFORM 7221-OPEN-C-5-TRANSFER     
                  PERFORM 7222-5-TRANSFER-TO-QUEUE                      
                  PERFORM 7223-CLOSE-C-5-TRANSFER                       
              END-IF                                                    
             PERFORM 7220-FETCH-C-4-TRANSFER                            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      7219-CLOSE-C-4-TRANSFER                    
      ******************************************************************
       7219-CLOSE-C-4-TRANSFER.                                         
           EXEC SQL                                                     
             CLOSE C-FIND-4-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7219-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7220-FETCH-C-4-TRANSFER                       
      ******************************************************************
       7220-FETCH-C-4-TRANSFER.                                         
           MOVE 4 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-5 TO TRUE                           
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE WS-NUM-OF-FREE-SEATS                              
           EXEC SQL                                                     
             FETCH C-FIND-4-TRANSFER                                    
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-ARRIVAL-TIMESTAMP,                                    
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :WS-NUM-OF-FREE-SEATS, 
             :T05-DEPARTURE-AIRPORT-CODE                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  > WS-TEMP-TICKET-NUMBER          
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO                       
                           WS-LAST-DESTINATION-AIRPORT                  
                SET SO-CONTINUE-WITH-ROW TO TRUE                        
                PERFORM 2204-MOVE-FETCHED-TO-QEUEU                      
              ELSE                                                      
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-C-NAME5 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7220-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7221-OPEN-C-5-TRANSFER                     
      ******************************************************************
       7221-OPEN-C-5-TRANSFER.                                          
           MOVE T05-ARRIVAL-AIRPORT-CODE TO  WS-LAST-DESTINATION-AIRPORT
           EXEC SQL                                                     
             OPEN C-FIND-5-TRANSFER                                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7221-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7222-5-TRANSFER-TO-QUEUE                      
      * PROGRAM WILL FETCH ALL FLIGHTS THAT CAN HAVE UP TO 5 TRANSFERS  
      * AND WILL PUT THEM IN A QUEUE                                    
      ******************************************************************
       7222-5-TRANSFER-TO-QUEUE.                                        
           PERFORM 7224-FETCH-C-5-TRANSFER                              
           PERFORM UNTIL SO-END-OF-C-NAME6                              
            MOVE 6 TO WS-WHICH-FLIGHT-IN-TRANSFERS                      
              IF SO-CONTINUE-WITH-ROW THEN                              
                 MOVE T05-ARRIVAL-AIRPORT-CODE TO WS-DESTINATION-AIRPORT
                  MOVE T05-DEPARTURE-AIRPORT-CODE TO WS-ORIGIN-AIRPORT  
                  PERFORM 2208-CALCULATE-DISTANCE                       
                  MOVE WS-CALCULATED-DISTANCE TO                        
                               WS-DISTANCE(WS-WHICH-FLIGHT-IN-TRANSFERS)
                  PERFORM 7230-PREPARE-ARV-TIME                         
                  PERFORM 2099-CALCULATE-SUM-OF-DIST                    
                    IF WS-SUM-OF-DISTANCES > WS-MAXIMAL-DISTANCE THEN   
                           CONTINUE                                     
                    ELSE                                                
                            PERFORM 2203-WRITE-ONE-WAY-QUEUE            
                   END-IF                                               
              END-IF                                                    
             PERFORM 7224-FETCH-C-5-TRANSFER                            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      7223-CLOSE-C-5-TRANSFER                    
      ******************************************************************
       7223-CLOSE-C-5-TRANSFER.                                         
           EXEC SQL                                                     
             CLOSE C-FIND-5-TRANSFER                                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
              SET SO-7223-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR              
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7224-FETCH-C-5-TRANSFER                       
      ******************************************************************
       7224-FETCH-C-5-TRANSFER.                                         
           MOVE 5 TO WS-WHICH-TRANSFER                                  
           SET SO-NOT-END-OF-C-NAME-6 TO TRUE                           
           INITIALIZE T05-FLIGHT-ID                                     
           INITIALIZE T05-ARRIVAL-TIMESTAMP                             
           INITIALIZE T05-ARRIVAL-AIRPORT-CODE                          
           INITIALIZE WS-NUM-OF-FREE-SEATS                              
           EXEC SQL                                                     
           FETCH C-FIND-5-TRANSFER                                      
             INTO                                                       
             :T05-FLIGHT-ID,                                            
             :T05-ARRIVAL-TIMESTAMP,                                    
             :T05-ARRIVAL-AIRPORT-CODE,                                 
             :WS-NUM-OF-FREE-SEATS,                                     
             :T05-DEPARTURE-AIRPORT-CODE                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              MOVE WS-Z02172-TICKET-NUMBER TO WS-TEMP-TICKET-NUMBER     
              IF WS-NUM-OF-FREE-SEATS  > WS-TEMP-TICKET-NUMBER          
                SET SO-CONTINUE-WITH-ROW TO TRUE                        
                PERFORM 2204-MOVE-FETCHED-TO-QEUEU                      
              ELSE                                                      
                SET SO-SKIP-THAT-ROW TO TRUE                            
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-END-OF-C-NAME6 TO TRUE                             
           WHEN OTHER                                                   
              SET SO-7224-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR      
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                      7230-PREPARE-ARV-TIME                      
      ******************************************************************
       7230-PREPARE-ARV-TIME.                                           
           MOVE    T05-ARRIVAL-AIRPORT-CODE   TO T02-AIRPORT-CODE       
           PERFORM 7010-FETCH-TIMEZONE                                  
           PERFORM 7012-PREPARE-THE-DES-TIME                            
           DISPLAY 'PO 7012 '                                           
           MOVE    WS-MODIFIED-TIMESTAMP      TO QUEUE-ARRIVAL-TIMESTAMP
           .                                                            
      ******************************************************************
      *                     7231-GET-GEOGRAF-POS                        
      * PRARAGRAPH WILL GET LATITUDE AND LONGITUDE FOR A GIVEN AIRPORT  
      ******************************************************************
       7231-GET-GEOGRAF-POS.                                            
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
      *                  2201-MOVE-FETCHED-TO-QEUEU                     
      ******************************************************************
       2201-MOVE-FETCHED-TO-QEUEU.                                      
           MOVE T05-FLIGHT-ID-TEXT TO  QUEUE-FLIGHT-ID                  
           MOVE T05-FLIGHT-NUMBER-TEXT TO QUEUE-FLIGHT-NUMBER           
           MOVE T05-DEPARTURE-AIRPORT-CODE TO                           
                                  QUEUE-DEPARTURE-AIRPORT-CODE          
           MOVE T05-DEPARTURE-TIMESTAMP TO QUEUE-DEPARTURE-TIMESTAMP    
           MOVE T05-ARRIVAL-AIRPORT-CODE TO                             
                                  QUEUE-ARRIVAL-AIRPORT-CODE            
           MOVE T05-ARRIVAL-TIMESTAMP TO QUEUE-ARRIVAL-TIMESTAMP        
           MOVE T05-AIRLINE-CODE TO QUEUE-AIRLINE-CODE                  
           MOVE '00' TO QUEUE-TRANSFER-NUMBER                           
           MOVE WS-NUM-OF-FREE-SEATS TO ONE-WAY-Q-FREE-SEATS            
           .                                                            
      ******************************************************************
      *                   2202-CHECK-IF-DEST-FINAL                      
      ******************************************************************
       2202-CHECK-IF-DEST-FINAL.                                        
           DISPLAY '2202 START '                                        
           DISPLAY 'FLIGHT LEVEL: ' WS-WHICH-TRANSFER                   
           DISPLAY 'DEST AIRPORT:  ' WS-Z02172-DEST-AIRPORT-IATA        
           DISPLAY 'CURRECT ARRIVAL : ' T05-ARRIVAL-AIRPORT-CODE        
           IF WS-Z02172-DEST-AIRPORT-IATA =                             
                            T05-ARRIVAL-AIRPORT-CODE   THEN             
                SET SO-THIS-IS-FINAL-FLIGHT     TO TRUE                 
           ELSE                                                         
                SET SO-THIS-IS-NOT-FINAL-FLIGHT TO TRUE                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2203-WRITE-ONE-WAY-QUEUE                         
      ******************************************************************
       2203-WRITE-ONE-WAY-QUEUE.                                        
           DISPLAY '2203:  ' CT-ONEWAY-QUEUE                            
           EXEC CICS                                                    
             WRITEQ TS                                                  
             QUEUE(CT-ONEWAY-QUEUE)                                     
             FROM(WS-ONE-WAY-Q-STRUCTURE)                               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                 2204-MOVE-FETCHED-TO-QEUEU                      
      * PARAGRAPH WILL BE CALLED EVERYTIME WE HAVE FOUND ANOTHER        
      * TRANSFER FLIGHT THAT MEETS CRITERIA (IT WILL BE STORED IN THIS  
      * QUEUE)                                                          
      ******************************************************************
       2204-MOVE-FETCHED-TO-QEUEU.                                      
           MOVE T05-ARRIVAL-AIRPORT-CODE TO QUEUE-ARRIVAL-AIRPORT-CODE  
           MOVE T05-FLIGHT-ID-TEXT                                      
                               TO ONE-WAY-Q-FLIGHT-ID(WS-WHICH-TRANSFER)
           MOVE WS-NUM-OF-FREE-SEATS TO ONE-WAY-Q-FREE-SEATS-T(         
                                                   WS-WHICH-TRANSFER)   
           MOVE WS-WHICH-TRANSFER TO QUEUE-TRANSFER-NUMBER              
           .                                                            
      ******************************************************************
      *                     2205-CHECK-DEPARTURE-DATE                   
      ******************************************************************
       2205-CHECK-DEPARTURE-DATE.                                       
           MOVE QUEUE-DEPARTURE-TIMESTAMP TO WS-TEMP-TIMESTAMP          
           DISPLAY '2205DATECHECKING '                                  
           DISPLAY 'WS-TEMP-DATE: ' WS-TEMP-DATE                        
           DISPLAY 'WS-Z02172-DEP-DATE: '  WS-Z02172-DEPARTURE-DATE     
           IF WS-TEMP-DATE = WS-Z02172-DEPARTURE-DATE THEN              
              DISPLAY 'SO DEP DATE VALID '                              
              SET SO-DEP-DATE-VALID TO TRUE                             
           ELSE                                                         
              DISPLAY 'SO DEP DATE INVALID '                            
              SET SO-DEP-DATE-INVALID TO TRUE                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2206-CHECK-ARRIVAL-DATE                     
      ******************************************************************
       2206-CHECK-ARRIVAL-DATE.                                         
           MOVE QUEUE-ARRIVAL-TIMESTAMP TO WS-TEMP-TIMESTAMP            
           DISPLAY 'ARRIVAL TIMESTAMP: '                                
           DISPLAY 'WS-Z02172-RETURN-DATE '   WS-Z02172-RETURN-DATE     
           DISPLAY 'WS-TEMP-DATE : ' WS-TEMP-DATE                       
                                                                        
           IF WS-TEMP-DATE = WS-Z02172-RETURN-DATE    THEN              
              SET SO-ARV-DATE-VALID TO TRUE                             
           ELSE                                                         
              SET SO-ARV-DATE-INVALID TO TRUE                           
           END-IF                                                       
           DISPLAY ' PO 2206'                                           
           .                                                            
      ******************************************************************
      *                   2207-INITIALIZE-SUBFLIGHTS                    
      *                                                                 
      * SUBLFIGHTS  = FLIGHT THAT IS NON FIRST FLIGHT IN A TRANSFER     
      * FLIGHTS                                                         
      *                                                                 
      * WE WILL DELETE ALL SUBLIFHT THAT COULD BE SAVED THERE           
      * WE WILL SAVE ONLY THOSE THAT ARE VALID HERE                     
      ******************************************************************
       2207-INITIALIZE-SUBFLIGHTS.                                      
           MOVE WS-WHICH-TRANSFER TO WS-ITER11                          
           ADD 1 TO WS-ITER11                                           
           PERFORM VARYING WS-ITER12 FROM WS-ITER11 BY 1                
                                 UNTIL WS-ITER12   > 5                  
                 DISPLAY 'INSIDE 2207 LOOP '                            
                 INITIALIZE ONE-WAY-Q-FLIGHT-ID(WS-ITER12)              
                 INITIALIZE ONE-WAY-Q-FREE-SEATS-T(WS-ITER12)    
           END-PERFORM                                                  
           DISPLAY ' PO 2207'                                           
           .                                                            
      ******************************************************************
      *                 2208-CALCULATE-DISTANCE                         
      * PARAGRAPH WILL USE HAVERSINE FORMULA                            
      *                                                                 
      * WE WILL GET LATITUDES AND LONGITUDES OF 2 POINTS                
      *                                                                 
      * THEN WE WILL CHANGE DEGREES TO RADIANS                          
      *                                                                 
      * LATER WE WILL CALCULATE DELTA OF LATITUDES AND                  
      * DELTA OF LONGITUDES                                             
      *                                                                 
      * THEN CALCULATIONS WILL BE MADE                                  
      *                                                                 
      * COMPUTE WS-A = ( FUNCTION SIN(WS-LATITUDE / 2) ** 2 ) +         
      *  FUNCTION COS(WS-RAD-DES-LATITUDE) *                            
      *  FUNCTION COS(WS-RAD-ORG-LATITUDE) *                            
      *  FUNCTION SIN(WS-LONGITUDE / 2) ** 2                            
      *                                                                 
      * COMPUTE WS-C  = 2 * FUNCTION ATAN(                              
      *    FUNCTION SQRT(WS-A) / FUNCTION SQRT(1 - WS-A) )              
      *                                                                 
      * COMPUTE WS-D = WS-EARTCH-RADIOUS * WS-C                         
      *                                                                 
      * HERE WS-D WILL STORE FINAL RESULT OF THIS FORMULA               
      * AND THIS VALUE WILL BE SENT BACK TO THE CALLING PARAGRAPH       
      *                                                                 
      *                                                                 
      * WS-A , WS-C AND WS-D  ARE JUST A TEMPRORARY VARIABLES           
      *                                                                 
      * FINAL RESULT WILL BE STORED IN  WS-CALCULATED-DISTANCE VARIABLE 
      ******************************************************************
       2208-CALCULATE-DISTANCE.                                         
      * FIRST WE NEED TO GET LATITUDE AND LONGITUDE OF AIRPORTS   
           MOVE WS-DESTINATION-AIRPORT       TO T02-AIRPORT-CODE     
           PERFORM 7231-GET-GEOGRAF-POS                              
           MOVE WS-LATITUDE TO WS-DES-LATITUDE                       
           MOVE WS-LONGITUDE TO WS-DES-LONGITUDE                     
           MOVE WS-ORIGIN-AIRPORT             TO T02-AIRPORT-CODE    
           PERFORM 7231-GET-GEOGRAF-POS                              
           MOVE WS-LATITUDE TO WS-ORG-LATITUDE                       
           MOVE WS-LONGITUDE TO WS-ORG-LONGITUDE                     
      * THEN WE NEED TO CHENGE IT TO RADIANDS                        
           DISPLAY 'PI VALUE : ' CT-PI-VALUE                         
                                                                     
           COMPUTE WS-RAD-DES-LATITUDE =                             
                (CT-PI-VALUE * WS-DES-LATITUDE)  / 180               
           COMPUTE WS-RAD-DES-LONGITUDE =                            
                (CT-PI-VALUE * WS-DES-LONGITUDE)  / 180              
           COMPUTE WS-RAD-ORG-LATITUDE =                             
                (CT-PI-VALUE * WS-ORG-LATITUDE)  / 180               
           COMPUTE WS-RAD-ORG-LONGITUDE =                            
                (CT-PI-VALUE * WS-ORG-LONGITUDE)  / 180              
      * NOW WE HAVE TO  CALCULATE DIFFERENCE BETWEEN LONGITUDES AND  
      * LONGITUDES                                                   
           COMPUTE WS-LATITUDE = WS-RAD-DES-LATITUDE -               
                                 WS-RAD-ORG-LATITUDE                 
           COMPUTE WS-LONGITUDE = WS-RAD-DES-LONGITUDE -             
                                 WS-RAD-ORG-LONGITUDE                
           COMPUTE WS-A = ( FUNCTION SIN(WS-LATITUDE / 2) ** 2 ) +   
            FUNCTION COS(WS-RAD-DES-LATITUDE) *                      
            FUNCTION COS(WS-RAD-ORG-LATITUDE) *                      
            FUNCTION SIN(WS-LONGITUDE / 2) ** 2                      
                                                                     
           COMPUTE WS-C  = 2 * FUNCTION ATAN(                        
              FUNCTION SQRT(WS-A) / FUNCTION SQRT(1 - WS-A) )        
                                                                     
           COMPUTE WS-D = WS-EARTCH-RADIOUS * WS-C                   
           MOVE WS-D TO WS-CALCULATED-DISTANCE                       
           .                                                  
      ******************************************************************
      *                    7301-VALIDATE-TWO-FLIGHTS                    
      * FROM THE QUEUES WE GOT ARRIVAL TIMESTAMP OF LAST "TO" FLIGHT    
      * AND DEPARTURE TIMESTAMP OF FIRST "FROM"  FLIGHT                 
      * HERE WE WILL JUST CHECK IF IT IS POSSIBLE TO TAKE BOTH THOSE    
      * FLIGHTS (SET OF FLIGHTS)                                        
      ******************************************************************
       7301-VALIDATE-TWO-FLIGHTS.                                       
           MOVE QUEUE-F-ARRIVAL-TIMESTAMP  TO                           
                               WS-LAST-TO-FLIGHT-TIMESTAMP              
           MOVE QUEUE-S-DEPARTURE-TIMESTAMP TO                          
                               WS-FIRST-FROM-FLIGHT-TIMESTAMP           
           DISPLAY 'SPRAWDZNEILOTOWSTRONNYCH'                           
           DISPLAY ' LAST TO FLIGHT TIME ' WS-LAST-TO-FLIGHT-TIMESTAMP  
           DISPLAY 'FIRST FROM FLIGHT TIME '                            
                               WS-FIRST-FROM-FLIGHT-TIMESTAMP           
           EXEC SQL                                                     
             SELECT "A"                                                 
             INTO :WS-RANDOM-VALUE                                      
             FROM T05_FLIGHT_TABLE                                      
             WHERE                                                      
              :WS-LAST-TO-FLIGHT-TIMESTAMP <                            
                :WS-FIRST-FROM-FLIGHT-TIMESTAMP                         
             FETCH FIRST ROW ONLY                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY 'SO-FLIGHT POSSIBLE '                             
              SET SO-POSSIBLE-FLIGHTS   TO TRUE                         
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY 'SO-FLIGHT UNPOSSIBLE '                           
              SET SO-IMPOSSIBLE-FLIGHTS TO TRUE                         
           WHEN OTHER                                                   
              SET SO-7301-PARA TO TRUE                                  
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
                                 
       

      
       

                                  
                              
                      
                                    
                   

                                         
                                                

                                                   
                           
                    

                                      
      
        
               
                                                                        

                           
                              
               
    
                          
                                                                        
                       
                               
             
                             
                                                  
                 
                                                  
             

 
                  
                     
                        
  
             
                           
             
          
                       
            
                                
                                                
                            
            
                                      
                                               

                    
                                       
                                                     
   
                                    
                               

              
                      
               


                                                                       
                                           
                 
                                          
                

                       

                                                                        
                               
          
                                                  
                      
                                                      
                                                  
                                                    
                                               
                                                
                      
      
       
                 
            
                   
                       
          
                     
                 
                        
                
                      


  
               
        
          
                                  
             
             
        
                   
                   
               
                         

  
           

