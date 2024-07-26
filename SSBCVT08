       *----------------------------------------------------------------*
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SPECIAL-NAMES.                                                   
            DECIMAL-POINT IS COMMA.                                      
        INPUT-OUTPUT SECTION.                                            
       *-----------------------------------------------------------------
        FILE-CONTROL.                                                    
       *-----------------------------------------------------------------
            SELECT FILEIN01        ASSIGN TO FILEIN01                    
                                   STATUS IS FILEIN01-STATUS.            
       *                                                                 
            SELECT FILEIN02        ASSIGN TO FILEIN02                    
                                   STATUS IS FILEIN02-STATUS.            
       *
            SELECT FILEOU01        ASSIGN TO FILEOU01                    
                                   STATUS IS FILEOU01-STATUS.            
       *                                                                 
       *-----------------------------------------------------------------
        DATA DIVISION.                                                   
       *-----------------------------------------------------------------
        FILE SECTION.                                                    
       *-----------------------------------------------------------------
        FD  FILEIN01                                                     
            LABEL RECORD IS STANDARD                                     
            RECORDING MODE IS F.                                         
        01  REC-FILEIN01                  PIC X(300).                    
       *                                                                 
        FD  FILEIN02                                                     
            LABEL RECORD IS STANDARD                                     
            RECORDING MODE IS F.                                         
        01  REC-FILEIN02                  PIC X(54).
       *                                                                 
        FD  FILEOU01                                                     
            LABEL RECORD IS STANDARD                                     
            RECORDING MODE IS F.                                         
        01  REC-FILEOU01.                                                
              05 REC-FILEOU01-TITOLO          PIC X(50).                 
              05 REC-FILEOU01-ANNO            PIC 9(04).                 
              05 REC-FILEOU01-GENERE          PIC X(20).                 
              05 REC-FILEOU01-VOTO            PIC 9(02)V9(02).           
              05 FILLER                       PIC X(22).                 
       *                                                                 
       *-----------------------------------------------------------------
        WORKING-STORAGE SECTION.                                         
       *-----------------------------------------------------------------
       *----------------------------------------------------------------*
       *--* COSTANTI                                                 *--*
       *----------------------------------------------------------------*
        77 ABEND                            PIC X(08) VALUE 'ABEND'.     
       *----------------------------------------------------------------*
       *--* GESTIONE ERRORI                                          *--*
       *----------------------------------------------------------------*
       *                                                                 
        01  ERRORI-CENSITI.                                              
          05  ERRORE-CENSITO                PIC  X(86).                  
            88  ERR-FILEIN01-OPEN                    VALUE               
                '0001CFERRORE OPEN    FILEIN01'.                         
            88  ERR-FILEIN01-READ                    VALUE               
                '0002CFERRORE LETTURA FILEIN01'.                         
            88  ERR-FILEIN01-VUOTO                   VALUE               
                '0003WNERRORE FILEIN01 VUOTO'.                           
            88  ERR-FILEIN01-CLOSE                   VALUE               
                '0004CFERRORE CLOSE   FILEIN01'.                         
            88  ERR-FILEIN02-OPEN                    VALUE               
                '0005CFERRORE OPEN    FILEIN02'.                         
            88  ERR-FILEIN02-READ                    VALUE               
                '0006CFERRORE LETTURA FILEIN02'.                         
            88  ERR-FILEIN02-VUOTO                   VALUE               
                '0007WNERRORE FILEIN02 VUOTO'.                           
            88  ERR-FILEIN02-CLOSE                   VALUE               
                '0008CFERRORE CLOSE   FILEIN02'.                         
            88  ERR-FILEOU01-OPEN                    VALUE               
                '0009CFERRORE OPEN    FILEOU01'.                         
            88  ERR-FILEOU01-WRITE                   VALUE               
                '0010CFERRORE SCRITTURA FILEOU01'.                       
            88  ERR-FILEOU01-CLOSE                   VALUE               
                '0011CFERRORE CLOSE   FILEOU01'.                         
       *                                                                 
          05  FILLER REDEFINES ERRORE-CENSITO.                           
            10  ERR-RETCODE                 PIC  X(04).                  
            10  ERR-TIPO                    PIC  X.                      
              88  ERR-CRITICO                  VALUE 'C'. 
              88  ERR-WARNING                  VALUE 'W'.                
            10  ERR-FG-DETT                 PIC  X.                      
              88  ERR-DETT-NO                  VALUE 'N'.                
              88  ERR-DETT-SQL                 VALUE 'S'.                
              88  ERR-DETT-FILE                VALUE 'F'.                
            10  ERR-DESCR.                                               
              15  ERR-DESCR-MSG             PIC  X(65).                  
              15  ERR-DESCR-DBG.                                         
                20  ERR-DBG-FILE.                                        
                  25  ERR-DBG-FILE-DESCR    PIC  X(10).                  
                    88  ERR-DBG-FILE-DESCR-ATTIVA                        
                        VALUE ' STATUS : '.                              
                  25  ERR-DBG-FILE-STATUS   PIC  X(02).                  
                                                                         
       *-----------------------------------------------------------------
       * Contatori per archivi                                           
       *-----------------------------------------------------------------
        01 WS-CONTA-LETTI               PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-LETTI2              PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-SCRITTI             PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-WARNING             PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-MINORI              PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-UGUALI              PIC 9(8) VALUE ZEROES.           
        01 WS-CONTA-MAGGIO              PIC 9(8) VALUE ZEROES.           
       *-----------------------------------------------------------------
       * Programmi richiamati                                            
       *-----------------------------------------------------------------
        01 UTRCDSYS                       PIC X(8) VALUE 'UTRCDSYS'.     
                                                                         
       *-----------------------------------------------------------------
       * Return code archivi work-field                                  
       *-----------------------------------------------------------------
        01 FILEIN01-STATUS                PIC XX.                        
           88 FILEIN01-OK                          VALUE '00'.           
           88 FILEIN01-EOF                         VALUE '10'.           
           88 FILEIN01-DUP                         VALUE '22'.           
           88 FILEIN01-NFD                         VALUE '23'.           
       *                                                                 
        01 FILEIN02-STATUS                PIC XX.                        
           88 FILEIN02-OK                          VALUE '00'.           
           88 FILEIN02-EOF                         VALUE '10'.           
           88 FILEIN02-DUP                         VALUE '22'.           
           88 FILEIN02-NFD                         VALUE '23'.           
       *                                                                 
        01 FILEOU01-STATUS                PIC XX.                        
           88 FILEOU01-OK                          VALUE '00'.           
           88 FILEOU01-EOF                         VALUE '10'.           
           88 FILEOU01-DUP                         VALUE '22'.           
           88 FILEOU01-NFD                         VALUE '23'.           
       *                                                                 
        01 WS-RETURN                      PIC XX  VALUE '00'.            
          88 WS-RETURN-OK                         VALUE '00'.            
          88 WS-RETURN-KO                         VALUE '12'.            
       *-----------------------------------------------------------------
       * Data e ora di elaborazione                                      
       *-----------------------------------------------------------------
        01 WRKA-DTSYSTE.                                                 
           05 WRKA-DTSYSSS                PIC 99.                        
           05 WRKA-DTSYSAA                PIC 99.                        
           05 WRKA-DTSYSMM                PIC 99.                        
           05 WRKA-DTSYSGG                PIC 99.                        
        01 WRKA-DATASYS             REDEFINES WRKA-DTSYSTE               
                                          PIC 9(8).                      
       *                                                                 
        01 WRKA-HHSYSTE.                                                 
           05 WRKA-HHSYSHH                PIC 99.                        
           05 WRKA-HHSYSMI                PIC 99.                        
           05 WRKA-HHSYSSE                PIC 99.                        
           05 WRKA-HHSYSCC                PIC 99.                        
        01 WRKA-ORASYS              REDEFINES WRKA-HHSYSTE               
                                          PIC 9(8).                      
       *-----------------------------------------------------------------
       * COPY                                                            
       *-----------------------------------------------------------------
        COPY SSCPTF08.                                                   
        COPY SSCPVT08.                                                   
       *-----------------------------------------------------------------
        PROCEDURE DIVISION.                                              
       *-----------------------------------------------------------------
        MAIN.                                                            
       *                                                                 
            PERFORM INIZIO-ELABORAZIONE                                  
               THRU INIZIO-ELABORAZIONE-EX.                              
       *                                                                 
            PERFORM LETTURA-FILEIN01                                     
               THRU LETTURA-FILEIN01-EX.                                 
       *                                                                 
            PERFORM LETTURA-FILEIN02                                     
               THRU LETTURA-FILEIN02-EX.                                 
       *                                                                 
            EVALUATE TRUE                                                
               WHEN FILEIN01-EOF                                         
                  DISPLAY '*   FILE DI INPUT 1 VUOTO !!!           *'    
                  DISPLAY '*---------------------------------------*'    
                  PERFORM TERMINA-ELABORAZIONE                           
                     THRU TERMINA-ELABORAZIONE-EX                        
               WHEN FILEIN02-EOF                                         
                  DISPLAY '*   FILE DI INPUT 2 VUOTO !!!           *'    
                  DISPLAY '*---------------------------------------*'    
                  PERFORM TERMINA-ELABORAZIONE                           
                     THRU TERMINA-ELABORAZIONE-EX                        
               WHEN FILEIN01-OK                                          
                AND FILEIN02-OK                                          
                  PERFORM ELABORAZIONE                                   
                     THRU ELABORAZIONE-EX                                
                    UNTIL FILEIN01-EOF                                   
                       OR FILEIN02-EOF                                   
            END-EVALUATE.                                                
       *                                                                 
            PERFORM TERMINA-ELABORAZIONE                                 
               THRU TERMINA-ELABORAZIONE-EX.                             
       *                                                                 
            MOVE WS-RETURN                TO RETURN-CODE.                
       *                                                                 
            STOP RUN.                                                    
                                                                         
       *-----------------------------------------------------------------
        INIZIO-ELABORAZIONE.                                             
       *-----------------------------------------------------------------
                                                                         
            DISPLAY '*----------------------------------------*'.        
            DISPLAY '* INIZIO ELABORAZIONE PGM SSBCVT08       *'.        
            DISPLAY '*----------------------------------------*'.        
       *                                                                 
            OPEN  INPUT FILEIN01.                                        
            IF NOT FILEIN01-OK                                           
               SET ERR-FILEIN01-OPEN TO TRUE                             
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
       *                                                                 
            OPEN  INPUT FILEIN02.                                        
            IF NOT FILEIN02-OK                                           
               SET ERR-FILEIN02-OPEN TO TRUE                             
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
       *                                                                 
            OPEN  OUTPUT FILEOU01.                                       
            IF NOT FILEOU01-OK                                           
               SET ERR-FILEOU01-OPEN TO TRUE                             
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
       *                                                                 
        INIZIO-ELABORAZIONE-EX.                                          
            EXIT.                                                        
                                                                         
       *-----------------------------------------------------------------
        LETTURA-FILEIN01.                                                
       *-----------------------------------------------------------------
                                                                         
       *    READ FILEIN01.                                               
            READ FILEIN01 INTO REC-SSCPTF08.                             
       *                                                                 
            EVALUATE TRUE                                                
                WHEN FILEIN01-OK                                         
                     ADD 1                TO WS-CONTA-LETTI              
                WHEN FILEIN01-EOF                                        
                     CONTINUE                                            
                WHEN OTHER                                               
                     SET ERR-FILEIN01-READ TO TRUE                       
                     PERFORM GESTIONE-ERRORI                             
                        THRU GESTIONE-ERRORI-EX                          
            END-EVALUATE.                                                
       *                                                                 
                                                                         
        LETTURA-FILEIN01-EX.                                             
            EXIT.                                                        

       *-----------------------------------------------------------------
        LETTURA-FILEIN02.                                                
       *-----------------------------------------------------------------
                                                                         
       *    READ FILEIN02.                                               
            READ FILEIN02 INTO REC-SSCPVT08.                             
       *                                                                 
            EVALUATE TRUE                                                
                WHEN FILEIN02-OK                                         
                     ADD 1                TO WS-CONTA-LETTI2             
                WHEN FILEIN02-EOF                                        
                     CONTINUE                                            
                WHEN OTHER                                               
                     SET ERR-FILEIN02-READ TO TRUE                       
                     PERFORM GESTIONE-ERRORI                             
                        THRU GESTIONE-ERRORI-EX                          
            END-EVALUATE.                                                
       *                                                                 
                                                                         
        LETTURA-FILEIN02-EX.                                             
            EXIT.                                                        
                                                                         
       *-----------------------------------------------------------------
        SCRITTURA-FILEOU01.                                              
       *-----------------------------------------------------------------
       *                                                                 
            WRITE REC-FILEOU01.                                          
       *                                                                 
            EVALUATE TRUE                                                
                WHEN FILEOU01-OK                                         
                   ADD 1                      TO WS-CONTA-SCRITTI        
                WHEN OTHER                                               
                   SET ERR-FILEOU01-WRITE TO TRUE                        
                   PERFORM GESTIONE-ERRORI                               
                      THRU GESTIONE-ERRORI-EX                            
            END-EVALUATE.                                                
       *                                                                 
        SCRITTURA-FILEOU01-EX.                                           
            EXIT.                                                        
       *                                                                 
       *----------------------------------------------------------------*
       * ROUTINE     : GESTIONE-ERRORI                                  *
       * DESCRIZIONE : GESTIONE CENTRALIZZATA DEGLI ERRORI              *
       *----------------------------------------------------------------*
        GESTIONE-ERRORI.                                                 
       *                                                                 
            EVALUATE TRUE                                                
               WHEN ERR-DETT-FILE                                        
                  SET ERR-DBG-FILE-DESCR-ATTIVA TO TRUE                  
                  IF FILEIN01-STATUS NOT = '00'                          
                     MOVE FILEIN01-STATUS                                
                       TO ERR-DBG-FILE-STATUS                            
                  END-IF                                                 
                  IF FILEOU01-STATUS NOT = '00'                          
                     MOVE FILEOU01-STATUS                                
                       TO ERR-DBG-FILE-STATUS                            
                  END-IF                                                 
            END-EVALUATE.                                                
       *                                                                 
            EVALUATE TRUE                                                
               WHEN ERR-DETT-FILE                                        
                  SET ERR-DBG-FILE-DESCR-ATTIVA TO TRUE                  
                  IF FILEIN02-STATUS NOT = '00'                          
                     MOVE FILEIN02-STATUS                                
                       TO ERR-DBG-FILE-STATUS                            
                  END-IF                                                 
                  IF FILEOU01-STATUS NOT = '00'                          
                     MOVE FILEOU01-STATUS                                
                       TO ERR-DBG-FILE-STATUS                            
                  END-IF                                                 
            END-EVALUATE.                                                
       *                                                                 
            EVALUATE TRUE                                                
               WHEN ERR-CRITICO                                          
                  PERFORM ERRORE-ABEND                                   
                     THRU ERRORE-ABEND-EX                                
               WHEN ERR-WARNING                                          
                  PERFORM ERRORE-WARNING                                 
                     THRU ERRORE-WARNING-EX                              
            END-EVALUATE.                                                
       *                                                                 
        GESTIONE-ERRORI-EX.                                              
            EXIT.                                                        
       *                                                                 
       *----------------------------------------------------------------*
       * ROUTINE     : ERRORE-ABEND                                     *
       * DESCRIZIONE : ESEGUE UN ABEND DI PROGRAMMA                     *
       *----------------------------------------------------------------*
        ERRORE-ABEND.                                                    
       *                                                                 
            DISPLAY ' '.                                                 
            DISPLAY '*****************************************'.         
            DISPLAY '****           A B E N D             ****'.         
            DISPLAY '*****************************************'.         
            DISPLAY ' ERRORE  : ' ERR-DESCR                              
            DISPLAY '*---------------------------------------*'          
            DISPLAY ' '.                                                 
       *                                                                 
            CALL ABEND.                                                  
       *                                                                 
            PERFORM TERMINA-ELABORAZIONE                                 
               THRU TERMINA-ELABORAZIONE-EX.                             
       *                                                                 
        ERRORE-ABEND-EX.                                                 
            EXIT.                                                        
       *                                                                 
       *----------------------------------------------------------------*
       * ROUTINE     : ERRORE-WARNING                                   *
       * DESCRIZIONE : SEGNALA UN WARNING DI PROGRAMMA                  *
       *----------------------------------------------------------------*
        ERRORE-WARNING.                                                  
       *                                                                 
            DISPLAY ' WARNING : ' ERR-DESCR.                             
            DISPLAY '*---------------------------------------*'.         
       *                                                                 
            ADD  1                          TO WS-CONTA-WARNING.         
       *                                                                 
        ERRORE-WARNING-EX.                                               
            EXIT.                                                        
       *                                                                 
       *-----------------------------------------------------------------
        ELABORAZIONE.                                                    
       *-----------------------------------------------------------------
            MOVE SPACES                 TO REC-FILEOU01.                 
            INITIALIZE REC-FILEOU01.                                     
       *                                                                 
       * LOGICA 1 TO 1                                                   
       *                                                                 
       *    IF REC-SSCPTF08-TITOLO < REC-SSCPVT08-TITOLO                 
       *       ADD 1                      TO WS-CONTA-MINORI             
       *       PERFORM LETTURA-FILEIN01                                  
       *          THRU LETTURA-FILEIN01-EX                               
       *    ELSE                                                         
       *       IF REC-SSCPTF08-TITOLO = REC-SSCPVT08-TITOLO              
       *          MOVE REC-SSCPTF08-TITOLO TO REC-FILEOU01-TITOLO        
       *          MOVE REC-SSCPTF08-ANNO   TO REC-FILEOU01-ANNO          
       *          MOVE REC-SSCPTF08-GENERE TO REC-FILEOU01-GENERE        
       *          MOVE REC-SSCPVT08-VOTO   TO REC-FILEOU01-VOTO          
       *          ADD 1                    TO WS-CONTA-UGUALI            
       *          PERFORM SCRITTURA-FILEOU01                             
       *             THRU SCRITTURA-FILEOU01-EX                          
       *          PERFORM LETTURA-FILEIN01                               
       *             THRU LETTURA-FILEIN01-EX                            
       *          PERFORM LETTURA-FILEIN02                               
       *             THRU LETTURA-FILEIN02-EX                            
       *       ELSE                                                      
       *          IF REC-SSCPTF08-TITOLO > REC-SSCPVT08-TITOLO           
       *          ADD 1                    TO WS-CONTA-MAGGIO            
       *          PERFORM LETTURA-FILEIN02                               
       *             THRU LETTURA-FILEIN02-EX                            
       *          END-IF                                                 
       *       END-IF                                                    
       *    END-IF.                                                      
       *                                                                 
       * LOGICA 1 TO MANY                                                
       *                                                                 
            IF REC-SSCPTF08-TITOLO < REC-SSCPVT08-TITOLO                 
               ADD 1                      TO WS-CONTA-MINORI             
               PERFORM LETTURA-FILEIN01                                  
                  THRU LETTURA-FILEIN01-EX                               
            ELSE                                                         
               IF REC-SSCPTF08-TITOLO = REC-SSCPVT08-TITOLO              
                  MOVE REC-SSCPTF08-TITOLO TO REC-FILEOU01-TITOLO        
                  MOVE REC-SSCPTF08-ANNO   TO REC-FILEOU01-ANNO          
                  MOVE REC-SSCPTF08-GENERE TO REC-FILEOU01-GENERE        
                  MOVE REC-SSCPVT08-VOTO   TO REC-FILEOU01-VOTO          
                  ADD 1                    TO WS-CONTA-UGUALI            
                  PERFORM SCRITTURA-FILEOU01                             
                     THRU SCRITTURA-FILEOU01-EX                          
                  PERFORM LETTURA-FILEIN02                               
                     THRU LETTURA-FILEIN02-EX                            
               ELSE                                                      
                  IF REC-SSCPTF08-TITOLO > REC-SSCPVT08-TITOLO           
                  ADD 1                    TO WS-CONTA-MAGGIO            
                  PERFORM LETTURA-FILEIN02                               
                     THRU LETTURA-FILEIN02-EX                            
                  END-IF                                                 
               END-IF                                                    
            END-IF.                                                      
       *                                                                 
        ELABORAZIONE-EX.                                                 
            EXIT.                                                        
                                                                         
       *-----------------------------------------------------------------
        TERMINA-ELABORAZIONE.                                            
       *-----------------------------------------------------------------

            DISPLAY '* REC LETTI        -FILEIN01- : 'WS-CONTA-LETTI     
                    ' *'                                                 
            DISPLAY '* REC LETTI 2      -FILEIN02- : 'WS-CONTA-LETTI2    
                    ' *'                                                 
            DISPLAY '* REC MINORI       -MINORI00- : 'WS-CONTA-MINORI    
                    ' *'                                                 
            DISPLAY '* REC UGUALI       -UGUALI00- : 'WS-CONTA-UGUALI    
                    ' *'                                                 
            DISPLAY '* REC MAGGIO       -MAGGIO00- : 'WS-CONTA-MAGGIO    
                    ' *'                                                 
            DISPLAY '* REC SCRITTI      -FILEOU01- : 'WS-CONTA-SCRITTI   
                    ' *'                                                 
            DISPLAY '*----------------------------------------*'         
            DISPLAY '* FINE ELABORAZIONE           SSBCVT08   *'         
            DISPLAY '*----------------------------------------*'         
       *                                                                 
            CLOSE FILEIN01.                                              
            IF NOT FILEIN01-OK                                           
               SET ERR-FILEIN01-CLOSE TO TRUE                            
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
            CLOSE FILEIN02.                                              
            IF NOT FILEIN02-OK                                           
               SET ERR-FILEIN02-CLOSE TO TRUE                            
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
            CLOSE FILEOU01.                                              
            IF NOT FILEOU01-OK                                           
               SET ERR-FILEOU01-CLOSE TO TRUE                            
               PERFORM GESTIONE-ERRORI                                   
                  THRU GESTIONE-ERRORI-EX                                
            END-IF.                                                      
       *                                                                 
        TERMINA-ELABORAZIONE-EX.                                         
            EXIT.                                                        
