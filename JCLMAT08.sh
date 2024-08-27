//CORSO08L JOB (SCHOOL),'SIDE',CLASS=A,MSGCLASS=X,REGION=0M,            
//         COND=(04,LT),NOTIFY=&SYSUID,                                 
//         RESTART=*                                                    
//*---------------------------------------------------------------------
//*                                                                     
//*---------------------------------------------------------------------
//DELETE01 EXEC PGM=IDCAMS                                              
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
 DELETE TEST.ACCA.JCLMAT08.SSBCVT08.FILEOU01                            
 SET MAXCC=0                                                            
/*                                                                      
//*--------------------------------------------------------------------*
//*                                                                     
//*--------------------------------------------------------------------*
//SSBCVT08 EXEC PGM=SSBCVT08                        
//STEPLIB  DD DSN=TEST.SS$00000.LOADBTEX,DISP=SHR                       
//         DD DSN=TEST.$$$00000.LOADBTEX,DISP=SHR                       
//SYSPRINT DD SYSOUT=*                                                  
//SYSTSPRT DD SYSOUT=*                                                  
//SYSCOUNT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=D                                                  
//CEEDUMP  DD SYSOUT=D                                                  
//SYSDBOUT DD SYSOUT=D                                                  
//SYSOUT   DD SYSOUT=*                                                  
//FILESYSO DD SYSOUT=*                                                  
//FILEIN01 DD DSN=TEST.ACCA.ANAG.FILM07,DISP=SHR                        
//FILEIN02 DD DSN=TEST.ACCA.ANAG.VOTI07,DISP=SHR                        
//FILEOU01 DD DSN=TEST.ACCA.JCLMAT08.SSBCVT08.FILEOU01,DISP=(,CATLG),   
//         UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),                           
//         DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)                           
