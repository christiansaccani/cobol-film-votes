//COBT0008 JOB (ORCHES),'COMPILE',                                      
//         CLASS=E,MSGCLASS=X,REGION=0M,COND=(4,LT),NOTIFY=&SYSUID      
//**********************************************************************
//*        COMPILATORE COBOL BATCH (NO DB2, NO CICS) LOAD SINGOLO      *
//**********************************************************************
//COMPILA PROC AMBPRFX='TEST',                                          
//             APPL='SS$00000',                                         
//             APP$='$$$00000',                                         
//             TYPE='GEBA',                                             
//             STDAPPL='SS$00000',                                      
//             LOADBT='LOADBTEX',                                       
//             LIBPRFX='CEE',                                           
//             LNGPRFX='IGY610',                                        
//             WMQPRFX='CSQ901',                                        
//*            LNKPARM='(LIST,MAP,RENT,XREF)',                          
//             LNKPARM='(LIST,MAP,RENT,XREF,AMODE(31),RMODE(ANY))',     
//             OBJLIB='NULLFILE',                                       
//             UNITDEV=SYSALLDA,                                        
//             PROGRAM=&PROGRAM                                         
//*====================================================================*
//*        STEP COBOL                                                  *
//*====================================================================*
//COBLSTEP EXEC PGM=IGYCRCTL,REGION=0M,                                 
//         PARM=('LIST,MAP,SOURCE,NONUMBER,XREF(SHORT)')                
//STEPLIB  DD DSN=&LNGPRFX..SIGYCOMP,DISP=SHR                           
//SYSIN    DD DSN=&AMBPRFX..&APPL..&TYPE(&PROGRAM),DISP=SHR             
//SYSLIB   DD DSN=&AMBPRFX..&APPL..GECP,DISP=SHR                        
//         DD DSN=&AMBPRFX..&APPL..D2CP,DISP=SHR                        
//         DD DSN=&AMBPRFX..&STDAPPL..GECP,DISP=SHR                     
//         DD DSN=&AMBPRFX..&STDAPPL..D2CP,DISP=SHR                     
//         DD DSN=&WMQPRFX..SCSQCOBC,DISP=SHR                           
//SYSPRINT DD SYSOUT=*                                                  
//SYSDEBUG DD SYSOUT=*                                                  
//SYSMDECK DD DUMMY                                                     
//SYSLIN   DD DISP=(MOD,PASS),DSN=&&LOADSET,UNIT=&UNITDEV,              
//            SPACE=(80,(10,10))                                        
//SYSUT1   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT2   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT3   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT4   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT5   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT6   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT7   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT8   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT9   DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT10  DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT11  DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT12  DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//SYSUT13  DD SPACE=(80,(10,10),,,ROUND),UNIT=&UNITDEV                  
//**********************************************************************
//ORBCXLKE EXEC PGM=ORBCXLKE,                                           
//         PARM='&PROGRAM',COND=(5,LT,COBLSTEP)                         
//*---------------------------------------------------------------------
//STEPLIB  DD DSN=&AMBPRFX..&APP$..&LOADBT,DISP=SHR                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//OUT4LKED DD DSN=&&OUT4LKED,DISP=(MOD,PASS),                           
//            UNIT=SYSDA,SPACE=(5,(5,5))                                
//**********************************************************************
//*        FASE LINKAGE EDITOR                                         *
//**********************************************************************
//LINKBT   EXEC PGM=IEWL,                                               
//         PARM=&LNKPARM,COND=(5,LT,COBLSTEP)                           
//SYSLIB   DD DSN=&LIBPRFX..SCEELKED,DISP=SHR                           
//         DD DSN=&LIBPRFX..SCEERUN,DISP=SHR                            
//         DD DSN=&LIBPRFX..SCEERUN2,DISP=SHR                           
//         DD DSN=&AMBPRFX..&APP$..&LOADBT,DISP=SHR                     
//OBJLIB   DD DSN=&OBJLIB,DISP=SHR                                      
//SYSLMOD  DD DSN=&AMBPRFX..&APPL..&LOADBT(&PROGRAM),DISP=SHR           
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,                              
//            SPACE=(1024,(200,20))                                     
//SYSPRINT DD SYSOUT=*                                                  
//SYSLIN   DD DSN=&&LOADSET,DISP=OLD                                    
//SYSIN    DD DSN=&&OUT4LKED,DISP=(MOD,DELETE,DELETE)                   
//**********************************************************************
//   PEND                                                               
//SSBCAN08 EXEC COMPILA,PROGRAM='SSBCVT08'                              
//                                                                      
