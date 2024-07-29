//CORSO08L JOB (SCHOOL),'SIDE',CLASS=A,MSGCLASS=X,REGION=0M,            JOB02336
//         COND=(04,LT),NOTIFY=&SYSUID,
//         RESTART=DELETE02
//*---------------------------------------------------------------------
//*        SCRATCH SORT
//*---------------------------------------------------------------------
//DELETE01 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.SORT
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* SORT FILM PER ANNO DAL MINORE
//*---------------------------------------------------------------------
//SORT01   EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.SORT,DISP=(,CATLG),
//         UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(91,4,ZD,D,1,50,CH,A)
/*
//*---------------------------------------------------------------------
//*        SCRATCH FANTASCIENZA
//*---------------------------------------------------------------------
//DELETE02 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* FILTRO FILM DEL GENERE FANTASCIENZA
//*---------------------------------------------------------------------
//INCLUD01 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.SORT
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
  INCLUDE COND=(99,20,CH,EQ,C'FANTASCIENZA        ')
/*
//*---------------------------------------------------------------------
//*        SCRATCH FANTA.S
//*---------------------------------------------------------------------
//DELETE04 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.S
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* SUM NEL FILE .FANTA, TENENDO L'ANNO MINORE
//*---------------------------------------------------------------------
//INCLUD02 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.S,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(91,4,ZD,A)
/*
//*---------------------------------------------------------------------
//*        SCRATCH SUM
//*---------------------------------------------------------------------
//DELETE05 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.SUM
 SET MAXCC=0
/*
//INCLUD21 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.S
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.SUM,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(51,40,CH,A)
  SUM FIELDS=NONE
/*
//*---------------------------------------------------------------------
//*        SCRATCH ALTRO
//*---------------------------------------------------------------------
//DELETE03 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* FILTRO FILM NON FANTASCIENZA IN .ALTRO
//*---------------------------------------------------------------------
//INCLUD03 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.SORT
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
  OMIT COND=(99,20,CH,EQ,C'FANTASCIENZA        ')
/*
//*---------------------------------------------------------------------
//*        SCRATCH SKIP
//*---------------------------------------------------------------------
//DELETE03 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.SKIP
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* SKIP DEL PRIMO DATO DEL FILE .ALTRO
//*---------------------------------------------------------------------
//INCLUD04 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.SKIP,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  OPTION COPY,SKIPREC=1
/*
//*---------------------------------------------------------------------
//*        SCRATCH FANTASCIENZA OUTREC
//*---------------------------------------------------------------------
//DELETE02 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.OUT
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* OUTREC DEL FILE .FANTA
//*---------------------------------------------------------------------
//INCLUD05 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.OUT,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=70,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTREC FIELDS=(1,50,99,20)
/*
//*---------------------------------------------------------------------
//*        SCRATCH ALTRO OUTREC
//*---------------------------------------------------------------------
//DELETE02 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO.OUT
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* OUTREC DEL FILE .ALTRO
//*---------------------------------------------------------------------
//INCLUD06 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO.OUT,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=70,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTREC FIELDS=(1,50,99,20)
/*
//*---------------------------------------------------------------------
//*        SCRATCH MERGE
//*---------------------------------------------------------------------
//DELETE02 EXEC PGM=IDCAMS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE TEST.ACCA.ANAG.FILM.ULTIMO08.MERGE
 SET MAXCC=0
/*
//*---------------------------------------------------------------------
//* MERGE DEI FILE .FANTA E .ALTRO
//*---------------------------------------------------------------------
//INCLUD07 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.FANTA.OUT
//         DD DISP=SHR,DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.ALTRO.OUT
//SORTOUT  DD DSN=TEST.ACCA.ANAG.FILM.ULTIMO08.MERGE,
//         DISP=(,CATLG),UNIT=WORKA,SPACE=(CYL,(1,1),RLSE),
//         DCB=(RECFM=FB,LRECL=70,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
