libname dataset "/folders/myfolders/datasetsBD";

/******************************************************************************/
/********************************the datasets**********************************/
/******************************************************************************/
/* IMPORT WITH 8 OBSERVATIONS */
PROC IMPORT OUT = WORK.DD8_HAND1
  DATAFILE = "/folders/myfolders/datasetsBD/assignment_dd8_hand1.csv"
  DBMS = CSV REPLACE;
  GETNAMES = YES;
RUN;

DATA DD8_HAND1;
	SET DD8_HAND1;
	CAGE = 0;
	IF AGE0 > 59 THEN CAGE = 1;
	IF AGE0 > 65 THEN CAGE = 2;
	IF AGE0 > 72 THEN CAGE = 3;
RUN;

PROC SORT DATA = DD8_HAND1;
	BY ID FU HAND;
RUN;

/******************************************************************************/
/****************************** The base model ********************************/
/******************************************************************************/
TITLE 'BASELINE';
proc mixed data = DD8_HAND1 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE MONTH SEX 
	 /solution ddfm=satterthwaite OUTP=PREDHAND1;
	RANDOM INT MONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

PROC UNIVARIATE DATA=PREDHAND1 NORMAL;
	VAR RESID;
RUN;