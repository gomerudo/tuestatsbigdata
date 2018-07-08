libname dataset "/folders/myfolders/datasetsBD";

/******************************************************************************/
/********************************the datasets**********************************/
/******************************************************************************/

/* IMPORT WITH 12 OBSERVATIONS */
PROC IMPORT OUT = WORK.DD12_HAND1
  DATAFILE = "/folders/myfolders/datasetsBD/assignment_dd12_hand1.csv"
  DBMS = CSV REPLACE;
  GETNAMES = YES;
RUN;

DATA DD12_HAND1;
	SET DD12_HAND1;
	CAGE = 0;
	IF AGE0 > 59 THEN CAGE = 1;
	IF AGE0 > 65 THEN CAGE = 2;
	IF AGE0 > 72 THEN CAGE = 3;
RUN;

PROC SORT DATA = DD12_HAND1;
	BY ID FU HAND;
RUN;

/* CREATE DATASET WITH 10 OBSERVATIONS */
DATA DD9_HAND1;
	SET DD12_HAND1;
	IF FU > 10 THEN DELETE;
RUN;

/******************************************************************************/
/******************************** Impute 5 ************************************/
/******************************************************************************/

proc mi data = DD9_HAND1 nimpute = 20 out = mi_mvn seed = 54321;
	mcmc plots = trace plots = acf;
	var month area angle fu sex;
run;


/******************************************************************************/
/***************************** The imputed models *****************************/
/******************************************************************************/
TITLE 'BY IMPUTATION';
ODS OUTPUT SolutionF=EST CovB=VAR_ESTIM covparms = cvparms;
proc mixed data = mi_mvn method = reml covtest;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE MONTH SEX 
	 /solution covb ddfm=satterthwaite;
	RANDOM INT MONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
	BY _IMPUTATION_;
run;

/******************************************************************************/
/****************************** The mi analysis *******************************/
/******************************************************************************/

/* For the fixed effects */
proc mianalyze parms = EST covb(effectvar=rowcol) = VAR_ESTIM;
	CLASS CAGE sex;
	modeleffects intercept CAGE SEX month;
run;

/* For the covariance parameters */
data cvparms;
	set cvparms;
	covparm2=covparm||subject;
run;

proc sort data=cvparms;
	by covparm2;
run;

proc mianalyze data=cvparms;
	by covparm2;
	modeleffects estimate;
	stderr stderr;
run;