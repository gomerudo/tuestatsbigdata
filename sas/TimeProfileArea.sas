proc import datafile="/folders/myfolders/dp_hand1_8fu.csv"
     out=RAWDATAHAND1
     dbms=csv
     replace;
run;

proc sort data=RAWDATAHAND1;                                                                                                                    
   by ID FU;                
run;                                                                                                                                    

/**
Covariances cannot be computed for FU=11. 
Hence, we want to work with entries that contain 8 measurements.
**/
DATA DATAHAND1;
	SET RAWDATAHAND1;
	IF FU > 7 	
	THEN DELETE;
RUN;


/**
Baseline variables:
SEX: doesn't change with time
AGE: change with time
SMOKING: might change with time
ALCOHOL: might change with time
Past injuries at hand: might change with time
Do direct relatives have DD: might change with time
Diabetes: might change with time
Epilepsy: might change with time
Liver disease: might change with time
**/

/** This is the best model we found for hand1.**/
proc mixed data=DPSubdata method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX 
	 /solution ddfm=satterthwaite;
	 RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;


/** We can run the same model for hand2.**/
proc import datafile="/folders/myfolders/dp_hand2_8fu.csv"
     out=RAWDATAHAND2
     dbms=csv
     replace;
run;

proc sort data=RAWDATAHAND2;                                                                                                                    
   by ID FU;                
run;                                                                                                                                    


DATA DATAHAND2;
	SET RAWDATAHAND2;
	IF FU > 7 	
	THEN DELETE;
RUN;


/** THE BEST MODEL FOR HAND TWO IS GIVEN BY **/
proc mixed data=SubDataHand2 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX 
	 /solution ddfm=satterthwaite;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;


/** Let's take a look at the residuals and construct the models **/
/** residuals for first model **/
proc mixed data=DATAHAND1 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX  
    /solution ddfm=satterthwaite OUTP=PREDHAND1;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

PROC UNIVARIATE DATA=PREDHAND1 NORMAL;
VAR RESID;
RUN;
/**
Tests for Normality
Test 	                    Statistic 	p Value
Shapiro-Wilk 	    W 	    0.927576 	Pr < W 	<0.0001
Kolmogorov-Smirnov 	D 	    0.102054 	Pr > D 	<0.0100
Cramer-von Mises 	W-Sq 	4.048619 	Pr > W-Sq 	<0.0050
Anderson-Darling 	A-Sq 	20.78971 	Pr > A-Sq 	<0.0050
**/

/** Let's check the influential values **/

proc mixed data=DATAHAND1 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX  
    /solution ddfm=satterthwaite 	
    RESIDUAL INFLUENCE(ITER=5 EFFECT=ID EST);
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

/**
Subjects with id 95 and 36 seem to be the most influential in the data.
**/

DATA NOINFLUENTIALHAND1;
SET DATAHAND1;
IF ID=95 then delete;
IF ID=36 THEN DELETE;
run;


proc mixed data=NOINFLUENTIALHAND1 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX  
    /solution ddfm=satterthwaite OUTP=PRED2HAND1;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

PROC UNIVARIATE DATA=PRED2HAND1 NORMAL;
VAR RESID;
RUN;
/**
Tests for Normality
Test 				Statistic 			p Value
Shapiro-Wilk 		W 	0.923944 		Pr < W 	<0.0001
Kolmogorov-Smirnov 	D 	0.106085 		Pr > D 	<0.0100
Cramer-von Mises 	W-Sq 	4.178459 	Pr > W-Sq 	<0.0050
Anderson-Darling 	A-Sq 	21.36484 	Pr > A-Sq 	<0.0050
**/


/** residuals for second model **/
proc mixed data=DATAHAND2 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX 
	 /solution ddfm=satterthwaite outp=PREDHAND2;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

PROC UNIVARIATE DATA=PREDHAND2 NORMAL;
VAR RESID;
RUN;
/**
Tests for Normality
Test 	Statistic 	p Value
Shapiro-Wilk 	W 	0.872374 	Pr < W 	<0.0001
Kolmogorov-Smirnov 	D 	0.102788 	Pr > D 	<0.0100
Cramer-von Mises 	W-Sq 	4.851706 	Pr > W-Sq 	<0.0050
Anderson-Darling 	A-Sq 	26.65194 	Pr > A-Sq 	<0.0050
**/

proc mixed data=DATAHAND2 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX  
    /solution ddfm=satterthwaite 	
    RESIDUAL INFLUENCE(ITER=5 EFFECT=ID EST);
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;
/** Subjects with id 165, 170, and 205 seem to be the most influential. **/

DATA NOINFLUENTIALHAND2;
SET DATAHAND2;
IF ID=165 then delete;
IF ID=170 THEN DELETE;
IF ID=205 THEN DELETE;
run;

proc mixed data=NOINFLUENTIALHAND2 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model AREA=CAGE NORMONTH SEX  
    /solution ddfm=satterthwaite OUTP=PRED2HAND2;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;

PROC UNIVARIATE DATA=PRED2HAND2 NORMAL;
VAR RESID;
RUN;

/**
Tests for Normality
Test 	Statistic 	p Value
Shapiro-Wilk 	W 	0.931687 	Pr < W 	<0.0001
Kolmogorov-Smirnov 	D 	0.082147 	Pr > D 	<0.0100
Cramer-von Mises 	W-Sq 	2.837088 	Pr > W-Sq 	<0.0050
Anderson-Darling 	A-Sq 	15.93045 	Pr > A-Sq 	<0.0050
**/

