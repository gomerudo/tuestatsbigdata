/** Load practice exam data**/
LIBNAME DPLib '/folders/myfolders/';
/** name assigned to dataset **/

proc import datafile="/folders/myfolders/dp_hand1.csv"
     out=dpdata
     dbms=csv
     replace;
run;

proc print;
run;

proc sort data=DPData;                                                                                                                    
   by ID FU;                
run;                                                                                                                                    

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

/** We use a marginal model to create a mean time profile **/


/** First, we start by selecting candidate covariance structures **/
proc mixed data=DPData method=reml;
	class ID FU SEX AGE0 SMOKE ALC INJ REL DIA EPI LIV;
	model AREA=MONTH SEX AGE0 SMOKE ALC INJ REL DIA EPI LIV 
			/solution ddfm=satterthwaite;
	repeated FU/subject=ID type=UN rcorr;
	ods output covparms=COV rcorr=CORR;
run;
