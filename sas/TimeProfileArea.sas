

proc mixed data=DATAHAND1 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model ANGLE=CAGE NORMONTH SEX SMOKE ALC INJ REL DIA EPI LIV 
	SMOKE*NORMONTH ALC*NORMONTH INJ*NORMONTH REL*NORMONTH 
	DIA*NORMONTH EPI*NORMONTH LIV*NORMONTH /solution ddfm=satterthwaite;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;


proc mixed data=DATAHAND2 method=reml;
	class ID SEX(REF="0") FU(REF="0") CAGE(REF="0") SMOKE(REF="0")
	ALC(REF="0") INJ(REF="0") REL(REF="0") DIA(REF="0") 
	EPI(REF="0") LIV(REF="0");
	model ANGLE=CAGE NORMONTH 
	 /solution ddfm=satterthwaite;
	RANDOM INT NORMONTH /TYPE=UN SUBJECT=ID G V;
	repeated FU/subject=ID type=ar(1) rcorr;
	ods output covparms=COV rcorr=CORR;
run;
