* Written by: 	P.Sanchez ;
* Date: 		3/31/2015 ;
* Description:  ;
* Update(s):	
	03.31.15	PS	-Adding code that merges 14 tables. Temporarily updating VISIT and
					BDVISIT.
	04.28.15	PS	-Adding 'medication database' code. Adding HAR/LAR SES libraries.
	05.05.15	PS	-Fixing country code.
	05.06.15	PS	-Fixing country code. Updating path for 'w' data set.
	05.07.15	PS	-Updating 'new visits' database. No longer using Access database.
	05.08.15	PS	-Updating study variable.
	05.11.15	PS	-Not using DRS data sets any more (using _all data sets). Updating 
					QINC filenames. Updated paths.
	06.08.15	PS	-FINAL.Participants_Medications now includes all cities.
	06.24.15	PS	-Removing hard coding.
	06.25.15	PS	-Using libnames U instead of Y. Removing 'proc freqs' to
					speed up program.
	07.10.15	PS	-Updating code for 'cvdmed', 'pare_med', 'any_diab_med', 'oral_med'.
					Merging by BDVISIT.
	07.29.15	PS	-Adding 5 pediatric tables. Removing code (if pediatric=1 then 
					study = 'pediatric').
	11.30.15	ES	-Reduced Code, optimized, cleaned.
	12.14.15	PS	-Update in Code Final.Participants_Medications
	07/11/16	ES	-Added new Tables A-Z from Database also keep consistency 
					over naming Folders in all programs.

;

OPTIONS NOCENTER;
OPTIONS NOFMTERR;

*======================================================================================;
%LET DATA1 = SES_BRO1; 					  %LET DATA2=SES_HAR1; 	  %LET DATA3= SES_LAR1;
%LET SET1 = SES_BRO.tracts_blocks_ses1234;%LET SET2 =SES_HAR.hrl4;%LET SET3 = SES_LAR.lrd5;

*======================================================================================;
%LET VAR1 = BIRTCITY;	%LET VAR2 = BIRTHST;		%LET VAR3 = FBIRST;		
%LET VAR4 = MBIRST;		%LET VAR5 = FFBIRST;		%LET VAR6 = FMBIRST;	
%LET VAR7 = MFBIRST;	%LET VAR8 = MMBIRST;		%LET VAR9 = HISPANIC_OTH;
%LET VAR10 = RACE_OTH;	%LET VAR11 = BIRTCTRY;		%LET VAR12 = FBIRCTRY;	
%LET VAR13 = MBIRCTRY;	%LET VAR14 = FFBIRCTR;		%LET VAR15 = FMBIRCTR;	
%LET VAR16 = MFBIRCTR;	%LET VAR17 = MMBIRCTR;		%LET VAR18 = SCHCTRY;
%LET VAR19 = schcity;
*======================================================================================;
%LET FIX1 = &&VAR1;		%LET FIX2 = &&VAR18;	%LET FIX3 = &&VAR12;
%LET FIX4 = &&VAR13;	%LET FIX5 = &&VAR14;	%LET FIX6 = &&VAR15;
%LET FIX7 = &&VAR16;	%LET FIX8 = &&VAR17;	%LET FIX9 = &&VAR8;
*======================================================================================;

*======================================================================================;
%MACRO CS_DATA;
data CS; set RAW.CONTACTSCHEDULINGPAT_converted;
*ADDED BY PS BECAUSE ALL RECORDS WILL BE CHANGED BY THIS PROGRAM(MAY REMOVE LATER 3.13.15);
IF SUBSTR(RRID,1,2) IN ('BD' 'HD' 'LD'); 
LENGTH TRACT 8 BLOCK 4;
DROP TRACT_S BLOCK_S;
TRACT_s=SUBSTR(KEY7,3,6);
TRACT=TRACT_S;
BLOCK_s=SUBSTR(KEY7,9,4);
BLOCK=BLOCK_S;

* Here I look for the last patient result;
array ctres (*) ctres1 ctres2 ctres3 ctres4 ctres5;
array ctdate (*) ctdate1 ctdate2 ctdate3 ctdate4 ctdate5;

do i=1 to 5;
   if year(ctdate(i))=9999 then ctdate(i)=.;
end;

PATIENT_RESULT=.;
date_patresult=.;
PATIENT_RESULT1p=.;
date_patresult1p=.;
lastcontact1p=.;
PATIENT_RESULT2p=.;
date_patresul2p=.;
lastcontact2p=.;
lastcontactpage='  ';

do i=5 to 1 by -1;
	if ctres (i) > 0 then
	do;
      	PATIENT_RESULT1p=ctres(i);
      	date_patresult1p=ctdate(i);
	  	lastcontact1p= i; *last time contacted in page1;
	  	i=1;
   	end;
end;

* Here it checks the result on the second page of Contact ;
array cores (*) co1res co2res co3res  co4res  co5res  co6res  co7res 
                co8res co9res co10res co11res co12res co13res co14res;
array codate (*) co1date co2date co3date  co4date  co5date  co6date  co7date
                 co8date co9date co10date co11date co12date co13date co14date;

do i=1 to 14;
	if year(codate(i)) = 9999 then codate(i)=.;
end;

do i=14 to 1 by -1;
	if cores (i) > 0 then
	do;
    	PATIENT_RESULT2p=cores(i);
      	date_patresul2p=codate(i);
	  	lastcontact2p= i; *last time contacted in page2;
	  	i=1;
   end;
end;

if year(DATEOGTT) = 9999 then DATEOGTT=.;

* Here it decides with wich last result code and date result is for the patient;
if PATIENT_RESULT1p = PATIENT_RESULT2p then 
	if date_patresult1p >= date_patresul2p then
		do;
		    PATIENT_RESULT=PATIENT_RESULT1p;
			date_patresult=date_patresult1p;
			lastcontactpage='P1';
			lastcontact=lastcontact1p;
		end;
	else
     	do; 
			PATIENT_RESULT=PATIENT_RESULT2p;
		 	date_patresult=date_patresul2p;
		 	lastcontactpage='P2';
		 	lastcontact=lastcontact2p;
	  	end;

if PATIENT_RESULT1p > 0 then 
	if PATIENT_RESULT2p > 0  then 
		if date_patresult1p > date_patresul2p then
			do;
		    	PATIENT_RESULT=PATIENT_RESULT1p;
				date_patresult=date_patresult1p;
				lastcontactpage='P1';
				lastcontact=lastcontact1p;
			end;
		else
			do;
			    PATIENT_RESULT=PATIENT_RESULT2p;
				date_patresult=date_patresul2p;
				lastcontactpage='P2'; 
				lastcontact=lastcontact2p;
			end;
		else
			do;
				PATIENT_RESULT=PATIENT_RESULT1p;
				date_patresult=date_patresult1p;
				lastcontactpage='P1';
				lastcontact=lastcontact1p;
			end;
	else
    	if PATIENT_RESULT2p > 0  then 
	   		do;
		  		PATIENT_RESULT=PATIENT_RESULT2p;
		  		date_patresult=date_patresul2p;
		  		lastcontactpage='P2';
		  		lastcontact=lastcontact2p;
       		end;
		else
	    	do;
		  		PATIENT_RESULT=0;
		  		date_patresult=0;
        	end;
			*********************************************************;
patient_Type=.;
id_patient=substr(rrid,1,2);
if id_patient = 'BD' then
	if random=1 then 
    	patient_Type=1; *bd STUDY;
	else if random=2 then
		patient_Type=2; *COHORT STUDY;
if id_patient='RE' then
	if random=1 then
    	patient_Type=4; *REFUSALS from Impact Study;
	else if random=2 then 
	    patient_type=3; *REFUSALS from Cohort Study;

drop date_patresul2p date_patresult1p PATIENT_RESULT2p PATIENT_RESULT1p
	 id_patient i /*CTID1*/;
PROC SORT; BY TRACT BLOCK;
RUN;
%MEND;

*=====================================================================================;
%MACRO BRO_HAR_LAR;
%DO I = 1 %TO 3;
	DATA &&DATA&I.; SET &&SET&I.; TOWN = &I.; KEEP TRACT BLOCK QINC TOWN; RUN;
%END;

DATA SES1; SET SES_BRO1 SES_HAR1 SES_LAR1; 
PROC SORT OUT=SES2; BY TRACT BLOCK;
RUN; *USING ALL LOCATIONS (PS 4.28.15);

%MEND;
*====================================================================================;
%MACRO PROC_SQL(TABLE_NAME,FROM,AS,LEFT_JOIN,AS2,ON1,ON1A, ON2,ON2B,FLAG);
PROC SQL noprint;	/*Added noprint ES 6/22/2017*/
	CREATE TABLE &TABLE_NAME AS
	SELECT * FROM &FROM AS &AS 
	LEFT JOIN &LEFT_JOIN AS &AS2
	ON &ON1=&ON1A 
	%IF &FLAG EQ 1 %THEN 
	%DO;
	AND &ON2=&ON2B
	%END;
	;
QUIT;

%MEND;
*==================================================================================;
%MACRO CS_SES2;
DATA CS_SES2; SET CS_SES1; DROP QINC;
CLUS=TRACT*BLOCK; *DR. MONIR DEFINED THIS VARIABLE TO USE IN SURVEY PROCEDURES;
QUINC=QINC;
SES = QINC;
proc sort out=CS_SES3; by KEY7 LINENUM;
RUN;

DATA MISSING_QUINC; SET CS_SES2; IF QUINC = . OR SES = .; RUN;
/*
TITLE1 'CHECKING SES, RRID, KEY7, TRACT, BLOCK...CLUSTER MISSING';
PROC FREQ DATA=CS_SES2; TABLE RRID KEY7 TRACT BLOCK CLUS; WHERE CLUS = .; RUN;
*/
%MEND;
*===================================================================================;
%MACRO PROC_SORT(SORT_DATA, OUT_DATA,KD, K1,K2,K3,K4,K5,EXTRA,BY);

PROC SORT DATA=&SORT_DATA /*enumeration*/  
	OUT=&OUT_DATA    (&KD=&K1 &K2 &K3 &K4 &k5/*KEY8*/ /*LINENUM*/ )&EXTRA; 
BY &BY; RUN; *HIDING KEY8-PS 5.5.14;

%MEND;
*===================================================================================;
%MACRO CSLIM;
DATA CSLIM1; MERGE ENUMER (IN=A) HHCS; IF A; BY KEY7; RUN;

*NEXT LINE ADDED BY PS 2.2.15;
DATA CSLIM2; SET CSLIM1 (RENAME=(HHPAGE2_LINENUM=LINENUM));
PROC SORT; BY KEY7 LINENUM;
RUN;

/*
DATA CSLIM1; SET HH.CONTACTSCHEDULING; 
KEEP KEY7 LINENUM INOUT SECTION SUBDIV TOWN DWELLING; 
IF LINENUM NE .;
RUN; 
*NOTE:The data set WORK.CSLIM1 has 646 observations and 7 variables.;*03.20.2014 0317PM-PS;
*/
*UPDATED BY PS 2.2.15;
DATA CS_FULL1; MERGE CS_SES3 (IN=A) CSLIM2; IF A; BY KEY7 LINENUM; 
IF SUBSTR(RRID,1,1) IN ('B' 'H' 'L'); 
RUN; 
%MEND;
*====================================================================================;
%MACRO ANTH;
DATA TEMPANTH1; SET RAW.anth_converted /*L2.anth_converted*/; 
KEEP RRID HEIGHT ANTH_EXAMDATE; 
IF HEIGHT IN (999 999.9) THEN HEIGHT = .;
PROC SORT; BY RRID ANTH_EXAMDATE;
RUN;

DATA CHECK1; SET TEMPANTH1; IF RRID = 'BD1943';RUN;

DATA TEMPANTH2; SET TEMPANTH1; 
RETAIN HOLDHEIGHT;
IF FIRST.RRID THEN HOLDHEIGHT = .;
IF HEIGHT NE . THEN HOLDHEIGHT = HEIGHT;
BY RRID;
OUTPUT;
RUN;

DATA TEMPANTH3; SET TEMPANTH2; DROP HEIGHT HOLDHEIGHT;
NEWHEIGHT = HOLDHEIGHT;
IF HOLDHEIGHT NE .;
PROC SORT OUT=TEMPANTH4 NODUPKEY; BY RRID ANTH_EXAMDATE;
RUN;

DATA RAW.HEIGHTS; SET TEMPANTH4; RUN;
*NOTE: The data set WORK.ANTH3 has 6089 observations and 3 variables.;

DATA ANTH1; SET RAW.ANTH_CONVERTED; DROP HEIGHT; RUN;
%MEND;
*===================================================================================;
%MACRO ANTH3;
DATA ANTH3; SET ANTH2; DROP NEWHEIGHT; HEIGHT = NEWHEIGHT; RUN;

DATA RAW.ANTH_CONVERTED2; SET ANTH3; RUN;
%MEND;
*===================================================================================;
%MACRO CONVERTED2;
DATA RAW.IDD_converted2; MERGE IDD1 IDENTIFIERS; BY RRID; 
DROP GENDER DOB BIRTCITY FBIRCITY MBIRCITY FFBIRCIT FMBIRCIT MFBIRCIT SCHCITY 
FFBIRCIT FMBIRCIT MFBIRCIT MMBIRCIT OCCLIFDE OCCMOSTL HISPANIC HISPANIC_OTH RACE
RACE_OTH BIRTCTRY BIRTHST SCHCOL SCHCTRY SCHGRAD SCHTECH SCHPREC SCHST FBIRCTRY 
FBIRST FFBIRCTR FFBIRST FMBIRCTR FMBIRST MBIRCTRY MBIRST MFBIRCTR MFBIRST MMBIRCTR
MMBIRST;
*;
YROB = YEAR(DOB);
AGE_AT_VISIT = floor((INTERVIEW_DATE-DOB)/365.25); 
if RRID = "BD0072" and VISIT = 7 then AGE_AT_VISIT = 65;
PROC SORT; BY RRID VISIT; 
RUN;

DATA CSPT1; SET CS; IF PATIENT_TYPE NE .; RUN;
%MEND;
*===================================================================================;
%MACRO CLEARING_COUNTRY_VAR;
%DO J=1 %TO 11;
&&VAR&J.=UPCASE(STRIP(&&VAR&J.));
%END;

%DO K=11 %TO 19;
&&VAR&K.=UPCASE(COMPRESS(STRIP(&&VAR&K.),"."));
%END;
%MEND;
*===================================================================================;
%MACRO REPLACE_1_NA_9_UNK;
%DO I=3 %TO 8;
IF &&VAR&I. in('-1', 'N/A', '9', '999', '9999', 'UNKNOWN') THEN  &&VAR&I. = '';
%END;

%DO J=12 %TO 18;
IF &&VAR&J. in('-1', 'N/A', '9', '999', '9999', 'UNKNOWN') THEN  &&VAR&J. = '';
%END;

%MEND;
*====================================================================================;
%MACRO REPLACE(FIX,FLAG);
%IF &FLAG NE 9 %THEN
%DO;
IF &FIX = 'ALEMANIA' THEN &FIX = 'GERMANY';
IF &FIX = 'BELICE' THEN &FIX = 'BELIZE';
IF &FIX = 'ESPANA' THEN &FIX = 'SPAIN';
IF &FIX = 'FRANCIA' THEN &FIX = 'FRANCE';
IF &FIX = 'HOLAND' THEN &FIX = 'HOLLAND';
IF &FIX = 'ITALIA' THEN &FIX = 'ITALY';
IF &FIX = 'PUERTORICO' THEN &FIX = 'PUERTO RICO';
IF &FIX = 'S KOREA' THEN &FIX = 'SOUTH KOREA';
IF &FIX = 'S KOREA' THEN &FIX = 'SOUTH KOREA';
IF &FIX = 'SALVADOR' THEN &FIX = 'EL SALVADOR';
IF &FIX = 'SALVADOR' THEN &FIX = 'EL SALVADOR';
IF &FIX = 'SICILI' THEN &FIX = 'SICILY';
IF &FIX = 'NICARGUA' THEN &FIX = 'NICARAGUA';
IF &FIX = 'HONDURA' THEN &FIX = 'HONDURAS';
%END;

%IF &FLAG LE 4 %THEN
%DO;
IF &FIX = 'US`' THEN &FIX = 'USA';
IF &FIX = 'WEBB' THEN &FIX = 'USA';
IF &FIX = 'LIBIA' THEN &FIX = 'LIBYA';
%END;

%IF &FLAG EQ 3 %THEN
%DO;
IF FBIRCTRY = 'DOMINICAN REPUBLIUE' THEN FBIRCTRY = 'DOMINICAN REPUBLIC';
%END;

%IF &FLAG EQ 4 %THEN
%DO;
IF MBIRCTRY = 'ARGENTINS' THEN MBIRCTRY = 'ARGENTINA';
%END;

%IF &FLAG EQ 9 %THEN
%DO;
IF MMBIRST = 'TEXAS' THEN MMBIRCTR = 'USA';
IF MFBIRST = 'SPAIN' THEN MFBIRCTR = 'SPAIN';
*IF FBIRST = 'MANAGUA' THEN FBIRCTRY = 'NICARAGUA';
IF MBIRST = 'MANAGUA' THEN MBIRCTRY = 'NICARAGUA';
IF MBIRCITY = 'HONDURAS' THEN MBIRCTRY = 'HONDURAS';
IF SCHCITY = 'HONDURAS' THEN SCHCTRY = 'HONDURAS';
IF BIRTCITY = 'HONDURAS' THEN BIRTCTRY = 'HONDURAS';
IF FBIRCITY = 'NICARAGUA' THEN FBIRCTRY = 'NICARAGUA';
IF FFBIRCIT = 'NICARAGUA' THEN FFBIRCTR = 'NICARAGUA';
IF FFBIRCIT = 'HONDURAS' THEN FFBIRCTR = 'HONDURAS';
IF FMBIRCIT = 'NICARAGUA' THEN FMBIRCTR = 'NICARAGUA';
IF FMBIRCIT = 'HONDURAS' THEN FMBIRCTR = 'HONDURAS';
%END;

%MEND;

*====================================================================================;
%MACRO FIX;
%DO I=1 %TO 9;
%REPLACE(&&FIX&I.,&I.)
%END;
%MEND;
*===================================================================================;
%MACRO IF_REPLACE;

**********************************************************************************;
IF BIRTHST = 'TEXAS' THEN BIRTCTRY = 'USA';
IF SCHCITY = 'GUADALAJARA' THEN SCHCTRY = 'MEXICO';
if schcity in ('MATAMOROS' 'VALLE HERMOSA' 'VERACRUZ') 	then SCHCTRY='MEXICO';
IF SCHST IN ('TAMAULIPAS' 'NUEVO LEON') 			THEN SCHCTRY = 'MEXICO';
IF MMBIRST = 'TAMALIPAS' 						THEN MMBIRST = 'TAMAULIPAS'; 
*IF SCHST = 'MEXICO' 							THEN SCHST = 'MEXICO DF';
IF BIRTCTRY IN ('UNITED STAES') 				THEN BIRTCTRY = 'USA   ';
IF MFBIRCTR = 'US`' 							THEN MFBIRCTR = 'USA   ';
*IF SCHCTRY = '-1'								THEN SCHCTRY = '';
*CITY = IMBODEN, STATE=VIRGINIA, COUNTRY=WISE;
IF BIRTHST = 'VIRGINIA' AND BIRTCTRY = 'WISE' 	THEN BIRTCTRY = 'USA   '; 
*IF FBIRCTRY = '999' 							THEN FBIRCTRY = '      ';

%DO K=3 %TO 8;		*	UPDATED REDUCED	;
IF &&VAR&K. = 'TX' 								THEN &&VAR&K. = 'TEXAS';
IF &&VAR&K. = 'MX' 								THEN &&VAR&K. = 'MEXICO';
%END;

IF FBIRST = 'TEXAS' AND FBIRCTRY NE 'USA' 		THEN FBIRCTRY = 'USA   ';

%DO L=11 %TO 18;	*	UPDATED REDUCED	;
IF &&VAR&L. IN ('MX' 'MEX')				THEN &&VAR&L. = 'MEXICO';
IF &&VAR&L. IN ('US' 'UNITED STATES') 	THEN &&VAR&L. = 'USA   ';
%END;


***********************************************************************************;
*UPDATED (PS 5.23.14);
MAJSCHT=.;
if schctry IN ('MEXICO') THEN MAJSCHT =1;
else if schctry IN ('USA') THEN MAJSCHT=2;
**********************************************************************************;
*UPDATED 6.27.14;
CTRYBIR=.;
if BIRTCTRY IN ('MEXICO') then CTRYBIR=1;
else if BIRTCTRY IN ('USA') THEN CTRYBIR=2;

BORN_MX = .;
BORN_US = .; 

if CTRYBIR = 1 then 
	do;
		BORN_MX=1;
		BORN_US=0;
	end;
if BORN_MX = . and BIRTCTRY NOT IN ('MEXICO' '') then BORN_MX=0;

if CTRYBIR = 2 then BORN_US=1;
if BORN_US = . and BIRTCTRY NOT IN ('USA' '') then BORN_US=0;

*********************************************************************************;
*UPDATED 3.23.15;
BOTH_BORN_MX=.;

if FBIRCTRY = 'MEXICO' AND MBIRCTRY = 'MEXICO' 			then BOTH_BORN_MX=1; 
else if FBIRCTRY IN ('') OR MBIRCTRY IN ('') 			then BOTH_BORN_MX=.; 
	 else if FBIRCTRY NOT IN ('') AND MBIRCTRY NOT IN ('')	then BOTH_BORN_MX=0; 
*else BOTH_BORN_MX = 0; 	

*********************************************************************************;
*UPDATED 3.23.15;
BOTH_BORN_US=.;

if FBIRCTRY = 'USA' AND MBIRCTRY = 'USA'	 			then BOTH_BORN_US = 1;
else if FBIRCTRY IN ('') OR MBIRCTRY in ('') 			then BOTH_BORN_US = .;
	 else if FBIRCTRY NOT IN ('') AND MBIRCTRY NOT IN ('')	then BOTH_BORN_US = 0;
*else BOTH_BORN_US = 0;

*********************************************************************************;
*UPDATED 6.27.14;
BOTHMEXICO = .;
IF BOTH_BORN_MX	= 1 		THEN BOTHMEXICO = 1; *BOTH BORN IN MEXICO;
ELSE IF BOTH_BORN_US = 1 	THEN BOTHMEXICO = 2; *BOTH BORN IN USA;

*********************************************************************************;
*UPDATED 6.27.14*;
FourMexico=.;

if FFBIRCTR IN('MEXICO') and FMBIRCTR IN ('MEXICO') and 
   MFBIRCTR IN('MEXICO') and MMBIRCTR IN ('MEXICO')then FourMexico=1;*ALL 4 BORN IN MEXICO;

else if FFBIRCTR in ('USA') and FMBIRCTR in ('USA') and 
	MFBIRCTR in ('USA') and MMBIRCTR in ('USA') then FourMexico=2; *ALL 4 BORN IN USA;

*********************************************************************************;

IF VISIT = 1 AND RRID NE '';

RUN;
%MEND;
*-------------------------------------------------------------------------------;
%MACRO RAW_IDD;
PROC SORT DATA=RAW.IDD_converted OUT=IDDCONV1; BY RRID VISIT; RUN;
PROC SORT DATA=IDDCONV1 OUT=IDDCONV2 NODUPKEY; BY RRID; RUN;
******************************************************************************************;
DATA CONSTANTS1; MERGE IDDCONV2 (IN=A) CSPT2; IF A; BY RRID; RUN;

DATA RAW.IDD_CONSTANTS; SET CONSTANTS1;
keep rrid GENDER BIRTCITY FBIRCITY MBIRCITY FFBIRCIT FMBIRCIT MFBIRCIT SCHCITY FFBIRCIT
FMBIRCIT MFBIRCIT MMBIRCIT OCCLIFDE OCCMOSTL HISPANIC HISPANIC_OTH RACE RACE_OTH BIRTCTRY
BIRTHST SCHCOL SCHCTRY SCHGRAD SCHTECH SCHPREC SCHST FBIRCTRY FBIRST FFBIRCTR FFBIRST 
FMBIRCTR FMBIRST MBIRCTRY MBIRST MFBIRCTR MFBIRST MMBIRCTR MMBIRST SCHOOL EDUCLVL LEVEL_EDU
YEARS_EDU BORN_MX BORN_US CTRYBIR BOTH_BORN_MX BOTH_BORN_US BOTHMEXICO MAJSCHT FourMexico;

array A1(*) $ _character_ ;
do i=1 to dim(A1);
	A1(i)=UPCASE(STRIP(A1(i)));
	if A1(i) in('-1' 'N/A' '9' '999' '9999' '99999' '999999'
				'99999999' 'UNKNOWN' '9`' 'UNNOWN') 
	then A1(i) = '';
end;

array A2(*) _numeric_;
do i=1 to dim(A2);
	if A2(i) in (-1) then A2(i) = .;
end;

IF HISPANIC = 9 THEN HISPANIC = .;
IF RACE = 9 THEN RACE = .;

*TO CALCULATE YEARS OF EDUCATION 
08/22/2007(this code comes from Clearing_database(Impact));

IF SCHPREC = 99 THEN SCHPREC = .; *NEW CODE BY PS (4.1.15);

YEARS_EDU=0;
SCHCOL1=SCHCOL;
schtech1=schtech;
schprec1=schprec;
schgrad1=schgrad;

if schgrad > 0 then 
	do;
		if SCHCOL=0 then SCHCOL1=4;
		if schtech=0 then schtech1=4;
		if schprec=0 then schprec1=12; 
	end;

if schgrad =0 and SCHCOL > 0 then 
	do;
		if schtech=0 then schtech1=4;
		if schprec=0 then schprec1=12;
	end;

if schgrad =0 and SCHCOL =0 and schtech > 0 then 
	do;
		if schprec=0 then schprec1=12;
	end;

YEARS_EDU=schgrad1+SCHCOL1+schtech1+schprec1;

SCHOOL=.;
if . < years_edu <= 8 then school=1;
else if years_edu > 8 then school=2; 

IF 0 <= YEARS_EDU < 12 THEN 
	do;
		LEVEL_EDU=1; 
	end; 
ELSE IF YEARS_EDU >= 12 THEN 
	DO;
		LEVEL_EDU=2;
	end; 
ELSE LEVEL_EDU=.; 

EDUCLVL=.;
IF SCHPREC  => 0 THEN EDUCLVL=SCHPREC;
*IF SCHTECH 	=> 0 THEN EDUCLVL=SCHTECH + 12;
*IF SCHCOL 	=> 0 THEN EDUCLVL=SCHCOL + 12;
*IF SCHGRAD 	=> 0 THEN EDUCLVL=SCHGRAD + 16;

*NEXT 3 LINES UPDATED BY PS 8.12.14;
IF SCHTECH 	=> 1 THEN EDUCLVL=SCHTECH + 12;
IF SCHCOL 	=> 1 THEN EDUCLVL=SCHCOL + 12;
IF SCHGRAD 	=> 1 THEN EDUCLVL=SCHGRAD + 16;

*UPDATING 4 EDUCATION VARIABLES (PS 3.6.15)**************************************;
*ELEVEL = '              ';
IF SCHPREC=0 AND SCHTECH=0 AND SCHCOL=0 AND SCHGRAD=0 THEN 
	DO;
		*ELEVEL = 'ZERO';
		SCHTECH=.;
		SCHCOL=.;
		SCHGRAD=.;
	END;
IF SCHPREC>0 AND SCHTECH=0 AND SCHCOL=0 AND SCHGRAD=0 THEN 
	DO;
		*ELEVEL = 'K-12';
		SCHTECH=.;
		SCHCOL=.;
		SCHGRAD=.;
	END;
IF SCHPREC=0 AND SCHTECH>0 AND SCHCOL=0 AND SCHGRAD=0 THEN 
	DO;
		*ELEVEL = 'TECH';
		SCHPREC=.;
		SCHCOL=.;
		SCHGRAD=.;
	END;
IF SCHPREC=0 AND SCHTECH=0 AND SCHCOL>0 AND SCHGRAD=0 THEN 
	DO;
		*ELEVEL = 'COLLEGE';
		SCHPREC=.;
		SCHTECH=.;
		SCHGRAD=.;
	END;
IF SCHPREC=0 AND SCHTECH=0 AND SCHCOL=0 AND SCHGRAD>0 THEN 
	DO;
		*ELEVEL = 'GRAD';
		SCHPREC=.;
		SCHCOL=.;
		SCHTECH=.;
	END;

%CLEARING_COUNTRY_VAR;
%REPLACE_1_NA_9_UNK;
%FIX;
%if_replace;
%MEND;
*===================================================================================;


%MACRO DATA_FREQ_CS8_CONTACTSCH;
DATA RAW.CONTACTSCHEDULINGPAT_converted2; SET CS8; RUN;
/*
TITLE1 'CS8...STUDY';
PROC FREQ DATA=CS8; WHERE STUDY=''; RUN;

TITLE1 'CONTACTSCHEDULINGPAT_converted2...STUDY';
PROC FREQ DATA=RAW.CONTACTSCHEDULINGPAT_converted2; TABLE STUDY; RUN;
*/
%MEND;


/*
proc sql;
create table mbugtest as
select *
from Raw.lab_with_crl as ekg
left join raw.ekg_converted as lab
on ekg.bdvisit = lab.bdvisit and ekg.study = lab.study;
quit;

data mbug;set merged1; keep bdvisit edta3;
run;
*/
/*
ekg_converted only .
LAB_with_crl has text

*/

%MACRO MERGE_RAW_TABLES_SQL;
PROC SQL noprint; /*Added noprint ES 6/22/2017*/
	CREATE TABLE MERGED1 AS
	SELECT * FROM CS8 AS CS
	LEFT JOIN RAW.IDD_converted2 AS i
		ON CS.BDVISIT = i.BDVISIT
	LEFT JOIN RAW.ANTH_converted2 AS A
		ON CS.BDVISIT = A.BDVISIT
	LEFT JOIN RAW.dmhxmed_converted AS D
		ON CS.BDVISIT = D.BDVISIT
	LEFT JOIN RAW.ekg_converted AS EK
		ON CS.BDVISIT = EK.BDVISIT
	LEFT JOIN RAW.exitcoverform_converted AS EX
		ON CS.BDVISIT = EX.BDVISIT
	LEFT JOIN RAW.familyhx_converted AS F
		ON CS.BDVISIT = F.BDVISIT
	LEFT JOIN RAW.LAB_converted AS L
		ON CS.BDVISIT = L.BDVISIT
	LEFT JOIN RAW.mhxmed_converted AS M
		ON CS.BDVISIT = M.BDVISIT
	LEFT JOIN RAW.smokedrink_converted AS S
		ON CS.BDVISIT = S.BDVISIT
	LEFT JOIN RAW.Welchbp_converted AS W
		ON CS.BDVISIT = W.BDVISIT
	LEFT JOIN RAW.BP_converted AS Z
		ON CS.BDVISIT = Z.BDVISIT
/*	PENDING IMPACT TABLES	*/
	LEFT JOIN WORK.IMPACTEAN_converted AS IEAN
		ON CS.BDVISIT = IEAN.BDVISIT
	LEFT JOIN WORK.IMPACTSP_converted AS ISP
		ON CS.BDVISIT = ISP.BDVISIT
	
/* ADDED ON 7.29.15 */
LEFT JOIN RAW.CTQ_converted AS PED1
		ON CS.BDVISIT = PED1.BDVISIT
LEFT JOIN RAW.SLES_C_converted AS PED2
		ON CS.BDVISIT = PED2.BDVISIT
LEFT JOIN RAW.SLES_P_converted AS PED3
		ON CS.BDVISIT = PED3.BDVISIT
LEFT JOIN RAW.TANR_F_converted AS PED4
		ON CS.BDVISIT = PED4.BDVISIT
LEFT JOIN RAW.TANR_M_converted AS PED5
		ON CS.BDVISIT = PED5.BDVISIT
/*		Pediatric CABSPD CESDPD SASPD	*/
/*LEFT JOIN WORK.CABSPD_CONVERTED AS PED6
		ON CS.BDVISIT = PED6.BDVISIT
LEFT JOIN WORK.CESDPD_CONVERTED AS PED7
		ON CS.BDVISIT = PED7.BDVISIT
LEFT JOIN WORK.SASPD_CONVERTED AS PED8
		ON CS.BDVISIT = PED8.BDVISIT*/
/*	Added on 7/11/16	new tables A-Z*/
LEFT JOIN RAW.PEDIATRIC_DATA_MERGED AS PED6
		ON CS.BDVISIT = PED6.BDVISIT
/*
LEFT JOIN RAW.LAB_with_crl AS CRL
		ON CS.BDVISIT = CRL.BDVISIT
*/
;
QUIT;
%MEND;

*=======================================================================================;
*=======================================================================================;
*								START OF PROGRAM										;
*=======================================================================================;
*=======================================================================================;
%CS_DATA;
%BRO_HAR_LAR;
%PROC_SQL(CS_SES1,CS,A,SES2,B,A.TRACT,B.TRACT,A.BLOCK,B.BLOCK,1);
%CS_SES2;
%PROC_SORT(HH.householdpage2,ENUMER,KEEP,KEY7,HHPAGE2_LINENUM,,,,,KEY7);
%PROC_SORT(HH.householdpage1,HHCS,KEEP,KEY7,,,,,,KEY7);
%CSLIM;
%ANTH;
%PROC_SQL(ANTH2,ANTH1,A,RAW.HEIGHTS,H,A.RRID,H.RRID,A.ANTH_EXAMDATE,H.ANTH_EXAMDATE,1);
%ANTH3;
%PROC_SORT(RAW.IDENTIFIERS,IDENTIFIERS,KEEP,RRID,DOB,,,,,RRID);
%PROC_SORT(RAW.IDD_converted,IDD1,DROP,DOB,,,,,,RRID);
%CONVERTED2;
%PROC_SORT(CSPT1,CSPT2,KEEP,RRID,PATIENT_TYPE,,,,NODUPKEY,RRID);
%RAW_IDD;
%PROC_SQL(CS8,CS_FULL1,CS,RAW.IDD_CONSTANTS,IDD,CS.RRID,IDD.RRID,,,0);
%DATA_FREQ_CS8_CONTACTSCH;
%MERGE_RAW_TABLES_SQL;
RUN;

proc sort data = merged1; by LABID; run;
proc sort data = labs_with_crl; by LABID; run;

data merged1_1;
set merged1;
if labid = "" then delete;
run;

proc sort data = merged1_1; by LABID; run;

data merged2;
merge merged1_1 (in = a) labs_with_crl (in = b);
by LABID;
if a;
proc sort; by labid;
run;

data drs_labs_with_crl;
set labs_with_crl;
where substr(labid,1,2) = "DR";
drop bioscann tribiosc fbg1 fbg2 glucosta h_date;
proc sort; by labid;
run;


/*

data mytest_data;
set drs_labs_with_crl;
keep rrid labid bioscann tribiosc fbg1 fbg2 glucosta fbg mfbg mmol_gluc ifg ada2010_cat ada2010_fbgcat ada2010_dm ada2010_undiag undiag h_date dm_nf;
run;

proc sort data = merged1; by labid; run;
proc sort data = RAW.LAB_converted; by labid; run;

data merged3;
merge merged2 (in = a) RAW.LAB_converted (in = b);
by labid;
if a;
proc sort; by labid;
run;
*/
*=======================================================================================;
*=======================================================================================;
*								END OF PROGRAM											;
*=======================================================================================;
*=======================================================================================;



/*
PROC SORT DATA=P5 OUT=U1 NODUPKEY; BY BDVISIT; RUN;

PROC SORT DATA=P5; BY BDVISIT; RUN;
DATA U2; SET P5; 
IF FIRST.BDVISIT THEN COUNT = 0; 
BY BDVISIT; 
COUNT +1;
RUN;
PROC PRINT; WHERE COUNT > 1; RUN;
PROC PRINT NOOBS; VAR RRID; WHERE COUNT > 1; RUN;

PROC PRINT DATA=P5; VAR RRID VISIT STUDY LABID; 
WHERE RRID IN ('BD0039' 'BD0072' 'BD0675' 'BD0711' 'BD0925' 'BD0926'); RUN;

NOTE: The data set RAW.MERGED1 has 7366 observations and 971 variables. 4.1.15;
NOTE: The data set RAW.MERGED1 has 7377 observations and 971 variables. 4.1.15;
NOTE: The data set RAW.MERGED1 has 7383 observations and 971 variables. 4.9.15;
NOTE: The data set RAW.MERGED1 has 7393 observations and 971 variables. 4.24.15
NOTE: The data set RAW.MERGED1 has 7395 observations and 975 variables. 4.28.15
NOTE: The data set RAW.MERGED1 has 7832 observations and 979 variables. 4.28.15
NOTE: The data set RAW.MERGED1 has 7832 observations and 980 variables. 4.29.15
NOTE: The data set RAW.MERGED1 has 7842 observations and 979 variables. 4.29.15
NOTE: The data set RAW.MERGED1 has 7849 observations and 979 variables. 5.7.15

NOTE: The data set RAW.MERGED1 has 8091 observations and 979 variables.	06.03.15




/*
DATA X; SET TEMP1; KEEP RRID LABID BP_examdate BPRANDOMZERO_FORM BPWELCH_FORM VISIT;RUN;
PROC SORT; BY LABID RRID; RUN;

/*
PROC FREQ;TABLE YROB ; RUN;
PROC FREQ;TABLE BORN_MX BORN_US CTRYBIR BOTH_BORN_MX BOTH_BORN_US BOTHMEXICO MAJSCHT;RUN;

/*
DATA IDD_M1;MERGE PERM1.IDD_CONVERTED PERM1.IDD_CONSTANTS; BY RRID; RUN;
DATA PERM1.IDD_deidentified; SET IDD_M1; 
DROP DOB FGIVEN FMATSUR FPATSUR FSPOUSUR MGIVEN MMATSUR MPATSUR MSPOUSUR SPGIVEN 
		SPMATSUR SPPATSUR;
RUN;

                                      Cumulative    Cumulative
BOTHMEXICO    Frequency     Percent     Frequency      Percent
ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
         1        3044       88.57          3044        88.57
         2         393       11.43          3437       100.00

Frequency Missing = 869

CHECKING SES>>>>>>>>>>>>>>>>>>>>>CLUSTER MISSI

DATA XIDD; SET RAW.IDD; WHERE RRID = 'HD0232'; RUN;
*/


/*
Adding RISK data   IZ   10/10/2017

data merged1_iz;
set MERGED1 my_risk_data;
proc sort; by rrid interview_date;
run;


proc contents data = merged2; run;

data lvidd_iz;
set labs_with_crl;
where lvidd ne .;
run;
