* Written by: 	P.Sanchez ;
* Date: 		2/4/2015 ;
* Description:  Creates new data set with new visits.
* Update(s):	
	05.7.15		PS	-Updating with new records.
	06.25.15	PS	-Using U not Y libnames.
	06.26.15	PS	-Removed hard coding. Not running 'proc freq' to speed up program.
	07.06.15	PS	-Testing. Using converted instead of converted2.
	07.29.15	PS	-Removed 'if visit = 1 then study = 'baseline'.
	07.11.16	ES	-Keep consistency over naming Folders in all programs.

;

OPTIONS NOFMTERR; 
OPTIONS NOCENTER; 

/*Removed because it overwrites it by next step ES 6/22/2017
DATA CCHC0; SET RAW.CONTACTSCHEDULINGPAT_converted;
*if RRID = 'BD2073';
RUN;*/

/**************************************************************** IZ 10102017
PROC SQL noprint; /*added noprint ES 6/22/2017
	CREATE TABLE CCHC0 AS
	SELECT * FROM RAW.CONTACTSCHEDULINGPAT_converted AS C
	LEFT JOIN RAW.IDD_CONVERTED AS I
		ON C.BDVISIT = I.BDVISIT AND C.STUDY = I.STUDY
	LEFT JOIN RAW.lab_with_crl AS L
		ON C.BDVISIT = L.BDVISIT AND C.STUDY = L.STUDY;;
QUIT;
************************************************************************/

/*
PROC SQL;
	CREATE TABLE CCHC0 AS
	SELECT * FROM RAW.CONTACTSCHEDULINGPAT_converted AS C
	LEFT JOIN RAW.IDD_CONVERTED AS I
		ON C.BDVISIT = I.BDVISIT AND C.STUDY = I.STUDY
	LEFT JOIN RAW.lab_with_crl AS L
		ON C.BDVISIT = L.BDVISIT AND C.STUDY = L.STUDY;
QUIT;
*/

/*
PROC FREQ; TABLE INTERVIEW_DATE; RUN;
*/

/* IH 10032017 *****************************************

DATA LAB1; SET RAW.lab_with_crl; RUN;

DATA IDD; SET RAW.IDD_CONVERTED; KEEP RRID VISIT STUDY INTERVIEW_DATE; RUN;


PROC SQL noprint; /*Added noprint ES 6/22/2017
	CREATE TABLE A1 AS
	SELECT * FROM CCHC0 AS C
	LEFT JOIN LAB1 AS L
		ON C.LABID = L.LABID
	LEFT JOIN IDD AS I
		ON C.RRID = I.RRID AND C.VISIT = I.VISIT AND C.STUDY = I.STUDY;
QUIT;
******************************************************** IH 10032017 */



/*
PROC SQL;
	CREATE TABLE A1 AS
	SELECT * FROM CCHC0 AS C
	LEFT JOIN LAB1 AS L
		ON C.LABID = L.LABID
	LEFT JOIN IDD AS I
		ON C.RRID = I.RRID AND C.VISIT = I.VISIT AND C.STUDY = I.STUDY
;
QUIT;
*/

/*
PROC FREQ DATA=A1; TABLE STUDY; RUN;
*/

/* IH 10032017
data risk_data;
LENGTH STUDY $30.;
set drs.patient_alldrs;
STUDY = "RISK";
run;

data NEW_A1;
set A1 risk_data;
run;
*/
/* IZ 10102017
DATA DATE1; SET CCHC0; IF SUBSTR(RRID,1,2)='RE' THEN DELETE; RUN;
*/

/*    Adding RISK data   IZ   10/10/2017    */

data my_risk_data;
LENGTH STUDY $30.;
set drs.patient_alldrs;
where interview_date NE .;
SUBDIV = '   ';
SECTION = ' ';
SUBDIV = SUBSTR(KEY7,13,3);
INOUT = SUBSTR(KEY7,16,1)*1;
SECTION = SUBSTR(KEY7,17,1);
DWELLING = SUBSTR(KEY7,18,3)*1;
STUDY = "RISK";
drop examdate_echo age_echo cardiac_history lv_size lvidd lvids lvh lv_mass lv_mass_index lv_ef rwt rv_fx tapse la_size la_dimension
la_volume ra_size ra_volume mac epi_fat mv_e mv_a e_a mv_decel_time ivrt ivct pv_s pv_d pv_a_vel pv_a_duration lateral_e lateral_a
lateral_s lateral_e_e medial_e medial_a medial_s medial_e_e diastolic_fx co ci rwma mr ai tr pasp rap as ms pericardial_effusion
echo_other_comments;
proc sort; by labid;
run;

/*
DATA my_risk_data1;
SET my_risk_data; 
DROP OCCUPATI_CH OCCMOSTL_CH SPOCCUPA_CH SPOCCMOS_CH;
FORMAT OCCUPATI OCCMOSTL SPOCCUPA SPOCCMOS Z3.;
OCCUPATI = OCCUPATI_CH;
OCCMOSTL = OCCMOSTL_CH;
SPOCCUPA = SPOCCUPA_CH;
SPOCCMOS = SPOCCMOS_CH;
proc sort; by labid;
RUN;
*/

data my_risk_data_all;
merge my_risk_data (in = a) drs_labs_with_crl (in = b);
by LABID;
if a;
run;

data MERGED3;
set MERGED2 my_risk_data_all;
if labid = "" then delete;
proc sort; by rrid interview_date;
run;

DATA DATE1; SET MERGED3; IF SUBSTR(RRID,1,2)='RE' THEN DELETE; RUN;

DATA DATE2; SET DATE1;
KEEP BDVISIT_OLD RRID VISIT STUDY INTERVIEW_DATE 
CTRES1-CTRES5 
DEAD EXAMDATE
CO1RES	CO2RES	CO3RES	CO4RES	CO5RES	CO6RES	CO7RES
CO8RES	CO9RES	CO10RES	CO11RES	CO12RES	CO13RES	CO14RES
CTDATE1-CTDATE5	
CO1DATE CO2DATE CO3DATE  CO4DATE CO5DATE CO6DATE CO7DATE
CO8DATE CO9DATE CO10DATE CO11DATE CO12DATE CO13DATE CO14DATE ;

BDVISIT_OLD = BDVISIT;
EXAMDATE=EXAMDATE/86400;
*;
/*
REMOVED BY PS-6.26.15
IF RRID = 'BD1267' AND VISIT = 4 THEN CO6DATE = '16AUG2010'D;
IF RRID = 'BD1752' AND VISIT = 4 THEN CO1DATE = '19DEC2009'D;
IF RRID = 'BD2067' AND VISIT = 7 THEN CO1DATE = '17SEP2011'D;
IF RRID = 'BD1748' AND VISIT = 8 THEN EXAMDATE = '04NOV2010'D; *WAS 2011;
*/
FORMAT CTDATE1-CTDATE5
CO1DATE CO2DATE CO3DATE CO4DATE CO5DATE CO6DATE CO7DATE CO8DATE CO9DATE CO10DATE
CO11DATE CO12DATE CO13DATE CO14DATE EXAMDATE DATE9.;
PROC SORT; BY RRID VISIT STUDY;
RUN;

%LET ORDER = INTERVIEW_DATE PATIENT_RESULT;
DATA A1; RETAIN &ORDER; SET MERGED3;
KEEP RRID VISIT STUDY INTERVIEW_DATE labid
LASTCONTACT /* FIX IN DRS */
LASTCONTACT1P /* FIX IN DRS */
LASTCONTACT2P /* FIX IN DRS */
LASTCONTACTPAGE LASTRESULT /* FIX IN DRS */
	DEAD 		PATIENT_RESULT;
*IF VISIT = 1 THEN STUDY = 'BASELINE         '; *REMOVED BY PS (7.29.15);
PROC SORT; BY RRID VISIT STUDY;
RUN;


DATA M1; MERGE A1 DATE2; BY RRID VISIT STUDY; RUN;
*+++++++++++++++++		proc freq disabled	ES 6/22/2017	+++++++++++++++++++++++++++;
PROC FREQ DATA=M1 noprint; TABLE RRID / OUT=RRIDCOUNT (DROP=PERCENT);PROC SORT; BY RRID; RUN;

DATA M2; MERGE M1 RRIDCOUNT; BY RRID; RUN;

DATA M3; SET M2;
MAXCTDATE =
	MAX(CTDATE1,CTDATE2,CTDATE3,CTDATE4,CTDATE5,
	CO1DATE,CO2DATE,CO3DATE,CO4DATE,CO5DATE,CO6DATE,CO7DATE,CO8DATE,
	CO9DATE,CO10DATE,CO11DATE,CO12DATE,CO13DATE,CO14DATE,EXAMDATE);
FORMAT MAXCTDATE DATE9.;
/*
REMOVED HARD CODE-PS 6.26.15
IF RRID = 'BD0649' AND STUDY = 'FIVE YEAR' THEN MAXCTDATE = '23APR2013'D; *NO DATA AT ALL;
*/
KEYDATE = INTERVIEW_DATE;
IF INTERVIEW_DATE = . THEN KEYDATE = MAXCTDATE;
RUN;

%LET ORDER = KEYDATE MAXCTDATE INTERVIEW_DATE LABID RRID VISIT STUDY COUNT  ;
DATA M4; RETAIN &ORDER; SET M3; 
*IF COUNT > 2; 
*IF VISIT = 1 THEN DELETE; 
FORMAT KEYDATE DATE9.;
RUN;

/*
PROC SORT DATA = M4 OUT = M4_1 NODUPKEY; BY BDVISIT_OLD LABID; RUN;

DATA M5; SET M4_1; *WHERE RRID IN ('BD1313' 'BD0649' 'BD0704');PROC SORT; BY RRID KEYDATE STUDY; RUN;
*/

DATA M5; SET M4; *WHERE RRID IN ('BD1313' 'BD0649' 'BD0704');PROC SORT; BY RRID KEYDATE STUDY; RUN;

DATA M6; SET M5; 
IF FIRST.RRID THEN NEWVISIT = 0;
BY RRID;
NEWVISIT + 1;
RUN;

/****************** IH 12112017 ************  
%LET ORDER=NEWVISIT;
DATA M7; RETAIN &ORDER; SET M6;
NEWVISIT = NEWVISIT + 1;
RUN;
************************************************************************* 12112017 */

%LET ORDER=VISIT_DIFF;
DATA M8; RETAIN &ORDER; SET M6;
VISIT_DIFF = VISIT-NEWVISIT;
DATEDIFF = (INTERVIEW_DATE-MAXCTDATE);
PROC SORT; BY RRID NEWVISIT;
RUN;

*+++++++++++++++++++++		proc freq disabled	ES 6/22/2017	+++++++++++++++++;
/*PROC FREQ noprint; TABLE VISIT_DIFF; RUN;*/

****************************************************************************************;
/****************** IH 12112017 ************  DATA TEST1; SET M7; IF KEYDATE = .; RUN;              */
****************************************************************************************;
/*
DATA TEST2; SET M7; IF PATIENT_RESULT IN (. 0); RUN;
*/
****************************************************************************************;
/*
RRID: BD0649
BASELINE INTERVIEW DATE: 3/17/2006
MAXCTDATE = MISSING
*/
****************************************************************************************;
/****************** IH 12112017 ************  
DATA TEST3; SET M8; IF STUDY IN ('FIVE YEAR' 'RISK'); 
KEY1 = CATS(RRID,INTERVIEW_DATE);
IF INTERVIEW_DATE = . THEN DELETE;
PROC SORT; BY RRID INTERVIEW_DATE;
RUN;
************************************************************************* 12112017 */

*+++++++++++++++++++++		proc freq disabled	ES 6/22/2017	+++++++++++++++++;
/****************** IH 12112017 ************  
PROC FREQ DATA=TEST3; TABLE KEY1 / OUT=X1; RUN;
************************************************************************* 12112017 */

/*PROC FREQ; TABLE COUNT; RUN; *124 SAME DAY 5YR-DRS VISITS;*/

/****************** IH 12112017 ************  
DATA X2; SET X1; DROP COUNT; 
RRID = SUBSTR(KEY1,1,6); 
INTERVIEW_DATE = SUBSTR(KEY1,7,20)*1; 
IF COUNT = 2; 
PROC SORT; BY RRID INTERVIEW_DATE;
RUN;

DATA TEST3A; MERGE TEST3 X2 (IN=A); BY RRID INTERVIEW_DATE; IF A; RUN;
************************************************************************* 12112017 */
****************************************************************************************;
/****************** IH 12112017 ************  
PROC SQL noprint;	/*Added noprint ES 6/22/2017
	CREATE TABLE TEST4 AS
		SELECT RRID FROM M8 
		WHERE VISIT_DIFF < -1 OR VISIT_DIFF > 1;
	CREATE TABLE TEST4A AS
		SELECT * FROM M8
		WHERE RRID IN (SELECT RRID FROM TEST4);
QUIT;
************************************************************************* 12112017 */
*+++++++++++++++++++++		proc freq disabled	ES 6/22/2017	+++++++++++++++++;
/*PROC FREQ DATA=M8 noprint;TABLE STUDY*VISIT_DIFF / LIST MISSING;RUN;*/

/****************** IH 12112017 ************  
DATA TEST4B; SET M8; IF (VISIT_DIFF < -2 OR VISIT_DIFF > 2) AND STUDY = 'FIVE YEAR';
RUN;

DATA TEST4C; SET TEST4B; IF INTERVIEW_DATE = .; RUN;
/*'BD0461'/*OK
PROC SQL noprint;	/*Added noprint ES 6/22/2017
	CREATE TABLE TEST5 AS
	SELECT * FROM M8
	WHERE RRID IN (SELECT RRID FROM TEST4C);
QUIT;
************************************************************************* 12112017 */
*ALL 16 RRIDS APPEAR OK;

/****************** IH 12112017 ************  
DATA TEST4D; SET TEST4B; IF INTERVIEW_DATE NE .; RUN;

DATA Y1; SET TEST4D;PROC SORT; BY VISIT_DIFF; RUN;

************************************************************************* 12112017 */
*+++++++++++++++++++++		proc freq disabled	ES 6/22/2017	+++++++++++++++++;
/*PROC FREQ noprint; TABLE DATEDIFF; RUN;*/

/****************** IH 12112017 ************  
DATA Y2; SET Y1; WHERE DATEDIFF > 0; PROC SORT; BY DESCENDING DATEDIFF; RUN;

DATA Y3; SET M8; WHERE RRID IN ('BD1773'); RUN;

PROC SQL noprint; /*Added noprint ES 6/22/2017
	CREATE TABLE TEST6 AS
	SELECT * FROM M8
	WHERE RRID IN (SELECT RRID FROM Y1);
QUIT;
************************************************************************* 12112017 */
*****************************************************************************;
DATA M9; SET M8;
BDVISIT = "        ";
IF NEWVISIT <= 9 		THEN BDVISIT = CATS(RRID,"0",NEWVISIT);
ELSE IF NEWVISIT >= 10	THEN BDVISIT = CATS(RRID,NEWVISIT);
PROC SORT OUT=M10 NODUPKEY; BY BDVISIT; 
PROC SORT DATA=M9; BY BDVISIT;
RUN;

DATA Z1; SET M9; 
IF FIRST.BDVISIT THEN COUNT = 0; 
BY BDVISIT; 
COUNT +1;
RUN;
/*PROC PRINT ; WHERE COUNT > 1; RUN;*/ /*ES 6/22/2017*/

DATA PACE.NEWVISITS; SET M9; RUN;
DATA PACE.NEWVISITSIH; SET M9;
KEEP STUDY RRID VISIT_NEW VISIT_OLD BDVISIT_NEW BDVISIT_OLD; 
VISIT_NEW = NEWVISIT;
VISIT_OLD = VISIT;
BDVISIT_NEW = BDVISIT;
RUN;


/*
PROC SORT DATA=PACE.NEWVISITSIH OUT=CHECK9 NODUPKEY; BY BDVISIT_OLD; RUN;

/*
RRID: BD0792; 5YR; INTERVIEW: 9/1/2010; BLOOD DRAW: 9/10/2010
RRID: BD1699; 5YR; INTERVIEW: 8/11/2011; SHOULD THIS BE 2014?

PROC FREQ DATA=M8; TABLE BDVISIT; RUN;

NOTE: The data set PACE.NEWVISITSIH has 4522 observations and 6 variables. 6.24.15

*/


%MACRO TITLE_FREQ_SORT_DATA;

/***************************************************** IH 12112017
PROC SORT DATA=MERGED3; BY RRID VISIT STUDY; RUN;
****************************************************** IH 12112017 ************/
PROC SORT DATA=MERGED3; BY RRID VISIT; RUN;

DATA ACCESS1;SET BRO1.newvisits; KEEP RRID VISIT NEWVISIT;*IF RRID = 'BD0002';
PROC SORT; BY RRID VISIT;
RUN;

DATA M1; MERGE MERGED3 (IN=A) ACCESS1; IF A; BY RRID VISIT; RUN;
/*
DATA M1; MERGE NEW_MERGED1 (IN=A) ACCESS1; IF A; BY RRID VISIT STUDY; RUN;
*/

DATA M2; SET M1; DROP NEWVISIT;
IF NEWVISIT NE . THEN VISIT = NEWVISIT; *IF RRID = 'BD0002';

BDVISIT = '        ';
IF VISIT <= 9 THEN BDVISIT = CATS(RRID,"0",VISIT);
ELSE IF VISIT >= 10 THEN BDVISIT = CATS(RRID,VISIT);

PROC SORT; BY RRID INTERVIEW_DATE;

RUN;
%MEND;

%MACRO TEMP2_SORT_PATIENT_MEDICATION;
data TEMP2; SET M2;

array medname (*) MED1-MED10 diabMed1-diabMed6 insul1 insul2;
*was: do i=1 to 18;
do i=1 to dim(medname);
	if medname(i) in (
	'-1                 '
	'9-DOES NOT REMEMBER' 
    'DOES NOT REMEMBER '           
	"DOESN'T REMEMBER"        
	"DOESN'T REMEMBER NAME"  
	'DOESNT REMEMBER' 
	'UNKNOWN'   
	'UNKNOWN ANTIBIOTIC' 
	'DOES NOT REMEMBER' 
	"DOESN'T REMEMBER" 
	"DOESN'T REMEMBER NAME"
	'UNKNOWN' 
	'UNKNOWN ANTIBIOTIC')
	then medname (i)= '                            ';
	medname(i) = UPCASE(medname(i));
end;

drop i;
PROC SORT; BY RRID;
run;


************************************************************************************
*MERGING OF RAW TABLES COMPLETE. IDENTIFING THE TYPE OF MEDICATIONS:
DIABETES MEDS
PARE_MED
ISULIN MEDS
CVD MEDS;
********************************************************************************************;

*Creating a file with participant and medication_name. 
The participant can appear several times,
it depends on the number of medication they are taking;

data patient_medication (keep=rrid visit LABID medication_name DBNAM LINENo STUDY BDVISIT town); 
	set TEMP2; *KEEPING BDVISIT - PS 7.10.15;
lineno=0;
countmed=0;
countora=0;
countins=0;
ok='NO';
array medic (*) med1-med10 diabmed1-diabmed6 insul1-insul2;
	*was: do i=1 to 18;
	do i=1 to dim(medic);
    	if medic(i) ne "  " then 
			do;
	   			OK='SI';
				IF i IN (1 2 3 4 5 6 7 8 9 10)		THEN DBNAM='MEDICAL';
				ELSE IF i IN (11 12 13 14 15 16)	THEN DBNAM='DIAORAL';
				ELSE IF i IN (17 18) 			 	THEN DBNAM='DIAINSU';
	   			medication_name=medic(i);
				if DBNAM='MEDICAL' then 
					do; countmed=countmed+1;
						lineno=countmed;
					end;
				if DBNAM='DIAORAL' then 
					do; 
						countORA=countORA+1;
						lineno=countORA;
					end;
				if DBNAM='DIAINSU' then 
					do; 
						countINS=countINS+1;
						lineno=countINS;
					end;

				output patient_medication;
			end;
	end;
*where substr(rrid,1,2)='BD'; *INCLUDING ALL SITES-PS 6.8.15;
proc sort; by medication_name;
run;

%MEND;

%MACRO FINAL_SORT_TRANSPOSE_TEMP3;


proc sort data=MEDICP.medicationlist OUT=PERM_MEDLIST; by medication_name; run;



PROC SQL noprint; /*Added noprint ES 6/22/2017*/
      CREATE TABLE FINAL.Participants_Medications AS
      SELECT * FROM patient_medication AS PT
      LEFT JOIN PERM_MEDLIST AS PL
      ON PT.medication_name = PL.medication_name;
QUIT;



********************************************************************************************;
DATA DIABMED; SET FINAL.Participants_Medications; IF CODE IN ('meddm1' 'meddm2') then output;
proc sort; by BDVISIT;
run;


*TRANSPOSING BY BDVISIT - PS 7.10.15; 
proc transpose data=diabmed out = tdiabmed ; var code; *by rrid; BY BDVISIT; run; 
*KEEPING BDVISIT - PS 7.10.15;
data pat_diabmed (keep= rrid oral_med pare_med ANY_DIAB_MED BDVISIT); set tdiabmed;
*array diab (*) col1-col8;
array diab (*) col: ; *UPDATED BY PS 7.10.15;
ANY_DIAB_MED=1;
*was: do i=1 to 8;
do i=1 to dim(diab);
  if diab (i) = 'meddm1' then oral_med=1;
  if diab (i) = 'meddm2' then pare_med=1;
end;
run;
proc sort data= TEMP2; *by rrid; by BDVISIT; run; *SORTING BY BDVISIT- PS 7.10.15;
 *merging BY BDVISIT- PS 7.10.15;
data TEMP3; merge TEMP2 (in=a) pat_diabmed (in=b); *by rrid; by BDVISIT; if a; run;

DATA cvdmed; SET FINAL.Participants_Medications; IF CODE IN ('medhp' 'medhd') then output;
proc sort; by BDVISIT; *SORTING BY BDVISIT- PS 7.10.15;
run;

*TRANSPOSING BY BDVISIT- PS 7.10.15; /*Added noprint ES 6/22/2017*/
proc transpose data=cvdmed out = tcvdmed; var code; *by rrid; by BDVISIT; run; 

data cvd_med (keep = rrid BDVISIT cvdmed); set tcvdmed; *KEEPING BDVISIT - PS 7.10.15;
cvdmed=0;
array medtys (*) col: ;
do i=1  to dim(medtys);
   if medtys(i) in ('medhd' 'medhp') then cvdmed=1;
end;
proc sort; by BDVISIT; *SORTING BY BDVISIT- PS 7.10.15;
run;

*merging BY BDVISIT- PS 7.10.15;
data P5; merge TEMP3 (in=a) cvd_med (in=b); *by rrid; by BDVISIT; if a; run; 
/*
PROC FREQ; TABLE RRID*LABID / LIST MISSING; RUN;

DATA RAW.MERGED1; SET P5; RUN;
*/

data mycovid;
set COV_LIB.covid;
run;

data mysyvcovid;
set SYV_LIB.syv_covid;
ANGINAGE_N = input(ANGINAGE, 8.);
STROKEAG_N = input(STROKEAG, 8.);
MHX_SOPTAGE_N = input(MHX_SOPTAGE, 8.);
LDLCALC_N = input(LDLCALC, 8.);
drop ANGINAGE STROKEAG MHX_SOPTAGE LDLCALC;
rename ANGINAGE_N=ANGINAGE;
rename STROKEAG_N=STROKEAG;
rename MHX_SOPTAGE_N=MHX_SOPTAGE;
rename LDLCALC_N=LDLCALC;
run;


DATA P6; 
SET P5 mycovid mysyvcovid;
IF FIRST.RRID THEN NEWVISIT = 0;
BY RRID;
NEWVISIT + 1; 
proc sort;by RRID visit; 
RUN;

DATA P7;drop newvisit; 
SET P6;
BDVISIT = "        ";
IF NEWVISIT <= 9 		THEN BDVISIT = CATS(RRID,"0",NEWVISIT);
ELSE IF NEWVISIT >= 10	THEN BDVISIT = CATS(RRID,NEWVISIT);
visit=NEWVISIT;
proc sort;by BDVISIT; 
RUN;

DATA RAW.MERGED1; SET P7; RUN;

%MEND;


%TITLE_FREQ_SORT_DATA;
%TEMP2_SORT_PATIENT_MEDICATION;
%FINAL_SORT_TRANSPOSE_TEMP3;
RUN;









/*

proc contents data = p5; run;

*/
