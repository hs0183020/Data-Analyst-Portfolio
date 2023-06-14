* CLEARING PROGRAM;
* Written by: G.Sanchez;
* Date:	05.24.12;
* Description: Creates a data set by merging all Access tables (except Addressbook
				, Phonebook). Merges CAB data set to CCHC data set.
* Generates: Recodes many variables from 999 to ., 99 to ., 9 to ., 0 to .;
* The final data set created is: FINAL.CCHC_BASELINE. 
* NOTES: Merge deceased variable when new data is available;
* Updates:
	08.28.13	PS	Removed variable 'weightkv' as per KV.
	09.04.13	PS	Added new code that changes HEMA values from 0 to missing if 
					all HEMA variables are 0.
	10.24.13	PS	Added new code that changes BIO3A, BIO3B, BIO3C, BIO3D from 0
					to missing if all 4 variables are 0.
	11.12.13	PS	Hiding AGE_TODAY.
	12.05.13	PS	C_HHINCYR should be missing or >= 0.
	03.06.14	PS	Using new 'HH' folder.
					Keeping KEY8 instead of dropping it.
	03.14.14	PS	Added 'NE .' code to variables ADA2010_DM as per KV.
					Calculate AGE_TODAY.
	03.17.14	PS	Added 'INSTYPE' in array that converts 99 to .
	03.18.14	PS	Changed 'length_diabetic' variable.
	03.20.14	PS/IH	Program was not updating 'KEY8', 'LINENUM', 'DWELLING'
						variables. 
					Added code to update these variables. As of 3/20/14, all 800
					records have these values.
	03.26.14	PS	Hiding libraries 'BASEMEDp', 'BASEMED2' as they are not used
					any more.
	03.28.14	PS	Updating code for: ADA_DM, ADA2010_diabA1c, ADA2010_UNDIAG.
					In 'data impact1, set IMPACTp.PATIENT_IMPAT', ADA_DM variable
					is added. The code for the other 5
					variables is updated.
					Updating code for: CTRYBIR, BORN_MX, BORN_US, BIRTCTRY.
	04.11.14	PS	Updating PROC FREQ.
	04.16.14	PS	Added 'LABID' to data set 'Participants_medications'.
	04.17.14	PS	Moved 'STUDY' variable to higher position.
	04.30.14	PS	Updated MFBG (adding CRL). 
	05.01.14	PS	Saved 5.1.14, no changes.
	05.05.14	PS	Hiding KEY8 from 'HH.enumeration'. All KEY8s are unique now.
	05.06.14	PS	Updating code for UNDIAG.
	05.20.14	PS	Fixing 2 vars
	05.21.14	PS	Fixing AGEDIAB, Length_diabetic, AGE_TODAY
	06.27.14	PS	Adding date formats
	08.01.14	PS	Changed location of 'patient_impat', renamed to 'patient_impact'.
					Removing 3 variables.
	08.05.14	PS	Adding new variables/labels that correspond to the 5YR/DRS datasets.
	08.12.14	PS	1) Updating "\\Rahc-s8\public\" to "Y:\BrownsvilleSD\public"
					2) Fixing EDUCLVL variable.
	08.27.14	PS	Updating paths only.
	09.03.14	PS	Updating paths. Updated this library: KVWGTp. Rearranged several
					variables.
	10.23.14	PS	Adding 2 new vars (ADIPONECTIN_mcg, RESISTIN_ng).
	10.24.14	PS	Replacing 'patient_impat' with 'impact_baseline'. Adding new labels.
	10.27.14	PS	*ADDING SAMPLING WEIGHTS, HCV, HBV, INVENTORY DATA
	11.17.14	PS	Changing temp names only.
	11.24.14	PS	Using LAB_WITH_CRL.
	11.26.14	PS	Merging 'hbvs_antigen_all'. Test if ok.
	12.01.14	PS	Adding vars to arrays 'var9_0' and 'numvar999'. Adding vars to 
					format 'YES_NOX'.
	12.02.14	PS	Added one var to array 'var9_0'.
	12.10.14	PS	Renaming array 'var9_0' to 'num9'. Adding more vars to this array.
	02.02.15	PS	Updated 2 paths. Many changes.
	03.06.15	PS	Merging Liver results and Liver scores. Study = Baseline for visit 1.
	03.13.15	PS	UPDATED CODE FOR: OTHENUM CVDMED ORAL_MED PARE_MED.
					TESTING 3 PROGRAMS THAT WILL CONVERT ALL VISITS AND ALL LOCATIONS.
	03.17.15	PS	TESTING.
	03.25.15	PS	TESTING.
	03.26.15	PS	Updating STUDY variable.
	03.27.15	PS	Updating order of data sets when creating TEMP1. Not updating 'w'
					variable. 
					Updating pace and drs variables.
	03.31.15	PS	Moved merging all 13 tables to 2nd program. Updating CAB_SURVEY
					variable.
	04.08.15	PS	Adding 'Other Studies' data set.
	04.28.15	PS	Moved 'medications' code to program 02A2.
	05.05.15	PS	Laredo is not collecting MPV.
	05.06.15	PS	Updated path for weights 'w'.
	05.07.15	PS	Removed 4 CAB surveys. Updating 'other survey' vars.
	05.08.15	PS	Renaming temp data sets.
	05.11.15	PS	Updating country/state vars.
	05.20.15	PS	Updating paths only.
	06.03.15	PS	Fixing income vars. Adding vars to arrays. CRP and other variables
					are appearing as zero. Added code to fix.
	06.05.15	PS	Hiding 2 arrays (-1, upcase). URINDAT format/value is causing errors.
	06.08.15	PS	Removing records with missing LABIDs for Harlingen & Laredo only.
					Renamed array 'num9' to 'numvar9'.
	06.09.15	PS	Added 3 labels.
	06.24.15	PS	Removing hard code.
	06.25.15	PS	Using libnames U instead of Y. Removing 'proc freqs' to speedup program.
	07.07.15	PS	Dropping 2 empty vars (FBG3, DATE_FBG3).
	07.09.15	PS	Testing c_hhincmth, c_hhincyr for drs visits.
	07.21.15	PS	Adding labels and formats. Fixing URINDAT. Should this variable be 
					deleted: STARTAGE_CIGS?
	07.29.15	PS	Pediatric data is now in cchc_baseline.
	07.31.15	PS	Final data set 'cchc_baseline' is now 'cchc'.
	07/11/16	ES	Keep consistency over naming Folders in all programs.

;
********************************************************************************************;
OPTIONS NOCENTER;
OPTIONS NOFMTERR;
/*
%LET PATH = \\uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core;

LIBNAME LIBRARY '\\uthouston.edu\uthsc\sph\Research\Studies Data\SAS Formats';
libname IMPACTp "&PATH\BD Study\Data\SAS_Data\Clean"; *PARTICIPANTS_MEDICATIONS;
LIBNAME HH "&PATH\MSAccess\FINAL DATA\HH";
libname RAW "&PATH\MSAccess\FINAL DATA\BASELINE\DATABASE TABLES SAS\PACKETS";
libname FINAL "&PATH\MSAccess\FINAL DATA\BASELINE";
LIBNAME SW "&PATH\Sampling_Weights\w";
LIBNAME HCVL "&PATH\LABORATORY_RAHC\HCV\FINAL DATA";
LIBNAME INV "&PATH\Lab_Inventory\COHORT_BROWNSVILLE\FINAL_DATA";
LIBNAME ECGXI "&PATH\Laboratory_specimens_sent_to otherLab\ECG\FINAL DATA";
LIBNAME HBV "&PATH\LABORATORY_RAHC\HBV\FINAL DATA";
LIBNAME LIVER "&PATH\ULTRASOUND DATA\LIVER\FINAL DATA";
LIBNAME HEV "&PATH\LABORATORY_RAHC\HEV\FINAL DATA";
LIBNAME LD "&PATH\DECEASED\COMBINED"; *DECEASED EXISTS IN PREVIOUS PROGRAM;
LIBNAME LUMIN2 "&PATH\Laboratory_specimens_sent_to otherLab\UTHSCSA\LUMINEX\FINAL DATA";
LIBNAME LCAB2 "&PATH\MSAccess\FINAL DATA\CAB";
*/

/*
*MOVE TO PROGRAM2 ASAP;
DATA FAMILYHX; 
BPRANDOMZERO_FORM = 1; 
DATA LAB; SET ; DROP KEY7 KEY8; 
DATA WELCH; SET ; BPWELCH_FORM = 1;
DATA mhxmed; SET ; 

PROC FREQ DATA=RAW.MERGED1; TABLE FBG3 FBG_CRL; WHERE LABID = '10Y0077'; RUN;

PROC FREQ DATA=RAW.MERGED1; TABLE FBG3*FBG_CRL*LABID / LIST MISSING; RUN;

*/

*******************************************************************************;
*CLEARING VALUES CONTINUES;
*******************************************************************************;
*PS-10.24.2013-NEW CODE CHANGES 0 TO MISSING IF ALL 4 VARIABLES ARE 0.;

/*
DATA TEST1; SET C1; WHERE BDVISIT='BD011215'; RUN;
*/

data RawRRIDLABID;
set RAW.MERGED1;
keep rrid labid;
proc sort nodupkey; by labid;
run;

%macro CheckDataset(Dataset,RawDataset);
%local count;

data A;
set &Dataset;
keep rrid labid;
proc sort; by labid;
run;

PROC COMPARE
BASE= RawRRIDLABID
COMPARE= A
out=error outnoequal noprint;
ID LABID; RUN;

proc sql noprint;
select count(*) into :count
from error;
quit;

%if (&count > 0) %then %do;
STOP;
%put Error on table &Dataset;
%end;

%mend CheckDataset;



data C1; LENGTH STUDY $30.; set RAW.MERGED1;

*NEXT 4 LINES, TEMP CODE (PS 6.5.15);
IF CRP = 0 THEN CRP = .;

/*
REMOVED AS OF 6.24.15-PS
IF BDVISIT='HD025101' THEN LABID ='HA0002'; 
IF BDVISIT='HD025201' THEN LABID ='HA0003';
IF BDVISIT='HD025301' THEN LABID ='HA0004';
IF BDVISIT='HD026801' THEN LABID ='HA0012';

SES=QINC;
*/

**************************************************************************************;
IF FBG1 IN (0 1 9) THEN FBG1 =.; *5.13.15 TEMP CODE/SEND TO CRU ASAP;
IF FBG2 IN (0 1 9) THEN FBG2 =.; *5.13.15 TEMP CODE/SEND TO CRU ASAP;
**************************************************************************************;

IF ARM = . THEN BPRANDOMZERO_FORM = 0; *TESTING PS 3.27.15;
**************************************************************************************;
*ADDING CODE 5.13.15;
IF SPSCHPRE IN (/*0*/ 99) AND SPSCHTEC IN (0) AND SPSCHCOL IN (0) AND SPSCHGRA IN (0) THEN
	DO;
		SPSCHPRE = .;
    	SPSCHTEC = .;
    	SPSCHCOL = .;
    	SPSCHGRA = .;
	END;
*ADDING CODE 6.3.15;
IF SPSCHPRE = 0 AND SPSCHTEC NE . AND SPSCHCOL NE . AND SPSCHGRA NE . THEN
	DO;
		SPSCHPRE = .;
	END;
**************************************************************************************;
*TEMPORARY HARD CODE (PS-8.1.14)/REMOVE AS SOON AS CRU FIXES VALUE;
/*
REMOVED AS OF 6.24.15-PS
IF LABID = 'BD6831' AND FBG_CRL = 3110 THEN FBG_CRL = 110;
IF LABID = 'BD6866' AND CHOL1 = 43 THEN CHOL1 = 182;
*/
**************************************************************************************;
IF BIO3A = 0 AND BIO3B = 0 AND BIO3C = 0 AND BIO3D = 0 THEN
	DO;
		BIO3A = .;
		BIO3B = .;
		BIO3C = .;
		BIO3D = .;
	END;
***************************************************************************************;
*NEW CODE (FOUND BY OUTLIERS PROGRAM/PS 09.04.13);
IF WBC=0 AND LY_NO=0 AND MO_NO=0 AND GR_NO=0 AND HCT=0 AND HGB=0 AND MCH=0 AND MCHC=0 AND 
	MCV=0 AND RBC=0 AND RDW=0 AND PLT=0 AND MPV=0 THEN 
		DO;
			WBC=.; 
			LY_NO=.; 
			MO_NO=.; 
			GR_NO=.; 
			HCT=.; 
			HGB=.; 
			MCH=.; 
			MCHC=.; 
			MCV=.; 
			RBC=.; 
			RDW=.; 
			PLT=.; 
			MPV=.;
		END;

if PIP = 1049 then PIP = .;
if PULSE2 = 1998 then PULSE2 = .;

IF POP = 0 AND PIP = 50 THEN
	DO;
		POP = .;
		PIP = .;
	END;

IF UNCSYS1 = 999 AND ZEROSYS1 = 99 THEN ZEROSYS1 = .;
IF UNCSYS2 = 999 AND ZEROSYS2 = 99 THEN ZEROSYS2 = .;
IF UNCSYS3 = 999 AND ZEROSYS3 = 99 THEN ZEROSYS3 = .;

IF UNCSYS3 = 0 AND CORRSYS3 = 0 AND ZEROSYS3 = 0 THEN
	DO;
		UNCSYS3 = .;
		CORRSYS3 = .;
		ZEROSYS3 = .;
	END;

IF ZERODIA3 = 0 AND CORRDIA3 = 0 AND UNCDIA3 = 0 THEN
	DO;
		ZERODIA3 = .;
		CORRDIA3 = .;
		UNCDIA3 = .;
	END;

array arr0 (*) 
	CORRDIA1-CORRDIA3 CORRSYS1-CORRSYS3 
	RETAGE
	ARM_CIRC 
/*************************** Change Pulse3 to Pulse2 ** IZ ** 10.10.2016 ****************/
	PULSE1W PULSE2W PULSE3W PULSE1-PULSE2 
	DIA1W DIA2W DIA3W 
	SYS1W SYS2W SYS3W
	;
	do i=1 to dim(arr0);
		if arr0(i) IN (0) then arr0(i) = .;
	end;

*IF RRID = 'BD1999' THEN GR_NO = 3.88; * instead of 3.94.-Dianey 9.7.2012;

*NEW CODE BY PABLO (9.7.2012). UPDATED BY PS (3.19.15);
array NUM999_9(*)
AGE_ER		ARM_CIRC	CORRDIA1	CORRDIA2	CORRDIA3	CORRSYS1
CORRSYS2	CORRSYS3	DIA1W		DIA2W		DIA3W		height
HIP			HIP2		MCORRDIA	MCORRSYS	PULSE1		PULSE1W
PULSE2		PULSE2W		PULSE3		PULSE3W		SYS1W		SYS2W
SYS3W		WAIST		WAISTB		weight1		RMD3MONS/*ADDED BY PS 3.20.15*/;
	do i=1 to dim(num999_9); 
		if NUM999_9(i) IN (999 999.9) then NUM999_9(i)=.; 
	end;
******************************************************;

CBC_DIFF=UPCASE(CBC_DIFF); *WHAT IS THIS VARIABLE? PS 3.19.15;

RUN;

DATA C2; SET C1;
*MISSING 3 VARIABLES (MO_NO, LY_NO, RBC), 13 TOTAL;
if year(EXAMDATE) = 9999 then EXAMDATE=.;
* 10/02/2007 GS include hhattage=99 should be missing; 
if htattyr=0 then htattyr=.;
*if htattage in (99) then htattage=.;
if anginayr IN (0 9999) then anginayr=.;
*if anginage IN (99) then anginage=.;
if strokeyr IN (0 9999) then strokeyr=.;

*NOVEMBER 06 2007 GS ADD THIS LOGIC;
IF LEGCRAMP=2 AND CRAMPSTP = 2 and CRAMPFT = 2 and CRAMPNIT = 2 THEN 
	DO;
		CRAMPSTP=0;
		CRAMPFT=0;
		CRAMPNIT=0;
	END;

* 10/02/2007 GS-include strokeag=99 should be missing; 
if hepatiti=7 then hepatiti=1;
ARRAY num0(*)
ASPIRIN		BRINGMED	CATARACT	CRAMPFT		CRAMPNIT	CRAMPSTP	ENDARAGE
ENDARYR		EXAMINER	GBSURAGE	GBSURYR		GLAUCOMA	HEP_AGE		HEP_YR
hepatiti	HI_CHAGE	HI_CHOLYR	HIGHBPAG	HIGHBPYR	LEGCRAMP	PRESCRIP
STROKEAG 	TRANSAGE	TRANSYR		;
do i=1 to dim(num0); if num0(i) = 0 then num0(i)=.; end;
RUN;

************************************************************************************;
DATA C3; SET C2;
array numvar0_999 (*)
BIO3A 		BIO3B		BIO3C 		BIO3D 		BIOID		BIOSTAT		CANCAGE	
CANCER 		ECG			ECGMACH		EXAMINER	FAAGE		FAALIVE		FABP
FABPMED		FACANCER	FACANDI		FACANTYP	FACARAGE	FACARAT		FACORAGE
FACORO 		FADEAAGE	FADEATYR	FAHRTAGE	FAHRTATK	FAMCOMM		FAMED
FASTRAGE	FAYRBIRT	FECANAGE	/*FECANCER*/FECANTYP	MACANAGE	/*MACANCER*/
MACANTYP	MOAGE		MOALIVE		MOBIRTYR	MOBPMED		MOCANCER	MOCANDIA
MOCANTYP	MOCARAGE	MOCARAT		MOCORAGE	MOCORO		MODEAAGE	MODEATYR
MOHRTAGE	MOHRTATK	MOSTRAGE	MOSTROKE	UNCSYS1		UNCDIA1		UNCSYS2
UNCDIA2		UNCSYS3		UNCDIA3		
/*ADDED 5.13.15*/
SPSCHTEC	SPSCHCOL	SPSCHGRA	;
do i=1 to dim(numvar0_999); if numvar0_999(i)IN (0 999) then numvar0_999(i)=.; end;

RUN;
************************************************************************************;
*Updated by PS (3.23.15);
DATA C4; SET C3;
ARRAY charv999(*)
SPOCCUPA
SPOCCMOS
/*ADDED BY PS 5.13.15*/
OCCUPATI
OCCMOSTL
;
*was: do i=1 to 2; 
do i=1 to dim(charv999); 
if charv999(i) IN ('-01' '999') then charv999(i)='.'; end;
*if STRIP(charv999(i)) IN ('.' '999') then charv999(i)=''; 
************************************************************************************;
array charv9e5(*) 
ECG1	ECG2	ECG3	ECG4	ECG5	ECG6	;
do i=1 to dim(charv9e5);
if STRIP(charv9e5(i)) IN ('-0001' '-00001' '-1' '-10000' '99999' '999999' '') 
then charv9e5(i)= '.'; end; *UPDATED BY PS 6.3.15;
************************************************************************************;

array numvar99(*)
MOBPMED		MOMED		MOHRTAGE		MOCORAGE		MOSTRAGE		MOCARAGE
FACANTYP	FACANDI		MOCANTYP		MOCANDIA		MACANTYP		MACANAGE
FECANTYP	FECANAGE	CANCAGE			BIOLESS6		BIOHRT			BIOHRTLE
BIOCORO		BIOCORLE	BIOSTRK			BIOSTRLE		BIOSCAR			BIOSCALE
BIOSDIAB	BIOCANCER	BIOCAN6			FACORAGE		FASTRAGE		FACARAGE
RETMTH		SPRETMTH	SPWKLSYR		FABPMED			FAMED			FAHRTAGE
SPSCHPRE	SPEMPYRS	SPINSTYP		WKLASTYR		BIOBP			MACANCER
BIOSIBS		SMOKEHR		SMOKEMIN		FECANCER		MDDIABSA		BIOSIBS 
BIOBP		BIOCAN6		BIOCANCER		BIOCORLE		BIOCORO			BIOHRT
BIOHRTLE	BIOLESS6	BIOSCALE		BIOSCAR			BIOSDIAB		BIOSTRK/*CHK99?*/
BIOSTRLE/*CHECK 99?*/	WINECWK      	WINECMO			WINEWK			WINEMO
BEERWK		BEERMO		LIQUORWK		LIQUORMO		INSTYPE/*ADDED BY PS*/
/*ADDED BY PS 12.10.14*/
BIOSCANN	EMPYEARS	TRIBIOSC
/*NEXT VARS ADDED BY PS 3.18.15*/
FADIAB		MODIAB		ZERODIA1		ZERODIA2		ZERODIA3		MACHINE
MACHINEW	ECGMACH		CANCER/*ADDED BY PS 5.11.15*/	GLUCOSTA /*ADDED BY PS 5.13.15*/
MOBP 		MOMED 		FASTROKE /*ADDED BY PS 7.21.15*/;
do i=1 to dim(numvar99); if numvar99(i) = 99 then numvar99(i)=.; end;
****************************************************************************************;
array numvar99_9(*)
 GR_NO	HCT		HGB		MCH		MCHC	RDW		WBC		MPV		HRSWK	 HRSWKEND	;
do i=1 to dim(numvar99_9); if numvar99_9(i) = 99.9 then numvar99_9(i) = .; end;
**************************************************************************;
array numvar999(*)
DIAB_AGE	RMDTIME		RETAGE		SPRETAGE	HTATTAGE	ANGINAGE	STROKEAG
HIGHBPAG	ENDARAGE	HI_CHAGE	HEP_AGE		GBSURAGE	TRANSAGE	QUITAGE
SPHRSAWK	STARTAGE	CIG_DAY		HRSAWK		RMD1MONS	TRIGS		HRSWEEK
/*SMOKE/DRINK FORM*/	MCV			PLT			HRSWK/*FIX THIS IN NEW CCHC ANTHRO*/
YRSINBRO	CHOL 		CORRDIA1-CORRDIA3 		CORRSYS1-CORRSYS3 	PULSE1-PULSE3
TRIGS		MCORRDIA	MCORRSYS
/*ADDED THESE VARS 12.1.14*/
CIG_SDAY	CIG_EDAY	STARTAGE_ECIG	STARTAGE_CIGR	STARTAGE_SNUS	STARTAGE_SNUF
AGE_HOS		POP			PIP			;
do i=1 to dim(numvar999); if numvar999(i)=999 then numvar999(i)=.; end;
***********************************************************************************;
array numvar9999(*)
MODEATYR	MOBIRTYR	DIAB_YR		RETYEAR		SPRETYR		HTATTYR		ANGINAYR
STROKEYR	HIGHBPYR	ENDARYR		HI_CHOLYR	HEP_YR		GBSURYR		TRANSYR
STARTYR		QUITYR		FADEATYR	FAYRBIRT	;
do i=1 to dim(numvar9999); if numvar9999(i)=9999 then numvar9999(i)=.; end;
**********************************************************************************;
*UPDATED 6.3.15, WAS numvar8;
array income1(*)
INCMONTH	INCYEAR		SPINCMTH 	SPINCYR		;
do i=1 to dim(income1); 
	if income1(i) IN (
	    . /*5YR & DRS RECORDS DO NOT HAVE SPOUSE INCOME- PS 7.9.15*/
       -1
        9 
	 9999
    99999 
   999999 
  9999999 
 96999999
 99999998
 99999999 
119999988
) then income1(i)=0; *UPDATED 6.3.15 PS;
end;
*******************************************************************************;
*ADDED 6.3.15. UPDATED 6.8.15;
IF HHINCMTH IN (. 999 9999 99999 999999 9999999 99999999) AND 
				(HHINCYR = HHINCMTH*12 OR HHINCYR = HHINCMTH) THEN
	DO;
		HHINCMTH =0;
		HHINCYR =0;
	END;

IF HHINCYR IN (. 999999 9999999 99999999) AND HHINCYR = FLOOR(HHINCMTH/12) THEN
	DO;
		HHINCMTH =0;
		HHINCYR =0;
	END;

array income2(*)
HHINCMTH 
HHINCYR
;
do i=1 to dim(income2); 
	if income2(i) IN (
	     . /*DRS RECORDS DO NOT HAVE HOUSEHOLD INCOME- PS 7.9.15*/
        -1
	 99999
	119988
	999999
   9999999 
  96999999
  99999998
  99999999 
 119999988
1163999988
)
then income2(i)=0; 
end;

**********************************************************************************
*The Income variables that are with value=9s first will be 0 then they will be "." (missing);
*Here the program calculates family income: monthly and annually;
C_hhincmth=.;
C_hhincyr=.;

*Here the program calculates month and year values and the new value will modify 
the income variables that have 0 (FROM BD0001-BD2000 PGM)-ADDED PS 6.3.15;
if incyear=0 	then incyear=round(incmonth*12);
if incmonth=0 	then incmonth=round(incyear/12);
if spincyr=0 	then spincyr=spincmth*12;
if spincmth=0 	then spincmth=round(spincyr/12);
if hhincyr=0 	then hhincyr=hhincmth*12;
if hhincmth=0 	then hhincmth=round(hhincyr/12);

C_hhincmth = INCMONTH + SPINCMTH;
C_hhincyr  = INCYEAR + SPINCYR;

*HERE THE PROGRAM CHANGES SOME 0s into ".";
IF INCMONTH=0 THEN INCMONTH=.;
IF INCYEAR=0 THEN INCYEAR=.;
if spincmth=0 then spincmth=.;
if spincyr=0 then spincyr=.;
if C_hhincmth=0 then C_hhincmth=.;
if C_hhincyr=0 then C_hhincyr=.;
IF HHINCMTH=0 THEN HHINCMTH=.; *ADDED BY PS 6.3.15;
IF HHINCYR=0 THEN HHINCYR=.; *ADDED BY PS 6.3.15;

*********************************************************************************;
CATJOB=.;

OCCUPATI_NUM = OCCUPATI*1;

IF OCCUPATI_NUM =001 THEN CATJOB=1;
IF OCCUPATI_NUM IN (002 108) THEN CATJOB=2;
IF OCCUPATI_NUM IN (104 049 028 106 069 072 076 083 005 006) THEN CATJOB=3;
IF OCCUPATI_NUM IN (003 004 010 095 096 019) THEN CATJOB=4;
IF OCCUPATI_NUM IN (119 103)THEN CATJOB=5;
IF OCCUPATI_NUM IN (113 ) THEN CATJOB=6;
IF OCCUPATI_NUM IN (014 015 022 052 027 037 074 075 009 016
017 024 117 055 007 011 018 082 078 087 116 025 048 053
059 066 043 029 033 118 041 070 073 086 105 089 110 013 081
) THEN CATJOB=7;
IF OCCUPATI_NUM IN (020 021 107 023 098 047 050 109 051 054 058 099 
011 034 008 063 065 032 035 036 115 100 039 051 068 071 097
077 079 084 091 092 088 111) THEN CATJOB=8;
IF OCCUPATI_NUM IN (055 056 060 061 062 031 064 030 042 043 031) THEN CATJOB=9;
IF OCCUPATI_NUM IN (026 038 040 085 086 090 011 012 067 090) THEN CATJOB=10;
IF OCCUPATI_NUM IN (057 102) THEN CATJOB=11;

RUN;
 /* Removed ES 6/22/2017 Activate if needed
TITLE 'CHECKING INCOME';
PROC PRINT DATA=C4; 
VAR RRID LABID TOWN INCMONTH INCYEAR HHINCMTH HHINCYR SPINCMTH SPINCYR C_hhincmth C_hhincyr;
WHERE 
INCMONTH >=999999 OR
INCYEAR  >= 999999 OR
HHINCMTH >= 999999 OR
HHINCYR >= 999999 OR
SPINCMTH >= 999999 OR
SPINCYR >= 999999 OR
C_hhincmth >= 999999 OR
C_hhincyr >= 999999
;
RUN;

PROC MEANS DATA=C4; 
	VAR TOWN INCMONTH INCYEAR HHINCMTH HHINCYR SPINCMTH SPINCYR C_hhincmth C_hhincyr; RUN;

PROC PRINT DATA=C4;
VAR RRID LABID TOWN INCMONTH INCYEAR HHINCMTH HHINCYR SPINCMTH SPINCYR C_hhincmth C_hhincyr;
WHERE SPINCYR IN (     999.0000000       430000.00); RUN;
*/

DATA C5; SET C4;
* June 20 2011 I asked Krisitna Vatcheva how is the best way to report 9s 0s or . ;
*10/11/13 - PS added (-1);
array num (*) _numeric_ ;
do i=1 to dim(num);
   if num(i) in (/*-1*/ 999.9 9999 9999.9 999.99 99999 99999999)  then 
   num(i) = .;
end;

/*
HIDING BY PS (6.5.15)

*10/11/13 - PS added this array (-1);
array ch(*) $ _character_ ;
do i = 1 to dim(ch);
	if ch(i) = ("-1") then ch(i) = "";
	ch(i) = UPCASE(ch(i)); *ADDED 5.13.15;
end;
*/
*5.13.15 new array;
array num_neg (*) RETAGE;
do i=1 to dim(num);
   if num(i) in (-9 -1) then num(i) = .;
end;


RUN;

DATA C6; SET C5;
*GS JULY/12/2011-Variables with 9.0 should not be = missing because there are two that 9.0 is
a valid answer. So after checkin mannualy the variables I got eh following variables that
should be converted into missing values;
array numvar9 (*) 
AGREECONT	AMPU		BRINGMED	CATARACT	CRAMPFT		CRAMPNIT	CRAMPSTP
DIABRETA	DIABRETI    GLAUCOMA	GOVHELP		HI_CHOL		HTATTACK    ILLDIS
INSPYMED    INSPYSME    INSULNOW    INSULTAK    KETOACID    KETOHORM    KIDNDIAL
KIDNDIS     /*LY_NO (RANGE=24.0*/ /*MO_NO (RANGE=?*//*RBC (MEAN = 4.0 */
NEUR 		OTHECOMP    PILLNOW     PILLTAKE    REMEDIES    RETI        RMDFREQ             
SORENOHE    SOREWHER    SPEMPST     SPGOVHEL    SPILLDIS    SPINSMED    SPINSSME            
SPRET       SPSTUD      TREATCHN    LANGUA 		MORETEST 	BORDPLX 	ULTRASTY
Q4BRSBP 	HOSPITAL	PACESTDY 	HISPANIC	/*PREG12M   /*<-pace only*/
/*EMERGEN-DRS ONLY*/	AGREECONT/*pace and what studies?*/	RACE/*NEW* PS 8/29/14*/
ARM    		GBSTONES    TRANS       HEPATITI    Q4BRSBP   	STUDENT HISPANIC/*NEW*PS9.3.14*/
ARMW	
/* ADDED THESE VARS ON 12.1.14 */
TRIEDCIGS	/*TRIEDECIG*/TRIEDCIGAR	TRIEDSNUS	TRIEDSNUFF	BRACHSONO1
BRACHSONO2	CCASONO1	CCASONO2	ECHOSONO1	ECHOSONO2	LIVERSONO1
LIVERSONO2	DEXASONO1	DEXASONO2	BLDSTOR     AGREE_URINE AGREE_STOOL    
AGRFUTRTEST WTSCIGS		WTSECIG		WTSCIGR		WTSSNUS		WTSSNUF
BFFCIGS		BFFECIG		BFFCIGR		BFFSNUS		BFFSNUF		WBSCIGS
WBSECIG		WBSCIGR		WBSSNUS		WBSSNUF		SMOKENOW
/*added by ps 12.10.14*/
ANGINA		DIABETIC	EMPSTAT		HIGHBP		Q1REV		Q2WPART
Q3GTOPAR	SPINSTYP	SPWKLSYR	STROKE		TRIEDECIGS	XTRIEDCIGAR/*NEED FORMAT*/
XTRIEDECIGS/*NEED NEW FORMAT*/		XTRIEDSNUFF/*NEED NEW FORMAT*/
XTRIEDSNUS/*NEED NEW FORMAT*/		HTATTACK	STROKE		HIGHBP
ENDAR		HI_CHOL		HEPATITI	GBSTONES	TRANS		HOSPITAL
LEGSNUMB	HANDNUMB	LEGCRAMP	CRAMPSTP	CRAMPFT		CRAMPNIT
CATARACT	GLAUCOMA	ASPIRIN		PRESCRIP	BRINGMED	DRINK /*ADDED BY PS 3.20.15*/
RETIRED /*ADDED 5.13.15*/FBG_COVER 	GLUCOSTA 	SMOKE_LAB /*ADDED 6.3.15*/	
BIOSCANN /*ADDED 6.8.15*/EMERGEN /*ADDED 6.10.15*/
;
do i=1 to dim(numvar9); if numvar9(i) = 9 then numvar9(i) = .; end;
********************************************************************************;
*NEXT 7 LINES ARE NEW (PS - 9.6.2012);
array var99_99(*) 
LY_NO		MO_NO		RBC		HEAVYACT	HRSWKEND	MODEACT		NOACT
SEDENACT	SLIGHACT	TOTALHRS  HEIGHT	 HIP		HIP2		WAIST
WAISTB		WEIGHT1		GR_NO;
do i=1 to dim(var99_99); if var99_99(i) = 99.99 then var99_99(i) = .; end;
*******************************************************************************; 
array CHAR1(*) 
NAME_HOS CITY_HOS /* OCCUPATI */
/*ADDED 5.13.15*/ 
SPSCHST	RETIHST	RETIHNAM	RETIHCIT	SPSCHCTR	;
do i=1 to dim(CHAR1);
	if CHAR1(i) IN ('-1' '0-1' '9' '999' '999999999' '') then CHAR1(I)='                                             ';*UPDATED 6.5.15 PS;
end;
*******************************************************************************; 
IF MOBIRTYR > 2008 THEN MOBIRTYR = .; *check/fix in raw data-ps 9.3.14;
*******************************************************************************; 
*UPDATED BY PS 6.5.15;
GALLS=.;
if GBSTONES in (1 2) then GALLS=1; 
else if gbstones =3 then GALLS=2; 
*******************************************************************************;
*UPDATED BY PS 6.5.15;
galls1=.; 
if gbstones =1 then galls1=1;
else if gbstones =2 then galls1=2;
*******************************************************************************;
*UPDATED BY PS 6.5.15;
MARTSTATNUM = .;
if marstat=1 or marstat=2 then MARTSTATNUM=1;
else if marstat=3 or marstat=4 then MARTSTATNUM=2;
else if marstat ne . then MARTSTATNUM=3;
*******************************************************************************;
*REMOVED COUNTRY VARIABLES CODE (PS 9.3.14), MOVED TO BOTTOM;
*******************************************************************************;
STATEM=.; *PS initializing this variable (3.19.15);
IF empstat IN (1 2) THEN 
	do;
		STATEM=1; 
	end;
ELSE IF empstat IN (3 4 5) THEN 
	do;
		STATEM=2; 
	end;
*******************************************************************************;
*UPDATE BY PS 6.5.15;
TYPEINS=.;
IF INSTYPE in (1 2 3 4 5 6 7) THEN TYPEINS=1;
ELSE IF INSTYPE = 8 THEN TYPEINS=2;

*******************************************************************************;
*PS 08012013 UPDATE (NEXT 9 LINES)-This avoids high bmi values;
IF WEIGHT1 = 999.9 THEN WEIGHT1 = .;
IF HEIGHT = 999.9 THEN HEIGHT = .;

BMI1 = .;
BMI2 = .;
IF WEIGHT1 NE . AND HEIGHT NE . THEN
	DO;
		BMI2=weight1/((Height/100)**2);
		BMI1=round(BMI2,0.1); 
	END;

*UPDATED BY PS 6.5.15;
BMI = .;
if . < bmi1<18.5 			then BMI=1;
else if (18.5=<bmi1 <25)	then BMI=2; 
else if (25 <=bmi1<30) 		then BMI=3;   
else if 30<=bmi1 < 40 		then BMI=4; 
else if bmi1 >=40 			then BMI=5;

*******************************************************************************;
*UPDATED BY PS 6.5.15;
BMIGR=.;
if . < bmi2<30 then BMIGR=0;
else if bmi2 GE 30 then BMIGR=1;

*******************************************************************************;
*UPDATED BY PS 6.5.15;
Obese_cat=.;
if BMI2 ge 30 and BMI2 < 35 then Obese_cat=1;
else if BMI2 ge 35 and BMI2 < 40 then Obese_cat=2;
else if BMI2 ge 40 then Obese_cat=3;
else if BMI2 < 30 and BMI2 NE . then obese_cat=0;

*******************************************************************************;
*UPDATED BY PS 6.9.15;
OBESE30=.;
IF BMI1 GE 30 THEN OBESE30=1;
else IF BMI1 < 30 AND BMI1 NE . THEN OBESE30=0;
*else if obese30=0 and bmi1 NE . then obese30=.;

*******************************************************************************;
*UPDATED BY PS 6.5.15;
OBESE40=.;
IF BMI1 >= 40 THEN OBESE40=1;
else IF BMI1 < 40 AND BMI1 NE . THEN OBESE40=0;

*******************************************************************************;
*UPDATED BY PS 6.5.15;
CRP_GE1=.;
if CRP >= 1 then CRP_GE1=1;
else IF CRP < 1 AND CRP NE . THEN CRP_GE1=0;

*******************************************************************************;
*UPDATED BY PS 6.5.15;
CRP_GE2_5=.;
if CRP >= 2.5 then CRP_GE2_5=1;
else if CRP < 2.5 AND CRP NE . THEN CRP_GE2_5=0;

*******************************************************************************;
WaistM = .;
HIPM = .;
WHR = .;

IF waist NE . AND waistb NE . THEN WaistM = round(mean(waist,waistb));

IF hip NE . AND hip2 NE . THEN HIPM = round(mean(hip,hip2));*Added by IH 03292017;

IF HIPM NE . AND HIPM > 0 THEN WHR = WaistM/HIPM;*Added by IH 03292017;
********************************************************************************************;
* GS MAY/17/2012-Updating BP variables based on WELCH variables;
/**********************************************************************
Modified Pulse reassignment by Israel H. 03/29/2017 
***********************************************************************/

if	ARMW		NE . 		And 	ARM			EQ	.	then	ARM			=	ARMW	;
if	ARM_CIRW	NE . 		And 	ARM_CIRC	EQ	.	then	ARM_CIRC	=	ARM_CIRW;
if	CUFFRANW	NE '   '	And 	CUFFRAND	EQ '  '	 then	CUFFRAND	=	CUFFRANW;
if	CUFFROTW	NE '   ' 	And 	CUFFROTH	EQ '   ' then	CUFFROTH	=	CUFFROTW;
if	SYS1W		NE . 		And 	CORRSYS1	EQ 	.	then	CORRSYS1	=	SYS1W	;
if	DIA1W		NE . 		And 	CORRDIA1	EQ 	.	then	CORRDIA1	=	DIA1W	;
if	PULSE1W		NE . 		And 	PULSE1		EQ 	. then	PULSE1		=	PULSE1W/2	;
if	SYS2W		NE . 		And 	CORRSYS2	EQ 	.	then	CORRSYS2	=	SYS2W	;
if	DIA2W		NE . 		And 	CORRDIA2	EQ 	.	then	CORRDIA2	=	DIA2W	;
if	PULSE2W		NE . 		And 	PULSE2		EQ 	. then	PULSE2		=	PULSE1W	;
if	SYS3W		NE . 		And 	CORRSYS3	EQ 	.	then	CORRSYS3	=	SYS3W	;
if	DIA3W		NE . 		And 	CORRDIA3	EQ 	.	then	CORRDIA3	=	DIA3W	;
*if	EXAMDATW	NE . 		And 	EXAMDATE	EQ 	.	then	EXAMDATE	=	EXAMDATW;
if	MACHINEW	NE . 		And 	MACHINE		EQ 	.	then	MACHINE		=	MACHINEW;
if	ARM_COMW	NE '   ' 	And 	ARM_COMM	EQ '   ' then	ARM_COMM	=	ARM_COMW;
*if	EXAMINRW	NE . 		And 	EXAMINR1	EQ 	.	then	EXAMINR1	=	EXAMINRW;
*if	TIMEW		NE . 		And 	TIME		EQ 	.	then	TIME		=	TIMEW	;

/********************************************************************************;
*NEW CODE BY PS (9.4.13);
PULSE3=PULSE3W;

UPDATE 06-28-22 Requested by Dr Choh
Take only into account the last 2 readings
*/
********************************************************************************;
*MCORRSYS=ROUND(MEAN(CORRSYS1,CORRSYS2,CORRSYS3)); 
*MCORRDIA=ROUND(MEAN(CORRDIA1,CORRDIA2,CORRDIA3)); 
MCORRSYS=ROUND(MEAN(CORRSYS2,CORRSYS3)); 
MCORRDIA=ROUND(MEAN(CORRDIA2,CORRDIA3));
********************************************************************************;
If MFBG =. THEN MFBG=ROUND(MEAN(FBG1,FBG2,FBG_CRL)); *ADDED 4.30.14-PS;
IF MFBG = 0 THEN MFBG = .;
*******************************************************************************;

/* HOMA-IR AND HOMA-BETA CALUCLATION - MATTHEWS DR ET AL., DIABETOLOGIA 1985;28:412-9*/
if INS eq 0 then INS = .;  
     *Note:  Insulin levels are measured using Mercordia insulin ELISA kit';
if MFBG eq 0 then MFBG = .;  
     *Note: Gluose levels was measured in house and are being measured at Valley Baptist CRL using Colorimetric method';  

MMOL_GLUC = .;
IF MFBG NE . THEN MMOL_GLUC=MFBG*0.0555556; 
     * MMOL_GLUC is a temporary variable; 

HOMA_IR = .;  
     *Note: HOMA-IR is based on Matthew DR et al. (1985) Diabetologia ';  
     IF MMOL_GLUC NE . AND INS NE . THEN HOMA_IR = MMOL_GLUC*INS/22.5; 

HOMA_beta = .;  
     *Note: HOMA-beta (%) is based on Matthew DR et al. (1985) Diabetologia 1985';  
     IF MMOL_GLUC NE . and ins ne . THEN do;
          if MFBG > 63  then HOMA_beta = (20*ins)/(MMOL_GLUC-3.5);
          if MFBG ne . and MFBG <= 63 then HOMA_beta = -25 ; 
*-25: Not calculated due to low levels of a source variable;
*-27: Not calculated due to high levels of a source variable;  
*-15: Below detectable limit;
*-17: Above detectable limit;
     End;

*DEFINED BY DR. MCCORMICK (7.10.2013);
/*MMOL_GLUC = .;
IF MFBG NE . THEN MMOL_GLUC=MFBG*0.055;

HOMA_IR = .;
IF MMOL_GLUC NE . AND INS NE . THEN HOMA_IR = MMOL_GLUC*INS/22.5;

HOMA_beta = .;
IF MMOL_GLUC NE . THEN HOMA_beta = 20*ins/(MMOL_GLUC-3.5); *CHECK THE NEGATIVE VALUES (PS-9.3.14);*/

********************************************************************************;
*UPDATED BY PS (6.9.15);
HOMAIR_ABNORMAL=.;
IF HOMA_IR GT 3.15 THEN HOMAIR_ABNORMAL=1;
ELSE IF HOMA_IR NE . AND HOMA_IR <= 3.15 THEN HOMAIR_ABNORMAL=0;
*END DR. MCCORMICK;
********************************************************************************;
if . < MFBG < 100 then do; GLCLVL = 1; FBG_lt100=1; end;		*Normal glucose levels;
If MFBG ge 100 & MFBG le 125 then DO;
	GLCLVL = 2; FBG_BET_100_125=1; END;	*IFG glucose levels;
If MFBG gt 125 then DO; GLCLVL = 3;	FBG_GT125=1; END;			*High glucose levels;

*MOVED ADA VARAIBLES, SENT TO BOTTOM (PS-9.3.14);

*This variable name 'FBG' appears in some Cover forms. The values are 1, 2, 9.(ps 3.28.14);
*********************************************************************************;
IF ANY_DIAB_MED=. THEN ANY_DIAB_MED=0;
********************************************************************************;
FBG=.;
if  MFBG NE . AND MFBG < 100 then fbg=1;
else if  (100 <= MFBG <= 126)  then fbg=2;
else IF MFBG > 126 THEN fbg=3;
********************************************************************************;
New_diabdef=0;
IF DIABETIC=1 OR ANY_DIAB_MED=1 OR FBG=3 THEN New_diabdef=1;
*******************************************************************************;
IFG=.;
if FBG = 2 AND DIABETIC = 2 AND ANY_DIAB_MED = 0 then IFG=1;
else IFG=0;
IF IFG=0 AND FBG=. THEN IFG=.;
*******************************************************************************;
*Next 5 lines updated by PS 5.6.14;
UNDIAG=.;
if diabetic=2 and MFBG > 126 then UNDIAG=1; 
else UNDIAG=0;
if diabetic = . then UNDIAG=.;
if MFBG = . then UNDIAG=.;
******************************************************************************;

*ADDED CODE PS 3.13.15*******************************************************;
IF DIABETIC = 2 AND OTHECOMP = . AND OTHESPEC = '' AND OTHENUM = 0 
		THEN OTHENUM = .;*necessary?;
IF CVDMED = . THEN CVDMED = 0; *NEW BY PS 3.19.15;
IF ORAL_MED = . THEN ORAL_MED = 0; *NEW BY PS 4.2.15;
IF PARE_MED = . THEN PARE_MED = 0; *NEW BY PS 4.2.15;
*******************************************************************************;

RUN;

DATA C7; SET C6; DROP EXAMINER;

*IF SUBSTR(LABID,1,2) = 'BD' AND 6000 >= SUBSTR(LABID,3,4)*1 >= 4001 
		THEN DELETE; *REMOVING RAW DATA (BD4001-BD6000);
IMPACTDATA = '                   '; *TEMP CODE BY PS 5.11.15;


*IF SUBSTR(LABID,1,2) = 'BD' AND VISIT = 1 AND SUBSTR(RRID,3,4)*1 <= 2000 
		THEN DELETE; *REMOVING RAW DATA (BD4001-BD6000);
IF SUBSTR(LABID,1,2) = 'BD' AND VISIT = 1 AND SUBSTR(RRID,3,4)*1 <= 2000 
		THEN IMPACTDATA = 'IMPACTDATA'; *TEMP CODE BY PS 5.11.15;
run;
/*
TITLE1 'P8-PROC FREQ';
PROC FREQ DATA=C7; TABLE RRID*LABID/LIST MISSING; RUN;
*/
*updated by ps 2.2.15;
data impact1; LENGTH STUDY $30.; set IMPACTp.PATIENT_IMPAT; 
DROP MED10FRE CO10OTHE CO11OTHE CO12OTHE CO13OTHE CO14OTHE H_TIME
CTFACE1-CTFACE5	CO1FACE	CO2FACE	CO3FACE	CO4FACE	CO5FACE	CO6FACE
CO7FACE	CO8FACE	CO9FACE	CO10FACE CO11FACE CO12FACE CO13FACE CO14FACE
CAB_SURVEY /*NEW VARIABLE-4.9.15 PS*/	TIMESUPI /*DROPPING 6.8.15*/
TIMEBIO/*DROPPING 6.8.15*/	TIME/*DROPPING 6.8.15*/	;
where substr(rrid,1,2) = 'BD'; 
BPRANDOMZERO_FORM = 1; 
BPWELCH_FORM = 0; 

*VARIABLES WITH NAMES LONGER THAN 8 CHARACTERS************;
MED10FREQ=MED10FRE;
CO10OTHER =	CO10OTHE;     
CO11OTHER = CO11OTHE;     
CO12OTHER = CO12OTHE;     
CO13OTHER = CO13OTHE;     
CO14OTHER = CO14OTHE; 

CTFACE1_CHAR='          '; IF CTFACE1=1 THEN CTFACE1_CHAR ='FACE      ';
CTFACE2_CHAR='          '; IF CTFACE2=1 THEN CTFACE2_CHAR ='FACE      ';
CTFACE3_CHAR='          '; IF CTFACE3=1 THEN CTFACE3_CHAR ='FACE      ';
CTFACE4_CHAR='          '; IF CTFACE4=1 THEN CTFACE4_CHAR ='FACE      ';
CTFACE5_CHAR='          '; IF CTFACE5=1 THEN CTFACE5_CHAR ='FACE      ';

CO1FACE_CHAR='          '; IF CO1FACE=1 THEN CO1FACE_CHAR ='FACE      ';
CO2FACE_CHAR='          '; IF CO2FACE=1 THEN CO2FACE_CHAR ='FACE      ';
CO3FACE_CHAR='          '; IF CO3FACE=1 THEN CO3FACE_CHAR ='FACE      ';
CO4FACE_CHAR='          '; IF CO4FACE=1 THEN CO4FACE_CHAR ='FACE      ';
CO5FACE_CHAR='          '; IF CO5FACE=1 THEN CO5FACE_CHAR ='FACE      ';
CO6FACE_CHAR='          '; IF CO6FACE=1 THEN CO6FACE_CHAR ='FACE      ';
CO7FACE_CHAR='          '; IF CO7FACE=1 THEN CO7FACE_CHAR ='FACE      ';
CO8FACE_CHAR='          '; IF CO8FACE=1 THEN CO8FACE_CHAR ='FACE      ';
CO9FACE_CHAR='          '; IF CO9FACE=1 THEN CO9FACE_CHAR ='FACE      ';
CO10FACE_CHAR='          '; IF CO10FACE=1 THEN CO10FACE_CHAR ='FACE      ';
CO11FACE_CHAR='          '; IF CO11FACE=1 THEN CO11FACE_CHAR ='FACE      ';
CO12FACE_CHAR='          '; IF CO12FACE=1 THEN CO12FACE_CHAR ='FACE      ';
CO13FACE_CHAR='          '; IF CO13FACE=1 THEN CO13FACE_CHAR ='FACE      ';
CO14FACE_CHAR='          '; IF CO14FACE=1 THEN CO14FACE_CHAR ='FACE      ';

LAB_EXAMDATE = TODAYDAT;

STUDY = 'BASELINE';

run;

DATA impact2; SET impact1;
DROP
CTFACE1_CHAR	CTFACE2_CHAR	CTFACE3_CHAR	CTFACE4_CHAR	CTFACE5_CHAR
CO1FACE_CHAR	CO2FACE_CHAR	CO3FACE_CHAR	CO4FACE_CHAR	CO5FACE_CHAR
CO6FACE_CHAR	CO7FACE_CHAR	CO8FACE_CHAR	CO9FACE_CHAR	CO10FACE_CHAR
CO11FACE_CHAR	CO12FACE_CHAR	CO13FACE_CHAR	CO14FACE_CHAR	CTTIME1 
CTTIME2 		CTTIME3			CTTIME4			CTTIME5			TIMEOGTT 
CO1TIME			CO2TIME			CO3TIME			CO4TIME			CO5TIME 
CO6TIME			CO7TIME			CO8TIME			CO9TIME			CO10TIME 
CO11TIME		CO12TIME		CO13TIME		CO14TIME;/*		OCCUPATI
OCCMOSTL		SPOCCUPA		SPOCCMOS;*/
CTFACE1=CTFACE1_CHAR;
CTFACE2=CTFACE2_CHAR;
CTFACE3=CTFACE3_CHAR;
CTFACE4=CTFACE4_CHAR;
CTFACE5=CTFACE5_CHAR;

CO1FACE=CO1FACE_CHAR;
CO2FACE=CO2FACE_CHAR;
CO3FACE=CO3FACE_CHAR;
CO4FACE=CO4FACE_CHAR;
CO5FACE=CO5FACE_CHAR;
CO6FACE=CO6FACE_CHAR;
CO7FACE=CO7FACE_CHAR;
CO8FACE=CO8FACE_CHAR;
CO9FACE=CO9FACE_CHAR;
CO10FACE=CO10FACE_CHAR;
CO11FACE=CO11FACE_CHAR;
CO12FACE=CO12FACE_CHAR;
CO13FACE=CO13FACE_CHAR;
CO14FACE=CO14FACE_CHAR;
RUN;

*******************************************************************************************;
%LET ORDER = BDVISIT RRID VISIT STUDY LABID INTERVIEW_DATE;
DATA CCHC1; RETAIN &ORDER; LENGTH LABID $7.; SET impact2 C7; FORMAT LABID $7.; RUN;
/*
DATA CCHC1; RETAIN &ORDER; LENGTH LABID $7.; SET C7; FORMAT LABID $7.; RUN;
*/

DATA CCHC2; SET CCHC1;
/*
NOT DROPPING: 6.5.15
ARRTIME DEPTIME   TODAYTIM   EAT_TIME
SCHTIME 
FBGTIME
FASTTIME
TIME 		 TIMESUPI    TIMEBIO	 H_TIME 
TIME  TIMESUPI    TIMEBIO	 H_TIME TIMEOGTT
*/
drop  
/*
EXAMDATE	LASTRESULT
*/
/*HIDING (PS-11.19.13) OCCMOSTL_NUM SPOCCUPA_NUM SPOCCMOS_NUM*/ 
weightkv EXAMINER BMI2 I QINC    schgrad1 SCHCOL1 schtech1 schprec1 today today_yr	
occupati_NUM 
EXAMINR1 FINAL
USER
ECGID 
AGEdiab
age_today
length_diabetic
date
SAMEDAY5YRDRS
SINGLE_ENTRY
HBsAb DATE_HBsAb
;
*if ancestry_marker= . then ancestry_marker=0;*SHOULD BE MOVED TO 'CONSTANT' DATASET-5.13.15;
****************************************************************************;
*IF VISIT NE 1 THEN ancestry_marker = .; *LINE ADDED BY PS 3.18.15-TESTING;
*****************************************************************************;
IF VISIT < 10 THEN BDVISIT = CATS(RRID,"0",VISIT);
IF VISIT >= 10 THEN BDVISIT = CATS(RRID,VISIT);
*****************************************************************************;
DSDATE = DATE();
*****************************************************************************;
IF BPRANDOMZERO_FORM = . THEN BPRANDOMZERO_FORM = 0;
IF BPWELCH_EXAMDATE NE . THEN BPWELCH_FORM = 1; *NEW BY PS 3.27.15;
IF ARMW IN (1 2) THEN BPWELCH_FORM = 1; *NEW BY PS 3.27.15;
IF BPWELCH_FORM = . THEN BPWELCH_FORM = 0; *NEW BY PS 3.27.15;
*****************************************************************************;
IF TOWN = 1 THEN LOCATION = 'BROWNSVILLE';
IF TOWN = 2 THEN LOCATION = 'HARLINGEN  ';
IF TOWN = 3 THEN LOCATION = 'LAREDO     ';
*TOWN = 1; *FIX;
*****************************************************************************;
*UPDATED 5.13.15;
CONTROL=.; 
if ghb ge 7 then CONTROL=1; 
if ghb < 7 AND ghb NE . then CONTROL=0; 
*IF CONTROL=0 AND GHB=. THEN CONTROL=.;
******************************************************************************;
GHB_CAT=.;
if (ghb ge 6.5) then GHB_CAT=1;
else IF . < GHB < 6.5 THEN GHB_CAT=0;
******************************************************************************;
*ADA VAR BEGIN(ADDING THESE VARIABLES AND HIDING OLD 'ADA' CODE, PS 3.28.14);
ADA_DM=.;
If DIABETIC = 1 | (DIABETIC = 2 & glclvl = 3) then 
	DO; 
		ADA_DM=1; 
	END;
If DIABETIC = 2 & glclvl in ( 1  2 ) then 
	DO; 
		ADA_DM=2; 
	END;
***************************************************************************;
*ADA 2010 DIABETES DEFINITION;
ADA2010_DM=.;
if Diabetic=1 or any_diab_Med=1 or MFBG >= 126 or ghb >=6.5 then ADA2010_DM=1;
else if 
	Diabetic=2 and 
	any_diab_Med=0 and 
	(MFBG < 126 AND MFBG NE .) and 
	(ghb < 6.5 AND GHB NE .)
then ADA2010_DM=0;
****************************************************************************;
*LABEL="ADA 2010 diabetics using A1c: Dx or Taking Med or ghb >=6.5-DIABETES CARE 33 (1)";
ADA2010_diabA1c=.;
if Diabetic=1 or any_diab_Med=1 or ghb >=6.5 then ADA2010_diabA1c=1;
else if Diabetic=2 AND any_diab_Med=0 AND (ghb <6.5 AND GHB ne .) then ADA2010_diabA1c=0;
****************************************************************************;
*LABEL: "Ada categories using A1c and FBG - DIABETES CARE 33 (1)";
ADA2010_Cat=.;
If Diabetic=1 or any_diab_Med=1 or MFBG >= 126 or ghb >=6.5 then 
	ADA2010_Cat=3;*Diabetics;
else if diabetic=2 and any_diab_Med=0 and ( (100 <= MFBG < 126) or (5.7 <= ghb < 6.5)) 
	then ADA2010_Cat=2; *Impaired;
else if (diabetic=2) and (MFBG < 100 AND MFBG NE .) and (. < ghb < 5.7) and (any_diab_Med=0)
	then ADA2010_Cat=1; *NORMAL;
****************************************************************************;
*LABEL: "ADA 2010 FBG categories - DIABETES CARE 33 (1)";
ADA2010_FBGcat=.;
If . < MFBG < 100 then ADA2010_FBGcat=1; *Normal;
else if 100 <= MFBG < 126 then ADA2010_FBGcat=2; *Impaired;
     else if MFBG >= 126 then ADA2010_FBGcat=3; *diabetic;
**************************************************************************;
*LABEL: "ADA 2010 HbA1c categories - DIABETES CARE 33 (1)";
ADA2010_A1cCat= .;
IF GHB NE . AND GHB < 5.7 	THEN ADA2010_A1cCat = 1;
else IF 5.7 <= GHB < 6.5 	THEN ADA2010_A1cCat = 2; 
else IF GHB >=6.5 			THEN ADA2010_A1cCat = 3;
***************************************************************************;
*LABEL: "ADA 2010 undiagnosed: PT not diabetic (diabetic=2)and either MFBG>=126 or ghb>=6.5-
DIABETES CARE 33 (1)";
ADA2010_UNDIAG=.;
if (diabetic=2 and MFBG >= 126) or (diabetic=2 and ghb > = 6.5) then 
	ADA2010_UNDIAG=1;
else ADA2010_UNDIAG=0;
if ADA2010_UNDIAG = 0 AND GHB=. THEN ADA2010_UNDIAG=.;
if ADA2010_UNDIAG = 0 AND MFBG=. THEN ADA2010_UNDIAG=.;

*ADA VARIABLES END (ADDED THIS CODE ON 3/28/14)***************************;
**************************************************************************;
proc sort out = cchc3 nodupkey; by rrid labid;
RUN;

*UPDATING AGE variables**************************************************;
PROC SQL noprint; /*Added noprint ES 6/22/2017*/
	CREATE TABLE CCHC4 AS
	SELECT * FROM CCHC3 AS C
	/*LEFT JOIN LDOB.IDD_DOB (KEEP=RRID DOB) AS DOB1 -ORIGINAL CODE (PS 3.13.15)*/
	LEFT JOIN RAW.IDENTIFIERS (KEEP=RRID DOB) AS DOB1 
	ON C.RRID = DOB1.RRID;
QUIT;

DATA CCHC5; SET CCHC4;

*PS-5.21.14-ADDING THE NEXT 7 LINES;
AGEDIAB=.;
if diab_AGE ne . and diabetic = 1 then AGEDIAB = diab_AGE;
else if diab_yr = year(Interview_Date) then AGEDIAB = AGE_AT_VISIT;
else if diab_AGE = . and diab_yr ne . and YROB ne . then AGEDIAB = diab_yr - YROB;

IF INTERVIEW_DATE = . THEN INTERVIEW_DATE = LAB_EXAMDATE;
IF IDD_EXAMDATE = . THEN IDD_EXAMDATE = INTERVIEW_DATE;

age_today=floor((DSDATE-DOB)/365.25);
length_diabetic=AGE_today - AGEDIAB;
today_yr=year(DSDATE);

ADIPONECTIN_mcg = ADIPONECTIN/1000000; *new by ps 10.23.14;
RESISTIN_ng = RESISTIN/1000; *new by ps 10.23.14;

**********************************************************************;
FORMAT /*IDD_Date*/
BIODATE			ECGDATE		LAB_EXAMDATE	H_DATE			Interview_Date
ANTH_EXAMDATE	BP_EXAMDATE	MHX_EXAMDATE 	DHX_EXAMDATE 	EXIT_EXAMDATE 
SMOKE_EXAMDATE 	TODAYDAT	BP_examdate 	DATE_CHOL1 		DATE_HDLC
DATE_CREA 		DATE_CRP 	DATE_HDLC 		DATE_LDLCALC 	DATE_URIC 
DATE_CHOL1    	FASTDATE	DATE_GOT 		DATE_GPT 		DATE_TBIL 
DATE_TRIG 		DATE_ALP 	DATE_CHLR 		DATE_DBIL 		DATE_LALB 
DATE_TP 		DSDATE 		IDD_EXAMDATE 	DATE9.
PACESTDY 		PLAQUE_ULT 	AGREECONT 		FASTING_COVER 	FBG_COVER
MORETEST 		SMOKE_BP 	BORDPLX 		ULTRASTY 		TRIEDCIGS
/*TRIEDECIG*/	TRIEDCIGAR	TRIEDSNUS		TRIEDSNUFF		BRACHSONO1
BRACHSONO2		CCASONO1	CCASONO2		ECHOSONO1		ECHOSONO2
LIVERSONO1		LIVERSONO2	DEXASONO1		DEXASONO2		BLDSTOR
AGREE_URINE		AGREE_STOOL	AGRFUTRTEST		YES_NOX.
HISPANIC 		HISPANCX.
SPSCHTEC 		SCHTECH 	TECHSCHX.
SCHCOL 			SPSCHCOL 	COLLEGEX.
SCHGRAD 		SPSCHGRA 	GRADSCHX.
ARM 			ARMW 		ARMX.
TUBECPT 		EDTAX.
SES 			QUARTILE.
BPRANDOMZERO_FORM 	BPWELCH_FORM 	DEAD 	microarray  COHORF.		;
PROC SORT; BY BDVISIT;
RUN;

********************************************************************************************;
/*
DATA CSFIVE; SET RAW.CONTACTSCHEDULINGPAT_converted; 
			KEEP FYR_SURVEY RRID; IF STUDY = 'FIVE YEAR'; 
FYR_SURVEY = 1;
FORMAT FYR_SURVEY YES_NOX.;
RUN;

DATA PACE1; SET P.FINAL_PART5; KEEP RRID PACE_NEW; PACE_NEW = 1; RUN;
PROC SORT DATA=PACE1 OUT=PACE2 NODUPKEY; BY RRID; RUN;

DATA OB1; SET OB.OBMAIN; KEEP KEYOB RRID; RUN;
PROC SORT DATA=OB1 OUT=OB2 NODUPKEY; BY RRID; RUN;
*/
*******************************************************************************************;
*PACESTDY;
*if pace_survey= . then pace_survey=0;

*10.27.14	PS	*ADDING SAMPLING WEIGHTS, HCV, HBV, INVENTORY DATA*************************;

DATA CCHC6; SET CCHC5; DROP DEAD DEATH_COMMENTS /*W*/
FYR_SURVEY /*DROPPING THIS VARIABLE (PS 4.6.15)*/
/*DROPPING VARIABLES (PS 5.5.15)*/
OB_SURVEY
/*CAB_SURVEY*/
DRS_SURVEY
PACE_SURVEY
DRS_SURVEY
W_RANGE
w
W_DATE

/* IZ 10032017 */
LIP1
PLAS1_1
PLAS1_2
PLAS1_3
PLAS1_4
PLAS1_5
PLAS1_6
BUFF1
RBC1
RBC2
SER1
SER2
SER3
SER4
PLAS2_1
PLAS2_2
PLAS2_3
PLAS2_4
PLAS2_5
PLAS2_6
PLAS2_7
PLAS2_8
BUFF2
PLAS3_1
PLAS3_2
PLAS3_3
HCV
HR
Pa
Pmax
Pmin
Pd
PAm
PRI
QRSD
QT
QTC
QRSa
Ta
RV5
SV1
R_S
ER
INF
LAT
V
INF_V
LAT_V
Ja
STj
hST
DIAPHRAGM
Diaphragm_notes
VESSEL
Vessel_notes
HEPAECHO
FATTYLIVER
Fatty_liver_notes
LIVER_ULT_NOTES
APRI
APRI_C1
BARD
BARD_C
FIB4_U
FIB4_C
NAF_U
NAF_C
NAF_C2
N_LF
N_LFC
ADIPONECTIN_adj
RESISTIN_adj
IL_6_adj
LEPTIN_adj
IL_8_adj
TNF_ALFA_adj
IL_1BETA_adj
/* IZ 10032017 */

; RUN;

PROC SQL noprint;	/*Added noprint ES 6/22/2017*/
	CREATE TABLE A1 AS
		SELECT * FROM CCHC6 as C1
	LEFT JOIN HBV.hbvcore_all AS HBC
    	ON C1.LABID = HBC.LABID
    LEFT JOIN HBV.hbvs_antibody_all  AS HBSAB
    	ON C1.LABID = HBSAB.LABID
/*    LEFT JOIN INV._inventory_all (DROP=RRID) AS INV    */
	 LEFT JOIN INV._inventory_all AS INV
    	ON C1.LABID = INV.LABID
    LEFT JOIN SW.cohort_w AS S1 
    	ON C1.RRID = S1.RRID

/*Rename dataset from HCV_ALL to FINAL_HCV 12122016 IH *****************/
    LEFT JOIN HCVL.FINAL_HCV AS HCVDATA
    	ON C1.LABID = HCVDATA.LABID
    LEFT JOIN ECGXI.ECG_ALL AS ECGX
    	ON C1.LABID = ECGX.LABID
/*Rename dataset from hbvs_antigen_all to final_hbsag 12122016 IH *****************/
    LEFT JOIN HBV.FINAL_HBSAG AS HBANTI
    	ON C1.LABID = HBANTI.LABID
/*    LEFT JOIN LIVER2.liverultresults (DROP=RRID)AS LR
    	ON C1.LABID = Lr.LABID
    LEFT JOIN LIVER2.liver_scores AS LS
    	ON C1.LABID = LS.LABID								12112017 IH    */
    LEFT JOIN HEV.hbeag_all AS HE
    	ON C1.LABID = HE.LABID
    LEFT JOIN LD.DECEASED AS DEC
    	ON C1.RRID = DEC.RRID
/*MERGE LUMINEX ADJUSTED VALUES (FROM UTHSCSA) 
		(03.23.15-PS)-MAY MOVE CODE TO FIRST PROGRAM LATER*****************/
	LEFT JOIN LUMIN2.LUMINEX_ADJUSTED AS L8
		ON C1.LABID = L8.LABID
	LEFT JOIN FINAL.OTHER_STUDIES AS OTHER
		ON C1.RRID = OTHER.RRID
;
QUIT;

/*
PROC FREQ; TABLE RRID*VISIT*W / LIST MISSING; RUN;
PROC FREQ DATA=SW.cohort_w; RUN;
*/

/*

PROC SQL noprint;	
	CREATE TABLE A1 AS
		SELECT * FROM CCHC6 as C1
	 FULL JOIN INV._inventory_all AS INV
    	ON C1.LABID = INV.LABID
;
QUIT;


data my_data;
set a1;
keep rrid labid plas1_1 plas1_2 plas1_3;
where labid = "5Y0002";
run;


data my_dat;
set a2;
format myurindat date9.;
format urindat date9.;
IF URINDAT NE . THEN
    DO;
        URINDAT = URINDAT/86400;
        MYURINDAT=URINDAT/86400;
	END;
keep rrid visit study urindat myurindat;
run;
*/

DATA A2; SET A1; drop LOC_CITY LOC_STATE KEYOB /*EXAMDATE*/;

IF H_DATE > . THEN HEM_EXAM = 1;

IF KEYOB NE '' THEN OB_SURVEY = 1;
/*
IF STUDY = "PEDIATRIC" then STUDY = "PEDIATRIC BASELINE";
*/

FORMAT TODAYDAT EAT_DATE W_DATE DATE9. PLAINRED EDTAX.;

*EDTA_2ML MUSTARD_5ML STAFFID;			
*end of 12.1.14 labels;			
*next labels added on 12.1.14;
	
/*DRSEXAMDATE = */

LABEL 
		
ADIPONECTIN_mcg = "Adiponectin (mcg/ml)"			
AGE = "Q7 Patient age reported (Only asked at baseline)"			
AGE_ECHO = "Age (At Echo)"			
AGE_HOS = "If you were hospitalized since your last visit, what was your age?"			
AGE_TODAY = 'Present age calculated'			
AGEDIAB = 'Diabetic Age calculated'			
AGREE_STOOL = "Agree to Stool Sample?"			
AGREE_URINE = "Agree to Urine Sample?"			
AGREECONT = "Agree to be contacted to inform about community health events?"			
AGRFUTRTEST = "Agree to Future Tests?"			
ALK        ="VBMC ALKALINE PHOSPHATASE"			
ARM_CIRW = "2. Arm Circumference (Welch Allyn)"			
ARM_COMW = "Arm used - Comments (Welch Allyn)"			
ARMW = "1. Arm used for blood pressure measurements (Welch Allyn)"			
BASO_NO = "Number of basophils (10^3/ul)"			
BDVISIT = "RRID + Visit"			
BFFCIGR="7. If one of your best friends were to offer you ___ would you smoke it? (Cigars, cigarillos, or little filtered cigars)"			
BFFCIGS="7. If one of your best friends were to offer you ___ would you smoke it? (Cigarettes)"			
BFFECIG="7. If one of your best friends were to offer you ___ would you smoke it? (E-cigarettes)"			
BFFSNUF="7. If one of your best friends were to offer you ___ would you smoke it? (Moist snuff, dip, or chewing tobacco)"			
BFFSNUS="7. If one of your best friends were to offer you ___ would you smoke it? (Snus)"			
BLDSTOR = "Allow Blood Storage?"			
BMI1 = "Body Mass Index: [Weight1/(Height*Height)]"			
BORDPLX = "Is PT in the BorderPlex Study? (Cover form)"			
BORN_MX = 'Participant born in Mexico?'			
BORN_US = 'Participant born in USA?'			
BOTHMEXICO = "Both parents were born in the following country:"/**/		
BRACHDATE1 = "Brachial sonogram date (Initial)"			
BRACHDATE2 = "Brachial sonogram date (Re-scan)"			
BRACHSONO1 = "Brachial sonogram (Initial)"			
BRACHSONO2 = "Brachial sonogram (Re-scan)"			
BUFF1= "(Inventory) Buffy 1"			
BUFF2= "(Inventory) Buffy 2"			
BUN        ="VBMC UREA NITROGEN BLOOD"			
		
CALC       ="VBMC CALCIUM"			
CCADATE1 = "Carotid sonogram date (Initial)"			
CCADATE2 = "Carotid sonogram date (Re-scan)"			
CCASONO1 = "Carotid sonogram (Initial)"			
CCASONO2 = "Carotid sonogram (Re-scan)"			
CHL        ="VBMC CHLORIDE"			
CHLR = "Cholesterol ratio (CHLR)"			
CIG_EDAY = "1D. If everyday, then: On average, how many cigarettes per day do you usually smoke?"			
CIG_SDAY = "1E. If some days, then: On average, how many cigarettes per day do you usually smoke?"			
CITY_HOS = '5A. Since your last visit have you been HOSPITALIZED? In what city: (DRS Only)'			
CO2        ="VBMC CARBON DIOXIDE"			
CREA = 'CREATININE (mg/dL)' /*MOVE TO OTHER PROGRAM? 09052013-PS*/	
CRL_COMMENTS = "CRL results - Comments"			
CTRYBIR = 'Country of birth'			
CUFFRANW = "Record Cuff Size used (Welch Allyn)"			
CUFFROTW = "3. Cuff Size used (Welch Allyn)"			
date_srage 	= 'Date when the sRAGE test was run'	/*move to other program?*/
DEXADATE1 = "Dexa sonogram date (Initial)"			
DEXADATE2 = "Dexa sonogram date (Re-scan)"			
DEXASONO1 = "Dexa sonogram (Initial)"			
DEXASONO2 = "Dexa sonogram (Re-scan)"			
DIA1W = 'Diastolic BP Reading 1 (Welch Allyn)'			
DIA1W = 'Diastolic BP Reading 1 (Welch Allyn)'			
DIA2W = 'Diastolic BP Reading 2 (Welch Allyn)'			
DIA2W = 'Diastolic BP Reading 2 (Welch Allyn)'			
DIA3W = 'Diastolic BP Reading 3 (Welch Allyn)'			
DIA3W = 'Diastolic BP Reading 3 (Welch Allyn)'			
DIAG_HOS = "If you were hospitalized since your last visit, what was the diagnostic?"			
DLDL = "Direct Low-Density Lipoprotein"			
DRS_survey	='Does PT have the DRS Survey?'		
DSDATE = 'Data Set last updated on:'			
ECHODATE1 = "Echo sonogram date (Initial)"			
ECHODATE2 = "Echo sonogram date (Re-scan)"			
ECHOSONO1 = "Echo sonogram (Initial)"			
ECHOSONO2 = "Echo sonogram (Re-scan)"			
EDTA_2ML = "CRL results: EDTA 2ML"			
EDTA1 = "EDTA #1 (purple top) - 10 ml Vacutainer filled?"			
EDTA2 = "EDTA #2 (purple top) - 10 ml Vacutainer filled?"			
EDTA3 = "EDTA #3 (purple top) - 2 ml Vacutainer filled?"			
EDTA4 = "EDTA #4 (purple top) - Vacutainer filled?" /* ADD THE MILLILITER */	
EDUCLVL = 'Level of education (Continuous)'			
EOS_NO = "Number of eosinophils (10^3/ul)"		
EXAMDATE = "DRS Exam date"
Examdate_Echo = "Echo Exam date"			
EXIT_EXAMDATE = 'WHAT IS THE COVER EXAM DATE?'		
FASTING_COVER = "10 hours fasting? (Cover Form)"
FBG_CRL = 'FBG run at CRL'	
FBG_COVER = "2nd FBG Needed? (Cover Form)"
FourMexico = "All four grandparents were born in the following country:"			
FYR_SURVEY	='Does PT have the 5YR Survey?' 		
GFR        ="VBMC ESTIMATED GFR"			
HEMAT_COMMENTS = 'Hematology form - Comments' /*use this variable for cbc comments:*/			
HISPANIC = "Are you Hispanic, Latino, or Spanish origin?"			
HISPANIC_OTH = "HISPANIC (OTHER)"			
HOSPITAL = "Since your last visit have you been hospitalized?"			
INOUT = "INOUT"			
KEY7 = 'KEY7'			
KEY8 = "KEY8"			
LANGUA = "Language (Cover form)"			
lastcontact='Last contact number'			
lastcontactpage='Page where the participant got the last contact result'			
LENGTH_DIABETIC = 'Length of time with diabetes'			
LEVEL_EDU = 'Level of education (Completed/Not Completed High School)' /* check these values/formats*/			
LIP1 = "(Inventory) Lipids 1"			
LIVERDATE1 = "Liver sonogram date (Initial)"			
LIVERDATE2 = "Liver sonogram date (Re-scan)"			
LIVERSONO1 = "Liver sonogram (Initial)"			
LIVERSONO2 = "Liver sonogram (Re-scan)"			
MACHINEW ="6. Machine Number (Welch Allyn)"			
MCGLUC     ="VBMC CALCULATED MEAN GLUCOSE"			
MCORRDIA='MEAN DIASTOLIC BP' 			
MCORRSYS='MEAN SYSTOLIC BP'			
MED10FREQ = "Q14.10 DO YOU TAKE THIS MEDICATION DAILY OR NOT DAILY?"			
MFBG='Mean FBG'			
MORETEST = "Allow more tests in the future? (Cover form)"			
MUSTARD_5ML = "CRL results: MUSTARD_5ML"			
NAME_HOS = 'If you have been hospitalized since your last visit, what is the hospital name? (DRS Only)'			
NEUT_NO = "Number of neutrophils (10^3/ul)"			
NEXTDUEDATE = "Next due date (5YR/DRS)" /*NEW LABEL (3.23.15) PS*/
OB_SURVEY = "Does the participant have the OB Survey?"			
PACE_SURVEY	='Does PT have the PACE Survey?'		
PACESTDY = "Is the PT in the Pace Study? (Cover form)"			
PLAINRED = "PLAIN RED (red top) – 7ml Vacutainer filled?"			
PLAS1_1= "(Inventory) Plasma 1.1"			
PLAS1_2= "(Inventory) Plasma 1.2"			
PLAS1_3= "(Inventory) Plasma 1.3"			
PLAS1_4= "(Inventory) Plasma 1.4"			
PLAS1_5= "(Inventory) Plasma 1.5"			
PLAS1_6= "(Inventory) Plasma 1.6"			
PLAS2_1= "(Inventory) Plasma 2.1"			
PLAS2_2= "(Inventory) Plasma 2.2"			
PLAS2_3= "(Inventory) Plasma 2.3"			
PLAS2_4= "(Inventory) Plasma 2.4"			
PLAS2_5= "(Inventory) Plasma 2.5"			
PLAS2_6= "(Inventory) Plasma 2.6"			
PLAS2_7= "(Inventory) Plasma 2.7"			
PLAS2_8= "(Inventory) Plasma 2.8"			
PLAS3_1= "(Inventory) Plasma 3.1"			
PLAS3_2= "(Inventory) Plasma 3.2"			
PLAS3_3= "(Inventory) Plasma 3.3"			
POT        ="VBMC POTASSIUM"			
PULSE1W = "Reading #1: Pulse (Welch Allyn)"			
PULSE2W = "Reading #2: Pulse (Welch Allyn)"			
PULSE3W = "Reading #3: Pulse (Welch Allyn)"			
RACE = "8B. What is your race?"			
RACE_OTH = "RACE (OTHER)"			
RBC1= "(Inventory) RBC 1"			
RBC2= "(Inventory) RBC 2"			
RESISTIN_ng = "Resistin (ng/ml)"			
RNA = "BD PAX gene (blood RNA tube) - 2.5 ml Vacutainer filled?"			
SCHOOL = 'Years of school (>8, <=8)' /*check these values*/	
SECTION = "SECTION"			
SER1= "(Inventory) Serum 1"			
SER2= "(Inventory) Serum 2"			
SER3= "(Inventory) Serum 3"			
SER4= "(Inventory) Serum 4"		
SMOKE_BP = "2. Have you smoked or used chewing tobacco, snuff, or nicotine gum within the last 4 hours? (Random Zero BP Form)"  
SMOKE_LAB = "HHRC Q5 DID YOU SMOKE OR USE TOBACCO IN THE LAST 10 HOURS? (Lab Form)"
SOD        ="VBMC SODIUM"			
srage 		= 'sRage pg/ml'	
STAFFID = "Staff ID entering CRL results"			
STARTAGE_CIGR="5. How old were you when you first tried ___, even one or two puffs? (Cigars, cigarillos, or little filtered cigars)"
STARTAGE_ECIG="5. How old were you when you first tried ___, even one or two puffs? (E-cigarettes)"			
STARTAGE_SNUF="5. How old were you when you first tried ___, even one or two puffs? (Moist snuff, dip, or chewing tobacco)"			
STARTAGE_SNUS="5. How old were you when you first tried ___, even one or two puffs? (Snus)"			
STARTYR_CIGR="5. In what year did you first try ___, even one or two puffs? (Cigars, cigarillos, or little filtered cigars)"			
STARTYR_ECIG="5. In what year did you first try ___, even one or two puffs? (E-cigarettes)"			
STARTYR_SNUF="5. In what year did you first try ___, even one or two puffs? (Moist snuff, dip, or chewing tobacco)"			
STARTYR_SNUS="5. In what year did you first try ___, even one or two puffs? (Snus)"			
STUDY = "STUDY"			
SYS1W = 'Systolic BP Reading 1 (Welch Allyn)'			
SYS2W = 'Systolic BP Reading 2 (Welch Allyn)'			
SYS3W = 'Systolic BP Reading 3 (Welch Allyn)'			
TIME="Q9 AT WHAT TIME WAS PULSE AND BLOOD PRESSURE TAKEN?"
TIMEBIO="Q2 WHAT TIME WERE THE BIOIMPEDANCE READINGS TAKEN?"
TIMESUPI="Q1 WHAT TIME DID SUPINE POSITION BEGIN?"
TOWN = "TOWN"			
TRIEDCIGAR="2. Have you ever tried ___, even one or two puffs? (Cigars, cigarillos, or little filtered cigars)"			
TRIEDCIGS = "3. How many times have you ever tried this product? (Cigar, cigarillo or little filtered cigars)"			
TRIEDECIGS="2. Have you ever tried ___, even one or two puffs? (E-cigarettes)"			
TRIEDSNUFF="2. Have you ever tried ___, even one or two puffs? (Moist snuff, dip, or chewing tobacco)"			
TRIEDSNUS="2. Have you ever tried ___, even one or two puffs? (Snus)"			
TUBECPT = "CPT TUBE filled?"			
TUBEMUST = "MUSTARD TOP Vacutainer filled?" /*TUBE_MUST in pace access db*/			
ULTRASTY = "Is the PT in the Ultrasound Study? (Cover form)"			
URINDAT = "Urine collection date"			
/*2 URINE VARIABLES?*/		URINSAMP = "Urine collected?"	
/*2 URINE VARIABLES?		URNCOLLECTED = 'Urine collected'*/
VISIT = "Visit number"			
		
WBSCIGR="8. Do you think you will be smoking ___ in a year from now? (Cigars, cigarillos, or little filtered cigars)"			
WBSCIGS="8. Do you think you will be smoking ___ in a year from now? (Cigarettes)"			
WBSECIG="8. Do you think you will be smoking ___ in a year from now? (E-cigarettes)"			
WBSSNUF="8. Do you think you will be smoking ___ in a year from now? (Moist snuff, dip, or chewing tobacco)"			
WBSSNUS="8. Do you think you will be smoking ___ in a year from now? (Snus)"
WHR="WAIST TO HIP RATIO"	
WTSCIGR="6. Do you think that you will try ___ soon? (Cigars, cigarillos, or little filtered cigars)"			
WTSCIGS="6. Do you think that you will try ___ soon? (Cigarettes)"			
WTSECIG="6. Do you think that you will try ___ soon? (E-cigarettes)"			
WTSSNUF="6. Do you think that you will try ___ soon? (Moist snuff, dip, or chewing tobacco)"			
WTSSNUS="6. Do you think that you will try ___ soon? (Snus)"			
XCIG30DAYS = "4. How many times in the past 30 days? (Cigar, cigarillo or little filtered cigars)"			
XCIGR30DAYS="4. How many times in the past 30 days? (Cigars, cigarillos, or little filtered cigars)"			
XECIG30DAYS="4. How many times in the past 30 days? (E-cigarettes)"			
XSNUF30DAYS="4. How many times in the past 30 days? (Moist snuff, dip, or chewing tobacco)"			
XSNUS30DAYS="4. How many times in the past 30 days? (Snus)"			
XTRIEDCIGAR="3. How many times have you ever tried this product? (Cigars, cigarillos, or little filtered cigars)"			
XTRIEDCIGS = "3. How many times have you ever tried this product? (E-cigarettes)"			
XTRIEDECIGS="3. How many times have you ever tried this product? (E-cigarettes)"			
XTRIEDSNUFF="3. How many times have you ever tried this product? (Moist snuff, dip, or chewing tobacco)"			
XTRIEDSNUS="3. How many times have you ever tried this product? (Snus)"			
YEAR_HOS = "If you were hospitalized since your last visit, in what year did it happen?"			
YEARS_EDU='Years of education (Continuous)'			
;
RUN;
/*
STARTYR_CIGS = "5. In what year or how old were you when you first tried this product,
					even one or two puffs?"
*/
/*DATE_VBMC  ="VBMC COLLECTION DATE"*/		
*NOTE: The data set WORK.A2 has 2860 observations and 1332 variables.;

%LET ORDER = BDVISIT RRID VISIT STUDY LABID INTERVIEW_DATE;
DATA A3; RETAIN &ORDER; SET A2;
DROP DOB
DATE_URINE_COLLECTED
/*URINE_COLLECTED*/
   /*CBC_DIFF                   */
   CRLDATA_STAFFID            
   DATE_CBC_DIFF              
  /*DATE_CRL                   */
  DATE_CRLDATA_STAFFID       
   DATE_CRL_COMMENTS          
   /*URINE_COLLECTED    */
BDLOCKED
GHB_DSYEAR
LASTEXAMDATE
IMPORT_FLAG
; 

IF IDD_EXAMDATE = . THEN IDD_EXAMDATE = INTERVIEW_DATE;

*NEXT 5 ROWS ADDED BY PS 3.6.15*;
SCHST=UPCASE(SCHST);
SPOCCDES = UPCASE(SPOCCDES);
SPSCHST= UPCASE(SPSCHST);
SPSCHCTR= UPCASE(SPSCHCTR);
MOCANTCO= UPCASE(MOCANTCO);
FECANTCO= UPCASE(FECANTCO);

IF SCHST = 'TX' THEN SCHST = 'TEXAS';
IF FBIRST = 'TX' THEN FBIRST = 'TEXAS';
IF MBIRST = 'TX' THEN MBIRST = 'TEXAS';
IF FMBIRST = 'TX' THEN FMBIRST = 'TEXAS';
IF FFBIRST = 'TX' THEN FFBIRST = 'TEXAS';
IF MFBIRST = 'TX' THEN MFBIRST = 'TEXAS';
IF MMBIRST = 'TX' THEN MMBIRST = 'TEXAS';

IF SPSCHCTR = 'MX' THEN SPSCHCTR = 'MEXICO'; *PS 6.3.2015;
IF SPSCHCTR = 'US' THEN SPSCHCTR = 'USA'; *PS 6.3.2015;

IF SCHST = '9' THEN SCHST = ' ';

*NEXT IF/THEN STATEMENT IS NEW BY PS (2.2.15);
IF NOACT IN (0 .) AND HEAVYACT IN (0 .) AND MODEACT IN (0 .) AND 
	SLIGHACT IN (0 .) AND SEDENACT IN (0 .) AND TOTALHRS IN (0 .) THEN 
	DO;
		NOACT = .;
		HEAVYACT = .;
		MODEACT = .;
		SLIGHACT = .;
		SEDENACT = .;
		TOTALHRS = .;
		HRSWK =  .;
		HRSWKEND = .;
	END;

*IF LABID = '' THEN DELETE;*PS 11.17.14-FINAL DATA SET SHOULD NOT HAVE MISSING LABIDS;

*IF STUDY = 'FYR' 						THEN STUDY = 'FIVE YEAR ';

IF SUBSTR(LABID,1,1) = '5' THEN STUDY = 'FIVE YEAR '; 
*CHECK ASAP (PS-3.26.15) BASELINE      BD1691    5Y0562;

*IF INTERVIEW_DATE = . AND LABID NE '' THEN DELETE;
*IF INTERVIEW_DATE NE . AND LABID = '' THEN DELETE;

*IF VISIT = 1 AND FYR_SURVEY = . THEN FYR_SURVEY = 0; *ADDED BY PS 4.6.15;

IF TOWN = 3 AND MPV IN (-1 0) THEN MPV = .; *ADDED BY PS 5.5.15;

IF town = 2 and tract = 115 then ses = 3;
IF town = 2 and tract = 116 then ses = 2;
IF town = 2 and tract = 117 then ses = 3;

/*
If SPSCHPRE = . and SPSCHTEC=0 and SPSCHCOL=0 and SPSCHGRA=0 then 
*/
PROC SORT; BY LABID;
RUN;
/*
TITLE1 'MERGED';
PROC FREQ DATA=RAW.MERGED1; TABLE RRID*INTERVIEW_DATE*LABID / LIST MISSING; 
WHERE RRID IN ('BD0001' 'LD0113' 'LD0150'); RUN;
TITLE1 'A2';
PROC FREQ DATA=A2; TABLE RRID*INTERVIEW_DATE*LABID / LIST MISSING; 
WHERE RRID IN ('BD0001' 'LD0113' 'LD0150'); RUN;
TITLE1 'A3';
PROC FREQ DATA=A3; TABLE RRID; RUN;
*/
********************************************************************************************;
/*
PROC FREQ DATA=A3; TABLE RRID*LABID*RESULTCOUNT*VISIT / LIST MISSING;
WHERE RESULTCOUNT NOT IN ('1111111111111' ''); 
RUN;
*/
/*
PROC FREQ DATA=A3; TABLE INTERVIEW_DATE*STUDY*RRID*LABID*RESULTCOUNT*VISIT / LIST MISSING;
WHERE LABID = '' AND RESULTCOUNT NE '1000000000000';
RUN;
*/
/*
PROC FREQ DATA=A3; TABLE INTERVIEW_DATE*STUDY*RRID*LABID*RESULTCOUNT*VISIT / LIST MISSING;
WHERE INTERVIEW_DATE = . AND LABID NE '';
RUN;
*/
/*
PROC FREQ DATA=A3; TABLE INTERVIEW_DATE*STUDY*RRID*LABID*RESULTCOUNT*VISIT / LIST MISSING;
WHERE INTERVIEW_DATE NE . AND LABID = '';
RUN;

PROC FREQ; TABLE RESULTCOUNT; RUN;
*/
/*PROC FREQ DATA=A2; TABLE STUDY*LABID / LIST MISSING; RUN;*/
/*
PROC FREQ DATA=A3; TABLE INTERVIEW_DATE*STUDY*LABID / LIST MISSING; RUN;
*/
/*
TITLE '1. WHERE GENDER, KEY7, KEY8, SES, OR QUINC IS MISSING';
PROC PRINT DATA=A3; 
VAR RRID LABID VISIT STUDY TRACT BLOCK GENDER KEY7 KEY8 SES QUINC ; 
WHERE GENDER = . OR KEY7 = '' OR KEY8 = '' OR  SES = . OR  QUINC = .;
RUN;
*/
/*
TITLE '2. WHERE LABID IS BLANK';
PROC FREQ DATA=A3; TABLE RRID KEY7 KEY8 SES QUINC LABID;WHERE LABID = '' AND VISIT = 1;RUN;
*/
/*
TITLE '3. WHERE WBC, LY_NO, MO_NO, OR GR_NO IS ZERO';
PROC PRINT NOOBS; VAR LABID WBC LY_NO MO_NO GR_NO; 
WHERE WBC = 0 OR LY_NO= 0 OR MO_NO= 0 OR GR_NO= 0; RUN; *BD5790;
*/
******************************************************************************************;
*CAN THIS VARIABLE BE UPDATED? PACESTDY-PS 3.27.15;
******************************************************************************************;

/*
proc sort data=a3 nodupkey; by rrid visit; run;
*/

/*data thisisatest;
set a3;
where visit = 2;
run;*/

DATA A4; SET A3; DROP RESULTCOUNT min_part min_part_exp max_part max_part_exp;
IF LABID = '' THEN
	DO;
		cvdmed=.;
		ANY_DIAB_MED=.;
		ORAL_MED=.;
		New_diabdef=.;
		IFG=.;
		ADA2010_Cat=.;
		ADA2010_DM=.;
		ADA2010_diabA1c=.;
		UNDIAG=.;
		BPRANDOMZERO_FORM=.;
		BPWELCH_FORM=.;
		PARE_MED=.;
		age_today = .;
		*today_yr = .;
		*FYR_SURVEY=.; *ADDED BY PS 4.6.15;

	END;

IF VISIT = . THEN DELETE;
IF STUDY = '' THEN DELETE;

*IF LASTRESULT = 1 AND LABID = '' THEN DELETE; *PS 6.9.15;
IF LABID = '' THEN DELETE; *IH 4.4.17;

*ADDED BY PS 6.3.15*************************************************************************;
IF CRP = 0 THEN CRP = .; 
/*LD010101*/	*IF C_hhincmth =99999998 THEN C_hhincmth = .;
/*HD022001,LD012701*/	*IF C_hhincyr IN (999999 20000000) THEN C_hhincyr = .;
/*HD021801,HD022301*/ IF CHOL IN (1 9) THEN CHOL = .;
/*LD015101;LD015201*/	IF CHOL1 = 0 THEN CHOL1 = .;
/*LD016201;LD017101;LD018201*/ IF DBIL = 0 THEN DBIL = .;
/*BD208312	DIABETIC	YES	XMissing*/
/*LD016201;LD017101;LD018201*/	IF DLDL = 0 THEN DLDL = .;
/*LD004301*/ IF GHB = 0 THEN GHB = .;
/*HD022001*/ *IF	HHINCYR	= 75000999 THEN HHINCYR	= .;
/*LD010101*/ *IF	INCMONTH = 99999998 THEN INCMONTH =.;
/*HD022001*/ *IF	INCYEAR	= 20000000 THEN INCYEAR=.;
/*LD005701*/ IF ldlcalc = 0 THEN ldlcalc = .;
/*HD017501;LD002501*/ IF TOWN > 1 THEN PACESTDY = 2;
/*LD004101*/ IF SCHCTRY='UX ' THEN SCHCTRY='USA';
/*LD006301;LD012701*/	*IF SPINCYR IN (9999999 999999) THEN SPINCYR = .;
/*HD021801;HD022301*/ IF TRIGS IN (1 9) THEN TRIGS = .;
/*LD016201;LD017101;LD018201*/ IF URIC = 0 THEN URIC = .;
/*LAREDO*/ IF ALP=0 THEN ALP = .;
/*LAREDO*/ IF BASO_NO=0 THEN BASO_NO = .;
/*LAREDO*/ IF ALK = 0 THEN ALK = .;
/*LAREDO*/ IF CANCERCO = '9' THEN CANCERCO = '';
*END IF ADDING 6.3.15***********************************************************************;

/*ADDED 6.8.15 - PS */
/*
REMOVED AS OF 6.24.15-PS
IF RRID = 'HD0220' AND HHINCMTH = 6250083 AND HHINCYR = 75000999 THEN
	DO;
		HHINCMTH=.;
		HHINCYR=.;
	END;
*/

/*ADDED 6.8.15 - PS */
*IF LABID = '' AND TOWN > 1 THEN DELETE;

/*ADDED 6.8.15 - PS */
PLT = MAX(PLTCNT,PLT);

/*ADDED 6.9.15 - PS */
/*
REMOVED AS OF 6.24.15-PS
IF RRID='HD0120' AND VISIT=1 AND BIO3A = . AND BIO3C = . THEN
	DO;
		BIO3A = 1026;
		BIO3C = 1026;
	END;
*/

/*ADDED 10.17.22 - HS */
if gender = 1 and CREA^=. then 
do;
min_part=min((CREA/0.9),1.0);
min_part_exp=min_part**-0.302;
max_part=max((CREA/0.9),1.0);
max_part_exp=max_part**-1.200;
CKD_EPI_GFR_Calc = 142*min_part_exp*max_part_exp*(0.9938**age_At_visit);
end;

if gender = 2 and CREA^=. then 
do;
min_part=min((CREA/0.7),1.0);
min_part_exp=min_part**-0.241;
max_part=max((CREA/0.7),1.0);
max_part_exp=max_part**-1.200;
CKD_EPI_GFR_Calc = 142*min_part_exp*max_part_exp*(0.9938**age_At_visit)*1.012;
end;


LABEL 
CKD_EPI_GFR_Calc="CKD-EPI creatinine,age, and sex equation 2021 to calculate eGFR variable. Equation used eGFR = 142*(min((CREA/k),1)**a)*(max((CREA/?),1)**-1.200)*(0.9938**age_At_visit)*1.012[if gender=2], ? = 0.7 (females) or 0.9 (males), a = -0.241 (female) or -0.302 (male)"
;
RUN;
/*
PROC FREQ; TABLE RRID; RUN;
*/
/*
PROC FREQ DATA=A4;TABLE ANY_DIAB_MED New_diabdef UNDIAG;WHERE LABID='' OR INTERVIEW_DATE=.;
RUN;
*/
***************************************************************************************;
*MAY MOVE THIS CODE UP;
DATA my_key7;
set raw.merged1;
keep rrid key7;
run;

proc sort data=my_key7 nodupkey; by rrid; run;

DATA CAB_ALL; SET CAB.all_cabparticipants; DROP VISIT;
IF LABID= 'N/A' THEN DELETE; 
/*
IF RRID = 'LD0028' AND SURVDATE = '01APR2014'D THEN DELETE;
IF RRID = 'LD0028' THEN 
	DO;
		ADMTIMES = 1; 
		RRIDTIMES = 'LD002801';
	END;
*/
PROC SORT OUT=UCABS NODUPKEY; BY LABID;
PROC SORT DATA=CAB_ALL; BY LABID;
RUN; *3 REMOVED;
/*
PROC PRINT DATA=CAB_ALL; WHERE LABID='10Y0067'; RUN;
*/
/*
PROC FREQ DATA=CAB_ALL; *WHERE LABID = 'LD4028'; *WHERE TOWN = 3; 
	TABLE RRID*survdate / LIST MISSING; RUN;
*/

DATA DUPES0; SET CAB_ALL; 
IF FIRST.LABID THEN COUNT = 0; 
BY LABID; 
COUNT +1;
RUN;
/*PROC PRINT; WHERE COUNT > 1; RUN;*//*Removed by ES 6/22/2017*/
***************************************************************************************;
/*    CHANGE to MERGE                            IZ 10/11/2017
PROC SQL noprint;	/*Added noprint ES 6/22/2017
	CREATE TABLE MCAB1 AS
	SELECT * FROM A4 AS A
	LEFT JOIN CAB_ALL AS C
	ON A.LABID = C.LABID;
QUIT;
***************************************************************************************;
*/

proc sort data = A4; by labid; run;

data MCAB1_IZ;
merge A4 (in = a) CAB_ALL (in = b);
by labid;
if a;
run;

data MCAB1;
set MCAB1_IZ;
run;


PROC CONTENTS NOPRINT
/*
DATA=FINAL.CCHC_BASELINE (KEEP=BIRTCTRY FBIRST FBIRCTRY MBIRST MBIRCTRY FFBIRCTR
					FMBIRST FMBIRCTR MFBIRST MFBIRCTR MMBIRST MMBIRCTR SCHCTRY)OUT=CTS;
*/
DATA=FINAL.CCHC (KEEP=BIRTCTRY FBIRST FBIRCTRY MBIRST MBIRCTRY FFBIRCTR
				FMBIRST FMBIRCTR MFBIRST MFBIRCTR MMBIRST MMBIRCTR SCHCTRY)OUT=CTS;
RUN;

DATA MCAB2; SET MCAB1; 
DROP 
	BIRTCTRY 	FBIRST 		FBIRCTRY 	MBIRST 		MBIRCTRY 
	FFBIRCTR 	FMBIRST		FMBIRCTR 	MFBIRST 	MFBIRCTR
	MMBIRST 	MMBIRCTR 	SCHCTRY		;
RUN;

PROC SQL noprint;	/*Added noprint ES 6/22/2017*/
	CREATE TABLE MCAB3 AS
	SELECT * FROM MCAB2 AS M
	LEFT JOIN RAW.idd_constants AS X
	ON M.RRID = X.RRID
	ORDER BY RRID, VISIT;

QUIT;

/*proc sort data = MCAB3; by rrid visit; run;*/
/*proc freq data=mcab3; table COMMENTS ; run;

proc freq data=mcab3; 
table ARMW*BP_EXAMDATE*BPWELCH_EXAMDATE*BPWELCH_FORM*BPRANDOMZERO_FORM / LIST MISSING;
run;
*/ /*Removed ES 6/22/2017*/

/**********************************************************************************************************/
/************************************ Merge Pediatric CAB *************************************************/
/**********************************************************************************************************/

data cabspd_at;
set BRO.cabspd_a;
if rrid = "BD1051" then delete;
if rrid = "BD2023" then delete;
if rrid = "BD3336" then delete;
proc sort; by rrid visit;
run;

data cesdpd_at;
set BRO.cesdpd_a;
if rrid = "BD1051" then delete;
if rrid = "BD2023" then delete;
if rrid = "BD3336" then delete;
proc sort; by rrid visit;
run;

data saspd_at;
set BRO.saspd_a;
if rrid = "BD1051" then delete;
if rrid = "BD2023" then delete;
if rrid = "BD3336" then delete;
proc sort; by rrid visit;
run;

data hrlcabspd_at;
set hrlpdcab.cabspd_a;
run;

data hrlcesdpd_at;
set hrlpdcab.cesdpd_a;
run;

data hrlsaspd_at;
set hrlpdcab.saspd_a;
run;

data lrdcabspd_at;
set lrdpdcab.cabspd_a;
run;

data lrdcesdpd_at;
set lrdpdcab.cesdpd_a;
run;

data lrdsaspd_at;
set lrdpdcab.saspd_a;
run;

data pdcab1;
merge cabspd_at (in=a) cesdpd_at (in=b);
by rrid visit;
run;

data final_pdcab;
merge pdcab1 (in=a) saspd_at (in=b);
by rrid visit;
if rrid = "" then delete;
drop study;
proc sort; by rrid visit;
run;

data hrlpdcab1;
merge hrlcabspd_at (in=a) hrlcesdpd_at (in=b);
by rrid visit;
run;

data final_hrlpdcab;
merge hrlpdcab1 (in=a) hrlsaspd_at (in=b);
by rrid visit;
if rrid = "" then delete;
drop study;
proc sort; by rrid visit;
run;

data lrdpdcab1;
merge lrdcabspd_at (in=a) lrdcesdpd_at (in=b);
by rrid visit;
run;

data final_lrdpdcab;
merge lrdpdcab1 (in=a) lrdsaspd_at (in=b);
by rrid visit;
if rrid = "" then delete;
drop study;
proc sort; by rrid visit;
run;

data final_cchc1;
merge MCAB3 (in=a) final_pdcab (in=b);
if a;
by rrid visit;
run;

data final_cchc2;
merge final_cchc1 (in=a) final_hrlpdcab (in=b);
if a;
by rrid visit;
run;

data final_cchc2_1;
merge final_cchc2 (in=a) final_lrdpdcab (in=b);
if a;
by rrid visit;
drop key7;
proc sort; by rrid;
run;


/*
data test_key7;
set final_cchc3;
keep rrid key7;
run;
*/

DATA final_cchc3;drop cesd_score;
MERGE final_cchc2_1 (in = a) my_key7 (in = b);
by rrid;
run;


/**********************************************************************************************************/
/********************************* Merge Depression and Anxiety Scores ************************************/
/**********************************************************************************************************/

data final_cchc4;
LENGTH CARDIAC_HISTORY $18.;
set final_cchc3;
rename RWT=RWT_REV1;
rename LVH=LVH_REV1;
rename LA_SIZE=LA_SIZE_REV1;
*DEPRESSION SCORE***********************************************************************************;
X_APPETITE=.;
X_BOTHERED=.;
X_CRYING=.;
X_DEPRESED=.;
X_DISLIKE=.;
X_EFFORT=.;
X_ENJOYED=.;
X_FAILURE=.;
X_FEARFUL=.;
X_GOING=.;
X_GOOD=.;
X_HAPPY=.;
X_HOPEFUL=.;
X_LONELY=.;
X_MIND=.;
X_RESTLESS=.;
X_SAD=.;
X_SHAKESAD=.;
X_TALKED=.;
X_UNFRIEND=.;

*4. I felt I was just as good as other people.;
if		GOOD		=4 THEN X_GOOD	=0;			else if	GOOD		=3 THEN X_GOOD	=1;*REVERSED;
else if	GOOD		=2 THEN X_GOOD	=2;			else if	GOOD		=1 THEN X_GOOD	=3;*REVERSED;

*8. I felt hopeful about the future.;
if		HOPEFUL		=4 THEN X_HOPEFUL	=0;			else if	HOPEFUL		=3 THEN X_HOPEFUL	=1;*REVERSED;
else if	HOPEFUL		=2 THEN X_HOPEFUL	=2;			else if	HOPEFUL		=1 THEN X_HOPEFUL	=3;*REVERSED;

*12. I was happy.;
if		HAPPY		=4 THEN X_HAPPY	=0;			else if	HAPPY		=3 THEN X_HAPPY	=1;*REVERSED;
else if	HAPPY		=2 THEN X_HAPPY	=2;			else if	HAPPY		=1 THEN X_HAPPY	=3;*REVERSED;

*16. I enjoyed life.;
if		ENJOYED		=4 THEN X_ENJOYED	=0;			else if	ENJOYED		=3 THEN X_ENJOYED	=1;*REVERSED;
else if	ENJOYED		=2 THEN X_ENJOYED	=2;			else if	ENJOYED		=1 THEN X_ENJOYED	=3;*REVERSED;

if		APPETITE	=1 THEN X_APPETITE	=0;		else if	APPETITE	=2 THEN X_APPETITE	=1;
else if	APPETITE	=3 THEN X_APPETITE	=2;		else if	APPETITE	=4 THEN X_APPETITE	=3;
if		BOTHERED	=1 THEN X_BOTHERED	=0;		else if	BOTHERED	=2 THEN X_BOTHERED	=1;
else if	BOTHERED	=3 THEN X_BOTHERED	=2;		else if	BOTHERED	=4 THEN X_BOTHERED	=3;
if		CRYING		=1 THEN X_CRYING	=0;			else if	CRYING		=2 THEN X_CRYING	=1;
else if	CRYING		=3 THEN X_CRYING	=2;			else if	CRYING		=4 THEN X_CRYING	=3;
if		DEPRESED	=1 THEN X_DEPRESED	=0;		else if	DEPRESED	=2 THEN X_DEPRESED	=1;
else if	DEPRESED	=3 THEN X_DEPRESED	=2;		else if	DEPRESED	=4 THEN X_DEPRESED	=3;
if		DISLIKE		=1 THEN X_DISLIKE	=0;			else if	DISLIKE		=2 THEN X_DISLIKE	=1;
else if	DISLIKE		=3 THEN X_DISLIKE	=2;			else if	DISLIKE		=4 THEN X_DISLIKE	=3;
if		EFFORT		=1 THEN X_EFFORT	=0;			else if	EFFORT		=2 THEN X_EFFORT	=1;
else if	EFFORT		=3 THEN X_EFFORT	=2;			else if	EFFORT		=4 THEN X_EFFORT	=3;
if		FAILURE		=1 THEN X_FAILURE	=0;			else if	FAILURE		=2 THEN X_FAILURE	=1;
else if	FAILURE		=3 THEN X_FAILURE	=2;			else if	FAILURE		=4 THEN X_FAILURE	=3;
if		FEARFUL		=1 THEN X_FEARFUL	=0;			else if	FEARFUL		=2 THEN X_FEARFUL	=1;
else if	FEARFUL		=3 THEN X_FEARFUL	=2;			else if	FEARFUL		=4 THEN X_FEARFUL	=3;
if		GOING		=1 THEN X_GOING	=0;			else if	GOING		=2 THEN X_GOING	=1;
else if	GOING		=3 THEN X_GOING	=2;			else if	GOING		=4 THEN X_GOING	=3;
if		LONELY		=1 THEN X_LONELY	=0;			else if	LONELY		=2 THEN X_LONELY	=1;
else if	LONELY		=3 THEN X_LONELY	=2;			else if	LONELY		=4 THEN X_LONELY	=3;
if		MIND		=1 THEN X_MIND	=0;			else if	MIND		=2 THEN X_MIND	=1;
else if	MIND		=3 THEN X_MIND	=2;			else if	MIND		=4 THEN X_MIND	=3;
if		RESTLESS	=1 THEN X_RESTLESS	=0;		else if	RESTLESS	=2 THEN X_RESTLESS	=1;
else if	RESTLESS	=3 THEN X_RESTLESS	=2;		else if	RESTLESS	=4 THEN X_RESTLESS	=3;
if		SAD			=1 THEN X_SAD	=0;				else if	SAD			=2 THEN X_SAD	=1;
else if	SAD			=3 THEN X_SAD	=2;				else if	SAD			=4 THEN X_SAD	=3;
if		SHAKESAD	=1 THEN X_SHAKESAD	=0;		else if	SHAKESAD	=2 THEN X_SHAKESAD	=1;
else if	SHAKESAD	=3 THEN X_SHAKESAD	=2;		else if	SHAKESAD	=4 THEN X_SHAKESAD	=3;
if		TALKED		=1 THEN X_TALKED	=0;			else if	TALKED		=2 THEN X_TALKED	=1;
else if	TALKED		=3 THEN X_TALKED	=2;			else if	TALKED		=4 THEN X_TALKED	=3;
if		UNFRIEND	=1 THEN X_UNFRIEND	=0;		else if	UNFRIEND	=2 THEN X_UNFRIEND	=1;
else if	UNFRIEND	=3 THEN X_UNFRIEND	=2;		else if	UNFRIEND	=4 THEN X_UNFRIEND	=3;

cesd_score=.;
cesd_score=
X_APPETITE + X_BOTHERED + X_CRYING + X_DEPRESED + X_DISLIKE + X_EFFORT + X_ENJOYED + X_FAILURE + X_FEARFUL +
X_GOING + X_GOOD + X_HAPPY + X_HOPEFUL + X_LONELY	+ X_MIND + X_RESTLESS + X_SAD + X_SHAKESAD + X_TALKED +
X_UNFRIEND;

*ANXIETY SCORE********************************************************************************************;
X_ALLRIGHT = .;
X_CALM = .;
X_BREATHE = .;
X_ASLEEP = .;
X_HANDS = .;

IF ALLRIGHT=1 THEN X_ALLRIGHT=4;	IF ALLRIGHT=2 THEN X_ALLRIGHT=3;	
IF ALLRIGHT=3 THEN X_ALLRIGHT=2;	IF ALLRIGHT=4 THEN X_ALLRIGHT=1;

IF CALM=1 THEN X_CALM=4;	IF CALM=2 THEN X_CALM=3;	
IF CALM=3 THEN X_CALM=2;	IF CALM=4 THEN X_CALM=1;

IF ASLEEP=1 THEN X_ASLEEP=4;	IF ASLEEP=2 THEN X_ASLEEP=3;	
IF ASLEEP=3 THEN X_ASLEEP=2;	IF ASLEEP=4 THEN X_ASLEEP=1;

IF HANDS=1 THEN X_HANDS=4;	IF HANDS=2 THEN X_HANDS=3;	
IF HANDS=3 THEN X_HANDS=2;	IF HANDS=4 THEN X_HANDS=1;

IF BREATHE=1 THEN X_BREATHE=4;	IF BREATHE=2 THEN X_BREATHE=3;	
IF BREATHE=3 THEN X_BREATHE=2;	IF BREATHE=4 THEN X_BREATHE=1;

ANXIETY_SCORE=.;

ANXIETY_SCORE=
NERVOUS	+ AFRAID + UPSET + FALLING + X_ALLRIGHT + TREMBLE + HEADACHE + WEAK + X_CALM + HEART + DIZZY + FAINTING +
X_BREATHE + NUMBNESS + STOMACH + BLADDER + X_HANDS + FACE + X_ASLEEP + NIGHTMARE;
drop X_APPETITE X_BOTHERED X_CRYING X_DEPRESED X_DISLIKE X_EFFORT X_ENJOYED X_FAILURE X_FEARFUL X_GOING X_GOOD
X_HAPPY X_HOPEFUL X_LONELY X_MIND X_RESTLESS X_SAD X_SHAKESAD X_TALKED X_UNFRIEND X_ALLRIGHT X_CALM X_BREATHE
X_ASLEEP X_HANDS CBC_DIFF;
proc sort; by labid;
run;

/**********************************************************************************************************/
/*********************************** Merge Other ECHO Data from Dr. Laing *********************************/
/**********************************************************************************************************/
/*
data my_echo_data;
set ECHO_L.final_echo_results;
proc sort; by labid;
run;
*/
/*
proc contents data=my_echo_data; run;
*/
/*
data final_cchc45;
merge final_cchc4 (in=a) my_echo_data (in=b);
by labid;
if a;
proc sort; by labid;
run;
*/
/*
PROC SQL noprint; 
	CREATE TABLE final_cchc_iz AS
	SELECT * FROM final_cchc4 AS CS
	JOIN my_echo_data AS echodata
		ON CS.LABID = echodata.LABID;
QUIT;
*/

/**********************************************************************************************************/
/**************************************** Merge DEXA Data *************************************************/
/**********************************************************************************************************/
data my_dexa_data;
set dxa.final_dexa;
proc sort; by labid;
run;

%CheckDataset(my_dexa_data,RawRRIDLABID)

data  final_cchc4_1;
merge final_cchc4 (in=a) my_dexa_data (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/**************************************** Merge DEXA Comments Data *************************************************/
/**********************************************************************************************************/
data my_dexa_cmt;
set DXA_CMT.final_dxa_comments;
proc sort; by labid;
run;

%CheckDataset(my_dexa_cmt,RawRRIDLABID)

data final_cchc5;
merge final_cchc4_1 (in=a) my_dexa_cmt (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/**************************************** Merge PAX GENE Data *************************************************/
/**********************************************************************************************************/
data my_pax_data;
set PAXGENE.final_pax;
proc sort; by labid;
run;

%CheckDataset(my_pax_data,RawRRIDLABID)

data final_cchc5_1;
merge final_cchc5 (in=a) my_pax_data (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add TBS_DXA Data *********************************************/
/**********************************************************************************************************/

data final_tbs_dxa;
set dxa.final_tbs_dxa;
proc sort; by labid;
run;

%CheckDataset(final_tbs_dxa,RawRRIDLABID)

data final_cchc5_2;
merge final_cchc5_1 (in=a) final_tbs_dxa (in=b);
by labid;
if a;
if has_tbs = "" then has_tbs = 0;
proc sort; by labid;
run;

/**********************************************************************************************************/
/****************************************** Merge PAD ABI *************************************************/
/**********************************************************************************************************/
data my_pad_abi;
set padabi.final_pad_abi;
if RRID = "Hadeco" then delete;
proc sort; by labid;
run;

%CheckDataset(my_pad_abi,RawRRIDLABID)

data final_cchc6;
merge final_cchc5_2 (in=a) my_pad_abi (in=b);
by labid;
if a;
proc sort out = particip; by rrid; run;

/**********************************************************************************************************/
/************************************ Calculate Metabolic Syndrome ****************************************/
/**********************************************************************************************************/

data indicationlist;
set medicp.indicationlist;
run;

proc sort data=medicp.participants_medications out=medpatient;
by medication_name; run;

proc sort data=medicp.medicationlist out=medication ;
by medication_name; run;

data patmed_ch_hp;
merge medpatient (in=a) medication (in=b);
by medication_name;
if a=b;
if code= '     ' then delete ;
drop lineno dbnam mednameclean;
run;

data patmed_somemed;

set patmed_ch_hp;
where code in ('MEDHP'  'MEDCHOL' 'MEDCHOL2' 'MEDCHOL3' 'MEDCHOL4' 'STATIN');
/*
set medpatient;
where code in ('MEDHP'  'MEDCHOL' 'MEDCHOL2' 'MEDCHOL3' 'MEDCHOL4' 'STATIN');
*/
proc sort; by rrid code;
run;

data statin_f;
set medicp.medicationlist;
STAT= INDEX(MEDICATION_NAME,'STATIN') ;
IF STAT > 0 THEN DO;
                HDLMED=1;
				OUTPUT;
			END;
RUN;

proc transpose data=patmed_somemed out=t_patmed;
var code;
by rrid;
run;

data chol_HP_med ;
set t_patmed;
cholmed=0;
hpmed=0;
array medtys (*) col: ;
do i=1  to dim(medtys);
   if medtys(i) in ('MEDHP' ) then hpmed=1;
   else if medtys(i) in ('MEDCHOL' 'MEDCHOL2' 'MEDCHOL3' 'MEDCHOL4' 'STATIN') then cholmed=1;
end;
drop _name_  _label_ i ;
proc sort; by rrid;
run;

data pat_treatment;
merge  particip (in=a) chol_hp_med (in=b);
by rrid;
if a;
run;

/*   data pat (keep = rrid bmi1 MFBG MCORRSYS MCORRDIA TRIG GENDER HDLC WAISTM AtpIII_count mets_atpiii idf_count mets_idf
			cholmed hpmed);     */


data final_cchc7; 
set pat_treatment;
/**************************************** Calculate METS_ATPIII *******************************************/
AtpIII_count=0;
METS_ATPIII=0;
if Mfbg >= 100 then AtpIII_count + 1;
if mcorrsys >= 130 or mcorrdia >=85 then AtpIII_count +1;
if trig >=150 then AtpIII_count + 1;
if gender=1 then do;
   				if 0 <= hdlc < 40 then AtpIII_count + 1;
   				if waistM > 102 then AtpIII_count + 1 ;
            end;
if gender= 2 then do;
                if 0 <= hdlc < 50 then AtpIII_count + 1;
				if waistM > 88 then AtpIII_count + 1;
			  END;

If AtpIII_count >= 3 then METS_ATPIII = 1;
/****************************************** Calculate METS_IDF ********************************************/
IDF_COUNT=0;
METS_IDF=0;
central_obesity=0;
IF bmi1 > 30 or (gender=1 and waistM > 90) or (gender=2 and waistM > 80) then central_obesity=1; *defining Central Obisity;

IF TRIG >=150 or cholmed=1 THEN IDF_COUNT + 1;
if (gender=1 and 0 <= hdlc < 40) or (gender= 2 and 0 <= hdlc < 50) then IDF_count + 1;
if mcorrsys >= 130 or mcorrdia >=85 or hpmed=1 then IDF_count +1;
if mfbg > 100 then idf_count + 1;

if idf_count >= 2 and central_obesity=1 then Mets_IDF =1;

*if substr(rrid,1,2)='BD' then output;


label 
METS_ATPIII= 'National Cholesterol Education Program Adult Treatment Panel (ATP III) Metabolic Syndrome Definition'
METS_IDF = 'International Diabetes Federation (IDF) Metabolic Syndrome World-Wide Definition';
drop idf_count atpIII_count hpmed cholmed central_obesity;
format HOMA_IR 8.6;
proc sort; by key7;
RUN;

/**********************************************************************************************************/
/***************************************** ADDING PER CAPITA INCOME ***************************************/
/**********************************************************************************************************/

data final_pci;
set final.final_pci;
proc sort; by key7;
run;


data final_cchc8;
merge final_cchc7 (in=a) final_pci (in=b);
by key7;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/************************************* ADDING EXERCISE and NUTRITION **************************************/
/**********************************************************************************************************/

data final_ean_data;
set final.final_ean_data;
proc sort; by labid;
run;

data final_cchc9;
merge final_cchc8 (in=a) final_ean_data (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************** ADDING LIVER DATA *******************************************/
/**********************************************************************************************************/
data final_liver_results;
set liver.final_liver_results;
proc sort; by labid;
run;

%CheckDataset(final_liver_results,RawRRIDLABID)

data final_cchc9_1;
merge final_cchc9 (in=a) final_liver_results (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************** ADDING ELASTOGRAPHY DATA ***************************************/
/**********************************************************************************************************/
data final_elasto_data;
set eusfnl.final_elasto;
proc sort; by labid;
run;

%CheckDataset(final_elasto_data,RawRRIDLABID)

data final_cchc10;
merge final_cchc9_1 (in=a) final_elasto_data (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************** ADDING FIBROSCAN DATA ***************************************/
/**********************************************************************************************************/
data final_te;
set FIBRSCN.final_te;
proc sort; by labid;
run;

%CheckDataset(final_te,RawRRIDLABID)

data final_cchc105;
merge final_cchc10 (in=a) final_te (in=b);
by labid;
if a;
proc sort; by labid;
run;


/**********************************************************************************************************/
/******************************************** Calculate FAST DATA ***************************************/
/**********************************************************************************************************/
data fast_cal;drop E X;
set final_cchc105;

if GOT in(.) or TE_KPA in (.,0) or TE_CAP_med in (.,0) then FAST_Score=.;
else do;
X=(-1.65+1.07*log(TE_KPA)+0.0000000266*(TE_CAP_med**3)-63.3*(GOT**-1));
E=exp(1.0);

FAST_Score=(E**X)/(1+(E**X));
end;
label FAST_Score="FibroScan-aspartate aminotransferase (FAST) score. Formula used={exp (–1.65 + 1.07 × ln (LSM) + 2.66 × 10–8 × CAP3 – 63.3 × AST–1)}/{1 + exp (–1.65 + 1.07 × ln (LSM) + 2.66 × 10–8 × CAP3 – 63.3 × AST–1)}";
proc sort; by labid;
run;

/**********************************************************************************************************/
/**************************************** ADDING BONE TURNOVER DATA ***************************************/
/**********************************************************************************************************/

data final_bone_turnover;
set ger_bt.final_bone_turnover;
proc sort; by labid;
run;

%CheckDataset(final_bone_turnover,RawRRIDLABID)

data final_cchc11;
merge fast_cal (in=a) final_bone_turnover (in=b);
by labid;
if a;
proc sort; by rrid visit;
run;

/**********************************************************************************************************/
/***************************************** FINALIZE DATABASE **********************************************/
/**********************************************************************************************************/
/*
data my_dat2;
set final_cchc11;
*format urindat date9.;
*format myurindat date9.;
*IF URINDAT/86400 < 1 THEN MYURINDAT = URINDAT;
*IF URINDAT/86400 > 1 THEN MYURINDAT = URINDAT/86400;
IF URINDAT/86400 > 1 THEN URINDAT = URINDAT/86400;
keep rrid labid visit study urindat;
run;
*/

*ADDING 'CONSTANT' VARIABLES BACK IN - PS 5.11.15;
%LET ORDER = BDVISIT RRID VISIT STUDY LABID INTERVIEW_DATE;
DATA final_cchc12 /*FINAL.CCHC_BASELINE FINAL.PEDIATRIC FINAL.BD2000S*/; RETAIN &ORDER; SET final_cchc11; 

APRI = .;
AST_ALT_RATIO = .;
BARD = .;
BARD_01 = .;
BARD_02 = .;
BARD_C = .;
DM_NF = .;
FIB4_U = .;
FIB4_C = .;
FIB4_C2 =.;
NAF_U = .;
NAF_C = .;
NAF_C2 = .;
N_LF = .;

IF ADA2010_DM = 0 AND IFG < 110 THEN DM_NF = 0.;
IF ADA2010_DM = 1 OR IFG >= 110 THEN DM_NF = 1;
IF BP_EXAMDATE = . THEN BP_EXAMDATE = BPWELCH_EXAMDATE;
IF HAS_PAD = . THEN HAS_PAD = 0;
IF HAS_DXA = . THEN HAS_DXA = 0;
IF DEAD = . THEN DEAD = 0;

LABEL DM_NF = "Diabetic or Impaired Fasting Glucose";

/************************************************ Calculate APRI ******************************************/

IF PLT NE . AND PLT NE 0 AND GOT NE . THEN
DO;
    if GENDER = 1 then APRI = ((GOT/37)*100)/(PLT);
    if GENDER = 2 then APRI = ((GOT/31)*100)/(PLT);
    if APRI <= 0.504 then APRI_C1 = 0;
    if 0.51 <= APRI <=1.50 then APRI_C1 = 1;
    if 1.51 <= APRI then APRI_C1 = 2;
    if APRI_C1 = 0 then APRI_C2 = 0;
    if APRI_C1 > 0 then APRI_C2 = 1;
END;

LABEL APRI = "AST to Platelet Ratio Index (Calculated)";
LABEL APRI_C1 = "Categorical APRI Score";
LABEL APRI_C2 = "Categorical APRI_C1 Score";

/************************************************ Calculate BARD ******************************************/

IF BMI < 28 THEN BARD_01 = 0;
IF BMI >= 28 THEN BARD_01 = 1;

IF GPT NE . AND GPT NE 0 AND GOT NE . AND GOT NE 0 THEN
DO;
    AST_ALT_RATIO = GOT/GPT;
    if AST_ALT_RATIO < 0.8 THEN BARD_02 = 0;
    if AST_ALT_RATIO >= 0.8 THEN BARD_02 = 2;
    
	BARD = BARD_01 + BARD_02 + DM_NF;
	IF BARD <= 1 THEN BARD_C = 0;
	IF BARD >= 2 THEN BARD_C = 1;
END;

LABEL BARD = "BARD Score (Calculated)";
LABEL BARD_01 = "Categorical BMI";
LABEL BARD_02 = "Categorical AST_ALT_RATIO";

/********************************************* Calculate Fibrosis-4 (FIB-4) Score ******************************************/

IF PLT NE . AND PLT NE 0 AND GPT NE . AND GPT NE 0  THEN FIB4_U = (age_at_visit * got)/(plt*sqrt(gpt));

IF FIB4_U <= 1.30 THEN FIB4_C = 0;
IF 1.30 < FIB4_U <= 2.66 THEN FIB4_C = 1;
IF FIB4_U > 2.66 THEN FIB4_C = 2;

IF FIB4_C < 1 THEN FIB4_C2 = 0;
IF FIB4_C >= 1 THEN FIB4_C2 = 1;

LABEL FIB4_U = "Calculated Fibrosis-4 (FIB-4) Score: ((age_at_visit * got)/(plt*sqrt(gpt)))";
LABEL FIB4_C = "Categorical FIB-4 Score";
LABEL FIB4_C2 = "Categorical FIB4_C Score";

/************************************************ Calculate NAFLD Liver Fat Score ******************************************/

N_LF = -2.89 + (1.18*METS_ATPIII) + (0.45*2*ADA2010_DM) + (0.15*INS) + (0.04*GOT) - (0.94*AST_ALT_RATIO);

IF N_LF < -0.640 THEN N_LFC = 0;
IF N_LF >= -0.640 THEN N_LFC = 1;

LABEL N_LF = "Calculated NAFLD Liver Fat Score: (-2.89 + (1.18*METS_ATPIII) + (0.45*2*ADA2010_DM) + (0.15*INS) + (0.04*GOT) - (0.94*AST_ALT_RATIO))";
LABEL N_LFC = "Categorical N_LF Score";
/************************************************ Calculate NAFLD Fibrosis Score ******************************************/

NAF_U = -1.675 + (0.037*age_at_visit) + (0.094*bmi1) + (1.13*DM_NF) + (0.99*AST_ALT_RATIO) - (0.013*PLT) - (0.66*LALB);

IF NAF_U <= -1.454 THEN NAF_C = 0;
IF -1.454 < NAF_U <= 0.676 THEN NAF_C = 1;
IF NAF_U > 0.676 THEN NAF_C = 2;

IF NAF_C = 0 THEN NAF_C2 = 0;
IF NAF_C >= 1 THEN NAF_C2 = 1;

LABEL NAF_U = "Calculated NAFLD Fibrosis Score: (-1.675 + (0.037*age_at_visit) + (0.094*bmi1) + (1.13*DM_NF) + (0.99*AST_ALT_RATIO) - (0.013*PLT) - (0.66*LALB))";
LABEL NAF_C = "Categorical NAF_U Score";
LABEL NAF_C2 = "Categorical NAF_C Score";

/**********************************************************************************************************/
/***************************** Calculate Exercise and Nutrition Variables *********************************/
/**********************************************************************************************************/

PORFVGTOT = SUM(PORFRUIT,PORVEGS);

/*  CUPPOR NOT DONE IN CCHC * IH 04/21/17

TOTFVG = SUM(CUPPOR,PORFVGTOT);

*/

TOTFVG = PORFVGTOT;

MEETFVG = .;
IF . < TOTFVG < 5 	THEN MEETFVG = 0;
ELSE IF TOTFVG >= 5 THEN MEETFVG = 1;

TOTFVGCAT = .;
IF TOTFVG IN (0)   		THEN TOTFVGCAT = 1; *1=0 portions of fruit & veg daily;
ELSE IF TOTFVG IN (1 2) THEN TOTFVGCAT = 2; *2=1-2 portions of fruit & veg daily;
ELSE IF TOTFVG IN (3 4) THEN TOTFVGCAT = 3; *3=3-4 portions of fruit & veg daily;
ELSE IF TOTFVG >= 5	   	THEN TOTFVGCAT = 4; *4=5+ portions of fruit & veg daily;

******************************************************************************************************;
FRUITVEG = SUM(ORANGEVG,SALADVEG,OTHERVEG,FRUITSYS);
******************************************************************************************************;

LANGACC = MEAN(LANGFRND,LANGHOME,LANGREAD,LANGTHNK);
LANG_CAT = .;

IF 0	< LANGACC <= 2.5		THEN LANG_CAT = 1;
ELSE IF 2.5	< LANGACC <= 3.5	THEN LANG_CAT = 2;
ELSE IF 3.5	< LANGACC <= 5		THEN LANG_CAT = 3;

******************************************************************************************************;

IF STRNX = 0 THEN STRNMIN = 0;
IF MODX = 0 THEN MODMIN = 0;
IF MILDX = 0 THEN MILDMIN = 0;

** creating the met variables based on the salis scale or some other scale;
MILDMETV3 = 3.0*(MILDMIN*MILDX); 
MODMETV3 = 5.0*(MODMIN*MODX); 
STRNMET = 9.0*(STRNMIN*STRNX); 
TOTMETV3 = MODMETV3+STRNMET; 

ALLMET = SUM(TOTMETM,TOTMETV3);

MEETPA = .;
IF ALLMET >= 600 			THEN MEETPA = 1;
ELSE IF . < ALLMET < 600 	THEN MEETPA = 0; 

PACAT = '                ';
IF ALLMET >=1500 			THEN PACAT = 'HIGH     ';
ELSE IF 1500 > ALLMET >=600 THEN PACAT = 'MODERATE ';
ELSE IF 600 > ALLMET >0 	THEN PACAT = 'LOW      ';
ELSE IF ALLMET = 0 			THEN PACAT = 'SEDENTARY';

*******************************************************************************************************;
LABEL 
ALLMET="Total MET adjusted minutes of moderate and vigorous/strenuous activity."
FRUITVEG="Sum of ORANGEVG, SALADVEG, OTHERVEG, FRUITSYS items"
LANG_CAT="Language acculturation categories"
LANGACC="Language acculturation score"
MEETFVG="Meets recommended guidelines of 5+ portions of fruit & vegetables a day?" 
MEETPA="Meet physical activity guidelines of 150 moderate and vigorous minutes per week." 
MILDMETV3="MET adjusted minutes of mild activity"
MODMETV3="MET adjusted minutes of moderate activity" 
PACAT="Four-category variable based on MET adjusted minutes of moderate/vigorous physical activity"
PORFVGTOT="Total portions of fruits and vegetables." 
STRNMET="MET adjusted minutes of strenuous activity"
TOTFVG="Total portions of fruits and vegetables." 
TOTFVGCAT="Categorized number of fruit & veg portions consumed daily."
TOTMETV3="Combined MET adjusted minutes of moderate and strenuous activity" 
;

IF URINDAT/86400 > 1 THEN URINDAT = URINDAT/86400;

DROP IMPACTDATA 
/*BPWELCH_EXAMDATE*/ 
EXAMDATE
BIRTCITY
FBIRCITY
FFBIRCIT
FMBIRCIT
MBIRCITY
MFBIRCIT
MMBIRCIT
SCHCITY
SPSCHCIT
PLTCNT /*SHOULD BE ALL MISSING OR REMOVED IN ACCESS*/
COMMENTS /*SHOULD BE RENAMED (PS 6.10.15)*/
/*DATE_FBG3*/ /*DROPPING (7.7.15 PS)*/
/*FBG3*/ /*DROPPING (7.7.15 PS)*/
; 
LABEL
AST_ALT_RATIO="AST/ALT RATIO"
BIRTCTRY='Q9 WHAT IS YOUR BIRTH COUNTRY?'
FBIRCTRY='Q11 WHAT IS YOUR FATHER`S BIRTH COUNTRY?'
FBIRST='Q11 WHAT IS YOUR FATHER`S BIRTH STATE?'
FFBIRCTR='Q14 WHAT IS YOUR FATHER`S FATHER`S BIRTH COUNTRY?'
FMBIRCTR='Q14 WHAT IS YOUR FATHER`S MOTHER`S BIRTH COUNTRY?'
FMBIRST='Q14 WHAT IS YOUR FATHER`S MOTHER`S BIRTH STATE?'
MBIRCTRY='Q13 WHAT IS YOUR MOTHER`S BIRTH COUNTRY?'
MBIRST='Q13 WHAT IS YOUR MOTHER`S BIRTH STATE?'
MFBIRCTR='Q14 WHAT IS YOUR MOTHER`S FATHER`S BIRTH COUNTRY?'
MFBIRST='Q14 WHAT IS YOUR MOTHER`S FATHER`S BIRTH STATE?'
MMBIRCTR='Q14 WHAT IS YOUR MOTHER`S MOTHER`S BIRTH COUNTRY?'
MMBIRST='Q14 WHAT IS YOUR MOTHER`S MOTHER`S BIRTH STATE?'
SCHCTRY='Q18 IN WHAT COUNTRY DID YOU SPEND THE MAJORITY OF YOUR SCHOOL YEARS?'
ADMTIMES     ="How many times was the survey administered?"
AGE_ER       ="5A. Since your last visit have you been in the ER: AGE"
ANNUAL       ="DRS Annual Visit?"
ARRTIME      ="WHAT IS THE ARRIVAL TIME?"
CAB_SURVEY   ="Does the participant have the CAB Survey?"
CITY_ER      ="5A. Since your last visit have you been in the ER: In what city?"
DATE_ALK     ="ALK date"
DATE_ALP     ="ALP date"
DATE_BUN     ="BUN date"
DATE_CALC    ="CALC date"
DATE_CHL     ="CHL date"
DATE_CHLR    ="CHLR date"
DATE_CHOL1   ="CHOL1 date"
DATE_CO2     ="CO2 date"
DATE_CREA    ="CREA date"
DATE_CRP     ="CRP date"
DATE_DBIL    ="DBIL date"
DATE_DLDL    ="DLDL date"
DATE_GFR     ="GFR date"
DATE_GHB     ="GHB date"
DATE_GOT     ="GOT date"
DATE_GPT     ="GPT date"
DATE_HDLC    ="HDLC date"
DATE_LALB    ="LALB date"
DATE_LDLCALC="LDLCALC date"
DATE_MCGLUC="MCGLUC date"
DATE_POT="POT date"
DATE_SOD="SOD date"
DATE_TBIL="TBIL date"
DATE_TP="TP date"
DATE_TRIG="TRIG date"
DATE_URIC="URIC date"
DEPTIME="WHAT IS THEIR DEPARTURE TIME?"
DIABFORMCOMM="Diabetes Form Comments: Q23 - Any Other Comments"
DIAG_ER="Since your last visit have you been in the ER: If yes, what was the diagnostic?"
EAT_TIME="HHRC Q3 WHAT IS THE TIME WHEN YOU LAST ATE OR DRANK?"
EDTA4="EDTA #4 (purple top) -  Vacutainer filled?"
EMERGEN="Since your last visit have you been in the emergency room?"
FASTTIME="WHAT IS THE RESCHEDULED EXAM TIME?"
FBGTIME="WHAT IS THE TIME OF THE 2ND FBG SCHEDULED?"
GHB_LOCATION="Where was the HBA1C test run?"
LANGUAG1="CAB survey was administered in this language"
MDCITY="Personal Physician: City"
HIPM="Mean Hip (hip,hip2)"
NAME_ER="Since your last visit have you been in the ER: Hospital Name"
NAFLD_FS="NAFLD FIBROSIS SCORE"
OTHECOMP="Q21 WERE YOU ADMITTED TO A HOSPITAL FOR OTHER REASONS?"
OTHESPEC="Q21 SPECIFY WHAT OTHER REASONS YOU WERE ADMITTED TO A HOSPITAL?"
REFUSE="The CAB survey was refused/incomplete - Comments"
RRIDTIMES="CAB survey (RRID + ADMTIMES)"
SCHTIME="HHRC Q4 WHAT IS THE RESCHEDULED TIME?"
STATUS="Status of CAB survey"
SURVDATE="Date the CAB survey was administered"
TODAYTIM="HHRC Q2 WHAT IS TODAY`S TIME?"
YEAR_ER="Since your last visit have you been in the ER: Year?"
;
FORMAT
CAB_SURVEY     
DRS_SURVEY     
FYR_SURVEY     
OB_SURVEY      
PACE_SURVEY    DICH.
ARRTIME        
DEPTIME        
EAT_TIME       
FASTTIME       
FBGTIME        
SCHTIME        
TODAYTIM       TIME.
FASTDATE       
URINDAT        DATE9.;

FORMAT
MEETPA      MEETFVG     X10789X. 
TOTFVGCAT               TOTFVGX.
LANG_CAT                LANG_CAX.
EAN_DATE				DATE9.;


IF IMPACTDATA = '' THEN OUTPUT final_cchc12;
*ELSE OUTPUT FINAL.BD2000S;
proc sort nodupkey; by rrid visit;
proc sort; by labid;
RUN;

/**********************************************************************************************************/
/******************************************* Add HbSAG Data ***********************************************/
/**********************************************************************************************************/

data final_hbsag1;
set HBV.FINAL_HBSAG;
proc sort; by labid;
run;

%CheckDataset(final_hbsag1,RawRRIDLABID)

data final_cchc13;
merge final_cchc12 (in=a) final_hbsag1 (in=b);
if a;
by labid;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Retina Data ***********************************************/
/**********************************************************************************************************/

data final_retina;
set RETINA.FINAL_RETINA;
proc sort; by labid;
run;

%CheckDataset(final_retina,RawRRIDLABID)

data final_cchc13_1;
merge final_cchc13 (in=a) final_retina (in=b);
by labid;
if a;
if has_retina_scan = "" then has_retina_scan = 0;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add ISCHEMIA Data ***********************************************/
/**********************************************************************************************************/

data final_ISCHEMIA;
set ISCHEMIA.worksheet;
proc sort; by labid;
run;

%CheckDataset(final_ISCHEMIA,RawRRIDLABID)

data final_cchc13_1_1;
merge final_cchc13_1 (in=a) final_ISCHEMIA (in=b);
by labid;
if a;
proc sort; by labid;
run;


/**********************************************************************************************************/
/******************************************* Add Chik data********************************************/
/**********************************************************************************************************/
data FINAL_Chik;
set TAKEDA.chik_final;
proc sort; by labid;
run;


data final_cchc13_2;
merge final_cchc13_1_1 (in=a) FINAL_Chik (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Dengue Abcam data********************************************/
/**********************************************************************************************************/
data FINAL_dengue_abcam;
set TAKEDA.dengue_abcam_final;
proc sort; by labid;
run;


data final_cchc13_3;
merge final_cchc13_2 (in=a) FINAL_dengue_abcam (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Dengue Euro data********************************************/
/**********************************************************************************************************/
data FINAL_dengue_euro;
set TAKEDA.dengue_euro_final;
proc sort; by labid;
run;

data final_cchc13_4;
merge final_cchc13_3 (in=a) FINAL_dengue_euro (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Zika data********************************************/
/**********************************************************************************************************/
data FINAL_zika;
set TAKEDA.zika_final;
proc sort; by labid;
run;

data final_cchc13_5;
merge final_cchc13_4 (in=a) FINAL_zika (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add NTD data********************************************/
/**********************************************************************************************************/
data FINAL_NTD;
set NTD.final_ntd;
proc sort; by labid;
run;

data final_cchc13_6;
merge final_cchc13_5 (in=a) FINAL_NTD (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Covid data********************************************/
/**********************************************************************************************************/
/*data FINAL_COVID;
set COVID.final_cov;
proc sort; by labid;
run;

%CheckDataset(FINAL_COVID,RawRRIDLABID)

data final_cchc13_6_01;
merge final_cchc13_6 (in=a) FINAL_COVID (in=b);
by labid;
if a;
proc sort; by labid;
run;
*/

/**********************************************************************************************************/
/******************************************* Add Covid ncp data********************************************/
/**********************************************************************************************************/
data FINAL_COVID_NCP;
set COV_NCP.final_cov_ncp;
proc sort; by labid;
run;

%CheckDataset(FINAL_COVID_NCP,RawRRIDLABID)

data final_cchc13_6_01_1;
merge final_cchc13_6 (in=a) FINAL_COVID_NCP (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add C-peptide********************************************/
/**********************************************************************************************************/
data FINAL_CPEPTIED;
set CPEPT.final_cpeptide;
proc sort; by labid;
run;

%CheckDataset(FINAL_CPEPTIED,RawRRIDLABID)

data final_cchc13_6_01_2;
merge final_cchc13_6_01_1 (in=a) FINAL_CPEPTIED (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Bile data********************************************/
/**********************************************************************************************************/
data FINAL_Bile;
set BILE.final_bile;
proc sort; by labid;
run;

%CheckDataset(FINAL_Bile,RawRRIDLABID)

data final_cchc13_6_02;
merge final_cchc13_6_01_2 (in=a) FINAL_Bile (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Sleep data********************************************/
/**********************************************************************************************************/
data FINAL_Sleep;
set SLEEP.final_sleep;
proc sort; by labid;
run;

%CheckDataset(FINAL_Sleep,RawRRIDLABID)

data final_cchc13_6_03;
merge final_cchc13_6_02 (in=a) FINAL_Sleep (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Covid Flag data********************************************/
/**********************************************************************************************************/
data FINAL_COVID_FLAG;
set COV_FG.final_covid_study;
proc sort; by labid;
run;

%CheckDataset(FINAL_COVID_FLAG,RawRRIDLABID)

data final_cchc13_6_04;
merge final_cchc13_6_03 (in=a) FINAL_COVID_FLAG (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Dis data********************************************/
/**********************************************************************************************************/
data FINAL_DisDsf;
set ISCHEMIA.disdsf;
proc sort; by labid;
run;

%CheckDataset(FINAL_DisDsf,RawRRIDLABID)

data final_cchc13_6_1;
merge final_cchc13_6_04 (in=a) FINAL_DisDsf (in=b);
by labid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add Pediatric BMI ********************************************/
/**********************************************************************************************************/
data final_pd_bmi_results;
set PD_BMI.final_pd_bmi_results;
proc sort; by labid;
run;


data final_cchc14;
merge final_cchc13_6_1 (in=a) final_pd_bmi_results (in=b);
if a;
by labid;
if SUBSTR(LABID,1,2) IN ('BL' 'HP' 'LP') then
    do;
        bmi = .;
		bmi1 = .;
		bmigr = .;
		obese_cat = .;
		obese30 = .;
		obese40 = .;
	end;
proc sort; by rrid;
run;


/**********************************************************************************************************/
/******************************************* Add NDI ********************************************/
/**********************************************************************************************************/
data final_NDI_Data;
set NDI_Data.final_ndi;
proc sort; by rrid;
run;


data final_cchc14_1;
merge final_cchc14 (in=a) final_NDI_Data (in=b);
if a;
by rrid;
proc sort; by rrid;
run;

/**********************************************************************************************************/
/******************************************* Add Ancestry Proportion********************************************/
/**********************************************************************************************************/
data FINAL_AncestryProps;
set Ancestry.final_ancestryprops;
proc sort; by rrid;
run;

data final_cchc14_2;
merge final_cchc14_1 (in=a) FINAL_AncestryProps (in=b);
by rrid;
if a;
proc sort; by rrid;
run;

/**********************************************************************************************************/
/******************************************* Add Sequencing Data********************************************/
/**********************************************************************************************************/
data FINAL_Sequencing_Data;
set SEQDATA.final_seq;
proc sort; by rrid;
run;

data final_cchc14_2A;
merge final_cchc14_2 (in=a) FINAL_Sequencing_Data (in=b);
by rrid;
if a;
proc sort; by labid;
run;

/**********************************************************************************************************/
/******************************************* Add EKG Proportion********************************************/
/**********************************************************************************************************/
data FINAL_EKG_WRKSHEET;
set EKGWRK.ekg_worksheet;
proc sort; by labid;
run;

%CheckDataset(FINAL_EKG_WRKSHEET,RawRRIDLABID)

data final_cchc14_3;
merge final_cchc14_2A (in=a) FINAL_EKG_WRKSHEET (in=b);
by labid;
if a;
proc sort; by labid;
run;


/**********************************************************************************************************/
/******************************************* Add FMD Proportion********************************************/
/**********************************************************************************************************/
data FINAL_FMD;
set Brachial.final_fmd_results;
proc sort; by labid;
run;

%CheckDataset(FINAL_FMD,RawRRIDLABID)

data final_cchc14_4;
merge final_cchc14_3 (in=a) FINAL_FMD (in=b);
by labid;
if a;
proc sort; by rrid;
run;


/**********************************************************************************************************/
/******************************************* Add LDLCal Proportion********************************************/
/**********************************************************************************************************/
* generate factor variable; 
data final_cchc14_31;drop nonhdl factor;
set final_cchc14_4;

nonhdl = (chol1 - hdlc); 
factor = .; 

if trig < 50 and nonhdl <100 then factor = 3.5; 
else if trig < 50 and nonhdl >= 100 and nonhdl < 130 then factor = 3.4;
else if trig < 50 and nonhdl >= 130 and nonhdl < 160 then factor = 3.3;
else if trig < 50 and nonhdl >= 160 and nonhdl < 190 then factor = 3.3;
else if trig < 50 and nonhdl >= 190 and nonhdl < 220 then factor = 3.2;
else if trig < 50 and nonhdl >= 220 then factor = 3.1;

else if trig >= 50 and trig < 57 and nonhdl <100 then factor = 4; 
else if trig >= 50 and trig < 57 and nonhdl >= 100 and nonhdl < 130 then factor = 3.9;
else if trig >= 50 and trig < 57 and nonhdl >= 130 and nonhdl < 160 then factor = 3.7;
else if trig >= 50 and trig < 57 and nonhdl >= 160 and nonhdl < 190 then factor = 3.6;
else if trig >= 50 and trig < 57 and nonhdl >= 190 and nonhdl < 220 then factor = 3.6;
else if trig >= 50 and trig < 57 and nonhdl >= 220 then factor = 3.4;

else if trig >= 57 and trig <62 and nonhdl < 100 then factor = 4.3;
else if trig >= 57 and trig <62 and nonhdl >= 100 and nonhdl < 130 then factor = 4.1; 
else if trig >= 57 and trig <62 and nonhdl >= 130 and nonhdl < 160 then factor = 4.0;
else if trig >= 57 and trig <62 and nonhdl >= 160 and nonhdl < 190 then factor = 3.9;
else if trig >= 57 and trig <62 and nonhdl >= 190 and nonhdl < 220 then factor = 3.8;
else if trig >= 57 and trig <62 and nonhdl >= 220 then factor = 3.6;

else if trig >= 62 and trig < 67 and nonhdl < 100 then factor = 4.5;
else if trig >= 62 and trig < 67 and nonhdl >= 100 and nonhdl < 130 then factor = 4.3; 
else if trig >= 62 and trig < 67 and nonhdl >= 130 and nonhdl < 160 then factor = 4.1;
else if trig >= 62 and trig < 67 and nonhdl >= 160 and nonhdl < 190 then factor = 4.0;
else if trig >= 62 and trig < 67 and nonhdl >= 190 and nonhdl < 220 then factor = 3.9;
else if trig >= 62 and trig < 67 and nonhdl >= 220 then factor = 3.9;

else if trig >= 67 and trig < 72 and nonhdl < 100 then factor = 4.7;
else if trig >= 67 and trig < 72 and nonhdl >= 100 and nonhdl < 130 then factor = 4.4; 
else if trig >= 67 and trig < 72 and nonhdl >= 130 and nonhdl < 160 then factor = 4.3;
else if trig >= 67 and trig < 72 and nonhdl >= 160 and nonhdl < 190 then factor = 4.2;
else if trig >= 67 and trig < 72 and nonhdl >= 190 and nonhdl < 220 then factor = 4.1;
else if trig >= 67 and trig < 72 and nonhdl >= 220 then factor = 3.9;

else if trig >= 72 and trig < 76 and nonhdl < 100 then factor = 4.8;
else if trig >= 72 and trig < 76 and nonhdl >= 100 and nonhdl < 130 then factor = 4.6; 
else if trig >= 72 and trig < 76 and nonhdl >= 130 and nonhdl < 160 then factor = 4.4;
else if trig >= 72 and trig < 76 and nonhdl >= 160 and nonhdl < 190 then factor = 4.2;
else if trig >= 72 and trig < 76 and nonhdl >= 190 and nonhdl < 220 then factor = 4.2;
else if trig >= 72 and trig < 76 and nonhdl >= 220 then factor = 4.1;

else if trig >= 76 and trig < 80 and nonhdl < 100 then factor = 4.9;
else if trig >= 76 and trig < 80 and nonhdl >= 100 and nonhdl < 130 then factor = 4.6; 
else if trig >= 76 and trig < 80 and nonhdl >= 130 and nonhdl < 160 then factor = 4.5;
else if trig >= 76 and trig < 80 and nonhdl >= 160 and nonhdl < 190 then factor = 4.3;
else if trig >= 76 and trig < 80 and nonhdl >= 190 and nonhdl < 220 then factor = 4.3;
else if trig >= 76 and trig < 80 and nonhdl >= 220 then factor = 4.2;

else if trig >= 80 and trig < 84 and nonhdl < 100 then factor = 5.0;
else if trig >= 80 and trig < 84 and nonhdl >= 100 and nonhdl < 130 then factor = 4.8; 
else if trig >= 80 and trig < 84 and nonhdl >= 130 and nonhdl < 160 then factor = 4.6;
else if trig >= 80 and trig < 84 and nonhdl >= 160 and nonhdl < 190 then factor = 4.4;
else if trig >= 80 and trig < 84 and nonhdl >= 190 and nonhdl < 220 then factor = 4.3;
else if trig >= 80 and trig < 84 and nonhdl >= 220 then factor = 4.2;

else if trig >= 84 and trig < 88 and nonhdl < 100 then factor = 5.1;
else if trig >= 84 and trig < 88 and nonhdl >= 100 and nonhdl < 130 then factor = 4.8; 
else if trig >= 84 and trig < 88 and nonhdl >= 130 and nonhdl < 160 then factor = 4.6;
else if trig >= 84 and trig < 88 and nonhdl >= 160 and nonhdl < 190 then factor = 4.5;
else if trig >= 84 and trig < 88 and nonhdl >= 190 and nonhdl < 220 then factor = 4.4;
else if trig >= 84 and trig < 88 and nonhdl >= 220 then factor = 4.3;

else if trig >= 88 and trig < 93 and nonhdl < 100 then factor = 5.2;
else if trig >= 88 and trig < 93 and nonhdl >= 100 and nonhdl < 130 then factor = 4.9; 
else if trig >= 88 and trig < 93 and nonhdl >= 130 and nonhdl < 160 then factor = 4.7;
else if trig >= 88 and trig < 93 and nonhdl >= 160 and nonhdl < 190 then factor = 4.6;
else if trig >= 88 and trig < 93 and nonhdl >= 190 and nonhdl < 220 then factor = 4.4;
else if trig >= 88 and trig < 93 and nonhdl >= 220 then factor = 4.3;

else if trig >= 93 and trig < 97 and nonhdl < 100 then factor = 5.3;
else if trig >= 93 and trig < 97 and nonhdl >= 100 and nonhdl < 130 then factor = 5.0; 
else if trig >= 93 and trig < 97 and nonhdl >= 130 and nonhdl < 160 then factor = 4.8;
else if trig >= 93 and trig < 97 and nonhdl >= 160 and nonhdl < 190 then factor = 4.7;
else if trig >= 93 and trig < 97 and nonhdl >= 190 and nonhdl < 220 then factor = 4.5;
else if trig >= 93 and trig < 97 and nonhdl >= 220 then factor = 4.4;

else if trig >= 97 and trig < 101 and nonhdl < 100 then factor = 5.4;
else if trig >= 97 and trig < 101 and nonhdl >= 100 and nonhdl < 130 then factor = 5.1; 
else if trig >= 97 and trig < 101 and nonhdl >= 130 and nonhdl < 160 then factor = 4.8;
else if trig >= 97 and trig < 101 and nonhdl >= 160 and nonhdl < 190 then factor = 4.7;
else if trig >= 97 and trig < 101 and nonhdl >= 190 and nonhdl < 220 then factor = 4.5;
else if trig >= 97 and trig < 101 and nonhdl >= 220 then factor = 4.3;

else if trig >= 101 and trig < 106 and nonhdl < 100 then factor = 5.5;
else if trig >= 101 and trig < 106 and nonhdl >= 100 and nonhdl < 130 then factor = 5.2; 
else if trig >= 101 and trig < 106 and nonhdl >= 130 and nonhdl < 160 then factor = 5.0;
else if trig >= 101 and trig < 106 and nonhdl >= 160 and nonhdl < 190 then factor = 4.7;
else if trig >= 101 and trig < 106 and nonhdl >= 190 and nonhdl < 220 then factor = 4.6;
else if trig >= 101 and trig < 106 and nonhdl >= 220 then factor = 4.5;

else if trig >= 106 and trig < 111 and nonhdl < 100 then factor = 5.6;
else if trig >= 106 and trig < 111 and nonhdl >= 100 and nonhdl < 130 then factor = 5.3; 
else if trig >= 106 and trig < 111 and nonhdl >= 130 and nonhdl < 160 then factor = 5.0;
else if trig >= 106 and trig < 111 and nonhdl >= 160 and nonhdl < 190 then factor = 4.8;
else if trig >= 106 and trig < 111 and nonhdl >= 190 and nonhdl < 220 then factor = 4.6;
else if trig >= 106 and trig < 111 and nonhdl >= 220 then factor = 4.5;

else if trig >= 111 and trig < 116 and nonhdl < 100 then factor = 5.7;
else if trig >= 111 and trig < 116 and nonhdl >= 100 and nonhdl < 130 then factor = 5.4; 
else if trig >= 111 and trig < 116 and nonhdl >= 130 and nonhdl < 160 then factor = 5.1;
else if trig >= 111 and trig < 116 and nonhdl >= 160 and nonhdl < 190 then factor = 4.9;
else if trig >= 111 and trig < 116 and nonhdl >= 190 and nonhdl < 220 then factor = 4.7;
else if trig >= 111 and trig < 116 and nonhdl >= 220 then factor = 4.5;

else if trig >= 116 and trig < 121 and nonhdl < 100 then factor = 5.8;
else if trig >= 116 and trig < 121 and nonhdl >= 100 and nonhdl < 130 then factor = 5.5; 
else if trig >= 116 and trig < 121 and nonhdl >= 130 and nonhdl < 160 then factor = 5.2;
else if trig >= 116 and trig < 121 and nonhdl >= 160 and nonhdl < 190 then factor = 5.0;
else if trig >= 116 and trig < 121 and nonhdl >= 190 and nonhdl < 220 then factor = 4.8;
else if trig >= 116 and trig < 121 and nonhdl >= 220 then factor = 4.6;

else if trig >= 121 and trig < 127 and nonhdl < 100 then factor = 6.0;
else if trig >= 121 and trig < 127 and nonhdl >= 100 and nonhdl < 130 then factor = 5.5; 
else if trig >= 121 and trig < 127 and nonhdl >= 130 and nonhdl < 160 then factor = 5.3;
else if trig >= 121 and trig < 127 and nonhdl >= 160 and nonhdl < 190 then factor = 5.0;
else if trig >= 121 and trig < 127 and nonhdl >= 190 and nonhdl < 220 then factor = 4.8;
else if trig >= 121 and trig < 127 and nonhdl >= 220 then factor = 4.6;

else if trig >= 127 and trig < 133 and nonhdl < 100 then factor = 6.1;
else if trig >= 127 and trig < 133 and nonhdl >= 100 and nonhdl < 130 then factor = 5.7; 
else if trig >= 127 and trig < 133 and nonhdl >= 130 and nonhdl < 160 then factor = 5.3;
else if trig >= 127 and trig < 133 and nonhdl >= 160 and nonhdl < 190 then factor = 5.1;
else if trig >= 127 and trig < 133 and nonhdl >= 190 and nonhdl < 220 then factor = 4.9;
else if trig >= 127 and trig < 133 and nonhdl >= 220 then factor = 4.7;

else if trig >= 133 and trig < 139 and nonhdl < 100 then factor = 6.2;
else if trig >= 133 and trig < 139 and nonhdl >= 100 and nonhdl < 130 then factor = 5.8; 
else if trig >= 133 and trig < 139 and nonhdl >= 130 and nonhdl < 160 then factor = 5.4;
else if trig >= 133 and trig < 139 and nonhdl >= 160 and nonhdl < 190 then factor = 5.2;
else if trig >= 133 and trig < 139 and nonhdl >= 190 and nonhdl < 220 then factor = 5.0;
else if trig >= 133 and trig < 139 and nonhdl >= 220 then factor = 4.7;

else if trig >= 139 and trig < 147 and nonhdl < 100 then factor = 6.3;
else if trig >= 139 and trig < 147 and nonhdl >= 100 and nonhdl < 130 then factor = 5.9; 
else if trig >= 139 and trig < 147 and nonhdl >= 130 and nonhdl < 160 then factor = 5.6;
else if trig >= 139 and trig < 147 and nonhdl >= 160 and nonhdl < 190 then factor = 5.3;
else if trig >= 139 and trig < 147 and nonhdl >= 190 and nonhdl < 220 then factor = 5.0;
else if trig >= 139 and trig < 147 and nonhdl >= 220 then factor = 4.8;

else if trig >= 147 and trig < 155 and nonhdl < 100 then factor = 6.5;
else if trig >= 147 and trig < 155 and nonhdl >= 100 and nonhdl < 130 then factor = 6.0; 
else if trig >= 147 and trig < 155 and nonhdl >= 130 and nonhdl < 160 then factor = 5.7;
else if trig >= 147 and trig < 155 and nonhdl >= 160 and nonhdl < 190 then factor = 5.4;
else if trig >= 147 and trig < 155 and nonhdl >= 190 and nonhdl < 220 then factor = 5.1;
else if trig >= 147 and trig < 155 and nonhdl >= 220 then factor = 4.8;

else if trig >= 155 and trig < 164 and nonhdl < 100 then factor = 6.7;
else if trig >= 155 and trig < 164 and nonhdl >= 100 and nonhdl < 130 then factor = 6.2; 
else if trig >= 155 and trig < 164 and nonhdl >= 130 and nonhdl < 160 then factor = 5.8;
else if trig >= 155 and trig < 164 and nonhdl >= 160 and nonhdl < 190 then factor = 5.4;
else if trig >= 155 and trig < 164 and nonhdl >= 190 and nonhdl < 220 then factor = 5.2;
else if trig >= 155 and trig < 164 and nonhdl >= 220 then factor = 4.9;

else if trig >= 164 and trig < 174 and nonhdl < 100 then factor = 6.8;
else if trig >= 164 and trig < 174 and nonhdl >= 100 and nonhdl < 130 then factor = 6.3; 
else if trig >= 164 and trig < 174 and nonhdl >= 130 and nonhdl < 160 then factor = 5.9;
else if trig >= 164 and trig < 174 and nonhdl >= 160 and nonhdl < 190 then factor = 5.5;
else if trig >= 164 and trig < 174 and nonhdl >= 190 and nonhdl < 220 then factor = 5.3;
else if trig >= 164 and trig < 174 and nonhdl >= 220 then factor = 5.0;

else if trig >= 174 and trig < 186 and nonhdl < 100 then factor = 7.0;
else if trig >= 174 and trig < 186 and nonhdl >= 100 and nonhdl < 130 then factor = 6.5; 
else if trig >= 174 and trig < 186 and nonhdl >= 130 and nonhdl < 160 then factor = 6.0;
else if trig >= 174 and trig < 186 and nonhdl >= 160 and nonhdl < 190 then factor = 5.7;
else if trig >= 174 and trig < 186 and nonhdl >= 190 and nonhdl < 220 then factor = 5.4;
else if trig >= 174 and trig < 186 and nonhdl >= 220 then factor = 5.1;
 
else if trig >= 186 and trig < 202 and nonhdl < 100 then factor = 7.3;
else if trig >= 186 and trig < 202 and nonhdl >= 100 and nonhdl < 130 then factor = 6.7; 
else if trig >= 186 and trig < 202 and nonhdl >= 130 and nonhdl < 160 then factor = 6.2;
else if trig >= 186 and trig < 202 and nonhdl >= 160 and nonhdl < 190 then factor = 5.8;
else if trig >= 186 and trig < 202 and nonhdl >= 190 and nonhdl < 220 then factor = 5.5;
else if trig >= 186 and trig < 202 and nonhdl >= 220 then factor = 5.2;

else if trig >= 202 and trig < 221 and nonhdl < 100 then factor = 7.6;
else if trig >= 202 and trig < 221 and nonhdl >= 100 and nonhdl < 130 then factor = 6.9; 
else if trig >= 202 and trig < 221 and nonhdl >= 130 and nonhdl < 160 then factor = 6.4;
else if trig >= 202 and trig < 221 and nonhdl >= 160 and nonhdl < 190 then factor = 6.0;
else if trig >= 202 and trig < 221 and nonhdl >= 190 and nonhdl < 220 then factor = 5.6;
else if trig >= 202 and trig < 221 and nonhdl >= 220 then factor = 5.3;

else if trig >= 221 and trig < 248 and nonhdl < 100 then factor = 8.0;
else if trig >= 221 and trig < 248 and nonhdl >= 100 and nonhdl < 130 then factor = 7.6; 
else if trig >= 221 and trig < 248 and nonhdl >= 130 and nonhdl < 160 then factor = 6.6;
else if trig >= 221 and trig < 248 and nonhdl >= 160 and nonhdl < 190 then factor = 6.2;
else if trig >= 221 and trig < 248 and nonhdl >= 190 and nonhdl < 220 then factor = 5.9;
else if trig >= 221 and trig < 248 and nonhdl >= 220 then factor = 5.4;

else if trig >= 248 and trig < 293 and nonhdl < 100 then factor = 8.5;
else if trig >= 248 and trig < 293 and nonhdl >= 100 and nonhdl < 130 then factor = 7.6; 
else if trig >= 248 and trig < 293 and nonhdl >= 130 and nonhdl < 160 then factor = 7.0;
else if trig >= 248 and trig < 293 and nonhdl >= 160 and nonhdl < 190 then factor = 6.5;
else if trig >= 248 and trig < 293 and nonhdl >= 190 and nonhdl < 220 then factor = 6.1;
else if trig >= 248 and trig < 293 and nonhdl >= 220 then factor = 5.6;

else if trig >= 293 and trig < 400 and nonhdl < 100 then factor = 9.5;
else if trig >= 293 and trig < 400 and nonhdl >= 100 and nonhdl < 130 then factor = 8.3; 
else if trig >= 293 and trig < 400 and nonhdl >= 130 and nonhdl < 160 then factor = 7.5;
else if trig >= 293 and trig < 400 and nonhdl >= 160 and nonhdl < 190 then factor = 7.0;
else if trig >= 293 and trig < 400 and nonhdl >= 190 and nonhdl < 220 then factor = 6.5;
else if trig >= 293 and trig < 400 and nonhdl >= 220 then factor = 5.9;

else if trig >= 400 and nonhdl < 100 then factor = 11.9;
else if trig >= 400 and nonhdl >= 100 and nonhdl < 130 then factor = 10.0;
else if trig >= 400 and nonhdl >= 130 and nonhdl < 160 then factor = 8.8;
else if trig >= 400 and nonhdl >= 160 and nonhdl < 190 then factor = 8.1;
else if trig >= 400 and nonhdl >= 190 and nonhdl < 220 then factor = 7.5;
else if trig >= 400 and nonhdl >= 220 then factor = 6.7;

else factor = .; 

ldl180 = nonhdl - (trig/factor); 

label ldl180="LDL Calculated Variable : non-hdlc(total cholesterol-HDL cholesterol) and triglyceride thresholds";

proc sort; by rrid;
run; 


/**********************************************************************************************************/
/****************************************** Add Zip Codes *************************************************/
/**********************************************************************************************************/

data final_zip_codes;
set myaddr.addressbook;
thiskey = substr(rrid,1,1);
thatkey = substr(rrid,2,1);
where rrid <> "";
keep rrid zip;
if thiskey = "R" or thatkey = "R" then delete;
proc sort; by rrid;
run;


data final_cchc15;
merge final_cchc14_31 (in=a) final_zip_codes (in=b);
if a;
by rrid;
run;

/**********************************************************************************************************/
/********************************************* ADDING WEIGHTS *********************************************/
/**********************************************************************************************************/
/*
data final_weights;
set tmpw8ts.all_weights;
W_DATE = '02JUN2017'd;
FORMAT W_DATE date9.;
LABEL W = 'Weights for the data set.';
LABEL W_DATE = "Date the weights 'W' were created.";
run;

proc sort data = final_cchc15; by rrid; run;
proc sort data = final_weights; by rrid; run;

data final_cchc16;
merge final_cchc15 (in=a) final_weights (in=b);
by rrid;
if a;
W_DATE = '02JUN2017'd;
FORMAT W_DATE date9.;
run;

proc sort data = final_cchc16; by rrid visit; run;
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*		TESTING NEW FORMATS		ES 6/21/2017	*/
DATA final_cchc15_iz; set final_cchc15;	/*<=== Gets a copy of cchc and performs some adjustments*/

numlen = length(compress(input(schdate,$32.)));/*This code check for date larger than 5 digits*/
if numlen >5 then SCHDATE=(SCHDATE/86400);	/*So it can be processed as date9*/
numlen7 = length(compress(input(EAN_DATE,$32.)));/*This code check for date larger than 5 digits*/
if numlen7 >5 then EAN_DATE=(EAN_DATE/86400);	/*So it can be processed as date9*/

*numlen8 = length(compress(input(ENTRYDATE,$32.)));/*This code check for date larger than 5 digits*/
*if numlen8 >5 then ENTRYDATE=(ENTRYDATE/86400);	/*So it can be processed as date9*/

*numlen9 = length(compress(input(SP_EXAMDATE,$32.)));/*This code check for date larger than 5 digits*/
*if numlen9 >5 then SP_EXAMDATE=(SP_EXAMDATE/86400);	/*So it can be processed as date9*/

numlen10 = length(compress(input(DTSTRESSOR_DATE,$32.)));/*This code check for date larger than 5 digits*/
if numlen10 >5 then DTSTRESSOR_DATE=(DTSTRESSOR_DATE/86400);	/*So it can be processed as date9*/
if numlen10 >8 then DTSTRESSOR_DATE=.;	/*So it can be processed as date9*/

numlen2 = length(compress(input(SURVDATE,$32.)));/*This code check for date larger than 5 digits*/
if numlen2 >5 then SURVDATE=(SURVDATE/86400);	/*So it can be processed as date9*/

numlen3 = length(compress(input(RMDDATE,$32.)));/*This code check for date larger than 6 digits*/
if numlen3 >6 then RMDDATE=(RMDDATE/86400);	/*So it can be processed as date9*/

numlen4 = length(compress(input(BP_EXAMDATE,$32.)));/*This code check for date larger than 6 digits*/
if numlen4 >6 then BP_EXAMDATE=(BP_EXAMDATE/86400);	/*So it can be processed as date9*/

numlen5 = length(compress(input(RMDDATE,$32.)));/*This code check for date larger than 6 digits*/
if numlen5 >5 then RMDDATE=(RMDDATE/86400);	/*So it can be processed as date9*/
if numlen5 >6 then RMDDATE=.;

numlen6 = length(compress(input(RMD1LAST,$32.)));/*This code check for date larger than 6 digits*/
if numlen6 >5 then RMD1LAST=(RMD1LAST/86400);	/*So it can be processed as date9*/
if numlen6 >6 then RMD1LAST=.;

*numlen11 = length(compress(input(UP_DATE,$32.)));/*This code check for date larger than 6 digits*/
*if numlen11 >6 then UP_DATE=(UP_DATE/86400);	/*So it can be processed as date9*/

*numlen12 = length(compress(input(LAST_UPDATE,$32.)));/*This code check for date larger than 6 digits*/
*if numlen12 >6 then LAST_UPDATE=(LAST_UPDATE/86400);	/*So it can be processed as date9*/

drop w w_date; *Drop old weights and date  IZ 06282017;

if urinsamp = . then
    do;
	    if lab_urinsamp NE . then urinsamp = lab_urinsamp;
		else if crl_urinsamp NE . then urinsamp = crl_urinsamp;
	end;
if urindat = . then
    do;
	    if lab_urindat NE . then urindat = lab_urindat;
	end;
if FBG3 = . then
	do;
	    if fbg_crl ne . then FBG3 = FBG_CRL;
	end;

proc sort; by labid;

run;

data my_drsall_data1;
LENGTH STUDY $30.;
set drs.patient_alldrs_iz;
where interview_date NE .;
SUBDIV = '   ';
SECTION = ' ';
SUBDIV = SUBSTR(KEY7,13,3);
INOUT = SUBSTR(KEY7,16,1)*1;
SECTION = SUBSTR(KEY7,17,1);
DWELLING = SUBSTR(KEY7,18,3)*1;

/*added to change TE_kPa and LA_SIZE_REV1 to numeric*/
LA_SIZE_REV1_new = input(LA_SIZE_REV1, 8.);
drop LA_SIZE_REV1;
rename LA_SIZE_REV1_new=LA_SIZE_REV1;

TE_kPa_new = input(TE_kPa, 8.);
drop TE_kPa;
rename TE_kPa_new=TE_kPa;

/* added 09/28/22*/
p_com1_new = input(p_com1, 8.);
drop p_com1;
rename p_com1_new=p_com1;

p_com2_new = input(p_com2, 8.);
drop p_com2;
rename p_com2_new=p_com2;


CESD_SCORE=DEPRESSION_SCORE;

if Diastolic_Dysfunction = 1 then Diastolic_Dysfunction_n="grade 1";
if Diastolic_Dysfunction = 0 then Diastolic_Dysfunction_n="normal";


drop LA_SIZE_REV1 TE_kPa visit bdvisit fibroscan_operator fibroscan_referring_physician fibroscan_indication fibroscan_exam_date fibroscan_exam_duration__s_
fibroscan_exam_type fibroscan_calibration_status fibroscan_total_measures_number fibroscan_valid_measures_number fibroscan_e_med__kpa_
fibroscan_e_iqr__kpa_ fibroscan_e_iqr___e_med Fibroscan_CAP_med__dB_m_ Fibroscan_CAP_IQR__dB_m_ Fibroscan_Height Fibroscan_Unit__Height_
Fibroscan_Weight Fibroscan_Unit__Weight_ Fibroscan_OMPLF Fibroscan_Patient_Positioning Fibroscan_Narrow_Intercostal_Spa HAS_FIBROSCAN_US
Fibroscan_Thick_Adipose_Paniculu Fibroscan_Suboptimal_Us_Signal Fibroscan_Major_Vascular_Structu Fibroscan_Appropriate_Fasting_Co lvpwd
Fibroscan_V_med__m_s_ Fibroscan_V_IQR__m_s_ age_echo cardiac_history lv_size lvidd lvids lvh rwt lv_mass lv_mass_index lv_ef lv_ef_ rv_fx tapse
la_size la_dimension la_volume ra_size ra_volume mac epi_fat mv_e mv_a e_a mv_decel_time ivrt ivct pv_s pv_d pv_a_vel pv_a_duration
lateral_e lateral_a lateral_s lateral_e_e medial_e medial_a medial_s medial_e_e diastolic_fx co ci rwma mr ai tr pasp rap as ms
pericardial_effusion echo_other_comments remodeling has_echo lvm_index_cat DEPRESSION_SCORE Diastolic_Dysfunction;

rename Diastolic_Dysfunction_n=Diastolic_Dysfunction;

STUDY = "RISK";
proc sort; by labid;
run;

data my_drsall_data;
merge my_drsall_data1 (in=a) final_liver_results (in=b);
by labid;
if a;
proc sort; by labid;
run;

data final_cchc15_iz1;
merge final_cchc15_iz (in = a) my_drsall_data (in = b);
by LABID;
if not a;
run;

/*
data missing_rrid;
set final.cchc_missing_rrid;
run;
*/

data final_cchc15_iz2;
*set final_cchc15_iz final_cchc15_iz1 missing_rrid;
set final_cchc15_iz final_cchc15_iz1;
drop 
REG10_AREA
REG10_BMC
REG10_BMD
REG10_FAT
REG10_LEAN
REG10_MASS
REG10_NAME
REG10_PFAT
REG11_AREA
REG11_BMC
REG11_BMD
REG11_FAT
REG11_LEAN
REG11_MASS
REG11_NAME
REG11_PFAT
REG12_AREA
REG12_BMC
REG12_BMD
REG12_FAT
REG12_LEAN
REG12_MASS
REG12_NAME
REG12_PFAT
REG13_AREA
REG13_BMC
REG13_BMD
REG13_FAT
REG13_LEAN
REG13_MASS
REG13_NAME
REG13_PFAT
REG14_AREA
REG14_BMC
REG14_BMD
REG14_FAT
REG14_LEAN
REG14_MASS
REG14_NAME
REG14_PFAT
REG1_AREA
REG1_BMC
REG1_BMD
REG1_FAT
REG1_LEAN
REG1_MASS
REG1_NAME
REG1_PFAT
REG2_AREA
REG2_BMC
REG2_BMD
REG2_FAT
REG2_LEAN
REG2_MASS
REG2_NAME
REG2_PFAT
REG3_AREA
REG3_BMC
REG3_BMD
REG3_FAT
REG3_LEAN
REG3_MASS
REG3_NAME
REG3_PFAT
REG4_AREA
REG4_BMC
REG4_BMD
REG4_FAT
REG4_LEAN
REG4_MASS
REG4_NAME
REG4_PFAT
REG5_AREA
REG5_BMC
REG5_BMD
REG5_FAT
REG5_LEAN
REG5_MASS
REG5_NAME
REG5_PFAT
REG6_AREA
REG6_BMC
REG6_BMD
REG6_FAT
REG6_LEAN
REG6_MASS
REG6_NAME
REG6_PFAT
REG7_AREA
REG7_BMC
REG7_BMD
REG7_FAT
REG7_LEAN
REG7_MASS
REG7_NAME
REG7_PFAT
REG8_AREA
REG8_BMC
REG8_BMD
REG8_FAT
REG8_LEAN
REG8_MASS
REG8_NAME
REG8_PFAT
REG9_AREA
REG9_BMC
REG9_BMD
REG9_FAT
REG9_LEAN
REG9_MASS
REG9_NAME
REG9_PFAT
numlen
numlen7
numlen10
numlen2
numlen3
numlen4
numlen5
numlen6
;
proc sort; by rrid visit;
run;

data final_cchc15_iz3;
set final_cchc15_iz2;
FORMAT OCCUPATI_CH OCCMOSTL_CH SPOCCUPA_CH SPOCCMOS_CH Z3.;
OCCUPATI_CH = OCCUPATI;
OCCMOSTL_CH = OCCMOSTL; *CONVERTING NUMERIC TO CHAR;
SPOCCUPA_CH = SPOCCUPA; *CONVERTING NUMERIC TO CHAR;
SPOCCMOS_CH = SPOCCMOS; *CONVERTING NUMERIC TO CHAR;
DROP OCCUPATI OCCMOSTL SPOCCUPA SPOCCMOS;
run;

data final_cchc15_iz4;
set final_cchc15_iz3;
DROP OCCUPATI_CH OCCMOSTL_CH SPOCCUPA_CH SPOCCMOS_CH URNCOLLECTED CS_INTERVIEW_DATE DATE_URINE CRL_URINSAMP LAB_URINDAT LAB_URINSAMP
COL8 COL9 COL10 COL11 COL12 COL13 COL14 COL15 COL16 COL17 COL18 COL19 COL20 COL21 COL22 COL23 COL24;
FORMAT OCCUPATI OCCMOSTL SPOCCUPA SPOCCMOS Z3.;
OCCUPATI = OCCUPATI_CH;
OCCMOSTL = OCCMOSTL_CH;
SPOCCUPA = SPOCCUPA_CH;
SPOCCMOS = SPOCCMOS_CH;
proc sort; by labid;
RUN;

data final_gad65ab_results;
set GAD65Ab.FINAL_GAD65Ab_RESULTS;
proc sort; by labid;
run;

%CheckDataset(final_gad65ab_results,RawRRIDLABID)

data final_cchc15_iz5;
merge final_cchc15_iz4 (in = a) FINAL_GAD65AB_RESULTS (in = b);
by labid;
if a;
drop BASELINE_mm RH60mm RH90mm FMDPCTCHANGE DATE_HCV FATTYLIVER HEPAECHO S2L;
run;

data my_fmd; length RRID $6.;
set fmdlib.fmd;
FORMAT RRID $6.;
INFORMAT RRID $6.;
IF INDEX(UPCASE(RRID), 'R') <> 0 THEN DELETE;
*MYRRID = RRID;
proc sort nodupkey; by rrid;
run;

/*
data my_fmd1;
set my_fmd;
drop RRID;
run;

data my_fmd2;
set my_fmd1;
rename MYRRID = RRID;
proc sort; by rrid;
run;
*/

proc sort data = final_cchc15_iz5; by rrid visit; run;

data final_cchc15_iz6;
merge final_cchc15_iz5 (in=a) my_fmd (in=b);
by rrid;
if a;
if has_retina_scan = "" then has_retina_scan = 0;
if has_tbs = "" then has_tbs = 0;
if substr(rrid,1,2) = "BD" then town = 1;
if substr(rrid,1,2) = "HD" then town = 2;
if substr(rrid,1,2) = "LD" then town = 3;
drop imp_weightAP imp_weightmh mh_weight rakeddesign_weight_block w_range weight_age_gender
base_weight_bk design_weight design_weight_tract ind_imp_wt02 weight_age_gender_2011 w w_date;
run;

data final_cchc16;
drop YRSINBRO2 interview_date2 f49-f103;
set final_cchc15_iz6;
by rrid;
length YRSINBRO2 $50.;
retain YRSINBRO2 interview_date2; 

/* Reset TEMP when the BY-Group changes */
if first.study_id then  do; YRSINBRO2=""; interview_date2=0; end;


if YRSINBRO ne . then do; YRSINBRO2=YRSINBRO; interview_date2=interview_date; end;
else if YRSINBRO = . then  YRSINBRO=YRSINBRO2 + ( year(interview_date)- year(interview_date2) );
run;


/*Jump to Process program from all Labels		ES 6/21/2017	*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*
NOTE: The data set FINAL.CCHC_BASELINE has 7933 observations and 1751 variables. 6.29.15
NOTE: The data set FINAL.CCHC_BASELINE has 7939 observations and 1751 variables. 7.6.15 6:18PM
NOTE: The data set FINAL.CCHC_BASELINE has 7940 observations and 1752 variables. 7.21.15
NOTE: The data set FINAL.CCHC_BASELINE has 8007 observations and 2145 variables. 7.29.15 (includes 5 pediatric tables)
NOTE: The data set FINAL.CCHC has 8018 observations and 2140 variables. 7.31.15 (06:25PM)

/*
PROC FREQ data=mcab1; WHERE LABID = 'LD4028'; TABLE lab_examdate INTERVIEW_DATE RRID; RUN;

DATA ZZ1; SET FINAL.CCHC_BASELINE; WHERE RRID = 'BD2073'; RUN;

PROC FREQ DATA=MCAB3; TABLE MFBG FBG1 FBG2 FBG_CRL; RUN;

DATA TEST1; SET MCAB3; WHERE LABID = '' AND INTERVIEW_DATE NE .; RUN;

PROC PRINT; RUN;


/*FBG1/FBG2/FBG_CRL 
= 1, 9, 0, .38*/
/*
PROC PRINT DATA = MCAB3; 
VAR RRID 
 INCMONTH
 INCYEAR
 spincmth
 spincyr
 C_hhincmth
 C_hhincyr
 HHINCMTH
 HHINCYR
;
RUN;
*/
/*
PROC FREQ data=FINAL.CCHC_BASELINE; WHERE RRID = 'LD0017'; RUN;
*/
/*
PROC FREQ data=FINAL.CCHC_BASELINE; TABLE EOS_NO*RRID*LABID / LIST MISSING; WHERE EOS_NO = 0;
RUN;
*/
/*
PROC FREQ DATA=RAW.MERGED1; WHERE RRID = 'LD0017'; RUN;
*/
*END OF PROGRAM********************************************************************;
***********************************************************************************;
***********************************************************************************;

/*
TITLE 'PROC FREQ DATA=A3; TABLE RED; RUN;';
PROC FREQ DATA=A3; TABLE RED; RUN;
TITLE 'PROC FREQ DATA=A3; TABLE RED; RUN;';
PROC FREQ DATA=A3; TABLE RRID*VISIT*RED /LIST MISSING; RUN;

DATA X; SET A3; KEEP RRID VISIT LABID STUDY INTERVIEW_DATE; RUN;
PROC SORT; BY RRID VISIT; RUN;

PROC SORT DATA=MCAB1 OUT=U1 NODUPKEY; BY BDVISIT; RUN;

PROC SORT DATA=MCAB1; BY BDVISIT; RUN;
DATA DUPES1; SET MCAB1; 
IF FIRST.BDVISIT THEN COUNT = 0; 
BY BDVISIT; 
COUNT +1;
RUN;
PROC PRINT; WHERE COUNT > 1; RUN;

*DUPLICATE FOUND: LD002801 LD0028 .... 5.7.15

PROC FREQ; TABLE LABID; RUN;


NOTE: The data set FINAL.CCHC_BASELINE has 7933 observations and 1751 variables. 6.29.15
NOTE: The data set FINAL.CCHC_BASELINE has 7939 observations and 1751 variables. 7.6.15 6:18PM
NOTE: The data set FINAL.CCHC_BASELINE has 7940 observations and 1752 variables. 7.21.15
NOTE: The data set FINAL.CCHC_BASELINE has 8007 observations and 2145 variables. 7.29.15 (includes 5 pediatric tables)
NOTE: The data set FINAL.CCHC has 8018 observations and 2140 variables. 7.31.15


data luminex;
set final_cchc13;
where adiponectin NE .;
keep BDVISIT RRID VISIT STUDY LABID INTERVIEW_DATE adiponectin;
run;

proc freq data = luminex; TABLE study; run;

proc contents data = my_dexa_data; run;
proc contents data = dxa.final_tbs_dxa; run;


data check_chol_trigs;
set compare_new_s8;
keep rrid visit labid study chol trigs;
run;

data check_BD0002;
set final_cchc16;
keep bdvisit rrid visit labid study interview_date;
where rrid = "BD0002";
run;

data check_my_missing;
set final_cchc15_iz2;
keep bdvisit rrid visit labid study interview_date;
where rrid in ("BD1625", "BD3437");
run;









proc contents data = c1; run;
proc contents data = c6; run;
proc contents data = cchc1; run;
proc contents data = cchc6; run;
proc contents data = a1; run;
proc contents data = a4; run;
proc contents data = final_cchc3; run;
proc contents data = final_cchc16; run;
proc contents data = final_cchc7; run;
proc contents data = final_cchc11; run;
proc contents data = my_drsall_data; run;
proc contents data = final_cchc15_iz2; run;
