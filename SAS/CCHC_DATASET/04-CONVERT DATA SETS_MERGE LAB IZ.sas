*	Written by: 	P.Sanchez; 
*	Date: 			3/05/2013 ;
*	Description:	Merges final data from MS Access with lab data in Server 8. Converts
	several 'date' variables from MS Access date formats into SAS date formats. 
*	Generates: 		These data sets are updated: RAW.DMHXMED, RAW.BP, RAW.LAB, RAW.IDD,
					RAW.MHXMED, RAW.SMOKEDRINK,
	RAW.WELCHBP, RAW.EXITCOVERFORM, RAW.HEMAT, RAW.ANTH, RAW.CONTACTSCHEDULINGPAT;
*	Updates:
	10.11.13	PS	Luminex data is in 2 files.
	12.16.13	PS	BNP data set has been renamed to bnp_all. 
					Troponin-T data set has been changed to troponin_t_all. New folders 
					created.
	03.06.14	PS	Temporarily using Breininger2 folder.
	03.07.14	PS	Adding CRLdata table. Adding 1 more crl data set 'CRL2013H'.
				PS	Fixed 1 'proc sort by' error.
	03.14.14	PS	Removed libraries. Saved 3.28.14.
	04.11.14	PS	Removing Cytokines library
	04.30.14	PS	Adding 3 new CRL files: 'crl2013b_extra', 'crl2013j', 'crp14a_rahc'.
	05.01.14	PS	Removed 2 CRL files with bad LABIDs.
	05.02.14	PS	Renamed 1 library 'tro'.
	05.06.14	PS	Renamed GHB library.
	05.09.14	PS	Removing 'PLQ' library. Added 1 CRL file (CRL14A).
	05.22.14	PS	Fixing CAB variable
	06.11.14	PS	Adding ECHO data from Beverly.
	06.27.14	PS	Adding 1 CRL file (crl2014b)
	08.01.14	PS	Adding 1 CRL file (crl2014c), adding HBV_CORE_ALL data, updating 
					'patient_impat'.
	08.27.14	PS	Changed paths only.
	09.03.14	PS	Adding ECG(XI) data and Sampling Weights*. Updated CRL server datasets.
	10.10.14	PS	Splitting program in 2 due to slow network.
	10.23.14	PS	Removing adiponection_ng and restisin_pg code from program. Moved to 
					another program.
	10.27.14	PS	Removing HCV, HBV_CORE & HBV_DATE_HBV_CORE, ECG variables.
	11.17.14	PS	Renaming temporary data sets. Keeping only 1st record. Keeping only 
					BD2001 and above.
	11.24.14	PS	No longer using data in CRL folder.
	11.25.14	PS	Removing unnecessary code.
	11.26.14	PS	Removing unused variables.
	12.02.14	PS	Added KIDNLAST. No longer merging ECG data in this program.
	12.03.14	PS	Dropping KEY8 and KEY7 from most data sets. Dropping PLTCNT.
					Recoding PLT.
	02.02.15	PS	Converting 23 date variables. (Lab and Cover). Changed 1 path 
					(was: CCHC. Now: BASELINE).
	02.19.15	PS	Updating COMMENTS in IDD form to: IDDCOMMENTS.
	03.06.15	PS	Removing 'pace_study', 'fyr_study', 'drs_study' variables.
	03.13.15	PS	Converting data sets for all visits.
	03.31.15	PS	Adding "_all" to data sets due to adding DRS data.
	04.29.15	PS	Updating CRLDATA to CRLDATA_ALL.
	05.20.15	PS	Adding 10Year insulin data.
	05.21.15	PS	Updating paths only.
	06.10.15	PS	Creating MEDCOMM from COMMENTS in the table 'mhxmed'.
	06.24.15	PS	Removed hard coding.
	06.25.15	PS	Using 'U' instead of 'Y'.
	07.29.15	PS	Adding 5 Pediatric tables (CTQ, SLES_C, SLES_P, TANR_F, TANR_M).
	07.11.16	ES	-Keep consistency over naming Folders in all programs.

;
OPTIONS NOCENTER;
/*%let path = \\uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core;

LIBNAME LIBRARY '\\uthouston.edu\uthsc\sph\Research\Studies Data\SAS Formats';
LIBNAME PACEP 'U:\Research\BReininger2\PACE\RECON\ACCESS2010\DATA\FINAL DATA\ALL DATA';
LIBNAME OBLIB 'U:\Research\Studies Data\OB_Study';
LIBNAME RAW "&path\MSAccess\FINAL DATA\BASELINE\DATABASE TABLES SAS\PACKETS";
LIBNAME S8GHB1 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\NEWCCHC";
LIBNAME S8GHB2 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\5YRS";
LIBNAME S8GHB3 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\DRS";
LIBNAME LIBINS1 "&path\LABORATORY_RAHC\INSULIN\CCHC\";
/************    Merged Insulin data into one dataset.           IH 10/13/2015
LIBNAME LIBINS1 "&path\LABORATORY_RAHC\INSULIN\NEWCCHC\COMBINED DATA";
LIBNAME LIBINS2 "&path\LABORATORY_RAHC\INSULIN\5YR\COMBINED DATA";
LIBNAME LIBINS3 "&path\LABORATORY_RAHC\INSULIN\DRS\COMBINED DATA";
LIBNAME LIBINS4 "&path\LABORATORY_RAHC\INSULIN\10YR\COMBINED DATA";
*************/
/*LIBNAME LIBCK18 "&path\LABORATORY_RAHC\CYTOKERATIN-18\FINAL DATA";
libname SRAGEp "&path\Laboratory_specimens_sent_to otherLab\sRageTest\COMBINED DATA";
libname MICROARp "&path\Microarray"; *0 RECORDS FOR OLD/NEW CCHC";
LIBNAME BNPL "&path\LABORATORY_RAHC\BNP\FINAL DATA\COMBINED";
LIBNAME IMT "&path\ULTRASOUND DATA\cIMT\FINAL DATA\";
LIBNAME TRO "&path\LABORATORY_RAHC\TROPONIN_T\COMBINED DATA";
LIBNAME URINE "&path\LABORATORY_RAHC\URINE\FINAL DATA\COMBINED";
LIBNAME LUMINEX "&path\LABORATORY_RAHC\LUMINEX\FINAL DATA\COMBINED";
LIBNAME ECHO "&path\ULTRASOUND DATA\ECHOS\FINAL DATA"; *UPDATED 8.27.14;
LIBNAME SW1 "&path\Sampling_Weights\Weight_age_gender";
LIBNAME SW "&path\Sampling_Weights\w";
LIBNAME LUMIN2 "&path\Laboratory_specimens_sent_to otherLab\UTHSCSA\LUMINEX\FINAL DATA";
*/
*FIX DATES******************************************************;
/*
DATA RAW.BP_converted; SET RAW.BP_ALL; DROP SMOKE EXAMDATE KEY7 KEY8;
*/
DATA RAW.BP_converted; SET RAW.BP; DROP SMOKE /*TIME*/ EXAMDATE;
	BP_EXAMDATE = EXAMDATE/86400; 
	SMOKE_BP = SMOKE; 
	BPRANDOMZERO_FORM = 1; 
	
	FORMAT BP_EXAMDATE DATE9.;
	IF RRID = '' THEN DELETE;
RUN;
*************************************************************************************;
DATA RAW.DMHXMED_converted; SET RAW.DMHXMED; DROP EXAMDATE i COMMENTS;
DIABFORMCOMM = UPCASE(COMMENTS);
med1=kupcase(med1); 
med2=kupcase(med2); 
med3=kupcase(med3); 
med4=kupcase(med4); 
med5=kupcase(med5); 
med6=kupcase(med6); 

KIDNLAST = KIDNLAST/86400;
RMDDATE = RMDDATE/86400;
RMD1LAST = RMD1LAST/86400;
RMD2LAST = RMD2LAST/86400;
RMD3LAST = RMD3LAST/86400;
DHX_EXAMDATE = EXAMDATE/86400; 

	array num (*) _numeric_ ;
	do i=1 to dim(num);
   		if num(i) in (-21917) then 
   		num(i) = .;
	end;

	rename 	med1=diabMed1
       	med2=diabMed2
	   	med3=diabMed3
	   	med4=diabMed4
		med5=diabMed5
		med6=diabmed6;

	
	FORMAT KIDNLAST RMDDATE RMD1LAST RMD2LAST RMD3LAST DHX_EXAMDATE DATE9.;
	IF RRID = '' THEN DELETE;
RUN;
**********************************************************************************;
**********************************************************************************;
DATA RAW.IDENTIFIERS; SET RAW.IDD; 
	KEEP KEY7 KEY8 RRID DOB FGIVEN FMATSUR FPATSUR FSPOUSUR MGIVEN MMATSUR MPATSUR MSPOUSUR 
		 SPGIVEN SPMATSUR SPPATSUR;
	DOB = DOB/86400;
	IF VISIT = 1;
	FORMAT DOB DATE9.;
	IF RRID = '' THEN DELETE;
	RUN;
DATA IDD1; SET RAW.IDD;
	DROP DOB OCCUPATI OCCMOSTL SPOCCUPA SPOCCMOS COMMENTS
	FGIVEN FMATSUR FPATSUR FSPOUSUR MGIVEN MMATSUR MPATSUR MSPOUSUR
	SPGIVEN SPMATSUR SPPATSUR; 
	OCCUPATI_CH = PUT(OCCUPATI,Z3.); 
	OCCMOSTL_CH = put(OCCMOSTL,Z3.); *CONVERTING NUMERIC TO CHAR;
	SPOCCUPA_CH = put(SPOCCUPA,Z3.); *CONVERTING NUMERIC TO CHAR;
	SPOCCMOS_CH = put(SPOCCMOS,Z3.); *CONVERTING NUMERIC TO CHAR;
	IDDCOMMENTS = UPCASE(COMMENTS);
*	IF VISIT > 1 THEN AGE = .;
	IF RRID = '' THEN DELETE;
	RUN;
	DATA IDD2; SET IDD1; 
	DROP OCCUPATI_CH OCCMOSTL_CH SPOCCUPA_CH SPOCCMOS_CH;
	OCCUPATI = OCCUPATI_CH;
	OCCMOSTL = OCCMOSTL_CH;
	SPOCCUPA = SPOCCUPA_CH;
	SPOCCMOS = SPOCCMOS_CH;
	RUN;
	DATA RAW.IDD_converted; SET IDD2; DROP EXAMDATE ;
	INTERVIEW_DATE = EXAMDATE/86400; 
	/*
	REMOVING (PS - 6.24.15)
	IF RRID = 'BD0202' AND STUDY = 'TEN YEAR' AND INTERVIEW_DATE = '18MAR2009'd 
	THEN INTERVIEW_DATE = '17JUL2014'd; *REMOVE AFTER THIS IS CORRECTED IN ACCESS
	(PS 5.27.15);
	*/

	IDD_EXAMDATE = INTERVIEW_DATE;
	FORMAT INTERVIEW_DATE IDD_EXAMDATE DATE9.;
	RUN;
*++++++++++++++++++++++		proc freq disabled es 6/22/2017		+++++++++++++;
/*PROC FREQ noprint; RUN;*/

*KEEP CHECKING RAW.IDD_DEIDENTIFIED;
*SAME WITH SAMPLING WEIGHTS, SOME HEIGHTS, OTHER VARIABLES (FOR 5YR-10YR);

*RENAME VARIABLES***********************************************;
DATA RAW.MHXMED_converted; SET RAW.MHXMED; DROP EXAMDATE COMMENTS; 
MHX_EXAMDATE = EXAMDATE/86400; 
MEDCOMM = UPCASE(COMMENTS);
FORMAT MHX_EXAMDATE DATE9. EMERGEN YES_NOX.;
IF RRID = '' THEN DELETE;
RUN;
**********************************************************************************;
DATA RAW.SMOKEDRINK_converted; SET RAW.SMOKEDRINK; DROP EXAMDATE; 
SMOKE_EXAMDATE = EXAMDATE/86400; 

FORMAT TRIEDCIGS /*TRIEDECIG*/ TRIEDCIGAR TRIEDSNUS TRIEDSNUFF YES_NOX.
WTSCIGS WTSECIG WTSCIGR WTSSNUS WTSSNUF BFFCIGS BFFECIG BFFCIGR BFFSNUS BFFSNUF 
WBSCIGS	WBSECIG WBSCIGR  WBSSNUS  WBSSNUF BEST. /*CREATE A NEW FORMAT ASAP!*/
SMOKE_EXAMDATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;
***********************************************************************************;
DATA RAW.WELCHBP_converted; SET RAW.WELCHBP; 
DROP /*TIMEW*/ EXAMDATW EXAMINRW; 
BPWELCH_EXAMDATE = EXAMDATW/86400; 
BPWELCH_FORM = 1;

FORMAT BPWELCH_EXAMDATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;
***********************************************************************************;
DATA RAW.EXITCOVERFORM_converted; SET RAW.EXITCOVERFORM; 
DROP FASTING FBG EXAMDATE /*SCHTIME ARRTIME DEPTIME*/ 
	EXAMINER FASTDATE; 

BRACHDATE1=BRACHDATE1/86400;
BRACHDATE2=BRACHDATE2/86400;
CCADATE1=CCADATE1/86400;
CCADATE2=CCADATE2/86400;
DEXADATE1=DEXADATE1/86400;
DEXADATE2=DEXADATE2/86400;
ECHODATE1=ECHODATE1/86400;         
ECHODATE2=ECHODATE2/86400;
EXIT_EXAMDATE = EXAMDATE/86400; 
FASTDATE = FASTDATE/86400; 
FASTING_COVER = FASTING; 
FBG_COVER = FBG; 
FBGDATE=FBGDATE/86400;
LIVERDATE1=LIVERDATE1/86400;
LIVERDATE2=LIVERDATE2/86400;


FORMAT
BLDSTOR AGREE_URINE AGREE_STOOL AGRFUTRTEST YES_NOX.
FBGDATE EXIT_EXAMDATE FASTDATE 
ECHODATE1 ECHODATE2
BRACHDATE1 	BRACHDATE2
CCADATE1 	CCADATE2
LIVERDATE1 	LIVERDATE2
DEXADATE1 	DEXADATE2
DATE9.;
IF RRID = '' THEN DELETE;
RUN;
****************************************************************************************;
DATA EKG; SET RAW.EKG;
DROP ECG1-ECG6 /*TIMESUPI TIMEBIO*/ECGID edta2 edta3;
ECG_CH1=put(ECG1,z5.); *CONVERTING NUMERIC TO CHAR;
ECG_CH2=put(ECG2,z5.); *CONVERTING NUMERIC TO CHAR;
ECG_CH3=put(ECG3,z5.); *CONVERTING NUMERIC TO CHAR;
ECG_CH4=put(ECG4,z5.); *CONVERTING NUMERIC TO CHAR;
ECG_CH5=put(ECG5,z5.); *CONVERTING NUMERIC TO CHAR;
ECG_CH6=put(ECG6,z5.); *CONVERTING NUMERIC TO CHAR;
BIODATE = BIODATE/86400;
ECGDATE = ECGDATE/86400;

FORMAT BIODATE ECGDATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;

DATA RAW.EKG_converted; SET EKG; DROP ECG_CH1 - ECG_CH6;
ECG1=ECG_CH1;
ECG2=ECG_CH2;
ECG3=ECG_CH3;
ECG4=ECG_CH4;
ECG5=ECG_CH5;
ECG6=ECG_CH6;
RUN;
******************************************************;
DATA RAW.ANTH_converted; SET RAW.ANTH; 
DROP EXAMINER EXAMDATE EDTA2 EDTA3;
ANTH_EXAMDATE = EXAMDATE/86400;

FORMAT ANTH_EXAMDATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;

*MERGE UPDATE CONTACT SCHEDULING******************************************************;
DATA RAW.CONTACTSCHEDULINGPAT_converted CS1; LENGTH STUDY $30.; SET RAW.CONTACTSCHEDULINGPAT; 
DROP UP_DATE RECONCILE
/*
CTTIME1-CTTIME5   TIMEOGTT
CO1TIME CO2TIME CO3TIME CO4TIME CO5TIME CO6TIME CO7TIME 
CO8TIME CO9TIME CO10TIME CO11TIME CO12TIME CO13TIME CO14TIME*/
;
*CO1ID CO2ID CO3ID CO4ID CO5ID CO6ID CO7ID CO8ID CO9ID CO10ID CO11ID...;

*TOWN = SUBSTR(KEY7,1,2)*1; 
SUBDIV = '   ';
SECTION = ' ';
SUBDIV = SUBSTR(KEY7,13,3);
INOUT = SUBSTR(KEY7,16,1)*1;
SECTION = SUBSTR(KEY7,17,1);
DWELLING = SUBSTR(KEY7,18,3)*1;

CS_INTERVIEW_DATE = DATEOGTT/86400;

CTDATE1 = CTDATE1/86400;
CTDATE2 = CTDATE2/86400;
CTDATE3 = CTDATE3/86400;
CTDATE4 = CTDATE4/86400;
CTDATE5 = CTDATE5/86400;
DATEOGTT = DATEOGTT/86400;
CO1DATE=CO1DATE/86400;
CO2DATE=CO2DATE/86400;
CO3DATE=CO3DATE/86400;
CO4DATE=CO4DATE/86400;
CO5DATE=CO5DATE/86400;
CO6DATE=CO6DATE/86400;
CO7DATE=CO7DATE/86400;
CO8DATE=CO8DATE/86400;
CO9DATE=CO9DATE/86400;
CO10DATE=CO10DATE/86400;
CO11DATE=CO11DATE/86400;
CO12DATE=CO12DATE/86400;
CO13DATE=CO13DATE/86400;
CO14DATE=CO14DATE/86400;
NEXTDUEDATE=NEXTDUEDATE/86400;       

IF SUBSTR(RRID,1,2) IN ('BD' 'HD' 'LD');

FORMAT 
STUDY $30.
ANNUAL COHORF.
NEXTDUEDATE CTDATE1 CTDATE2 CTDATE3 CTDATE4 CTDATE5 DATEOGTT
CO1DATE CO2DATE CO3DATE CO4DATE CO5DATE CO6DATE CO7DATE CO8DATE
CO9DATE CO10DATE CO11DATE CO12DATE CO13DATE CO14DATE CS_INTERVIEW_DATE	DATE9.;
IF RRID = '' THEN DELETE;
RUN;
/*
proc freq data=cs1; table pediatric; run;
*/

/*
PROC SQL;
	CREATE TABLE CS2 AS
	SELECT * FROM CS1
	LEFT JOIN SW1.WAG AS W1
		ON CS1.RRID = W1.RRID
*/

/*	LEFT JOIN SW.cohort_2867_w (KEEP=W RRID) AS W2
		ON CS1.RRID = W2.RRID*/ /*ADD ONLY TO VISIT '1' - PS 3.26.15*/
/*	LEFT JOIN LD.deceased AS D
		ON CS1.RRID = D.RRID
;
QUIT;
*/

/*
DATA RAW.CONTACTSCHEDULINGPAT_converted; SET CS1; IF RRID = '' THEN DELETE; RUN;
*/

*****************************************************************************************;
DATA RAW.familyhx_converted; SET RAW.familyhx; DROP COMMENTS; 
FAMCOMMENTS = UPCASE(COMMENTS);
IF RRID = '' THEN DELETE;
RUN;

*****************************************************************************************;
DATA RAW.CTQ_converted; SET RAW.CTQ;
DROP CTQDOB CTQAGE CTQGENDER EXAMDATE EXAMINER; 
CTQ_EXAMDATE = EXAMDATE/86400;
CTQ_EXAMINER = EXAMINER;
FORMAT CTQ_EXAMDATE DATE9.;
IF RRID = '' THEN DELETE;
run;

*****************************************************************************************;
DATA RAW.SLES_C_converted; SET RAW.SLES_C;
DATE_SLESC=DATE_SLESC/86400;
FORMAT DATE_SLESC DATE9.;
IF RRID = '' THEN DELETE;
RUN;

*****************************************************************************************;
DATA RAW.SLES_P_converted; SET RAW.SLES_P;
DATE_SLESP=DATE_SLESP/86400;
FORMAT DATE_SLESP DATE9.;
IF RRID = '' THEN DELETE;
RUN;
*****************************************************************************************;

DATA RAW.TANR_F_converted; SET RAW.TANR_F;
TANRF_DATE=TANRF_DATE/86400;
FORMAT TANRF_DATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;
*****************************************************************************************;

DATA RAW.TANR_M_converted; SET RAW.TANR_M; 
TANRM_DATE=TANRM_DATE/86400;
FORMAT TANRM_DATE DATE9.;
IF RRID = '' THEN DELETE;
RUN;

*TEMP LAB DATA***************************************************************************;
/*
DATA LAB1; SET RAW.LAB_ALL; DROP SMOKE FASTING /*TODAYTIM EAT_TIME KEY7 KEY8 RED;
*/


DATA RAW.LAB_CONVERTED LAB1; SET RAW.LAB; DROP SMOKE FASTING /*TODAYTIM EAT_TIME*/ RED;
SMOKE_LAB = SMOKE; 
FASTING_LAB = FASTING;
LAB_EXAMDATE = TODAYDAT;
LAB_URINSAMP = URINSAMP;
LAB_URINDAT = URINDAT;
OLDPLAINRED = RED;
IF FBG1 IN (999 999.9) THEN FBG1 = .;*MOVE TO NEXT PROGRAM?;
IF FBG2 IN (999 999.9) THEN FBG2 = .;*MOVE TO NEXT PROGRAM?;
EAT_DATE = EAT_DATE/86400;
TODAYDAT = TODAYDAT/86400;
LAB_EXAMDATE = LAB_EXAMDATE/86400;
URINDAT = URINDAT/86400;
LAB_URINDAT = LAB_URINDAT/86400;

rename PEDISTUDY=HAS_PEDISTUDY
	   BASESTUDY=HAS_BASESTUDY
	   _YRSTUDY=HAS_5YRSTUDY
	   _0YRSTUDY=HAS_10YRSTUDY
	   _5YRSTUDY=HAS_15YRSTUDY
	   _0YRSTUDY0=HAS_20YRSTUDY
	   LIVSTUDY=HAS_LIVERSTUDY
	   PHGENSTUDY=HAS_PHGENSTUDY
	   COVSTUDY=HAS_COVSTUDY
	   ADTSUDY=HAS_ADTSUDY;
LABEL 
FASTING_LAB = '4. Is person at least 10 hours fasting (except water)? (Lab Form)'
/*HIDING THIS LABEL?*/
SMOKE_LAB = 'HHRC Q5 DID YOU SMOKE OR USE TOBACCO IN THE LAST 10 HOURS?';
FORMAT 
LAB_EXAMDATE EAT_DATE TODAYDAT URINDAT LAB_URINDAT DATE9.
EDTA1 EDTA2 EDTA3 EDTA4 RNA TUBEMUST PLAINRED OLDPLAINRED EDTAX.
URINSAMP LAB_URINSAMP FASTING_LAB SMOKE_LAB YES_NOX.
PEDISTUDY BASESTUDY _YRSTUDY _0YRSTUDY _5YRSTUDY 
_0YRSTUDY0 LIVSTUDY PHGENSTUDY COVSTUDY ADTSUDY YES_NOX.;
IF RRID = '' THEN DELETE;
drop visit;
PROC SORT; BY LABID;
RUN; 


*MERGE LAB DATA**************************************************************************;
DATA FULL_INSULIN; SET LIBINS1.final_insulin; RUN;
/************************
DATA FULL_INSULIN; SET LIBINS1.final_ins_newcchc 
						LIBINS2.ins5yr_all LIBINS3.ins_drs_all LIBINS4.ins10yr_all; 
RUN;
*************************/

/********************  REMOVE DRS IZ 10052017 *************************************************

DATA FULL_HBA1C; SET S8GHB1.final_ghb_NEWCCHC S8GHB2.final_ghb_5yr S8GHB3.final_ghb_drs; 
S8_GHB = GHB; DROP GHB GHB_DS; RUN;

***********************************************************************************************/

DATA FULL_HBA1C; SET S8GHB1.final_ghb_NEWCCHC S8GHB2.final_ghb_5yr S8GHB3.final_ghb_drs; 
S8_GHB = GHB; DROP GHB GHB_DS; RUN;

/*
data mycrldata;
set raw.crldata raw.crl_other_all;
if labid = '' then delete;
run;

proc sort data = mycrldata out = crldata_all nodupkey; by labid; run;

data oldcrldata_all;
set raw.crldata_all;
if labid = '' then delete;
run;

data crldata_all;
set raw.crldata_all;
run;

proc sort data = crldata_all; by labid; run;
proc sort data = labcrl1; by labid; run;

data mycrldata_all;
merge labcrl1 (in=a) crldata_all (in=b);
by labid;
run;

proc sort data = mycrldata_all out = newcrldata_all nodupkey; by labid; run;

data raw.crldata_all;
set newcrldata_all;
run;

data final_urine;
set URINE.FINAL_URINE;
run;

data labcrlisrael;
set labcrl1;
run;

************************************

data crldata_all;
set raw.crl_other_all;
run;

proc sort data = crldata_all; by labid; run;
proc sort data = lab1; by labid; run;

data mycrldata_all;
merge lab1 (in=a) crldata_all (in=b);
by labid;
run;

proc sort data = mycrldata_all out = newcrldata_all nodupkey; by labid; run;

data raw.crldata_all;
set newcrldata_all;
run;
*/


PROC SQL noprint;	/*Added noprint ES 6/22/2017*/
*MERGE CRLDATA***************************************************************************;
/*
    CREATE TABLE LABCRL1 AS
	SELECT * FROM LAB1 AS L
	LEFT JOIN RAW.CRLDATA_ALL AS C
	ON L.LABID = C.LABID
*/
	CREATE TABLE LABCRL1 AS
	SELECT * FROM RAW.CRL_DATA_ALL AS L
	/*MERGE INSULIN DATA (7/17/13)***********************************************************/
	LEFT JOIN FULL_INSULIN AS i 
	ON L.LABID = i.LABID
/*MERGE BNP******************************************************************************/
	LEFT JOIN BNPL.bnp_all AS BNP1
	ON L.LABID = BNP1.LABID
/*MERGE cIMT*****************************************************************************/
	LEFT JOIN IMT.cimt_ALL /*(DROP=RRID)*/ AS CIMT 
	ON L.LABID = CIMT.LABID
/*MERGE CK18 DATA************************************************************************/
	LEFT JOIN LIBCK18.CK18_ALL AS CK18
	ON L.LABID = CK18.LABID
/*MERGE HBA1C DATA***********************************************************************/
	LEFT JOIN FULL_HBA1C /*S8GHB.final_ghb_NEWCCHC*/ AS GHBALL
	ON L.LABID = GHBALL.LABID
/*MERGE sRAGE DATA***********************************************************************/
	LEFT JOIN sRAGEp.srage_ALL /*(DROP=YEAR_sRAGE)*/ AS SRAGE 
	ON L.LABID = SRAGE.LABID
/*MERGE MICROARRAY DATA******************************************************************/
	LEFT JOIN MICROARp.participants_microarrays (KEEP=LABID MICROARRAY) AS MA 
	ON L.LABID = MA.LABID
/*MERGE TROPONIN T DATA******************************************************************/
	LEFT JOIN TRO.troponin_t_all AS TROP
	ON L.LABID = TROP.LABID
/*MERGE URINE (07/12/13)*****************************************************************/
	LEFT JOIN URINE.FINAL_URINE AS URN /*WHAT IS THIS DATA?*/
	ON L.LABID = URN.LABID
/*MERGE ECHO DATA HERE (06/11/2014)******************************************************/
	LEFT JOIN ECHO.final_echo_results AS ECHO1 
	ON L.LABID = ECHO1.LABID
/*MERGE CYTOKINES DATA (LUMINEX) (08.01.13)*(PS UPDATE 10.16.13)*************************/
	LEFT JOIN LUMINEX.LUMINEX_FINAL AS L1
		ON L.LABID = L1.LABID
/************************************* Combined them all into one dataset (IZ 08.11.2016)

        LEFT JOIN LUMINEX.LUMINEX_ADP AS L1
		ON L.LABID = L1.LABID
	LEFT JOIN LUMINEX.LUMINEX_IL1B AS L2
		ON L.LABID = L2.LABID
	LEFT JOIN LUMINEX.LUMINEX_IL6 AS L3
		ON L.LABID = L3.LABID
	LEFT JOIN LUMINEX.LUMINEX_IL8 AS L4
		ON L.LABID = L4.LABID
	LEFT JOIN LUMINEX.LUMINEX_LEP AS L5
		ON L.LABID = L5.LABID
	LEFT JOIN LUMINEX.LUMINEX_RES AS L6
		ON L.LABID = L6.LABID
	LEFT JOIN LUMINEX.LUMINEX_TNFA AS L7
		ON L.LABID = L7.LABID
***************************************************/
/*MERGE LUMINEX ADJUSTED VALUES(FROM UTHSCSA)(03.23.15-PS)-DO NOT MERGE IN THIS PROGRAM YET**
	LEFT JOIN LUMIN2.LUMINEX_ADJUSTED AS L8
		ON L.LABID = L8.LABID*/
	ORDER BY LABID
;
QUIT;

*******************************************************************************;
PROC SORT DATA=LABCRL1 OUT=LAB_FINAL_SO_FAR; BY LABID; RUN;

*CONVERT FORMATS ON DATE VARIABLES*********************************************;

*%LET ORDER=BDVISIT RRID LABID VISIT GHB INS CK18 SRAGE DATE_SRAGE MICROARRAY
			IL_1BETA IL_6 IL_8 LEPTIN TNF_ALFA;
%LET ORDER=RRID LABID GHB INS CK18 SRAGE DATE_SRAGE MICROARRAY
			IL_1BETA IL_6 IL_8 LEPTIN TNF_ALFA;

/*(RENAME=(DATE_FBG3=DATE_FBG_CRL))*/
DATA RAW.LAB_WITH_CRL LABS_WITH_CRL /*CRL1*/; RETAIN &ORDER; SET LAB_FINAL_SO_FAR; 

IF GHB = . THEN GHB = S8_GHB; *NEW CODE BY PS 3.13.15;

IF FBG_CRL = . THEN FBG_CRL = FBG3;
IF DATE_FBG_CRL = . THEN DATE_FBG_CRL = DATE_CRL;
IF DATE_FBG3 = . THEN DATE_FBG3 = DATE_CRL;

IF URNCOLLECTED NE "" AND URNCOLLECTED = "YES" THEN CRL_URINSAMP = 1;
ELSE IF URNCOLLECTED NE "" AND URNCOLLECTED NE "YES" THEN CRL_URINSAMP = 2;

DROP STUDYCRL CRL_Time S8_GHB URNCOLLECTED;
DATE_FBG_CRL=DATE_FBG_CRL/86400;
DATE_ALK   =DATE_ALK   /86400;
DATE_BUN   =DATE_BUN   /86400;
DATE_CALC  =DATE_CALC  /86400;
DATE_CHL   =DATE_CHL   /86400;
DATE_CO2   =DATE_CO2   /86400;
DATE_DLDL  =DATE_DLDL  /86400;
DATE_GFR   =DATE_GFR   /86400;
DATE_GHB   =DATE_GHB   /86400;
DATE_MCGLUC=DATE_MCGLUC/86400;
DATE_POT   =DATE_POT   /86400;
DATE_SOD   =DATE_SOD   /86400;
DATE_FBG3=DATE_FBG3/86400;
DATE_CRL=DATE_CRL/86400;
DATE_CREA=DATE_CREA/86400;
DATE_CRP=DATE_CRP/86400;
DATE_HDLC=DATE_HDLC/86400;
DATE_URIC=DATE_URIC/86400;
DATE_ALP=DATE_ALP/86400;
DATE_CHLR=DATE_CHLR/86400;
DATE_CHOL1=DATE_CHOL1/86400;
DATE_DBIL=DATE_DBIL/86400;
DATE_GOT=DATE_GOT/86400;
DATE_GPT=DATE_GPT/86400;
DATE_LALB=DATE_LALB/86400;
DATE_TBIL=DATE_TBIL/86400;
DATE_TP=DATE_TP/86400;
DATE_TRIG=DATE_TRIG/86400;
DATE_LDLCALC=DATE_LDLCALC/86400;

H_DATE = H_DATE/86400; 

FORMAT 
DATE_ALK DATE_BUN DATE_CALC DATE_CHL DATE_CO2 DATE_DLDL DATE_FBG_CRL DATE_GFR   
DATE_GHB DATE_MCGLUC DATE_POT DATE_SOD DATE_FBG3 DATE_FBG_CRL URINDAT
DATE_CREA DATE_CRP DATE_HDLC DATE_URIC DATE_ALP DATE_CHLR DATE_CHOL1 DATE_DBIL 
DATE_GOT DATE_GPT DATE_LALB DATE_TBIL DATE_TP DATE_TRIG DATE_LDLCALC DATE_CRL
H_DATE
DATE9.;

FORMAT CRL_URINSAMP YES_NOX.;

LABEL
CIMT_R_ANT = "R ANT r-wave"
CIMT_R_LAT = "R LAT r-wave"
CIMT_R_POST = "R POST r-wave"
CIMT_L_ANT = "L ANT r-wave"
CIMT_L_LAT = "L LAT r-wave"
CIMT_L_POST = "L POST r-wave"
CIMT_SMOKER = "Did the participant say they were a smoker? (at cIMT)"
CIMT_AGE = "Age (at cIMT)"
CIMT_PLAQUE = "Plaque? (at cIMT)"
;
drop visit bdvisit;
RUN;

/*
PROC FREQ; TABLE EDTA1*EDTA2*RRID*LABID / LIST MISSING; WHERE STUDY = 'TEN YEAR'; RUN;

PROC FREQ DATA=CS1; TABLE STUDY*PEDIATRIC / LIST MISSING; RUN;

PROC FREQ DATA=RAW.LAB_WITH_CRL; TABLE FBG_CRL; RUN;

*RUN NEXT PROGRAM;

*NEW LABELS MAY NOT APPEAR IN FINAL DATA
WARNING: Variable BDVISIT already exists on file WORK.LABCRL1.
WARNING: Variable RRID already exists on file WORK.LABCRL1.
WARNING: Variable VISIT already exists on file WORK.LABCRL1.
WARNING: Variable URNcollected already exists on file WORK.LABCRL1.

/*
NOTE: The data set RAW.LAB_WITH_CRL has 3700 observations and 222 variables.
NOTE: The data set RAW.LAB_WITH_CRL has 3851 observations and 215 variables. 3.27.15
NOTE: The data set RAW.LAB_WITH_CRL has 6107 observations and 215 variables. 3.31.15
NOTE: The data set RAW.LAB_WITH_CRL has 6107 observations and 216 variables. 4.1.15
NOTE: The data set RAW.LAB_WITH_CRL has 6124 observations and 216 variables. 4.8.15
NOTE: The data set RAW.LAB_WITH_CRL has 6128 observations and 216 variables. 4.9.15
NOTE: The data set RAW.LAB_WITH_CRL has 6138 observations and 216 variables. 4.24.15
NOTE: The data set RAW.LAB_WITH_CRL has 6141 observations and 216 variables. 4.28.15
NOTE: The data set RAW.LAB_WITH_CRL has 6577 observations and 217 variables. 4.28.15
NOTE: The data set RAW.LAB_WITH_CRL has 6577 observations and 218 variables. 4.29.15
NOTE: The data set RAW.LAB_WITH_CRL has 6586 observations and 218 variables. 5.5.15
NOTE: The data set RAW.LAB_WITH_CRL has 6591 observations and 218 variables. 5.8.15
NOTE: The data set RAW.LAB_WITH_CRL has 6607 observations and 218 variables. 5.13.15
NOTE: The data set RAW.LAB_WITH_CRL has 6632 observations and 217 variables. 5.22.15
NOTE: The data set RAW.LAB_WITH_CRL has 6644 observations and 217 variables. 5.28.15
NOTE: The data set RAW.LAB_WITH_CRL has 6660 observations and 217 variables. 6.5.15
NOTE: The data set RAW.LAB_WITH_CRL has 6701 observations and 217 variables. 6.24.15
NOTE: The data set RAW.LAB_WITH_CRL has 6705 observations and 217 variables. 6.29.15
