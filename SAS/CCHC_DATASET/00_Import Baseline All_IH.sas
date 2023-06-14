/********************************************************************************
---------------------------------------------------------------------------------
|Program Name: Import Baseline All.sas											|
|Date Created: November 30, 2015												|
|Author: Emmanuel Santillana, Hugo Soriano													|
|Purpose: Run all baseline import sequencially									|
|Inputs: 01A-COPY CCHC SAS DATA AFTER RECON-FROM ACCESS.sas						|
|		02-IDENTIFY PTS IN OTHER STUDIES UPDATED.sas							|
|		03-ADD DRS DATA SETS.sas												|
|		04-CONVERT DATA SETS+MERGE LAB DATA.sas									|
|		04-01 -Add new Tables A-Z.sas											|
|		05-CREATE DATASET-NEWVISITS.sas											|
|		06-CONVERT CS-IDD-ANTHRO-MEDS+MERGE 13 TABLES.sas						|		
|		07-Clearing_databaseCCHC.sas											|
|		09-COMPARE ALL-VARIABLE NAMES-FORMATS-LABELS+RRIDS+DATA.sas				|
|Outputs: Process many tables.													|
|-------------------------------------------------------------------------------|
|Macros: Sleep, call other programs												|
|-------------------------------------------------------------------------------|
|Notes: Excecute this program, this will import and clean sas tables from access|
|																				|
|																				|
|-------------------------------------------------------------------------------|
|Date Modified:07/11/16															|
|Modified by: Hugo Soriano												|
|Reason for Modification: Have a better overview of the results					|
|Description of Modification: Added as comments the final size of Base_Old_Ydrive
|							  and Compare_New_S8								|
|-------------------------------------------------------------------------------|
|Date Modified:7/26/16 															|
|Modified by:Hugo Soriano												|
|Reason for Modification: Ease of updating										|
|Description of Modification: Move all the libraries into this file to updtade	|
|							  all of them instead of each file					|
|-------------------------------------------------------------------------------|
|Date Modified:																	|
|Modified by:																	|
|Reason for Modification:														|
|Description of Modification:													|
|-------------------------------------------------------------------------------|
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%		LIBRARIES		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
%let path = \\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core;
%let pathuthscsa = \\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\UTHSCSA_CRUs;

*SAS files location ;
%let path1 = \\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\FINAL DATA\BASELINE;

*waiting time in seconds before each program run;
%let x=2;	
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 01			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
LIBNAME LIBRARY "\\uthouston.edu\uthsc\SPH\Research\Studies Data\SAS Formats";
LIBNAME FL "&path\MSAccess\TempData";
LIBNAME BRO "&path\MSAccess\TEMPDATA\NEWCCHC";
LIBNAME HAR "&path\MSAccess\TEMPDATA\HARLINGEN\BASELINE";
LIBNAME LAR "&path\MSAccess\TEMPDATA\LAREDO\BASELINE";
libname RAW "&path\MSAccess\FINAL DATA\BASELINE\DATABASE TABLES SAS\PACKETS";

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 02			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts:LIBRARY;
LIBNAME FINAL "&path\MSAccess\FINAL DATA\BASELINE";
LIBNAME OB "&path\MSAccess\FINAL DATA\OB";
LIBNAME CAB "&path\MSAccess\FINAL DATA\CAB";
LIBNAME DRS "&path\MSAccess\FINAL DATA\DRS";
LIBNAME BRO1 "&path\MSAccess\FINAL DATA";
LIBNAME PACE "&path\MSAccess\FINAL DATA";

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 03			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts: RAW;
LIBNAME DRS1 "&path\MSAccess\TEMPDATA\DRS";
LIBNAME CRL_FL "&path\LABORATORY_RAHC\CRL_DATA\FINAL DATA";

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 04			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts:LIBRARY, RAW;

*LIBNAME PACEP '\\uctnascifs.uthouston.edu\sph\Research\BReininger2\PACE\RECON\ACCESS2010\DATA\FINAL DATA\ALL DATA'; /*No longer used 5/11/18*/
*LIBNAME OBLIB '\\uctnascifs.uthouston.edu\sph\Research\Studies Data\OB_Study'; /*No longer used 5/11/18*/
LIBNAME S8GHB1 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\NEWCCHC";
LIBNAME S8GHB2 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\5YRS";
LIBNAME S8GHB3 "&path\LABORATORY_Multiple\HBA1C\9-FINAL DATA\DRS";
LIBNAME LIBINS1 "&path\LABORATORY_RAHC\INSULIN\New_Insulin\";
/************    Merged Insulin data into one dataset.           IH 10/13/2015
LIBNAME LIBINS1 "&path\LABORATORY_RAHC\INSULIN\NEWCCHC\COMBINED DATA";
LIBNAME LIBINS2 "&path\LABORATORY_RAHC\INSULIN\5YR\COMBINED DATA";
LIBNAME LIBINS3 "&path\LABORATORY_RAHC\INSULIN\DRS\COMBINED DATA";
LIBNAME LIBINS4 "&path\LABORATORY_RAHC\INSULIN\10YR\COMBINED DATA";
*************/
LIBNAME LIBCK18 "&path\LABORATORY_RAHC\CYTOKERATIN-18\FINAL DATA";
libname SRAGEp "&path\Laboratory_specimens_sent_to otherLab\sRageTest\COMBINED DATA";
libname MICROARp "&path\Microarray"; *0 RECORDS FOR OLD/NEW CCHC";
LIBNAME BNPL "&path\LABORATORY_RAHC\BNP\FINAL DATA\COMBINED";
LIBNAME IMT "&path\ULTRASOUND DATA\cIMT\FINAL DATA\";
LIBNAME TRO "&path\LABORATORY_RAHC\TROPONIN_T\COMBINED DATA";
LIBNAME URINE "&path\LABORATORY_RAHC\URINE";
LIBNAME LUMINEX "&path\LABORATORY_RAHC\LUMINEX\FINAL DATA\COMBINED";
LIBNAME ECHO "&path\MSAccess\FINAL DATA\ULTRASOUND DATA\ECHO"; *UPDATED 2.14.2018;
*LIBNAME ECHO "&path\ULTRASOUND DATA\ECHOS\FINAL DATA"; *UPDATED 8.27.14;
*LIBNAME ECHO_L "&path\MSAccess\FINAL DATA\ULTRASOUND DATA\ECHO"; *UPDATED 8.27.14;
LIBNAME LUMIN2 "&path\Laboratory_specimens_sent_to otherLab\UTHSCSA\LUMINEX\FINAL DATA";
/********************* Remove old sampling weights calculation   IH 06.02.2017
LIBNAME SW1 "&path\Sampling_Weights\Weight_age_gender";
LIBNAME SW "&path\Sampling_Weights\w";
**********************/
LIBNAME SW1 "&path\Sampling_Weights\Weight_age_gender";
LIBNAME SW "&path\Sampling_Weights\w";
LIBNAME tmpw8ts "&path\msaccess\test_pgms\sample weights program";*WEIGHTS;

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 05			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts:LIBRARY, PACE, RAW, HH;

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$			PART 06			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts:LIBRARY, KVWGTp, RAW, FINAL, SW;
libname MEDICP "&path\MEDICATIONS";
libname IMPACTp "&path\BD Study\Data\SAS_Data\Clean"; *PARTICIPANTS_MEDICATIONS;
LIBNAME HH "&path\MSAccess\FINAL DATA\HH";
libname LDOB "&path\MSAccess\FINAL DATA\DOB";
LIBNAME HCVL "&path\LABORATORY_RAHC\HCV";
LIBNAME INV "&path\Lab_Inventory\COHORT_BROWNSVILLE\FINAL_DATA";
LIBNAME ECGXI "&path\Laboratory_specimens_sent_to otherLab\ECG\FINAL DATA";
LIBNAME HBV "&path\LABORATORY_RAHC\HBsAg\FINAL DATA";
LIBNAME LIVER "&path\MSAccess\FINAL DATA\ULTRASOUND DATA\LIVER";
LIBNAME LIVER2 "&path\ULTRASOUND DATA\LIVER\FINAL DATA";
LIBNAME EUSFNL "&path\MSAccess\FINAL DATA\ULTRASOUND DATA\ELASTOGRAPHY";*Liver Elastography Final SAS Dataset;
LIBNAME FIBRSCN "&path\MSAccess\FINAL DATA\ULTRASOUND DATA\FIBROSCAN";*Liver Fibroscan Final SAS Dataset;
LIBNAME HEV "&path\LABORATORY_RAHC\HEV\FINAL DATA";
*tracts_blocks_ses1234;
libname SES_BRO "&path\New_CCHC\GIS_sampling\Tracts_blocks_2nd_4th_quartiles"; 
*diab_ss01;
libname SES_HAR "&pathuthscsa\Harlingen CRU\HARLINGEN_COHORT\Harlingen Final";
*diab_ss01; 
libname SES_LAR "&pathuthscsa\LAREDO CRU\LAREDO_COHORT\"; 
libname COV_LIB "\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid"; 
libname SYV_LIB "\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid"; 

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$			PART 07			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts:LIBRARY, IMPACTp, HH, RAW, FINAL, SW, HCVL,
								INV, ECGXI, HBV, LIVER, HEV, LUMIN2;
LIBNAME LD "&PATH\DECEASED\COMBINED"; *DECEASED EXISTS IN PREVIOUS PROGRAM;
/* CAB LIBRARY IS ALREADY DEFINED ABOVE              IZ 10/11/2017
LIBNAME LCAB2 "&PATH\MSAccess\FINAL DATA\CAB";
*/
LIBNAME HRLPDCAB "&PATH\MSAccess\TEMPDATA\HARLINGEN\BASELINE";
LIBNAME LRDPDCAB "&PATH\MSAccess\TEMPDATA\LAREDO\BASELINE";
LIBNAME DXA "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\DXA";
LIBNAME DXA_CMT "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\DXA\COMMENTS";
LIBNAME PADABI "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\PAD_ABI";
LIBNAME GER_BT "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\BONE_TURNOVER";
LIBNAME MYADDR "&PATH\MSAccess\FINAL DATA\addressbook-phonebook";
LIBNAME MEDIC "&PATH\BD Study\Medicines\Separating_medicines_bytype";
LIBNAME PD_BMI "&PATH\MSAccess\TEMPDATA\PEDIATRIC_BMI";
LIBNAME NDI_Data "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\NDI_DEATH";
LIBNAME GAD65Ab "&PATH\msaccess\final data\gad65ab_results";
LIBNAME FMDLIB "&PATH\ultrasound data\cchc_brachials_imt\combined data";
LIBNAME RETINA "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\RETINA";
LIBNAME Ancestry "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\ANCESTRY_PROPORTION";
LIBNAME EKGWRK "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\EKG";
LIBNAME Brachial "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\Brachial";
LIBNAME TAKEDA "&PATH\LABORATORY_RAHC\TAKEDA\FINAL FILES";
LIBNAME NTD "&PATH\LABORATORY_RAHC\NTD";
*LIBNAME COVID "&PATH\LABORATORY_RAHC\COVID";
LIBNAME PAXGENE "&PATH\LABORATORY_RAHC\INVENTORY\FINAL DATA";
LIBNAME ISCHEMIA "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\ISCHEMIA";
LIBNAME BILE "&PATH\LABORATORY_RAHC\BILE";
LIBNAME SLEEP "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\Questioneers\PIT_SLEEPING";
LIBNAME COV_FG "&PATH\MSAccess\FINAL DATA\ULTRASOUND DATA\Questioneers\COVID";
LIBNAME COV_NCP "&PATH\LABORATORY_RAHC\COVID_NCP";
LIBNAME CPEPT "&PATH\LABORATORY_RAHC\C-peptide";
LIBNAME SEQDATA "&PATH\LABORATORY_RAHC\SEQ_INVENTORY";
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$			PART 09			$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
*Libraries used in other parts: LIBRARY,FINAL;
LIBNAME OLDY "\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC"; *CCHC;
******************************************************************************;

%macro sleep(x);
data _null_;
	vVar1=SLEEP(&x);
run;
%mend;


/**********************************************************************
*************************************************************************
part 1
***********************************************************************
**********************************************************************/
%include "&path1\01A-COPY CCHC SAS DATA AFTER RECON-FROM ACCESS.sas";
%sleep(&x);
/**********************************************************************
*************************************************************************
part 2
***********************************************************************
**********************************************************************/
%include "&path1\02-IDENTIFY PTS IN OTHER STUDIES UPDATED.sas";
%sleep(&x);
/**********************************************************************
*************************************************************************
part 3
***********************************************************************
**********************************************************************/

%include "&path1\03-ADD DRS DATA SETS_HS.sas";
%sleep(&x);

/**********************************************************************
*************************************************************************
part 4
***********************************************************************
**********************************************************************/
/*
%include "&path1\04-CONVERT DATA SETS+MERGE LAB DATA.sas";
*/

%include "&path1\04-CONVERT DATA SETS_MERGE LAB IZ.sas";
%sleep(&x);
/**********************************************************************
*************************************************************************
part 4.5
***********************************************************************
**********************************************************************/
%include "&path1\04-01 Add new Tables A-Z.sas";
%sleep(&x);
/**********************************************************************
*************************************************************************
part 5
***********************************************************************
**********************************************************************/
%include "&path1\06-CONVERT CS-IDD-ANTHRO-MEDS+MERGE 13 TABLES_IZ.sas";
/*
%include "&path1\05-CREATE DATASET-NEWVISITS.sas";
*/
%sleep(&x);
/**********************************************************************
*************************************************************************
part 6
***********************************************************************
**********************************************************************/
%include "&path1\05-CREATE DATASET-NEWVISITS_HS.sas";
/*
%include "&path1\06-CONVERT CS-IDD-ANTHRO-MEDS+MERGE 13 TABLES.sas";
*/
%sleep(&x);
/**********************************************************************
*************************************************************************
part 7
***********************************************************************
**********************************************************************/
%include "&path1\07-Clearing_databaseCCHC_HS.sas";
%sleep(&x);
/**********************************************************************
*************************************************************************
part 8 (LABELS)
***********************************************************************
**********************************************************************/
%include "&path\MSAccess\TEST_PGMS\All Labels\New_Labels_Program.sas";
*%sleep(&x);
/**********************************************************************
*************************************************************************
part 9 (WEIGHTS) points to final_cchc17
***********************************************************************
**********************************************************************/
%include "&path\MSAccess\TEST_PGMS\Sample Weights Program\June 2 2017(All in one)\Files\Process Data_IZ.sas";
*%sleep(&x);
/**********************************************************************
*************************************************************************
part 9.1 
***********************************************************************
**********************************************************************/
%include "&path1\08_Labels_Format_Rename.sas";
*%sleep(&x);
/**********************************************************************
*************************************************************************
part 10 (COMPARING)
***********************************************************************
**********************************************************************/
%include "&path1\09-COMPARE ALL-VARIABLE NAMES-FORMATS-LABELS+RRIDS+DATA.sas";

