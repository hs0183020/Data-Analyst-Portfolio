/************************************************ HEADER ********************************************************************
	* TITLE: LTCH_Asmt_
	
	* AUTHOR: Elizabeth Oliver (EO)

	* PURPOSE:	This program is the most current and production version used for APU and Outreach reporting.
				- It combines assessments for the current quarter using extracts from history table and current assessments.

	* INPUTS:	There are multiple inputs
			-	current quarter Assessment extract 
			-	current quarter History extract (for LTCH this does not get used but we import and check file)
			-	current quarter Provider extract
			-	previous quarter assembled file (for APU reporting only)
			-	Control file (Excel)

	* NOTES:	To update this file review the macro variable definitions. 
                Quarterly updates: inputs and their directories, date, and output file naming parameters 
				The schema, database and libname references can be modified as needed.
				
				This code has the following main parts: 
				- 	Parameter definition and libname references via macro variables 
				-	QA extracts before assembly 
				-	Extract assembly:	1) Combine historical and current assessments with a quarter
										2) Combine quarters, if creating a YTD file
				-	QA final combined assessments
				-	Write final combined assessments to Workbench

	* UPDATES: 
	05-AUG-2021 (PP):	Adapted from 2021-Q1 Outreach program (_2021713_PP)
		- added a indicator for quarter

	09-AUG-2021 (PP): applied post-QA edits made to IRF
		-	no longer using PROC DATASETS to create _nodup datasets
		-	using gate %IF/%THEN to assign datasets to &XXXX_nodup macros
		-	shored up naming of intermediate datasets
		- 	fixed flipped data element rules
			-	confirmed that this does not change results, no new output run

	20-AUG-2021 (PP): Production
		- updated inputs, added some notes to log for the inputs

	06-OCT-2021 (EO): Development
		-	updated header to reflect change in principal programmer and cycle
		- 	updated inputs and control flags

	18-OCT-2021 (EO): Production - 2021-Q2 Outreach
		-	updated input files for extracts (asmt and prvdr)
		-	moved all code for notes to log when control flags do not trigger code to the top of chunks (per Alex)

	09-NOV-2021 (EO): development - 2021-Q2 APU
		-	made header generic for ease of maintenance
		-	inputs and parameters updated for apu reporting
		-	added signposting of sections to log
		-	changed a few titles or comments
		-	cleaned up some commenting styles within macro functions
		-	added row count check to final file, grabbed from IRF

	19-NOV-2021 (EO): production - 2021-Q2 APU
		-	input changes


	27-DEC-2021 (Chid): pre-development - 2021-Q3 Outreach	
		-	input changes and control flag settings for outreach	
		-	cleaned up commenting
		-	added blank title statements where needed	
	
	06-JAN-2022 (EO): development - 2021-Q3 OR
		-	development inputs

	19-JAN-2022 (EO): production - 2021-Q3 OR
		-	production inputs

	07-FEB-2022 (EO): development - 2021-Q3 APU
		-	updated parameters and paths
		-	moved control table processing to top of program - after parameters
		-	streamlined qa checks at beginning to use control table to check variable type
		-	changes in top 275 lines or so based on changes in IRF

	18-FEB-2022 (EO): production q3 apu
		-	production inputs
		
	3/8/2022 KAS: Pre-dev 2021-Q4 OR
		-	Updated inputs and parameters
		-	Streamlined parameter updating
		-	Element start and end dates in control tables are now saved as dates, no conversion needed

	04-05-2022 (KAB): Development - 2021-Q4 OR
		-	Updated inputs

	04-19-2022 (EO): production - 2021-Q4 OR
		-	Updated inputs
		
	5/6/2022 KAS: Development 2021-Q4 APU
		
		- 	Updated parameters
		-	Removed reference to history file since we aren't using it anymore
		-	Reduced QA that is now redundant with extract
		-	QA clarification
		- 	Pulled past Q deduplication macro out -- it now lives in Assemble_Assessments_Common
		
	5/11/2022 KAS
		- Re-ordered CCN backfill so that it happens BEFORE deduplication. CCN is required for this stage -- we just never noticed
		before because there are rarely if ever any dups for LTCH.
	5/16/2022 EO
		- wrote call to common extract macro instead of program defined one
	5/25/2022 EO
		- production inputs
		- added a preimport QA to monitor duplicate asmt ids in extract

	07/07/2022 (Chid)
		- 2021 Q1 OR development run
		
	07/19/2022 EO
		- production inputs

	08/05/2022 EO 2022-Q1 APU development
		-	rearranged parameters to put common macro inclusion first
		-	expanded use of common macro assignements
		-	added notes to log in parameter section for when and what to update
		-	added nomprint option to bottom of code
		-	expanded parameters summary
		-	cleared control table import to use default assigned in common macro
	08/18/2022 EO production inputs

	10/11/2022 (CT): Development run for 2022-Q2 OR
			-	added a check to monitor assessments with duplicate orgnl_asmt_id
					and missing correction numbers (crctn_num)
	10/12/2022 (EO): development
			-	change to import macro
				rerun without new output
				proc compare in sep program will verify exactness of method

	10/19/2022 (CT): Production run for 2022-Q2 OR

	11/04/2022 (CT): Development run for 2022 Q2 APU

	11/22/2022 (EO): prod run

    01/05/2023 (SS): Development run for 2022-Q3 OR

    01/18/2023 (SS): Production run for 2022-Q3 OR
    
	2/7/2023 (KAS): Dev run for 2022-Q3 APU
		- Added mprint/mlogic option  

	02/22/2023 (CT): Production run for 2022-Q3 APU 

	03/27/2023 (SS): Development run for 2022-Q4 OR 

	04/20/2023 (CT): Production run for 2022-Q4 OR

	05/08/2023 (CT): Development run for 2022-Q4 APU

	05/22/2023 (SS): Production run for 2022-Q4 APU

	07/14/2023 (CT): Development run for 2023-Q1 OR

	07/19/2023 (CT): Production run for 2023-Q1 OR

	08/16/2023 (CT): Development run for 2023-Q1 APU

	08/18/2023 (RJ): Production run for 2023-Q1 APU

	10/19/2023 (CT): Production run for 2023-Q2 OR

	11/09/2023 (CT): Development run for 2023-Q2 APU

	11/28/2023 (SS): Production run for 2023-Q2 APU

	01/09/2024 (SS): Development run for 2023-Q3 OR
	- Updated current Assessment extract name to include DBX.

	01/17/2024 (SS): Production run for 2023-Q3 OR
	- Commented out QA check "Assessments with duplicate orginal assessment id and missing correction numbers".
	01/18/2024 (EO): prod rerun for 2023-Q3 OR
					- new control table on bench for accomodating LTCH assessment additional vairable
					- added code to step that combines all current assessments
						this filters out any remaining I, X assessments
					- added QA just after
			
	02/13/2024 (SS): Development run for 2023-Q3 APU

	03/11/2024 (SS): Production run for 2023-Q3 APU

	03/22/2024 (HS): Production Re-run for 2023-Q3 APU
					- using provider re-run

	04/12/2024 (SS): Development run for 2023-Q4 OR

	04/16/2024 (EO): prod 2023-Q4 OR

	04/24/2024 (SS): pre dev 2023-Q4 APU
		- prepped a cummulative asmt file for use in
			an predev APU to send to acumen
		- Some COMMON are still pointing to OR folders, some are APU
		- ALL data input is still pointing to OR folders
		- data output is pointing to APU folders

	05/10/2024 (HS): dev 2023-Q4 APU
		- added code to assign the correct CCN to El Paso assessments for Q1-Q3
		- added QA code to verify the assessments were corrected after CCN assignment

********************************************* END HEADER ********************************************************************/

%put NOTE: END header / START parameters ; 

/******************************************* PROGRAM PARAMETERS ***************************************/
* common macro set up ; 
%put NOTE: update date each reporting cycle - common file for report related assignments ; 
	* for control table import and ccn pattern ; 
	%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";

%put NOTE: update date each reporting cycle - common file for assessment related functions ; 
	%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Extract and Assemble Assessments/Programs/Assemble_Assessments_Common_20240509.sas";


%put NOTE: folder updated automatically from APU common macros each reporting cycle ; 

* reporting cycle folder populated from APU common macros- 
			;
	%LET folder = &cmn_folder;
    %put NOTE: &=folder ;

%put NOTE: paths should not normally need updating ; 

* database/schema/caslib references - update as needed ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data ;
	%LET benchqrp = &bench/SAS/Quarterly and Annual Reporting ;
	%LET asmt_dir = Extract and Assemble Assessments/Data ;
	%LET prvdr_dir = Extract and Assemble Providers/Data ;


* Static names ;
	%LET setting = LTCH ;
	%LET asmt_setting = LTCH ;

%put NOTE: ytd assembly - update each reporting cycle ;
* Determine type of assembly and report type ;
	* YTD (=1) or current quarter only (=0) ;
	%LET YTD = 1 ;
	* APU (=1) or Outreach (=O) ;
	%LET APU = 1 ;

%put NOTE: Naming parameters - will auto update in this chunk ; 

	%LET year = &cmn_yy ;
	%put NOTE: &=year ; 
	%LET quarter = &cmn_qq ;
	%put NOTE: &=quarter ; 

%put NOTE: Naming parameters - update each reporting cycle;

  	%LET prev_year = 23 ;
	%LET prev_quarter = Q3 ;

	
%put NOTE: auto updates -  	This should be the previous APU folder	;
	%LET prev_folder = 20&prev_year.-&prev_quarter APU ;

* Establish librefs ;

%put NOTE: Session options and LIBREFs - update as needed ;
	OPTIONS COMPRESS=YES 
		 ;
  											
	LIBNAME lib_old
		"&benchqrp/&prev_folder/&asmt_dir" ;
	LIBNAME lib_asmt 
		"&benchqrp/&folder/&asmt_dir" ;
	LIBNAME lib_prvd
		"&benchqrp/&folder/&prvdr_dir" ;

* Input Parameters ;

%put NOTE: parameters in this chunk need to be updated with each report cycle ; 

	%put NOTE: previous assembled file for assessments should come from last APU reporting cycle ; 
	* Previous quarter combined assessments 
		*** This will be used to make sure there are no duplicates carried over from previous submission *** ;
	%LET in_prev = LTCH_ASMT_FULL_&prev_year.&prev_quarter._240311 ;

%put NOTE: parameters in this chunk need to be updated each programming cycle ; 
	* Current quarters assessment extracts ;
	%LET in_asmt = LTCH_DBX_ASMT_XTRCT_&year.&quarter._240502 ;

	* Use provider extract to incorporate CCNs (missing from ASMT tables) ;
	%LET in_prvdr = lib_prvd.LTCH_PRVDR_XTRCT_240502;

%put NOTE: parameters below here should auto update - please read log for what values are triggering ; 
* Input date parameters  ;
	%LET q_start = &cmn_qtr_start ;
	%put NOTE: &=q_start ; 
	%LET q_end = &cmn_qtr_end ;
	%put NOTE: &=q_end ; 
	%LET q_cutoff = &cmn_submit_cutoff ;
	%put NOTE: &=q_cutoff ;


* Control sheet: Source_File_Name - check that setting (sheet=) and source (sourcefile=) are up to date and correct ;
	%put NOTE: control file input is now assigned as default in the common macro import function; 
	%LET ctrl_sheet = LTCH_Assessment ;

	* Element Active range ;
	%LET LTCH_control_start = &q_start ;
	%LET LTCH_control_end = &q_end ;	

* Combined Output Naming Parameters - dynamically generated naming parameters ;
	%LET report_date = %sysfunc(today(), yymmddn6.) ;
	%LET report_qrtr = &year.&quarter ;

	* Name output based on APU vs. OR ;
	%IF &APU = 1 %THEN %DO ;
		%LET combinedout = LTCH_asmt_full_&report_qrtr._&report_date ;
	%END ;
	%IF	&APU = 0 %THEN %DO ;
		%LET combinedout = LTCH_asmt_OR_&report_qrtr._&report_date ;
	%END ;

	* Check flag warning ;
	%IF &APU ~= 1 AND &APU ~= 0 %THEN %DO ;
		%PUT: WARNING Check APU flag! ;
	%END ;

	* Make sure output name is correct for APU vs. OR ;
	%PUT NOTE: Check that report type and output name line up. &=APU and &=combinedout ;

	%put NOTE: parameters summary ; 

	* Make sure that inputs are updated ;
	%put NOTE: assessments will be assembled for &setting ; 

	%put NOTE: it will use scope of &year.&quarter with current extract from &q_start to &q_end ;

	%put NOTE: assembly will use a cut off date of &q_cutoff for current extract ;
	
	%PUT NOTE: Check current ASMT extract input &=in_asmt ;

	%PUT NOTE: recall ltch does not use the hist extract due to issues ;

	%PUT NOTE: Check current PRVDR extract input &=in_prvdr ;

	%PUT NOTE: Check current control table input &=cmn_control_table_full using  &=ctrl_sheet;

	%PUT NOTE: Check previous assembled ASMT file input &=in_prev --- should be from previous APU cycle ;

	%PUT NOTE: Check output name --- &=combinedout ;


%put NOTE: end parameters ; 

options	mprint
		mlogic
		;

/****************************** Process control table *******************************/
%put NOTE: process control table ; 
*Load Control File for Assessments - this will be used to restrict data to only what we need for APU ;
*	This uses the control file extract macro from the common macros file	;
%set_control_file_dated(setting= LTCH,
						/* cntrl_in will use defaulted definition within common macro program*/
						ctrl_sheet= &ctrl_sheet,
						start_date= &LTCH_control_start,
						end_date= &LTCH_control_end,
						ctrl_out = work.control_LTCH_clean);

	* Create a macro list of columns we need for APU from control file ;	
		* Need a SQL and DATA step compatible variable list, so two macros ;
	PROC SQL NOPRINT ;
		SELECT Source_File_Name
     	INTO :varlist separated by " "
     	FROM  work.control_LTCH_clean
     	WHERE	Element_Active_Start <= "&LTCH_control_end"d 
				AND Element_Active_End >= "&LTCH_control_start"d
				AND Source_File_Name ~= "c_ccn_num" ;

		SELECT Source_File_Name
     	INTO :varlist_sql separated by ", "
     	FROM  work.control_LTCH_clean
     	WHERE	Element_Active_Start <= "&LTCH_control_end"d 
				AND Element_Active_End >= "&LTCH_control_start"d
				AND Source_File_Name ~= "c_ccn_num" ;
	QUIT ;

%put NOTE: QA raw assessemnt file ;


** check to monitor assessments with duplicate orgnl_asmt_id and missing correction numbers (crctn_num);
%put NOTE: check to monitor assessments with duplicate orgnl_asmt_id and missing correction numbers;
/*title "Assessments with duplicate orginal assessment id and missing correction numbers ";
title2 "No Results will be displayed if such assessments do not exist ";
proc sql;
select orgnl_asmt_id
	, ltch_asmt_id
	, prvdr_intrnl_num
	, calcd_ccn_num
	, crctn_num
	, ltch_crctn_stus_cd
	, submsn_dt
from lib_asmt.&in_asmt
where orgnl_asmt_id in
	(select orgnl_asmt_id
	from lib_asmt.&in_asmt
	group by orgnl_asmt_id
	having count(ltch_asmt_id) > 1)
order by orgnl_asmt_id;
title;*/

%put NOTE: Monitoring duplicates in Assessment Extract ; 
TITLE	"Monitoring: Number of Duplicate Assessments in Extract" ;
		PROC SQL ; 
			SELECT	N_Records,
					COUNT(*) AS orgnl_asmt_ids
			FROM
				(SELECT	orgnl_asmt_id,
					COUNT(*) AS N_Records
				FROM	lib_asmt.&in_asmt 
				GROUP BY	orgnl_asmt_id)
			GROUP BY N_Records;
		QUIT ;
		TITLE ;

/*************************************** Assemble Extracted Assessments ***********************************/
%put NOTE: Assembly Extracted Assessments	;

* Import extracted assessments ;
* NOTE: Values for ltch_crctn_stus_cd are missing FROM assessemnts_iqies
	  	As such, we will not be using the historical table ;
%put NOTE: importing raw asmt extract --- recall --- LTCH has missing values in historical table and is not used ; 

	* Call the macro from common program;
	

	%import_asmt (current_asmt = lib_asmt.&in_asmt.,
					cutoff_dt = &q_cutoff.,
					columns =  %str(&varlist.) ,
					submit_dt = &asmt_setting._submsn_day,
					out = work.&asmt_setting._asmt_&report_qrtr._reduced,
					dt_flag = 0 /* Set this to 1 if trgt_dt and submsn_dt are datetime instead of date	*/) ;


%put NOTE: eliminate any remaining I, X assessments ; 

DATA	work.&asmt_setting._all_&report_qrtr
		work.&asmt_setting._inactivs ;
	FORMAT	orgnl_asmt_id
			&asmt_setting._asmt_id 32. ;
	SET work.&asmt_setting._asmt_&report_qrtr._reduced ;
	BY	orgnl_asmt_id
		crctn_num
		submsn_dt ;
	* create a quarter indicator ;
	quarter = "&quarter" ;
	* split data into keep and inactivs ; 
	if crctn_state_cd not in ("I", "X") then output work.&asmt_setting._all_&report_qrtr ;
	else output work.&asmt_setting._inactivs ; 

RUN ;

title "QA for I, X Assessments" ; 
title2 "No I,X assessments should remain in the table work.&asmt_setting._all_&report_qrtr"  ; 
proc sql ; 
	select count(*) as num_records,
			crctn_state_cd
	from work.&asmt_setting._all_&report_qrtr
	group by crctn_state_cd
	;
quit ; 
title2 "All dropped assessments should be I or X records - table work.&asmt_setting._inactivs" ; 
proc sql ; 
	select count(*) as num_records,
			crctn_state_cd
	from work.&asmt_setting._inactivs
	group by crctn_state_cd
	;
quit ;


%put NOTE: restore missing ccns and QA; 

* Restore CCNs using LTCH provider extract for current quarter ;

	* Create look-up file from provider extract ;
	DATA	work.lk_ltch_prvdr;
		SET	&in_prvdr
			(KEEP = prvdr_num
					prvdr_intrnl_num) ;
			RENAME prvdr_num = CCN;
	RUN ;	
	
	* Link CCNs via prvdr_intrnl_num ;
	DATA	work.&setting._all_&report_qrtr._w_ccn ;
	*	Initialize CCN variable	;
		LENGTH ccn $6 ;
		FORMAT ccn $char6. ;
		SET	work.&asmt_setting._all_&report_qrtr ;
		* Create a quarter indicator ;
		quarter = "&quarter" ;	
		CALL MISSING(ccn);
		*	Hash table for lookup	;
		IF _n_ = 1 THEN DO ;
			DECLARE hash prvdr(dataset:"work.lk_ltch_prvdr");
				rc = prvdr.defineKey(key:"prvdr_intrnl_num");
				rc = prvdr.defineData(DATA:"ccn");
				rc = prvdr.defineDone();
			END ;
		rc = prvdr.find();
		DROP rc;
	RUN ;

	* QA CCN incorporation ;
	title 	"QA - backfill CCNs";
	TITLE2	"Check to see whether CCN was populated appropriately. None should be missing." ;
	PROC FREQ	DATA = work.&setting._all_&report_qrtr._w_ccn
				NLEVELS ;
		TABLE	ccn / MISSPRINT NOPRINT ;
	RUN ;

* 	We cannot do the historical assessments dance because all values for LTCH_CRCTN_STUS_CD 
	are missing for LTCH, BUT we are going to check for duplicates from last quarter.
	
	From SNF, we know that here can be assessments that are modified records from last quarter.
	Even though the field that would indicate that this is a modification is missing, 
	CRCTN_NUM is NOT missing and is populated in a way that does make sense/is helpful.
	
	Note: it is unlikely that we will have duplicates, but I have been burned once so here we are ;
	
	*	Deduplicate asmt extracts if there are any repeats from last quarter.
		If there are none, rename the dataset all_&report_qrtr and move on ;

	* call macro to de-duplicate assessments ;
	%deduplicate_asmt (	prev_asmt = lib_old.&in_prev,
						curr_asmt = work.&setting._all_&report_qrtr._w_ccn,
						type = asmt,
						setting = &setting,
						pac_table = ltch_asmt,
						prev_quarter = &prev_quarter,
						quarter = &quarter,
						varlist_sql = %quote(&varlist_sql),
						out_file = work.asmt_nodup_all,
						ccn_current = ccn);

	* Create macro to check that date ranges are not suspect/no glaringly early cutoffs ;
	%MACRO check_dates (xtrct = ,
						xtrct_start = ,
						xtrct_end = ,
						submit_cutoff = ,) ;
		TITLE	"QA Check &xtrct -- target and submission dates within expected limits";
		TITLE2	"Target dates should be between &xtrct_start and &xtrct_end";
		TITLE3	"Max. Submission date should be a little earlier than or equal to &submit_cutoff";
		PROC SQL ;
			SELECT	MIN(trgt_dt) AS min_target_date FORMAT = MMDDYY10.,
					MAX(trgt_dt) AS max_target_date FORMAT = MMDDYY10.,
					MIN (submsn_dt) AS min_submsn_dt FORMAT = MMDDYY10.,
					MAX (submsn_dt) AS max_submsn_dt FORMAT = MMDDYY10.
			FROM &xtrct ;
		QUIT ;
	%MEND check_dates ;
	
	* Run macro to check dates on assembled extract ;
	%check_dates (xtrct = work.asmt_nodup_all,
				  xtrct_start = &q_start,
				  xtrct_end = &q_end,
			  	  submit_cutoff = &q_cutoff) ;

%put  NOTE: creating final file based on YTD and APU control flag values ; 

/*************************************** Create Clean YTD or Current Quarter output ***********************************/

* Create a file with current quarter data only  ;
	%IF &YTD = 0 %THEN %DO	;
		%PUT	NOTE: Only looking at current quarter. Create final file here. ;
		DATA	work.&setting._assembled_clean ;
			SET	work.asmt_nodup_all ;
		RUN ;	
	%END ;

* Create a cummulative YTD data file ; 
	%IF &YTD = 1 %THEN %DO	;
		DATA	work.&setting._assembled_clean ;
			SET	lib_old.&in_prev 	
				work.asmt_nodup_all ;

			%put NOTE: Code added 05/10/24 to correct El Paso CCN assessments for Q1-Q3 ;
			if prvdr_intrnl_num=1177226 and ccn="452103" then ccn="452122";
		RUN ;
	
	/* QA the combining */
	TITLE	"QA Check: Rowcounts for Combined Assessments" ;
	TITLE2	"Number of records from previous quarter combined file" ;
	PROC SQL ;
		SELECT	COUNT(DISTINCT orgnl_asmt_id) AS prev_q_asmts
		INTO	:prev_asmt_num
		FROM	lib_old.&in_prev ;
	QUIT ;
	
	TITLE2	"Number of records from current quarter combined file" ;
	PROC SQL ;
		SELECT	COUNT(DISTINCT orgnl_asmt_id) AS curr_q_asmts
		INTO	:curr_asmt_num
		FROM	work.asmt_nodup_all ;
	QUIT ;
	
	TITLE2	"Sum of current quarter and previous quarter" ;
	PROC SQL ;
		SELECT DISTINCT(&prev_asmt_num + &curr_asmt_num) AS sum_asmts
		FROM	work.&setting._assembled_clean ;
	QUIT ;

	TITLE2	"Acutal combined record count (should match sum above)" ;
	PROC SQL ;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS actual_records_in_combined
		FROM	work.&setting._assembled_clean ;
	QUIT ;

	TITLE	"Number of records needing corrections for Q1-Q3 " ;
	TITLE2  "(El Paso ccn=452103/prvdr_intrnl_num=1177226)" ;
	PROC SQL ;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS num_needing_corrections
		FROM	lib_old.&in_prev 
		WHERE   prvdr_intrnl_num=1177226 and ccn="452103";
	QUIT ;

	TITLE	"Number of records needing corrections after clean up " ;
	TITLE2  "(El Paso ccn=452103/prvdr_intrnl_num=1177226)" ;
	PROC SQL ;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS num_needing_corrections
		FROM	work.&setting._assembled_clean 
		WHERE   prvdr_intrnl_num=1177226 and ccn="452103";
	QUIT ;

	TITLE	"Number of records with corrections " ;
	TITLE2  "(El Paso ccn=452122/prvdr_intrnl_num=1177226)" ;
	PROC SQL ;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS num_corrected
		FROM	work.&setting._assembled_clean 
		WHERE   prvdr_intrnl_num=1177226 and ccn="452122";
	QUIT ;

	TITLE	"Number of assessments (El Paso ccn=452122)" ;
	PROC SQL ;
		SELECT ccn, prvdr_intrnl_num, COUNT(DISTINCT orgnl_asmt_id) AS num_assessments
		FROM	work.&setting._assembled_clean 
		WHERE   ccn="452122" 
		GROUP by ccn,prvdr_intrnl_num;
	QUIT ;
	TITLE ;


	TITLE ;
	%END ;

	* We only have values of YTD=1 or YTD=0 ;
	%IF &YTD~=1 AND &YTD~=0 %THEN %DO ;
		%PUT WARNING: YO! CHECK YOUR CONTROL FLAG VALUE FOR YTD ;
	%END ;

	TITLE	"QA Check: No Duplicate Assessments in Combined File" ;
	PROC SQL ; 
		SELECT	N_Records,
				COUNT(*) AS Orgnl_asmt_ids
		FROM
			(SELECT	orgnl_asmt_id,
					COUNT(*) AS N_Records
			FROM	work.&setting._assembled_clean 
			GROUP BY	orgnl_asmt_id)
		GROUP BY N_Records;
	QUIT ;

%put NOTE: QA final file ; 

/*************************************** QA and Output Assessments ***********************************/
* Double check columns and their formats ;
	TITLE "Double check character column formats: CCN and Assessment items" ;
	PROC SQL ;
		SELECT	name,
				type,
				length
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "char" ;
	QUIT ;
	TITLE ;

	TITLE "Double check numeric column formats: Dates, internal IDs, asmt IDs" ;
	PROC SQL ;
		SELECT	name,
				type,
				length,
				format
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "num" ;
	QUIT ;
	TITLE ;

* Check that there are no missing IDs, dates, etc. ;
	TITLE "Check for missing IDs/Dates - &combinedout" ;
	TITLE2 "There should be no missing levels (except maybe CCN)" ;
	PROC FREQ	DATA = work.&setting._assembled_clean 
				NLEVELS ;
			TABLE 	ccn
					prvdr_intrnl_num
					caads_dt
					trgt_dt
					submsn_dt
					crctn_num
					LTCH_asmt_id
					orgnl_asmt_id/ MISSING NOPRINT ;
	RUN ; 
	TITLE ;

* Check distribution of records ;
	TITLE "Check that distribution of records is reasonable" ;
	PROC SQL ;
		SELECT	quarter,
				caads_dt,
				MIN(trgt_dt) format date9. AS trgt_dt_start,
				MAX(trgt_dt) format date9. AS trgt_dt_end,
				COUNT(*) AS num_records,
				calculated num_records/(SELECT COUNT(*) FROM work.&setting._assembled_clean) AS percent format=percent10.2
		FROM	work.&setting._assembled_clean
			GROUP BY	quarter,
						caads_dt ;
	QUIT ; 
	TITLE ;

%put NOTE: write final file to workbench and verify row counts written ; 

/*************************************** Write Output to Workbench ***********************************/

* Write combined table to workbench, dropping any assessments without a CCN ;
	* There are no missing CCNs, but that may not be always be true so the restriction remains ;

%put NOTE: output data location changed for predev - change back to lib_asmt on dev and prod ;

	DATA lib_asmt.&combinedout ;
		SET	work.&setting._assembled_clean
		(WHERE = (ccn ~= ""));
	RUN ;		
* Convince ourselves that final rowcount is OK ;
	TITLE	"Check that final count of records is reasonable" ;
	PROC SQL ;
		SELECT	COUNT(*) AS num_asmts
		FROM work.&setting._assembled_clean ;
		SELECT	COUNT(*) AS num_no_ccn
		FROM work.&setting._assembled_clean
			WHERE	ccn = "" ;
		SELECT	COUNT(*) AS num_with_ccn
		FROM work.&setting._assembled_clean
			WHERE	ccn ~= "" ;
		SELECT	COUNT(*) AS num_asmts_output
		FROM lib_asmt.&combinedout ;
	QUIT ;
	TITLE ;

options	nomprint
		nomlogic
		;

/************************************************* END OF CODE ***********************************************************/
%put NOTE: END active code ; 