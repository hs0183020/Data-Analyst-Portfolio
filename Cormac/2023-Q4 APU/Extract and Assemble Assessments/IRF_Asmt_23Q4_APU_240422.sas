/*************************************************************** HEADER ******************************************************************************************
	TITLE: IRF_Asmt_
	
	AUTHOR: Elizabeth Oliver (EO)

	PURPOSE:	This program is the most current and production version used in APU and OR reporting.
				This program combines assessment files for the current quarter to be used 
				for IRF reporting for calendar year (CY) 202x.
					
					It assembles assessments for the current quarter using extracts from history 
					table and current assessments to create a cummulative file of assessments 
					to date (for use in APU reporting) or for current quarter only (for OR reporting)

	INPUTS:	 There are mutliple inputs
			-	Current Qrt Assessment extract
			-	Current Qrt History extract
			-	Current Qrt Provider extract
			-	Previous Qrt assembled file (not applicable to OR reporting)
			-	Control table (Excel)

	NOTES:	To update this file review the macro variable definitions. 
                Quarterly updates: inputs and their directories, date, and output file naming parameters 
				The schema, database and libname references can be modified as needed.
				
				This code has the following main parts: 
				- 	Parameter definition and libname references via macro variables 
				-	QA extracts before assembly 
				-	Extract assembly:	1) Combine historical and current assessments with a quarter
										2) Combine quarters, if creating a YTD file
				-	QA final combined assessments
				-	Write final combined assessments to workbench

/****** UPDATES: 
	05-AUG-2021 (PP): Adapted from 2021-Q1 OR program (_20210706_PP)
		- simplified some QA now that the subsetting of history table records is separated from de-duplication
		- NOTE: CCN 49T114 is not in CSP_PRVDR_CMN, but has an assessment. It is being dropped for now.

	06-AUG-2021 (PP): Tightened up processes
		- more specific names for intermediate outputs: all labelled IRF_
		- changed PROC DATASETS to a set of macros that are controlled by &num_duplicates_XXXX
			- PROC DATASETS throws up an error if the dataset already exists and forcing a rename seems less than ideal
		- added a claims check for the CCN 49T114 weirndess

	09-AUG-2021 (PP): fixed flipped data element rules
		- confirmed that this does not change results, no new output run 

	19-AUG-2021 (PP): Production
		- updated inputs, added some notes to log for the inputs
		- added some history table subset QA as we now have history table records that will make it through IRF assembly

	06-OCT-2021 (EO): Development
		-	updated header to reflect assignement and cycle change
		- 	updated inputs, control flags, and library directory for prev asmt path

	18-OCT-2021 (EO): production - 2021-Q2 Outreach
		-	updated input file for extracts (asmt and prvdr)
		-	removed investigations into ccn 422588 - it is no longer an issue, see 21Q1 apu file for dets
		- 	removed reference to claims file, since it was only used in ccn 422588 investigation
		-	moved all notes to log when control flag does not trigger code to run to the top of code chuck (per Alex)

	09-NOV-2021 (EO): development - 2021-Q2 APU
		-	updated header with generic language to make maintenance of code easier
		-	updated inputs and parameters for APU development
		-	added section signposting notes to log
		-	cleaned up some lingering commenting style things in macro functions
		-	removed some outdated inline comments
		-	fixed error in data set name in gated code
		-	removed a drop variable statement from output data step
		- 	rewrote a problem check about line 950 to exclude rows where quarter = Q1 from the check
			(the two changes above should allow the code to run in future too)

	19-NOV-2021 (EO): production - 2021-Q2 APU
		-	input changes for production data

	22-DEC-2021 (EO): pre-development - 2021-Q3 Outreach
		-	input changes and control flag settings for outreach
		-	cleaned up commenting
		-	added blank title statements where needed
		-	expanded titles in QA checks for duplicate asmt and hist table asmts

	06-JAN-2022 (EO): development - 2021-Q3 OR
		-	development inputs

	19-JAN-2022 (EO): production - 2021-Q3 OR
		-	production inputs

	02-FEB-2022 (EO): development - 2021-Q3 APU
		-	updated parameters and paths
		-	moved control table processing to top of program - after parameters
		-	streamlined qa checks at beginning to use control table to check variable type

	07-FEB-2022 (EO): development 21Q3 APU
		-	updated to development inputs
		- 	tested changes from 02FEB - no changes needed after line 300 :)
		-	streamlined output of changes made after informal review

	18-FEB-2022 (EO): production 21Q3 Apu
		-	production inputs
		
	3/8/2022 KAS: Pre-dev 2021-Q4 OR
	
		-	Updated inputs and parameters
		-	Streamlined parameter updating
		-	Element start and end dates in control tables are now saved as dates, no conversion needed

	04/05/2022 (KAB): Development 2021-Q4 OR
		-	Updated inputs

	04/19/2022 (KAB): Production 2021-Q4 OR
		-	Updated in_asmt, in_asmt_hist, in_prvdr
		
	5/11/2022 KAS: Development 2021-Q4 APU
		
		- 	Updated parameters
		-	Reduced QA that is now redundant with extract
		-	QA clarification
		- 	Pulled past Q deduplication macro out -- it now lives in Assemble_Assessments_Common
		- 	Same for subset history check macro
		-	Broke up assessment import macro
		
	5/13/2022 KAS
		- Moved import assessment code into common macro

	5/25/2022 (EO): production for 2021-Q4 APU
		-	production inputs
		-	added one preextract qa step to monitor duplicates in extract

	7/07/2022 (EO): development for 2022-Q1 OR
		-	development parameters and other exepcted changes
		-	cleaned up year references in header
		- 	cleaned up QA step toward the end which previously excluded Q1 data,
			this was needed in 2021 reporting because we dropped a variable in 21Q1
			the exclusion is no longer needed, but the check is

	7/19/2022 EO: production inputs
	
	8/4/2022 KAS: Dev for 2022-Q1 APU
		- Updated to use common macros
	8/18/2022 EO: prod inputs

	10/11/2022 (CT): Development run for 2022-Q2 OR
		-	added a check to monitor assessments with duplicate orgnl_asmt_id
				and missing correction numbers (crctn_num)
	10/12/2022 (EO): development 
		-	change to common macro for assessment assembly
			reran and did proc compare to verify output is the same

	10/19/2022 (CT): Production run for 2022-Q2 OR
	
	11/3/2022 KAS: Dev run for 2022-Q2 APU

	11/22/2022 EO: prod run

	05-JAN-2023 (EO): development - 2022-Q3 OR
		- expected changes only for parameters

	01/18/2023 (SS): Production run for 2022-Q3 OR
	
	2/7/2023 (KAS): Dev for 2022-Q3
		- Added MPRINT and MLOGIC options
		- Added check against records from the history table with missing correction numbers (check to see if they are also duped on submission date)
		- Removed a check meant to check for a problem with records duped on orgnl_asmt_id in the main assessment table (issue has been fixed)
	
	02/22/2023 (SS): Production run for 2022-Q3 APU	
	
	3/27/2023 (KAS): Pre-dev run for 2022-Q4 OR. No code changes but keeping an eye on outputs due to oddities in the history table source data.

    04/20/2023 (SS): Production run for 2022-Q4 OR

	05/08/2023 (SS): Development run for 2022-Q4 APU

	05/22/2023 (EO): prod run 22-Q4 APU

	07/14/2023 (CT): Development run for 2023-Q1 OR

	07/19/2023 (CT): Production run for 2023-Q1 OR

	08/16/2023 (CT): Development run for 2023-Q1 APU

	08/18/2023 (SS): Production run for 2023-Q1 APU

	10/19/2023 (CT): Production run for 2023-Q2 OR

	11/09/2023 (CT): Development run for 2023-Q2 APU

	11/28/2023 (SS): Production run for 2023-Q2 APU

	01/09/2024 (SS): Development run for 2023-Q3 OR
					 - Updated IRF extarct input names to point to DBX files.

	01/17/2024 (SS): Production run for 2023-Q3 OR

	01/18/2024 (EO): prod rerun for 2023-Q3 OR
					- new control table on bench for accomodating LTCH assessment additional vairable
					- added code to step that combines all current assessments
						this filters out any remaining I, X assessments
					- added QA just after
			
	02/13/2024 (HS): Development run for 2023-Q3 APU

	03/11/2024 (SS): Production run for 2023-Q3 APU

	04/12/2024 (SS): Development run for 2023-Q4 OR

	04/16/2024 (EO): prod 2023-Q4 OR

	04/22/2024 (EO): pre dev 2023-Q4 APU
		- prepped a cummulative asmt file for use in
			an predev APU to send to acumen
		- Some COMMON are still pointing to OR folders, some are APU
		- ALL data input is still pointing to OR folders
		- data output is pointing to APU folders

	05/09/2024 (HS): dev 2023-Q4 APU


/*********************************************************** END HEADER *********************************************************/

%put NOTE: END header / START parameters ; 

*	Common macros program	;
* change this manually;
%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";

*	Common assessment assembly macro	;
* change this manually;
%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Extract and Assemble Assessments/Programs/Assemble_Assessments_Common_20240509.sas";


/************************************PROGRAM PARAMETERS***************************************/
* Static names ;
	%LET setting = IRF ;
	%LET asmt_setting = IRF ;

* Determine type of assembly and report type ;
	* YTD (=1) or current quarter only (=0) ;
	%LET YTD = 1   ;
	
	
	* APU (=1) or Outreach (=O) ;
	%if "&cmn_cycle" = "APU" %then %do;
		%let APU = 1;
	%end;
	%if "&cmn_cycle" = "OR" %then %do;
		%let APU = 0;
	%end;
	%put NOTE: &=cmn_cycle, therefore &=APU;

* Naming parameters - update from common macro ;
	%LET year = &cmn_yy. ;
	%LET quarter = &cmn_qq ;
	%put	NOTE: date variables assigned from common macros:
			&=year
			&=quarter
			;
			
	***	This needs to be updated manually ***	;
	* prev_year is only needed if prev_quarter = Q4 ;
  	%LET prev_year = 23 ;
	%LET prev_quarter = Q3 ;

	*	Folders update automatically	; 
	%let folder = &cmn_folder;
	%put	NOTE: folder macro set from common macros:
			&=folder;
	
	*	This should point to the APU folder for the previous quarter	;
	%LET prev_folder = 20&prev_year.-&prev_quarter APU ;
	%put	NOTE: &=prev_folder;

* Establish librefs ;

	* database/schema/caslib references - update if these have changed ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data ;
	%LET benchqrp = &bench/SAS/Quarterly and Annual Reporting ;
	%LET asmt_dir = Extract and Assemble Assessments/Data ;
	%LET prvdr_dir = Extract and Assemble Providers/Data ;

	* Session options and LIBREFs - update if these have changed ;
	OPTIONS COMPRESS=YES 
		MPRINT ;
  											
	LIBNAME lib_old
		"&benchqrp/&prev_folder/&asmt_dir" ;
	LIBNAME lib_asmt
		"&benchqrp/&folder/&asmt_dir" ;
	LIBNAME lib_prvd
		"&benchqrp/&folder/&prvdr_dir" ;

* Input Parameters - Update quarterly ;
***	Requires manual update	***;

	%put NOTE: assembled assessment file comes from most recent APU cycle reporting ; 
	* Previous quarter combined assessments 
		*** This will be used to make sure there are no duplicates carried over from previous submission *** ;
	%LET in_prev = IRF_ASMT_FULL_&prev_year.&prev_quarter._240311 ;

	* Current quarter's assessment extracts ;
	%LET in_asmt = IRF_DBX_ASMT_XTRCT_&year.&quarter._240502 ;
	%LET in_asmt_hist = IRF_DBX_HIST_XTRCT_&year.&quarter._240502 ;

	* Use provider extract to incorporate CCNs (missing from ASMT tables) ;
	%LET in_prvdr = lib_prvd.IRF_PRVDR_XTRCT_240502 ;

* Input date parameters - update from common macros ;
	%LET q_start = &cmn_qtr_start ;
	%LET q_end = &cmn_qtr_end;
	%LET q_cutoff = &cmn_submit_cutoff ;
	%put NOTE: quarter limits and submission cutoff set from common macros
			&=q_start
			&=q_end
			&=q_cutoff
			;

* Control sheet: setting (sheet=) is correct. Control import macro should default to current control table. ;
	%LET ctrl_sheet = IRF_Assessment ;

	* Element Active range ;
	%LET IRF_control_start = &q_start ;
	%LET IRF_control_end = &q_end ;	

* Combined Output Naming Parameters - dynamically generated naming parameters ;
	%LET report_date = %sysfunc(today(), yymmddn6.) ;
	%LET report_qrtr = &year.&quarter ;

	* Name output based on APU vs. OR ;
	%IF &APU = 1 %THEN %DO ;
		%LET combinedout = IRF_asmt_full_&report_qrtr._&report_date ;
	%END ;
	%IF	&APU = 0 %THEN %DO ;
		%LET combinedout = IRF_asmt_OR_&report_qrtr._&report_date ;
	%END ;
	* Check flag warning ;
	%IF &APU ~= 1 AND &APU ~= 0 %THEN %DO ;
		%PUT: WARNING Check APU flag! ;
	%END ;

	* Make sure output name is correct for APU vs. OR ;
	%PUT NOTE: Check that report type and output name line up. &=APU and &=combinedout ;

	* Make sure that inputs are updated ;
	%PUT NOTE: Check ASMT input &=in_asmt ;
	%PUT NOTE: Check HIST input &=in_asmt_hist ;
	%PUT NOTE: Check PRVDR input &=in_prvdr ;

	

%put NOTE: END parameters / START QA raw extract ; 

options mprint
		mlogic
		;

/*************************************** CONTROL TABLE PROCESSING ***********************************/
%put NOTE: process control table ; 

*Load Control File for Assessments - this will be used to restrict data to only what we need for APU ;
*	This uses the control file extract macro from the common macros file	;
%set_control_file_dated(setting= IRF,
						ctrl_sheet= &ctrl_sheet,
						start_date= &IRF_control_start,
						end_date= &IRF_control_end,
						ctrl_out = work.control_IRF_clean);

	* Create a macro list of columns we need for APU from control file ;
		
		* Need a SQL and DATA step compatible variable list, so two macros ;
	PROC SQL NOPRINT ;
		SELECT Source_File_Name
     	INTO :varlist separated by " "
     	FROM  work.control_irf_clean
     	WHERE	Element_Active_Start <= "&irf_control_end"d 
				AND Element_Active_End >= "&irf_control_start"d
				AND Source_File_Name ~= "prvdr_num" ;

		SELECT Source_File_Name
     	INTO :varlist_sql separated by ", "
     	FROM  work.control_irf_clean
     	WHERE	Element_Active_Start <= "&irf_control_end"d 
				AND Element_Active_End >= "&irf_control_start"d
				AND Source_File_Name ~= "prvdr_num" ;
	QUIT ;



%put NOTE: Check Correction numbers/codes	;

	* Create macro to check that we have crctn_num ;
	* Check correction fields (crctn_num and irf_crctn_stus_cd) ; 
	* Create macro ;
	%MACRO check_crctns (in_db = ,
						table = ,
						qrtr = ,
						pac =) ;

		PROC FREQ	DATA = &in_db..&table ;
			TABLE	crctn_num
					&pac._crctn_stus_cd /NOCUM ;
			TABLE	crctn_num * &pac._crctn_stus_cd /NOCOL NOPERCENT NOCUM ;
			TITLE	"Check Input &qrtr values and their frequencies for crctn_num and 
					&pac._crctn_stus_cd -- &in_db..&table" ;
		RUN ;
		title ; 
	%MEND check_crctns ;

	* Run macros to check correction fields in the inputs to be assembled ;
	%check_crctns (in_db = lib_asmt,
					table = &in_asmt,
					qrtr = &quarter,
					pac = &asmt_setting) ;

	%check_crctns (in_db = lib_asmt,
					table = &in_asmt_hist,
					qrtr = &quarter,
					pac = &asmt_setting) ;	
					
	%missing_crctn_issue(	table = &in_asmt_hist)
					
 
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

%put NOTE: END QA raw asmt extract / START Assembly ; 

/*************************************** Assemble Extracted Assessments***********************************/


* Import extracted assessments ;
* References macro from assessment assembly common macro	;
%import_asmt	(current_asmt = lib_asmt.&in_asmt.,
				cutoff_dt = &q_cutoff.,
				out =	work.&asmt_setting._asmt_&report_qrtr._reduced,
				columns = %str(&varlist.),
				submit_dt = &asmt_setting._submsn_day)
				
%import_asmt	(current_asmt = lib_asmt.&in_asmt_hist,
				cutoff_dt = &q_cutoff.,
				out = work.&asmt_setting._hist_&report_qrtr._reduced,
				columns = %str(&varlist.),
				submit_dt = &asmt_setting._submsn_day)				

%put NOTE: deduplication and subsetting of curent and historic asmts ; 

* Combine Current and Historical assessments for the CURRENT quarter ;

	* Create a macro to combine Current and Historical assessments ;
	* From SNF, we know that here can be assessments that are modified records from last quarter 
	so we need to drop any of those ;
	** Previously, this was fixed at the YTD file stage, but we are dropping those duplicates here ;
	
	* Uses the deduplication macro from the assessment assembly provider macro	;
	%deduplicate_asmt (	prev_asmt = lib_old.&in_prev,
						curr_asmt = work.&asmt_setting._asmt_&report_qrtr._reduced,
						type = asmt,
						setting = &setting,
						pac_table = irf_asmt,
						prev_quarter = &prev_quarter,
						quarter = &quarter,
						varlist_sql = %quote(&varlist_sql),
						out_file = work.asmt_nodup_all,
						ccn_current = fac_mdcr_prvdr_num);
						
	%deduplicate_asmt (	prev_asmt = lib_old.&in_prev,
						curr_asmt = work.&asmt_setting._hist_&report_qrtr._reduced,
						type = hist,
						setting = &setting,
						pac_table = irf_asmt,
						prev_quarter = &prev_quarter,
						quarter = &quarter,
						varlist_sql = %quote(&varlist_sql),
						out_file = work.asmt_nodup_hist_all,
						ccn_current = fac_mdcr_prvdr_num);
	
	*	Subset historical macros using macro from assessment assembly common macro	;
	
	%subset_hist (	hist_table = work.asmt_nodup_hist_all,
					asmt_table = work.asmt_nodup_all,
					pac_table = IRF,
					report_qrtr = &report_qrtr,
					quarter = &quarter,
					asmt_setting = IRF,
					out = work.asmt_nodup_hist_subset,
					ccn = fac_mdcr_prvdr_num
					)

	
%put NOTE: combine current and historical assessments then QA; 

%put NOTE: combine current and historical assessments plus eliminate any remaining I, X assessments ; 

* Combine current and historical assessments ;
DATA	work.&asmt_setting._all_&report_qrtr
		work.&asmt_setting._inactivs ;
	FORMAT	orgnl_asmt_id
			&asmt_setting._asmt_id 32. ;
	SET work.asmt_nodup_all
		work.asmt_nodup_hist_subset ;
	BY	orgnl_asmt_id
		crctn_num
		submsn_dt ;
	* create a quarter indicator ;
	quarter = "&quarter" ;
	* split data into keep and inactivs ; 
	if &asmt_setting._crctn_stus_cd not in ("I", "X") then output work.&asmt_setting._all_&report_qrtr ;
	else output work.&asmt_setting._inactivs ; 

RUN ;
		
* Run macro from assessment assembly macro file to check merge ;
%check_hist_merge(	table = work.&asmt_setting._all_&report_qrtr,
					ancestor = work.asmt_nodup_all,
					history = work.asmt_nodup_hist_subset,
					pac = &asmt_setting) ;
	
title "QA for I, X Assessments" ; 
title2 "No I,X assessments should remain in the table work.&asmt_setting._all_&report_qrtr"  ; 
proc sql ; 
	select count(*) as num_records,
			&asmt_setting._crctn_stus_cd
	from work.&asmt_setting._all_&report_qrtr
	group by &asmt_setting._crctn_stus_cd
	;
quit ; 
title2 "All dropped assessments should be I or X records - table work.&asmt_setting._inactivs" ; 
proc sql ; 
	select count(*) as num_records,
			&asmt_setting._crctn_stus_cd
	from work.&asmt_setting._inactivs
	group by &asmt_setting._crctn_stus_cd
	;
quit ;
	
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
		FROM work.&xtrct ;
	QUIT ;
	title ; 
%MEND check_dates ;

* Run macro to check dates on assembled extract ;
%check_dates (xtrct = &asmt_setting._all_&report_qrtr,
			  xtrct_start = &q_start,
			  xtrct_end = &q_end,
		  	  submit_cutoff = &q_cutoff) ;

%put NOTE: get missing ccns and fix internal ids ; 
* ADDRESS INTERNAL ID AND MISSING CCNS ;

*	Assessments in the historical assessments were missing CCNs, but they did not
	make it into the combined set so missing levels for CCN is not an issue for now.
	Refer to previous iterations of code for CCN back-fill logic or to the current LTCH code ;

* Populate Assessments with CORRECT prvdr_intrnl_num FROM provider file 
		-	Prvdr_intrnl_num links assessments to providers correctly ONLY for standalone providers.
		-	For subunits (T or R), prvdr_intrnl_num points to the PARENT hospital instead of their own ;

	%MACRO fix_intrnl_ids	(in_prvdr = ) ;	
		
		/* Import provider extract to create lookup table */
		DATA	work.lk_irf_prvdr_&report_qrtr ;
			SET	&in_prvdr
				(KEEP = prvdr_num
						prvdr_intrnl_num) ;
			RENAME prvdr_num = CCN ;
		RUN ;	

		PROC SQL ;
			CREATE TABLE work.&asmt_setting._all_&report_qrtr._ids AS
			SELECT	prvdr.ccn length = 6,
					prvdr.prvdr_intrnl_num,
					asmt.* 
			FROM	work.&asmt_setting._all_&report_qrtr (DROP = prvdr_intrnl_num) asmt
					LEFT JOIN
					work.lk_irf_prvdr_&report_qrtr prvdr
				ON	asmt.fac_mdcr_prvdr_num = prvdr.ccn ;
		QUIT ;

		title	"QA fix for internal IDs";
		/* Make sure that there were no odd changes in count, no duplicates got generated */
		TITLE2	"Original &report_qrtr assessment count" ;
		PROC SQL ;
			SELECT COUNT(DISTINCT orgnl_asmt_id) AS before_asmt_count
			FROM work.&asmt_setting._all_&report_qrtr ;
		QUIT ;
	
		TITLE2	"&report_qrtr assessment count after join with provider file to add prvdr_intrnl_num" ;
		TITLE3	"Should be the same as original count" ;
		PROC SQL ;
			SELECT COUNT(DISTINCT orgnl_asmt_id) AS after_asmt_count
			FROM work.&asmt_setting._all_&report_qrtr._ids ;
		QUIT ;
	
		TITLE2 "Check for duplicate original assessment ID after joins" ;
		title3	"All orgnl_asmt_ids should have an N of exactly 1";
		PROC SQL ;
			SELECT	N_Records,
					COUNT(*) AS orgnl_asmt_ids
			FROM
				(SELECT	orgnl_asmt_id,
						COUNT(*) AS N_Records
				FROM work.&asmt_setting._all_&report_qrtr._ids
				GROUP BY orgnl_asmt_id)
			GROUP BY N_Records ;
		QUIT ;
		TITLE ;

	%MEND fix_intrnl_ids ;

* Call macro to fix IDs ;
	%fix_intrnl_ids	(in_prvdr = &IN_PRVDR) ;

%put NOTE: prep for YTD assembly if APU flag is set to 1 and YTD flag is set to 1 --- otherwise rename ; 

/*************************************** Create Clean YTD or Current Quarter output ***********************************/
	* Current quarter only ;
	%IF &YTD = 0 %THEN %DO	;
		%PUT	NOTE: Only looking at current quarter. Create output here. ;
		DATA	work.&asmt_setting._assembled_clean ;
			SET	work.&asmt_setting._all_&report_qrtr._ids ;
			DROP	c_ccn_num ;
		RUN ;	
	%END ;

	* Cummulative YTD file ;
	%IF &YTD = 1 %THEN %DO	;
		/* Combine previous quarter full file with current quarter */
		DATA	work.&asmt_setting._assembled_clean ;
			SET	lib_old.&in_prev 	
			work.&asmt_setting._all_&report_qrtr._ids ;
			/* Get rid of empty c_ccn_num column */
			DROP	c_ccn_num ;
		RUN ;		
	
		/* QA the combining */
		TITLE	"QA Check: Rowcounts for Combined Assessments (YTD file)" ;
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
			FROM	work.&asmt_setting._all_&report_qrtr._ids ;
		QUIT ;
	
		TITLE2	"Sum of current quarter and previous quarter" ;
		PROC SQL ;
			SELECT DISTINCT(&prev_asmt_num + &curr_asmt_num) AS sum_asmts
			FROM	work.&asmt_setting._assembled_clean ;
		QUIT ;

		TITLE2	"Acutal combined record count (should match sum above)" ;
		PROC SQL ;
			SELECT COUNT(DISTINCT orgnl_asmt_id) AS actual_records_in_combined
			FROM	work.&asmt_setting._assembled_clean ;
		QUIT ;
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
			FROM	work.&asmt_setting._assembled_clean 
			GROUP BY	orgnl_asmt_id)
		GROUP BY N_Records;
	QUIT ;
	TITLE ;


%put NOTE: QA final file  ;

/*************************************** QA and Output Assessments ***********************************/
* Double check columns and their formats ;
	title	"QA -- final file for output";
	TITLE2 "Double check character column formats: CCN and Assessment items" ;
	PROC SQL ;
		SELECT	name,
				type,
				length
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&asmt_setting._assembled_clean") 
					AND type = "char" ;
	QUIT ;

	TITLE2 "Double check numeric column formats: Dates, internal IDs, asmt IDs" ;
	PROC SQL ;
		SELECT	name,
				type,
				length,
				format
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&asmt_setting._assembled_clean") 
					AND type = "num" ;
	QUIT ;

* Check that there are no missing IDs, dates, etc. ;
	TITLE2 "Check for missing IDs/Dates in final assembled file" ;
	TITLE3 "There should be no missing levels, apart from subunit unit issues (see below)" ;
	PROC FREQ	DATA = work.&asmt_setting._assembled_clean 
				NLEVELS ;
			TABLE 	ccn
					fac_mdcr_prvdr_num
					prvdr_intrnl_num
					caads_dt
					trgt_dt
					submsn_dt
					crctn_num
					irf_asmt_id
					orgnl_asmt_id/ MISSING NOPRINT ;
	RUN ; 

 

	TITLE2	"Check for missing/discordant fac_mdcr_prvdr-CCN pairs" ;
	TITLE3	"This is expected if we have asmts from subunits with parent unit CCNs" ;
	TITLE4	"Or if there are assessments that belong to a CCN not in the provider extract(?)" ;
	TITLE5	"Previously, we fixed this. At present, CMS has advised us not to do so." ;

	PROC SQL ;
		SELECT	ccn label="CCN", 
				fac_mdcr_prvdr_num,
				COUNT(*) as n_asmts
		FROM	(SELECT	ccn,
						fac_mdcr_prvdr_num
				FROM	work.&asmt_setting._assembled_clean 
					WHERE	(ccn = ""
						OR ccn ~= fac_mdcr_prvdr_num)
						)
		GROUP BY	fac_mdcr_prvdr_num,
					ccn ;
	QUIT ;
	
	

* Check distribution of records ;
	TITLE2	"Check that distribution of records is reasonable" ;
	PROC SQL ;
		SELECT	quarter,
				caads_dt,
				MIN(trgt_dt) format date9. AS trgt_dt_start,
				MAX(trgt_dt) format date9. AS trgt_dt_end,
				COUNT(*) AS num_records,
				calculated num_records/(SELECT COUNT(*) FROM work.&asmt_setting._assembled_clean) AS percent format=percent10.2
		FROM	work.&asmt_setting._assembled_clean 
		GROUP BY 	quarter,
					caads_dt ;
	QUIT ;
	TITLE ;

%put NOTE: Output final file to workbench and a sanity check on row count written to bench ; 

/*************************************** Write Output to Workbench ***********************************/

* Write combined table to workbench, dropping any assessments without a CCN ;
	* There are no missing CCNs, but that may not be always be true so the restriction remains ;
	DATA lib_asmt.&combinedout ;
		SET	work.&asmt_setting._assembled_clean
		(WHERE = (ccn ~= ""));
	RUN ;		

* Convince ourselves that final rowcount is OK ;
	TITLE	"Check that final count of records is reasonable" ; 
	PROC SQL ;
		SELECT	COUNT(*) AS num_asmts
		FROM work.&asmt_setting._assembled_clean ;
		SELECT	COUNT(*) AS num_no_ccn
		FROM work.&asmt_setting._assembled_clean
			WHERE	ccn = "" ;
		SELECT	COUNT(*) AS num_with_ccn
		FROM work.&asmt_setting._assembled_clean
			WHERE	ccn ~= "" ;
		SELECT	COUNT(*) AS num_asmts_output
		FROM lib_asmt.&combinedout ;
	QUIT ; 
	TITLE ;


/********************END OF CODE**************************/

options nomprint
		nomlogic
		;

%put NOTE: END active code ; 