/************************************************ HEADER ********************************************************************
	* TITLE: SNF_Asmt_
	
	* AUTHOR: Elizabeth Oliver (EO)
	* PURPOSE:	This program is the most current and production version used for APU and Outreach.
				- It assembles assessments for the current quarter using extracts from history table and current assessments.

	* INPUTS:	 There are multiple inputs
			-	current quarter Assessment extract
			-	current quarter History extract (last quarter of use will be/was 23Q2 APU 
					- historical assessments combined with main table in CDR after 13APR2023)
			-	current quarter Provider extract
			-	previous quarter assembled file (for APU reporting only)
			-	Control table

	* NOTES:	To update this file review the macro variable definitions. 
                Quarterly updates: inputs and their directories, date, and output file naming parameters 
				The database and libname references can be modified as needed.
				
				This code has the following main parts: 
				- 	Parameter definition and libname references via macro variables 
				-	QA extracts before assembly 
				-	Extract assembly:	1) Combine historical if applicable) and current assessments with a quarter
										2) Combine quarters, if creating a YTD file
				-	QA final combined assessments
				-	Write final combined assessments to Workbench

	* UPDATES: 
	04-AUG-2021 (PP): Adapted from 2021-Q1 OR program (_20210708_PP)
		- simplified libref naming 
		- added an expected row count to QAs to make life easier/elimiate side math

	09-AUG-2021 (PP): applied post-QA edits made to IRF + enhanced CCNs backfill QA
		-	no longer using PROC DATASETS to create _nodup datasets
		-	using gate %IF/%THEN to assign datasets to &XXXX_nodup macros
		-	shored up naming of intermediate datasets
		-	CCN backfill QA now lists which CCNs got filled in, all those not present before the backfill

	20-AUG-2021 (PP): Production
		- updated inputs, added some notes to log for the inputs
		- added some history table subset QA as we now have history table records that will make it through IRF assembly

	06-OCT-2021 (EO): Development
		-	updated header to reflect change in programmer assignement and cycle
		-	updated inputs, control flags, and prev quarter library name structure

	18-OCT-2021 (EO): production - 2021-Q2 Outreach
		-	updated inputs on ectracts (asmt and prvdr)
		-	moved notes to log when control flags do not trigger code to run to top of chunk (per Alex)
		-	removed code investigating ccns still missing after ccn backfill
			(recall during development there was one provider with facility_internal_id = 1591714 that was still missing ccn
				because the prvdr extract was also missing ccn... THIS IS NO LONGER AN ISSUE IS PROD DATA
				the ccn assigned is 146196)

	09-NOV-2021 (EO): development - 2021-Q2 APU
		-	made header generic for ease of maintenance
		-	updated inputs and parameters for APU reporting
		-	added sign posting to log for sections
		-	cleaned up some commenting styles within macro function definitions, etc
		-	added row count qa of final file used in IRF

	19-NOV-2021 (EO): production - 2021-Q2 APU
		-	inputs updated to production data
		
	22-DEC-2021 (EO): pre-development - 2021-Q3 Outreach
		-	inputs and control flags for OR development
		-	cleaned up some commenting
		-	added title clear statement
		-	expanded titles in deduplication checks and history table checks

	06-JAN-2022 (EO): development - 2021-Q3 OR
		-	development inputs

	19-JAN-2022 (EO): production - 2021-Q3 OR
		-	production inputs

	07-FEB-2022 (EO): development - 2021-Q3 APU
		-	updated parameters and paths
		-	moved control table processing to top of program - after parameters
		-	streamlined qa checks at beginning to use control table to check variable type
		-	changes in top 275 lines or so based on changes in IRF

	18-FEB-2022 (EO): production - 2021-Q3 APU
		-	production inputs
		
	3/8/2022 KAS: Pre-dev 2021-Q4 OR
	
		-	Updated inputs and parameters
		-	Streamlined parameter updating
		-	Element start and end dates in control tables are now saved as dates, no conversion needed

	4/5/2022 EO: dev Q4 OR
		-	development inputs

	4/19/2022 EO: prod Q4 OR
		-	prod inputs
		
	5/13/2022 KAS -- dev 2021 Q4 APU
	
		- Consolidated QA -- removed QA that is now duplicative with extract phase
		- Switched to using common and assemble assessments common macros where appropriate
		- Updated inputs and parameters
		- Clarified QA with top level titles

	5/25/2022 EO -- prod for 21Q4 APU
	
		- production inputs
		- added preimport qa to monitor for duplicate asmt in extract


	07/07/2022 (Chid) -- 2022 Q1 OR development run

	07/19/2022 EO - production inputs

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
	10/12/2022 (EO): devleopment
				-	change to import macro in common program
					ran without new output and used proc compare in sep program to verify

	10/19/2022 (CT): Production run for 2022-Q2 OR

	11/04/2022 (CT): Development run for 2022-Q2 APU

	11/22/2022 (EO): prod run

	01/05/2023 (eo): Development run for 2022-Q3 OR

	01/18/2023 (SS): Production run for 2022-Q3 OR
	
	2/7/2023 (KAS): Dev run for 2022-Q3 APU
		- Added call against records in the history table with missing correction numbers 
			(Historically this has not been a problem with SNF but I guess you never know...)
		- Added mprint/mlogic option

	02/22/2023 (SS): Production run for 2022-Q3 APU

	03/27/2023 (CT): Pre-development run for 2022-Q4 OR
	
	4/20/2023 (KAS): Prod run for 2022-Q4 OR

	05/23/2023 (SS): Prod run for 2022-Q4 APU
					 - Removed code for extract check - check to monitor assessments with duplicate 
                       orgnl_asmt_id and missing correction numbers. This was causing issues in terms
                       of runtime and also being unable to output results due to sheer volume.
					 - Added code to Convert orgnl_asmt_id from char to num.

	05/24/2023 (EO): prod rerun for 22Q4 APU
					- a few unanticipated problems in yesterdays run
					- datepart code no longer needed and flag needs to be turned off
					- duplicates from Q3 are found and not sure why
					  that issue is potentially fine based on added poking
					- added a few new checks here and there

	07/14/2023 (CT): Development run for 2023-Q1 OR

	07/19/2023 (CT): Production run for 2023-Q1 OR

	08/16/2023 (CT): Development run for 2023-Q1 APU

	08/18/2023 (CT): Production run for 2023-Q1 APU

	11/14/2023 (CT): Development run for 2023-Q2 APU 
					(no code changes as still no decision from CMS on X asmts)
					
	12/13/2025 (EO): development run for 2023-Q2 APU
					- problem with SNF provider data, still using development file
					- CMS decision based on problem with inactivated assessments
						is to exclude all X assessments from processing
						in the other settings we pull in all assessments 
						and exlcude any record with an associated I submitted 
						before the data deadline for each quarter
						for SNF the I records do not have the fields related to pps record type
						populated, and thus are never pulled from the CDR into our data
						for this reason, we will exlude all X records
					- This change is being applied to the Q1 data retroactively
						within THIS code by adding an exclusionary statement to the final data set 
					- The change is being applied to the Q2 and forward data 
						within THIS code by amending the assembly steps 
					- recall the historical table was last populated within the CDR on 13APR2023
						so there are Q2 historical record in historical table AND main table
						starting in Q3 there will be no historical table.
						the changes for excluding X records from Q2 will be applied to both historical table
						and main now, since both are still in use that this time,
						the code will need further amendments during Q3 OR to redact all historical table
						processing

	01/09/2024 (HS): development run for 2023-Q3 OR
					- commented out all code related to in_asmt_hist

	01/12/2024 (SS) - Development rerun for 2023-Q3 OR using the 10-18 stand alone provider data extract.

	01/18/2024 (EO): prod run for 2023-Q3 OR
					- new control table on bench for accomodating LTCH assessment additional vairable
					- added code to step that combines all current assessments
						this filters out any remaining I, X assessments
					- added QA just after
					- removed some code that previously existed for I,X
						did so to standardize processing order across settings
			
	03/19/2024 (SS): dev run for 2023-Q3 APU

	04/12/2024 (EO): dev 2023-Q4 OR
		- extensive changes throughout
		- there is now a missing ccn that is a SB,
			previously all misisngs were SA
			to grab the corrected CCN we now need to read
			both SA and SB provider extracts
			related changes in
			- parameters to add seperate files
			- read in of raw provider data step amended for SA
			- read in of raw provider date step added for SB
			- data step to combine above added to be able to 
				create only one hash table, since it would be 
				possibly indeterminate as SB vs SA based on 
				internal id only
		- there were some changes in the assessment assembly common macro
			to accomodate a few control table changes needed
			- dropped resident internal id from dedup macro checks
		- there were a small number of assessments missing orgnl asmt id
			- five total, one of which made its way into the final file
				it is an M record, we are unable to link it to 
				the orginal record to determine if it is a resubmissions,
				thus added step to data read in to drop all records
				missing orgnl assessment id
	
	04/16/2024 (EO): 2023-Q4 OR prod
		- READ NOTES FOR DEV TOO, ABOVE, THERE WAS NOT REVIEW OF THIS THEN

	05/09/2024 (HS): dev run for 2023-Q4 APU

************************************************** END HEADER ********************************************/

%put NOTE: END header / START parameters ; 

/******************************************** PROGRAM PARAMETERS ***************************************/
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
	%LET setting = SNF ;
	%LET asmt_setting = MDS ;

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
		MPRINT ;
  											
	LIBNAME lib_old
		"&benchqrp/&prev_folder/&asmt_dir" ;
	LIBNAME lib_asmt
		"&benchqrp/&folder/&asmt_dir" ;
	LIBNAME lib_prvd
		"&benchqrp/&folder/&prvdr_dir" ;


* Input Parameters ;

%put NOTE: parameters in this chunk need to be updated with each report cycle ; 
	* Previous quarter combined assessments 
		*** This will be used to make sure there are no duplicates carried over from previous submission *** ;
	%LET in_prev = SNF_ASMT_FULL_&prev_year.&prev_quarter._240319 ;

%put NOTE: parameters in this chunk need to be updated each programming cycle ;
	* Current quarters assessment extracts ;
	%LET in_asmt = SNF_DBX_ASMT_XTRCT_&year.&quarter._240502 ;
	/*%LET in_asmt_hist = SNF_HIST_XTRCT_&year.&quarter._231120 ;*/

	* Use provider extract to incorporate CCNs (missing from ASMT tables) ;
	%LET in_prvdr_SA = lib_prvd.SNF_PRVDR_XTRCT_240502 ;
	%LET in_prvdr_SB = lib_prvd.SNF_SB_PRVDR_XTRCT_240502 ;

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
	%LET ctrl_sheet = SNF_Assessment ;

	* Element Active range ;
	%LET SNF_control_start = &q_start ;
	%LET SNF_control_end = &q_end ;	

* Combined Output Naming Parameters - dynamically generated naming parameters ;
	%LET report_date = %sysfunc(today(), yymmddn6.) ;
	%LET report_qrtr = &year.&quarter ;

	* Name output based on APU vs. OR ;
	%IF &APU = 1 %THEN %DO ;
		%LET combinedout = SNF_asmt_full_&report_qrtr._&report_date ;
	%END ;
	%IF	&APU = 0 %THEN %DO ;
		%LET combinedout = SNF_asmt_OR_&report_qrtr._&report_date ;
	%END ;
	
	* Check flag warning ;
	%IF &APU ~= 1 AND &APU ~= 0 %THEN %DO ;
		%PUT: WARNING Check APU flag! ;
	%END ;

	
	* Make sure output name is correct for APU vs. OR ;
	%PUT NOTE: Check that report type and output name line up. &=APU and &=combinedout ;

	* Make sure that inputs are updated ;
	%put NOTE: parameters summary ; 

	* Make sure that inputs are updated ;
	%put NOTE: assessments will be assembled for &setting ; 

	%put NOTE: it will use scope of &year.&quarter with current extract from &q_start to &q_end ;

	%put NOTE: assembly will use a cut off date of &q_cutoff for current extract ;
	
	%PUT NOTE: Check current ASMT extract input &=in_asmt ;

	/*%PUT NOTE: check history asmt extract input &=in_asmt_hist ;*/

	%PUT NOTE: Check current PRVDR extract input &=in_prvdr_SA and &=in_prvdr_SB ;

	%PUT NOTE: Check current control table input &=cmn_control_table_full using  &=ctrl_sheet;

	%PUT NOTE: Check previous assembled ASMT file input &=in_prev --- should be from previous APU cycle ;

	%PUT NOTE: Check output name --- &=combinedout ;


	
%put NOTE: end parameters ;

options mprint
		mlogic
		;

/*************************************** process control table ***********************************/

%put NOTE: process control table ;

* Load Control File for Assessments - this will be used to restrict data to only what we need for APU ;
*	This uses the control file extract macro from the common macros file	;
%set_control_file_dated(setting= SNF,
						/* cntrl_in will use defaulted definition within common macro program*/
						ctrl_sheet= &ctrl_sheet,
						start_date= &SNF_control_start,
						end_date= &SNF_control_end,
						ctrl_out = work.control_SNF_clean);


	* Create a macro list of columns we need for APU from control file ;
	* The list will be based on date as columns are subject to change ;
	* NOTE: SNF uses DATA step and a later PROC SQL so there are two flavors of &varlist ;
	PROC SQL NOPRINT ;
		SELECT Source_File_Name
     	INTO :varlist separated by " "
     	FROM  work.control_snf_clean
     	WHERE	Element_Active_End >= "&snf_control_start"d
				AND Element_Active_Start <= "&snf_control_end"d ;

    	SELECT Source_File_Name
		INTO :varlist_sql separated by ","
     	FROM  work.control_snf_clean
     	WHERE	Element_Active_End >= "&snf_control_start"d
				AND Element_Active_Start <= "&snf_control_end"d
				/* this list is used to compare to previous dataset, which has clean CCN column, not c_ccn_num */
				and Source_File_Name ~= "c_ccn_num" 
		;
	QUIT ;



/*************************************** QA Extracted Assessment(s)***********************************/

%put NOTE: QA raw extracts ; 

* Check correction fields (crctn_num and mds_crctn_stus_cd) ; 
	* Create macro ;
	%MACRO check_crctns (table = ,
						qrtr = ,
						pac_table = ,
						pac = ,) ;

		PROC FREQ	DATA = lib_asmt.&table ;
			TABLE	crctn_num
					&pac_table._crctn_stus_cd /NOCUM ;
			TABLE	crctn_num * &pac_table._crctn_stus_cd /NOCOL NOPERCENT NOCUM ;
			TITLE	"Check Input &qrtr values and their frequencies for crctn_num and 
				&pac_table._crctn_stus_cd -- &table" ;
		RUN ;
		title ;
	%MEND check_crctns ;

	* Run macros to check correction fields in the inputs to be assembled ;

	%check_crctns	(table = &in_asmt,
					qrtr = &report_qrtr.,
					pac_table = &asmt_setting.,
					pac = &setting) ;
/*
	%check_crctns	(table = &in_asmt_hist,
					qrtr = &report_qrtr.,
					pac_table = &asmt_setting.,
					pac = &setting)	;
					
	%missing_crctn_issue(	table = &in_asmt_hist)
*/

%put NOTE: Monitoring duplicates in assessments extract ; 
TITLE	"Monitoring: Number of Duplicate Assessments in Extract" ;
title2 "note - SNF will have duplicates since historical table was combined into main CDR table" ; 
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



%put NOTE: read in files, check key fields, perform deduplication, and QA things;

/*************************************** Assemble Extracted Assessments***********************************/

* IMPORT EXTRACTS AND FIX DATE COLUMNS SO THAT FORMATS MATCH ;

**SS 23MAY2023 - Convert orgnl_asmt_id from char to num before it is passed to the below macro**;
data interm;
	label orgnl_asmt_id="orgnl_asmt_id";

	set lib_asmt.&in_asmt.(rename=orgnl_asmt_id=orgnl_asmt_idc);
	where orgnl_asmt_idc ~= "" ; 
	orgnl_asmt_id=input(trim(left(orgnl_asmt_idc)),32.);
	
	
	drop orgnl_asmt_idc;

run;

* Import extracted assessments ;
* References macro from assessment assembly common macro;
%import_asmt	(current_asmt = interm,
				cutoff_dt = &q_cutoff.,
				out =	work.&asmt_setting._asmt_&report_qrtr._reduced,
				dt_flag = 0,
				columns = %str(&varlist.),
				submit_dt = &asmt_setting._submsn_day)
				;
/*				
%import_asmt	(current_asmt = lib_asmt.&in_asmt_hist.,
				cutoff_dt = &q_cutoff.,
				out = work.&asmt_setting._hist_&report_qrtr._reduced,
				columns = %str(&varlist.),
				submit_dt = &asmt_setting._submsn_day)	
				;
*/				


* Combine Current and Historical assessments for the CURRENT quarter ;

* From 2020-Q4, we know that there can be assessments that are modified records from last quarter in the records taken from MDS_ASMT_FED,
not just MDS_ASMT_FED_HSTRY so we need to drop any of those and keep the records from the previous quarter ;
** Previously, this was fixed at the YTD file stage, but we are heading off those duplicates here ;
	
*	Deduplicate both extracts ;
* Uses the deduplication macro from the assessment assembly provider macro	;
%deduplicate_asmt (	prev_asmt = lib_old.&in_prev,
					curr_asmt = work.&asmt_setting._asmt_&report_qrtr._reduced,
					type = asmt,
					setting = &setting,
					pac_table = snf_asmt,
					prev_quarter = &prev_quarter,
					quarter = &quarter,
					varlist_sql = %quote(&varlist_sql),
					out_file = work.asmt_nodup_all,
					ccn_current = c_ccn_num,
					prv_internal = fac_prvdr_intrnl_id);
					
/*
%deduplicate_asmt (	prev_asmt = lib_old.&in_prev,
					curr_asmt = work.&asmt_setting._hist_&report_qrtr._reduced,
					type = hist,
					setting = &setting,
					pac_table = snf_asmt,
					prev_quarter = &prev_quarter,
					quarter = &quarter,
					varlist_sql = %quote(&varlist_sql),
					out_file = work.asmt_nodup_hist_all,
					ccn_current = c_ccn_num,
					prv_internal = fac_prvdr_intrnl_id);
*/

*	Subset historical macros using macro from assessment assembly common macro	;

/*
%subset_hist (	hist_table = work.asmt_nodup_hist_all,
				asmt_table = work.asmt_nodup_all,
				pac_table = MDS,
				report_qrtr = &report_qrtr,
				quarter = &quarter,
				asmt_setting = MDS,
				out = work.asmt_nodup_hist_subset,
				ccn = c_ccn_num
				);

*/
	
%put NOTE: combine current and historical assessments ; 
%put NOTE: combine current and historical assessments plus eliminate any remaining I, X assessments ; 
* Combine current and historical assessments ;
DATA	work.&setting._all_&report_qrtr
		work.&setting._inactivs ;
		FORMAT	orgnl_asmt_id
				mds_asmt_id 32. ;
		SET work.asmt_nodup_all
			/*work.asmt_nodup_hist_subset*/
		;
		BY	orgnl_asmt_id
			crctn_num
			submsn_dt ;
		/*create a quarter indicator*/
		quarter = "&quarter" ;

		* split data into keep and inactivs ; 
	if &asmt_setting._crctn_stus_cd not in ("I", "X") then output work.&setting._all_&report_qrtr ;
	else output work.&setting._inactivs ; 

RUN ;

* Run macro from assessment assembly macro file to check merge ;
/*
%check_hist_merge(	table = work.&setting._all_&report_qrtr,
					ancestor = work.asmt_nodup_all,
					history = work.asmt_nodup_hist_subset,
					pac = &asmt_setting) ;
*/

title "QA for I, X Assessments" ; 
title2 "No I,X assessments should remain in the table work.&setting._all_&report_qrtr"  ; 
proc sql ; 
	select count(*) as num_records,
			&asmt_setting._crctn_stus_cd
	from work.&setting._all_&report_qrtr
	group by &asmt_setting._crctn_stus_cd
	;
quit ; 
title2 "All dropped assessments should be I or X records - table work.&setting._inactivs" ; 
proc sql ; 
	select count(*) as num_records,
			&asmt_setting._crctn_stus_cd
	from work.&setting._inactivs
	group by &asmt_setting._crctn_stus_cd
	;
quit ;

* Check for potential date vs. datetime issues ;
	* Create a macro to check dates we care about ;
	%macro check_asmbld_dts (table = ,) ;
		TITLE	"QA -- Check range of key dates in &table";
		TITLE2	"These are both formatted and unformatted to check for date vs datetime issues";
		title3 "datetime issues would show up as 0 in unformated dates" ;
		PROC SQL ;
		TITLE4	"&table" ;
			SELECT 	MIN(trgt_dt) AS min_trgt_dt,
					MAX(trgt_dt) AS max_trgt_dt,
					MIN(trgt_dt) AS min_trgt_dt_format FORMAT = DATE9.,
					MAX(trgt_dt) AS max_trgt_dt_format FORMAT = DATE9.
			FROM	&table ;
			SELECT	MIN(submsn_dt) AS min_submsn_dt,
					MAX(submsn_dt) AS max_submsn_dt,
					MIN(submsn_dt) AS min_submsn_dt_format FORMAT = DATE9.,
					MAX(submsn_dt) AS max_submsn_dt_format FORMAT = DATE9.
			FROM	&table ;
		QUIT ;	
		TITLE ;
	%mEnd ;
	
	* Call macro, check dates in current, history, and current+history table for the quarter ;
	%check_asmbld_dts 	(table = work.asmt_nodup_all) ;
	/*%check_asmbld_dts 	(table = work.asmt_nodup_hist_subset) ;*/
	%check_asmbld_dts 	(table = work.&setting._all_&report_qrtr) ;	


%put NOTE: restore missing ccns and QA ; 

* Restore missing CCNs using SNF provider extract for current quarter ;
* Hotness from 2020-Q4: Adapted hash table approach from LTCH ;
	* Create look-up file from provider extract ;

%put NOTE: monitor missing ccn count before fix ; 
	title "Num missing and present ccns in work.&setting._all_&report_qrtr" ; 
	proc sql ; 
		select n(c_ccn_num) as nccns
			, nmiss(c_ccn_num) as missccn
		from	work.&setting._all_&report_qrtr 
		; 
	quit ; 
	title ; 
	DATA	work.lk_snf_prvdr_SA;
		SET	&in_prvdr_SA
			(KEEP = mcare_id
					facility_internal_id) ;
			RENAME	mcare_id = ccn
					facility_internal_id = fac_prvdr_intrnl_id ;
	RUN ;	

	DATA	work.lk_snf_prvdr_SB;
		SET	&in_prvdr_SB
			(KEEP = prvdr_num
					prvdr_intrnl_num) ;
			RENAME	prvdr_num = ccn
					prvdr_intrnl_num = fac_prvdr_intrnl_id ;
	RUN ;	

	data work.lk_snf_prvdr ; 
		
		set work.lk_snf_prvdr_SA
			work.lk_snf_prvdr_SB ; 
	
	run ; 

	* Link CCNs via fac_prvdr_intrnl_id ;
	* Fill in CCN where missing, otherwise leave CCN as is (set ccn = c_ccn_num) ;
	DATA	work.&setting._all_&report_qrtr._w_ccn ;
	*	Initialize CCN variable	;
		LENGTH ccn $6 ;
		*	Hash table for lookup	;
		IF _n_=1 THEN DO ;
			DECLARE hash prvdr(dataset:"work.lk_snf_prvdr") ;
					prvdr.defineKey(key:"fac_prvdr_intrnl_id") ;
					prvdr.defineData(DATA:"ccn") ;
					prvdr.defineDone() ; 
		END ; 
		SET	work.&setting._all_&report_qrtr ;
		IF c_ccn_num = "" THEN DO ;
			rc = prvdr.find() ; 
		END ; 
		ELSE  IF c_ccn_num ~= "" THEN DO ;
			ccn = c_ccn_num ;
		END ;
		DROP rc ;
	RUN ;

	* QA CCN incorporation ;
	title	"QA -- populate missing CCN";
	title2	"Number of records and unique facility provider IDs missing CCN in source file (work.&setting._all_&report_qrtr)";
	proc sql;
		select	count(distinct fac_prvdr_intrnl_id) as Fac_Prvdr_w_Missing_CCN,
				count(*) as Records_Missing_CCN
		from work.&setting._all_&report_qrtr
		where c_ccn_num = "";
		;
	quit;

	title2	"List of records not fixed (work.&setting._all_&report_qrtr._w_ccn) - blank if none";
	proc sql;
		select	fac_prvdr_intrnl_id ,
				ccn,
				c_ccn_num
		from work.&setting._all_&report_qrtr._w_ccn
		where c_ccn_num = "";
		;
	quit;
	
	TITLE2	"Check to see whether CCN was populated appropriately in output file (work.&setting._all_&report_qrtr._w_ccn). None should be missing." ;
	PROC FREQ	DATA = work.&setting._all_&report_qrtr._w_ccn
				NLEVELS ;
		TABLE	ccn
				c_ccn_num / MISSPRINT NOPRINT ;
	RUN ;

	* Make sure CCNs line up after filling-in ;
	TITLE2	"There should be no existing values for c_ccn_num with miss-matched CCNs" ;
	TITLE3	"ZERO expected - Count of records with values for c_ccn_num originally" ;
	PROC SQL ;	
		SELECT	COUNT(*) AS nonmissing_ccns_changed
		FROM	(SELECT	ccn,
						c_ccn_num
				FROM	work.&setting._all_&report_qrtr._w_ccn
					WHERE	c_ccn_num ~= '' 
							AND	ccn ~= c_ccn_num) ;
	QUIT ;
	

/* 09-AUG-21 (PP): Added listing of corrected/backfilled CCNs so we can track with time */	
proc sql;
	TITLE2	"LIST of records missing c_ccn_num with filled in CCNs, for awareness" ;
	SELECT	ccn,
			c_ccn_num,
			COUNT(*) AS n_asmts
		FROM	work.&setting._all_&report_qrtr._w_ccn
			WHERE	c_ccn_num = '' 
					AND	ccn ~= c_ccn_num 
		GROUP BY ccn,
				 c_ccn_num ;
QUIT ;

* Known issue with fac_prvdr_intrnl_id not being unique ;
title2 "Make sure that we did not lose/gain values for fac_prvdr_intrnl_id" ;
TITLE3 "Provider ID and assessment count BEFORE filling in missing CCNs" ;
PROC SQL ;
	SELECT	COUNT(DISTINCT orgnl_asmt_id) AS N_records,
			COUNT(DISTINCT fac_prvdr_intrnl_id) AS N_prvdr_intrnl_id,
			COUNT(DISTINCT c_ccn_num) AS N_ccn
	FROM work.&setting._all_&report_qrtr ;
QUIT ;

TITLE3 "Provider ID and assessment count AFTER filling in missing CCNs" ;
title4	"No change for orgnl_asmt_id or fac_prvdr_intrnl_id expected from BEFORE filling in CCNs";
PROC SQL ;
	SELECT	COUNT(DISTINCT orgnl_asmt_id) AS N_records,
			COUNT(DISTINCT fac_prvdr_intrnl_id) AS N_prvdr_intrnl_id,
			COUNT(DISTINCT ccn) AS N_ccn
	FROM work.&setting._all_&report_qrtr._w_ccn ;
QUIT ;


* 09-AUG-21 (PP): Added listing of corrected/backfilled CCNs so we can track with time ;
TITLE2	"CCN(s) not originally in assessments -- if any -- for awareness" ;
PROC SQL ;
	SELECT	DISTINCT ccn
	FROM work.&setting._all_&report_qrtr._w_ccn 
		WHERE ccn NOT IN (SELECT c_ccn_num FROM work.&setting._all_&report_qrtr) ;
QUIT ;
TITLE ;

%put NOTE: prepare final file based on YTD and APU settings ; 

/*************************************** Create Clean YTD or Current Quarter output ***********************************/
* Current quarter only ;
	%IF &YTD = 0 %THEN %DO	;
		%PUT	NOTE: Only looking at current quarter. Create output here. ;
		DATA	work.&setting._assembled_clean ;
			SET	work.&setting._all_&report_qrtr._w_ccn ;
			DROP	c_ccn_num ;
		RUN ;	
	%END ;

* Cummulative YTD file. OVERALL: Combine current and previous assessments ;
%put NOTE: previous file is checked for I or X assessments and they are excluded if present per CMS decision for 23Q2 APU and onward; 

	%IF &YTD = 1 %THEN %DO	;
		DATA	work.&setting._assembled_clean ;
			SET	work.&setting._all_&report_qrtr._w_ccn
				lib_old.&in_prev (where = (mds_crctn_stus_cd not in ("I", "X")))
				 ;
			/* Get rid of c_ccn_num column since we have CCN */
			DROP	c_ccn_num ;
		RUN ;

		/* QA the combining */
		TITLE	"QA Check: Rowcounts for Combined Assessments" ;
		TITLE2	"Number of records from previous quarter combined file" ;
		PROC SQL ;
			SELECT	COUNT(*) AS prev_q_asmts
			INTO	:prev_asmt_num
			FROM	lib_old.&in_prev 
			where mds_crctn_stus_cd not in ("I", "X");
		QUIT ;
	
		TITLE2	"Number of records from current quarter only" ;
		PROC SQL ;
			SELECT	COUNT(*) AS curr_q_asmts
			INTO	:curr_asmt_num
			FROM	work.&setting._all_&report_qrtr._w_ccn  ;
		QUIT ;
	
		TITLE2	"Sum of current quarter and previous quarter" ;
		PROC SQL ;
			SELECT DISTINCT(&prev_asmt_num + &curr_asmt_num) AS sum_asmts
			FROM	work.&setting._assembled_clean ;
		QUIT ;


		TITLE2	"Acutal combined record count (should match sum above)" ;
		PROC SQL ;
			SELECT COUNT(*) AS num_asmts_final
			FROM	work.&setting._assembled_clean ;
		QUIT ;
		TITLE ;

		
		TITLE ;
	%END ;

	* We only have values of YTD=1 or YTD=0 ;
	%IF &YTD~=1 AND &YTD~=0 %THEN %DO ;
		%PUT WARNING: YO! CHECK YOUR CONTROL FLAG VALUE FOR YTD ;
	%END ;

	TITLE2	"No Duplicate Assessments in Combined File" ;
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
	title	"QA Final output file";
	TITLE2 "Double check character column formats: CCN and Assessment items" ;
	PROC SQL ;
		SELECT	name,
				type,
				length
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "char" ;
	QUIT ;

	TITLE2 "Double check numeric column formats: Dates, internal IDs, asmt IDs" ;
	PROC SQL ;
		SELECT	name,
				type,
				length
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "num" ;
	QUIT ;

* Check that there are no missing IDs, dates, etc. ;
	TITLE2 "Check for missing IDs/Dates in final assembled file" ;
	PROC FREQ	DATA = work.&setting._assembled_clean 
				NLEVELS ;
			TABLE 	ccn
					fac_prvdr_intrnl_id
					caads_dt
					trgt_dt
					submsn_dt
					crctn_num
					&asmt_setting._asmt_id
					orgnl_asmt_id/ MISSING NOPRINT ;
	RUN ; 

* Check distribution of records ;
	TITLE2	"Check that distribution of records is reasonable" ;
	PROC SQL ;
		SELECT	quarter,
				caads_dt,
				MIN(trgt_dt) format date9. AS trgt_dt_start,
				MAX(trgt_dt) format date9. AS trgt_dt_end,
				COUNT(*) AS num_records,
				calculated num_records/(SELECT COUNT(*) FROM work.&setting._assembled_clean) AS percent format=percent10.2
		FROM work.&setting._assembled_clean
			GROUP BY	quarter,
						caads_dt ;
	QUIT ;
	TITLE ;

%put NOTE: write final file to workbench and double check row counts written ; 

/*************************************** Write Output to Workbench ***********************************/

* Write combined table to workbench, dropping any assessments without a CCN ;

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

options noMPRINT ; 
/********************************************** END OF CODE ************************************/

options nomprint
		nomlogic
		;

%put NOTE: End active code ; 