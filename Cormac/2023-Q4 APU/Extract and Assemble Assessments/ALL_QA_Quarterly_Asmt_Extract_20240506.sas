/*	
	This is the QA for the Assessment Extract. 
	It should be run AFTER the quarterly extract has been run.
	This is a departure from our standard approach to QA -- we do not typically separate QA into its own file.
	However, the Assessment extract can take an excessive amount of time to run, often a few hours.
	Separating out the QA eases development as the QA logic runs much more quickly but is more complex.
	
	ALSO, Program checks values for all relevant assessment fields for IRF, LTCH, and SNF, confirms whether or not they are within expected limits.
	We do not do this for Hospice as we use only a handful of fields for Hospice.
	Compares values from the data element library (DEL) as recorded in the control table against actual values. Prepares reports but
	does not generate output data.
	Note that we've had some issues in the past in which the DEL is missing dashes/skip values that are present in the
	actual data. This is generally not an issue. We're really looking for nonsense values.

	This program supercedes the QA portion of the extract code 
			and all previous element level check programs	

updates:
	Split off from assessment extract 7/29/2022 -- KAS
		Also added check for unique assessment and original assessment IDs	
	
	updates:
	8/3/2022 - EO - 2022-Q1 APU
		-	added element level check programming - required rewrite of a few parameters
			this allows all assessment related QA to be in one program
		-	added run control flags to the element level check calls
		-	expanded use of common macros
		-	updated header to reflect combined program
		
	8/4/2022 - KAS
		- Fixed issue with names of extract files
		
	8/17/2022 - KAS
		- Production run, updated parameters
		- Small change to label for results

	10/05/2022 - EO
		- development for 2022-Q2 Outreach
	10/19/2022 - EO
		-	production pull date for Q2 OR
	11/2/2022 -KAS
		- Updated with parameters for 2022-Q2 APU dev
	11/18/2022 - KAS
		- 2022-Q2 APU production

	01/04/2023 - CT
		- 2022-Q3 OR development run
	1/17/2023 - KAS
		- 2022-Q3 OR prod run
		
	2/6/2023 - KAS
		- 2023-Q3 APU
		-	Added a check for SNF assessments that confirms that fields 2400b and 2400c (Medicare start and end dates) contain something that could
		be a valid date (these are technically string fields). This check is only done for discharge assessments (where a310H = 1)
		These fields are only relevant for discharges -- they are used in LOS calculations which are in turn used to determine whether the stay
		counts as complete or incomplete.

	2/21/2023 - SS
		- 2022-Q3 APU production run

	03/08/2023 - EO
		- 2022-Q4 Hospice APU production run
		
	03/22/2023 -- KAS
		- Updated for 2022-Q4 OR
		- Updated data element check
			- There are new fields that are ranges, but they are expressed differently than they were previously. Had to update the logic to
			expand these ranges using the new pattern.
			- Heads up that there are a lot of fields that don't conform perfectly to the criteria -- these are generally harmless form our perspective
			but make for messy output

	4/20/2023 -- EO
		- prod run 22-Q4 OR

	05/05/2023 -- EO
		- dev run 22-Q4 APU
		- submission dt in snf is now a character
		  changed first qa extract odd set section to convert for qa
		- lots of problems in SNF checks due to long column lenghts for
		  all character variables (length = 4000 for all chars)
		- manually set snf run to 0 to test all other settings
		- THERE WAS NO CONTROL FLAG GATE ON FIRST QA CHECK, FIXED

	05/15/2023 - EO
		- run for snf tables only
		- manually overwrote the control flags
		- manually changed the out_asmt and out_asmt_hist to be dates needed for SNF 
		- those changes will be reverted after code runs but before production

	05/15/2023 - EO
		- all changes reverted back

	05/18/2023 - CT
		- Production run for 2022-Q4 APU ILS and 2023-Q1 APU Hospice
		- removed input functions from the qa_xtrct() macro
		  (as submsn_dt in SNF asmt extract changed from DATETIME25.6 to DATE9.)
		- error for SNF orgnl_asmt_id during the %check_numerical_range() macro is left unresloved intentionally -- it changed from Num to Char 
		  (Since it is something that we need to fundamentally fix in lots of places to accomodate, we need a record of the change)

	07/12/2023 -- CT
		- Development run 2023-Q1 OR ILS

	07/18/2023 -- EO
		- prod run 23Q1 OR

	08/10/2023 -- CT
		- Development run 2023-Q1 APU ILS and 2023-Q2 APU Hospice
		- fixed orgnl_asmt_id from Num to Char in Control table

	08/15/2023 -- CT
		- Fixed/Added a couple of titles in Development run 2023-Q1 APU ILS and 2023-Q2 APU Hospice.

	08/17/2023 -- SS
		- prod run 23Q1 APU ILS and 23Q2 APU Hospice.

	10/17/2023 -- SS
		- dev run 23Q2 OR ILS.

	10/18/2023 -- EO
		- prod run 23Q2 OR ILS

	11/08/2023 -- SS
		- dev run 23Q2 APU ILS and 23Q3 APU Hospice.

	11/20/2023 -- EO
		- prod run 23Q2 APU

	01/04/2024 -- SS
		- dev run 23Q3 OR ILS
		- Added dbx to all current extract file names.
		- Commented out all code related to SNF Hist as there will be no SNF Hist table produced from this cycle.
	
	01/17/2024 -- HS
		- prod run 23Q3 OR

	03/05/2024 -- HS
		- prod run 23Q3 APU

	04/11/2024 -- SS
		- dev run 23Q4 OR ILS
		- Added dbx to all previous extract file names.
	04/12/2024 -- EO
		- post dev run change to the prxmatches for the start and end stay dates in SNF
		- no formal rerun

	04/16/2024 -- EO
		- prod run

	05/06/2024 -- HS
		- dev run 23Q4 APU

	05/07/2024 -- HS
		- dev re-run 23Q4 APU to correct hospice folder and inputs. Added title for comparing current and previous assessment counts in hospice only.
*/

%put NOTE: Begin QA parameters	;

/* Common macros + location variables	*/
%let bench = /workspace/workbench/pac_qrp_swingtech/data;
%let qrp_dir = SAS/Quarterly and Annual Reporting;
* Update this manually;
%let folder = 2023-Q4 APU;

%include "&bench/&qrp_dir/&folder/Support Programs/APU_Common_Macros_20240422.sas";

%put NOTE: control flag set up and check; 

* ##### Control flags to selectively run extract (=1) for Outreach (IRF/LTCH/SNF) or APU (IRF/LTCH/SNF/Hospice);
	
%let run_hosp 	= &cmn_run_hosp;
%let run_irf 	= &cmn_run_irf;
%let run_ltch 	= &cmn_run_ltch;
%let run_snf 	= &cmn_run_snf ;

* Double check that flags are correct for each PAC;
%put NOTE: &=run_hosp;
%put NOTE: &=run_irf;
%put NOTE: &=run_ltch;
%put NOTE: &=run_snf;

%put NOTE: assign libraries to workbenches needed AND hive database; 

* Assign libref parameters and Establish librefs;
* Workbench reference - manually update as needed; 

%put NOTE: for asmt pulls folder previous should point to last APU cycle; 

%let folder_prev = 2023-Q3 APU;

%let folder_prev_hosp = 2023-Q4 Hospice;
%let subfolder = Extract and Assemble Assessments/Data;

* External (IQIES) database references; 
%let iqies = assessment_iqies;

* Establish libref to workbench for extracts;
libname lb_asmt
	"&bench/&qrp_dir/&folder/&subfolder"	
	; 
		
* Libref for previous quarter's data	;
libname old_asmt
	"&bench/&qrp_dir/&folder_prev/&subfolder"
	access = readonly
	;

* Libref for previous quarter's hospice data	;
libname old_hosp
	"&bench/&qrp_dir/&folder_prev_hosp/&subfolder"
	access = readonly
	;

%put NOTE: parameters with updates needed each cycle --- more often if needed; 

* Assign parameters;

* ##### Date parameters;
* These get updated automatically from the common macros program;
* Note: dates are in Hive format and Hospice is a quarter ahead;
%let submit_cutoff = &cmn_submit_cutoff_hive; 
%put NOTE: &=submit_cutoff ; 
%let pull_start_hive = &cmn_qtr_start_hive;
%put NOTE: &=pull_start_hive ; 
%let pull_end_hive = &cmn_qtr_end_hive;
%put NOTE: &=pull_end_hive ; 
%let pull_start_hive_hosp = &cmn_qtr_start_hive_hosp;
%put NOTE: &=pull_start_hive_hosp ; 
%let pull_end_hive_hosp = &cmn_qtr_end_hive_hosp;
%put NOTE: &=pull_end_hive_hosp ; 

%let q_start = &cmn_qtr_start;
%put NOTE: &=q_start ; 
%let q_end = &cmn_qtr_end;
%put NOTE: &=q_end ; 
%let q_start_hosp = &cmn_qtr_start_hosp;
%put NOTE: &=q_start_hosp ; 
%let q_end_hosp = &cmn_qtr_end_hosp;
%put NOTE: &=q_end_hosp ; 

* ##### Reporting quarter parameter;
*** These get updated automatically from the common macros program;
* Note: Hospice is a quarter ahead;
%let report_qtr = &cmn_yy.&cmn_qq;
%put NOTE: &=report_qtr ; 
%let report_qtr_hosp = &cmn_yy_hosp.&cmn_qq_hosp;
%put NOTE: &=report_qtr_hosp ; 


* Control sheet: Source_File_Name - check that setting (sheet=) and source (sourcefile=) are up to date and correct;
*** These get updated automatically from the common macros program;
%let ctrl_dir = &cmn_control_table_path;
%let ctrl_fname = &cmn_control_table_file;
%let control_full = &cmn_control_table_full ; 
%put NOTE: &=control_full ; 

* Previous extracts for comparison;
* USUALLY all extracts are pulled on the same day and refer to the same quarter;
*** Update these manually;
*** These are extract dates and not assembly dates;
%let prev_date = 240304;
%let prev_date_hosp = 240304;
%let prev_qtr = 23Q3;
%let prev_qtr_hosp = 23Q4;

%put NOTE: parameters with static assignments --- update as needed; 

%let prev_irf = irf_dbx_asmt_xtrct_&prev_qtr._&prev_date;
%let prev_irf_hist = irf_dbx_hist_xtrct_&prev_qtr._&prev_date;
%let prev_ltch = ltch_dbx_asmt_xtrct_&prev_qtr._&prev_date;
%let prev_ltch_hist = ltch_dbx_hist_xtrct_&prev_qtr._&prev_date;
%let prev_snf = snf_dbx_asmt_xtrct_&prev_qtr._&prev_date;
%let prev_snf_hist = snf_dbx_hist_xtrct_&prev_qtr._&prev_date;
%let prev_hosp = HOSPC_DBX_ASMT_XTRCT_&prev_qtr_hosp._&prev_date_hosp;
%let prev_hosp_hist = HOSPC_DBX_HIST_XTRCT_&prev_qtr_hosp._&prev_date_hosp;

* Assign parameters - these autofill based on the previous parameter chunk;

* Output naming - update if needed;
* Files named _ASMT are "current" assessments. Files named _HIST are historical.  
* Note: Hospice is a quarter ahead so it has its own base naming parameter;
%let out_root = dbx_asmt_xtrct_&report_qtr._;
%let out_root_hist = dbx_hist_xtrct_&report_qtr._;
%let out_root_hosp = dbx_asmt_xtrct_&report_qtr_hosp._;
%let out_root_hist_hosp = dbx_hist_xtrct_&report_qtr_hosp._;


*** Update this manually;
%put NOTE: pull date below needs updating each run ; 
%let pull_date = 240502;

%let extract_date = &pull_date;
%let extract_date_hosp = &pull_date;

%let out_asmt = &out_root.&extract_date;
%let out_asmt_hosp = &out_root_hosp.&extract_date_hosp.;
%let out_hist = &out_root_hist.&extract_date;
%let out_hist_hosp = &out_root_hist_hosp.&extract_date_hosp.;

* Dynamically generate output names for each PAC from above parameters;
%let out_hosp = HOSPC_&out_asmt_hosp;
%let out_irf = IRF_&out_asmt;
%let out_ltch = LTCH_&out_asmt;
%let out_snf = SNF_&out_asmt; 

%let out_hosp_hist = HOSPC_&out_hist_hosp;
%let out_irf_hist = IRF_&out_hist;
%let out_ltch_hist = LTCH_&out_hist;
%let out_snf_hist = SNF_&out_hist; 

%put NOTE:	Begin QA extract;
************************************************	BEGIN QA	***************************************************;
* Create QA macro that checks date ranges and row count of extracts

	* There are four parts: 
	-	first PROC SQL returns the row count in the extracted assessments
	-	second PROC SQL returns the range for target dates (trgt_dt) and 
		submission dates (submsn_dt) in the extracted assessments
	-	third PROC SQL returns the counts of third and fourth digits in the CCNs
		of the extracted assessments 
	-	fourth PROC SQL returns character count (how many digits are there?) for
		the CCNs in the extracted assessments

	* Conditioned on whether:
	-	extract exists
	-	extract is SNF_ASMT (has a different format for date)
	-	extract is LTCH (is missing CCN column)

	* Values to be filled in on running:
	-	in_lib -->	location of output extract on workbench
	-	xtrct -->	assessments previously extracted from IQIES 
					(use output parameters defined above)
	-	odd_set -->	SNF assessments have a timestamp instead of date, may need to change in future
	-	has_ccn --> =1 for all extracts other than LTCH (=0)
	-	ccn_specs --> PAC facility-specific CCN rules for QAing
	-	ccn --> variable name for CCN (changes across PACs)  ;

	%MACRO qa_xtrct 	(control = ,
						xtrct = ,
						 odd_set = ,
						 prev_lib = ,
						 prev_xtrct = ,
						 has_ccn = ,
						 ccn = ,
						 ccn_specs = ,
						 in_lib = lb_asmt,
						 trgt_start = &pull_start_hive,
						 trgt_end = &pull_end_hive,
						 cutoff = &submit_cutoff) ;
	%if &control NE 1 %then %do ; 
		%put NOTE: not running due to control flag settings ; 
	%end  ;
	%else %do ;
		%IF (%SYSFUNC(EXIST(&in_lib..&xtrct)) = 1 AND &xtrct ~= &odd_set) %THEN %DO ;
			TITLE	"QA &xtrct -- number of assessments";
			TITLE2	"Compare num_ and prev_num_ , make sure numbers are in the same ballpark" ;
			%IF %sysfunc(find(upcase(&xtrct.),HOSP)) ge 1 %THEN %DO; 
				TITLE3	"Hospice Asmt extracts are YTD" ;
			%END;
			PROC SQL ;
				SELECT COUNT(*) as num_asessments format = comma10.
				FROM &in_lib..&xtrct ;

				SELECT COUNT(*) as prev_num_asessments format = comma10.
				FROM &prev_lib..&prev_xtrct ;
			QUIT ;	
			TITLE ;
			TITLE	"QA &xtrct -- target and submission dates within expected limits";
			TITLE2	"Target dates should be between &trgt_start and &trgt_end";
			TITLE3	"Max. Submission date should be close to (<=) &cutoff";
			PROC SQL ;
				SELECT	MIN(trgt_dt) AS min_target_date FORMAT=DATE9.,
						MAX(trgt_dt) AS max_target_date FORMAT=DATE9.,
						MIN (submsn_dt) AS min_submsn_dt FORMAT=DATE9.,
						MAX (submsn_dt) AS max_submsn_dt FORMAT=DATE9.
				FROM &in_lib..&xtrct ;
			QUIT ;
		%END ;

		%ELSE %IF (%SYSFUNC(EXIST(&in_lib..&xtrct)) = 1 AND &xtrct = &odd_set) %THEN %DO ;
			TITLE	"QA &xtrct -- number of assessments";
			TITLE2	"Compare num_ and prev_num_ , make sure numbers are in the same ballpark" ;
			PROC SQL ;
				SELECT COUNT(*) as num_asessments format = comma10.
				FROM &in_lib..&xtrct ;

				SELECT COUNT(*) as prev_num_asessments format = comma10.
				FROM &prev_lib..&prev_xtrct ;
			QUIT ;	

			TITLE	"QA &xtrct -- target and submission dates within expected limits";
			TITLE2	"Target dates should be between &trgt_start and &trgt_end";
			TITLE3	"Max. Submission date should be close to but less than &cutoff";
			PROC SQL ;
				SELECT	MIN(trgt_dt) AS min_target_date FORMAT=DATE9.,
						MAX(trgt_dt) AS max_target_date FORMAT=DATE9.,
						MIN (submsn_dt) AS min_submsn_dt FORMAT=DATE9.,
						MAX (submsn_dt) AS max_submsn_dt FORMAT=DATE9.
				FROM &in_lib..&xtrct ;
			QUIT ;
		%END ;

		%IF (%SYSFUNC(EXIST(&in_lib..&xtrct)) = 1 AND &has_ccn = 1) %THEN %DO ;
			TITLE	"QA -- formatting of CCN for &xtrct" ;
			TITLE2	"CCN should be six alphanumeric characters" ;
			TITLE3	"&ccn_specs" ;
			PROC SQL ;
				SELECT	SUBSTR(&ccn, 3, 2) AS CCN_pos34,
						COUNT(*) AS num_assessments format = comma10.
				FROM	&in_lib..&xtrct
				GROUP BY calculated CCN_pos34 ; 
				title3;

				SELECT	LENGTHN(&ccn) AS length_CCN,
						COUNT(*) AS num_assessments format = comma10.
				FROM	&in_lib..&xtrct
				GROUP BY calculated length_CCN ;
			QUIT ;
		%END ;
		%IF (%SYSFUNC(EXIST(&in_lib..&xtrct)) = 1 AND &has_ccn = 0) %THEN %DO ;
			%PUT	NOTE: No CCNs to QA -- extract does not have CCN column ;	
		%END ;

		%ELSE %IF %SYSFUNC(EXIST(&in_lib..&xtrct)) = 0 %THEN %DO ;
			%PUT	NOTE: No table to QA -- extract was not pulled ;	
		%END ;
		
		
	
		%end ; 
	
	%MEND qa_xtrct ;

* Call macros;
	* IRF ;
	%qa_xtrct 	(control = &run_irf,
				xtrct = &out_irf,
				 odd_set = &out_snf, 
				 prev_lib = old_asmt, 
				 prev_xtrct = &prev_irf, 
				 has_ccn = 1,
				 ccn = 	fac_mdcr_prvdr_num,
				 ccn_specs = %STR(Standalone IRFs have third and fourth digit = 30. 
								  R and T CCNs are subunits of larger institutions)) ;
						
	%qa_xtrct 	(control = &run_irf,
				xtrct = &out_irf_hist,
				 odd_set = &out_snf,
				 prev_lib = old_asmt,
				 prev_xtrct = &prev_irf_hist,
				 has_ccn = 1,
				 ccn = 	fac_mdcr_prvdr_num,
				 ccn_specs = %STR(Standalone IRFs have third and fourth digit = 30. 
								  R and T CCNs are subunits of larger institutions)) ;

	*LTCH ;
	%qa_xtrct 	(control = &run_ltch,
				xtrct = &out_ltch, 
				 odd_set =&out_snf,
				 prev_lib = old_asmt,
				 prev_xtrct = &prev_ltch, 
				 has_ccn=0) ;
	
	%qa_xtrct 	(control = &run_ltch,
				xtrct = &out_ltch_hist, 
				odd_set = &out_snf, 
				prev_lib = old_asmt,
				prev_xtrct = &prev_ltch_hist, 
				has_ccn=0) ;
	
	* SNF ;


	%qa_xtrct 	(control = &run_snf,
				xtrct = &out_snf, 
				odd_set = &out_snf, 
				has_ccn=1,
				prev_lib = old_asmt,
				prev_xtrct = &prev_snf,  
				ccn = c_ccn_num,
				ccn_specs = %STR(Standalone SNFs have third and fourth digit between 50 and 64. 
								 SBs are from 00 thru 08, 20, and 30.)) ;


	/*%qa_xtrct 	(control = &run_snf,
				xtrct = &out_snf_hist, 
				odd_set =&out_snf_hist, 
				prev_lib = old_asmt,
				prev_xtrct = &prev_snf_hist, 
				has_ccn=1, 
				ccn = c_ccn_num,
				ccn_specs = %STR(Standalone SNFs have third and fourth digit between 50 and 64.
								 SBs are from 00 thru 08, 20, and 30.)) ;*/
	

	* Hospice ;
	%qa_xtrct 	(control = &run_hosp,
				xtrct = &out_hosp, 
				odd_set = &out_snf,
				prev_lib = old_hosp,
				prev_xtrct = &prev_hosp,  
				has_ccn=1, 
				ccn = c_ccn_num,
				ccn_specs = %STR(Hospice: third and fourth digit should be 15, 16, or 17),
				trgt_start = &pull_start_hive_hosp,
				trgt_end = &pull_end_hive_hosp) ;


	%qa_xtrct 	(control = &run_hosp,
				xtrct = &out_hosp_hist, 
				odd_set = &out_snf, 
				prev_lib = old_hosp,
				prev_xtrct = &prev_hosp_hist, 
				has_ccn=1, 
				ccn = c_ccn_num,
				ccn_specs = %STR(Hospice: third and fourth digit should be 15, 16, or 17),
				trgt_start = &pull_start_hive_hosp,
				trgt_end = &pull_end_hive_hosp) ;

/*	Checks to see if assessment ID (name is different for each PAC type) and orgnl_asmt_ID are unique	
	We only do this for the main tables -- we expect there will be non-unique orgnl_asmt_ID in the history tables
	Note that there is a known issue with Hospice in which non-unique original assessment IDs are present in the main table
*/

%macro check_ids(	control =,
					source_lib = lb_asmt,
					source =,
					asmt_id =);
	
	
	%if &control NE 1 %then %do;
	
		%put NOTE: "Control not set to 1, check_id not run for &source";
	
	%end;
	
	%if &control = 1 %then %do;
	
		title	"Confirm unique assessment IDs and original assessment IDs for &source";
		title2	"All counts should be equal";
		
		%if &asmt_id = mds_asmt_id %then %do;
		
			title3	"In SNF, we expect the counts not to match.";
			
		
		%end;	
	
		%if &asmt_id = hospc_asmt_id %then %do;
		
			title3	"There is a known issue for Hospice assessment in which there are providers with duplicate orginal assessment IDs";
			title4	"This is problematic but workarounds are in place.";
		
		%end;
		
		proc sql;
			select	count(&asmt_id) as Total_Assessment_ID format = comma10.,
					count(distinct &asmt_id) as Unique_Assessment_ID format = comma10.,
					count(orgnl_asmt_id) as Total_Orgnl_Asmt_ID format = comma10.,
					count(distinct orgnl_asmt_id) as Unique_Orgnl_Asmt_ID format = comma10.
			from	&source_lib..&source
			;
		quit;
		
		title;
		
	%end;
	
%mEnd check_ids;

%check_ids(	control = &run_irf,
			source = &out_irf,
			asmt_id = irf_asmt_id)
			
%check_ids(	control = &run_ltch,
			source = &out_ltch,
			asmt_id = ltch_asmt_id)
			
%check_ids(	control = &run_snf,
			source = &out_snf,
			asmt_id = mds_asmt_id)	
			
%check_ids(	control = &run_hosp,
			source = &out_hosp,
			asmt_id = hospc_asmt_id)	;			

/* check extract column formats to make sure they do not need to be fixed */
%macro check_formats (
	qrtr=, 
	table=, 
	setting=,
	control = ,);

	/* qrtr: two digit year and quarter used for naming tables and QA titles (e.g. '21Q4') */
	/* table: output data set name from xtrct_asmt() macro */
	/* setting: PAC setting (e.g. 'IRF', 'LTCH', 'SNF') */

	%if &control = 1 %then %do;
		/* find field names for num and char variables */
		proc sql;
			create table columns_&setting._&qrtr._check as
				select
					asmt.name as actual_name,
					asmt.type as actual_type,
					cntrl.source_file_name as expected_name,
					cntrl.var_type as expected_type
				from (
					select name, type
					from dictionary.columns
					where libname = upcase("lb_asmt")
					and memname = upcase("&table")
					) as asmt
				right join work.control_&setting as cntrl
					on cntrl.source_file_name = asmt.name
				where cntrl.source_file_name not like 'c_ccn_num';
		quit;
		
		title "Check input &setting &qrtr column formats";
		title2 "Dates, timestamps, crctn_num, internal and assessment IDs should be numeric"; 
		title3 "Assessment fields and CCN (fac_mdcr_prvdr_num) should be character";
		title4 "actual type and expected type should match along the diagonal";
		%if &setting = SNF %then %do;
			title5	"The var orgnl_asmt_id is char in main SNF asmt table and num in the SNF hist asmt table.";
		%end;
		proc freq data = columns_&setting._&qrtr._check;
			table actual_type * expected_type / missing nocum norow nopercent;
			where expected_type ~= ""; 
		run; 
		title;
	%end;

	%else %do;
		%put NOTE: running check_formats() for &setting -- control flag set to &control;
	%end;
%mend check_formats;

%macro check_hospice_formats(
    control_table = ,
    lib = ,
    table = ,
	qrtr = ,
	setting=,
	control = ,);

	%if &control = 1 %then %do;
		proc sql;
			create table columns_&setting._&qrtr._check as
				select 
					asmt.name as actual_name,
					asmt.type as actual_type,
					cntrl.source_file_name as expected_name,
					cntrl.var_type as expected_type
				from (
					select name, type
					from dictionary.columns
					where libname = upcase("lb_asmt")
					and memname = upcase("&table")
					) as asmt
				right join work.control_&setting as cntrl
					on cntrl.source_file_name = asmt.name
				where cntrl.source_file_name not like 'c_ccn_num';
		quit;

		title "Check input &setting &qrtr column formats -- &table";
		title2 "Dates, timestamps, crctn_num, internal and assessment IDs should be numeric"; 
		title3 "Assessment fields and CCN (fac_mdcr_prvdr_num) should be character";
		title4 "actual type and expected type should match along the diagonal";
		
		proc freq data = columns_&setting._&qrtr._check;
			table actual_type * expected_type / missing nocum norow nopercent;
			where expected_type ~= ""; 
		run;

		/* create a string of char-valued column names separated by a 
		/* ' ' character that are in the control file */
		proc sql noprint;
			select cntrl.source_file_name 
			into :list_raw separated by ' '
			from (
					select name, type
					from dictionary.columns
					where libname = upcase("lb_asmt")
					and memname = upcase("&table")
					and
						type = "char"
					) as asmt
			inner join &control_table as cntrl
				on asmt.name = cntrl.source_file_name
			
			;
		quit;

		/* remove c_ccn_num and a0270_dschrg_dt from the list */
		/* note: a0270_dschrg_dt should be numeric-valued in assessment file but was char-valued as of 04/26/2022 */
		%let list_clean = %sysfunc(transtrn(&list_raw, c_ccn_num, %sysfunc(trimn(%str()))));
		%let list_clean = %sysfunc(transtrn(&list_clean, a0270_dschrg_dt, %sysfunc(trimn(%str()))));

		title "Check that all Hospice character fields are discrete and manageable";
		title2	"Table &table";
		proc freq data = &lib..&table;
			tables &list_clean;
		run;

		/* set bool to not zero if a0270_dschrg_dt is in is_in_list */
		%let is_in_list = %sysfunc(count(&list_raw, a0270_dschrg_dt));
		
		%if &is_in_list > 0 %then %do;
			proc freq data = &lib..&table nlevels;
				tables a0270_dschrg_dt / noprint;
			run;
		%end;
	%end;

	%else %do;
		%put NOTE: running check_hospice_formats() -- control flag set to &control;
	%end;
%mend check_hospice_formats;

/* create a string of all source file variables of type num */
/* separated by a ' ' character */
%macro make_source_file_array(
	field_name = ,
	array_name = ,
	control_table = ,);

	select &field_name
	into :&array_name
	separated by ' '
	from &control_table
	where var_type = 'Num';
%mend make_source_file_array;

/* create a string of all source file variables of type num */
/* separated by a ' ' character that are not dates */
%macro make_source_file_array_num(
	field_name = ,
	array_name = ,
	control_table = ,);

	select &field_name
	into :&array_name
	separated by ' '
	from &control_table
	where
		var_type = 'Num'
		and source_file_name not like '%dt'
		and source_file_name not in ('trgt_dt', 'submsn_dt');
%mend make_source_file_array_num;

/* create a string of all source file variables of type num */
/* separated by a ' ' character that are dates */
%macro make_source_file_array_date(
	field_name = ,
	array_name = ,
	control_table = ,);

	select &field_name
	into :&array_name
	separated by ' '
	from &control_table
	where
		var_type = 'Num'
		and source_file_name like '%dt'
		and source_file_name not in ('trgt_dt', 'submsn_dt');
%mend make_source_file_array_date;

/* check that we have intact IDs, dates, and crctn_num */
%macro check_numerical_range(
    setting = ,
    in_db = ,
    table = ,);

	/* setting: PAC setting (e.g. 'IRF', 'LTCH', 'SNF') */
	/* in_db: libref to workbench for extracts */
	/* table: output data set name from xtrct_asmt() macro */

    %local &setting._source_file_num_params;
    %local &setting._source_file_id_params;
    %local &setting._source_file_date_params; 

    proc sql noprint;
        %make_source_file_array(
            field_name = source_file_name,
            array_name = &setting._source_file_num_params,
            control_table = control_&setting);
        %make_source_file_array_num(
            field_name = source_file_name,
            array_name = &setting._source_file_id_params,
            control_table = control_&setting);
        %make_source_file_array_date(
            field_name = source_file_name,
            array_name = &setting._source_file_date_params,
            control_table = control_&setting);
    quit;

	title "QA IDs and Dates - &setting assessments -- &in_db..&table";
	title2 "There should be no missing levels, ranges should be reasonable";
    proc tabulate data = &in_db..&table;
        var		
            &&&setting._source_file_num_params;
        table
            &&&setting._source_file_id_params,
            nmiss
            (min max) * F = 20.0;
        table
            &&&setting._source_file_date_params,
            nmiss
            (min max) * F = date9.;
    run; 
%mend check_numerical_range;

/* run check_format macro on inputs */
* run macro to check CCNs/IDs for current quarter extracts. Note that:
* c_ccn_num is missing values (always has been)
* fac_mdcr_prvdr_num has CCNs, but is missing values in the history extacts
* so far, this is not a big deal as records from the history extracts wind up not being added due to merge logic
* prvdr_intrnl_num is not missing any values ;
%if &run_irf = 1 %then %do;

	%set_control_file_dated (
		ctrl_in = &control_full,
		setting = IRF,
		ctrl_sheet = IRF_Assessment,
		start_date = &q_start,
		end_date = &q_end);

	%check_formats (
		qrtr = &report_qtr,
		table = &out_irf,
		setting = IRF,
		control = &run_irf);

	%check_formats (
		qrtr = &report_qtr._hist,
		table = &out_irf_hist,
		setting = IRF,
		control = &run_irf);
	
	%check_numerical_range(
    	setting = irf,
		in_db = lb_asmt,
		table = &out_irf);
	
	%check_numerical_range(
    	setting = irf,
		in_db = lb_asmt,
		table = &out_irf_hist);
%end;

%if &run_ltch = 1 %then %do;

	%set_control_file_dated (
		ctrl_in = &control_full,
		setting = LTCH,
		ctrl_sheet = LTCH_Assessment,
		start_date = &q_start,
		end_date = &q_end);

	%check_formats (
		qrtr = &report_qtr,
		table = &out_ltch,
		setting = LTCH,
		control = &run_ltch);

	%check_formats (
		qrtr = &report_qtr._hist,
		table = &out_ltch_hist,
		setting = LTCH,
		control = &run_ltch);

	%check_numerical_range(
    	setting = ltch,
		in_db = lb_asmt,
		table = &out_ltch);
	
	%check_numerical_range(
    	setting = ltch,
		in_db = lb_asmt,
		table = &out_ltch_hist);
%end;

%if &run_snf = 1 %then %do;

	%set_control_file_dated (
		ctrl_in = &control_full,
		setting = SNF,
		ctrl_sheet = SNF_Assessment,
		start_date = &q_start,
		end_date = &q_end);

	%check_formats (
		qrtr = &report_qtr,
		table = &out_snf,
		setting = SNF,
		control = &run_snf);
	

	/*%check_formats (
		qrtr = &report_qtr._hist,
		table = &out_snf_hist,
		setting = SNF,
		control = &run_snf);
	%put NOTE: The var orgnl_asmt_id is char in main SNF asmt table and num in the SNF hist asmt table.;*/
	
	%check_numerical_range(
    	setting = snf,
		in_db = lb_asmt,
		table = &out_snf);
	
	/*%check_numerical_range(
    	setting = snf,
		in_db = lb_asmt,
		table = &out_snf_hist);*/
		
	title	"For &out_snf";
	title2	"For SNF Records indicated as a discharge (a0310h_ppsprta_dschrg_cd = 1) --";
	title3	"Validate that fields a2400b_mdcr_stay_strt_dt and a2400c_mdcr_stay_end_dt contain valid dates";
	title4	"That is, field matches the regex /^\[12][90]\d{2}[01]\d{3}$/ *";
	title5	"1 = Match, 0 = No Match";
	footnote	"* Checks for valid dates in the 20th or 21st century.";
	footnote2	"Also accepts as valid dates in the 11th and 30th centuries. Hopefully that won't be an issue...";
	proc sql;
		select	prxmatch("/[12][90]\d{2}[01][0-9]\d{2}/", a2400b_mdcr_stay_strt_dt) as Valid_a2400b_dt,
				prxmatch("/[12][90]\d{2}[01][0-9]\d{2}/", a2400c_mdcr_stay_end_dt) as Valid_a2400c_dt,
				count(*) as Records				
		from	lb_asmt.&out_snf
		where	a0310h_ppsprta_dschrg_cd = "1"
		group by	calculated Valid_a2400b_dt,
					calculated Valid_a2400c_dt
		;
	quit;

	
	/*title	"For &out_snf_hist";
	title2	"For SNF Records indicated as a discharge (a0310h_ppsprta_dschrg_cd = 1) --";
	title3	"Validate that fields a2400b_mdcr_stay_strt_dt and a2400c_mdcr_stay_end_dt contain valid dates";
	title4	"That is, field matches the regex /^\[12][90]\d{2}[01]\d{3}$/";
	title5	"1 = Match, 0 = No Match";
	proc sql;
		select	prxmatch("/^[12][90]\d{2}[01]\d{3}$/", a2400b_mdcr_stay_strt_dt) as Valid_a2400b_dt,
				prxmatch("/^[12][90]\d{2}[01]\d{3}$/", a2400c_mdcr_stay_end_dt) as Valid_a2400c_dt,
				count(*) as Records			
		from	lb_asmt.&out_snf_hist
		where	a0310h_ppsprta_dschrg_cd = "1"
		group by	calculated Valid_a2400b_dt,
					calculated Valid_a2400c_dt
		;
	quit;*/
	
	title;
	footnote;
		
%end;

%if &run_hosp = 1 %then %do;

	%set_control_file_dated (
		ctrl_in = &control_full,
		setting = HOSPICE,
		ctrl_sheet = HOSP_Assessment,
		start_date = &q_start_hosp,
		end_date = &q_end_hosp);

	%check_hospice_formats(
		control_table = control_HOSPICE,
		lib = lb_asmt,
		table = &out_hosp,
		setting = hospice,
		qrtr = &report_qtr_hosp,
		control = &run_hosp);

	%check_hospice_formats(
		control_table = control_HOSPICE,
		lib = lb_asmt,
		table = &out_hosp_hist,
		setting = hospice,
		qrtr = &report_qtr_hosp,
		control = &run_hosp);

%end;

%put NOTE: End QA portion ; 

/************************* start element level checks *********************/
%put NOTE: Start Element level checks ; 
%put NOTE: these are completed for IRF, LTCH, and SNF only ; 

%put NOTE: control tables were imported in above code ; 

%put NOTE: 	Process control table -> list of element-valid value pairs	;
*	This gets complicated slightly because some values are actually ranges!	There is a flag for these.	;
*	Checks for whether the record HAS any valid values -- there might be a better way to do this.	;
*	Excludes fields flagged as free text 	;
%macro	element_value_pairs(	control=,
								pairs=);

	data	&pairs ;
		set	&control;
		where	valid_values ~= ""
				and ~prxmatch("/Text/i", valid_values);
		/*	Each of these SHOULD be 1 or 2, but use a wider length to catch errors...	*/
		length 	valid_value $8
				loop_valid_value $8;
		do i = 1 to countw(valid_values, "|");
			valid_value = strip(scan(valid_values, i, "|"));			
			end_of_series = strip(scan(valid_values, i+1, "|"));
			/*	Special case where we have a RANGE and we hit an integer value. Assumes that ranges are in the form First Digit|Last Digit.	*/
			if Range_Flag = 1 
				and anydigit(valid_value)
				and anydigit(end_of_series)
				and end_of_series ~= "99" /*	This is the "don't know" code -- we don't want to calculate all possible numbers up to 99!	*/
				then do;
				/*	Figure out max width of these variables. Will be needed to zero pad.	*/
				fieldwidth = length(end_of_series);
				loop_valid_value = valid_value;
				do until (input(loop_valid_value, 8.) > input(end_of_series, 8.));
					output;
					loop_valid_value = strip(put(input(valid_value, 8.) + 1, z8.));
					valid_value = substr(loop_valid_value, 9-fieldwidth, fieldwidth);
				end;
				i = i + 1;
			end;
			/*	Standard case -- just output and move on	*/
			else do;
				output;
			end;
		end;	/*	End of loop over valid values	*/
		keep	source_file_name
				valid_value

				;
	run;			
	
	title	"Convert &control to element-value pairs";
	title2	"Check for any outrageous values. Compare against raw control table if necessary";
	title3	"Also check for any MISSING values";
	proc freq	data = &pairs;
		table	valid_value / 	missing
								nocum
								nopercent
								;
	run;
	title;
	
%mEnd;	


%if &run_irf = 1 %then %do;
%element_value_pairs(	control= work.control_irf,
						pairs= work.element_value_irf)
%end;
%if &run_ltch = 1 %then %do;
%element_value_pairs(	control= work.control_ltch,
						pairs= work.element_value_ltch)
%end;
%if &run_snf = 1 %then %do;						
%element_value_pairs(	control= work.control_snf,
						pairs= work.element_value_snf)
%end;
						
%put NOTE:	Pivot assessment tables so that we have field-observed element pairs	;

%macro	long_assessment(control =,
						in_assess =,
						out_long =);
						
	/*	Create a macro variable containing the field names we need to check	*/
	title	"QA -- Fields to be checked from &in_assess";
	proc sql;
		select	source_file_name
		into	:fields_to_check
				separated by " "
		from	&control
		where	valid_values ~= ""
				and ~prxmatch("/Text/i", valid_values)
				;
	quit;
	
	title;
	
	%put &=fields_to_check;
	
	/*	Make long	*/
	data	&out_long;
		set	&in_assess;
		length	source_file_name $32
				valid_value	$8
				
				;
		keep	valid_value
				source_file_name
				;
		array to_check &fields_to_check;
		do over to_check;
			source_file_name = vname(to_check);
			valid_value = to_check;
			output;
		end;
	run;
	
%mEnd;

%if &run_irf = 1 %then %do;
%long_assessment(	control = control_irf,
					in_assess = lb_asmt.&out_irf,
					out_long = work.long_assess_irf)
%long_assessment(	control = control_irf,
					in_assess = lb_asmt.&out_irf_hist,
					out_long = work.long_hist_irf)	
%end;					
%if &run_ltch = 1 %then %do;		
%long_assessment(	control = control_ltch,
					in_assess = lb_asmt.&out_ltch,
					out_long = work.long_assess_ltch)
%end;
%if &run_snf = 1 %then %do;					
%long_assessment(	control = control_snf,
					in_assess = lb_asmt.&out_snf,
					out_long = work.long_assess_snf)

/*%long_assessment(	control = control_snf,
					in_assess = lb_asmt.&out_snf_hist,
					out_long = work.long_hist_snf)*/
%end;
					
%put NOTE: Check for field-value pairs present in source only, data only, and both.;
*	We take a count of pairs present in both, display field names and values for those present in one only.;
*	In reality, we generally only care about values present in the CDR data but missing from the DEL reference.;

%macro	check_pairs(assessment =,
					pairs =,
					output_table=);

	/*	Create master list of unique key-value pairs	*/
	proc sql;
		create table &output_table as
		select	distinct
				coalesce(pair.source_file_name,
						assess.source_file_name)
						as source_file_name,
				pair.valid_value as Expected_Value,
				assess.valid_value as Observed_Value
		from	&pairs pair
					full join
				&assessment assess
					on 	pair.source_file_name = assess.source_file_name
						and pair.valid_value = assess.valid_value
		;
	quit;

	title	"Comparing &assessment vs &pairs";
	
	proc sql;
		title2	"Total number of pairs";
		select count(*) as Total_Pairs
		from &output_table
		;
	quit;
	
	proc sql;
		title2	"Count of element-value pairs present in both reference and data";
		select count(*) as Concordant_Pairs
		from	&output_table
		where Expected_Value = Observed_Value
		;
	quit;
	
	title2	"List of values present in reference but missing from data (if any)";
	title3	"This is mostly FYI. Check for any oddities, however";
	proc sql;
		select count(*) as Discordant_Pairs
		from	&output_table
		where 	Expected_Value ~= ""
				and Observed_Value = ""
				;
		select 	source_file_name,
				Expected_Value
		from	&output_table
		where 	Expected_Value ~= ""
				and Observed_Value = ""
				;
	quit;
	
	title2	"List of values present in data but missing from reference (if any)";
	title3	"These are potentially a serious issue";
	title4	"Note however that we have seen instances in which a dash or skip group indicator (^) was missing from reference";
	title5	"These are probably errors in the reference and not cause for alarm";
	title6	"Beginning with 2022-Q4 data, we are also seeing lack of consistency for some numeric values that are being variously captured with and without a leading zero";
	title7	"This creates a lot of extra output but ultimately isn't hugely meaningful to our analysis.";
	proc sql;
		select count(*) as Discordant_Pairs
		from	&output_table
		where 	Expected_Value = ""
				and Observed_Value ~= ""
				;
		select 	source_file_name,
				Observed_Value
		from	&output_table
		where 	Expected_Value = ""
				and Observed_Value ~= ""
				;
	quit;			
	
	title;
					
%mEnd check_pairs;	


%if &run_irf = 1 %then %do;
%check_pairs(assessment = work.long_assess_irf,
			pairs = element_value_irf,
			output_table = work.results_irf_assess)
			
%check_pairs(assessment = work.long_hist_irf,
			pairs = element_value_irf,
			output_table = work.results_irf_hist)
%end;
%if &run_ltch = 1 %then %do;			
%check_pairs(assessment = work.long_assess_ltch,
			pairs = element_value_ltch,
			output_table = work.results_ltch_assess)
%end;
%if &run_snf = 1 %then %do;			
%check_pairs(assessment = work.long_assess_snf,
			pairs = element_value_snf,
			output_table = work.results_snf_assess)
			
/*%check_pairs(assessment = work.long_hist_snf,
			pairs = element_value_snf,
			output_table = work.results_snf_hist)
%end;*/	


%put NOTE: END active code;