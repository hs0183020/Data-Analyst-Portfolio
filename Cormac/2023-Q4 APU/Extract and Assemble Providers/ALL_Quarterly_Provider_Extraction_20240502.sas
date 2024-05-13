/*	The purpose of this program is to collect a quarterly extract of providers for each PAC type (SNF, Hospice, IRF, LTCH.)
	This is needed to update the list of providers for each PAC type for quarterly APU and OR reporting.
	This quarterly extract will be combined with data from past quarters (necessary in order to capture earliest OPD.)
	
	Inputs:	CDR provider Hive database (currently legacy_provider_data), table csp_prvdr_cmn
	Outputs: One quarterly extract/provider type, output to project directory
	
	Strategy:
		Repeat for each PAC location:
			Extract data from Hive source using a passthrough query in PROC SQL
			This doesn't allow for compression, so place results in the CASLIB for the output location
			Write table to the CASLIB
		Updated strategy:
			Extract data from Hive source using a passthrough query in PROC SQL
			QA data in work library
			Write table to the workbench

	Notes:
		This pulls all fields from the provider file. We don't really NEED all the fields for our purposes, so it might make sense to
		restrict the fields at some point in the future.
		
		With that said, these are very small files, for the most part.
	
	Created 7/16/2020 -- KAS
	
	Update 8/19/2020 -- Updated to better conform to details of historical QBIC extract and to better leverage capabilities of Hive
	
	Update 8/25/2020 -- Neglected to add caads_dt. This has been fixed.
	
	Update	10/22/2020 -- 	Added pull for swing bed providers. This uses a swing bed file which is created by the program import_swing_bed_file.
							Modified to use iqies databases (provider_iqies for csp_prvdr_cmn, assessment_iqies for natl_facility_mds)
							We don't need the SNF extract from csp_prvdr_cmn anymore; removed the pull against this data source. Pull against
							natl_facility_mds was previously labeled as "alt", is no longer so labeled.
							Removed condition that dropped rehab units located in CAHes (this should never have been added in the first place)
	Update 10/23/2020 -- DTA
							Expanded with the QA checks for all provider extracts.
							added swingbed type facilities as a temporary table from compare_nursing_home database.
	Update 11/16/2020-- DTA
						Updated SNF Provider source with assessment_iqies and transformation rule. Reference for swingbed finder file updated.
						Added Schema and table parameters to the extraction macro
						updated all the dates and naming convention to match YYQx or YYMMDD.
	Update 01/07/2021-- DTA
						Updated program to selectively choose which settings to run for the provider extract each time. 
						Updated Swingbed providers to pull from source csp_prvdr_cmn, sb_asmt_sbmt_cd=S
						Removed references to swingbed providers in the finder file, which was used historically.
	Update 01/18/2021--DTA
						Added QA for provider count with length!=6
	Update 02/03/2021-- DTA
						Added title2 statement for sanity on the QA:check_count macro
						Modified query for ccn=null as count(ccn)=null
						replaced sum(1) with count(*) as sum(1) could give spurious results(result=1 in some cases)
	Update 02/24/2021--DTA
						Added macro to generate table for all providers with length!=6
						
	Update 4/5/2021 -- 	KAS
						Updated for 2020-Q4 OR
						Changed destination for extract from Hive to workbench

	Update 4/19/2021 -- PP
						Added additional notes and data exploration to previous 4/19 DTA version
	Update 5/5/2021 -- DTA
						Added new librefs to the provider extraction files on Workbench directories
	Update 7/6/2021 -- DTA
						Cleaned up program : chunks of code/ librefs that are no longer in use.
	Update 8/4/2021 -- RJ
						Production for 2021 Q1 APU

	Update 8/19/2021 -- RJ
						Updated old tables for QA from Previous cycle(2020 Q4 APU) to previous instance of extraction(2021 Q1 OR)
	
	Update 10/4/2021 -- CT (Development for 2021 Q2 OR)
						(line 83) set the parameter run_hospice to 0 (as we don't do outreach for hospice)
						(lines 94-96) updated the parameters - workbench and old_workbench
						(lines 123) created only one libref for IRF, LTCH and SNF instead of three separate ones (based on new folder structure)
						(lines 144-148) updated the reference to the old/past extracts

	Update 10/18/2021 -- CT (Production run for 2021 Q2 OR)
	
	Update 11/3/2021	-- 	KAS
							Development run 2021 Q2 APU
							Updated control flags and data locations
							Added notes to designate program sections in the log
							Highlighted elements in the parameters that need to be updated frequently
							Cleaned up assignment of old hospice library
							Added some additional interpretive notes to QA
							Tweaked new providers QA a bit to show work

	update 12/20/2021 	--	EO
						 -	pre-development for 2021 Q3 Outreach
						 -	updated parameters and control flags, as needed
						 - 	updated header for older, no longer relevant notes
						 -	removed some no longer relavent commenting

	Update 01/05/2022	-- Raki (2021 Q3 OR development Run)

	Update 01/19/2022	-- KAS (2021 Q3 OR production Run)

	Update 02/04/2022	-- 	KAB
							- Development run 2022 Q3 APU
							- Updated control flags and data locations
	
	Update 02/18/2022	-- 	KAB
							- Production run 2022 Q3 APU
							- Included missing update notes for 12/21 and 01/22

	update 02/22/2022	--	EO
							- pre-development for 2021 Q3 APU - Hospice Only pull

	update 03/03/2022	--	EO
							- pre-development for 2021 Q4 Outreach
							- rearranged parameters section to put those with updates together and up top
							- added notes to log where parameters updates needed or not
							
	Update 4/4/2022		-- KAS
							- Dev 
							- Minor change when checking for CCN length -- now checks to see if 6 alphanumeric characters.
								Used to use LENGTH which would return a length of 1 when blank.

	update 4/4/2022		--	EO (post run minor fix)
							-	fixed title error on one QA
							
	Update 4/18/2022	-- KAS
							- Production run, confirmed parameters

	Update 05/02/2022 - KAB: 2021-Q4 APU Pre-Development
		- changed all run_* control gates to 1 for pre-development
		- added set_control_file() macro definition
		- added set_control_file() macro call behind run_* = 1 control gates
		- added variable definitions for set_control_file() and check_formats() macros
		- added logic in check_formats() to change control file name field for join if table was SNF swingbed
		- added proc freq for IRF_ASMT_SBMT_CD, LTCH_ASMT_SBMT_CD, category (SNF), SB_ASMT_SBMT_CD, and hospc_asmt_sbmt_cd for assoicated settings
		- added variable definitions for raw provider extracts output files
		- created macro calls to qtr_prvdr_xtract() for raw provider extracts
		- added SQL procedures to check values in raw provider data for fields used to subset extracts

	Update 05/03/2022 - EO: 2021-Q4 APU Development
		- development data pulls
		- moved around some macro definitions and qa calls
		- added common macro call to set_control_file to a larger processing macro
		- cleaned up new qa as needed to get it running
		- moved some checks to top and created a pre-extract check section
		
	Update 5/4/2022 - KAS -- Post-QA
		- Streamlined pre-extract check
		- Removed piece of code made redundant by pre-extract check
		
	Update 5/23/2022 - KAS
		- Production run -- no changes made to parameters or inputs
		
	Update 7/5/2022 - KAS
		- Dev run for 2022-Q1 OR
		
	Update 7/6/2022 - KAS
		- Re-run due to data on CDR not being updated -- no code changes.

	update 7/19/2022 EO
		- production run no code changes

	Update 08/02/2022 -- CT (Development Run for 2022 Q1 APU)
					  -- changed few parameters and parameter names
					  -- additional usage of macros from common macros program
	update 08/17/2022 -- EO production run for 2022 Q1 APU
						-- no code changes
	update 08/17/2022 -- EO POST-production run changes - NO NEW OUTPUTS OR LOGS
						-- the control table calls were still using 
							a parameter section assignment
							cleared them to use the default set up
							in the common macro program
							(cleared in call for all settings)
							several lines affected in first QA section that processes control table
							confirmed all still run as expected

	update 10/05/2022 	-- 	EO development for 22Q2 Outreach
						-	parameter changes as expected
						
						
	Update 10/18/2022	-- KAS Production run, no updates.

	Update 11/02/2022 	-- 	(CT) Development run for 2022 Q2 APU

	Update 11/18/2022	--	(EO) Production for 2022 Q2 APU
	
	Update 11/18/2022	-- KAS
							Added extraction for MDS providers from csp_prvdr_cmn (mds_asmt_sbmt_cd = M*)
							This will not replace assessment_iqies.natl_facility_mds as the primary source of provider data for standalone SNF
							It will be used to backfill missing information from natl_facility_mds during the assembly stage -- at present, this is
							just SSA codes.
							* There are some providers with a CCN indicating that they are a SNF that don't have mds_asmt_sbmt_cd = M, but they all
							appear to have been closed since at least 2002.
							
	update 01/04/2023 	-- 	SS development for 22Q3 Outreach
						-	parameter changes as expected					
						
	update 01/17/2023	--	EO production run for 2022-Q3 OR

	update 02/06/2023 	-- 	SS development for 22Q3 APU
						-	parameter changes as expected	

	update 02/17/2023 	--	EO
						- production run no code changes

	update 03/22/2023 	--	CT Development run for 2022-Q4 OR

	update 04/18/2023	--	EO production run for 2022-Q4 OR

	update 05/03/2023	--	EO development 22Q4 APU ILS / 23Q1 Hospice

	update 05/18/2023	--	SS production 22Q4 APU ILS / 23Q1 Hospice

	update 07/11/2023 	--	CT Development run for 2023-Q1 OR

	update 07/18/2023	--	EO prod run 23-Q1 OR

	update 08/04/2023 	--	CT Development run for 2023-Q1 APU

	update 08/17/2023 	--	CT Production run for 2023-Q1 APU

	update 10/03/2023 	--	EO development for 2023-Q2 OR
			- updated all cdr calls to use explicit connection
				to DBX using the ISG supplied macro
				applies to pre checks and extractions
	update 10/11/2023	--	EO development for 23Q2 OR
			- updated macro function to clean control table for QA portion
				it was not handling SNF as intended
				added the SNF definition explicity to correct
	updated 10/18/2023	--	EO production for 23Q2 OR

	update 11/07/2023 -- EO developement for 2023-Q2 APU
					  -- provider data was empty in CDR, hence,
						 Chid copied provider extract datasets from 2023-Q2 OR folder.

	update 11/21/2023 -- CT production for 2023-Q2 APU ILS/ 2023-Q3 Hospice

	update 12/28/2023 -- SS developement for 2023-Q3 OR
						 Added new macro variable old_snfsa_pull_dt to hardcode previous SNF SA file for comparison.
						 This hardcode is temporary and needs to be dropped for next cycle.

	update 01/02/2024 -- HS dev run post decommission of Ambari 23Q3 OR

	update 01/16/2024 -- EO prod run 23Q3 OR

	update 02/09/2024 -- SS dev run 23Q3 APU

	update 03/07/2024 -- EO prod run 23Q3 APU
					initial prod run to acertain status of SNF provider
					table fixes
	update 03/08/2024 -- EO prod rerun for 23Q3 APU for SNF only

	update 04/08/2024 -- RJ Dev run 23Q4 OR

	update 04/15/2024 -- SS prod run 23Q4 OR

	update 05/02/2024 -- HS Dev run 23Q4 APU
*/

************ END HEADER ********************************************************************;

%put NOTE: END header / set up options ; 

OPTIONS MPRINT
		set=SAS_HADOOP_READ_MULTIPLIER=2;

%put NOTE: ***	Begin program parameters	***	;

* common macro set up ; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;

*	Provider-specific output libraries-- current and past	;
***	UPDATE QUARTERLY!	***	;
%let folder = &cmn_folder ;
%put NOTE: &=folder;
***************** Need to manually update these two parameters **************;
%let old_folder = 2023-Q4 OR;
%let old_hosp_folder = 2023-Q4 Hospice; 

%let workbench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Extract and Assemble Providers/Data;
%let old_workbench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&old_folder/Extract and Assemble Providers/Data;
%let old_hosp_workbench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&old_hosp_folder/Extract and Assemble Providers/Data;


libname out_dir 	"&workbench";
libname old_ils 	"&old_workbench" access = readonly;
libname old_hosp 	"&old_hosp_workbench" access = readonly;


%put NOTE: parameters with updates in each cycle - more often if needed ; 

*	Control flags -- should be set to 1 if you want to run for the PAC, else 0	;
** These are 1 for IRF/LTCH/SNF in OR and 1 for IRF/LTCH/SNF/Hospice in APU
and these are updated in common macro programs;
%let run_ltch 		= &cmn_run_ltch;
%let run_irf 		= &cmn_run_irf;
%let run_snf 		= &cmn_run_snf;	
%let run_hospice 	= &cmn_run_hosp;


* Double check that flags are correct for each PAC;

%put NOTE: &=run_ltch;
%put NOTE: &=run_irf;
%put NOTE: &=run_snf;
%put NOTE: &=run_hospice;


*	Extracts from past quarters -- used in QA	;
***	UPDATE QUARTERLY!	***	;
%put NOTE:	Use most recent extract for each provider. For Hospice, this will be the previous quarter APU. ;
%put NOTEL	All others -- OR will use prior quarter APU, APU will use current quarter OR -- e.g., the last report run.	;


%let old_pull_dt = 240415 ;
%let old_hosp_pull_dt = 240304 ; 	
		
%let old_irf 		= old_ils.irf_prvdr_xtrct_&old_pull_dt;
%let old_ltch		= old_ils.ltch_prvdr_xtrct_&old_pull_dt;
%let old_snf 		= old_ils.snf_prvdr_xtrct_&old_pull_dt; 
%let old_snf_sb 	= old_ils.snf_sb_prvdr_xtrct_&old_pull_dt;
%let old_hospice 	= old_hosp.hospc_prvdr_xtrct_&old_hosp_pull_dt;

%put NOTE: paramters with static assignments - update as needed ; 

*	Run metadata -- year, quarter, and APU/OR	;
%let report_date = %sysfunc(today(), yymmddn6.);

*	Input and output locations	;
%let source_provider_db = provider_iqies;
*	Source for natl_facility_mds, which is used by SNF (but not for swing beds...)	; 
%let source_provider_db_snf = assessment_iqies;
*	Input files	;
%let in_prvdr_tbl = csp_prvdr_cmn;
%let in_prvdr_tbl_snf = natl_facility_mds;

/* set extract dates to the date of already existing pull for development */
/* set to &pull_date during production) */
%let report_date = %sysfunc(today(), yymmddn6.);
%let extract_date = &report_date ; * pre-development parameter - change to existing data date if needed;
/* %let extract_date = &report_date; */

*	Output files (updated dyamically)	;
%let out_root = _PRVDR_XTRCT_&extract_date;
%let out_prvdr_irf = IRF&out_root;
%let out_prvdr_ltch = LTCH&out_root;
%let out_prvdr_snf = SNF&out_root;
%let out_prvdr_snf_sb = SNF_SB&out_root;	 
%let out_prvdr_hospice = HOSPC&out_root;
%let out_prvdr_snf_csp = SNF_CSP&out_root;

* Control Table -- use from the Common Macros program;
%let ctrl_dir = &cmn_control_table_path;
%put NOTE: &=ctrl_dir;
%let ctrl_fname = &cmn_control_table_file;
%put NOTE: &=ctrl_fname;

* assigning ccn pattern to local macro ; 
* we just use the 6 char alpha numeric pattern here ; 
%let ccn_pattern_all = &ccn_pattern ; 
%put NOTE: ccn pattern will match to &=ccn_pattern_all ; 


%put NOTE: *** End program parameters ***	;

/**************** pre extraction checks ***********************************************************/
%put NOTE: check source data on subsetting fields ; 

%put NOTE: define macro that checks distinct values of fields used to subset the source data ; 
%macro pre_extract_check(	setting = ,
							control = ,
							in_db = ,
							in_tbl = ,
							field = ,
							title_specs = );

	%if &control = 0  %then %do ;
		%put NOTE: check against source data not needed due to control flag settings ; 
	%end ; 

	%if &control = 1 %then %do ;
		title 'Check Values in Source Data of Fields Used to Subset Extracts';
		title2 "checking &in_db..&in_tbl for &setting of values in &field" ; 
		title3 "&title_specs"; 
		%hive_exec_sql( %STR(

           		select distinct &field
				from 	&in_db..&in_tbl),
			use_dbx=Y);		

		title ; 

		
	%end ; 

%mEnd pre_extract_check ; 

%pre_extract_check(			setting = IRF,
							control = &run_IRF,
							in_db = &source_provider_db,
							in_tbl = &in_prvdr_tbl,
							field = IRF_ASMT_SBMT_CD,
							title_specs = %str(Expecting values of R or blank));
%pre_extract_check(			setting = LTCH,
							control = &run_LTCH,
							in_db = &source_provider_db,
							in_tbl = &in_prvdr_tbl,
							field = LTCH_ASMT_SBMT_CD,
							title_specs = %str(Expecting values of L or blank));
%pre_extract_check(			setting = SNF_SA,
							control = &run_SNF,
							in_db = &source_provider_db_snf,
							in_tbl = &in_prvdr_tbl_snf,
							field = category,
							title_specs = %str(expecting values of at least 02, 03, 04, or 10));
%pre_extract_check(			setting = SNF_SB,
							control = &run_SNF,
							in_db = &source_provider_db,
							in_tbl = &in_prvdr_tbl,
							field = SB_ASMT_SBMT_CD,
							title_specs = %str(Expecting values of S or blank));
%pre_extract_check(			setting = SNF_CSP,
							control = &run_SNF,
							in_db = &source_provider_db,
							in_tbl = &in_prvdr_tbl,
							field = mds_asmt_sbmt_cd,
							title_specs = %str(Expecting values of M or blank));													
%pre_extract_check(			setting = HOSP,
							control = &run_HOSPICE,
							in_db = &source_provider_db,
							in_tbl = &in_prvdr_tbl,
							field = hospc_ASMT_SBMT_CD,
							title_specs = %str(Expecting values of P or blank));

/*****************  extraction ************************************************************/
%put NOTE: define extraction macro needed ; 

%macro qtr_prvdr_xtract(	out_lib = ,
							out_table =,
							where_condition = ,
							in_schema = ,
							in_table = , 
							control = );
	/*	Only execute if control flag is set	*/
	%if &control = 1 %then %do;
	
		%select_to_dataset(

				%STR(select * , 
					current_date() as caads_dt 
				from 	&in_schema..&in_table
				where 	&where_condition),
				&out_lib..&out_table,
				use_dbx=Y);
	%end;

	%else %do;
			%put NOTE: control flag for &out_table set to &control. Tally not produced;
	%end;
%mEnd qtr_prvdr_xtract; 

%put NOTE: end macro definition ; 

%put NOTE: extract data  - call extract macro for each setting ; 
	

%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_irf, 
					where_condition = %str(IRF_ASMT_SBMT_CD LIKE 'R'), 
					in_schema =	&source_provider_db, 
					in_table = &in_prvdr_tbl, 
					control = &run_irf)
					
%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_ltch, 
					where_condition = %str(LTCH_ASMT_SBMT_CD LIKE 'L'), 
					in_schema =	&source_provider_db, 
					in_table = &in_prvdr_tbl, 
					control = &run_ltch)
							
%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_snf_sb.,
					where_condition = %str(SB_ASMT_SBMT_CD LIKE 'S'),
					in_schema =	&source_provider_db,
					in_table = &in_prvdr_tbl, 
					control = &run_snf)

%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_snf, 
					where_condition = %str(category in ('02','03','04','10')), 
					in_schema =	&source_provider_db_snf, 
					in_table = &in_prvdr_tbl_snf, 
					control = &run_snf)		
					
%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_snf_csp.,
					where_condition = %str(mds_asmt_sbmt_cd LIKE 'M'),
					in_schema =	&source_provider_db,
					in_table = &in_prvdr_tbl, 
					control = &run_snf)					
					
%qtr_prvdr_xtract(	out_lib = work, 
					out_table = &out_prvdr_hospice, 
					where_condition = %str(hospc_asmt_sbmt_cd like 'P'),
					in_schema =	&source_provider_db, 
					in_table = &in_prvdr_tbl, 
					control = &run_hospice)


					
%put NOTE: ***	End extract data ***	;

/*************************************** QA checks *********************************/
%put NOTE: *** QA checks	***	;

/********** check extracted data against expected using control table *******/
%put NOTE: define macros needed for control table processing and related QA ; 

%macro check_formats(
    setting = ,
    lib = ,
    table = ,
	field_name = );

   
    /* create a string of char-valued column names separated by a 
    /* ' ' character that are in both the assessment table and control file */
    proc sql noprint;
        select prvdr.var_name 
		into :list_raw separated by ' '
        from  ( select name as var_name
				from dictionary.columns
				where
            	libname = upcase("&lib") and
            	memname = upcase("&table")) as prvdr
			right join
			control_&setting._clean as cntrl
			on prvdr.var_name = cntrl.&field_name
		;
    quit;

    title "Check that all &setting fields required for analysis are present and in the &table source provider data";
    proc freq data = &lib..&table nlevels;
        tables &list_raw / noprint;
    run;

	proc sql;
		create table &setting._columns_check as
			select
				prvdr.var_name as actual_name,
				cntrl.source_file_name as expected_name,
				prvdr.type as actual_type,
				cntrl.var_type as expected_type
			from  ( select name as var_name,
							type
				from dictionary.columns
				where
            	libname = upcase("&lib") and
            	memname = upcase("&table")) as prvdr
			right join
			control_&setting._clean as cntrl
			on prvdr.var_name = cntrl.&field_name;
		;
	quit;

	title "Check expected variable type for &setting." ; 
	title2 "should match along the diagonal" ; 
	proc freq data = &setting._columns_check;
		table actual_type * expected_type / missing nocum norow nopercent;
		where expected_type ~= ""; 
	run;
%mend check_formats;  

 
%macro process_control_file(setting=,
							/* raw_in no longer needed */
							raw_sheet=,
							raw_out = work.control_&setting._raw) ; 
	
	/* common macro defined in another program - see support programs/APU_common_macros_[date].sas for details */
	%set_control_file(
							setting= &setting,
							/* use defaut definition */
							ctrl_sheet= &raw_sheet ,
							ctrl_out = &raw_out);

	
	data work.control_&setting._clean;
		set &raw_out (
			where=(
				Source_File_Name ~= "prvdr_num" and
                Source_File_Name ~= "caads_dt" and
                Source_File_Name ~= "prvdr_intrnl_num" and
				Source_File_Name ~= "facid" and
				Source_File_Name ~= "mcare_id"));
	run;
%mend process_control_file; 

 

/* macro calls */
%if &run_irf = 1 %then %do;

	%process_control_file(setting= IRF,
							/* use default def for control in so raw_in no longer needed*/
							raw_sheet= IRF_Provider,
							raw_out = work.control_IRF_raw) ;

    %check_formats(
        setting = IRF,
		lib = work,
		table = &out_prvdr_irf,
		field_name = source_file_name); 

	
%end; 

%if &run_ltch = 1 %then %do;

	%process_control_file(setting= LTCH,
							/* use default def for control in so raw_in no longer needed*/
							raw_sheet= LTCH_Provider,
							raw_out = work.control_LTCH_raw) ;


    %check_formats(
        setting = LTCH,
		lib = work,
		table = &out_prvdr_LTCH,
		field_name = source_file_name);


%end;
 
%if &run_snf = 1 %then %do;

	%process_control_file(setting= SNF_SA,
							/* use default def for control in so raw_in no longer needed*/
							raw_sheet= SNF_Provider,
							raw_out = work.control_SNF_raw) ;

    %check_formats(
        setting = SNF_SA,
		lib = work,
		table = &out_prvdr_snf,
		field_name = source_file_name);


	%process_control_file(setting= SNF_CSP,
							/* use default def for control in so raw_in no longer needed*/
							raw_sheet= SNF_SB_Provider,
							raw_out = work.control_SNF_SB_raw) ;
	%check_formats(
        setting = SNF_CSP,
		lib = work,
		table = &out_prvdr_snf_sb,
		field_name = source_file_name);	

	%check_formats(
        setting = SNF_CSP,
		lib = work,
		table = &out_prvdr_snf_csp,
		field_name = source_file_name);
	
%end; 

%if &run_hospice = 1 %then %do;

	%process_control_file(setting= HOSP,
							/* use default def for control in so raw_in no longer needed*/
							raw_sheet= HOSP_Provider,
							raw_out = work.control_HOSP_raw) ;


    %check_formats(
        setting = HOSP,
		lib = work,
		table = &out_prvdr_hospice,
		field_name = source_file_name);
	
	/* dev: hospice requires a different lib than IRF, LTCH, SNF */
	
%end; 



/* ****************************************	QA Checks- check CCN 3 & 4 digit	*************************************/
%macro check_ccn(	check_table = , 
					ccn= , 
					control = );
					
	%if &control=1 %then %do;
		proc sql;
			select 	distinct
					substr(&ccn, 3, 2) as CCN_Check
				from	&check_table
			;
		quit;	
	%end;
	
	%else %do;
		
			%put NOTE: control flag for &check_table set to &control. Tally not produced.	;
	%end;

%mEnd check_ccn; 	 
 
/* 	######################		CALL 3&4 position MACROS	######################			 */
title	"QA Check -- CCN digits 3 and 4 for each output";
title2	"IRF -- should be '30', 'R#', or 'T#'";
%check_ccn(	check_table = work.&out_prvdr_irf., 
			ccn=prvdr_num, 
			control=&run_irf)

title2	"LTCH -- should be 20-22";
%check_ccn(	check_table = work.&out_prvdr_ltch, 
			ccn=prvdr_num, 
			control = &run_ltch)

title2	"SNF -- SNFs are 50-64, but there is a lot of junk in this table for SNF";
%check_ccn(	check_table = work.&out_prvdr_snf, 
			ccn=mcare_id, 
			control =  &run_snf)

title2	"SNF -- SBs are from 00 thru 08 and 20,30";
%check_ccn(	check_table = work.&out_prvdr_snf_sb, 
			ccn=prvdr_num, 
			control =  &run_snf)
			
title2	"SNF from CSP_PRVDR_CMN -- SNFs are 50-64, but there is a lot of junk in this table for SNF";
%check_ccn(	check_table = work.&out_prvdr_snf_csp, 
			ccn=prvdr_num, 
			control =  &run_snf)			

title2	"Hospice -- should be 15-17";
%check_ccn(	check_table = work.&out_prvdr_hospice, 
			ccn=prvdr_num, 
			control = &run_hospice )
title;


*	QA Checks- check CCN unique values, length, garbage like null values and past quarter comparison	;
*****************************************	QA for CCN counts	************************************; 
%macro check_count(	check_table =, 
					ccn=, 
					control = );
					
	%if &control = 1 %then %do;
	
		title "Total count and unique count";
		title2 "For &check_table";
		proc sql;
			select 	count(*) as Provider_Count, 
					count(distinct(&ccn)) as uniq 
			from &check_table;
		quit;
		
		title "CCN conforms to six alphanumeric characters. checking - &ccn_pattern_all";
		title2	"1 = Conforms, 0 = does not conform";
		title3 "For &check_table";
		proc sql;
			select 	prxmatch("&ccn_pattern_all", &ccn) as conforming , 
					count(*) as totalct 
			from 	&check_table 
			group by calculated conforming;
		quit;
		
		title "checking if CCN length other than 6 are null";
		title2 "For &check_table";
		 proc sql;
			select  missing(&ccn) as Is_Null, 
					count(*) as Providers	
			from &check_table 
			group by calculated is_null;
		 quit;
		 
	%end;
	
	%else %do; 
		%put NOTE: control flag for &check_table set to &control. Tally not produced ; 
	%end;
	
%mEnd check_count; 

*****************************************	QA for null CCNs	************************************;

/* ######################	CALL CCN counts & null CCN MACROS	###################### */
title	"QA Check -- number of providers. This should be in line with past quarters' extracts";
title3	"IRF";
%check_count(	check_table = work.&out_prvdr_irf, 
				ccn=prvdr_num, 
				control = &run_irf)

title3	"LTCH";
%check_count(	check_table = work.&out_prvdr_ltch, 
				ccn=prvdr_num, 
				control = &run_ltch)

title3	"SNF- SA";
%check_count(	check_table = work.&out_prvdr_snf, 
				ccn=mcare_id, 
				control =  &run_snf)

title3	"SNF- SB";
%check_count(	check_table =	work.&out_prvdr_snf_sb, 
				ccn=prvdr_num, 
				control =  &run_snf)
				
title3	"SNF- CSP";
%check_count(	check_table =	work.&out_prvdr_snf_csp, 
				ccn=prvdr_num, 
				control =  &run_snf)				

title3	"Hospice";
%check_count(	check_table = work.&out_prvdr_hospice, 
				ccn=prvdr_num, 
				control = &run_hospice)

title;

* QA -- characterize providers that have been added to or removed from provider extract compared to the past quarter.
		Check to see if they are new, newly in the system, terminated, etc.
		This may prompt additional data exploration.
		;


*	SQL  select statement for providers pulled from CASPER (e.g., everything that's not SNF);
%put NOTE: For SNF the compare results are showing up labels as opposed to column names, might have to do something with CVP changing the default settings;

%let compare_select_csp =
	%str(	coalesce(curr.prvdr_num,
					old.prvdr_num) as prvdr_num,
			old.fac_name as old_fac_name,
			curr.fac_name as curr_fac_name,
			old.csp_prvdr_add_dt as old_csp_prvdr_add_dt,
			curr.csp_prvdr_add_dt as curr_csp_prvdr_add_dt,
			old.orgnl_prtcptn_dt as old_orgnl_prtcptn_dt,
			curr.orgnl_prtcptn_dt as curr_orgnl_prtcptn_dt,
			old.trmntn_exprtn_dt as old_trmntn_exprtn_dt,
			curr.trmntn_exprtn_dt as curr_trmntn_exprtn_dt)
		;
					
*	SQL select statement for SNF;
%let compare_select_snf =
	%str(coalesce(	curr.mcare_id,
					old.mcare_id) as mcare_id,
		old.name as old_fac_name,
		curr.name as curr_fac_name,
		old.partci_dt as old_orgnl_prtcptn_dt,
		curr.partci_dt as curr_orgnl_prtcptn_dt,
		old.closeddate as old_closeddate,
		curr.closeddate as curr_closeddate)	
	; 
					

%macro	compare_old(pac =,
					ccn = prvdr_num,
					control = ,
					check_table = ,
					old_table =,
					compare_table =,
					select =,
					extranote =);

	%if &control = 1 %then %do;				

		data	work.old_table_&pac;
			set	&old_table;
			if ~missing(&ccn);
		run;
	
		proc sql;
			create table &compare_table as
				select	&select
				from	&check_table curr
							full join
						work.old_table_&pac old
							on curr.&ccn = old.&ccn
				where	missing(curr.&ccn)
							or
						missing(old.&ccn)
			;
		quit;
		
		/*	This may need to be updated to handle cases in which there have been no changes in provider list...	*/
		title	"QA -- Provider change, &pac";
		title2	"Check to see if providers that dropped out of the extract termed or otherwise have been modified.";
		
		proc sql;
			select count(*) as Total_Changes
					into :new_providers
			from &compare_table
			where &ccn ~= ""
			;
		quit;
		
		%if &new_providers > 0 %then %do;
		
			title3	"Check to see if providers added to the extract -- if any -- are new.";
			title4	"You may need to explore data in more depth.";
			%if %length(&extranote) > 0 %then %do;
				title5	"&extranote";
			%end;
			
				proc sql;
					select *
					from &compare_table
					where &ccn ~= ""
					;
				quit;
			
		%end;
		
		title;
		
	%end;
	
	%else %do;
	
		%put NOTE: Control flag for &pac set to &control -- provider flux QA not run;
		
	%end;				
%mEND compare_old;
		
%compare_old(	pac = irf,
					ccn = prvdr_num,
					control = &run_irf,
					check_table = work.&out_prvdr_irf,
					old_table = &old_irf,
					compare_table = prvdr_comp_irf,
					select = &compare_select_csp,
					extranote = %str(For IRFs, subunits may have an add date that matches their parent -- this is probably not recent. Focus on OPD.)) ;

%compare_old(	pac = ltch,
					ccn = prvdr_num,
					control = &run_ltch,
					check_table = work.&out_prvdr_ltch,
					old_table = &old_ltch,
					compare_table = prvdr_comp_ltch,
					select = &compare_select_csp) ;

%compare_old(	pac = snf,
					ccn = mcare_id,
					control =&run_snf,
					check_table = work.&out_prvdr_snf,
					old_table = &old_snf,
					compare_table = prvdr_comp_snf,
				select = &compare_select_snf) ;

%compare_old(	pac = snf_sb,
					ccn = prvdr_num,
					control = &run_snf,
					check_table = work.&out_prvdr_snf_sb,
					old_table = &old_snf_sb,
					compare_table = prvdr_comp_snf_sb,
					select = &compare_select_csp,
					extranote = %str(There is no time dimension for when providers start serving swingbeds and most providers have been open for a while. This is more for awareness than QA.)) ;

/*	No Compare_old for snf_csp as of 2022-Q2 APU. We may not necessarily care to run this check in any event since currently the SNF CSP table is only going to
	be used to fill holes in the authoritative table.	*/
				
%compare_old(pac = hospice,
					ccn = prvdr_num,
					control = &run_hospice,
					check_table = work.&out_prvdr_hospice,
					old_table = &old_hospice,
					compare_table = prvdr_comp_hospice,
					select = &compare_select_csp) ; 


/*****************************************************************************/

%put NOTE: *** End QA	***	;

%put NOTE: *** Output	***; 
*	OUTPUT	;
%macro prvdr_out(pac = ,
				control = ,
				work_file = ,
				out_file = );
		
	%if &control = 1 %then %do;	
		
		data &out_file (compress=YES);
			set &work_file;
		run;
		
	%end;
	
	%else %do;
	
		%put NOTE: &pac control set to &control, no output file written;
		
	%end;
				
%mEnd prvdr_out;  

%prvdr_out(pac = irf, 
			control=&run_irf, 
			work_file=work.&out_prvdr_irf, 
			out_file=out_dir.&out_prvdr_irf)

%prvdr_out(pac = ltch,
			control=&run_ltch, 
			work_file=work.&out_prvdr_ltch, 
			out_file=out_dir.&out_prvdr_ltch)

%prvdr_out(pac = snf,
			control=&run_snf, 
			work_file=work.&out_prvdr_snf, 
			out_file=out_dir.&out_prvdr_snf)

%prvdr_out(	pac = snf_sb,
			control = &run_snf,
			work_file=work.&out_prvdr_snf_sb,
			out_file = out_dir.&out_prvdr_snf_sb)

%prvdr_out(	pac = snf_csp,
			control = &run_snf,
			work_file=work.&out_prvdr_snf_csp,
			out_file = out_dir.&out_prvdr_snf_csp)
			
%prvdr_out(pac = hospice,
			control = &run_hospice,
			work_file=work.&out_prvdr_hospice,
			out_file = out_dir.&out_prvdr_hospice) */

%put NOTE: End active code ; 
