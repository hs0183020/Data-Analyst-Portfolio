/****** TITLE: ALL_Quarterly_Asmt_Extract_
/
/******

/****** PURPOSE: The program extracts assessment files from ASSESSMENT_IQIES for downstream use 
				 in assembly programs and APU logic for all four PACs. Extracts from assessment
				 tables and historical assessment tables are taken here.

/****** NOTES:	To update this file review the macro variable definitions. 
                The date, reporting quarter, and type parameters need to be updated each quarter/run.
				The database reference, and libname statements can be 
				modified as needed.
				
				This code covers the following processes: 
				- 	Libname references and parameter definition via macro variables 
				-	Creation of extracting macro and instructions for calling the macro
				-	Extraction assessments from IQIES and save to PAC Project workbench using the extracting macro
				- 	Creation of QA macro for checking row counts (including calling prev q rowcount), dates and CCNs included in the extract
				-	QAing extracted assessments

 

/****** UPDATES:
	01-JUL-2021 (PP):	updated from 2020-Q4 APU production (_210518_DTA) version
						- 	blocked off bonus QA code to prevent errant executions
						-	removed old Hive references now that we use workbench

	06-JUL-2021 (PP): 	ran with updated parameters post-CDR refresh

	15-JUL-2021 (PP): removed extraneous parameter reference to project Hive
	
	8/4/2021:	- Fixed problem w/ QA of hospice date ranges -- previously displayed IRF/LTCH/SNF expected date ranges instead of hospice
				- Fixed QA for A310X parameters -- QA for SNF historical actually checked against SNF main assessment
	
	10/05/2021: Updated the control flag and the libref for OR Q2, 
				Updated the libref for previous quater outputs from "&out_dir/ &pac_folderr" (which resulted in error OLD_IRF not found) to  "&out_dir/&subfolder" 
	10/18/2021: Production run for Q2 (no changes to program)
	
	11/8/2021: 2021 Q2 APU development update
					Parameters updated
					Streamlined parameter updates
					Added program flow log notes

	11/18/2021: 2021 Q2 APU Production update
						No changes made to code

	12/20/2021 (EO): 2021 Q3 Outreach pre-development updates
		-	parameters updated
		-	amended a few titles, added two titles for clarification in pre-extract check of SNF db

	01/18/2022 (EO): Q3 OR production run - no changes

	02/04/2022 (Raki) :Q3 APU Dvelopment run - updated the parameters.
	02/18/2022 (Raki) : Q3 APU Production Run


	2021-Q4 APU - Hospice only
	2/22/2022 (EO): pre-development work for Hospice Q4 

	2021-Q4 Outreach
		03/03/2022 (EO): pre-development work for Outreach Q4
			-	added more notes to log in parameters section
			
		4/4/2022 -- Multiple updates to implement data validation. Most of these are from Kwame -- Alex made a few minor edits.
					This code is, however, not ready for a clean run and will NOT be used for 2021-Q4 OR Development.

	04/28/2022 (KAB) 2021-Q4 APU Pre-Development
		- created check_numerical_range() to replace check_ids_nums() based on 
		  dynamic creation of variables in the control file
		- created make_source_file_array(), make_source_file_array_num, and 
		  make_source_file_array_date to facilitate the check_numerical_range() macro
		- changed calls from check_ids_nums() to check_numerical_range()
		- changed lengthn(&ccn) to return 0 for null values
		- added check_formats() macro definition
		- added check_hospice_formats() macro call behind run_hosp = 1 control gate
		- changed all run_* control gates to 1 for pre-development
		- added pre-development variable definitions for Hospice setting
		- changed left join to right join in check_hospice_formats()
		- changed inner join to right join in check_hospice_formats

	05/05/2022 (KAB) 2021-Q4 APU Development
		- set variable definitions for development data run
		- made {q_start, q_end, q_start_hosp, q_end_hosp} dates dynamic based on {pull_start_hive, pull_end_hive, pull_start_hospice, pull_end_hospice}
		- changed parameter call inside run_hsop control gate for set_control_file_dated() to use q_start_hosp and q_end_hosp arguments
		- added folder_prev_hosp variable definintion
		- added old_hosp library name definition
		- changed qa_xtrct() calls for hospice to use prev_lib = old_hosp instead of prev_lib = 
		- changed check_formats() and check_hospice_formats() to use control parameter

	05/23/2022 (EO) 2021-Q4 APU production
		- prod run with all settings
	05/23/2022 (EO) post-production fix
		- cleared weird maps reference in log for Hospice format check
		- added hopsice hist to check formats call

	06/30/2022 (EO) 2022-Q1 OR development
		-	updated header for use of maintenance
		-	updated parameters for new reporting year
		-	removed a duplicate parameter definition and aligned downstream call for it
	07/05/2022 (EO) run for development
	07/05/2022 (EO) post run changes
		-	QA portion failed
		-	fixed parameter assignments that caused failed
		-	reran in portions
	07/05/2022 (EO) post rerun changes
		-	dang stamp was used in filenaming of outputs
	07/06/2022 (EO) post REFRESH data pull, lol

	07/19/2022 EO 22Q1 or production pull
	
	7/26/2022 KAS
		- Updated for 2022Q1 APU
		- Uses new features from the common macro program
		- Split off most post-extract QA into a new program -- run this program, then run QA separately!
	8/02/2022 EO
		- added put notes to log when common macros used
		- mprint option was turned on at top already, I added code to turn off at bottom
		
	8/4/2022 KAS
		- Fixed issue with output naming
		
	8/17/2022 KAS
		- Production run -- no updates.

	10/05/2022 EO development for 2022-Q2 Outreach
		- expected changes only

	10/18/2022 (CT) -- Production run for 2022-Q2 OR
	
	11/02/2022 -- KAS
		- Dev for 2022-Q2 APU

	11/18/2022 -- EO
		- prod for 22Q2 apu

	12/02/2022 -- CT
		- special re-run for Hospice 2022-Q3 APU

	03/06/2023 -- EO
		- development for 22-Q4 Outreach

	4/19/2023 -- EO
		- production run for 22-Q4 OR for SNF only
		- common macro control flags are set to 0 for IRF and LTCH
		- fail, issue in snf hist too
	
	4/20/2023 -- EO
		- prod run for 22-Q4 OR all settings relevant :)

	05/03/2023 -- SS
		- development for 22-Q4 APU

	05/18/2023 -- EO
		- production run for 22-Q4 APU (+23Q1 Hospice)

	07/05/2023 -- EO
		- development for 23-Q1 Outreach
		- for now, left SNF HIST table pull to see if it has any relevant data
			the change over was at beginning of Q2 2023, so there should still be data
			for Q1-Q2 target dates existing there

	07/18/2023 -- EO
		- prod for 23-Q1 Outreach
	
	08/04/2023 -- CT
		- Development run for 2023-Q1 APU

	08/17/2023 -- EO
		- prod run 23q1 apu
	
	10/11/2023 (CT) -- Development run for 2023-Q2 OR
					--  updated all cdr calls to use explicit connection
						to DBX using the ISG supplied macro
					-- Databricks calls are successful for all but SNF asmt dataset;
					   hence, using old extract call for SNF 
					   but leaving the DBX SNF call in the code for debugging/ServiceNow ticket.

	10/12/2023 (EO) -- changed quotes to double in snf spec definition
						added %str to definition as well. still fails in DBX connect

	10/16/2023 (CT) -- Development run for 2023-Q2 OR
					-- For the tranisition phase, divided the code into two sections -- Databricks vs Hadoop
					-- Created flags for the Database connection type -- running Hadoop for this run

	10/18/2023 (EO)	-- Production run for 2023-Q2 OR

	11/07/2023 (SS)	-- Development run for 2023-Q2 APU

	11/20/2023 (EO) -- Production run 23Q2 APU

	12/20/2023 (SS) -- Development for 23Q3 OR

	12/29/2023 (SS) 
					-- Commented out extraction and proc tabulate code related to SNF HIST extracts as 
                       the SNF HIST table is no longer maintained as of 2023 Q3.
					-- Below update only for Databricks section of the code:
					-- Updated to use TO_DATE function and BETWEEN for date comparisons in WHERE statements 
                       due to DBX code migration changes.

	01/02/2024 (EO) -- dev run post decomission of Ambari 23Q3 OR

	01/16/2024 (EO) -- prod run 23Q3 OR
					-- removed the hadoop libname statement 
					-- rewrote the SNF precheck to use a dbx connect

	02/09/2024 (HS) -- dev run 23Q3 APU

	03/01/2024 (HS) -- prod run 23Q3 APU

	03/04/2024 (HS) -- prod re-run 23Q3 APU

	04/05/2024 (SS) -- dev run 23Q4 OR

	04/15/2024 (HS) -- prod run 23Q4 OR

	05/02/2024 (HS) -- dev run 23Q4 APU

/*****************************************END HEADER********************************************/
%put NOTE: END header;
%put NOTE: Program parameters;

* OPITONAL: use MPRINT and MLOGIC options to troubleshoot macros;
options mprint;  /* Enables MPRINT */
options mlogic;  /* Enables MLOGIC */


/* Common macros + location variables	*/
%let bench = /workspace/workbench/pac_qrp_swingtech/data;
%let qrp_dir = SAS/Quarterly and Annual Reporting;
%let folder = 2023-Q4 APU;

%put NOTE: the include statement for the common macro program will write notes to the log from that program ; 
%put NOTE: additional put statements will be written to the log in the program at hand when a common macro is used ; 
%include "&bench/&qrp_dir/&folder/Support Programs/APU_Common_Macros_20240422.sas";

%put NOTE: control flag set up and check; 

* ##### Control flags to selectively run extract (=1) for Outreach (IRF/LTCH/SNF) or APU (IRF/LTCH/SNF/Hospice);
	
%let run_hosp 	= &cmn_run_hosp;
%let run_irf 	= &cmn_run_irf;
%let run_ltch 	= &cmn_run_ltch;
%let run_snf 	= &cmn_run_snf;

* Double check that flags are correct for each PAC;
%put NOTE: &=run_hosp;
%put NOTE: &=run_irf;
%put NOTE: &=run_ltch;
%put NOTE: &=run_snf;
	
%put NOTE: assign libraries to workbenches needed AND hive database; 

* ##### Assign libref parameters and Establish librefs;
* Workbench reference - update as needed; 

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

%put NOTE: parameters with updates needed each cycle --- more often if needed; 

* Assign parameters - mostly autofilled from common macro program parameters;

* ##### Date parameters - autofilled from common macro program parameters;
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

* ##### date parameters in SAS form;
* used in preextract QA for SNF table only; 
%let pull_start_sas = &cmn_qtr_start;
%put NOTE: &=pull_start_sas ; 
%let pull_end_sas = &cmn_qtr_end;
%put NOTE: &=pull_end_sas ; 

* ##### Reporting quarter parameter - autofilled from common macro program parameters;
* Note: Hospice is a quarter ahead;
%let report_qtr = &cmn_yy.&cmn_qq;
%put NOTE: &=report_qtr ; 
%let report_qtr_hosp = &cmn_yy_hosp.&cmn_qq_hosp;
%put NOTE: &=report_qtr_hosp ; 

* Assign parameters - these autofill based on the previous parameter chunk;

* Output naming - update if needed;
* Files named _ASMT are "current" assessments. Files named _HIST are historical.  
* Note: Hospice is a quarter ahead so it has its own base naming parameter;
%let out_root = asmt_xtrct_&report_qtr._;
%let out_root_hist = hist_xtrct_&report_qtr._;
%let out_root_hosp = asmt_xtrct_&report_qtr_hosp._;
%let out_root_hist_hosp = hist_xtrct_&report_qtr_hosp._;

%let pull_date = %sysfunc(today(), yymmddn6.);

%let extract_date = &pull_date;
%let extract_date_hosp = &pull_date;

%let out_asmt = &out_root.&extract_date;
%let out_asmt_hosp = &out_root_hosp.&extract_date_hosp.;
%let out_hist = &out_root_hist.&extract_date.;
%let out_hist_hosp = &out_root_hist_hosp.&extract_date_hosp.;

* Dynamically generate output names for each PAC from above parameters;
%let out_hosp = HOSPC_&out_asmt_hosp;
%let out_irf = IRF_&out_asmt;
%let out_ltch = LTCH_&out_asmt;
%let out_snf = SNF_&out_asmt; 
%put	NOTE: Output filenames:
		&=out_hosp
		&=out_irf
		&=out_ltch
		&=out_snf
		;

%let out_hosp_hist = HOSPC_&out_hist_hosp;
%let out_irf_hist = IRF_&out_hist;
%let out_ltch_hist = LTCH_&out_hist;
%let out_snf_hist = SNF_&out_hist; 

%put	NOTE: Output filenames (history files):
		&=out_hosp_hist
		&=out_irf_hist
		&=out_ltch_hist
		&=out_snf_hist
		;

*Databricks amst output files;
%let out_hosp_dbx = HOSPC_dbx_&out_asmt_hosp;
%let out_irf_dbx = IRF_dbx_&out_asmt;
%let out_ltch_dbx = LTCH_dbx_&out_asmt;
%let out_snf_dbx = SNF_dbx_&out_asmt; 
%put	NOTE: Databricks Output filenames:
		&=out_hosp_dbx
		&=out_irf_dbx
		&=out_ltch_dbx
		&=out_snf_dbx
		;


*Databricks amst history output files;
%let out_hosp_hist_dbx = HOSPC_dbx_&out_hist_hosp;
%let out_irf_hist_dbx = IRF_dbx_&out_hist;
%let out_ltch_hist_dbx = LTCH_dbx_&out_hist;
%let out_snf_hist_dbx = SNF_dbx_&out_hist; 

%put	NOTE: Databricks history Output filenames (history files):
		&=out_hosp_hist_dbx
		&=out_irf_hist_dbx
		&=out_ltch_hist_dbx
		&=out_snf_hist_dbx
		;

* Input (iqies) table parameters;
%let in_irf = IRF_ASMT_FED;
%let in_ltch = LTCH_ASMT_FED;
%let in_snf = MDS_ASMT_FED;
%let in_hosp = HOSPC_ASMT_FED;

* Input IQIES historical table parameters;
%let in_irf_hist = IRF_ASMT_FED_HSTRY;
%let in_ltch_hist = LTCH_ASMT_FED_HSTRY;
%let in_snf_hist = MDS_ASMT_FED_HSTRY;
%let in_hosp_hist = HOSPC_ASMT_FED_HSTRY;

*Control flag to determine database connection type -- Hadoop vs Databricks;
%let pull_hadoop = 0;
%put NOTE: &=pull_hadoop ;

%let pull_databricks = 1;
%put NOTE: &=pull_databricks ;


 
************************************************	END PARAMETERS	***************************************************;
%put NOTE: END parameters; 

* Create macro for data extraction across all 4 PAC settings
	* PROC SQL extracts assessments from the appropriate IQIES table within 
		the pre-specified reporting dates for the quarter

	* Preset values 
 	-	runspec = &CONNECT_TO_HADOOP --> calls the CVP-desgined macro 
	-	in_db = &iqies --> specifies that tables are extracted from assessment_iqies

	* Values to be filled in on running:
	-	control --> use the pre-defined &run_xxxx parameters for each PAC
						1 = will run extract
						0 = will not run extract
	-	out_db --> specifies output (extracted) table will be written to folder on workbench
	-	out_table --> name of the output extract (use output parameters defined above)
	-	in_table --> source of assessments data (use input parameters defined above) 
	-	pull_start and pull_end --> refer to pre-specified parameters for quarter dates 
	-	add_spec --> filter for admissions and discharges only (SNF), could be expanded
					 for any additional specifications we want to add later ;


/*====================== Start of Databricks Section ==============================*/



/* Macro to extract using Databricks connection */
%put	NOTE: macro function definition to Extract assessments using Databricks	;

	%MACRO xtrct_asmt_databricks (	control = ,
									out_table = ,
									in_table = ,
									pull_start = ,
									pull_end = ,
									add_spec = ,	
									out_db = lb_asmt, 
									in_db = &iqies.) ;
	%IF &control = 1 %THEN %DO ;

		%select_to_dataset(
			%STR(SELECT	*,
						current_date() as caads_dt
				 FROM	&in_db..&in_table 
				 	WHERE	/*DATE("&pull_start") <= to_date(trgt_dt,'YYYY-MM-DD')
							AND to_date(trgt_dt,'YYYY-MM-DD') <= DATE("&pull_end")*/
							to_date(trgt_dt,'YYYY-MM-DD') between DATE("&pull_start") and DATE("&pull_end")
							&add_spec
				),
			&out_db..&out_table,
			use_dbx=Y);
	%END ;
	
	%ELSE %DO ;
		%PUT	NOTE: Not creating &out_table -- control flag set to &control ;	
	%END ;
	%mEnd xtrct_asmt_databricks ;

%put NOTE: Extract asmt per control flag settings ; 




* Databricks calls;
%MACRO databricks_calls;

/* LTCH */
%xtrct_asmt_databricks 	(control = &run_ltch,
				out_table = &out_ltch_dbx, 
				in_table  = &in_ltch,
				pull_start = &pull_start_hive,
				pull_end = &pull_end_hive) ;




/* LTCH historical */
%xtrct_asmt_databricks 	(control = &run_ltch,
				out_table = &out_ltch_hist_dbx, 
				in_table  = &in_ltch_hist,
				pull_start = &pull_start_hive,
				pull_end = &pull_end_hive) ;



/* IRF */
%xtrct_asmt_databricks 	(control = &run_irf,
				out_table = &out_irf_dbx, 
				in_table = &in_irf,
				pull_start  = &pull_start_hive,
				pull_end = &pull_end_hive) ;

/* IRF historical */
%xtrct_asmt_databricks 	(control = &run_irf,
				out_table = &out_irf_hist_dbx, 
				in_table = &in_irf_hist,
				pull_start  = &pull_start_hive,
				pull_end = &pull_end_hive) ;


%if &run_snf = 1  %then %do;

	TITLE	"Check parameter values - SNF assessments" ;
	title2 "Expecting values for a0310b_pps_cd = 01 
						 and a0310h_ppsprta_dschrg_cd = 1" ; 

	%hive_exec_sql( %STR(

           select a0310b_pps_cd ,
					count(*) as num_codes
		from	assessment_iqies.MDS_ASMT_FED
		WHERE	to_date(trgt_dt, 'YYYY-MM-DD') between DATE("&pull_start_hive") and DATE("&pull_end_hive")
		group by a0310b_pps_cd),
	use_dbx=Y);
	%hive_exec_sql( %STR(

           select a0310h_ppsprta_dschrg_cd ,
					count(*) as num_codes
		from	assessment_iqies.MDS_ASMT_FED
		WHERE	to_date(trgt_dt, 'YYYY-MM-DD') between DATE("&pull_start_hive") and DATE("&pull_end_hive")
		group by a0310h_ppsprta_dschrg_cd),
	use_dbx=Y);
	
	TITLE ;

/*	TITLE	"Check parameter values - SNF historical assessments" ;
	title2 "Expecting values for a0310b_pps_cd = 01 
						 and a0310h_ppsprta_dschrg_cd = 1" ;
	PROC TABULATE	DATA = lb_iqies.&in_snf_hist 
					(WHERE = ( to_date(trgt_dt,'YYYY-MM-DD') between "&pull_start_sas"d and "&pull_end_sas"d)) ;
		CLASS	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd /MISSING ;
		TABLE	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd ;
	RUN ;
	TITLE ;*/
%end;

/* SNF Additional assessment parameters - update as needed based on above check */
%LET snf_spec = %str(AND (a0310b_pps_cd = "01" 
						OR a0310h_ppsprta_dschrg_cd = "1")) ;



/* SNF */
%xtrct_asmt_databricks 	(control = &run_snf,
				out_table = &out_snf_dbx, 
				in_table = &in_snf,
				pull_start=&pull_start_hive,
				pull_end=&pull_end_hive,
				add_spec =	&snf_spec) ;



/* SNF historical */
/*%xtrct_asmt_databricks 	(control = &run_snf,
				out_table = &out_snf_hist_dbx, 
				in_table = &in_snf_hist,
				pull_start=&pull_start_hive,
				pull_end=&pull_end_hive,
				add_spec =	&snf_spec) ;*/


/* Hospice */
%xtrct_asmt_databricks 	(control = &run_hosp,
				out_table = &out_hosp_dbx, 
				in_table = &in_hosp,
				pull_start = &pull_start_hive_hosp,
				pull_end = &pull_end_hive_hosp) ;


/* Hospice historical */
%xtrct_asmt_databricks 	(control = &run_hosp,
				out_table = &out_hosp_hist_dbx, 
				in_table = &in_hosp_hist,
				pull_start = &pull_start_hive_hosp,
				pull_end = &pull_end_hive_hosp) ;


%MEND databricks_calls;


%IF &pull_databricks = 1 
	%THEN %DO ;

	%put NOTE: Using Databricks Connection;
	%databricks_calls;
	%END ;
	
	%ELSE %DO ;
		%PUT	NOTE: Not Using Databricks Connection;	
%END ;


/*====================== End of Databricks Section ==============================*/


/*====================== Start of Hadoop Section ==============================*/


/* Macro to extract using Hadoop connection */
%put NOTE: macro function definition to Extract assessments using Hadoop connection	;

	%MACRO xtrct_asmt_hadoop (	control = ,
						out_table = ,
						in_table = ,
						pull_start = ,
						pull_end = ,
						add_spec = ,
						runspec = &CONNECT_TO_HADOOP.,			
						out_db = lb_asmt, 
						in_db = &iqies.) ;
	%IF &control = 1 %THEN %DO ;

		PROC SQL ;
			&runspec ;
			CREATE TABLE &out_db..&out_table AS
			SELECT	*
			FROM	CONNECTION TO HADOOP
					(SELECT	*,
							current_date() as caads_dt
					 FROM	&in_db..&in_table 
					 	WHERE	DATE("&pull_start") <= trgt_dt
								AND trgt_dt <= DATE("&pull_end")
								&add_spec
				);
			DISCONNECT FROM HADOOP ;
		QUIT ;
	%END ;
	
	%ELSE %DO ;
		%PUT	NOTE: Not creating &out_table -- control flag set to &control ;	
	%END ;
	%mEnd xtrct_asmt_hadoop ;



%put NOTE: Extract asmt per control flag settings ; 


* Hadoop calls;
%MACRO hadoop_calls;

/* LTCH */
%xtrct_asmt_hadoop 	(control = &run_ltch,
				out_table = &out_ltch, 
				in_table  = &in_ltch,
				pull_start = &pull_start_hive,
				pull_end = &pull_end_hive) ;


/* LTCH historical */
%xtrct_asmt_hadoop 	(control = &run_ltch,
				out_table = &out_ltch_hist, 
				in_table  = &in_ltch_hist,
				pull_start = &pull_start_hive,
				pull_end = &pull_end_hive) ;



/* IRF */
%xtrct_asmt_hadoop 	(control = &run_irf,
				out_table = &out_irf, 
				in_table = &in_irf,
				pull_start  = &pull_start_hive,
				pull_end = &pull_end_hive) ;


/* IRF historical */
%xtrct_asmt_hadoop 	(control = &run_irf,
				out_table = &out_irf_hist, 
				in_table = &in_irf_hist,
				pull_start  = &pull_start_hive,
				pull_end = &pull_end_hive) ;

		
%if &run_snf = 1  %then %do;

	TITLE	"Check parameter values - SNF assessments" ;
	title2 "Expecting values for a0310b_pps_cd = 01 
						 and a0310h_ppsprta_dschrg_cd = 1" ; 
	PROC TABULATE	DATA = lb_iqies.&in_snf 
					(WHERE = ("&pull_start_sas"d <= trgt_dt <= "&pull_end_sas"d)) ;
		CLASS	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd /MISSING ;
		TABLE	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd ;
	RUN ;
	TITLE ;

/*	TITLE	"Check parameter values - SNF historical assessments" ;
	title2 "Expecting values for a0310b_pps_cd = 01 
						 and a0310h_ppsprta_dschrg_cd = 1" ;
	PROC TABULATE	DATA = lb_iqies.&in_snf_hist 
					(WHERE = ("&pull_start_sas"d <= trgt_dt <= "&pull_end_sas"d)) ;
		CLASS	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd /MISSING ;
		TABLE	a0310b_pps_cd
				a0310h_ppsprta_dschrg_cd ;
	RUN ;
	TITLE ;*/
%end;

/* SNF Additional assessment parameters - update as needed based on above check */
%LET snf_spec = %str(AND (a0310b_pps_cd = "01" 
						OR a0310h_ppsprta_dschrg_cd = "1")) ;

/* SNF */
%xtrct_asmt_hadoop 	(control = &run_snf,
				out_table = &out_snf, 
				in_table = &in_snf,
				pull_start=&pull_start_hive,
				pull_end=&pull_end_hive,
				add_spec =	&snf_spec) ;


/* SNF historical */
/*%xtrct_asmt_hadoop 	(control = &run_snf,
				out_table = &out_snf_hist, 
				in_table = &in_snf_hist,
				pull_start=&pull_start_hive,
				pull_end=&pull_end_hive,
				add_spec =	&snf_spec) ;*/



/* Hospice */
%xtrct_asmt_hadoop 	(control = &run_hosp,
				out_table = &out_hosp, 
				in_table = &in_hosp,
				pull_start = &pull_start_hive_hosp,
				pull_end = &pull_end_hive_hosp) ;

/* Hospice historical */
%xtrct_asmt_hadoop 	(control = &run_hosp,
				out_table = &out_hosp_hist, 
				in_table = &in_hosp_hist,
				pull_start = &pull_start_hive_hosp,
				pull_end = &pull_end_hive_hosp) ;


%MEND hadoop_calls;


%IF &pull_hadoop = 1
	%THEN %DO ;

	%put NOTE: Using Hadoop Connection;
	%hadoop_calls;
	%END ;
	
	%ELSE %DO ;
		%PUT	NOTE: Not Using Hadoop Connection;	
%END ;

/*====================== End of Hadoop Section ==============================*/



/*************************************************	END EXTRACTION	****************************************************/
/*** QA has been moved to its own file for ease of devleopment!	***/

* turn off mprint and mlogic options ; 
options nomprint;  /* Disables MPRINT */
options nomlogic;  /* Disables MLOGIC */
 

