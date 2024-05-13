/*	Common macros program
  
	This program is intended to serve as a repository for macros that are referenced by more than one type of program.
	
	Initial example: macros to import and QA worksheets from the control table.
	
	Add further macros as necessary.

	Syntax to reference this program:
	
	%include "this/files/path/and/name.sas";
	
	OR
	
	filename cmacro "this/files/path/and/name.sas";
	%include cmacro;
	
	Created 5/3/2022 -- KAS

	updates:
	06/30/2022 -- EO
		-	no changes, just prepped for 2022-Q1 OR
		
	7/28/2022 -- KAS
		- Added some global macros for cycle type, control table name, active folder, and reporting dates in different formats. 
		These are all prefixed with cmn_ and can be referenced by programs to streamline updating.
		
	8/2/2022 -- KAS
		- Added global macros for which PAC types to run
		- Control sheet defaults to importing the control table as defined by the common macros file
			- NOT doing this for dates as sometimes we need current quarter as limits and sometimes we need report range
		- Added tie in and term macro variables.

	10/03/2022 -- EO
		- prepped for 22Q2 Outreach
		
	11/02/2022 -- KAS
		- Updated for 2022-Q2 APU
		
	01/03/2023 -- KAS
		- Updated for 2022-Q3 OR. Also set up for Hospice Q4 while I was at it.
		- Added a check that validates whether the control file is correctly named.
	
	01/31/2023 - EO
		- updated for 22Q3 APU

	03/03/2023 - EO
		- updated for 22Q4 Hospice APU

	03/06/2023 - EO
		- updated for 22Q4 Outreach
		
	03/22/2023 -- KAS
		- Updated control table

	04/05/2023 -- EO
		- updated control table

	4/19/2023 -- EO
		- updated control flags for setting runs
		- we need to run assessment extract for SNF only
		- there is a data issue in CDR affecting IRF and LTCH
	4/19/2023 -- EO
		- changed back to 1 for all OR settings
		- there is a data issue in history table for snf too

	05/03/2023 -- EO
		- updates for 22Q4 APU ILS / 23Q1 Hospice

	07/05/2023 -- EO
		- updates for 23Q1 OR 

	08/01/2023 -- EO
		- updates for 23Q1 APU

	10/03/2023 -- EO
		- updates for 23Q2 OR
		- automated report start date for OR vs APU

	10/31/2023 -- HS
		- updates for 23Q2 APU

	11/08/2023 -- CT
		- updates for 23Q2 APU (fixed the cmn_qtr_start_hosp parameter)

	12/19/2023 -- SS
		- updates for 23Q3 OR
		- turned on the cmn_qtr_start_hosp for dev. Need to turn off for production.

	01/16/2024 - EO
		- prod 23Q3 OR
		- turned off Hospice

	01/18/2024 - EO
		- control table change due to addition in ltch asmt variables needed

	02/08/2024 -- HS
		- updates for 23Q3 APU

	04/01/2024 -- SS
		- updates for 23Q4 OR

	04/11/2024 -- HS
		- updated control table
	04/12/2024 -- EO
		- new control table with changes
	04/15/2024 -- HS
		- corrected previous control table

	04/22/2024 -- EO
		- predev work on Q4 APU to prep data reviews for Acumen
*/

/*	Parameters for current reporting cycle	*/

/*	APU or OR reporting cycle? This should be APU or OR.	*/
%let 	cmn_cycle = APU ;


/*** These you need to modify each reporting cycle	***/
/*	Basic year and quarter info	*/
%let 	cmn_yyyy = 2023;
%let	cmn_q = 4;
%let	cmn_yyyy_hosp = 2024;
%let 	cmn_q_hosp = 1;

%let	cmn_qtr_start	= 01OCT&cmn_yyyy;
%let	cmn_qtr_end		= 31DEC&cmn_yyyy;
%put NOTE:  &=cmn_qtr_start
			&=cmn_qtr_end ;
/*	Report start = entire duration of report. 
	Will typically end on qtr end date.
	For APU, will typically start on Jan 1
	For OR, typically same as quarter start date	*/
%if "&cmn_cycle" = "APU" %then %do ; 
%let	cmn_report_start	= 01JAN&cmn_yyyy;
%end ; 
%else %do ; 
%let	cmn_report_start	= &cmn_qtr_start;
%end ; 
%let	cmn_report_end		= &cmn_qtr_end;
%put NOTE:  &=cmn_report_start
			&=cmn_report_end ;

/*	Same macros but hospice is one quarter off	*/
/* common quarter start for hosp should always reflect JAN due to how it is used downstream */
%let	cmn_qtr_start_hosp		= 01JAN&cmn_yyyy_hosp;
%let	cmn_qtr_end_hosp		= 31MAR&cmn_yyyy_hosp;
%let	cmn_report_start_hosp	= 01JAN&cmn_yyyy_hosp;
%let	cmn_report_end_hosp		= &cmn_qtr_end_hosp;
%put NOTE:  &=cmn_qtr_start_hosp
			&=cmn_qtr_end_hosp
			&=cmn_report_start_hosp
			&=cmn_report_end_hosp ; 

/*	Submission cutoff date	*/
%let	cmn_submit_cutoff	= 15MAY2024;


/*	Which PACs to run?	*/
%let	cmn_run_irf = 1;
%let	cmn_run_ltch = 1;
%let	cmn_run_snf = 1;
%let	cmn_run_hosp = 1;

/*** These are all calculated based upon other parameters	***/

/*	Alternate formats for year and quarter	*/
%let cmn_yy = %substr(&cmn_yyyy, 3,2);
%put NOTE:  &=cmn_yy ; 
%let cmn_qq = Q&cmn_q;
%put NOTE:  &=cmn_qq ; 
%let cmn_yy_hosp = %substr(&cmn_yyyy_hosp, 3,2);
%put NOTE:  &=cmn_yy_hosp ; 
%let cmn_qq_hosp = Q&cmn_q_hosp;
%put NOTE:  &=cmn_qq_hosp ; 

/*	Workbench folder for this reporting cycle	*/
%let cmn_folder = &cmn_yyyy-&cmn_qq &cmn_cycle;
%put NOTE:  &=cmn_folder ; 

/*	Data extract start dates in Hive format	*/
%let cmn_qtr_start_hive = %sysfunc(putn("&cmn_qtr_start"d, yymmdd10.));
%put NOTE:  &=cmn_qtr_start_hive ; 
%let cmn_qtr_end_hive = %sysfunc(putn("&cmn_qtr_end"d, yymmdd10.));
%put NOTE:  &=cmn_qtr_end_hive ; 

/* hospice pulls from beginning of reporting year to end of current quarter */
%let cmn_qtr_start_hive_hosp = %sysfunc(putn("&cmn_report_start_hosp"d, yymmdd10.));
%put NOTE:  &=cmn_qtr_start_hive_hosp ; 
%let cmn_qtr_end_hive_hosp = %sysfunc(putn("&cmn_qtr_end_hosp"d, yymmdd10.));
%put NOTE:  &=cmn_qtr_end_hive_hosp ; 

/*	Submission cutoff date	*/
%let cmn_submit_cutoff_hive = %sysfunc(putn("&cmn_submit_cutoff"d, yymmdd10.));
%put NOTE:  &=cmn_submit_cutoff_hive ; 

/*	Tiein cutoff -- providers with tiein >= cutoff are OPD	*/
%let cmn_tiein_cutoff = %sysfunc(intnx(days, "&cmn_report_end"d, 1), date9.);
%put NOTE:  &=cmn_tiein_cutoff ; 
/*	Note that we don't need a tiein cutoff for Hospice...	*/

/*	Cutoffs -- providers with term dates < these dates are out of scope	*/
%let cmn_term_year		= %eval(&cmn_yyyy + 1);
%put NOTE:  &=cmn_term_year ; 
%let cmn_term_cutoff	= %sysfunc(mdy(1, 1, &cmn_term_year), date9.);
%put NOTE:  &=cmn_term_cutoff ; 

%let cmn_term_year_hosp		= %eval(&cmn_yyyy_hosp + 1);
%put NOTE:  &=cmn_term_year_hosp ; 
%let cmn_term_cutoff_hosp	= %sysfunc(mdy(1, 1, &cmn_term_year_hosp), date9.);
%put NOTE:  &=cmn_term_cutoff_hosp ; 

/***	These are partially calculated based upon other parameters but you need to make some updates	***/
%let cmn_control_table_path = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&cmn_folder/Control Table;
/* Raki --- The control table date is temporary camge it once we get the orginal file*/
%let cmn_control_table_file = Source_File_Name_&cmn_yy.&cmn_qq._240422.xlsx; 
%let cmn_control_table_full = &cmn_control_table_path/&cmn_control_table_file;

/*	Validates control file	*/
filename chkctrl "&cmn_control_table_full";

%put Control File validated if this equals 1: %sysfunc(fexist(chkctrl));

filename chkctrl;


/*	Macro to import and QA control table -- case in which there are data element dates
	(e.g., assessments)	*/

%macro set_control_file_dated(
								setting=,
								ctrl_in= &cmn_control_table_full,
								ctrl_sheet=,
								start_date=,
								end_date=,
								ctrl_out = work.control_&setting.);

	/* setting: PAC setting (e.g. 'IRF', 'LTCH', 'SNF') */
	/* ctrl_in: directory and filename for the control sheet
	/*          e.g. '/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2021-Q4 OR/Control Table/source_file_name_21Q4_220308.xlsx' */
	/* ctrl_sheet: sheet name in control file for the corresponding PAC setting (e.g. 'irf_assessment') */
	/* start_date: start date for active element in control file */
	/* end_date: end date for active element in control file */

	/*	Import raw control file	*/	
	filename source "&ctrl_in";

	proc import datafile=source
				out = &&ctrl_out._raw (where = (Source_File_Name ~= ""))
				dbms = xlsx 
				replace;
		getnames = yes;
		sheet = "&ctrl_sheet";	
	run;

	/*	Subset elements based upon start and end date	*/
	data &ctrl_out;
		set &ctrl_out._raw (
			where=(
					Element_Active_Start <= "&end_date"d and 
					Element_Active_End >= "&start_date"d and
					Source_File_Name ~= "prvdr_num"));
	run;

	title "Confirm field types in control table for &setting";
	title2 "Element start and end should be dates. Most other relevant fields should be strings";
	proc contents 	data = &ctrl_out 
					order = varnum;
		ods select position;
	run;
	title;
	
%mend set_control_file_dated;



/*	Macro to import and QA control table -- case in which there are no data element dates
	(e.g., everything except assessments)	*/

%macro set_control_file(
							setting=,
							ctrl_in= &cmn_control_table_full,
							ctrl_sheet=,
							ctrl_out = work.control_&setting.);

	/* setting: PAC setting (e.g. 'IRF', 'LTCH', 'SNF') */
	/* ctrl_in: directory and filename for the control sheet
	/*          e.g. '/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2021-Q4 OR/Control Table/source_file_name_21Q4_220308.xlsx' */
	/* ctrl_sheet: sheet name in control file for the corresponding PAC setting (e.g. 'irf_assessment') */


	/*	Import raw control file	*/	
	filename source "&ctrl_in";

	proc import datafile=source
				out = &&ctrl_out (where = (Source_File_Name ~= ""))
				dbms = xlsx 
				replace;
		getnames = yes;
		sheet = "&ctrl_sheet";	
	run;

	title "Confirm field types in control table for &setting";
	title2 "Element start and end should be dates. Most other relevant fields should be strings";
	proc contents 	data = &ctrl_out 
					order = varnum;
		ods select position;
	run;
	title;
	
%mend set_control_file;

/* Macro variables containing regular expressions identifying specific provider types, including a generic six digit alphanumeric	*/
%let ccn_pattern		= /\b[0-9A-Z]{6}\b/i;
%let ccn_pattern_snf	= /\b\d{2}[56]\d{3}\b/;
%let ccn_pattern_snfsb	= /\b\d{2}[023]\d{3}\b/;
%let ccn_pattern_snfany = /\b\d{2}[02356]\d{3}\b/;
%let ccn_pattern_ltch	= /\b\d{2}2\d{3}\b/ ;
%let ccn_pattern_irf 	= /\b\d{2}[3RT][0-9ABC]\d{2}\b/i;
%let ccn_pattern_hosp	= /\b[A-F0-9]\d1[567]\d{2}\b/i;
