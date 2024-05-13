/****** LTCH Data Dump

	PURPOSE:	This program will generate data extracts in csv format of the BIMS PHQ issue to 
                be provided to Acumen
	INPUTS:	Use the following inputs from the workbench (/workspace/workbench/pac_qrp_swingtech/data/)
				-	ltch_asmt_comply dataset created by the APU program.
			

	NOTES:	To update this file review/revise the macro variable definitions. 
                The file location and file name references should be updated each quarter.
				The schema, caslib, database reference, and libname statements can be 
				modified as needed.

			This code generates two reports for LTCH.

	UPDATES:

	04/12/2023 (SS) - Added new program for 2022 Q4 OR.

    04/19/2023 (SS) - Updated output report names.

    04/21/2023 (SS) - 2022 Q4 OR Production Run.

	04/24/2023 (SS) - Added in_scope filter to all reports.

	05/03/2023 (EO) - modified org program to investigate impact of failures for only 
						each reason listed. currently providing fails for at least each
					- dropped rsdnt_intrnl_id from all sets
					- deleted libname for out_asc we dont need

	05/16/2023 (SS) - 2022 Q4 APU Development Run.

	05/17/2023 EO - deve run to send to Acumen
	05/31/2023 EO - added QA to prepare for Acumen with expanded logic
				  - added raw asmt assembly file as well
				  - added gates to suppress output for sub-reports if row count is 0

	06/02/2023 EO - removed subsetting by record type
			      - added record type to QA check

	07/24/2023 EO - production summaries for 23Q1 outreach reporting

	08/29/2023 EO - prod data for 23Q1 APU
				- set to trigger output of all failures data
					since there is high failure in assessments for IRF
					not related to the Acumen fixes, it may be helpful 
					for Acumen to review in case CMS wants to expand 
					correction logic, sending for LTCH as well just in case
				- moved control flag creation for all fails output to the parameters section
				- dropped resident internal id

	10/23/2023 EO - prod 23Q2 OR

	11/17/2023 CT - Development run for 2023-Q2 APU

	12/01/2023 CT - Production run for 2023-Q2 APU

	01/22/2024 HS - Production run for 2023-Q3 OR

	02/21/2024 SS - Development run for 2023-Q3 APU

	03/25/2024 HS - Production run for 2023-Q3 APU

	04/25/2024 (SS): Production run for 2023-Q4 APU
					 -  QA Checks for BIMS and PHQ correction logic updated to match checks in the IRF_Asmt_Processing program.
					 -  Added out_rpt6 related parameters in the parameter section.
					 -  Assign tieindefault from common macro.

/*********************************************	END HEADER	***************************************************************/
%put NOTE: END Header / START Active Code ;

* common macro set up - Update every cycle; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;

/************************************PROGRAM PARAMETERS***************************************/
%put NOTE: parameters and non-static libraries ;

* ccn pattern definition for an LTCH ; 
%let ccn_ltch = &ccn_pattern_ltch ; 
%put NOTE: ccn pattern will resolve to &ccn_ltch ; 


%let year = &cmn_yyyy ;
%put NOTE: &=year;

%let yy = &cmn_yy;
%put NOTE: &=yy;

%let quarter = &cmn_qq;
%put NOTE: &=quarter;

%let cycle = &cmn_cycle ;
%put NOTE: &=cycle;

* workbench folder - current quarter --- UPDATE EVERY CYCLE; 
%let folder = &cmn_folder ;
%put NOTE: &=folder;


* assign tie in exception date ; 
%let tiein_excptn = &cmn_tiein_cutoff ; 
%put NOTE: &=tiein_excptn ;

* default date for tiein to signify OPD  - when reporting not required;
* tiein default is the first day of the quarter following the current reporting period ; 
%let tieindefault = &cmn_tiein_cutoff ;
%put	NOTE: Tie in default set using common macros
		&=tieindefault
		;

%let fix_start = 01OCT2022 ; 
%let fix_end = 30SEP2023 ; 

* database and workbench reference - update as needed; 
%let bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ;
%let ltch_bench = &bench/&folder/LTCH APU and Summary/Data;

libname in_asc "&bench/&folder/LTCH APU and Summary/Data";
libname asmt_bn "&bench/&folder/Extract and Assemble Assessments/Data" ; 

* APU input files ;
***	Update each reporting phase with current data files (update the date stamps) ;

%LET in_asc 		= in_asc.ltch_asmt_comply_&yy.&quarter._240425 ;
%let in_fails		= in_asc.ltch_asmt_fails_&yy.&quarter._240425 ; 
%let in_raw			= asmt_bn.ltch_asmt_full_23q4_240424 ; 


* control flag for all fails data creation ; 
%put NOTE: determine if we want all fails report ; 
%let want_all_fails = 1 ; 
%put NOTE: &=want_all_fails ; 

%LET report_date = %sysfunc(today(), yymmddn6.) ;
*%let out_asmt = LTCH_asmt_comply_&yy.&quarter._&report_date;

**Output files - No need to be updated**;
%let out_rpt1 = LTCH_asmt_Recall_Word_Admit_Fails_&yy.&quarter._&report_date;
%let out_rpt2 = LTCH_asmt_All_Fails_&yy.&quarter._&report_date;


%put NOTE: input and output summary ; 
%put NOTE: input for assessment comply file is &in_asc ;
%put NOTE: ouput for Report LTCH_asmt_Recall_Word_Admit_Fails is &out_rpt1 ;
%put NOTE: ouput for Report LTCH_asmt_All_Fails is &out_rpt2 ;


%put NOTE: END parameters ; 

%put NOTE: grab tie in date with raw assessment file ; 

proc sql ; 

	create table ltch_asmt_raw as 
	select raw.*,
			tie.tieIn,
			tie.in_scope
	from 	&in_asc as tie
				left join
			&in_raw as raw
				on tie.ccn = raw.ccn
					and
				tie.orgnl_asmt_id = raw.orgnl_asmt_id
	; 
	
quit ; 

/* QA raw and corrected data */
%put NOTE: Checks for BIMS and PHQ correction logic	;
	title	"QA: Checking outcomes of Recall words correction logic";
	
	
	proc sql;
		title2	"Initial number of assessments with C0400A-C Admit any Dashes";
		select count(*) as initial_all_fail, 
				a0250_rsn_for_asmt_cd,
				quarter
		from	work.ltch_asmt_raw
		where	
				(c0400a_rcall_first_word_cd = "-"
				or c0400b_rcall_scnd_word_cd = "-"
				or c0400c_rcall_thrd_word_cd = "-")
				and tieIn < "&tieindefault"d
		group by a0250_rsn_for_asmt_cd,
				quarter
				;
		title2	"Final Number of assessments with C0400A-C Admit any Dashes After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(c0400a_rcall_first_word_cd = "-"
				or c0400b_rcall_scnd_word_cd = "-"
				or c0400c_rcall_thrd_word_cd = "-")
				and ltch_asmt_denom > 0
		group by quarter
		;
	quit;

/*************************LOAD, SUBSET and OUTPUT CSV FILES********************************/
%put NOTE: read in fails data ; 
proc sql ;
	create table  ltch_fail_reasons as
	select fls.*,
			cmply.in_scope
	from  &in_fails as fls
			left join
			&in_asc as cmply
			on fls.orgnl_asmt_id = cmply.orgnl_asmt_id
	where cmply.in_scope = 1
; 
quit ; 
title "confirm no out of scope" ; 
proc means data = ltch_fail_reasons n nmiss min mean max ; 
var in_scope ; 
run ; 
title "confirm no missing key vars" ; 
proc freq data = ltch_fail_reasons nlevels;
	tables fail_field orgnl_asmt_id / missing noprint ; 
run ; 

%put NOTE: report for admit word recall fails ; 
%put NOTE: at least fails ; 
**Report 1 - LTCH, any admission assessment (a0250_rsn_for_asmt_cd = "01") where c0400a_rcall_first_word_cd, c0400b_rcall_scnd_word_cd, or c0400c_rcall_thrd_word_cd is a dash.**;
data ltch_report_1;
	set &in_asc;
	where in_scope=1  
			and
			ltch_asmt_numer = 0 
			and
          (c0400a_rcall_first_word_cd='-'
		  or c0400b_rcall_scnd_word_cd='-'
		  or c0400c_rcall_thrd_word_cd='-');
	drop rsdnt_intrnl_id ; 
run;


%put NOTE: grab row count into a macro ; 
title "Row count for recall admit fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_recall_admit_fails
	from ltch_report_1 ;
quit ; 

%put NOTE: &=num_recall_admit_fails ; 

%if &num_recall_admit_fails = 0 %then %do ;
	%put NOTE: the number of records failing for recall admit words is &num_recall_admit_fails ;
	%put NOTE: there will be no additional output related to report 1 ; 
%end ; 


%if &num_recall_admit_fails > 0 %then %do ; 
proc sql ;
	select *
	from ltch_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from ltch_report_1) ; 
quit ; 
title "confirm no missing numers" ; 
proc means data = ltch_report_1 n nmiss min mean max ; 
var ltch_asmt_numer ; 
run ; 

%put NOTE: now only fails for report reason above; 

proc sql noprint; 
	select quote(strip(fail_field))
	into :not_admit_rcall separated by ","
	from (select distinct fail_field
			from ltch_fail_reasons
			where fail_field not in ("c0400a_rcall_first_word_cd",
							"c0400b_rcall_scnd_word_cd",
							"c0400c_rcall_thrd_word_cd")
			) ; 
quit ; 

proc sql ; 
	create table ltch_report_1_only_fails as
	select *
	from ltch_report_1
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from ltch_fail_reasons
		where fail_field in (&not_admit_rcall)
		)
	;
quit ; 

title "confirm no fail reasons other than admit word recall variables" ; 
proc sql ;
	select *
	from ltch_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from ltch_report_1_only_fails ); 
quit ; 
%end ; * end conditional subsetting of report 1 ; 


**Report 2 - All LTCH failed assessments.**;
data ltch_report_2;
	set &in_asc;
	where in_scope=1
          and ltch_asmt_denom>0
          and ltch_asmt_numer=0;
	drop rsdnt_intrnl_id ; 
run;
title "confirm no missing numers" ; 
proc means data = ltch_report_2 n nmiss min mean max ; 
var ltch_asmt_numer ; 
run ; 



**Output each of the two reports as a CSV file** ; 

%put NOTE: if &=want_all_fails is 0 there will be no output triggered ;  

%if &num_recall_admit_fails > 0 %then %do ;
proc export data=ltch_report_1_only_fails
	outfile="&bench/&folder/LTCH APU and Summary/Outputs/&out_rpt1..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 
%if &want_all_fails = 1 %then %do ;
proc export data=ltch_report_2
	outfile="&bench/&folder/LTCH APU and Summary/Outputs/&out_rpt2..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 


/**************************************END ACTIVE CODE ***************************************************/	

%put NOTE: END ACTIVE CODE ; 