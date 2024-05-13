/****** IRF Data Dump

	PURPOSE:	This program will generate data extracts in csv format of the BIMS PHQ issue to 
                be provided to Acumen
	INPUTS:	Use the following inputs from the workbench (/workspace/workbench/pac_qrp_swingtech/data/)
				-	irf_asmt_comply dataset created by the APU program.
			

	NOTES:	To update this file review/revise the macro variable definitions. 
                The file location and file name references should be updated each quarter.
				The schema, caslib, database reference, and libname statements can be 
				modified as needed.

			This code generates five reports for IRF.

	UPDATES:

	04/12/2023 (SS) - Added new program for 2022 Q4 OR.

    04/18/2023 (SS) - Split Table 2 to Table 2 and 5 as per requirements.
                    - Report 3 - Remove the incomplete stay trigger.

    04/19/2023 (SS) - Updated output report names.

	04/21/2023 (SS) - 2022 Q4 OR prod Run.

	04/24/2023 (SS) - Added in_scope filter to all reports.

	05/03/2023 (EO) - modified org program to investigate impact of failures for only 
						each reason listed. currently providing fails for at least each
					- dropped rsdnt_intrnl_id from all sets
					- deleted libname for out_asc we dont need

	05/16/2023 (SS) - 2022 Q4 APU dev Run.

	05/17/2023 EO - development data creation for share with Acumen
	05/31/2023 EO - added QA to prepare for Acumen with expanded logic
				  - added raw asmt assembly file as well
				  - added gates to suppress output for sub-reports if row count is 0

	07/24/2023 EO - production data for 23Q1 outreach summary share with Acumen
				  
	08/29/2023 EO - prod data for 23Q1 APU
				- set to trigger output of all failures data
					since there is high failure in assessments for IRF
					not related to the Acumen fixes, it may be helpful 
					for Acumen to review in case CMS wants to expand 
					correction logic
				- moved control flag creation for all fails output to the parameters section
	
	09/22/2023 (CT): updates for production run for 2023-Q1 APU -- added CAMS fixes

	10/23/2023 (EO): prod 23-Q2 OR

	11/17/2023 (SS): dev 23-Q2 APU

	12/01/2023 (CT): Production run for 2023-Q2 APU

	01/22/2024 (SS): prod 23-Q3 OR

	02/21/2024 (SS): dev 23-Q3 APU

	03/25/2024 (SS): prod 23-Q3 APU

	04/25/2024 (SS): prod 23-Q4 APU
					 -  QA Checks for BIMS and PHQ correction logic updated to match checks in the IRF_Asmt_Processing program.
					 -  Added out_rpt6 related parameters in the parameter section.

/*********************************************	END HEADER	***************************************************************/
%put NOTE: END Header / START Active Code ;

* common macro set up - Update every cycle; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;

/************************************PROGRAM PARAMETERS***************************************/
%put NOTE: parameters and non-static libraries ;

* ccn pattern definition for an IRF ; 
%let ccn_irf = &ccn_pattern_irf ; 
%put NOTE: ccn pattern will resolve to &ccn_irf ; 


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

%let fix_start = 01OCT2022 ; 
%let fix_end = 30SEP2023 ; 

* database and workbench reference - update as needed; 
%let bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ;
%let irf_bench = &bench/&folder/IRF APU and Summary/Data;

libname in_asc "&bench/&folder/IRF APU and Summary/Data";
libname asmt_bn "&bench/&folder/Extract and Assemble Assessments/Data" ; 

* APU input files ;
***	Update each reporting phase with current data files (update the date stamps) ;

%LET in_asc 		= in_asc.irf_asmt_comply_&yy.&quarter._240425 ;
%let in_fails		= in_asc.irf_asmt_fails_&yy.&quarter._240425 ; 
%let in_raw			= asmt_bn.irf_asmt_full_23q4_240422 ; 



%LET report_date = %sysfunc(today(), yymmddn6.) ;
*%let out_asmt = IRF_asmt_comply_&yy.&quarter._&report_date;

* control flag for all fails data creation ; 
%put NOTE: determine if we want all fails report ; 
%let want_all_fails = 1 ; 
%put NOTE: &=want_all_fails ; 


**Output files - No need to be updated**;
%let out_rpt1 = IRF_asmt_Recall_Word_Admit_Fails_&yy.&quarter._&report_date;
%let out_rpt2 = IRF_Unplan_BIMS_Fails_&yy.&quarter._&report_date;
%let out_rpt3 = IRF_asmt_Recall_Word_Discharge_Fails_&yy.&quarter._&report_date;
%let out_rpt4 = IRF_asmt_All_Fails_&yy.&quarter._&report_date;
%let out_rpt5 = IRF_asmt_Unplan_PHQ_Fails_&yy.&quarter._&report_date;
%let out_rpt6 = IRF_asmt_CAMS_Fails_Unplanned_&yy.&quarter._&report_date;


%put NOTE: input and output summary ; 
%put NOTE: input for assessment comply file is &in_asc ;
%put NOTE: input for assessment fail reasons file is &in_fails ;
%put NOTE: output for Report IRF_asmt_Recall_Word_Admit_Fails is &out_rpt1 ;
%put NOTE: output for Report IRF_Unplan_BIMS_Fails is &out_rpt2 ;
%put NOTE: output for Report IRF_asmt_Recall_Word_Discharge_Fails is &out_rpt3 ;
%put NOTE: output for Report IRF_asmt_All_Fails is &out_rpt4 ;
%put NOTE: output for Report IRF_asmt_Unplan_PHQ_Fails is &out_rpt5 ;
%put NOTE: output for Report IRF_asmt_CAMS_Fails_Unplanned is &out_rpt6 ;


%put NOTE: END parameters ; 

%put NOTE: grab tie in date with raw assessment file ; 

proc sql ; 

	create table irf_asmt_raw as 
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
	title	"QA: Checking outcomes of BIMS and PHQ correction logic";
	
	
	proc sql;
		title2	"Initial number of assessments with C0400A-C Admit any Dashes";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(rcall_first_word_cd = "-"
				or rcall_scnd_word_cd = "-"
				or rcall_thrd_word_cd = "-")
			
				and tieIn < "&tiein_excptn"d
		group by quarter
				;
		title2	"Final Number of assessments with C0400A-C Admit any Dashes After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(rcall_first_word_cd = "-"
				or rcall_scnd_word_cd = "-"
				or rcall_thrd_word_cd = "-")
				
				and irf_asmt_denom > 0
		group by quarter
				;
	quit;

	proc sql;
		title2	"Initial number of assessments with C0400A-C discharge any Dashes";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(rcall_first_word_dschrg_cd = "-"
				or rcall_scnd_word_dschrg_cd = "-"
				or rcall_thrd_word_dschrg_cd = "-")
		
				and tieIn < "&tiein_excptn"d
		group by quarter
				;
		title2	"Final Number of assessments with C0400A-C discharge any Dashes After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(rcall_first_word_dschrg_cd= "-"
				or rcall_scnd_word_dschrg_cd = "-"
				or rcall_thrd_word_dschrg_cd = "-")
				
				and irf_asmt_denom > 0
		group by quarter
				;
	quit;
	
	proc sql;
		title2	"Initial number of unplanned discharge assessments with BIMS any dash";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(cndct_mentl_stus_dschrg_cd='-'
				or word_rpet_frst_atmpt_dschrg_cd='-'
				or rpt_crct_yr_dschrg_cd='-'
				or rpt_crct_mo_dschrg_cd='-'
				or rpt_crct_day_dschrg_cd='-'
				or rcall_first_word_dschrg_cd='-'
				or rcall_scnd_word_dschrg_cd='-'
				or rcall_thrd_word_dschrg_cd='-'
				or bims_scre_dschrg_num='-')
				
				and tieIn < "&tiein_excptn"d
				and 
					(dschrg_agnst_mdcl_advc_cd in ("1")
					OR dschrg_alive_cd in ("0")
					OR dschrg_dstntn_cd in ("02", "63", "65", "66")
					)
		group by quarter
				;
		title2	"Final Number of unplanned discharge assessments with BIMS any dash After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(cndct_mentl_stus_dschrg_cd='-'
				or word_rpet_frst_atmpt_dschrg_cd='-'
				or rpt_crct_yr_dschrg_cd='-'
				or rpt_crct_mo_dschrg_cd='-'
				or rpt_crct_day_dschrg_cd='-'
				or rcall_first_word_dschrg_cd in ('-')
				or rcall_scnd_word_dschrg_cd in ('-')
				or rcall_thrd_word_dschrg_cd in ('-')
				or bims_scre_dschrg_num='-')
				
				and irf_asmt_denom > 0
				and incomplete_stay = 1
		group by quarter
				;
	quit;

	proc sql;
		title2	"Initial number of unplanned discharge assessments with PHQ (presence only) any dash";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(lttl_intrst_dschrg_cd='-'
				or down_dprsd_dschrg_cd='-'
				or fall_aslp_dschrg_cd='-'
				or feel_trd_dschrg_cd='-'
				or aptit_ovreat_dschrg_cd='-'
				or feel_bad_dschrg_cd='-'
				or trbl_cncntrt_dschrg_cd='-'
				or move_slow_dschrg_cd='-'
				or thght_btr_dd_dschrg_cd='-')
			
				and tieIn < "&tiein_excptn"d
				and 
					(dschrg_agnst_mdcl_advc_cd in ("1")
					OR dschrg_alive_cd in ("0")
					OR dschrg_dstntn_cd in ("02", "63", "65", "66")
					)
		group by quarter
				;
		title2	"Final Number of unplanned discharge assessments with PHQ (presence only) any dash After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(lttl_intrst_dschrg_cd='-'
				or down_dprsd_dschrg_cd='-'
				or fall_aslp_dschrg_cd='-'
				or feel_trd_dschrg_cd='-'
				or aptit_ovreat_dschrg_cd='-'
				or feel_bad_dschrg_cd='-'
				or trbl_cncntrt_dschrg_cd='-'
				or move_slow_dschrg_cd='-'
				or thght_btr_dd_dschrg_cd='-')
				
				and irf_asmt_denom > 0
				and incomplete_stay = 1
		group by quarter
				;
	quit;
	
	proc sql;
		title2	"Initial number of unplanned discharge assessments with CAMS (presence only) any dash";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(mentl_stus_chg_dschrg_cd = "-"
						or	dfclty_attntn_dschrg_cd	= "-"
						or 	thnkg_disorgnz_dschrg_cd	= "-"
						or	lvl_conscs_dschrg_cd = "-")
		
				and tieIn < "&tiein_excptn"d
				and 
					(dschrg_agnst_mdcl_advc_cd in ("1")
					OR dschrg_alive_cd in ("0")
					OR dschrg_dstntn_cd in ("02", "63", "65", "66")
					)
		group by quarter
				;
		title2	"Final Number of unplanned discharge assessments with CAMS (presence only) any dash After Fix Applied";
		select count(*) as corrected_all_fail,
				quarter
		from	&in_asc
		where	(mentl_stus_chg_dschrg_cd = "-"
						or	dfclty_attntn_dschrg_cd	= "-"
						or 	thnkg_disorgnz_dschrg_cd	= "-"
						or	lvl_conscs_dschrg_cd = "-")
				
				and irf_asmt_denom > 0
				and incomplete_stay = 1
		group by quarter
				;
	quit;
	

	title;


/*************************LOAD, SUBSET and OUTPUT CSV FILES********************************/

%put NOTE: read in fails data ; 
proc sql ;
	create table  irf_fail_reasons as
	select fls.*,
			cmply.in_scope
	from  &in_fails as fls
			left join
			&in_asc as cmply
			on fls.orgnl_asmt_id = cmply.orgnl_asmt_id
	where cmply.in_scope = 1
; 
run ; 
title "confirm no out of scope" ; 
proc means data = irf_fail_reasons n nmiss min mean max ; 
var in_scope ; 
run ; 
title "confirm no missing key vars" ; 
proc freq data = irf_fail_reasons nlevels;
	tables fail_field orgnl_asmt_id / missing noprint ; 
run ; 
title; 


%put NOTE: report for admit word recall fails ; 
%put NOTE: at least fails ; 
**Report 1 - IRF, any assessment with a dash in rcall_first_word_cd, rcall_scnd_word_cd, or rcall_thrd_word_cd.**;
data irf_report_1;
	set &in_asc;
	where in_scope=1  
			and
			irf_asmt_numer = 0
			and
		  (rcall_first_word_cd = "-"
		  or rcall_scnd_word_cd = "-"
		  or rcall_thrd_word_cd = "-");

	drop rsdnt_intrnl_id ; 
run;

%put NOTE: grab row count into a macro ; 
title "Row count for recall admit fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_recall_admit_fails
	from irf_report_1 ;
quit ; 

%put NOTE: &=num_recall_admit_fails ; 

%if &num_recall_admit_fails = 0 %then %do ;
	%put NOTE: the number of records failing for recall admit words is &num_recall_admit_fails ;
	%put NOTE: there will be no additional output related to report 1 ; 
%end ; 


%if &num_recall_admit_fails > 0 %then %do ; 
proc sql ;
	select *
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_1) ; 
quit ; 


title "confirm no missing numers" ; 
proc means data = irf_report_1 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ; 

%put NOTE: now only fails for report reason above; 

 
proc sql noprint; 
	select quote(strip(fail_field))
	into :not_admit_rcall separated by ","
	from (select distinct fail_field
			from irf_fail_reasons
			where fail_field not in ("rcall_first_word_cd",
							"rcall_scnd_word_cd",
							"rcall_thrd_word_cd")
			) ; 
quit ; 

proc sql ; 
	create table irf_report_1_only_fails as
	select *
	from irf_report_1
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from irf_fail_reasons
		where fail_field in (&not_admit_rcall)
		)
	;
quit ; 

title "confirm no fail reasons other than admit word recall variables" ; 
proc sql ;
	select *
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_1_only_fails ); 
quit ;  
	
%end ; *end conditional report 1 subsetting ; 

%put NOTE: report for bims fails unplanned; 
%put NOTE: at least fails ; 
**Report 2 - IRF, any incomplete stay assessment with a dash in any of the following:

BIMS report:

cndct_mentl_stus_dschrg_cd
word_rpet_frst_atmpt_dschrg_cd
rpt_crct_yr_dschrg_cd
rpt_crct_mo_dschrg_cd
rpt_crct_day_dschrg_cd
rcall_first_word_dschrg_cd
rcall_scnd_word_dschrg_cd
rcall_thrd_word_dschrg_cd
bims_scre_dschrg_num

**;
data irf_report_2;
	set &in_asc;
	where   in_scope=1 
				and 
			incomplete_stay = 1 
				and
			irf_asmt_numer = 0
				and
		    (cndct_mentl_stus_dschrg_cd='-'
			or word_rpet_frst_atmpt_dschrg_cd='-'
			or rpt_crct_yr_dschrg_cd='-'
			or rpt_crct_mo_dschrg_cd='-'
			or rpt_crct_day_dschrg_cd='-'
			or rcall_first_word_dschrg_cd='-'
			or rcall_scnd_word_dschrg_cd='-'
			or rcall_thrd_word_dschrg_cd='-'
			or bims_scre_dschrg_num='-');

	drop rsdnt_intrnl_id ; 

run;

%put NOTE: grab row count into a macro ; 
title "Row count for unplanned bims fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_unplan_bims_fails
	from irf_report_2 ;
quit ; 

%put NOTE: &=num_unplan_bims_fails ; 

%if &num_unplan_bims_fails = 0 %then %do ;
	%put NOTE: the number of records failing for unplan bims is &num_unplan_bims_fails ;
	%put NOTE: there will be no additional output related to report 2 ; 
%end ; 


%if &num_unplan_bims_fails > 0 %then %do ;


title "confirm no missing numers" ; 
proc means data = irf_report_2 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ;

%put NOTE: now only fails for report reason above; 


proc sql noprint; 
	select quote(strip(fail_field))
	into :not_bims separated by ","
	from (select distinct fail_field
			from irf_fail_reasons
			where fail_field not in ("cndct_mentl_stus_dschrg_cd"
			, "word_rpet_frst_atmpt_dschrg_cd"
			, "rpt_crct_yr_dschrg_cd"
			, "rpt_crct_mo_dschrg_cd"
			, "rpt_crct_day_dschrg_cd"
			, "rcall_first_word_dschrg_cd"
			, "rcall_scnd_word_dschrg_cd"
			, "rcall_thrd_word_dschrg_cd"
			, "bims_scre_dschrg_num")
			) ; 
quit ; 

proc sql ; 
	create table irf_report_2_only_fails as
	select *
	from irf_report_2
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from irf_fail_reasons
		where fail_field in (&not_bims)
		)
	;
quit ; 

title "confirm no fail reasons other than bims variables" ; 
proc sql ;
	select distinct fail_field
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_2_only_fails ); 
quit ;  

%end ; * end conditional unplan bims fails subsetting ; 

%put NOTE: report for discharge word fails; 
%put NOTE: at least fails ; 

**Report 3 - IRF, any stay with a dash in rcall_first_word_dschrg_cd,  rcall_scnd_word_dschrg_cd, or rcall_thrd_word_dschrg_cd**;

data irf_report_3;
	set &in_asc;
    where  in_scope=1 
				and
			irf_asmt_numer = 0
				and
		  (rcall_first_word_dschrg_cd = "-"
		   or rcall_scnd_word_dschrg_cd = "-"
		   or rcall_thrd_word_dschrg_cd = "-");

	drop rsdnt_intrnl_id ;
run;

%put NOTE: grab row count into a macro ; 
title "Row count for recall discharge fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_recall_dschrg_fails
	from irf_report_3 ;
quit ; 

%put NOTE: &=num_recall_dschrg_fails ; 

%if &num_recall_dschrg_fails = 0 %then %do ;
	%put NOTE: the number of records failing for recall discharge words is &num_recall_dschrg_fails ;
	%put NOTE: there will be no additional output related to report 3 ; 
%end ; 


%if &num_recall_dschrg_fails > 0 %then %do ;

title "confirm no missing numers" ; 
proc means data = irf_report_3 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ;

%put NOTE: now only fails for report reason above; 


proc sql noprint; 
	select quote(strip(fail_field))
	into :not_dschrg_words separated by ","
	from (select distinct fail_field
			from irf_fail_reasons
			where fail_field not in ("first_word_dschrg_cd"
			, "rcall_scnd_word_dschrg_cd"
			, "rcall_thrd_word_dschrg_cd"
			)
			) ; 
quit ; 

proc sql ; 
	create table irf_report_3_only_fails as
	select *
	from irf_report_3
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from irf_fail_reasons
		where fail_field in (&not_dschrg_words)
		)
	;
quit ; 

title "confirm no fail reasons other discharge recall word variables" ; 
proc sql ;
	select distinct fail_field
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_3_only_fails ); 
quit ;  

%end ; *end conditional subsetting for recall discharge words; 

**Report 4 - All IRF failed assessments**;
data irf_report_4;
	set &in_asc;
	where in_scope=1
          and irf_asmt_denom>0
          and irf_asmt_numer=0;
	drop rsdnt_intrnl_id ;
run;

title "confirm no missing numers" ; 
proc means data = irf_report_4 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ;

%put NOTE: report for phq fails unplanned; 
%put NOTE: at least fails ; 
**Report 5 - IRF, any incomplete stay assessment with a dash in any of the following:
PHQ report:

lttl_intrst_dschrg_cd
down_dprsd_dschrg_cd
fall_aslp_dschrg_cd
feel_trd_dschrg_cd
aptit_ovreat_dschrg_cd
feel_bad_dschrg_cd
trbl_cncntrt_dschrg_cd
move_slow_dschrg_cd
thght_btr_dd_dschrg_cd**;

data irf_report_5;
	set &in_asc;
	where   in_scope=1 
				and 
			incomplete_stay = 1 
				and
			irf_asmt_numer = 0
				and
		    (lttl_intrst_dschrg_cd='-'
			or down_dprsd_dschrg_cd='-'
			or fall_aslp_dschrg_cd='-'
			or feel_trd_dschrg_cd='-'
			or aptit_ovreat_dschrg_cd='-'
			or feel_bad_dschrg_cd='-'
			or trbl_cncntrt_dschrg_cd='-'
			or move_slow_dschrg_cd='-'
			or thght_btr_dd_dschrg_cd='-');
	drop rsdnt_intrnl_id ;
run;

%put NOTE: grab row count into a macro ; 
title "Row count for unplan phq  fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_unplan_phq_fails
	from irf_report_5 ;
quit ; 

%put NOTE: &=num_unplan_phq_fails ; 

%if &num_unplan_phq_fails = 0 %then %do ;
	%put NOTE: the number of records failing for unplan phq is &num_unplan_phq_fails ;
	%put NOTE: there will be no additional output related to report 5 ; 
%end ; 


%if &num_unplan_phq_fails > 0 %then %do ;

title "confirm no missing numers" ; 
proc means data = irf_report_5 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ;

%put NOTE: now only fails for report reason above; 
 
proc sql noprint; 
	select quote(strip(fail_field))
	into :not_phq separated by ","
	from (select distinct fail_field
			from irf_fail_reasons
			where fail_field not in ("lttl_intrst_dschrg_cd"
			, "down_dprsd_dschrg_cd"
			, "fall_aslp_dschrg_cd"
			, "feel_trd_dschrg_cd"
			, "aptit_ovreat_dschrg_cd"
			, "feel_bad_dschrg_cd"
			, "trbl_cncntrt_dschrg_cd"
			, "move_slow_dschrg_cd"
			, "thght_btr_dd_dschrg_cd"
			)
			) ; 
quit ; 

proc sql ; 
	create table irf_report_5_only_fails as
	select *
	from irf_report_5
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from irf_fail_reasons
		where fail_field in (&not_phq)
		)
	;
quit ; 

title "confirm no fail reasons other discharge recall word variables" ; 
proc sql ;
	select distinct fail_field
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_5_only_fails ); 
quit ;  
%end ; *end conditional subsetting of unplan phq fails ; 


%put NOTE: report for CAMS fails unplanned; 
%put NOTE: at least fails ; 
**Report 6 - IRF, any incomplete stay assessment with a dash in any of the following:
CAMS report:

(mentl_stus_chg_dschrg_cd='-'
or dfclty_attntn_dschrg_cd='-'
or thnkg_disorgnz_dschrg_cd='-'
or lvl_conscs_dschrg_cd='-')

mentl_stus_chg_dschrg_cd
dfclty_attntn_dschrg_cd
thnkg_disorgnz_dschrg_cd
lvl_conscs_dschrg_cd;

data irf_report_6;
	set &in_asc;
	where   in_scope=1 
				and 
			incomplete_stay = 1 
				and
			irf_asmt_numer = 0
				and
		 (mentl_stus_chg_dschrg_cd='-'
			or dfclty_attntn_dschrg_cd='-'
			or thnkg_disorgnz_dschrg_cd='-'
			or lvl_conscs_dschrg_cd='-');
	drop rsdnt_intrnl_id ;
run;


%put NOTE: grab row count into a macro ; 
title "Row count for unplan CAMS  fails" ; 
proc sql  ;
	select count(*) as num_rows
	into :num_unplan_cams_fails
	from irf_report_6 ;
quit ; 

%put NOTE: &=num_unplan_cams_fails ; 

%if &num_unplan_cams_fails = 0 %then %do ;
	%put NOTE: the number of records failing for unplan CAMS is &num_unplan_cams_fails ;
	%put NOTE: there will be no additional output related to report 6 ; 
%end ; 


%if &num_unplan_cams_fails > 0 %then %do ;

title "confirm no missing numers" ; 
proc means data = irf_report_6 n nmiss min mean max ; 
var irf_asmt_numer ; 
run ;

%put NOTE: now only fails for report reason above; 
 
proc sql noprint; 
	select quote(strip(fail_field))
	into :not_cams separated by ","
	from (select distinct fail_field
			from irf_fail_reasons
			where fail_field not in ("mentl_stus_chg_dschrg_cd"
			, "dfclty_attntn_dschrg_cd"
			, "thnkg_disorgnz_dschrg_cd"
			, "lvl_conscs_dschrg_cd"
			)
			) ; 
quit ; 



proc sql ; 
	create table irf_report_6_only_fails as
	select *
	from irf_report_6
	where orgnl_asmt_id not in 
		(select orgnl_asmt_id
		from irf_fail_reasons
		where fail_field in (&not_cams)
		)
	;
quit ; 

title "confirm no fail reasons other discharge recall word variables" ; 
proc sql ;
	select distinct fail_field
	from irf_fail_reasons
	where orgnl_asmt_id in (select orgnl_asmt_id
							from irf_report_6_only_fails ); 
quit ;  
%end ; *end conditional subsetting of unplan CAMS fails ; 


%put NOte: if &=want_all_fails is 0 there will be no output triggered ;  

**Output each of the reports as a CSV file**;
%if &num_recall_admit_fails > 0 %then %do ;
proc export data=irf_report_1_only_fails
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt1..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 
%if &num_unplan_bims_fails > 0 %then %do ;
proc export data=irf_report_2_only_fails
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt2..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 
%if &num_recall_dschrg_fails > 0 %then %do ;
proc export data=irf_report_3_only_fails
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt3..csv" dbms=csv replace;
	delimiter=',';
run;
%end ;
%if &want_all_Fails = 1 %then %do ;
proc export data=irf_report_4
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt4..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 
%if &num_unplan_phq_fails > 0 %then %do ;
proc export data=irf_report_5_only_fails
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt5..csv" dbms=csv replace;
	delimiter=',';
run;
%end ; 

%if &num_unplan_cams_fails > 0 %then %do ;
proc export data=irf_report_6_only_fails
	outfile="&bench/&folder/IRF APU and Summary/Outputs/&out_rpt6..csv" dbms=csv replace;
	delimiter=',';
run;
%end ;
/**************************************END ACTIVE CODE ***************************************************/	

%put NOTE: END ACTIVE CODE ; 