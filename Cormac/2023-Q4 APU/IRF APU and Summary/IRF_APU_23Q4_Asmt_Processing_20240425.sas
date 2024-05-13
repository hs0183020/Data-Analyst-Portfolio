/* IRF APU assessment procssing only */

/*********************************************	END HEADER	***************************************************************/
%put NOTE: END Header / START Active Code ;

* common macro set up ; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;

*	Control flag for waivers	;
*	We don't need to apply waivers for outreach. This flag can let us avoid having to add or remove code.	;
%let has_waivers = 0 ;

*	Flag to correct the BIMS PHQ Flaw	
	Set to 1 if you need to fix this issue
	;
%let fix_bims_phq = 1;
%let strict = 0 ;
%put	NOTE: Fix bims contorl flag is set to &=fix_bims_phq with &=strict ;

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



* database and workbench reference - update as needed; 
%let bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ;
%let irf_bench = &bench/&folder/IRF APU and Summary/Data;

libname in_asmt "&bench/&folder/Extract and Assemble Assessments/Data";
libname in_prvd "&bench/2023-Q4 OR/Extract and Assemble Providers/Data";

libname in_clms "&bench/2023-Q4 OR/Extract and Tally Claims/Data";



* location of Control File (Source_File_Name) - check that setting (sheet=) and source (sourcefile=) are up to date and correct ;
%LET ctrl_dir = &cmn_control_table_path;
%put NOTE: &=ctrl_dir;

%LET in_ctrl = &cmn_control_table_file;
%put NOTE: &=cmn_control_table_file;

%LET ctrl_sheet = IRF_Assessment ;

* APU input files ;
***	Update each reporting phase with current data files (update the date stamps) ;

	
	*	This is FULL for APU, OR for OR	(assessment data file);
	%if "&cycle" = "OR" %then %do ; 
		%let full = OR ; 
	%end ; 
	%else %do ; 
		%let full = full ; 
	%end ; 

%LET in_asmt 		= in_asmt.irf_asmt_&full._&yy.&quarter._240422 ;
%LET in_prvd 		= in_prvd.irf_prvdr_&yy.&quarter._OR_240416 ;

%LET in_clms 		= in_clms.clm_tally_irf_&yy.OR_240416;


* APU-specific parameters - update for each quarter, check Criteria Plan for most current guidance ;

* Assessment compliance thresholds:	Meet APU = Yes 	(94.45% or higher)
					Meet APU = No	(0 > compliance > 94.45%)
					Meet APU = NA	(compliance = 0/no data) ;
 
%LET trgt_start 	= &cmn_report_start;
%put NOTE: &=trgt_start;

%LET trgt_end 		= &cmn_report_end;
%put NOTE: &=trgt_end;

%LET tiein_excptn 	= &cmn_tiein_cutoff ;
%put NOTE: &=tiein_excptn;


%let nhsn_month_start 	= %sysfunc(month("&trgt_start"d));
%let nhsn_month_end 	= %sysfunc(month("&trgt_end"d));
%let nhsn_year 			= %sysfunc(year("&trgt_start"d));

%put NOTE: report cycle is 20&yy &quarter &cycle and target dates are &trgt_start to &trgt_end with a tiein date of &tiein_excptn ; 
%put NOTE: likewise, nhsn is set to run for months &nhsn_month_start to &nhsn_month_end of &nhsn_year ; 


%if &fix_bims_phq = 1 %then %do ; 
	%let fix_start 	= 01OCT2022 ; 
	%let fix_end 	= 30SEP2023 ;
	
	%put NOTE: Fix bims will apply to target dates from &=fix_start to &=fix_end ; 

%end ; 

%LET pass_rate = 0.9445 ;

%put NOTE: report for IRF will have an assessment pass rate of &pass_rate for a provider to be considered compliant; 

%LET report_date = %sysfunc(today(), yymmddn6.) ;

libname out_apu	"&bench/&folder/IRF APU and Summary/Data";

%LET out_APU = IRF_&cycle._&yy.&quarter._&report_date ;
%let out_asmt = IRF_asmt_comply_&yy.&quarter._&report_date;
%let out_fails = IRF_asmt_fails_&yy.&quarter._&report_date;


%put NOTE: input and output summary ; 
%put NOTE: input for control file is &in_ctrl ;
%put NOTE: input for assessments is &in_asmt ; 
%put NOTE: input for providers is &in_prvd ; 





%put NOTE: Format used throughout program for QA ; 
*	Format, lets you lump all the providers with tie in prior to the exception date into a single group	;
proc format;
	value tie_applies
	low -< "&trgt_start"d = "Tie in not applicable"
	"&trgt_start"d - high = [date9.]
	;

	picture nicepercent
		low - high = "009.99%";
run;

%put NOTE: END parameters ; 

/*************************LOAD AND COMBINE APU INPUTS********************************/
%put NOTE: Start input loading and processing ; 

%put NOTE: read in and process control table ; 
*** Assessment-level APU Processing ;
	** Load Control File for Assessments ;
 
 
	/* from common macro program */
	%set_control_file(
							setting		= irf,
							ctrl_sheet	= &ctrl_sheet ,
							ctrl_out 	= control_irf_clean);

	 


%put NOTE: Set up arrays of performance and discharge items for logic processing ; 

** Create macro lists of performance items and goals from control file for downstream arrays ;
* Macro for Admissions and Discharge performance items ;
%macro control_elements(into =,
						where = );

	%global &into;

	PROC SQL NOPRINT ;
			SELECT	Source_File_Name
			INTO	:&into separated by " "		
			FROM	work.control_irf_clean
			WHERE	&where ;
	QUIT ;
	
	
%mEnd;

%control_elements(	into = perf_admit,
					where = %str(Element_Type = "perform" and required_admit = "yes")) ;

%put	NOTE: perf_admit resolved to &perf_admit;

%control_elements (	into = perf_discharge,
					where = %str(Element_Type = "perform" and required_dschrg = "yes")) ;

%put	NOTE: perf_discharge resolved to &perf_discharge;

%control_elements(	into = goal_items,
					where = %str(Element_Type = "goal")) ;

%put	NOTE: goal_items resolved to &goal_items;

* Macro for start and end dates ;

%macro control_elements_dates(	into =,
								where =,
								endpoint = /*	Use Start or End	*/);
	
	%global &into;
	
	PROC SQL NOPRINT ;
		SELECT	CATT('"', PUT(Element_Active_&endpoint, date9.), '"d')
		INTO	:&into separated by " "		
		FROM	work.control_irf_clean 
		WHERE	&where ;
	QUIT ;			
					
%mEnd;										

%control_elements_dates(	into = perf_admit_start,
							where = %str(Element_Type = "perform" and required_admit = "yes"),
							endpoint = Start) ;
							
%control_elements_dates(	into = perf_admit_end,
							where = %str(Element_Type = "perform" and required_admit = "yes"),
							endpoint = End) ;
							
%control_elements_dates(	into = perf_discharge_start,
							where = %str(Element_Type = "perform" and required_dschrg = "yes"),
							endpoint = Start) ;
							
%control_elements_dates(	into = perf_discharge_end,
							where = %str(Element_Type = "perform" and required_dschrg = "yes"),
							endpoint = End) ;

%control_elements_dates(	into = goal_start,
							where = %str(Element_Type = "goal"),
							endpoint = Start) ;
							
%control_elements_dates(	into = goal_end,
							where = %str(Element_Type = "goal"),
							endpoint = End)	;								

** Define performance and goal array lengths ;
	%LET perf_admit_length= %SYSFUNC(COUNTW(&perf_admit)) ;
	%let perf_discharge_length = %SYSFUNC(COUNTW(&perf_discharge)) ;
	%LET goal_length = %SYSFUNC(COUNTW(&goal_items)) ;

%put NOTE:	Import and QA provider-level waiver data if required	;



%put NOTE: Load Current Assessments for processing from workbench to work ;

PROC SQL ;
	CREATE TABLE work.irf_asmt_raw AS
	select 	asmt.*,
			prv.tieIn,
			prv.in_scope
	from 	&in_asmt asmt
				inner join
			&in_prvd	prv
				on asmt.ccn = prv.ccn
		;
QUIT ;

%put NOTE: QA fields used to define an unplanned discharge/incomplete stay ; 

title "QA fields used to define an unplanned discharge/incomplete stay" ; 
title2 "Fields should not have large swaths of missing values" ; 
proc sql ; 
select 	nmiss(dschrg_agnst_mdcl_advc_cd) as miss_damd
	,	nmiss(dschrg_alive_cd) as miss_alive
	,	nmiss(dschrg_dstntn_cd) as miss_destination
	,	nmiss(dschrg_dt) as miss_d_date
	,	nmiss(admsn_dt) as miss_a_date
from irf_asmt_raw
;
quit ; 
title ; 

/***** LET THE FUN BEGIN ---- APPLY APU logic to assessments - base case *****/

%put NOTE: Apply APU logic to Assesments data in work ;

DATA work.irf_asmt_compliance (COMPRESS=YES) ;
	SET work.irf_asmt_raw 	end = eof;

	* Base APU logic	;	
	* Determine Denominator: Denom=1 if after tieIn, otherwise 0 ;
	if tieIn < "&tiein_excptn"d then irf_asmt_denom=1 ;
	else irf_asmt_denom=0;

	* Hash table to capture perofrmance fields that are noncompliant	;
	call missing(incomplete_stay);
	length fail_field $32;
	drop fail_field;
	if _n_ = 1 then do;
	
		declare hash failures();
		rc = failures.defineKey(	"orgnl_asmt_id", 
									"fail_field");
		rc = failures.defineData(	"ccn",
									"orgnl_asmt_id", 
									"incomplete_stay", 
									"trgt_dt", 
									"quarter",
									"fail_field");
		rc = failures.defineDone();
		
	end;

	/* define and create stay length variable */
	stay_length = (dschrg_dt - admsn_dt) ; 

	if irf_asmt_denom then do;

		* Determine whether assessment refers to an incomplete stay	;
		* Discharge items are ignored for incomplete stays	;
		if	dschrg_agnst_mdcl_advc_cd in ("1")	/*	Discharged against medical advice	*/
			OR dschrg_alive_cd in ("0")			/*	Died	*/
			OR dschrg_dstntn_cd in ("02", "63", "65", "66")	 /*	Discharge destination not consistent with a complete stay	*/ 
			then incomplete_stay = 1;
		else incomplete_stay = 0;
		
		*	Logic for the BIMS PHQ issue	;
		if &fix_bims_phq = 1 then do;
			
			* old flavor of fix with AND logic ;
			if &strict = 1 then do ;
		
				*	Instructions from Acumen:
				When C0400A_1=[-] AND C0400B_1=[-] AND C0400C_1=[-], treat dashes (“-“) in these items as skips (“^”) and another valid response.
				;
				if	(rcall_first_word_cd = "-"
					and rcall_scnd_word_cd = "-"
					and rcall_thrd_word_cd = "-")
					and "&fix_start"d <= trgt_dt <= "&fix_end"d
				then do;
					rcall_first_word_cd = "^";
					rcall_scnd_word_cd = "^";
					rcall_thrd_word_cd = "^";
				end;
	
				* 	4/18/2023 update new guidance for all assessments; 
				*	Instructions from Acumen:
				When C0400A_2=[-] AND C0400B_2=[-] AND C0400C_2=[-], treat dashes (“-“) in these items as skips (“^”) and another valid response.
				;
	
				if	(rcall_first_word_dschrg_cd  = "-"
					and rcall_scnd_word_dschrg_cd  = "-"
					and rcall_thrd_word_dschrg_cd  = "-")
					and "&fix_start"d <= trgt_dt <= "&fix_end"d
				then do;
					rcall_first_word_dschrg_cd  = "^";
					rcall_scnd_word_dschrg_cd  = "^";
					rcall_thrd_word_dschrg_cd  = "^";
				end;
				
	
				*	Instructions from Acumen:
				If ‘Unplanned Discharge’=”yes” then:
				When C0100_2=[-] AND C0200_2=[-] AND C0300A_2=[-] AND C0300B_2=[-] AND C0300C_2=[-] AND C0400A_2=[-] 
				AND C0400B_2=[-] AND C0400C_2=[-] AND C0500_2=[-] AND D0150A_2=[-] AND D0150B_2=[-] AND D0150C_2=[-] AND D0150D_2=[-] 
				AND D0150E_2=[-] AND D0150F_2=[-] AND D0150G_2=[-] AND DO150H_2=[-] AND D0150I_2=[-], D0160_2=[-], 
				treat dashes (“-“) in these items as skips (“^”) and another valid response.
				;
				*	NOTES:
					1. The D series appear to have two fields each associated with each code (e.g., D0150A1 and D0150A2)
					2. Based upon errata and experience with the data, the D0150X2 series cannot take on a missing value, and neither can D0160.
					We accordingly do NOT check these fields.
					;
				*	4/18/2023 updates
					separated BIMS and PHQ logic
					also changed logic to BIMS to accomodate new fix on word recall ;
	 
				* 	09/15/2023 updates
					added CAMS fix (C1310A-D_2) ; 

				if	incomplete_stay = 1 then do ;
					 
					* check and fix BIMS ; 
					 if (cndct_mentl_stus_dschrg_cd='-'
					 	and word_rpet_frst_atmpt_dschrg_cd='-'
					 	and rpt_crct_yr_dschrg_cd='-'
					 	and rpt_crct_mo_dschrg_cd='-'
					 	and rpt_crct_day_dschrg_cd='-'
					 	and rcall_first_word_dschrg_cd='-'
					 	and rcall_scnd_word_dschrg_cd='-'
					 	and rcall_thrd_word_dschrg_cd='-'
					 	and bims_scre_dschrg_num='-')
						and "&fix_start"d <= trgt_dt <= "&fix_end"d
					then do;
						cndct_mentl_stus_dschrg_cd='^';
						word_rpet_frst_atmpt_dschrg_cd='^';
						rpt_crct_yr_dschrg_cd='^';
						rpt_crct_mo_dschrg_cd='^';
						rpt_crct_day_dschrg_cd='^';
						rcall_first_word_dschrg_cd='^';
						rcall_scnd_word_dschrg_cd='^';
						rcall_thrd_word_dschrg_cd='^';
						bims_scre_dschrg_num='^';
					end ; 
				
					if (lttl_intrst_dschrg_cd='-'
						 and down_dprsd_dschrg_cd='-'
						 and fall_aslp_dschrg_cd='-'
						 and feel_trd_dschrg_cd='-'
						 and aptit_ovreat_dschrg_cd='-'
						 and feel_bad_dschrg_cd='-'
						 and trbl_cncntrt_dschrg_cd='-'
						 and move_slow_dschrg_cd='-'
						 and thght_btr_dd_dschrg_cd='-')
						and "&fix_start"d <= trgt_dt <= "&fix_end"d
					then do;
						lttl_intrst_dschrg_cd='^';
						down_dprsd_dschrg_cd='^';
						fall_aslp_dschrg_cd='^';
						feel_trd_dschrg_cd='^';
						aptit_ovreat_dschrg_cd='^';
						feel_bad_dschrg_cd='^';
						trbl_cncntrt_dschrg_cd='^';
						move_slow_dschrg_cd='^';
						thght_btr_dd_dschrg_cd='^';
					end;

					if (mentl_stus_chg_dschrg_cd = "-"
						and	dfclty_attntn_dschrg_cd	= "-"
						and 	thnkg_disorgnz_dschrg_cd	= "-"
						and	lvl_conscs_dschrg_cd = "-")
						and "&fix_start"d <= trgt_dt <= "&fix_end"d
					then do;
							mentl_stus_chg_dschrg_cd = "^" ; 
							dfclty_attntn_dschrg_cd	= "^" ; 
							thnkg_disorgnz_dschrg_cd	= "^" ; 
							lvl_conscs_dschrg_cd = "^"	;
					end ; 	
		
	
				end ; /* end for incomplete stay fixes */
			end ;  /* end for strict version of fixes */
			
			if &strict = 0 then do ;
				%macro lenient_fix	(field = ) ;
					if "&fix_start"d <= trgt_dt <= "&fix_end"d  then do ;
						&field = tranwrd (&field,"-","^")  ;
					end ;
				%mend lenient_fix ;
			*	NEW 4/18/2023 Instructions from Acumen:
				When C0400A_1=[-] or C0400B_1=[-] or C0400C_1=[-], treat dashes (“-“) in these items as skips (“^”) or another valid response.
				;
				%lenient_fix (field = rcall_first_word_cd) ;
				%lenient_fix (field = rcall_scnd_word_cd) ;
				%lenient_fix (field = rcall_thrd_word_cd) ;
			
			
			*	NEW 4/18/2023 Instructions from Acumen:
				When C0400A_2=[-] or C0400B_2=[-] or C0400C_2=[-], treat dashes (“-“) in these items as skips (“^”) or another valid response.
				;
				%lenient_fix (field = rcall_first_word_dschrg_cd) ;
				%lenient_fix (field = rcall_scnd_word_dschrg_cd) ;
				%lenient_fix (field = rcall_thrd_word_dschrg_cd) ;
			
				*	NEW 4/18/2023 Instructions from Acumen:
				If ‘Unplanned Discharge’=”yes” then:
					When C0100_2=[-] or C0200_2=[-] or C0300A_2=[-] or C0300B_2=[-] or C0300C_2=[-] or C0400A_2=[-] 
					or C0400B_2=[-] or C0400C_2=[-] or C0500_2=[-] or D0150A_2=[-] or D0150B_2=[-] or D0150C_2=[-] or D0150D_2=[-] 
					or D0150E_2=[-] or D0150F_2=[-] or D0150G_2=[-] or DO150H_2=[-] or D0150I_2=[-], D0160_2=[-], 
					treat dashes (“-“) in these items as skips (“^”) or another valid response.
					;
				*	NOTES:
					1. The D series appear to have two fields each associated with each code (e.g., D0150A1 or D0150A2)
					2. Based upon errata or experience with the data, the D0150X2 series cannot take on a missing value, or neither can D0160.
					We accordingly do NOT check these fields.
					;
				*	4/18/2023 updates
					separated BIMS or PHQ logic
					also changed logic to BIMS to accomodate new fix on word recall ;
			 
				* 09/15/2023 updates
					added CAMS C1310A-D_2 ; 

					if	incomplete_stay = 1 then do ;				 
						* fix BIMS ; 
						%lenient_fix (field = cndct_mentl_stus_dschrg_cd) ;
						%lenient_fix (field = word_rpet_frst_atmpt_dschrg_cd) ;
						%lenient_fix (field = rpt_crct_yr_dschrg_cd) ;
						%lenient_fix (field = rpt_crct_mo_dschrg_cd) ;
						%lenient_fix (field = rpt_crct_day_dschrg_cd) ;
						%lenient_fix (field = rcall_first_word_dschrg_cd) ;
						%lenient_fix (field = rcall_scnd_word_dschrg_cd) ;
						%lenient_fix (field = rcall_thrd_word_dschrg_cd) ;
						%lenient_fix (field = bims_scre_dschrg_num) ;
			
						%lenient_fix (field = lttl_intrst_dschrg_cd) ;
						%lenient_fix (field = down_dprsd_dschrg_cd) ;
						%lenient_fix (field = fall_aslp_dschrg_cd) ;
						%lenient_fix (field = feel_trd_dschrg_cd) ;
						%lenient_fix (field = aptit_ovreat_dschrg_cd) ;
						%lenient_fix (field = feel_bad_dschrg_cd) ;
						%lenient_fix (field = trbl_cncntrt_dschrg_cd) ;
						%lenient_fix (field = move_slow_dschrg_cd) ;
						%lenient_fix (field = thght_btr_dd_dschrg_cd) ;	

						%lenient_fix (field = mentl_stus_chg_dschrg_cd) ; 
						%lenient_fix (field = dfclty_attntn_dschrg_cd) ;
						%lenient_fix (field = thnkg_disorgnz_dschrg_cd) ;
						%lenient_fix (field = lvl_conscs_dschrg_cd) ;

					end ; /* end for incomplete stay fixes */
			end ; /* end for lenient fix flavor */
		end ; /* end for BIMS/PHQ FIXES */


		* Numerators depend on compliance for assessment type: performance-admit, performance-discharge, and goal ;	
		
		*	Performance admission	;	
		
			* Check compliance for performance items to be included --> only count if the trgt_dt fails in the start/end dates ;
			* Set up arrays for PERFORMANCE items, start and end dates ;
			ARRAY perform_admit_items [&perf_admit_length] &perf_admit ;
			ARRAY perform_admit_start [&perf_admit_length]  _TEMPORARY_ (&perf_admit_start) ;
			ARRAY perform_admit_end [&perf_admit_length] _TEMPORARY_ (&perf_admit_end) ;
			
			* PERFORMANCE ADMIT compliance: ALL items have to have a valid value (no dashes) ;	
			* Variable PERFORM_ADMIT_CHECK contains concatenated performance items that fall in the current trgt_dt range ;
			*LENGTH perform_admit_check $250 ;

			
			* Pass variable defaults to yes (1)	;
			perform_admit_pass = 1;

			* Set up a do loop to concatenate performance items over APU dates b/w item start and end dates ;
		
			DO p_admit=1 to &perf_admit_length ;
				IF perform_admit_start[p_admit] <= trgt_dt <= perform_admit_end[p_admit] THEN DO ;
				
					fail_field = "";
					if strip(perform_admit_items[p_admit]) = "-" then do;
						*	Flag the assessment as failed	;
						perform_admit_pass = 0;
						*	Add the field to the failures hash table	;
						fail_field = vname(perform_admit_items[p_admit]);
						rc = failures.add();
					end;
					
				END ;
			END ;
			
			* Set up arrays for PERFORMANCE DISCHARGE items, start and end dates -- for complete stays only ;
			
			if incomplete_stay = 0 then do;
			
				ARRAY perform_discharge_items [&perf_discharge_length] &perf_discharge ;
				ARRAY perform_discharge_start [&perf_discharge_length]  _TEMPORARY_ (&perf_discharge_start) ;
				ARRAY perform_discharge_end [&perf_discharge_length] _TEMPORARY_ (&perf_discharge_end) ;	
			
				* PERFORMANCE DISCHARGE compliance: ALL items have to have a valid value (no dashes) ;	
				* Pass variable defaults to yes (1)	;
				perform_discharge_pass = 1;
				
				* Set up a do loop to concatenate performance items over APU dates b/w item start and end dates ;
				DO p_dsch=1 to &perf_discharge_length ;
					IF perform_discharge_start[p_dsch] <= trgt_dt <= perform_discharge_end[p_dsch] THEN DO ;
						fail_field = "";
						if	strip(perform_discharge_items[p_dsch]) = "-" then do;
							*	Flag the assessment as failed	;
							perform_discharge_pass = 0;
							*	Add the field to the failures hash table	;
							fail_field = vname(perform_discharge_items[p_dsch]);
							rc = failures.add();
						end;
					END ;
				END ;
			
			end;
			
			* Check compliance for goals items to be included --> only count if the trgt_dt fails in the start/end dates ;
			* Set up arrays for GOAL items, start and end dates ;	
			ARRAY goals [&goal_length] &goal_items ;
			ARRAY goals_start [&goal_length] _TEMPORARY_ (&goal_start) ;
			ARRAY goals_end [&goal_length] _TEMPORARY_ (&goal_end) ;

			* GOALS compliance: At least one goal must have a valid value (non-dash) ;
			* Variable GOAL_CHECK contains concatenated goals items that fall in the current trgt_dt range ;
			LENGTH goal_check $250 ;

			* Set up a do loop to concatenate goals items over APU dates b/w item start and end dates ;
			* Variable GOAL_VALID flags goal items that do not have a dash (-) as valid within this loop ;			
			goal_valid = 0 ;
			
			DO g=1 to &goal_length ;
				IF goals_start[g] <= trgt_dt <= goals_end[g] THEN DO ; 
					goal_check = catt(goal_check, goals[g]) ;
					IF goals[g] NOT IN('-', "^") THEN do ; 
						goal_valid = (goal_valid + 1) ;
						
					end ;
					 
				END ;
				else if ~(goals_start[g] <= trgt_dt <= goals_end[g]) then do ;
					goal_valid = . ;
					 
				end ; 
			END ;
			if goal_valid = . then goal_pass = . ; 
			else if goal_valid > 0 then goal_pass = 1 ; 
			else if goal_valid = 0  then do ; 
						goal_pass = 0 ; 
						fail_field = "Goal_Fail";
						rc = failures.add();
			end ;
			
		* Numerator computation: if assessments pass BOTH performance and goals, then they count towards the numerator ;
		* For incomplete stays, we ignore discharge performance items	;

		*	Complete stay case	;		
		IF 	incomplete_stay = 0
			and perform_admit_pass = 1 
			and perform_discharge_pass = 1
			AND goal_pass in (1, . )
			THEN irf_asmt_numer = 1 ;
		*	Incomplete stay case	;
		else if	incomplete_stay = 1
			and perform_admit_pass = 1 
			AND goal_pass in (1, . )
			THEN irf_asmt_numer = 1 ;
		*	Failure	;
		ELSE irf_asmt_numer = 0 ;
	
	end ; /*end for denoms */

	*	Output the failures hash table to file	;
	if eof then	rc = failures.output(dataset:"work.irf_assessment_failures");



	* Clean up: drop extra columns created for DO loops ;
	DROP	p_admit
			p_dsch
			g 
			rc;
RUN ;

%put NOTE: QA Assessments-Level Processing Logic ;

title	"QA -- work.irf_asmt_compliance";
title2	"Denominator value versus Tie In";
proc freq	data = work.irf_asmt_compliance;
	format	tieIn tie_applies.;
	table	tieIn * irf_asmt_denom / missing nocum nocol nopercent norow;
run;

title2	"QA -- Confirm incomplete stay correctly assigned";
title3	"An assessment should always be flagged as an incomplete stay if any of the following are true:";
title4	"Discharged against medical advice: dschrg_agnst_mdcl_advc_cd = 1";
title5	"Not discharged alive:  dschrg_alive_cd = 0";
title6	"Discharge location not consistent with a completed stay: dschrg_dstntn_cd in (02, 63, 65, 66)";

proc freq	data = work.irf_asmt_compliance;
	table	dschrg_agnst_mdcl_advc_cd * incomplete_stay
			dschrg_alive_cd * incomplete_stay
			dschrg_dstntn_cd * incomplete_stay
			/
			nocum
			norow
			nocol
			nopercent
			missing
			;
	where	irf_asmt_denom > 0;
run;




title2	"Confirm that discharge items are null for incomplete stays";

proc freq	data = work.irf_asmt_compliance;
	table	incomplete_stay * perform_discharge_pass
			/	nocum norow nocol nopercent missing;
	where	irf_asmt_denom > 0;
run;
		
title2	"Diagnostic -- Breakdown of failure points";

proc tabulate	data = work.irf_asmt_compliance;
	class	incomplete_stay
			perform_admit_pass
			perform_discharge_pass
			goal_pass
			irf_asmt_numer
			/
			missing
			;
	table	incomplete_stay
			,
			irf_asmt_numer
			,
			perform_admit_pass * N = ""
			perform_discharge_pass * N = ""
			goal_pass * N = ""
			;
	where	irf_asmt_denom > 0;
run;

title2	"Diagnostic -- Pass/Fail by complete stay";
proc freq	data = work.irf_asmt_compliance;
		table	incomplete_stay * irf_asmt_numer / chisq missing nocum  nocol;
		where	irf_asmt_denom > 0;
run;

title;

%put NOTE: added code related to goal fail - there should be no Goal gails 2023-Q4 and beyond ; 

title "Checking failure reasons in data - work.irf_assessment_failures " ; 
title2 "There should be no Goal related Failures for 2023-Q4 and beyond" ; 
proc sql ; 
	select count(*) as num_Goal_Fails,
		quarter
	from work.irf_assessment_failures
	where Fail_field = "Goal_Fail" 
	group by quarter
	;
quit ; 
title ; 


%if &fix_bims_phq = 1 and "&cycle" = "APU" %then %do;

	/* 4/18/2023 correction to QA logic
		fix of recall word discharges now apply to all assessments
		BIMS and PHQ checks separated  */

	/* 09/15/2023 updated to include CAMS fixes C1310A-d_2 */

	%put NOTE: Checks for BIMS and PHQ correction logic	;
	title	"QA: Checking outcomes of BIMS and PHQ correction logic";
	proc sql;
		title2	"Initial number of assessments with C0400A-C Admit any Dashes - by quarter";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(rcall_first_word_cd = "-"
				or rcall_scnd_word_cd = "-"
				or rcall_thrd_word_cd = "-")
			
				and tieIn < "&tiein_excptn"d
		group by quarter
				;
		title2	"Number of assessments with C0400A-C Admit any Dashes After Fix Applied - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.irf_asmt_compliance
		where	(rcall_first_word_cd = "-"
				or rcall_scnd_word_cd = "-"
				or rcall_thrd_word_cd = "-")
				
				and irf_asmt_denom > 0
		group by quarter
				;
	quit;

	proc sql;
		title2	"Initial number of assessments with C0400A-C discharge any Dashes - by quarter";
		select count(*) as initial_all_fail,
				quarter
		from	work.irf_asmt_raw
		where	(rcall_first_word_dschrg_cd = "-"
				or rcall_scnd_word_dschrg_cd = "-"
				or rcall_thrd_word_dschrg_cd = "-")
		
				and tieIn < "&tiein_excptn"d
		group by quarter
				;
		title2	"Number of assessments with C0400A-C discharge any Dashes After Fix Applied - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.irf_asmt_compliance
		where	(rcall_first_word_dschrg_cd= "-"
				or rcall_scnd_word_dschrg_cd = "-"
				or rcall_thrd_word_dschrg_cd = "-")
				
				and irf_asmt_denom > 0
		group by quarter
				;
	quit;
	
	proc sql;
		title2	"Initial number of incomplete stay assessments with BIMS any dash - by quarter";
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
		title2	"Number of incomplete stay assessments with BIMS any dash - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.irf_asmt_compliance
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
		title2	"Initial number of incomplete stay assessments with PHQ (presence only) any dash - by quarter";
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
		title2	"Number of incomplete stay assessments with PHQ (presence only) any dash - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.irf_asmt_compliance
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
		title2	"Initial number of incomplete stay assessments with CAMS (C1310A-D_2) any dash - by quarter";
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
		title2	"Number of incomplete stay assessments with CAMS (C1310A-D_2) any dash - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.irf_asmt_compliance
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
	
%end;
	 

/********* 

CREATE OUTPUT ---- Don't overwrite my output on QA, por favor y gracias!!!!

***************************************************/	


data out_apu.&out_fails;
	set work.irf_assessment_failures;
run;

data	out_apu.&out_asmt;
	set	work.irf_asmt_compliance;
run;



/**************************************END ACTIVE CODE ***************************************************/	

%put NOTE: END ACTIVE CODE ; 
