/*	Program creates provider-level tallies of claims for use in quarterly/annual APU reporting

	First, run the import_inpatient claims program to generate a claims extract. Use these claims extracts as inputs to this program.
	
	Created 8/25/2020 -- KAS
	
	Update 11/18/2020 -- 	updated references for Q2
							Added some QA to compare files against prior quarter's data
							
	Update	12/18/2020	--	Added control flags, to allow for claims tallies to be generated for some or all PACs -- KAS
	
	Update 01/6/2021	-- Run for 2020-Q3 outreach. Hospice files were NOT refreshed and parameters were not updated. -- KAS
	
	Update	04/02/2021	-- 	Claims file is now on workbench -- updated references accordingly.
							Separated out tally, QA, and output phases so you can QA without writing a new file.
							Control flags now used to set up librefs.
	
	Update 05/05/2021	-- DTA
							Updated revised Claims extract file name references
	Update 07/07/2021	-- DTA
							Updated output directories to reflect new folder structure from 21Q1. 
	Update 08/04/2021 	-- RJ
							Updated output directories to match 2021 Q1 APU, and QA compare files to 2021 Q1 OR
							
	Update 09/20/2021	-- KAS
							Updated inline QA to make results easier to interpret. Also updated instructions.
							
	Update 10/04/2021	-- KAS
							Updated inputs for 2021-Q2 OR
	
	Update 10/18/2021	-- Raki
							Updated inputs for 2021-Q2 OR Production run 

	Update 11/03/2021	-- Raki
							Updated inputs, control flags, Librefs & outfiles for 2021-Q2 APU Development
	
	Update 11/18/2021	-- Raki
							2021 Q2 APU Production Run

	update 12/21/2021	-- 	EO
						- 	2021 Q3 Outreach predevelopment
						- 	updated inputs and control flags
						- 	streamlined old library assignments --- all irf, ltch, and snf point to same locale just once now
						- 	parameretized the extract date for inputs and tally date for comparisons
						- 	added notes to log for sections

	update 01/05/2021	-- Raki (2021 Q3 OR development Run)
						 - updated no of quarters covered parameters

	update 01/17/2022	-- EO (2021 Q3 OR production run)
						- 	input date changed to prod pull

	update 02/04/2022   -- Chid (2021 Q3 APU development run)

	update 02/18/2022   -- Chid (2021 Q3 APU production run)
						-  updated the xtrct_dt parameter
						
	Update 3/4/2022		--	Pre-development update for Q4 OR
							Streamlined updating inputsand outputs
	
	Update 4/4/2022		-- Development for Q4 OR
						   Updated input parameters		
	Update 4/18/2022	-- Production for Q4 OR	
						   Updated input parameters
						   
	Update 5/3/2022		-- Development for Q4 APU
						- Updated parameters
	
	Update 5/19/2022 (EO)	-- Production for Q4 APU
	
	Update 07/05/2022 (Raki) -- Development for Q1 OR 2022
							  - Updated parameters
	
	Update 07/18/2022 (EO) - production inputs for Q1 OR

	update 08/03/2022 (EO) - development for 22Q1 APU
							-	added common macro usage where able
							-	removed some antiquated commenting
							-	redid library structure call for hospice
	update 08/17/2022 (EO) production 22Q1 APU
							- prod inputs
	
	update 10/05/2022 (EO) development for 22Q2 Outreach
	
	update 10/19/2022 (RJ) Production for 22Q2 Outreach

	update 11/02/2022 (EO) development for 22Q2 APU

	update 11/18/2022 (CT) production run for 2022-Q2 APU
	

	update 01/04/02023 EO development for 2022-Q3 OR
			-	expected updates for new cycle

	update 01/17/2023 EO prod for 2022-Q3 OR

	update 02/06/2023 (CT) Development run for 2022-Q3 APU ILS

	update 02/17/2023 (CT) Production run for 2022-Q3 APU ILS

	update 03/27/2023 (CT) Pre-development run for 2022-Q4 OR

    update 04/17/2023 (SS) Production run for 2022-Q4 OR

	update 05/04/2023 (CT) Development run for 2022-Q4 APU ILS and 2023-Q1 Hospice

	update 05/05/2023 (EO) new dev file run 22Q4 ILS/ 23Q1 Hospice
			- 	raw claims have duplicates spanning full year
			-	tally QA numbers will be off
			-	ticket opened, not correcting for development

	update 05/19/2023 (EO) prod 22Q4 ILS / 23Q1 Hospice APU
			-	claims duplicates appear resolved
			-	we were given guidance by eSimplicity to add
				claim segment number = 1 to ensure unique claims
				investigating whether our current extract criteria 
				would be affect by the change
				the extract criteria change would need to be approved 
				by CMS

	update 07/12/2023 (CT) Development run for 2023-Q1 OR

	update 07/18/2023 EO prod run 23Q1 OR

	update 08/14/2023 RJ dev run 23Q1 APU 
					 - Mannualy changed the Hospice old data Lib location, 
					   since old hospice does not have folder of its own  

	update 08/17/2023 CT Production run 2023-Q1 APU

	update 10/16/2023 SS Development run 2023-Q2 OR

	update 10/18/2023 RJ Prod run 2023-Q2 OR

	update 11/08/2023 RJ Dev run 2023-Q2 APU

	update 11/20/2023 RJ Prod run 2023-Q2 APU

	update 01/04/2024 RJ Dev run 2023-Q3 OR

	update 01/17/2024 RJ Prod run 2023-Q3 OR

	update 02/15/2024 CT Development run for 2023-Q3 APU

	update 03/06/2024 HS Prod run for 2023-Q3 APU

	update 04/08/2024 SS dev run 2023-Q4 OR

	update 04/16/2024 HS prod run 2023-Q4 OR

	update 05/06/2024 RJ Dev run 2023-Q4 APU
*/
%put NOTE: END header ; 

%put NOTE: get common macro program ; 
* main folder for reporting cycle ; 
%let folder = 2023-Q4 APU ; 
* date of common macro program ; 
%let cmn_prgm_date = 20240422 ; 
%put NOTE: the include statement below will write additional notes to the log that exist in the common program ; 


%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Support Programs/APU_Common_Macros_&cmn_prgm_date..sas";

%put NOTE:	Control flags	;
*	These should be set to 1 if you want to run extract, 0 otherwise	;		
%let run_ltch 		= &cmn_run_ltch;
%let run_irf 		= &cmn_run_irf;
%let run_snf 		= &cmn_run_snf;	
%let run_hospice 	= &cmn_run_hosp;

%put NOTE: control flags set to &=run_ltch &=run_irf &=run_snf &=run_hospice ; 



*	Year and quarter information;
** These get updated automatically from the common macros program;
%let year = &cmn_yyyy;
%put NOTE: &=year ; 
%let quarter = Q&cmn_q;
%put NOTE: &=quarter ; 
%let year_hosp = &cmn_yyyy_hosp;
%put NOTE: &=year_hosp ; 
%let quarter_hosp = Q&cmn_q_hosp;
%put NOTE: &=quarter_hosp ; 

*	YYQ macro variables automatically calculated	;
%let yyq = %substr(&year, 3, 2)&quarter;
%put NOTE: &=yyq ; 
%let yyq_hosp = %substr(&year_hosp, 3, 2)&quarter_hosp;
%put NOTE: &=yyq_hosp ; 

* date to assign to output files ; 
%let pull_date = %sysfunc(today(), yymmddn6.);

***	Update Prior year information every report cycle	;
** These have to updated manually;
*	Recommend comparing APU to APU and OR to OR	;
%let year_prior = 2023;
%let quarter_prior = Q3;

%let yyq_prior = %substr(&year_prior, 3, 2)&quarter_prior;

** These have to updated manually;
%let year_hosp_prior = 2023;
%let quarter_hosp_prior = Q4;

%let yyq_hosp_prior = %substr(&year_hosp_prior, 3, 2)&quarter_hosp_prior;;

*	Update run type	every report cycle -- either APU or OR	;
** These get updated automatically from the common macros program;
%let run_type = &cmn_cycle;
%put NOTE: &=run_type ; 

***	 These generally update automatically but take care	;
*	Librefs for output files and (for QA) last quarter's files.	;
*	Recommend comparing APU to APU and OR to OR	;

libname	outdir
		"/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&year.-&quarter &run_type/Extract and Tally Claims/Data";

%if &run_hospice = 1 %then %do;
	libname	old_hosp
			"/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&year_hosp_prior.-&quarter_hosp_prior Hospice/Extract and Tally Claims/Data";	
		
%end;	


		
%if &run_irf = 1 or &run_ltch = 1 or &run_snf = 1 %then %do;		

	libname	old_ils
			"/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&year_prior.-&quarter_prior &run_type/Extract and Tally Claims/Data";	
		
%end;		

*	Output files	;
%let out_hosp = clm_tally_hosp_&yyq_hosp._&pull_date;
%let out_irf = 	clm_tally_irf_&yyq._&pull_date;
%let out_ltch = clm_tally_ltch_&yyq._&pull_date;
%let out_snf = 	clm_tally_snf_&yyq._&pull_date;
		
*	Inputs	;
*** Manually Update extract date for extract files every report cycle; 
*** Other parameters should update themselves	;
%let xtrct_dt = 240502 ; 
%let xtrct_dt_hosp = 240502 ; 
%let in_hosp = outdir.CLAIMS_HOSPICE_&yyq_hosp._&xtrct_dt_hosp;
%let in_irf = outdir.CLAIMS_IRF_&yyq._&xtrct_dt;
%let in_ltch = outdir.CLAIMS_LTCH_&yyq._&xtrct_dt;
%let in_snf = outdir.CLAIMS_SNF_&yyq._&xtrct_dt;

***	Update every quarter!	;
*	Number of quarters covered by current quarters' filers (used for QA)	;

%if "&run_type" = "APU" %then %do;

	%let hosp_n_q = %substr(&quarter_hosp, 2, 1);
	%put NOTE: &=hosp_n_q ; 
	%let irf_n_q = %substr(&quarter, 2, 1);
	%put NOTE: &=irf_n_q ; 
	%let ltch_n_q = %substr(&quarter, 2, 1);
	%put NOTE: &=ltch_n_q ; 
	%let snf_n_q = %substr(&quarter, 2, 1);
	%put NOTE: &=snf_n_q ; 

%end;

%if "&run_type" = "OR" %then %do;

	%let hosp_n_q = 1;	/*	We don't really need this for OR but for the sake of argument...	*/
	%let irf_n_q = 1;
	%let ltch_n_q = 1;
	%let snf_n_q = 1;

%end;

*	Last quarter's files -- for QA	;
***	Manually update old tally file dates every report cycle ;
***	This should point to the prior tally for this run type (OR for OR and APU for APU);
%let old_dt = 240306 ; 
%let old_dt_hosp = 240306 ;

%let old_hosp = old_hosp.clm_tally_hosp_&yyq_hosp_prior._&old_dt_hosp;
%let old_irf = old_ils.clm_tally_irf_&yyq_prior._&old_dt;
%let old_ltch = old_ils.clm_tally_ltch_&yyq_prior._&old_dt;
%let old_snf = old_ils.clm_tally_snf_&yyq_prior._&old_dt;	

%if "&run_type" = "APU" %then %do;

	%let hosp_n_q_old = %substr(&quarter_hosp_prior, 2, 1);
	%put NOTE: &=hosp_n_q_old ; 
	%let irf_n_q_old = %substr(&quarter_prior, 2, 1);
	%put NOTE: &=irf_n_q_old ; 
	%let ltch_n_q_old = %substr(&quarter_prior, 2, 1);
	%put NOTE: &=ltch_n_q_old ; 
	%let snf_n_q_old = %substr(&quarter_prior, 2, 1);
	%put NOTE: &=snf_n_q_old ; 

%end;

%if "&run_type" = "OR" %then %do;

	%let hosp_n_q_old = 1;	/*	We don't really need this for OR but for the sake of argument...	*/
	%let irf_n_q_old = 1;
	%let ltch_n_q_old = 1;
	%let snf_n_q_old = 1;

%end;


%put NOTE:	End parameters	;		
		
options compress = no;		

 
%put NOTE: checking frequency of claim segment number in claims extracted ; 

%macro check_segment(in_table = ,
				control = ) ; 
	%if &control = 1 %then %do ; 
		title "Claim Segment number frequency of raw extract" ; 
		title2 "Hoping to see segment =1 for all records" ; 
		title3 "Checking &in_table." ; 
		proc freq data = &in_table ; 
			table clm_sgmt_num / missing ; 
		run ;
		title ; 
	%end ;
	
	%else %do ;
		%put NOTE: not running check segment due to control flag settings ; 
	%end ; 
%mEnd ; 

%check_segment(in_table = &in_hosp,
				control = &run_hospice) ;
%check_segment(in_table = &in_irf,
				control = &run_irf) ;
%check_segment(in_table = &in_ltch,
				control = &run_ltch) ;
%check_segment(in_table = &in_snf,
				control = &run_snf) ;

%put NOTE: macro function to make tally ; 

%macro make_claims_tally(	in =,
							out =,
							control =);

	/*	Only execute if control flag is set	*/
	%if &control = 1 %then %do;

		proc sql;
			create table &out as
			select	prvdr_num,
					count(*) as Claim_Count
			from	&in
			group by prvdr_num
			;
		quit;							
		
	%end;
	
	/*	If control flag is not set, write a note to the log	*/
	%else %do;
	
		%put NOTE: control flag for &out set to &control. Tally not produced.	;
	
	%end;
							
%mEnd make_claims_tally;							

%put NOTE: call macro for each setting based on control flag settings ; 

%make_claims_tally(	in = &in_hosp,
					out = work.&out_hosp,
					control = &run_hospice)
					
%make_claims_tally(	in = &in_irf,
					out = work.&out_irf,
					control = &run_irf)
					
%make_claims_tally(	in = &in_ltch,
					out = work.&out_ltch,
					control = &run_ltch)					
					
%make_claims_tally(	in = &in_snf,
					out = work.&out_snf,
					control = &run_snf)			
					
					
%put NOTE:	QA	;
%macro	claims_tally_QA(pac =,
						tally = work.&&out_&pac,
						old_tally = &&old_&pac,
						control = &&run_&pac) ;

	/*	Only execute if control flag is set	*/
	%if &control = 1 %then %do;
						
		title	"QA for &PAC Claims Tally";
		
		title2	"Compare provider count. If they aren't similar, departures should be explainable.";
		title3	"Big changes in number of providers may be explicable when comparing previous year's data against current data.";
		title4	"Providers that closed during prior year will not be reflected in current year's data.";
		
		proc sql;
			title5 "Prior Reporting Cycle";
			select count(*) as Providers
			from	&old_tally;
			title5	"Current Reporting Cycle";
			select count(*) as Providers
			from	&tally;
		quit;
		
		title2	"Average claims/provider/quarter. Should stay more or less the same.";
		proc sql;
			title3 "Prior Reporting Cycle";
			select	mean(Claim_Count) / &&&pac._n_q_old as Mean_Claim_Count
			from	&old_tally
			;
			title3	"Current Reporting Cycle";
			select	mean(Claim_Count) / &&&pac._n_q as Mean_Claim_Count
			from	&tally
			;
		quit;
		
		title2	"Change in providers included in file.";
		proc sql;
			title3 	"Present in prior reporting cycle but absent from current reporting cycle.";
			title4 	"This should generally be zero if comparing YTD data (e.g., current APU extract vs prior APU extract w/in same year.)";
			title5	"This is likely to be greater than zero if not performing a YTD to YTD comparison";
			title6	"(e.g., OR comparison current quarter vs prior quarter in same year ";
			title7	"or APU after change in reporting year, comparing YTD vs prior year.)";
			select	count(*) as Missing_from_Current
			from	&old_tally
			where	prvdr_num not in (select distinct prvdr_num from &tally)
			;
			title3 "Present in current reporting cycle but absent from prior reporting cycle.";
			title4 "This may increase as new providers open or exisitng providers start seeing Medicare patients that previously were not.";
			select	count(*) as Missing_from_Old
			from	&tally
			where	prvdr_num not in (select distinct prvdr_num from &old_tally)
			;
		quit;
		title;	
	
	%end;
	
	/*	If control flag is not set, write a note to the log	*/
	%else %do;
	
		%put NOTE: control flag for &pac set to &control. Tally not produced.	;
	
	%end;
						
%mEnd;		


%claims_tally_QA(	pac = IRF,control = &run_irf)				
%claims_tally_QA(	pac = LTCH,control = &run_ltch)	
%claims_tally_QA(	pac = HOSP,
					control = &run_hospice)
%claims_tally_QA(	pac = SNF,control = &run_snf)

%put NOTE:	Output	;

%macro	output_claims_tally(pac =,
							control = &&run_&pac,
							out_tally = outdir.&&out_&pac,
							work_tally= work.&&out_&pac);
	
	/*	Only execute if control flag is set	*/
	%if &control = 1 %then %do;
	
		data	&out_tally;
			set	&work_tally;
		run;
	
	%end;
	
	/*	If control flag is not set, write a note to the log	*/
	%else %do;
	
		%put NOTE: control flag for &pac set to &control. Tally not produced.	;
	
	%end;
	
%mEnd;	


%output_claims_tally(	pac = irf)
%output_claims_tally(	pac = ltch)
%output_claims_tally(	pac = snf)
%output_claims_tally(	pac = hosp,
						control = &run_hospice)

%put NOTE: End active code ; 