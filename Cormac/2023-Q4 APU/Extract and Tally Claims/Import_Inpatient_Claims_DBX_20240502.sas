/*

This program extracts inpatient claims data for Hopsice, IRF, LTCH, and SNF from 
the NCH Part A database on CDR and saves them to the workbench.




Update 5/21/2020 -- HCQS has changed the way it wants us to access data.
We no longer have access to the claims CASLIB; we're supposed to access
that using a connection to Hive. We also are expected to primarily use
a project Hive connection.

Update 8/20/2020 -- Minor change so that Hospice can be on a different quarter than the other PACs

Update 10/16/2020 -- 	Added swing bed claims to SNF. 
						Also tweaked all queries to use the "heavy" queue. Note sure if this is strictly necessary, but wanted to see how it works.
						
Update 11/18/2020 --	IRF criteria got changed from "R" or "T" to just "R." Fixed -- KAS		

Update 12/18/2020 --	Added control structures so that we can selectively run extracts for some or all of the extracts
						Created a common macro to run all extracts
						Added some basic QA/diagnostics
						Stopped using Heavy queue -- it didn't seem to make a difference in execution
						-- KAS
Update 01/06/2021 --	Updated for 2020-Q3 outreach. This means that we don't need data for hospice. Hospice parameters were not updated.	

Update 04/02/2021 --	Modified to write to the workbench instead of Hive.
						Separated import, QA, and output steps.		
						Added section allowing QA to be run on files that have already been extracted and written to workbench.
						
Update 4/15/2021 --		Tweaked extract query slightly so it will use patitioning on clm_thru_dt. This decreases the time this query takes to 
						run by a lot. Like, queries that used to take 20, 30, or more minutes now run in about 3.

Update 5/5/21-- Changing Date parameters to n6. and XXQX for naming claims extracts-- inline with other extracts.

Update 7/7/21--	Updated output directories to reflect new folder strucuture from 21q1.

Update 10/04/21 -- Updated output directory and dates to refect to Q2 OR
Update 10/18/21 -- Production run for Q2 OR

Update 11/03/21 -- Updated output directory and control flags for Q2 APU Dev
update 11/18/21 -- Production Run for Q2 APU

update 12/20/21 -- EO
				 - pre-development work for Q3 Outreach
update 01/05/2022 -- Raki
					- Q3 outreach development run
update 01/17/2022 -- EO
					- Q3 OR production run

update 02/04/2022 -- Chid
				    - Q3 APU development run (only IRF, LTCH and SNF)

update 02/18/2022 -- Chid
				    - Q3 APU production run (only IRF, LTCH and SNF)
update 02/22/2022 - EO
					- Q4 Hospice devlopment (Hospice only runs)

2021-Q4 Outreach
	03/03/2022 (EO)
		-	pre-development work
		
	4/4/2022 KAS 
		- Running for development. No code changes from pre-dev.
	04/18/2022 Raki
		 - Running the program for production.

2021-Q4 APU
	5/3/2022 KAS
		- Updated parameters for 2021-Q4 APU
		-	 Streamlined parameter update
	5/19/2022 EO
		- productions run

	07/05/2022 Raki
				- Dev run for 2022-Q1 OR
				- Changed the program name from Import_Inpatient_Claims_220605 to Import_Inpatient_Claims_220705.sas
	07/18/2022 EO
				- production run for 2022-Q1 OR.

	08/02/2022 - EO - 2022-Q1 APU
				-	development for APU
				-	converted macros to common assingments where able
				-	added put statements to log to follow best practices
				-	updated the header to reflect current usage of this program

	08/17/2022 - EO - production 22Q1 APU
				-	 no code changes

	10/05/2022 - EO - development for 22Q2 Outreach
				-	expected changes for reporting cycle change
	10/05/2022 - EO post dev run change
				- location of a call to hosp parameter threw a warning, fixed it
				- did not rerun
	10/18/2022 - Production run KAS

	11/02/2022 - EO development for 22Q2 APU

	11/02/2022 - (CT) Production run for 2022-Q2 APU
	
	01/04/2023 - EO - development for 2022-Q3 OR
		-	rearranged or wrote note to log on some parameters for ease of updating
		-	updated hardcoded years to trigger to common

	01/17/2023 - EO - prodcution run for 2022-Q3 OR

	02/06/2023 - (CT) - Development run for 2022-Q3 APU

    02/17/2023 - (SS) - Production run for 2022-Q3 APU

	03/06/2023 - (EO) - pre developement for 2022-Q4 OR

	04/17/2023 - (EO) - production for 2022-Q4 OR

	05/03/2023 - (EO) - development for 2022-Q4 APU ILS/ 23Q1 Hospice

	05/05/2023 - (EO) - added variables that may be claim id
						(cwf_clm_asgn_num)
						we saw possible duplicates in previous pull and need to invesitgate

	05/18/2023 - (EO) - production run for 2022-Q4 ILS, 2023Q1 Hospice
					  - cwf_clm_asgn_num removed (not what we thought it was)
					  - claim_sk and clm_sgmt_num added

	07/05/2023 (CT) -- Development run for 2023-Q1 OR
					  -- deleted legacy code used for old QA process (new QA process uses saved logs/results)
					  -- comments added/edited throughout the program

	07/18/2023 - (RJ) -- Production run for 2023-Q1 OR

	08/04/2023 (CT) -- Development run for 2023-Q1 APU

	08/17/2023 (SS) -- Production run for 2023-Q1 APU

	10/12/2023 (CT) -- Development run for 2023-Q2 OR

	10/18/2023 (EO)	-- Production run for 2023-Q2 OR

	11/07/2023 (HS) -- Development run for 2023-Q2 APU

	11/17/2023 (EO) -- Production run for 2023-Q2 APU
	
	12/28/2023 (HS) -- Development run for 2023-Q3 OR
					  -- modified extract_claims macro, added to_date to clm_thru_dt
					  -- modified extract_claims macro, modified to use between on OR comparisons 
					  -- modified Definitions for IRF, LTCH, and SNF, modified BIGINT to INT

	01/02/2024 (HS) -- dev run post decommission of Ambari 23Q3 OR

	01/16/2024 (HS) -- prod run post decommission of Ambari 23Q3 OR

	02/12/2024 (RJ) -- Dev run for 23Q3 APU

	03/01/2024 (SS) -- Prod run for 23Q3 APU

	04/09/2024 (SS) -- Dev run for 23Q4 OR

	04/15/2024 (EO) -- prod run for 23Q4 OR

	05/02/2024 (HS) -- Dev run for 23Q4 APU

*/
%put NOTE: END header ; 


%put NOTE: begin parameters ; 

%put NOTE: get common macro program ; 
* main folder for reporting cycle ; 
%let folder = 2023-Q4 APU ; 
* date of common macro program ; 
%let cmn_prgm_date = 20240422 ; 
%put NOTE: the include statement below will write additional notes to the log that exist in the common program ; 
%put NOTE: there will be additional notes to the log when the common definitions are used in the program at hand ; 

%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Support Programs/APU_Common_Macros_&cmn_prgm_date..sas";

%put NOTE:	Control flags automated based on common	;
*	These should be automatically set to 1 if you want to run extract, 0 otherwise	;		
%let run_ltch 		= &cmn_run_ltch;
%let run_irf 		= &cmn_run_irf;
%let run_snf 		= &cmn_run_snf;	
%let run_hospice 	= &cmn_run_hosp;

%put NOTE: control flags set to &=run_ltch &=run_irf &=run_snf &=run_hospice ; 


%put NOTE: program parameters with automated updates based on common file 	;

*	Check the dates and other parameters below carefully and update as needed. Most of these should be updated automatically from the common macros program.	;			
		
%let claim_vintage = &cmn_yy ;
%let claim_vintage_hosp = &cmn_yy_hosp;

%let claim_q = &cmn_qq ;
%let claim_q_hosp = &cmn_qq_hosp;

%put NOTE: claims will extract for &=claim_vintage and &=claim_q for IRF, LTCH, and SNF ;

%if &run_hospice = 1 %then %do ; 
	%put NOTE: claims will extract for &=claim_vintage_hosp and &=claim_q_hosp for Hospice ;
%end ;

*	RCYCLE should be APU or OR	(updated automatically from the common macros program);
%let rcycle = &cmn_cycle;
%put NOTE: claims will extract for reporting cycle &=rcycle ; 

%put NOTE:	Claim start and end dates formatted for Hive	
			Should be YTD for APU, current quarter only for OR	;
%if "&rcycle" = "APU" %then %do ; 
	
	%put NOTE: reporting year automated based on common; 
	%let hive_claim_start = "&cmn_yyyy.-01-01";

	%put NOTE: claims will extract for &=hive_claim_start for IRF, LTCH, and SNF ; 

%end ;

%if "&rcycle" = "OR" %then %do ; 
	%let hive_claim_start = "&cmn_qtr_start_hive";
	%put NOTE: claims will extract for &=hive_claim_start for IRF, LTCH, and SNF ; 
%end ;

%let hive_claim_end = "&cmn_qtr_end_hive";
%put NOTE: claims will extract for &=hive_claim_end for IRF, LTCH, and SNF ; 

%let hive_claim_start_hosp = "&cmn_yyyy_hosp.-01-01";
%let hive_claim_end_hosp = "&cmn_qtr_end_hive_hosp";
%put NOTE: &=hive_claim_start_hosp and  &=hive_claim_end_hosp will be used for Hospice ;


*	Output locations	;
%let workbench = 
	/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Extract and Tally Claims/Data;
libname outdir "&workbench";

%put NOTE: program parameters with static assignments --- update as needed ; 

*	Inputs	;
* Schema/Database;
%let schema_claims = nch_part_a;

*	Extraction date -- for constructing name of the outfile	;
%let report_date = %sysfunc(today(), yymmddn6.);

*	Outfiles	;
%let out_irf = CLAIMS_IRF_&claim_vintage.&claim_q._&report_date;
%let out_ltch = CLAIMS_LTCH_&claim_vintage.&claim_q._&report_date;
%let out_snf = CLAIMS_SNF_&claim_vintage.&claim_q._&report_date;
%let out_hosp = CLAIMS_HOSPICE_&claim_vintage_hosp.&claim_q_hosp._&report_date;


*	Definitions for IRF, LTCH, SNF (Stand Alones and Swing Beds) and Hospice claim types	;
*	Note that these use Hive syntax, not SAS.	;
%let def_irf =	%str(	nch_clm_type_cd = '60' 
						AND clm_fac_type_cd in('1', '4') 
						AND clm_srvc_clsfctn_type_cd = '1'
						AND ( cast(substr(prvdr_num, 3, 4) as INT) between 3025 and 3099
							OR	substr(prvdr_num, 3, 1) in('R', 'T')
							)
					);
%let def_ltch =	%str(	nch_clm_type_cd = '60' 
						AND clm_fac_type_cd in('1', '4') 
						AND clm_srvc_clsfctn_type_cd = '1'
						AND cast(substr(prvdr_num, 3, 4) as INT) between 2000 and 2299
					);
%let def_snf =	%str(	nch_clm_type_cd = '20' 
						AND clm_fac_type_cd = '2' 
						AND clm_srvc_clsfctn_type_cd = '1'
						AND cast(substr(prvdr_num, 3, 4) as INT) between 5000 and 6499 );

*	SNF Swing beds	;
%let def_snf_swb =	%str(	substr(prvdr_num, 3, 1) in ('U', 'W', 'Y') );
						
%let def_hospice = %str(nch_clm_type_cd = '50'
						AND clm_fac_type_cd = '8'
						AND clm_srvc_clsfctn_type_cd in ('1', '2')	);	
						


%put NOTE:	End program parameters	;

%put NOTE: macro function assignment for extract ; 

*	Load the claims data into Project Workbench	;
*	Generic macro to pull claims. Data are initially written to work library in preparation for QA.	;

%macro extract_claims(	outfile =,
						sourcefile =,
						def =,
						control =,
						claim_start =,
						claim_end =);
						
	/*	Run only if control flag is set to 1	*/
	%if &control = 1 %then %do;		
		%select_to_dataset(
			%STR(select	prvdr_num,
						nch_clm_type_cd,
						clm_pmt_amt,
						clm_fac_type_cd,
						clm_from_dt,
						clm_thru_dt,
						clm_srvc_clsfctn_type_cd,
						clm_finl_actn_ind,
						claim_sk,
						clm_sgmt_num
				from	&schema_claims..&sourcefile
				where	to_date(clm_thru_dt,'YYYY-MM-DD') >= &claim_start
						AND 
						((to_date(clm_from_dt, 'YYYY-MM-DD') between &claim_start and &claim_end)								
							or 
						(to_date(clm_thru_dt, 'YYYY-MM-DD') between &claim_start and &claim_end))
						AND clm_finl_actn_ind = 1
						AND clm_pmt_amt > 0
						AND &def),
			&outfile,
			use_dbx=Y);
	%end;
		
	/*	Do not run if control flag not set to 1, plus add a message to the log	*/
	%else %do;
		%put	NOTE: No QA for &outfile -- control flag set to &control;
	%end;	
%mEnd extract_claims;						

%put NOTE: Extract claims from CDR based on contol flag settings ; 

%extract_claims(outfile = work.&out_irf,
				sourcefile = ipsn_header,
				def = &def_irf,
				control = &run_irf,
				claim_start = &hive_claim_start,
				claim_end = &hive_claim_end)

%extract_claims(outfile = work.&out_ltch,
				sourcefile = ipsn_header,
				def = &def_ltch,
				control = &run_ltch,
				claim_start = &hive_claim_start,
				claim_end = &hive_claim_end)
				
%extract_claims(outfile = work.&out_snf,
				sourcefile = ipsn_header,
				def = %str(
								(
									(&def_snf)
									OR
									(&def_snf_swb)
								)
							),
				control = &run_snf,
				claim_start = &hive_claim_start,
				claim_end = &hive_claim_end)	
				
%extract_claims(outfile = work.&out_hosp,
				sourcefile = hosp_header,
				def = &def_hospice,
				control = &run_hospice,
				claim_start = &hive_claim_start_hosp,
				claim_end = &hive_claim_end_hosp)
	;			
%put NOTE:	QA	; 

%macro qa_claims(pac =,
				qafile= ,
				control = );
	%if &control = 1 %then %do;
		title	"Diagnostics -- record count and date ranges, &qafile";
		proc sql;
			select	count(*) as n_records,
					count(distinct prvdr_num) as n_providers,
					min(clm_thru_dt) as min_clm_thru format = date9.,
					max(clm_thru_dt) as max_clm_thru format = date9.
					from	&qafile
				;
		quit;
		title;
	%end;	
	/*	Do not run if control flag not set to 1, plus add a message to the log	*/
	%else %do;
		%put	NOTE: No QA for &qafile -- control flag set to &control;
	%end;
%mEnd qa_claims;

%qa_claims(	pac = irf,
				qafile= work.&out_irf,
				control = &run_irf)
			
%qa_claims(	pac = ltch,
				qafile= work.&out_ltch,
				control = &run_ltch)
			
%qa_claims(	pac = snf,
				qafile= work.&out_snf,
				control = &run_snf)
			
%qa_claims(	pac = hospice,
				qafile= work.&out_hosp,
				control = &run_hospice)
;


%put NOTE: Output to workbench ; 
/*******	Output	 **********/
%macro output_claims(pac =,
					lib_name= ,
					workfile = , 
					outfile = ,
					control = 
					);
	%if &control = 1 %then %do;
		data &lib_name..&outfile;
			set	&workfile;
		run;
	%end;
	
	%else %do;
		%put NOTE: No output file for &pac;
	%end;
%mEnd output_claims;


%output_claims(pac = irf, 
				lib_name=outdir,
 				workfile = work.&out_irf,
				outfile = &out_irf,
				control = &run_irf)
					
%output_claims(pac = ltch,
				lib_name=outdir,
 				workfile = work.&out_ltch,
				outfile = &out_ltch,
				control = &run_ltch)

%output_claims(pac = snf,
				lib_name=outdir,
 				workfile = work.&out_snf,
				outfile = &out_snf,
				control = &run_snf)

%output_claims(pac = hospice,
				lib_name=outdir,
 				workfile = work.&out_hosp,
				outfile = &out_hosp,
				control = &run_hospice)	
				
%put NOTE: End active code ; 		
				