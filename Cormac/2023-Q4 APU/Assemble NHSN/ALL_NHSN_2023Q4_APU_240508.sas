/*	This program imports NHSN files capturing measures that we report on with a monthly granularity (CLABSI, CAUTI, CDI, COVID), cleans the data,
	and consolidates measures for each provider type. Data source is flat files received from the CDC.
	
	The program is based off of previous programs that carried out these functions for each provider type separately. These were consolidated
	as the logic was very consistent from provider type to provider type.
	
	Created 5/4/2022 for 2021-Q4 APU -- KAS
	
	Updates
	
	5/6/2022 KAS
		- 	Updated provider consistency macro -- it was pointed out in QA that SNFs have a totally different set of fields in the provider
			table than IRF and LTCH. Parameterized this macro to accommodate.
			
	5/23/2022 KAS
		- Updated for prod
		- Added a couple extra "THIS PARAMETER NEEDS TO BE UPDATED PAY ATTENTION" comments
		- Fixed a problem with the PERIOD macro not resolving inside of a macro. Too many quote marks. It works now but there are a ton
		of superfluous WARNING messages.
		- Added a note for another WARNING message
	5/23/2022 EO
		- there was an if statement that was not triggering correctly
		- I conditioned the trigger
		- I also quoted the title statement in question above
		- and removed the notes to the log.
		
	7/5/2022 KAS
		- Updated for 2022-Q1 OR dev
		- Fixed errors
			- Regular comment in a macro was causing weird macro errors
			- Subset on clctn_prd_cd in %process_flat_file was occurring BEFORE field was cleaned up; this has been fixed
			- Also the cleanup step for clctn_prd_cd inteacted poorly with the COVID file, which already has months zero padded (CAUTI, CLABSI, and CDI don't)
			- Naming convention for LTCH CovidVac files has changed so that it agrees with the IRF and SNF naming conventions
			- Labeling for cleaned CCN confirmity check said it was raw -- this was fixed to say cleaned
			- CLABSI misnamed in LTCH import titles
			
	7/5/2022 KAS (again)
		- Converted comments in nhsn_provider_consistency into block comment style
		- At Liz's request, added a note for LTCH consistency QA about provider 332006, which isn't really an LTCH and can be disregarded
		
	7/6/2022 KAS
		- Run with refreshed CDR data
	7/19/2022 EO
		-	production inputs
		
	8/3/2022 KAS
		- 2022-Q1 APU development inputs
		- Adapted to work with common macro program as appropriate
	8/17/22 EO
		- 22Q1 production inputs for nhsn and providers

	10/12/22 CT
		- 2022-Q2 OR Development run
		- assigned local variables for ccn patterns (which point to apu common macros)
	10/19/2022 EO
		- production inputs

	11/03/2022 EO
		- 2022-Q2 APU development

	11/22/2022 CT
		- 2022-Q2 APU Production

	01/05/2023 CT
		- 2022-Q3 OR Development run
		- added a filter for only valid months in the Quarter in the QA step 
		  where current and previous files are compared provider consistency

	01/18/2023 CT
		- 2022-Q3 OR Production run

	02/06/2023 CT
		- 2022-Q3 APU Development run (with 7 and 15 day files)

	02/17/2023 EO
		- 2022-Q3 production

	03/23/2023 CT
		- 2022-Q4 OR Development run (with March monthly/candidate final files)

	04/18/2023 CT
		- 2022-Q4 OR Production run (with April monthly/candidate final files)


	05/04/2023 CT
		- 2022-Q4 APU ILS and 2023-Q1 APU Hospice Development run 
		  (with May 2023 15-day CDC/NHSN files)

	05/18/2023 EO
		- 2022Q4 APU production run

	07/13/2023 CT
		- 2023-Q1 OR Development run (with June monthly/candidate final files)

	07/19/2023 EO
		-	23q1 or prod run

	07/20/2023 CT
		- program name fixed from 2022Q4_APU to 2023Q1_OR (did not re-run)
	
	08/16/2023 RJ
		- 2023 Q1 APU Development run

	08/18/2023 SS
		- 2023 Q1 APU Production run 	

	10/12/2023 RJ
		- 2023 Q2 OR Development run 

	10/18/2023 EO
		- 2023 q2 OR prod	

	11/08/2023 RJ
		- 2023 Q2 APU Development run 

	11/22/2023 RJ
		- 2023 Q2 APU Prod run 

	01/05/2024 RJ
		- 2023 Q3 OR Dev run 

	01/18/2024 EO
		- 2023 Q3 OR prod run

	02/13/2024 RJ
		- 2023 Q3 APU Dev run

	03/14/2024 HS
		- 2023 Q3 APU Prod run

	03/25/2024 HS
		- 2023 Q3 APU Prod re run
		- using prod 23Q2 231122 for YTD(LTCH,IRF)
		- using prod 23Q2 240314 for YTD(SNF)

	04/09/2024 SS
		- 2023 Q4 OR dev run

	04/16/2024 EO 2023-Q4 OR prod

	05/08/2024 HS
		- 2023 Q4 APU Dev run

*/	

%put NOTE: End Header ; 

%put NOTE: *** PROGRAM PARAMETERS *** ;

* 	Reference to common macros ;
/******* manually change the folder name/path and program date ********/
%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";


**	Control flags -- determine if code is run for IRF, LTCH, SNF	;
*	Determined by common macro file	;
%let run_irf = &cmn_run_irf;
%let run_ltch = &cmn_run_ltch;
%let run_snf = &cmn_run_snf;
%put	NOTE: Run flags from common macro program:	;
%put	NOTE: 
		&=run_irf	
		&=run_ltch	
		&=run_snf
		;

** Report type: OR vs. APU --- this is an output label, updated by common macro file ;
%LET type = &cmn_cycle ;
%put	NOTE: &=type;

** Quarter and year updated by common macro program	; 
%LET year = &cmn_yy ;
%let qq = &cmn_q;
%LET quarter = &cmn_qq ;
%put	NOTE: Check date variables set by common macro
		&=year
		&=qq
		&=quarter
		;

*	The following parameters should update automatically	;
*	These should resolve to the format 20XXMYY, where YY is zero padded	;
%LET firstmonth 	= "20&year.M%sysfunc(putn(%eval((&qq*3)-2), z2.))" ;
%LET secondmonth 	= "20&year.M%sysfunc(putn(%eval((&qq*3)-1), z2.))" ;
%LET thirdmonth 	= "20&year.M%sysfunc(putn(%eval(&qq*3), z2.))" ;
%LET qmonths = %QUOTE(&firstmonth,&secondmonth,&thirdmonth);
%put NOTE: Checking qmonths macro variable: &qmonths;

/******* YTD control flag -- update every control cycle ********/
* OR will always be current quarter -- APU will be current quarter in Q1, YTD in Q2-Q4 ;
* 1 = create cumulative file, 0 = current quarter only ;
%LET YTD = 1 ;

*	Assembly date -- updates automatically	;
%LET asmbld_date = %sysfunc(today(),yymmddn6.) ;

** Database and workbench reference - update only if folder structure changes; 
%LET bench = /workspace/workbench/pac_qrp_swingtech/data ; 
%LET folder = SAS/Quarterly and Annual Reporting ;
%LET output_folder = Assemble NHSN/Data ;
%LET prvdr_folder = Extract and Assemble Providers/Data ;

* current reporting cycle folder ;
%LET qrtr_folder = &cmn_folder ;
%put	NOTE: folder for this quarter defined by common macro.
		&=qrtr_folder
		;

*** Prior run locations -- update every reporting cycle	;
* For runs in which you need YTD data, folder containing the prior YTD file;
* This should always be the previous APU Folder;
%let ytd_folder = 2023-Q3 APU;
	
* Previous Cycle Folder (can be either OR or APU) -- used for comparison in QA	;
* This may be different from the YTD folder	;
%LET old_qrtr_folder = 2023-Q4 OR;

%put NOTE: when the ytd folder and the old qrt folder are the same there will be note about same library name reference --- this is expected ; 

** Establish librefs. These should update automatically ;

LIBNAME	nhsnold
		"&bench/&folder/&old_qrtr_folder/&output_folder" ;

LIBNAME nhsnpac
		"&bench/&folder/&qrtr_folder/&output_folder";

LIBNAME nhsnprv
		"&bench/&folder/&qrtr_folder/&prvdr_folder";
		
** NHSN flat files references - manually update each run/quarter ;
	* for development --- choose THE MOST RECENT FILE between folder options below ;
	* Development: use 7&15 day Files files ;
	* Production: use candidate_final files ;

** for NHSN flat file reference - manually update each run/reporting cycle/quarter ; 
	* for development: Use the latest files in Candidate_final directory.
	  There is a 7-day/15-day directory, but using files from it hasn't shown significant benefits.;
	* for production: Use the official production dated files within the Candidate_final directory.
	  These files are typically dated 30 days before the quarterly submission deadline for OR cycles 
	  and the day after the quarterly data submission deadline for APU cycles.;

%LET nhsn_dir = &bench/CDC_files/candidate_final/2024_04 ;

%let nhsn_date = 04_16_2024;

*	References to input files below should update automatically.	;
*	CAUTION: these are case sensitive. Some of the references end up being a bit eccentric as a result.	;

	*	IRF: CAUTI, CDI, COVID	;
	%LET in_cauti_irf	= IRF_CAUTI_20&year.&quarter._fac_&nhsn_date..txt ; 
	%LET in_cdi_irf		= IRF_CDI_20&year.&quarter._fac_&nhsn_date..txt ;
	%let in_covid_irf	= IRF_CovidVac_20&year.&quarter._fac_&nhsn_date..txt; 	
	
	*	LTCH: CAUTI, CDI, CLABSI, COVID	;
	%LET in_cauti_ltch	= LTAC_CAUTI_20&year.&quarter._fac_&nhsn_date..txt ; 
	%LET in_cdi_ltch	= LTAC_CDI_20&year.&quarter._fac_&nhsn_date..txt ;
	%LET in_clabsi_ltch	= LTAC_CLABSI_20&year.&quarter._fac_&nhsn_date..txt ;
	%let in_covid_ltch	= LTAC_CovidVac_20&year.&quarter._fac_&nhsn_date..txt; 
	
	*	SNF: COVID	;
	%let in_covid_snf	= SNF_CovidVac_20&year.&quarter._fac_&nhsn_date..txt;
	
** Previous assembled files --- Update each run/quarter ;
*	"old" file is used for comparison during QA -- most recent NHSN assembled files, either OR or APU	;
*	Assuming all NHSN assembly files were created on the same day, update this date once to update all prior files	;
*	These parameters will calculate automatically for you. Make sure they work for your circumstances	;

*** UPDATE HERE ***	;
%let old_date = 240416;

*	NULL data step assigns default macro variables to old YYQ and cycle variables	;
*	APU defaults to this quarter's OR file
	OR defaults to prior quarter's APU file
	;
data _null_;
	*	Old Year/Quarter and cycle designations for OR cycles	;
	if "&type" = "OR" then do;
		call symputx(
				"old_yq",
				put(
					intnx(
							"qtr",
							yyq(20&year, &qq),
							-1
							),
					yyq4.
					)
				);
		call symputx("old_cycle", "APU");
	end;
	*	Old Year/Quarter and cycle designations for APU cycles	;
	else if "&type" = "APU" then do;
		call symputx("old_yq",
					catt(&year, "q", "&qq")
					);
		call symputx("old_cycle", "OR");					
	end;
run;

%LET old_qrtr_NHSN_ltch	= ltch_nhsn_&old_yq.&old_cycle._&old_date._full;
%let old_qrtr_NHSN_irf	= irf_nhsn_&old_yq.&old_cycle._&old_date._full;
%let old_qrtr_NHSN_snf	= snf_nhsn_&old_yq.&old_cycle._&old_date._full;

%put	NOTE: Prior quarter files (for comparison) set by program:
		&=old_qrtr_NHSN_ltch
		&=old_qrtr_NHSN_irf
		&=old_qrtr_NHSN_snf
		;
	
*	For creating YTD files -- most recent YTD files	;
*	These should generally be the most recent APU file -- not OR	;
%if &ytd = 1 %then %do;
	
	libname nhsnytd
			"&bench/&folder/&ytd_folder/&output_folder" ;
		
	/*	Note that this will calculate a quarter value of 0 if it's quarter 1.
		That's of course a nonsense value BUT we don't actually use a YTD file in the first quarter, anyway.
		Hopefully this won't mess stuff up! Nested the whole logic inside a control flag conditioned to YTD = 1 just to be tidier...	*/
	%let ytd_q = %eval(&qq-1);
	/*** UPDATE QUARTERLY	***/
	/*	File date for YTD NHSN file. If you are creating a YTD output, you will combine your imported file with this.	*/
	/*** UPDATE HERE	***/
	%let ytd_date = 240325;
	
	%LET ytd_ltch_NHSN	= nhsnytd.ltch_nhsn_&year.Q&ytd_q.apu_&ytd_date._full ;
	%LET ytd_irf_NHSN	= nhsnytd.irf_nhsn_&year.Q&ytd_q.apu_&ytd_date._full ;
	%LET ytd_snf_NHSN	= nhsnytd.snf_nhsn_&year.Q&ytd_q.apu_&ytd_date._full ;
	
	%put 	NOTE: YTD files set by program:
			&=ytd_ltch_NHSN
			&=ytd_irf_NHSN
			&=ytd_snf_NHSN
			;
%end;

*	YTD filenames will be set to null if YTD = 0 (NE 1, really) -- this streamlines file combination later	;
%if &ytd NE 1 %then %do;

	%LET ytd_ltch_NHSN	= ;
	%LET ytd_irf_NHSN	= ;
	%LET ytd_snf_NHSN	= ;

%end;

** Current provider extract ;
*** UPDATE HERE EVERY DEV PHASE	***	;
%let prvdr_xtract_dt = 240502;
*	These should auto-update	;
%LET ltch_prvdr	= ltch_prvdr_xtrct_&prvdr_xtract_dt;
%LET irf_prvdr	= irf_prvdr_xtrct_&prvdr_xtract_dt;
%LET snf_prvdr	= snf_prvdr_xtrct_&prvdr_xtract_dt; %put NOTE: CDR issue was resolved. Harcode of the date for SNF file was removed. ; 
	
** Output files	;
%let out_ltch	= ltch_NHSN_&year&quarter.&type._&asmbld_date._FULL;
%let out_irf	= irf_NHSN_&year&quarter.&type._&asmbld_date._FULL;
%let out_snf	= snf_NHSN_&year&quarter.&type._&asmbld_date._FULL;


** CCN patterns (assigned from the APU common macros program);
%let ccn_irf = &ccn_pattern_irf ;
%let ccn_ltch = &ccn_pattern_ltch ;
%let ccn_snf = &ccn_pattern_snf ;


%put NOTE: *** END PROGRAM PARAMETERS	***	;


%put NOTE:	Macros Declarations	;
*	Macros to read the flat files. CLABSI/CAUTI/CDI have different formats than COVID.	;
*	In the future it may make sense to add these to a common macro file, control table, or other external reference.;

%MACRO read_nhsn_data (	output_data=,
				  		input_data=);

	data &output_data  ;
	
		  infile &input_data
		  delimiter='09'x 
		  missover 
		  dsd 
		  firstobs=2 ;
	
	
	      informat RPT_GRP_CD $6. ;
	      informat LOCN_TYP $3. ;
	      informat CLCTN_PRD_CD $7. ;
	      informat MSR_ID $8. ;
	      informat GRP_RATE_NMRTR $1. ;
	      informat GRP_RATE_DNMNTR $1. ;
	      informat GRP_RATE $1. ;
	      informat GRP_RATE_CI_LOWER $1. ;
	      informat GRP_RATE_CI_UPPER $1. ;
	      informat GRP_SIR_NMRTR best32. ;
	      informat GRP_SIR_DNMNTR best32. ;
	      informat GRP_SIR best32. ;
	      informat GRP_SIR_CI_LOWER best32. ;
	      informat GRP_SIR_CI_UPPER best32. ;
	      informat GRP_ADJST_SIR_NMRTR $1. ;
	      informat GRP_ADJST_SIR_DNMNTR $1. ;
	      informat GRP_ADJST_SIR $1. ;
	      informat GRP_ADJST_SIR_CI_LOWER $1. ;
	      informat GRP_ADJST_SIR_CI_UPPER $1. ;
	      informat GRP_DVC_PRCDR_CNT best32. ;
	      informat GRP_THRESH_IND best32. ;
	      informat GRP_ICU best32. ;
	      informat GRP_SCA $1. ;
	      informat GRP_WARD_OTHR best32. ;
	      informat cmplt_sbmsn $1. ;
	
	      format RPT_GRP_CD $6. ;
	      format LOCN_TYP $3. ;
	      format CLCTN_PRD_CD $7. ;
	      format MSR_ID $8. ;
	      format GRP_RATE_NMRTR $1. ;
	      format GRP_RATE_DNMNTR $1. ;
	      format GRP_RATE $1. ;
	      format GRP_RATE_CI_LOWER $1. ;
	      format GRP_RATE_CI_UPPER $1. ;
	      format GRP_SIR_NMRTR best12. ;
	      format GRP_SIR_DNMNTR best12. ;
	      format GRP_SIR best12. ;
	      format GRP_SIR_CI_LOWER best12. ;
	      format GRP_SIR_CI_UPPER best12. ;
	      format GRP_ADJST_SIR_NMRTR $1. ;
	      format GRP_ADJST_SIR_DNMNTR $1. ;
	      format GRP_ADJST_SIR $1. ;
	      format GRP_ADJST_SIR_CI_LOWER $1. ;
	      format GRP_ADJST_SIR_CI_UPPER $1. ;
	      format GRP_DVC_PRCDR_CNT best12. ;
	      format GRP_THRESH_IND best12. ;
	      format GRP_ICU best12. ;
	      format GRP_SCA $1. ;
	      format GRP_WARD_OTHR best12. ;
	      format cmplt_sbmsn $1. ;
	
	
	      input
		   RPT_GRP_CD $
		   LOCN_TYP  $
		   CLCTN_PRD_CD  $
		   MSR_ID  $
		   GRP_RATE_NMRTR  $
		   GRP_RATE_DNMNTR  $
		   GRP_RATE  $
		   GRP_RATE_CI_LOWER  $
		   GRP_RATE_CI_UPPER  $
		   GRP_SIR_NMRTR
		   GRP_SIR_DNMNTR
		   GRP_SIR
		   GRP_SIR_CI_LOWER
		   GRP_SIR_CI_UPPER
		   GRP_ADJST_SIR_NMRTR  $
		   GRP_ADJST_SIR_DNMNTR  $
		   GRP_ADJST_SIR  $
		   GRP_ADJST_SIR_CI_LOWER  $
		   GRP_ADJST_SIR_CI_UPPER  $
		   GRP_DVC_PRCDR_CNT
		   GRP_THRESH_IND
		   GRP_ICU
		   GRP_SCA  $
		   GRP_WARD_OTHR
		   cmplt_sbmsn  $;
	
	run;

%mEnd read_nhsn_data;

%MACRO read_nhsn_data_covid (output_data=,
				  			input_data=);

	data &output_data  ;
	
		  infile &input_data
		  delimiter='09'x 
		  missover 
		  dsd 
		  firstobs=2 ;

		  informat RPT_GRP_CD $6. ;
          informat CLCTN_PRD_CD $7. ;
          informat MSR_ID $8. ;
          informat HCP_GRP $9. ;
          informat ONSITE_ADHR_CNT $1. ;
          informat OFFSITE_ADHR_CNT $1. ;
          informat CONTRA_ADHR_CNT best32. ;
          informat DCLN_ADHR_CNT best32. ;
          informat GRP_ADHR_NMRTR best32. ;
          informat GRP_ADHR_DNMNTR best32. ;
          informat GRP_ADHR_PCT best32. ;
          informat GRP_ADHR_PCT_CI_LOWER best32. ;
          informat GRP_ADHR_PCT_CI_UPPER best32. ;
          informat CMPLT_SBMSN $1. ;

          format RPT_GRP_CD $6. ;
          format CLCTN_PRD_CD $7. ;
          format MSR_ID $8. ;
          format HCP_GRP $9. ;
          format ONSITE_ADHR_CNT $1. ;
          format OFFSITE_ADHR_CNT $1. ;
          format CONTRA_ADHR_CNT best12. ;
          format DCLN_ADHR_CNT best12. ;
          format GRP_ADHR_NMRTR best12. ;
          format GRP_ADHR_DNMNTR best12. ;
          format GRP_ADHR_PCT best12. ;
          format GRP_ADHR_PCT_CI_LOWER best12. ;
          format GRP_ADHR_PCT_CI_UPPER best12. ;
          format CMPLT_SBMSN $1. ;
      	 
		  		input
                   RPT_GRP_CD  $
                   CLCTN_PRD_CD  $
                   MSR_ID  $
                   HCP_GRP  $
                   ONSITE_ADHR_CNT  $
                   OFFSITE_ADHR_CNT  $
                   CONTRA_ADHR_CNT
                   DCLN_ADHR_CNT
                   GRP_ADHR_NMRTR
                   GRP_ADHR_DNMNTR
                   GRP_ADHR_PCT
                   GRP_ADHR_PCT_CI_LOWER
                   GRP_ADHR_PCT_CI_UPPER
                   CMPLT_SBMSN  $;
      run;

%mEnd read_nhsn_data_covid;


** Create macro to process flat files from workbench. Calls the macros that import flat files. ;
%macro process_flat_file 	(msr = ,
							folder = &nhsn_dir,
							in_file = ,
							table = ,
							title = ,
							period = &qmonths,
							ccn_pattern =) ;

	FILENAME	&msr
				DISK
				"&folder/&in_file" ;	
	/* importing the specified measure file= raw table*/
	
	title "&title";
	
	/*	COVID and non-COVID files have different structures. We also have to do an extra check on the COVID measure to make sure we have correct
		values for HCP_GRP	*/
	%if &msr = COVID %then %do;
			
		%read_nhsn_data_covid(output_data = &table._raw,
						input_data = &msr)
	
		/* Covid Raw Table QA */ 
		title2 "Counts of HCP_GRP values in raw import &table._raw";
		title3	"We only care about records where HCP_GRP = ALL. Confirm this value is present and properly formatted.";
		proc freq data=&table._raw;
			table HCP_GRP;
		run;
			
	%end;
	
	%else %do;
	
		%read_nhsn_data(output_data = &table._raw,
					input_data = &msr)
	
	%end;
	
	title2	"Check formatting of CCNs in raw imported file.";
	title3	"1 = conforming, 0 = not conforming";
	title4	"Next step will attempt to correct case where nonconforming fields have a dropped zero (common-ish problem)";
	proc sql;
		select	prxmatch("&ccn_pattern", rpt_grp_cd) as conforms_to_CCN,
				count(*) as Records
		from	&table._raw
		group by calculated conforms_to_CCN
		;
	quit;
	
	/* 	Fix CCN if necessary. 
		Drop records for HCP_GRPs that we don't care about if appropriate.
		Zero pad month.
		Keep only required fields
	*/
	DATA &table ; 
		
		%if &msr = COVID %then
			%do;
				SET	&table._raw (WHERE = (HCP_GRP IN("ALL")));  /* subset covid nhsn on HCP_GRP = ALL */
			%end;
		%else
			%do;
				SET	&table._raw;
			%end;
		if prxmatch("/\b[0-9A-Z]{5}\b/", rpt_grp_cd) then
		/*	In the event that a CCN has lost its zero padding, add it back.
			This will ONLY trigger if the CCN is exactly five alphanumeric characters long.	*/
		rpt_grp_cd = prxchange("s/\b([0-9A-Z]{5})\b/0${1}/", 1, rpt_grp_cd);
		
		/* Put leding zero in front of month for Q1-Q3 if applicable */
		
		clctn_prd_cd = compress(
									prxchange("s/(\b\d{4}M)(\d\b)/$1 0 $2/i", -1, clctn_prd_cd)
								);
		
		if clctn_prd_cd IN(&period);
		KEEP	rpt_grp_cd
				clctn_prd_cd
				msr_id
				cmplt_sbmsn	;
	RUN ;

	title2	"Column names and types in clean table (&table)";
	
	ods select position;
	proc contents 	data = &table
					order = varnum
					;
	run;

	/* Checking for the row counts in the raw table */

    TITLE2 	"Counts of months (clctn_prd_cd) in raw import, &table._raw" ;
    /*title3	"Should include the months required for the report period %bquote(&period).";*/
    title4	"There will be other months and quarterly summaries as well.";
    PROC FREQ DATA=&table._raw  ;
        TABLE clctn_prd_cd ;
    RUN ;
	
	/* Checking for the row counts in the measure table */ 

    TITLE2 "Count of months in cleaned table, &table.";
    /*title3	"Should include only months in %bquote(&period), from above, formatted appropriately (M##)";*/
	PROC FREQ DATA=&table ;
		TABLE clctn_prd_cd ;
	RUN ;

	/* Check CCNs - cleaned file */
	TITLE2	"CCN check in cleaned file, &table";
	title3	"CCNs should have 6 characters and conform to expected pattern (&ccn_pattern)" ;
	PROC SQL ;
		TITLE4	"Correctly formatted CCNs" ;	
		SELECT	COUNT(*) AS clean_CCN_Correctly_Formatted
		FROM	&table
		WHERE 	prxmatch("&ccn_pattern", rpt_grp_cd) ;

		TITLE4	"Incorrectly formatted CCNs" ;	
		SELECT	COUNT(*) AS clean_CCN_Incorrectly_Formatted
		FROM	&table
		WHERE	~prxmatch("&ccn_pattern", rpt_grp_cd)  ;
	QUIT ;			
	TITLE ;
	
%mENd process_flat_file ;

%if &run_irf ~= 1 %then %do;

	%put NOTE: &run_irf not equal to 1, IRF flat files will not be imported.	;

%end;

%if &run_irf = 1 %then %do;

	%put NOTE: Begin IRF flat file imports and combine	;

	%process_flat_file (msr = CAUTI,
				  		in_file = &in_cauti_irf,
				  		table = work.irf_cauti,
				  		title = Import flat file for IRF CAUTI,
				  		ccn_pattern = &ccn_irf) ;
	
	%process_flat_file (msr = CDI,
				  		in_file = &in_cdi_irf,
				  		table = work.irf_cdi,
				  		title = Import flat file for IRF CDI,
				  		ccn_pattern = &ccn_irf) ;
	
	%process_flat_file (msr = COVID,
				  		in_file = &in_covid_irf,
				  		table = work.irf_covid,
				  		title = Import flat file for IRF COVID,
				  		ccn_pattern = &ccn_irf) ;
				  		
	%put NOTE: There were some field size changes in 2021-Q4 -- this may cause a MULTIPLE LENGTHS WARNING OH NO! Not an issue.	;
	data	work.irf_all;
		set	work.irf_cauti
			work.irf_cdi
			work.irf_covid
			&ytd_irf_NHSN
			;
	run;
	
%end;

%if &run_ltch ~= 1 %then %do;

	%put NOTE: &run_ltch not equal to 1, LTCH flat files will not be imported.	;

%end;

%if &run_ltch = 1 %then %do;

	%put NOTE: Begin LTCH flat file imports and combine ;
	%process_flat_file (msr = CAUTI,
				  		in_file = &in_cauti_ltch,
				  		table = work.ltch_cauti,
				  		title = Import flat file for LTCH CAUTI,
				  		ccn_pattern = &ccn_ltch) ;
	
	%process_flat_file (msr = CLABSI,
				  		in_file = &in_clabsi_ltch,
				  		table = work.ltch_clabi,
				  		title = Import flat file for LTCH CLABSI,
				  		ccn_pattern = &ccn_ltch) ;
	
	%process_flat_file (msr = CDI,
				  		in_file = &in_cdi_ltch,
				  		table = work.ltch_cdi,
				  		title = Import flat file for LTCH CDI,
				  		ccn_pattern = &ccn_ltch) ;
	
	%process_flat_file (msr = COVID,
				  		in_file = &in_covid_ltch,
				  		table = work.ltch_covid,
				  		title = Import flat file for LTCH COVID,
				  		ccn_pattern = &ccn_ltch) ;
				  		
	data work.ltch_all;
		set	work.ltch_cauti
			work.ltch_clabi	  		
			work.ltch_cdi
			work.ltch_covid
			&ytd_ltch_NHSN
			;
	run;
	
%end;

%if &run_snf ~= 1 %then %do;

	%put NOTE: &run_snf not equal to 1, SNF flat files will not be imported.	;

%end;

%if &run_snf = 1 %then %do;

	%put NOTE: Begin SNF flat file imports;
	%process_flat_file (msr = COVID,
				  		in_file = &in_covid_snf,
				  		table = work.snf_covid,
				  		title = Import flat file for SNF COVID,
				  		ccn_pattern = &ccn_snf) ;
				  		
	data work.snf_all;
		set work.snf_covid
			&ytd_snf_NHSN
			;
	run;
				  		
%end;

%put NOTE: Macro to run QA on the completed file	;
%macro NHSN_QA(	table=,
				ccn_pattern=) ;

	TITLE "QA -- assembled NHSN file &table";	
	TITLE2	"Check column layout and formats" ;
	
	ods select position;
	proc contents	data = &table
					order = varnum
					;
	run;
	
	TITLE2	"Confirm no Missing Levels for any fields" ;
	title3	"CMPLT_SBMSN should have two levels (assumed Y/N)";
	title4	"MSR_ID and CLCTN_PRD_CD checked below.";
	title5	"RPT_GRP_CD equivalent to CCN, expected to have many levels.";
	PROC FREQ	DATA = &table
			 	NLEVELS ;
		TABLE	msr_id
				clctn_prd_cd
				cmplt_sbmsn
				RPT_GRP_CD
				/
				MISSING NOPRINT ;
	RUN ;

	TITLE2	"Confirm uniform distribution across months and measures" ;
	title3	"Confirm that all required months and measures are present.";
	PROC FREQ DATA = &table ;
		TABLE msr_id * clctn_prd_cd /NOCOL NOROW NOPERCENT ;
	RUN ;
	
	* Check CCNs ;
	TITLE2	"CCNs should have 6 characters and conform to expected patterns (&ccn_pattern)" ;
	PROC SQL ;
	TITLE3	"Correctly formatted CCNs" ;	
		SELECT	COUNT(*) AS CCN_Correctly_Formatted
		FROM	&table 
			WHERE 	prxmatch("&ccn_pattern", rpt_grp_cd) ;

	TITLE4	"Incorrectly formatted CCNs" ;	
	SELECT	COUNT(*) AS CCN_Incorrectly_Formatted
		FROM	&table 
			WHERE	~prxmatch("&ccn_pattern", rpt_grp_cd)  ;
	QUIT ;	
	
	TITLE ;

%mEnd NHSN_QA;

%put NOTE: Run final file QA macro	;

%if &run_irf = 1 %then %do;
	
	%NHSN_QA(	table= work.irf_all,
				ccn_pattern= &ccn_irf)
	
%end;

%if &run_ltch = 1 %then %do;
	
	%NHSN_QA(	table= work.ltch_all,
				ccn_pattern= &ccn_ltch)
	
%end;

%if &run_snf = 1 %then %do;
	
	%NHSN_QA(	table= work.snf_all,
				ccn_pattern= &ccn_snf)
	
%end;

%put NOTE: Macro to compare current file against prior file for provider consistency	;
*	SNF providers have a totally different set of fields than IRF/LTCH, so we parameterize all of these.
	The default parameters are for IRF and LTCH
	;
%macro	nhsn_provider_consistency(	table=,
									old_qtr=,
									provider=,
									prv_id = prvdr_num,
									prv_term = trmntn_exprtn_dt,
									prv_opd = orgnl_prtcptn_dt,
									prv_add = csp_prvdr_add_dt,
									prv_update = csp_updt_dt)
									;
	/*	Comparison across reporting cycles */
	TITLE	"Check for provider differences between reporting cycles -- &table" ;
	TITLE2	"Number of providers from last report not in current report file" ;	
	PROC SQL ;
		SELECT	COUNT(DISTINCT rpt_grp_cd) AS num_prev_not_curr
		INTO:	num_prev_not_curr
		FROM	&old_qtr
		WHERE	rpt_grp_cd NOT IN(SELECT rpt_grp_cd FROM &table
								  where CLCTN_PRD_CD in (&qmonths)) 
		;
	QUIT ;
	
	/*	Only look at provider details if there is a discrepancy */
	%IF &num_prev_not_curr = 0 %THEN %DO ;
		%PUT NOTE: No discrepant providers were found. Below code to check differences not run. ;
	%END ;
	
	%IF &num_prev_not_curr > 0 %THEN %DO ;
		TITLE3	"They should all be closed" ;
		%if %index(&table, ltch) > 0 or %index(&table, LTCH) > 0 %then %do;
			title4 "For LTCH, provider 332006 is known to not be a real LTCH, disregard";
		%end;
		PROC SQL ;
			SELECT	nhsn.rpt_grp_cd,
					nhsn.last_observed,
					prvdr.&prv_term LABEL="closed_dt",
					prvdr.&prv_update
			FROM	(SELECT	rpt_grp_cd,		
							MAX(clctn_prd_cd) AS last_observed
					FROM	&old_qtr
						WHERE rpt_grp_cd NOT IN(SELECT rpt_grp_cd FROM &table where CLCTN_PRD_CD in (&qmonths)) 
					GROUP BY	rpt_grp_cd) nhsn
				LEFT JOIN 
				&provider prvdr
				ON nhsn.rpt_grp_cd = prvdr.&prv_id ;
		QUIT ;
	%END ;


	TITLE2	"Number of providers in current report not in previous file" ;
	PROC SQL ;
		SELECT	COUNT(DISTINCT rpt_grp_cd) AS num_curr_not_prev
		INTO:	num_curr_not_prev
		FROM	&table
		WHERE	rpt_grp_cd NOT IN(SELECT rpt_grp_cd FROM &old_qtr)
				AND CLCTN_PRD_CD in (&qmonths)
		;
	QUIT ;

	/*	Only look at provider details if there is a discrepancy */
		%IF &num_curr_not_prev = 0 %THEN %DO ;
		%PUT NOTE: No discrepant providers were found. Below code to check differences not run. ;
	%END ;
	
	%IF &num_curr_not_prev > 0 %THEN %DO ;
		TITLE3	"They should all be new" ;
		PROC SQL ;
			SELECT	nhsn.rpt_grp_cd,
					prvdr.&prv_opd LABEL="OPD",
					prvdr.&prv_add,
					prvdr.&prv_update
			FROM 	(SELECT	DISTINCT rpt_grp_cd
					 FROM	&table
						WHERE rpt_grp_cd NOT IN(SELECT rpt_grp_cd FROM &old_qtr)
							AND CLCTN_PRD_CD in (&qmonths)) nhsn
					LEFT JOIN
					&provider prvdr
				ON nhsn.rpt_grp_cd = prvdr.&prv_id ;
		QUIT ;
	%END ;
	
	TITLE ;
	
%mEnd nhsn_provider_consistency;

%put NOTE: Run provider consistency macro	;

%if &run_irf = 1 %then %do;
	%nhsn_provider_consistency(	table=work.irf_all,
								old_qtr= nhsnold.&old_qrtr_NHSN_irf,
								provider= nhsnprv.&irf_prvdr)
								
%end;

%if &run_ltch = 1 %then %do;
	%nhsn_provider_consistency(	table=work.ltch_all,
								old_qtr= nhsnold.&old_qrtr_NHSN_ltch,
								provider= nhsnprv.&ltch_prvdr)
								
%end;	

%if &run_snf = 1 %then %do;
	%nhsn_provider_consistency(	table=work.snf_all,
								old_qtr= nhsnold.&old_qrtr_NHSN_snf,
								provider= nhsnprv.&snf_prvdr,
								prv_id = mcare_id,
								prv_term = closeddate,
								prv_opd = partci_dt,
								prv_add = adddate,
								prv_update = sys_ld_id) /*	sys_ld_id may not actually be the right field here, NB -- may be the date
															the database was most recently updated, not the record. Best we can do for now. */
								
%end;

%put NOTE: Outputs	;
%macro nhsn_output(	workfile =,
					outfile =);
				
	PROC SORT	DATA = &workfile
				out = &outfile ;
		BY	rpt_grp_cd
			msr_id
			clctn_prd_cd ;
	RUN ;
				
%mEnd nhsn_output;					

%if &run_irf = 1 %then %do;

	%nhsn_output(workfile = work.irf_all,
				outfile = nhsnpac.&out_irf)

%end;

%if &run_ltch = 1 %then %do;

	%nhsn_output(workfile = work.ltch_all,
				outfile = nhsnpac.&out_ltch)

%end;

%if &run_snf = 1 %then %do;

	%nhsn_output(workfile = work.snf_all,
				outfile = nhsnpac.&out_snf)

%end;