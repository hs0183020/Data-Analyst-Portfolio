/*	

	This program imports NHSN HCP Flu Vaccine data for IRF and LTCH -- received from CDC as a flat file -- and converts it to a SAS dataset
	so it can be used by programs downstream. Resulting dataset should include one record per provider along with CMPLT_SBMSN_CD, which indicates
	whether the provider is in compliance or not.
	
	Notes:
	
	Records are uniquely identified by provider (RPT_GRP_CD) and health care provider group (HCP_GRP.) HCP_GRP can take on four levels; we
	want to use the group "ALLHCP" for our APU analysis; all other levels should be dropped from output.
	
	Created 3/29/2021 -- DTA
	
	Updated 3/30/2021 -- KAS
	
		Restructured to bring into compliance with coding standards.
		Added QA
		Restructured to run for both IRF and LTCH

	Updated 4/16/2021 -- PP
		Updated input parameters for production run
		
	Update 5/10/2021 -- KAS
		Updated input for 2020-Q4 APU development
		(OK, actually for development we're using the 2020-Q4 OR file, so really just re-running for the sake of
		creating a file with a current date of creation and updating the output location...)
		
	Update	5/18/2021 -- KAS
		Updated input for 2020-Q4 APU production

	Update	03/10/2022 -- KAB
		- updated input for 2021-Q4 OR pre-development
		- updated output libname and paramater references

	update 04/04/2022 -- EO
		-	development inputs

	Update	03/10/2022 -- KAB
		- updated inputs for 2021-Q4 OR production
		
	Update 5/4/2022 -- KAS
		- Updated inputs for 2021-Q4 APU dev
		- Streamlined parameter maintenance
		- Tidied up QA output a bit
		
	Update 5/23/2022 -- KAS
		- Updated inputs for 2021-Q4 APU prod

	Update 03/23/2023 -- CT
		- 2022-Q4 OR Development run (with March monthly/candidate final files)
		- added reference to the common macros program
		- added code for SNF FluVac based on IRF/LTCH

	update 04/18/2023 -- EO
		- 2022-Q4 OR production run

	Update 05/04/2023 -- CT
		- 2022-Q4 APU ILS Development run 
		  (with May 2023 15-day CDC/NHSN files)

	update 05/18/2023 -- EO
		- 2022-Q4 APU prod run


	update 04/12/2024 -- CT
		- 2023-Q4 OR Development Run
		- renamed the file to make it consistent with the ALL_NSHN program
		- used the 2024_03 Candidate Final/Monthly CDC Files

	update 04/16/2024 -- EO
		- 2023-Q4 OR prod

	update 05/06/2024 -- RJ
		- 2023-Q4 APU Dev
		- used the 2024_05 7&15 day Files
*/	

/************************************PROGRAM PARAMETERS***************************************/
* 	Reference to common macros ;
/******* change the folder name/path and program date ********/
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


*** INPUT: workbench reference - update as needed; 
%LET bench = /workspace/workbench/pac_qrp_swingtech/data ; 

*	Note that nhsn_dir will change between dev and prod
	Dev will typically use the 7 and 15 day files, which are in the directory
		7&15 day Files
	While prod uses candidate final files which are in the directory
		candidate_final
		;	
%LET nhsn_dir = &bench/CDC_files/7&15 day Files/2024_05 ;  * update once the files are received from CDC;

*	Update YYYYQQ and file date -- these will then be used to automatically update file names, below	;
*	Important -- these dates are for the INPUT FILE, not the REPORTING QUARTER, and they apply to INPUTS	;
%let yyyyqq = 2024Q1;
%let yyqq = %substr(&yyyyqq, 3, 4);
%let file_date = 05_02_2024;

%LET irf_fluv = IRF_FluVac_&yyyyqq._fac_&file_date..txt ;  
%LET ltch_fluv = LTAC_FluVac_&yyyyqq._fac_&file_date..txt ;
%LET snf_fluv = SNF_FluVac_&yyyyqq._fac_&file_date..txt ;
***  OUTPUT: Location references-- assign output path for workbench;

** Quarter and year updated by common macro program -- they will automatically update outputs, below	;
%let report_year = &cmn_yy;
%let report_q = &cmn_q;

%put	NOTE: Check date variables set by common macro
		&=report_year
		&=report_q
		;

** Report cycle: OR vs. APU --- this is an output label, updated by common macro file ;
%let report_cycle = &cmn_cycle;
%put	NOTE: &=report_cycle;



*	Parameters below should update automatically...	;
%LET assy_date = %sysfunc(today(),yymmddn6.) ;

libname out_dir "&bench./SAS/Quarterly and Annual Reporting/20&report_year.-Q&report_q &report_cycle./Assemble NHSN/Data";
%let out_irf = out_dir.irf_hcpflu_&report_year.Q&report_q._&assy_date;
%let out_ltch = out_dir.ltch_hcpflu_&report_year.Q&report_q._&assy_date;
%let out_snf = out_dir.snf_hcpflu_&report_year.Q&report_q._&assy_date;

*** Create macro to import and process flat files from workbench ;

%macro process_flat_file (	PAC =,
							in_file=);

/*	Import Flu Vaccine flat file	*/
data WORK.FLUV_RAW_&pac    ;
	infile "&nhsn_dir/&in_file"
			delimiter='09'x 
			MISSOVER 
			DSD 
			FIRSTOBS=2
			;
	informat	RPT_GRP_CD	
				CLCTN_PRD_CD MSR_ID		$6.00 	;
	informat	HCP_GRP					$9.00 	;
	informat	GRP_ONSITE_ADHR_CNT		$9.00 	;
	informat	GRP_OFFSITE_ADHR_CNT	$9.00 	;
	informat	GRP_CONTRA_ADHR_CNT		$9.00 	;
	informat	GRP_DCLN_ADHR_CNT		$9.00 	;
	informat	GRP_ADHR_NMRTR			$9.00 	;
	informat	GRP_ADHR_DNMNTR			$9.00 	;
	informat	GRP_ADHR_PCT			$9.00 	;
	informat	GRP_ADHR_PCT_CI_LOWER	$9.00 	;
	informat	GRP_ADHR_PCT_CI_UPPER	$9.00 	;
	informat	CMPLT_SBMSN				$1.00 	;
	
	input	RPT_GRP_CD 
			CLCTN_PRD_CD 
			MSR_ID  
			HCP_GRP  
			GRP_ONSITE_ADHR_CNT 
			GRP_OFFSITE_ADHR_CNT 
			GRP_CONTRA_ADHR_CNT 
			GRP_DCLN_ADHR_CNT 
			GRP_ADHR_NMRTR 
			GRP_ADHR_DNMNTR 
			GRP_ADHR_PCT 
			GRP_ADHR_PCT_CI_LOWER 
			GRP_ADHR_PCT_CI_UPPER 
			CMPLT_SBMSN  $;		
	
run;

* 	QA for the table Generated;
TITLE "QA for &pac HCP Flu"; 

title2	"Structure and field formats";
ods select position;
PROC CONTENTS 
	 DATA=WORK.FLUV_RAW_&pac 
	 VARNUM ;	
RUN ;


title2	"Field Values";
title3	"MSR_ID should ONLY be HCWFLU";
title4	"clctn_prd_cd is determined by the flu season -- generally should be Q1 of (the reporting year + 1)";
title5	"cmplt_sbmsn should be Y or N";
title6	"hcp_grp should include ALLHCP, which is the group we want to report off of.";
title7	"Also, all hcp_grp levels should have the same number of records (1/provider)";
PROC FREQ DATA=WORK.FLUV_RAW_&pac;
	TABLE	msr_id
			clctn_prd_cd			
			cmplt_sbmsn
			hcp_grp
			/	MISSING;
RUN;


TITLE2	"CCNs should have 6 characters" ;
proc sql;
	title3	"Correctly formatted CCNs";	
	select count(*) as CCN_Correctly_Formatted
	from	WORK.FLUV_RAW_&pac 
	where	length(rpt_grp_cd) = 6 
	;
	
	title3	"Incorrectly formatted CCNs";	
	select count(*) as CCN_Incorrectly_Formatted
	from	WORK.FLUV_RAW_&pac
	where	length(rpt_grp_cd) ~= 6  
	;
	
	title3	"Third position of CCN (validate provider type)";
	select	substr(rpt_grp_cd, 3, 1) as CCN_Third_Position,
			count(*) as Providers
	from	work.fluv_raw_&pac
	group by	calculated CCN_Third_Position
	;
quit;		

/*	Clean up file -- drop fields we don't need and subset to include only */
data	work.fluv_&pac;
	set	work.fluv_raw_&pac;
	/*	We don't actually need most of these fields; keep only those that are relevant	*/
	keep	RPT_GRP_CD
			CLCTN_PRD_CD
			MSR_ID
			CMPLT_SBMSN	
			;
			
	/*	We only care about the "ALLHCP" group (cumulative)	*/
	if	hcp_grp = "ALLHCP";

run;

title2	"QA -- Record count for clean FluVac file, &PAC";
proc sql;
	select count(*) as Records
	from work.fluv_&pac
	;
quit;

TITLE;
%mENd process_flat_file ;

%process_flat_file(	PAC = IRF,
					in_file= &irf_fluv);
					
%process_flat_file(	PAC = LTCH,
					in_file= &ltch_fluv);	

%process_flat_file(	PAC = SNF,
					in_file= &snf_fluv);		
					
*	OUTPUT	;

data	&out_irf;
	set	work.FLUV_IRF;
run;

data	&out_ltch;
	set	work.FLUV_LTCH;
run;


data	&out_snf;
	set	work.FLUV_SNF;
run;

