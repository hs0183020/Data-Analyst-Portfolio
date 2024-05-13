/*************************************************************** HEADER *******************************************************
	TITLE: Hospice_Asmt_
	
	AUTHOR: Pam Phojanakong (PP)
	Edited: Elizabeth Oliver (EO)

	IMPORTANT: Hospice is a quarter ahead of the other PAC QRPs


	PURPOSE:	This program is the most current and production version used for APU reporting.
				It uses the extracted assessment file from the main and history tables 
				to find the earliest submitted assessment that is not an inactivation.
				The current extraction method for both tables pulls all assessments
				from the beginning of the reporting year to the end of the reporting
				quarter for this report.


	INPUTS:	 There are multiple inputs
			-	Current Quarter Assessment extract
			-	Current Quarter History extract 
			-	Previous Quarter assembled assessments 
			-	Control Table (Excel) (Hospice is a quarter ahead of the other PAC QRPs)

	NOTES:	To update this file review the macro variable definitions. 
                Quarterly updates: inputs and their directories, date, and output file naming parameters 
				The schema, database and libname references can be modified as needed.
				
				This code has the following main parts: 
				- 	Parameter definition and libname references via macro variables 
				-	QA extracts before assembly 
				-	Extract assembly: Combine historical and current assessments within report scope
					taking the earliest submission that is not an Inactivation
				- 	compare assembled assessments to previous reporting files (for all but Q1)
				-	QA final assessments
				-	Write final combined assessments to Workbench

	UPDATES: 
	05-AUG-2021 (PP): 
		-	Adapted from 2021-Q1 APU assembly code (HOSPC_Asmt_2021Q1_20210511_PP.sas)
		-	added in revised de-dup logic from 2021-Q1 OR cycle (used IRF as a reference)
		-	simplified the naming and librefs
	09-AUG-2021 (PP): 
		-	applied post-QA edits made to IRF
		-	no longer using PROC DATASETS to create _nodup datasets
		-	using gate %IF/%THEN to assign datasets to &XXXX_nodup macros
		-	shored up naming of intermediate datasets
	19-AUG-2021 (PP): Production
		- updated inputs, added some notes to log for the inputs
	10-NOV-2021 (EO): 2021-Q3 APU development
		-	made header wording generic for ease of maintenance
		-	updated inputs and parameters for Q3 APU
		-	added signposting notes to log
		-	added a few notes to log for the benefit of tired QA peeps
		-	amended commenting styles in a few macro definitions and within gate logic
		-	reworded a few titles and removed some older, unneeded comments
		-	added row count checks to output, from IRF code
	19-NOV-2021 (EO): 2021-Q3 APU production
		-	production inputs


	25-FEB-2022 (EO): 2021-Q4 APU development
		-	updated parameters as possible
		-	moved processing of control table to after parameters
		-	changed (but did not test) pre-import data check for format based on changes in other settings
		-	added more notes to log to delineate important parameters, macro definition, macro calls, etc
	04-MAR-2022 (EO): production run
		-	production inputs
		-	new control table required amendments to cleaning steps

	19-MAY-2022 (EO): 2022-Q1 APU development (in 2021-Q4 APU folders)
		-	development updates for 22Q1 Hospice
		-	added common macro call for control table import and ccn checks
		-	added common macro calls for assessment processing
		-	pulled date cleaning out of the branched code, since it was same for both cases of ytd control flag
		-	switched up some parameter assignments to make maintenance easier
	25-MAY-2022 (EO): 2022-Q1 APU production (in 2021-Q4 APU folders)
		-	production inputs
		-	cleaned up header wording to reflect more generic info
		-	added a preimport QA to monitor num duplicate asmt ids in amst extract

	04-AUG-2022 (EO): 2022-Q2 APU development (in 2022-Q1 APU folders)
		-	rearranged parameters for common macro inclusion at the top
		-	expanded use of common macros to assign parameters
		-	added nomprint to bottom of code
		-	streamlined a few macro assignments beyond common assignments
			the unintended downside was some renaming of titles and calls 
			throughout the code
		-	added notes to the log in parameter section to call out chunks that need
			updating vs autoupdates AND the frequency with which they need updates
	18-AUG-2022 (EO): production inputs

	11/04/2022 (CT): Development run for 2022-Q3 APU development (in 2022-Q2 APU folders)

	11/22/2022 (EO): production run 
	
	12/19/2022 -- Special development period -- KAS
	
		The purpose of this update is to modify the logic used to assemble Hospice assessments. 
		
		The previous approach was very similar to the
		approach used for IRF, LTCH, and SNF -- determine the assessment state as of the assessment "submission" cutoff for the quarter, then if necessary
		append to the previous quarter's assessment file.
		
		The prior approach has proven to be untenable as CMS has issued multiple extensions that extend past the point where we would extract data.
		There is also some question as to whether we should ever have been trying to capture the assessment's state as of the "submission" cutoff or whether
		we should have been taking the first instance of each assessment, given that compliance for hospice is not based upon completion but on
		timeliness.
		
		This update, therefore, changes the logic for assembling hospice assessments. It assumes that it will receive an extract including all assessments
		YTD and will deduplicate and clean this file to take the earliest version of each assessment.
		
	12/23/2022 -- Minor fixes from QA

	03/10/2023 -- EO
		- prodution for 2022-Q4 Hospice APU
		- added submission cutoff to the parameter section
		- added submission cutoff to the import sections for both main and hist
		- added submission cutoff to pre import QA sections
		- removed a few Q3 hospice run specific comments

	05/08/2023 -- EO
		- development for 2023-Q1 Hospice APU

	05/22/2023 -- EO
		- prod for 23Q1 Hospice APU


	08/16/2023 -- Raki (Changes were incomplete/inaccurate)
		- Development run for 2023-Q2 Hospice APU (2023-Q1 ILS)


   	08/21/2023 -- CT
		- Production run for 2023-Q2 Hospice APU (2023-Q1 ILS)

   	11/09/2023 -- SS
		- Development run for 2023-Q3 Hospice APU (2023-Q2 ILS)
		- Had to hard-code the input provider file due to provider data not currently available.
          We are using the previous APU provider xract file. This will need to be updated when
          provider data becomes available for this run.

	11/28/2023 -- CT
		- Production run for 2023-Q3 Hospice APU (2023-Q2 ILS)

	02/29/2024 -- SS
		- Development run for 2023-Q4 Hospice APU 
		- Updated in_asmt and in_asmt_hist filenames to include DBX.


	03/08/2024 -- HS
		- Prod run for 2023-Q4 Hospice APU 

	05/09/2024 -- HS
		- Dev run for 2023-Q1 Hospice APU 

************************************************************** END HEADER *****************************************/

%put NOTE: END header / START parameters ; 

/************************************PROGRAM PARAMETERS***************************************/

* common macro set up ; 
%put NOTE: update date each reporting cycle - common file for report related assignments ; 
	* for control table import and ccn pattern ; 
	%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";

%put NOTE: update date each reporting cycle - common file for assessment related functions ; 
	%include "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Extract and Assemble Assessments/Programs/Assemble_Assessments_Common_20240509.sas";


%put NOTE: folder updated automatically from APU common macros each reporting cycle ; 

* reporting cycle folder populated from APU common macros- 
			;
	%LET folder = &cmn_folder;
    %put NOTE: &=folder ;


%put NOTE: paths should not normally need updating ; 

* database/schema/caslib references - update as needed ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data ;
	%LET benchqrp = &bench/SAS/Quarterly and Annual Reporting ;
	%LET asmt_dir = Extract and Assemble Assessments/Data ;
	%LET prvdr_dir = Extract and Assemble Providers/Data ;

* Static names ;
	%LET asmt_setting = HOSPC;
	%LET setting = Hospice ;

%put NOTE: Hospice is a quarter ahead of the other PAC QRPs which should be reflected in the parameters and libraries below ; 

%put NOTE: Naming parameters - will auto update in this chunk ; 
	%LET year = &cmn_yy_hosp ;
	%put NOTE: &=year ; 
	%LET quarter = &cmn_qq_hosp ;
	%put NOTE: &=quarter ; 

%put NOTE: Naming parameters - update each reporting cycle;
	* prev_year is only needed if prev_quarter = Q4 ;
	%LET prev_year = 23 ;
	%LET prev_quarter = Q4 ;
	
	%LET prev_folder =  2023-Q4 Hospice;

* Establish librefs ;

	* Session options and LIBREFs - update as needed ;
	OPTIONS COMPRESS=YES 
		MPRINT ;
  											
	LIBNAME lib_old
		"&benchqrp/&prev_folder/&asmt_dir" ;
	LIBNAME lib_asmt 
		"&benchqrp/&folder/&asmt_dir" ;
	LIBNAME lib_prvd
		"&benchqrp/&folder/&prvdr_dir" ;

%put NOTE: parameters in this chunk need to be updated with each report cycle ; 

* Input File Parameters - Update each quarter/run ;

	* Previous quarter combined assessments 
	* This will be used for comparison purposes only ;
	%LET in_prev = HOSPC_ASMT_FULL_&prev_year.&prev_quarter._240308 ;

%put NOTE: parameters in this chunk need to be updated each programming cycle ; 

	* Current quarters assessment extracts ;
	%let xtrct_date = 240502 ; 
	%LET in_asmt = HOSPC_DBX_ASMT_XTRCT_&year.&quarter._&xtrct_date ;
	%LET in_asmt_hist = HOSPC_DBX_HIST_XTRCT_&year.&quarter._&xtrct_date ;

	* Use provider extract to incorporate CCNs (missing from ASMT tables) ;
	%LET in_prvdr = lib_prvd.HOSPC_prvdr_xtrct_&xtrct_date;
    *%LET in_prvdr = lib_prvd.HOSPC_prvdr_xtrct_240227;

%put NOTE: parameters below here should auto update - please read log for what values are triggering ; 


	%let submsn_cutoff_dt = &cmn_submit_cutoff ; 
	%put NOTE: &=submsn_cutoff_dt ; 

%put NOTE: Hospice is a quarter ahead of the other PAC QRPs which should be reflected in date assingments below ; 
* Input date parameters - update each quarter ;
	%LET q_start = &cmn_qtr_start_hosp ;
	%put NOTE: &=q_start ; 
	%LET q_end = &cmn_qtr_end_hosp ;
	%put NOTE: &=q_end ; 
	
%put NOTE: Set test_prior flag. This determined whether we want to run QA checks that compare this assessment file against the prior file. Defaults to set except for Q1;
%if &quarter NE Q1 %then %do;
	%let test_prior = 1;
%end;
%if &quarter EQ Q1 %then %do;
	%let test_prior = 0;
%end;
%put NOTE: &=test_prior;

* Control sheet: Source_File_Name - check that setting (sheet=) and source (sourcefile=) are up to date and correct ;
	%put NOTE: control file input is now assigned as default in the common macro import function; 
	%LET ctrl_sheet = HOSP_Assessment ;

	* Element Active range ; 
	%put NOTE: note to reviewers - set these to hospice current quarter scope  ; 
	%LET HOSPC_control_start = &cmn_qtr_start_hosp ;
	%put NOTE: &=HOSPC_control_start ; 
	%LET HOSPC_control_end = &cmn_qtr_end_hosp ;	
	%put NOTE: &=HOSPC_control_end ; 

* Combined Output Naming Parameters - dynamically generated naming parameters ;
	%LET report_date = %sysfunc(today(), yymmddn6.) ;


	* Name output ;
	%LET combinedout = HOSPC_asmt_full_&year.&quarter._&report_date ;

	%put NOTE: parameters summary ; 

	* Make sure output name is correct and that inputs are updated ;
	%put NOTE: Hospice is a quarter ahead of the other PAC QRPs which should be reflected in the parameters below ;

	%put NOTE: assessments will be assembled for &year.&quarter with current extract from &q_start to &q_end ;

	%PUT NOTE: Check current ASMT extract input &=in_asmt ;

	%PUT NOTE: Check current HIST extract input &=in_asmt_hist ;

	%PUT NOTE: Check current PRVDR extract input &=in_prvdr ;

	%PUT NOTE: Check current control table input &=cmn_control_table_full using  &=ctrl_sheet;

	%PUT NOTE: Check previous assembled ASMT file input &=in_prev --- should be from previous APU cycle ;

	%PUT NOTE: Check output name --- &=combinedout ;

%put NOTE: End parameters ;

 
/*************************************** process control table***********************************/
%put NOTE: process control table ;

*Load Control File for Assessments - this will be used to restrict data to only what we need for APU ;
*	This uses the control file extract macro from the common macros file	;
%set_control_file_dated(setting= HOSPC,
						/* ctrl_in is set to default within macro definition */
						ctrl_sheet= &ctrl_sheet,
						start_date= &HOSPC_control_start,
						end_date= &HOSPC_control_end,
						ctrl_out = work.control_Hospc_clean);

	

	* Create a macro list of columns we need for APU from control file ;
	* The list will be based on date as columns are subject to change ;
	* NOTE: HOSPC uses DATA step and a later PROC SQL so the varlist is space-separated instead of comma-separated ;
	PROC SQL NOPRINT ;
		SELECT Source_File_Name
     	INTO :varlist separated by " "
     	FROM  work.control_HOSPC_clean
     	WHERE	Element_Active_End >= "&HOSPC_control_start"d
				AND Element_Active_Start <= "&HOSPC_control_end"d ;
    	SELECT Source_File_Name
		INTO :varlist_sql separated by ","
     	FROM  work.control_HOSPC_clean
     	WHERE	Element_Active_End >= "&HOSPC_control_start"d
				AND Element_Active_Start <= "&HOSPC_control_end"d ;
	QUIT ;



 

/*************************************** QA Extracted Assessment(s)***********************************/
%put NOTE: QA raw extracts before import ;

	* Create macro to check that we have crctn_num + check correction fields (crctn_num and XXXX_crctn_stus_cd) ; 
%put NOTE: Create macro ;
	%MACRO check_crctns (in_db = ,
						table = ,
						qrtr = ,
						pac = ,
						submsn_cut = ) ;

		PROC FREQ	DATA = lib_asmt.&table ;
			where	("&q_start"d <= trgt_dt <= "&q_end"d)
					AND submsn_dt <= "&submsn_cut"d;
			TABLE	crctn_num
					&pac._crctn_stus_cd  / NOCUM missing;
			TABLE	crctn_num * &pac._crctn_stus_cd /NOCOL NOPERCENT NOCUM missing ;
			TITLE	"Check Input &qrtr values and their frequencies for crctn_num and 
				&pac._crctn_stus_cd --  &table" ;
		RUN ;
	%MEND check_crctns ;

%put NOTE: call macros to check correction fields in the inputs to be assembled ;
	%check_crctns (table = &in_asmt,
					qrtr = &year.&quarter,
					pac = &asmt_setting,
						submsn_cut = &submsn_cutoff_dt) ;

	%check_crctns (table = &in_asmt_hist,
					qrtr = &year.&quarter,
					pac = &asmt_setting,
						submsn_cut = &submsn_cutoff_dt) ;				

 

TITLE	"Monitoring: Number of Duplicate Assessments in Extract" ;
		PROC SQL ; 
			SELECT	N_Records,
					COUNT(*) AS orgnl_asmt_ids
			FROM
				(SELECT	orgnl_asmt_id,
					COUNT(*) AS N_Records
				FROM	lib_asmt.&in_asmt
				where	("&q_start"d <= trgt_dt <= "&q_end"d)
							AND
						submsn_dt <= "&submsn_cutoff_dt"d
				GROUP BY	orgnl_asmt_id)
			GROUP BY N_Records;
		QUIT ;
		TITLE ;

/*************************************** Assemble Extracted Assessments***********************************/
%put NOTE: import needed files and assemble ;

*	Sort assessment and history files by 
		orgnl_asmt_id
		descending submission date
		descending correction number
	
	This sort order will ensure that the original record by orgnl_asmt_id is LAST in the dataset. This will allow us to determine whether the
	assessment was inactivated and do some other debugging.
		;
		
proc sql;
	create table work.hosp_asmt_sort as
	select 	&varlist_sql
	from	lib_asmt.&in_asmt
	where	("&q_start"d <= trgt_dt <= "&q_end"d)
				AND
						submsn_dt <= "&submsn_cutoff_dt"d
	order by	orgnl_asmt_id,
				submsn_dt desc,
				crctn_num desc
				;
quit;				

proc sql;
	create table work.hosp_asmt_hist_sort as
	select 	&varlist_sql
	from	lib_asmt.&in_asmt_hist
	where	("&q_start"d <= trgt_dt <= "&q_end"d)
				AND
						submsn_dt <= "&submsn_cutoff_dt"d
	order by	orgnl_asmt_id,
				submsn_dt desc,
				crctn_num desc
				;
quit;

*	Combine	;
data	work.hosp_asmt_all_sort_debug;
	set		work.hosp_asmt_sort
			work.hosp_asmt_hist_sort
			;
	by	orgnl_asmt_id
		descending submsn_dt 
		descending crctn_num
		;
	*	Create indicator for an inactivated record	;
	retain Inactivated;
	if first.orgnl_asmt_id then Inactivated = 0;
	if hospc_crctn_stus_cd = "I" then Inactivated = 1;

	*	Create indicator for correction number out of order	;
	retain 	prior_crctn_num
			crctn_num_issue
			;
	if first.orgnl_asmt_id then do;
		prior_crctn_num = .;
		crctn_num_issue = 0;
	end;
	if 	(prior_crctn_num ~= . and prior_crctn_num < crctn_num)
		then crctn_num_issue = 1;
	else crctn_num_issue = 0;
	prior_crctn_num = crctn_num;
	
	if last.orgnl_asmt_id;
	
	drop	prior_crctn_num;

run;

title	"QA -- combine assessment and assessment history files";
title2	"Actual records determined to be inactivated";
proc freq	data = work.hosp_asmt_all_sort_debug;
	table	Inactivated / missing nocum;
run;

title2	"Records expected to be inactivated = distinct orgnl_asmt_id with an 'I'";
title3	"From history table";
proc sql;
	select	count(distinct orgnl_asmt_id) as distinct_inact_orgnl_asmt_id
	from	work.hosp_asmt_hist_sort
	where	hospc_crctn_stus_cd = "I"
	;
quit;

title2	"Correction numbers out of sequence";
title3	"Should be zero";
proc sql;
	select	sum(crctn_num_issue) as assessments_with_issues
	from	work.hosp_asmt_all_sort_debug
	;
quit;

*	Clean up	;
data	work.&setting._assembled_clean;
	set	work.hosp_asmt_all_sort_debug;
	*	subset to drop inactivated records	;
	where inactivated = 0;
	*	rename and shorten CCN field	;
	length ccn $6;
	ccn =	c_ccn_num;
	
	*	create quarter indicator	;
	length quarter $2.;
	quarter = catt("Q", put(trgt_dt, qtr.));

	*	convert discharge date field;
	format clean_dd date9.;
	if a0270_dschrg_dt ~= . then do;
		clean_dd = input(put(a0270_dschrg_dt, $8.), yymmdd8.);
	end ;
	else clean_dd = .;
	rename	clean_dd = a0270_dschrg_dt;
	
	*	drop debug fields and others	;
	drop	inactivated
			crctn_num_issue
			a0270_dschrg_dt
			c_ccn_num
			;
run;				


/*************************************** QA and Output Assessments ***********************************/
%put NOTE: QA final file ; 

* Double check columns and their formats ;
	TITLE "Double check character column formats: CCN and Assessment items" ;
	PROC SQL ;
		SELECT	name,
				type,
				length,
				format
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "char" ;
	QUIT ;
	TITLE ;

	TITLE "Double check numeric column formats: Dates, internal IDs, asmt IDs" ;
	PROC SQL ;
		SELECT	name,
				type,
				length,
				format
		FROM	dictionary.columns
			WHERE	libname = UPCASE("work") 
					AND memname = UPCASE("&setting._assembled_clean") 
					AND type = "num" ;
	QUIT ;
	TITLE ;

* Check that there are no missing IDs, dates, etc. ;
	TITLE "Check for missing IDs/Dates in final assembled file" ;
	TITLE2 "There should be no missing levels" ;
	PROC FREQ	DATA = work.&setting._assembled_clean 
				NLEVELS ;
			TABLE 	ccn
					prvdr_intrnl_num
					caads_dt
					trgt_dt
					submsn_dt
					crctn_num
					&asmt_setting._asmt_id
					orgnl_asmt_id/ MISSING NOPRINT ;
	RUN ; 
	TITLE ;

* Check distribution of records ;

	%if &test_prior = 0 %then %do;
		%put NOTE: Not checking against prior assessment file, possibly because this is Q1 of a new year.;
	%end;
	
	%if &test_prior = 1 %then %do;
	
		TITLE	"Check that distribution of records is reasonable" ;
		title2	"Current quarter";
		proc sql ;
			select	quarter,
					caads_dt,
					min(trgt_dt) format date9. as trgt_dt_start,
					max(trgt_dt) format date9. as trgt_dt_end,
					count(*) as num_records format = comma12.,
					calculated num_records/(select count(*) from work.&setting._assembled_clean) as percent format=percent10.2
			from work.&setting._assembled_clean
				group by	quarter,
							caads_dt ;
		quit ;
		
		title2	"Prior quarter's file";
		title3	"If comparing to a quarter in the prior year, numbers may change a bit.";
		title4	"If comparing to prior year, numbers may at best be in the same ballpark.";
		proc sql ;
			select	quarter,
					caads_dt,
					min(trgt_dt) format date9. as trgt_dt_start,
					max(trgt_dt) format date9. as trgt_dt_end,
					count(*) as num_records format = comma12.,
					calculated num_records/(select count(*) from work.&setting._assembled_clean) as percent format=percent10.2
			from lib_old.&in_prev
				group by	quarter,
							caads_dt ;
		quit ;
		
		TITLE ;
		
		/*	Characterize assessments from earlier quarters that are missing from prior extracts	
			Only runs if test_prior flag is set, which should be done automatically in the parameters based upon reporting quarter
				(we don't care to do this during Q1, when the prior assessment file will represent the past year...) */
		proc sql;
			create table work.delta as
			select	*
			from work.&setting._assembled_clean 
			/*	orgnl_asmt_id in current extract but missing from prior AND in a quarter covered by prior extract	*/
			where	orgnl_asmt_id not in
						(select orgnl_asmt_id from lib_old.&in_prev)
					and put(trgt_dt, yyq.) in
						(select put(trgt_dt, yyq.) from lib_old.&in_prev)
			;
		quit;
		
		title	"Characterize records from prior quarters that are present in current assessment file but missing from past files.";
		title2	"Ideally, these providers will have a submission date after the date that we extracted data for the prior file.";
		proc sql;
			select	quarter,
					min(submsn_dt) as min_submsn_dt format = date9.,
					max(submsn_dt) as max_submsn_dt format = date9.,
					count(*) as Records format = comma11.
			from	work.delta
			group by quarter
			;
		quit;
		title;
		
		/*	Check providers missing from current quarter but present in prior files.
			Hopefully these are easily explained e.g. by having been inactivated.
			Note that we harken back to the debug file here.
		*/
		proc sql;
			create table work.disappeared as
			select	base.*,
					db.inactivated
			from	lib_old.&in_prev base
						left join
					work.hosp_asmt_all_sort_debug db
						on base.orgnl_asmt_id = db.orgnl_asmt_id
			where base.orgnl_asmt_id not in
				(select orgnl_asmt_id from work.&setting._assembled_clean)
				;
		quit;
		
		title	"Characterize records present in past assessment files but now missing";
		title2	"Impact attributable to inactivation";
		footnote	"We have noted that a small number of assessments may just vanish from the extract, BTW.";
		proc sql;
			select	quarter,
					inactivated,
					count(*) as records_inactive
			from	work.disappeared
			group by	quarter,
						inactivated
						;
		quit;
		footnote;
		
		
		/*	Characterize any records that have different trgt_dts relative to prior assessment file	
			(Hopefully this is none!)	*/
		proc sql;
			create table work.changed as
			select	current.orgnl_asmt_id,
					current.ccn,
					current.trgt_dt as trgt_dt_current format = date9.,
					past.trgt_dt as trgt_dt_past format = date9.,
					current.crctn_num as crctn_num_current,
					past.crctn_num as crctn_num_past
			from	work.&setting._assembled_clean current
						inner join
					lib_old.&in_prev past
						on current.orgnl_asmt_id = past.orgnl_asmt_id
			where	current.trgt_dt ~= past.trgt_dt
					;
		quit;
		
		title	"Characterize records, if any, with changed trgt_dt";
		title2	"There may (should) be none.";
		proc sql;
			select	crctn_num_current,
					crctn_num_past,
					count(*) as Records
			from work.changed
			group by	crctn_num_current,
						crctn_num_past
			;
		quit;
		
		title;

	%end;

 

/*************************************** Write Output to Workbench ***********************************/
%put NOTE: Write final file to workbench and QA rows written ;

*** Yo Yo, QA slow your roll --- do not run the code below during QA ; 

* Write combined table to workbench, dropping any assessments without a CCN ;
	* There are no missing CCNs, but that may not be always be true so the restriction remains ;
	DATA lib_asmt.&combinedout ;
		SET	work.&setting._assembled_clean
		(WHERE = (ccn ~= ""));
	RUN ;		

	* Convince ourselves that final rowcount is OK ;
	TITLE	"Check that final count of records is reasonable" ; 
	PROC SQL ;
		SELECT	COUNT(*) AS num_asmts
		FROM work.&setting._assembled_clean ;
		SELECT	COUNT(*) AS num_no_ccn
		FROM work.&setting._assembled_clean
			WHERE	ccn = "" ;
		SELECT	COUNT(*) AS num_with_ccn
		FROM work.&setting._assembled_clean
			WHERE	ccn ~= "" ;
		SELECT	COUNT(*) AS num_asmts_output
		FROM lib_asmt.&combinedout ;
	QUIT ; 
	TITLE ;


options noMPRINT ; 

/********************************************** END OF CODE *****************************************************/

%put NOTE: End of Active code ; 
