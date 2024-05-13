/****************** HEADER ***********************
	PROGRAM:		SNF_prvdr_

	CREATED BY:		Elizabeth Oliver (EO)

	PURPOSE:	This program combines provider files used in the most recent 
				APU reporting cycle as part of a larger process to produce APU Analytics.

	INPUTS:		This program has 5 inputs:
				snf_prvdr_2xQx_(type)_21xxxx (previous quarter's assembled provider file)
					the previous quarter's assembled providers file can be found  on the workbench
				snf_prvdr_xtrct_XXXXXX        (current quarter's extract Stand alone snfs)
				snf_sb_prvdr_xtrct_XXXXXXX    (current quarter's extract - SWING BED SNFS)
					ALL provider XTRCT files can be on workbench
				Source_file_name_2XQX_21XXXX.xlxl Sheet = SNF_Provider (current control table)
					Sourced in workbench 
				cy202xposurbanrural - POS Dec 202x update - now stored on workbench as a sas data set
					Souced in workbench path:
					/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/Static Files

	METHOD:		This program contains the following subsections and methods.
				MACROS: 		assign libraries and define macro variables and parameters.
				IMPORTS: 		Import all data files needed and perform any cleaning necessary.
				COMBINING:		Combine all data sets, keep only fields needed to calculate historical OPD
				MANIPULATIONS:	Manipulate to calculate historical opd and calculate tiein date
				COMBINE:		Combine the historical OPD set with the current quarters extract
								the original participation date from the current quarter's extract will be the new current_OPD	
				EXTRADATA:		Combine with POS file to get urban rural indicator using the POS table as a hash object
				COMPARE:		Compare results to previous quarter's assembled provider file
				SAVING:			Write the final table to the workbench

	NOTES:		SNF is known to have missing ccns in both the provider and assessment files 
				For the provider files - we are monitoring how many rows are missing from each quarter
				and keeping an eye out for any dramatic increases
				If the number of missing providers in each new quarter does not point to a larger problem
				then we are dropping rows where the provider ccn is missing. 
	New for 2020Q3 Outreach, 
				The monitoring steps and dropping of rows is handled when each quarter is imported.
				Previously, this was handled after all sets were combined.
				
	NEW for 2020Q3:
				This program will read the 2019 direct files from their home database directly.
				Previously, the files were written from pac_qrp_swgt_qies to pac_qrp_swgt then imported into caslib
				This was a throwback to the beginning days on hive when Liz did not know any better.
				She has evolved with time and now her code has too. ;)

				also new, the pos file is joined using a hash object instead of a merge

	New for 20Q4 OR:
				The inputs for this program have been streamlined.
				We will use the assembled provider file from the previous quarter 
				and combine it with the current quarter's extract to find the historical_opd
				(the oldest orgnl_prctptn_dt on record). The current quarter's extract will be used
				to find the current_opd (the value of orgnl_prctpnt_dt from the extract).
				No changes were made to how cbsa_urbn_rrl_ind is found.

	New for 21Q1 OR:
				We will no longer reduce the assembled provider file based on termination date.
				This will now be handled in downstream programming.

	TO UPDATE:	To update this program the macro variables at the top will need to be updated extensively.
				
	RECORD OF current quarter's UPDATES:
		07/30/2021	-	updated for use in 21Q1 apu reporting cycle
		08/09/2021	-	input pos file changed to pre-cleaned version by ccn, removed cleaning, and some QA steps associated
		08/19/2021	-	production date inputs updated
	
		10/04/2021 - Updates for 2021 Q2 OR (Chid)
						- (lines 82-129) updated program parameters updated to reflect 2021 Q2 OR,
						latest - directories, assembled provider file, provider extract,
								POS file and control table.

		10/18/2021 - Production run for 2021 Q2 OR (Chid)
				- (lines 694-711) QA -- Month of tie in by month of OPD
					---> added OPD_plus30_month column to make reading the table more straight forward
						(Pam's suggestion from development)

		11/16/2021	-	development for 2021 Q2 APU (EO)
					-	updated inputs and parameters
					-	added signposting for sections to log
					-	added data step filtering for provider specific opd assingment
					-	integrated QA changes made in IRF into SNF, where appropriate
					-	added transpose option to proc compare at end

		11/19/2021  -   production run for 2021 Q2 APU (Chid)

		12/28/2021	-	pre-development for 2021 Q3 OR (EO)
					-	inputs and parameters

		01/06/2022  - development run for 2021 Q3 OR (Chid)

		01/19/2022  - production run for 2021 Q3 OR (KAB)

		02/04/2022  -   development for 2021 Q3 APU (KAB)

		02/08/2022  -   udated POS file (KAB)

		02/18/2022 - production for 2021 Q3 APU (KAB)
				   - updated extract parameters for production
				   - included update notes from 12/21 and 01/22
				   
		3/8/2022	- Pre-development for 2021-Q4 OR -- KAS
					- Updated inputs and parameters
					
		4/5/2022	- Development for 2021-Q4 OR
					- Updated parameters
					- Streamlined parameter update

		4/18/2022	- production for 2021-Q4 OR
					- inputs for prod

		5/4/2022	- Development for 2021-Q4 APU (EO)
					- development inputs
					- cleared extraneous commenting
					- added statements to fix ccn length to minimize downstream messaging
					- cleaned up some QA
					- added SA and SB control sheets
					- added lots of notes to the log

		5/23/2022	- production
					- prod inputs

		07/06/2022 (EO) - 22Q1 OR development
					- dev inputs and other parameters
		07/19/2022 EO - prod
					- production inputs

		08/03/2022 (Chid) - Developement Run for 2022 Q1 APU
					  - additional use of common macros program in the parameter section
		08/18/2022 (EO) production inputs

		10/06/2022 (EO)	-	Development 2022-Q2 Outreach
					-	expected updates for reporting cycle change
					-	added an upcase function to all demographic fields that end up used down the line
						(facility name, street address, and city)
						it was noted after the 21Q4 report that some new providers were in the source database
						with lower case or prop case names and cities, etc.
						to standardize, we will upcase all fields where this is needed
					-	the control table was still being locally assigned, I amended it to use the default in the common macros
					-	ccn pattern was being directly references within code from common
						I assigned it to a local and references that instead
		10/18/2022 (EO) - 	post QA add
					-	added code for creation of 2/3 needed flags for the multi org issue
					-	used a common program to do so
					-	added flag to include this new code in run or not
		10/20/2022 (EO) - 	production inputs for 22Q2 Outreach
					-	added flag data date macro so that it is not hardcoded at end of program

		11/03/2022 (EO) -	development for 2022-Q2 APU
		
		11/21/2022 (KAS)	- Production for 2022-Q2 APU. Also, added code to update the SSA CD using the CSP_PRVDR_CMN file.
		
		1/5/2023 (KAS) - Dev for 2022-Q3 OR.

		01/18/2023 (EO) - prod 22Q3 OR

		02/07/2023 (CT) - Development run for 2022-Q3 APU (updated POS File)

		02/21/2023 (EO) - prod run 22Q3 APU

		03/23/2023 (SS) - dev run 22Q4 OR
	
		04/18/2023 (EO) - prod run 22Q4 OR

		05/05/2023 (SS) - dev run 22Q4 APU

		05/08/2023 (SS) - Updated SB provider file processing to keep ssa variables from source data. 
                          Augmented cbsa hash table based on sb vs sa status.

		05/18/2023 (EO) - 22Q4 APU prod run

		07/14/2023 (EO) - 23Q1 Outreach development
						- unlike other settings, SNF stand alone active provider
							files do not have an OPD
						- OPD does exist in SA active provider file
							however, to ease confusion
							SNF will hold on grabbing historical OPD from 
							active provider files until all SNFs can be captured with same 
							method
		7/19/2023 (EO) 	- prod run 23Q1 OR

       08/14/2023 (SS)  - dev run 23Q1 APU

	   08/18/2023 (CT)  - Production run for 2023-Q1 APU

	   08/22/2023 (SS)  - Production rerun for 2023-Q1 APU.
	                    - Overhauled code to use Active Provide Files instead of Historical.

	   08/23/2023 (SS)  - Production rerun for 2023-Q1 APU.
                        - Removed unwanted code and reassigned a input dataset name.

	   10/12/2023 (SS)  - Development Run for 2023-Q2 OR.
                        - Removed all code related to old method of finding historical OPD.
						- Added length and format statements for mcare_id and prvdr_num.

	   10/18/2023 (CT)  - Production run for 2023-Q2 OR
						- 

	   10/19/2023 (EO) - prod 23Q2 OR post prod changes, no rerun
						- some code related to historical opd old method was still there
						- it was just commented out
						- removed, no rerun needed

	   10/19/2023 (CT) -   Production Run for 2023-Q2 OR (no rerun needed)
					   -   Deleted notes/titles implying we are waiting on the decision 
					       for handling active provider files missing OPD

	   11/8/2023 (CT) -   Development Run for 2023-Q2 APU
					      (provider extracts copied from Q2 OR folder due to CDR refresh delay)

	   11/24/2023 (CT) -   Production Run for 2023-Q2 APU
					   -   used the August 2023 Quarterly Active Provider Files for SA
						   & September 2023 Quarterly Active Provider Files for SB
	   
	   12/04/2023 (HS) -   Production Run for 2023-Q2 APU
					   -   used the November 2023 Quarterly Active Provider Files for SA
						   & September 2023 Quarterly Active Provider Files for SB

	   01/04/2024 (HS) -   Development Run for 2023-Q3 OR

	   01/11/2024 (SS) -   Development rerun for 2023-Q3 OR using the 10-18 stand alone provider data extract.

	   01/12/2024 (SS) -   Development rerun for 2023-Q3 OR using the new Active Provider file for SB from 2023_12.

	   01/17/2024 (HS) -   Production Run for 2023-Q3 OR

	   01/18/2024 (HS) -   Production rerun for 2023-Q3 OR
					   -   Modified active_prvdr_curr_clean to include facility_name
					   -   Further filtered QA(missing from APF not termed and providers only missing historical opd) using ccn_snfany pattern 
					   -   On pac_curr_active_combined_clean added a hash to assign fac_name to those CCN in APF not in CDR
					   -   On pac_curr_active_combined_clean added if statement to assign Swing_Bed to those CCN in APF not in CDR

	   01/19/2024 (HS) -   Production rerun for 2023-Q3 OR
					   -   Modified active_prvdr_curr_clean to include state_cd,city_name,zip_cd,st_adr
					   -   On pac_curr_active_combined_clean added zip code, state, city and address to hash for those CCN in APF not in CDR

	   02/13/2024 (HS) -   Development Run for 2023-Q3 APU

	   02/14/2024 (HS) -   Development Re Run for 2023-Q3 APU
					   -   Modified stand alone active provider file. Now file is in xlsx format so needs to be import with PROC IMPORT.

	   03/14/2024 (HS) -   Production run for 2023-Q3 APU

	   04/12/2024 (HS) -   Development run for 2023-Q4 OR

	   04/16/2024 (HS) -   Production run for 2023-Q4 OR

	   04/17/2024 (HS) -   Production re-run for 2023-Q4 OR
					   -   Modified swing bed active provider file

	   04/17/2024 (HS) -   Production re-run for 2023-Q4 OR
					   -   added format date9 to certification_date on data cleaning step 
						   after importing both APF

	   05/06/2024 (HS) -   Development run for 2023-Q4 APU

	   05/07/2024 (HS) -   Development re-run for 2023-Q4 APU
					   -   New csv standalone APF, commented out excel import code from previous APF
******************* END HEADER **************************/

%put NOTE: MACROS AND PARAMETERS ;
* common macro set up ; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;
	* setting ; 
	%let pac 	 = SNF ; 

	* reporting cycle type should be set to OR for outreach or APU --- for output naming only ; 
	%let type	= &cmn_cycle ;
	%put NOTE: &=type;
	
	* report year and quarter; 
	%let rpt_year = &cmn_yyyy ;
	%put NOTE: &=rpt_year;
	%let qtr = &cmn_qq;
	%put NOTE: &=qtr;
	
	
	%let yy = &cmn_yy;
	%put NOTE: &=yy;
	%let qrtyr	 = &cmn_yy.&cmn_qq ;
	%put NOTE: &=qrtyr; 

	* libnames etc ; 
	* general path to workbench --- should not need updating ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ; 
	* current quarter specific folder --- update every reporting cycle; 
	%let q_folder = &cmn_folder ;
	%put NOTE: &=q_folder; 
	*current quarter's workbench path for data (input and output) --- should not need updating; 
	%let wkbn_path = &bench/&q_folder/Extract and Assemble Providers/Data ; 

	* path to active provider files - One for SA and one for SB ; 
	%let active_monthsa = 2024_05 ; 
	%let active_dirsa = &bench/Active Provider Files/Quarterly Files/&active_monthsa ; 
	%let active_filesa = NH-Based SNF APF for SwingTech_20240502.csv ; 
	%let active_pathsa = &active_dirsa/&active_filesa ;
	* stand alone active provider sheet name ;
	*%let active_sheetsa = NH based SNF APF for SwingTech_ ;

	%let active_monthsb = 2024_03 ; 
	%let active_dirsb = &bench/Active Provider Files/Quarterly Files/&active_monthsb ; 
	%let active_filesb = 112683264_Active_SB_032024.txt ; 
	%let active_pathsb = &active_dirsb/&active_filesb ; 
	
* previous quarter's folder --- update every reporting cycle; 
	%put NOTE: for Provider data, we use the immediate previous reporting cycle either OR or APU ; 
	************* Need to update this parameter manually ***********;
	%let p_folder = 2023-Q4 OR ;  
	*previous quarter's workbench path --- should not need updating;
	%let prev_path = &bench/&p_folder/Extract and Assemble Providers/Data ;
	* workbench path for new pos file --- should not need updating; 
	%LET POS_dir = &bench/Static Files ;

	* current quarter's library ; 
	libname wkbn
			"&wkbn_path" ; 
	* previous quarter's library ;
	libname lib_prev
			"&prev_path" ; 
	* library to pos file ; 
	libname posbn
			"&POS_dir" ; 

	* input file information ; 

	***** IMPORTANT: Update previous quarter's assembled file and this quarter's extracts manually	*****;
	* previous quarter's assembled file name ;
	%let in_prev =  snf_prvdr_23q4_or_240417 ;

 	* current quarter's extract file names ; 
 	%let in_snf_date 	= 240502; /* Usually, all input files will have the same extract date -- you should just need to update this macro variable	*/
	%let in_curr_SA   	= SNF_PRVDR_XTRCT_&in_snf_date ; * stand alone file ;
	%let in_curr_SB		= SNF_SB_PRVDR_XTRCT_&in_snf_date ; * swing bed file ;
	%let in_curr_CSP	= snf_csp_prvdr_xtrct_&in_snf_date; * CSP_PRVDR_CMN file -- to be used to backfill SSA CD if it is missing from the main file	;

	* control table ; 
	
	* control table paths are automated in the common macro program ; 
	* here we just need the relevant sheet names ;
	* stand alone snf sheet name - should not change ; 
	%let SA_sheet = SNF_Provider ; 
	* swing bed snf sheet name - should not change ; 
	%let SB_sheet = SNF_SB_Provider ;

	* references for POS file -- update when a new POS file becomes available (generally after the New Year each calendar year);
	%LET in_POS = pos_2022_ur_byccn_20230125 ;	
	
	%put NOTE: inputs summary ; 
	%put NOTE: provider data will be pulled from &in_curr_SA for stand alone providers ; 
	%put NOTE: and &in_curr_SB for swing bed prividers ; 
	%put NOTE: current data will be combined with previous cycles data in &in_prev ; 
	%put NOTE: finally, the POS file used will be &in_POS ; 
	%put NOTE: Historical OPD/Tie in will derive from the active provider Standalone file &active_filesa received in &active_monthsa ;
	%put NOTE: Historical OPD/Tie in will derive from the active provider Swing Bed file &active_filesb received in &active_monthsb ;

	* output tools ; 

	* report date assignment ; 
	%let report_date = %sysfunc(today() , yymmddn6.) ; 
	
	*final output naming ; 
	%let outfull = &pac._prvdr_&qrtyr._&type._&report_date ; 
	%put NOTE: the output report shall be calleth &outfull ; 

	* fixed report values ; 

	* you should have a real good, lead approved reason for changing this date ;
	* date desired for default tie in if orgnl_prtcptn_dt is missing ;
	%let tieindefault = "01JAN2018"d ; 
	%put NOTE: tiein default should always be 01JAN2018. Is it - &=tieindefault ?; 

	* dynamically create default date to flag termed providers ;  
	%let term_year = &cmn_term_year ;
	%put NOTE: &term_year ;
 
	%let in_scope_criteria = 
			%str((missing(trmntn_exprtn_dt)
				OR
			trmntn_exprtn_dt >= "01JAN&term_year."d)) ; 


	%put NOTE: in scope criteria is &in_scope_criteria ;
	%let ccn_snfany = &ccn_pattern_snfany ; 
	%let ccn_snf = &ccn_pattern_snf ; 
	%let ccn_sb = &ccn_pattern_snfsb ; 
	%put NOTE: ccn pattern to assign locally using common definition ; 
	%put NOTE: these resolve to &=ccn_snfany &=ccn_snf and &=ccn_sb ;


/******************* END parameters ***********/

%put NOTE: END PROGRAM PARAMETERS ; 

%put NOTE: IMPORT DATA ; 

	* import and clean control file ; 
	* the control file is used to subset desired columns ; 
	
%macro process_control_file(	setting 	= &pac 		, 
								subtype 	= 			,
								raw_sheet 	= 			,
								keep_fields = 
						) ; 


	/* from common macro program */
	%set_control_file(
							setting		= &setting,
							ctrl_sheet	= &raw_sheet ,
							ctrl_out 	= control_&subtype._raw);
	 
	
	/* FOR THE OTHER PAC SETTINGS IT DOESN'T ACTUALLY MATTER MUCH IF THE IMPORT PROCESS ADDS A ROW 
	* FOR SNF THE CONTROL TABLE WILL BE USED IN AN ARRAY TO RENAME THE VARIABLES  
	* IF THERE IS AN EXTRA LINE THE ARRAY MACRO WILL NOT RUN */
	title "table control_&subtype._raw - check for an an empty row" ; 
    title2 "num_empty_rows should be 0" ;
	title3 "it will not be" ;
	title4 "we clean this below" ; 
	proc sql ;
		select nmiss(source_file_name) as num_empty_rows 
		from work.control_&subtype._raw 
		;
	quit ;
	title ; 
	/* now clean control file */
	data work.control_&subtype._clean ; 
         set work.control_&subtype._raw (keep=&keep_fields) ;
         if source_file_name = "" then delete;
    run;  
	title "table control clean for &subtype - check again for empty row" ; 
    title2 "num_empty_rows MUST now be 0 " ;
	proc sql ;
		select nmiss(source_file_name) as num_empty_rows
		from work.control_&subtype._clean 
		;
	quit ;
	title ;
%mEnd process_control_file ; 
	
%put NOTE: call import control file for SB and SA ; 

%process_control_file(			setting 	= &pac 		, 
								subtype 	= SA		,
								raw_sheet 	= &SA_sheet	,
								keep_fields = %str(source_file_name standardized_name)
						) ; 
%process_control_file(			setting 	= &pac 		, 
								subtype 	= SB		,
								raw_sheet 	= &SB_sheet	,
								keep_fields = %str(source_file_name)
						) ; 

%put NOTE: verify control table for SA matches that for SB on key fields ; 
title "The control table for SNF SA should be sufficient for all our needs" ; 
title2 "however, we need to make sure the key variables listed are identical to those on the SB list" ; 
proc sql ; 
	select count(*) as control_table_probs
	into :count_cntrl_probs
	from(
	select SA_file.standardized_name as SA_in_name,
			SA_file.source_file_name as SA_out_name,
			SB_file.source_file_name as SB_name
	from control_SA_clean as SA_file
			full join
		control_SB_clean as SB_file
		on SA_file.standardized_name = SB_file.source_file_name
	where SA_file.standardized_name NE SB_file.source_file_name);
quit ; 

%if &count_cntrl_probs > 0 %then %do ; 
	title "find the problems - good peeps" ;
	title2 "if the only problems are ssa related variables - ignore this and use the SA file for all further processing" ;  
	proc sql ; 
	select SA_file.standardized_name as SA_in_name,
			SA_file.source_file_name as SA_out_name,
			SB_file.source_file_name as SB_name
	from control_SA_clean as SA_file
			full join
		control_SB_clean as SB_file
		on SA_file.standardized_name = SB_file.source_file_name
	where SA_file.standardized_name NE SB_file.source_file_name;
	quit ; 
%end ; 
	

	* the control file contains the list of variable from the input we wish to keep ;
	* use proc sql to create a space delimited list into a macro variable ; 
	* this will be used in the import steps to subset to the desired variables ; 

	* for SNF - ;
	* we need a macro with the names of variables as they exist in the extract and previous quarter's files ; 
	* and a macro for the names of variables as we wish to rename them ; 
	* we rename the variables to match the other pac settings ; 
	* source_file_name is the variable existing names ; 
	* standardized_name is the new names ; 
	* the swing bed file already has the standardized_names. Create a seperate varlist for swing bed ; 
	
	* the macro varlist will be used during the import process for all stand alone files ; 
	* the macro varchange will be used to import all swing bed files ; 
	
	* I need the macros in space delimited lists ; 
	* the macro varlist and varchange will be used to rename the variables ;
	* the macro varlist_sb will be created fro swing bed ;
	title	"QA  -- Control file";
	title2 "all variables in control file SNF providers should be listed" ;
	title3 "BOTH source_file_name and standardized_name should be included" ; 
	title4 "from file work.control_SA_clean" ;  
	proc sql ;
	     select Source_File_Name,
				Standardized_Name
	     into :varlist separated by " " ,
			  :varchange separated by " "
	     from work.control_SA_clean
	     ;
	quit;

	* QA check that the macro varlist contains the same values as the source_file_name column in the control file;
	%PUT NOTE: are these separated by a space... &varlist;
	%PUT NOTE: are these separated by a space... &varchange;

	title	"QA  -- Control file";
	title2 "all variables in control file SNF SB providers should be listed" ;
	title3 "source_file_name should be included" ; 
	title4 "from file work.control_SB_clean" ;  
	proc sql ;
	     select Source_File_Name
	     into :varlist_sb separated by " " 
	     from work.control_SB_clean
	     ;
	quit;

	* QA check that the macro varlist_sb contains the same values as the source_file_name column in the control file;
	%PUT NOTE: are these separated by a space... &varlist_sb;


%put NOTE: import extract files ; 
%put NOTE: previous quarter file ; 

	data work.&pac._prev_qrt (compress = no) ; 
		set lib_prev.&in_prev (keep = 	ccn 
										historical_opd 
										caads_dt) ; 
		rename historical_opd = orgnl_prtcptn_dt ; 
	run ; 

		* for 2020Q4 OR and beyond; 
		* there is a provider missing partci_dt in the extract ;
		* we were given approval to manually add their opd from the star list ; 
		* however we should check to see if it is still missing ; 
		title "checking table wkbn.&in_curr_SA for value on one provider" ; 
		title2 "if missing partci date, we have been directed to assign their date from other sources"  ; 
		proc sql ; 
			select mcare_id,
					partci_dt
			from wkbn.&in_curr_SA
			where mcare_id = "555920" ; 
		quit ;
		title ;  

%put NOTE: current quarter file ;
%put NOTE: stand alone file ;  
	data work.&pac._curr_qrt_SA_raw (compress = yes) ; 
		length mcare_id $6.;
		format mcare_id $6.;
		set wkbn.&in_curr_SA ; 
		keep &varlist ; 
	
		* new for Q4 OR and beyond; 
		* see prvdr extract file from Q4 OR for notes ; 
		if mcare_id = "555920" 
			AND	
			missing(partci_dt)
			then partci_dt = "12NOV2019"d ; 

	run ; 

	
	title "Checking table &pac._curr_qrt_SA_raw"  ; 
	title2 "provider 555920 should have partci_dt = 12NOV2019 OR match the non-missing date in previous check " ; 
	proc print data = &pac._curr_qrt_SA_raw ; 
		var 	mcare_id 
				partci_dt ; 
		where mcare_id = "555920" ; 
	run ; 
	title ; 

%put NOTE: swing bed file ;
	data work.&pac._curr_qrt_SB_raw (compress = no) ; 
		length prvdr_num $6.;
		format prvdr_num $6.;
		set wkbn.&in_curr_SB ; 
		keep &varlist_sb ; 
	run ; 

%put NOTE: QA extracts and drop rows with missing ccn;
 
 

		* check for missing values in ccn field ; 
		* reduce data set to those without missing ccns ;

%macro check_extract (file_root = ,
						ccn_name = 
					) ; 

			
		/* monitor missing ccns ; 
		* first want total record count ;*/ 
		title "file &file_root._raw - Total obs is the full record count" ; 
		proc sql ;
			select count(*) as total_obs
			from work.&file_root._raw ; 
		quit ; 
		title ; 

		title "file &file_root._raw - num_miss_ccn is the count of rows missing ccn values" ;
		title2 "monitor this number for increases between quarters" ;
		title3 "around 330-340 expected for SA, 0 for SB" ; 
		proc sql ; 
			select count(*) as num_miss_ccn
			from work.&file_root._raw
			where missing(&ccn_name) ; 
		quit ; 
		title ; 

		/* drop rows with missing ccn ; */
		data &file_root._reduced (compress = yes) ; 
			set work.&file_root._raw ; 
			if &ccn_name = "" then delete ; 
		run ; 

%mEnd check_extract ; 


	* call for SA file ; 
	***** monitor qty missing ccn quarter over qrt ; 

	%check_extract (file_root = &pac._curr_qrt_SA,
						ccn_name = mcare_id
					) ;
	* call for SB file ;
	* there should be no missing ccns ;  
	%check_extract (file_root = &pac._curr_qrt_SB,
						ccn_name = prvdr_num
					) ; 

	
%put NOTE: fix attribute differences between swing bed and stand alone file  ;

%put NOTE: the swing bed files have prvdr_intrnl_num as a numeric type whereas the standalones have facid as a char ; 
%put NOTE: guidance for 20Q4 apu reporting cycle and beyong confirmed 
					prvdr_intrnl_num should be used for SB  
				and facid should be used for SA; 
		%macro SB_num_to_char(	file_root = 
									) ; 
				data work.&file_root._clean (compress = no) ;
					set work.&file_root._reduced ; 
					
					prvdr_intrnl_num_char = strip(put(prvdr_intrnl_num, 25.)) ;
					drop prvdr_intrnl_num ; 
					rename prvdr_intrnl_num_char = prvdr_intrnl_num ;
		
				run ; 
	
				title "file &file_root._clean - confirm prvdr_intrnl_num is now a char" ; 
				title2 "if it is not on this list the conversion did not work" ; 
				proc sql ;
					select name,
							type
					from dictionary.columns
					where libname = "WORK"
							and memname = upcase("&file_root._clean")
							and type = "char"
					;
				quit ; 
				title ; 

		%mEnd SB_num_to_char ;    

		%SB_num_to_char(file_root = &pac._curr_qrt_SB) 
						;  
%put NOTE: import active provider file - Standalone SNF ; 
	data work.active_prvdr_curr_raw_sa  ;
	
		infile "&active_pathsa"
		delimiter=',' 
		missover 
		dsd 
		firstobs=2 ;
	
		informat state $4.  ;	
		informat facility_name $100. ; 
		informat CCN $6. ;
		informat address_line_1 $100. ; 
		informat city $56. ; 
		informat zip_code $5. ;
		informat prvdr_intrl_num $7. ; 
		informat fac_intrl_id $7. ; 
		informat fac_id $7. ; 
    	informat certification_date yymmdd8. ;
 
		format state $4.  ;
		format facility_name $100. ;
		format CCN $6. ;
		format address_line_1 $100. ; 
		format city $56. ; 
		format zip_code $5. ;
		format prvdr_intrl_num $7. ; 
		format fac_intrl_id $7. ; 
		format fac_id $7. ; 
    	format certification_date date9. ; 

		input
    		state $
    		facility_name $
			ccn $
    		address_line_1 $
    		city $
    		zip_code $
			prvdr_intrl_num $
			fac_intrl_id $
			fac_id $
    		certification_date
			;

	run;

/*	OPTIONS VALIDVARNAME = V7;
	proc import datafile="&active_pathsa"
				out = work.active_prvdr_curr_raw_sa  (rename=(STATE_CD=state 
															 FAC_NAME=facility_name
															 PRVDR_NUM=ccn_num
															 ST_ADR=address_line_1
															 CITY_NAME=city
															 ZIP_CD=zip_code_num
															 PRVDR_INTRNL_NUM=prvdr_intrl_num
															 FAC_INTRNL_ID=fac_intrl_id
															 FAC_ID=fac_id
															 VAR10=certification_date
													))
				dbms = xlsx 
				replace;
		getnames = yes;
		sheet = "&active_sheetsa";	
	run;
*/
	data active_prvdr_curr_clean_sa ; 
		
		set active_prvdr_curr_raw_sa(rename=ccn=ccn_sub);
		length ccn $6.;
		/*zip_code = STRIP(PUT (zip_code_num, 8.));
		ccn_sub = STRIP(PUT (ccn_num, 8.));*/

		if length(ccn_sub)=5 then ccn='0'||trim(left(ccn_sub)) ;
		else  if length(ccn_sub)=6 then ccn=trim(left(ccn_sub));
		
		keep 	ccn
				facility_name
				certification_date

				/****** added 01/19/24 ******/
				city
				state
				zip_code
				address_line_1
				;
		format certification_date date9.;
		rename certification_date = active_OPD ; 

	run ; 

%put NOTE: import active provider file - Swing Bed SNF; 
	data work.active_prvdr_curr_raw_sb  ;
	
		infile "&active_pathsb"
		delimiter='09'x 
		missover 
		dsd 
		firstobs=2 ;
	
		informat state $4.  ; 
		informat facility_name $100. ; 
		informat CCN $6. ;
		informat address_line_1 $100. ; 
		informat city $56. ; 
		informat zip_code $5. ; 
		informat prvdr_id $8. ;
		informat facility_id $7. ;
    	informat certification_date mmddyy10. ;

		format state $4.  ; 
		format facility_name $100. ; 
		format CCN $6. ;
		format address_line_1 $100. ; 
		format city $56. ; 
		format zip_code $5. ; 
		format prvdr_id $8. ;
		format facility_id $7. ;
    	format certification_date mmddyy10. ;

		input
		   	state $
    		facility_name $
			ccn $
    		address_line_1 $
    		city $
    		zip_code $
    		prvdr_id	$
    		facility_id $
    		certification_date
			;

	run;

	data active_prvdr_curr_clean_sb ; 
		
		set active_prvdr_curr_raw_sb ; 
		
		keep 	ccn
				facility_name
				certification_date

				/****** added 01/19/24 ******/
				city
				state
				zip_code
				address_line_1
				;
		format certification_date date9.;	
		rename certification_date = active_OPD ; 

	run ; 

%put NOTE: import additional files that will be needed later ; 
%put NOTE: import pos file ;    

	data work.pos_clean (compress = no); 
		set posbn.&in_POS ; 
	run ;    
 

	 
	
	title "file pos_clean - ssa variables should be char" ;   
	proc sql ; 
			select name,
					type
			from dictionary.columns
			where libname = "WORK" 
				and	
					memname = upcase("pos_clean") 
				and 
					type = "char"
			;
	quit ;   
	title ;  

	
	
%put NOTE: attribute fix ; 

%put NOTE: the current quarter extract for stand alone SNFs need to have the variables renamed ; 
	
	* I noted this above with the import and cleaning of the control table ; 
	* I set the lenght of this array with the number of control table elements ; 
	* that way this code does not need to change if we add variables to the control table for provider files ; 
	* however if a blank row slips in then this step will fail ; 
	* if this step fails - make certain the cleaning of the control table worked ;  
	  
	%macro ChangeVar(  
                       input = , /* Reference input table here */
                       output = , /* Reference output table here */);
									
			/*	Open the input table	*/
			%let dsid = %sysfunc(open(work.control_SA_clean));
	
			/*	Get number of records -- will be needed to loop through individual observations.	*/
			%let nvar = %sysfunc(attrn(&dsid, nobs));    

    		/* below - macro varlist is current variable names, space delimited;
    		* below - macro varchange is standardized new variable names, space delimited;*/
    		data work.&output (compress = yes);
        		set work.&input;

        		%do k=1 %to &nvar;
            		rename %scan(&varlist,&k) = %scan(&varchange,&k);
        		%end;
    		run; 

			/*	Close dataset. This step is VERY important. If it fails to execute, you can end up locking your dataset in an open state, which means you can't modify it.	*/
			%let	dsid = %sysfunc(close(&dsid));

			title "file &output" ; 
			title2 "new variable names should be listed" ; 
			proc sql ; 
				select name
				from dictionary.columns
				where libname = "WORK"
						and 
					memname = upcase("&output")
				;
			quit ; 
			title ; 

	%mEnd ChangeVar ;   

	
	* run for current extract stand alone only; 
	%ChangeVar( input = &pac._curr_qrt_SA_reduced,
				output = &pac._SA_named
				); 
	 
%put NOTE: combining data sets --- current extracts for SA and SB ;   

	* creating a current combined set ; 
	* this combines the swing bed and stand alone extracts for the current quarter ; 
	* we need to create an indicator variable to denote swing bed (1) vs stand alone (0) ; 
	* this set should contain ALL variables except cbsa ; 
	  
	%put NOTE: correcting length of ccn - will throw a warning ;
	data work.&pac._current_SBSA (compress = yes) ; 
		length city_name $56 
			   prvdr_num $6.; 
		set work.&pac._curr_qrt_SB_clean (in=SB)
            work.&pac._SA_named (in=SA)
			;
		*strip prvdr_intrnl_num for current set ; 
		prvdr_intrnl_num = strip(prvdr_intrnl_num) ; 
		* rename prvdr_num to ccn ; 
		rename 	prvdr_num = ccn 
				; 
		
		* create indicator for swing bed (true = 1 = swing bed) (false = 0 = stand alone) ; 
		if SB=1 then Swing_Bed = 1 ; 
		else Swing_Bed = 0 ; 
	run ;  
  
	title "total obs should be reasonable for setting's raw extracts combined" ;  
	title2 "Checking table &pac._current_SBSA" ;   
	proc sql ;  
		select count(*) as num_obs
		from work.&pac._current_SBSA 
		;  
	quit ;    
	title ; 
	
	* check length of variables ;
	title "checking var lengths" ;  
	title2 "Checking table &pac._current_SBSA" ; 
	proc sql ; 
		select name,
				length
		from dictionary.columns
		where libname = "WORK"
			and memname = upcase("&pac._current_SBSA")
		;
	quit ;  
	title ; 

	*check that the indicator variable created correctly ;  
	title "swing bed file num obs" ; 
	title2 "Checking table &pac._curr_qrt_SB_clean" ; 
	proc sql ; 
		select count(*) as num_obs 
		from work.&pac._curr_qrt_SB_clean ; 		
	quit; 
	title "combined file summary numbers" ;
	title2 "freq of swing_bed = 1 should equal num_obs in proc sql above" ; 
	title3 "Checking table &pac._current_SBSA" ; 
	proc freq data = work.&pac._current_SBSA ; 
          table Swing_Bed / missing nocum; 
	run; 
	title ; 

	* qa for missing orgnl_prctptn_dt ;
	title "are there too many missing values for orgnl_prtcptn_dt" ;
	title2 "too many is subjective - see Analytics Lead if you are concerned over results here" ; 
	title3 "historically this number for uncleaned file is about 40" ; 
	title4 "checking table &pac._current_SBSA" ;  
	proc means data = work.&pac._current_SBSA n nmiss ; 
		var orgnl_prtcptn_dt ;
	run ;  
	title ; 




%put NOTE: Combine the Standa Alone and Swing Bed Active Provider files to create a single active file ;

data active_prvdr_curr_clean;
	set active_prvdr_curr_clean_sb
		active_prvdr_curr_clean_sa;
		
run;


title "QA the active provider file for missing OPD" ;
proc sql ; 
	select n(ccn) as num_snf_total,
		n(active_OPD) as num_with_OPD,
		nmiss(active_OPD) as num_miss_OPD
	from active_prvdr_curr_clean ; 
quit ; 

%put NOTE: combining active provider file with CURRENT CDR extract ; 

proc sort data = &pac._current_SBSA
			out = &pac._curr_qrt_sort ; 
	by ccn ; 
run ; 

proc sort data = active_prvdr_curr_clean
			out = active_prvdr_curr_clean_sort ; 
	by ccn ; 
run ; 

data &pac._curr_active_combined_raw ; 

	merge &pac._curr_qrt_sort (in=in_cdr)
			active_prvdr_curr_clean_sort (keep = ccn 
											active_OPD 
										in=in_act)
			;
	by ccn  ;

	cdr_prvdr = in_cdr ; 
	act_prvdr = in_act ; 

run ; 

title "QA combining data" ; 
title2 "providers in one list and not other" ; 
proc sql ; 
	select count(*) as num_snf_total
	from &pac._curr_active_combined_raw
	;
	title3 "num in cdr not in active provider file" ;
	title4 "grouped by termination date status" ;
	title5 "term status of 1 on this list is expected" ; 
	title6 "term status of 0 on this list will be investigated more" ;  
	select count(*) as num_miss_act,
			case when missing(trmntn_exprtn_dt) then 0
				else 1
			end as termed_status
	from &pac._curr_active_combined_raw
	where cdr_prvdr = 1
			and
			act_prvdr = 0
	group by calculated termed_status
	;
	/*title3 "num in active provider file not in cdr" ; 
	select count(*) as num_miss_cdr
	from &pac._curr_active_combined_raw
	where cdr_prvdr = 0
			and
			act_prvdr = 1
	;*/
	title3 "List of providers in active provider file not in cdr" ; 
	select ccn,facility_name as facility_name,active_OPD as active_OPD
	from active_prvdr_curr_clean
	where ccn in ( select ccn
	from &pac._curr_active_combined_raw
	where cdr_prvdr = 0
			and
			act_prvdr = 1
	);
quit ; 

title "Investigate providers missing from Active Provider File that have not termed" ;  
proc sql  ; 
	select ccn,
			fac_name,
			orgnl_prtcptn_dt
	from &pac._curr_active_combined_raw
	where cdr_prvdr = 1
			and
			act_prvdr = 0
			and
			missing(trmntn_exprtn_dt)
			and
			prxmatch("&ccn_snfany", ccn)
	;
quit ; 
title ; 

title "Investigate providers missing opd from active file but on both lists" ; 
title2 "There should be no output for SNF" ; 
proc sql ; 
	select ccn,
			fac_name,
			orgnl_prtcptn_dt,
			active_opd,
			trmntn_exprtn_dt
	from &pac._curr_active_combined_raw 
	where missing(orgnl_prtcptn_dt)
			and
			missing(active_OPD)
			and 
			act_prvdr = 1
	;
quit ; 
title ; 

%put NOTE: clean up var names of curr active combined raw ; 
%put NOTE: a step here has been added to capture the original participation date from CDR if the provider is not on active provider file ; 
 

data &pac._curr_active_combined_clean ; 

	set &pac._curr_active_combined_raw (rename =
				(active_OPD = historical_OPD 
				orgnl_prtcptn_dt = current_OPD)) ;

	if _n_ = 1 then do;
		declare hash lookup(dataset:"work.active_prvdr_curr_clean (rename = (facility_name = fac_name city = city_name state = state_cd zip_code = zip_cd address_line_1=st_adr))"); 
		lookup.definekey("ccn"); 
		lookup.definedata("fac_name",
						  "city_name",
						  "state_cd",
						  "zip_cd",
						  "st_adr"); 
		lookup.definedone(); 
	end ; 

	if cdr_prvdr = 0 and act_prvdr = 1 then do;
		rc=lookup.find(); 
	end;

	if missing(Swing_Bed) and cdr_prvdr = 0 and act_prvdr = 1 then do;
		if prxmatch("&CCN_SB",ccn) then 
			Swing_Bed = 1;
		else
			Swing_Bed = 0;
	end;

	if missing(historical_OPD)
		and
		act_prvdr = 0
	then historical_OPD = current_OPD ; 

	drop 
			act_prvdr
			cdr_prvdr
			rc
			;
run ; 

title "verify providers only missing historical opd are subunits" ;
title2 "those providers will be assigned the default tie in date" ;   
proc sql ;  
	select ccn,
			fac_name
	from &pac._curr_active_combined_clean
	where missing(historical_OPD) and prxmatch("&ccn_snfany", ccn)
	;
quit ; 
title; 

%put NOTE: calulate and QA tiein ; 
	* tie in specfications can be found in criteria ; 
	data work.&pac._tiein (compress = yes) ; 
		set &pac._curr_active_combined_clean ; 
		if historical_opd ~= . then 
			do;
				historical_OPD_plus30 = historical_OPD + 30;
	    		months_to_next_quarter = 3 - mod(month(historical_OPD_plus30)-1, 3);
	    		next_quarter=month(historical_OPD_plus30) + months_to_next_quarter;
	    	end ; 
		if missing(historical_OPD) then 
           tiein = &tieindefault;
	    else if next_quarter = 13 then 
           tiein=MDY(01, 01, year(historical_OPD_plus30)+1);
    	else if next_quarter in (4, 7, 10) then
           tiein = MDY(next_quarter, 01, year(historical_OPD_plus30));
		format historical_OPD_plus30
				tiein
				date9. ;
	run ;

	title "QA of tie in calculations -- checking table &pac._tiein" ;
	title2 "num obs not changed since last" ;  
	proc sql ; 
		select count(*) as num_obs
		from work.&pac._tiein ; 
	quit ; 
	title ; 
	
	title2 "historical_opd and tiein should be listed here" ; 
	title3 "all dates should be formatted with date9." ;   
	proc sql ; 
		select name,
				format
		from dictionary.columns
		where libname = "WORK"
				and 
				memname = upcase("&pac._tiein")
				and
				index(format, "DATE") > 0
		; 
	quit ; 
	title ; 
	
	
	title "do dates display as such on these limited obs" ; 
	title2 "checking table &pac._tiein" ;  
	proc print data = work.&pac._tiein (obs = 5) ; 
		var ccn
			next_quarter 
	        historical_OPD 
	        historical_OPD_plus30 
	        tiein;
	run;
	title ;

	 
	
	
	title "There should be no providers whose tiein year did not change when it should have" ; 
	title2 "checking table &pac._tiein" ;  
	proc sql  ; 
		select count(*) as num_year_wrong
		from work.&pac._tiein
		where next_quarter = 13 
			AND
			(year(tiein) = year(historical_OPD));
	quit ;

	
	title "There should be no providers whose tiein < historical OPD" ; 
	title2 "checking table &pac._tiein" ;  
	proc sql  ; 
		select count(*) as num_tie_gt_opd
		from work.&pac._tiein
		where tiein < historical_OPD;
	quit ; 

	title	"QA -- Month of tie in by month of OPD";
	title2	"Should follow the rule that tie in date is the first day of the next quarter unless OPD is within 30 days of the start of the new quarter";
	title3 "checking table &pac._tiein - where not missing(historical_OPD)" ; 
	proc sql;
		select	month(historical_OPD) as OPD_month,
				month(historical_OPD_plus30) as OPD_plus30_month,
				month(tieIn) as tieIn_month,
				count(*) as Providers
			from	work.&pac._tiein
			where not missing(historical_OPD)
			group by	calculated OPD_month,
						calculated OPD_plus30_month,
						calculated tieIn_month
		;
	quit;
	
	title;
	
	title	"QA -- Check to make sure day of Tie In is always 1";
	title3 "checking table &pac._tiein" ; 
	proc sql;
		select	distinct day(tieIn) as tieIn_Day_of_month
		from	work.&pac._tiein
		;
	quit;
	title;
	
	title	"Make sure year of Tie in is always either the same as OPD or the next year";
	title2	"If quarter of OPD is 1 or 2, delta year should be zero.";
	title3	"If quarter of OPD is 3, delta year may be 0 or 1.";
	title4	"If quarter of OPD is 4, delta year should be 1.";
	title5 "checking table &pac._tiein - where not missing(historical_OPD)" ; 
	proc sql;
		select	distinct
				qtr(historical_opd) as Historic_OPD_Qtr,
				year(tieIn) - year(historical_opd) as Delta_Year
		from	work.&pac._tiein
		where not missing(historical_OPD)
		;
	quit;	
	title;

	title "QA of tie in calculations -- checking table &pac._tiein" ;
	title2 "looking for OPD with year of current report where tiein is month following historical opd and day of month is not the first" ; 
	title3 "number_of_problems should be 0 :)" ;
	proc sql ; 
		select count(*) as number_of_problems
		from	(select	historical_OPD,
						tiein
				from 	&pac._tiein
				where 	intck("month", historical_opd, tieIn) = 1
						and day(historical_opd) ~= 1
				)
				;
	quit ;
title ; 


		
%put NOTE: combine with extra data - POS file ; 
	* the urban rural indicator is the only value needed from POS for OR and APU ; 
	* however we may need to look up waivers by county and will need ; 
	* both ssa_state_cd and ssa_cnty_cd ; 
	* the existing variable in the SNF sets county_st is an unreliable version of ssa_cnty_cd ; 
	* emphasis on unreliable ;  
	data work.dates_pos (compress = yes) ; 
		length cbsa_urbn_rrl_ind $3 ;
		length ssa_state_cd $3 ; 
		length ssa_cnty_cd $4 ;  
		if _N_ = 1 then 
			do ; 
				declare hash lookup(dataset:"work.pos_clean") ; 
				lookup.definekey("ccn") ; 
				lookup.definedata("cbsa_urbn_rrl_ind",
									"ssa_state_cd", 
									"ssa_cnty_cd") ; 
				lookup.definedone() ; 
				call missing(cbsa_urbn_rrl_ind, 
								ssa_state_cd, 
								ssa_cnty_cd) ; 

				declare hash lookup_sb(dataset:"work.pos_clean") ; 
				lookup_sb.definekey("ccn") ; 
				lookup_sb.definedata("cbsa_urbn_rrl_ind") ; 
				lookup_sb.definedone() ; 
				call missing(cbsa_urbn_rrl_ind) ; 
			end ; 
		set work.&pac._tiein ;
		If Swing_Bed=1 then
			RC = lookup_sb.find() ; 
		else if Swing_Bed=0 then	
			RC = lookup.find() ;
		drop RC ;  
	run ; 

	* I want to see how many are missing cbsa_urbn_rrl_ind ;
	* this number should be monitored quarter to quarter ; 
	* an unexpected rise in this value may indicate a problem with the provider data ; 
	title "file dates_pos - num missing cbsa" ;
	title2 "num missing historically is low in Q1 and grows throught the year up to 60 max expected" ; 
	title3 "monitor num missing quarter to quarter" ; 
	title4 "a severe increase in num missing without explaination might point to a problem" ;   
	proc freq data = work.dates_pos ;
		table cbsa_urbn_rrl_ind / missing 
									nocum		
									nopercent;
	run ; 
	title ; 

	* since there are missing values we need more context ;
	title "file dates_pos - are the missing cbsa ind mostly swing bed or stand alone providers?" ; 
	proc freq data = work.dates_pos ; 
		table cbsa_urbn_rrl_ind*swing_bed / missing  ;
		where missing(cbsa_urbn_rrl_ind) ; 
	run ; 
	title ; 
	* I want to see if these providers are missing from the raw pos file ;
	* dump their ccns into a macro variable ;
	  
	proc sql noprint; 
		select quote(strip(ccn))
		into :ccnlist separated by ", "
		from work.dates_pos
		where missing(cbsa_urbn_rrl_ind)
		;
	quit ; 
	title ; 
	* use the macrovariable to proc print from the raw pos file ;
	title "checking raw pos file for ccns still missing cbsa" ; 
	title2 "is it reasonable that these providers found no match for cbsa" ; 
	title3 "a num_ccn of 0 means they were not in the clean pos file" ; 
	proc sql;
		select count(ccn) as num_ccn
		from work.pos_clean  
		where strip(ccn) in (&ccnlist) ; 
	quit ; 
	title ; 
	
	*	Backfill SSA using CSP_PRVDR_CMN dataset	;
	title	"Providers missing SSA information";
	proc sql;
		select	nmiss(ssa_state_cd) as nmiss_ssa_state_cd,
				nmiss(ssa_cnty_cd)	as nmiss_ssa_cnty_cd
		from	work.dates_pos
		;
	quit;		
	
	data	work.dates_pos_backfill_debug (keep = ccn backfill)
			work.dates_pos_backfill (drop = backfill)
			;
		set	work.dates_pos;
		*	Hash table with CSP_PRVDR_CMN data	;
		*	We're assuming, in order to make things easy, that providers that are missing state cd are also missing county code	;
		if _n_ = 1 then do;
			declare hash csp(dataset:"wkbn.&in_curr_CSP (rename = (prvdr_num = ccn))");
			rc = csp.defineKey("CCN");
			rc = csp.defineData("ssa_state_cd", "ssa_cnty_cd");
			rc = csp.defineDone();
		end;
		backfill = 0;
		if ssa_state_cd = "" and ssa_cnty_cd = "" then do;
			rc = csp.find();
			backfill = ~rc;			
		end;
		drop rc;
	run;
	
	title	"Check SSA code backfill";
	title2	"Providers backfilled";
	title3	"This should be the number of providers missing SSA code at most";
	title4	"May be less than that if any providers are missing from the CSP_PRVDR_CMN table";
	proc freq	data = work.dates_pos_backfill_debug;
		table	backfill / missing;
	run;
	
	title2	"Check providers with populated SSA codes in backfilled table";
	title3	"Should be the difference between providers missing SSA code in original table and providers backfilled";
	proc sql;
		select	nmiss(ssa_state_cd) as nmiss_ssa_state_cd,
				nmiss(ssa_cnty_cd)	as nmiss_ssa_cnty_cd
		from	work.dates_pos_backfill
		;
	quit;		
	
	title;

	* before I clean the final set ; 
	* I want to see if the third position is U, W, oy Y in any providers ; 
	* if there are U, W, or Y s then the cleaning stage needs to be amended ; 
	* if there are none of the above then then cleaning statements can be run as is ; 
	data work.QAccn (compress = no) ;
		set work.dates_pos_backfill ; 
		third_pos = substr(ccn, 3, 1) ; 
	run ; 
	title "file QAccn  - third position of ccn in table dates_pos_backfill" ; 
	title2 "look for U, W, Y" ;
	title3 "if U, W, or Y is present the data cleaning step will need to change" ; 
	proc freq data = QAccn ; 
		table third_pos / missing ; 
	run ; 
	title ; 


%put NOTE: clean up final set ; 
	* we drop unwanted providers ; 
	* for SNF any provider with a letter in the ccn is dropped ; 
	* SNF is also known to have several junk files ;
	* prior to 2020Q2 these were dropped ;
	* instead we will flag them as potential junk ; 
	* the APU checks that all junk flagged providers were caught by the no data rule ; 
	 
%put NOTE: in scope criteria will evaluate as &=in_scope_criteria ; 
%put NOTE: using common macro ccn pattern for all Snf &ccn_snfany ;
%put NOTE: upcase function here for demographic variables that are inconsistently upcase in source data ;  
	data work.&pac._final (compress = yes) ; 
		set work.dates_pos_backfill ; 
		
		fac_name 	= 	upcase(fac_name) ; 
		st_adr		=	upcase(st_adr) ; 
		city_name	=	upcase(city_name) ;


		* SNF is known to have several test files which need to be removed;
   		
   		*	We do delete providers with invalid CCNs	;
    	if ccn = "123456" 
    		or ~prxmatch("&ccn_snfany", ccn) then delete;

		*	Most of these we flag as junk. The expectation is that ;
		*   these will be dropped during APU due to the no data rule	;
    	if prxmatch("/\bADDRESS\b/i",st_adr) then junk = 1;
    	if prxmatch("/\bTRAIN(ING)?\b/i", fac_name) then junk = 1;
    	if prxmatch("/\bTRNG\b/i", fac_name) then junk = 1;
    	if prxmatch("/\bTEST\d*\b/i", fac_name) then junk = 1;
    	if prxmatch("/\bFAKE\b/i", fac_name) then junk = 1;
    	if prxmatch("/\bNULL\b/i", fac_name) then junk = 1;
    	if prxmatch("/\bTRAIN(ING)?\b/i", st_adr) then junk = 1;
    	if prxmatch("/\bTRNG\b/i", st_adr) then junk = 1;
    	if prxmatch("/\bTEST\d*\b/i", st_adr) then junk = 1;
    	if prxmatch("/\bFAKE\b/i", st_adr) then junk = 1;
    	if prxmatch("/\bNULL\b/i", st_adr) then junk = 1;
    	if prxmatch("/\bTO BE REMOVED\b/i", fac_name) then junk = 1;
		if prxmatch("/\bSAMPLE\b/i", fac_name) then junk = 1 ; 
		if prxmatch("/\bTESTING\b/i", fac_name) then junk = 1;
		if prxmatch("/\bHOME FOR RETIRED PROGRAMMERS\b/i", fac_name) then junk = 1;
		if upcase(fac_name) = "PRACTICE" then junk = 1 ; 
		if upcase(fac_name) = "PRACTICE NURSING HOME" then junk = 1;
		if upcase(fac_name) = "NAME" then junk = 1;
		if prxmatch("/\bINVALID\b/i", fac_name) then junk = 1;
		if prxmatch("/\bACCT CLSD\b/i", fac_name) then junk = 1;
		if prxmatch("/\bCLOSED\b/i", fac_name) then junk = 1;
		if upcase(st_adr) = "Z" then junk = 1;
		if upcase(fac_name) = "ZZZ" then junk = 1;
		

		if &in_scope_criteria
			then in_scope = 1;
		else if not &in_scope_criteria
			then in_scope = 0;


	run ; 

	*check num junk ;
	
	title "QA: records flagged as junk" ;
	title2 "how many junk files have in_scope = 1" ;
	title3 "I will expect in scope = 1 junk records to be around 9" ;  
	title4 "checking table &pac._final " ; 
	proc freq data = &pac._final ; 
		table junk * in_scope / missing ; 
	run ; 
	title ; 
	
	title	"QA -- Manual review of providers flagged as junk";
	title2	"Look at CCN, facility name, and street address -- consider whether these seem like training or junk providers or not" ; 
	title3 "checking table &pac._final subset - where junk = 1 and  in scope = 1";  
	proc print	data =	work.&pac._final
			(keep =	fac_name 
					st_adr
					state_cd 
					ccn 
					junk 
					trmntn_exprtn_dt
					in_scope);
			where junk = 1 
						AND 
				in_scope = 1;
	run;
	title;

	title	"QA of final data set -- &pac._final";
	title2 "Check for missing values.";
	proc means data=work.&pac._final n nmiss min max;
	    var _numeric_;
		title3 "tiein CANNOT have missing values" ; 
		title4 "in_scope should NOT have missing values" ; 
	run;
	proc freq data=work.&pac._final nlevels;
	    tables _character_ / missing noprint;
		title3 "U/R, ssa_state_cd and ssa_cnty_cd can have missing levels" ;
		title4 "no other char should have missing levels" ; 
		title5 "nlevels of ccn should equal num obs" ;
	run;
	title ; 
	
	title "how many providers have an in_scope of 0" ; 
	proc sql;
		title2 "Checking current_extract SB and SA combined" ;	
		title3 "number expected classified as out of scope" ; 
		select count(*) as num_expected_OUT_of_scope
		from   &pac._current_SBSA
		where not( &in_scope_criteria
				)
					AND
				ccn ~= "123456" 
    				AND
				prxmatch("&ccn_snfany", ccn)
					 ; 
		title2 "Checking table &pac._final" ;
		title3 "number actually classified as out of scope" ; 
		select count(*) as num_OUT_of_scope
		from work.&pac._final  
		where in_scope = 0 ; 
	quit ;
	title ;

	title "how many providers have an in_scope of 1" ; 
	proc sql;
		title2 "Checking current_extract SA and SA combined" ;
		title3 "number expected classified as in scope" ; 
		select count(*) as num_expected_IN_scope
		from   &pac._current_SBSA
		where (&in_scope_criteria
					) 
					AND
				ccn ~= "123456" 
    				AND
				prxmatch("&ccn_snfany", ccn) ; 
		title2 "Checking table &pac._final" ;
		title3 "number actually classified as in scope" ; 
		select count(*) as num_IN_scope
		from work.&pac._final  
		where in_scope = 1 ; 
	quit ;
	title ; 

	title "checking creation of in_scope" ; 
	title2 "summary stats for term date by in_scope value" ;
	title3 "in_scope of 1 should have EITHER a missing term date" ; 
	title4 "or a term date >= 01JAN&term_year. " ; 
	title5 "in_scope of 0 should have a term date < 01JAN&term_year. " ; 
	title6 "checking table &pac._final " ; 
	proc tabulate data = work.&pac._final ; 
		class in_scope
			   
				/ missing;
		var trmntn_exprtn_dt	
				; 
		table in_scope ,
				trmntn_exprtn_dt * (n nmiss)
				trmntn_exprtn_dt * (min max) *f=date9.
				;		 
	run ; 
	title ; 


%put NOTE: proc compare the current assembled provider file
		with the previous quarters assembled file ; 
	title "compare the final output for current quarter - base
		to the final output of previous quarter - compare" ; 

	proc compare    
				base = work.&pac._final (drop = caads_dt
												historical_OPD
												current_OPD
												tiein )
				compare = lib_prev.&in_prev (drop = caads_dt
												historical_OPD
												current_OPD
												tiein)
				listall
				transpose
				;
				id ccn;
	run;



%put NOTE: investigate the provider differences ; 

	title "are the providers in curr qrt not in prev qrt new? - stand alone providers" ; 
	title2 "if count is not zero -- check for a recent csp_prvdr_add_dt (adddate) or a generic one " ; 
	title3 "checking file current quarter's extract - stand alone" ; 
	proc sql ; 
		select count(*) as num_not_in_prev
		into :num_miss_prev_SA
		from wkbn.&in_curr_SA
		where mcare_id in (select ccn
							from work.&pac._final
							where ccn not in (select ccn
												from work.&pac._prev_qrt
											)

								AND swing_bed = 0
				)
		;
	quit ; 
	title ; 


%if &num_miss_prev_SA >0 %then %do ; 
    title "are the providers in curr qrt not in prev qrt new? - stand alone providers" ; 
	title2 "check for a recent csp_prvdr_add_dt (adddate) or a generic one and the partci_dt" ;
	title3 "checking file current quarter's extract - stand alone" ; 
	proc sql ; 
		select mcare_id
			  , adddate
			  , partci_dt
		from wkbn.&in_curr_SA
		where mcare_id in (select ccn
							from work.&pac._final
							where ccn not in (select ccn
												from work.&pac._prev_qrt
											)

								AND swing_bed = 0
				)
		;
	quit ; 
    title;
%end ;

	title "are the providers in curr qrt not in prev qrt new? - swing bed providers" ; 
	title2 "if count is not zero -- check for a recent csp_prvdr_add_dt (adddate) or a generic one " ; 
	title3 "checking file current quarter's extract - swing bed" ;
	proc sql ; 
		select count(*) as num_miss_prev_SB
		into :num_miss_prev_SB
		from wkbn.&in_curr_SB
		where prvdr_num in (select ccn
							from work.&pac._final
							where ccn not in (select ccn
												from work.&pac._prev_qrt
											)

								AND swing_bed = 1
				)
		;
	quit ; 

%if &num_miss_prev_SB >0 %then %do ;
	title "are the providers in curr qrt not in prev qrt new? - swing bed providers" ; 
	title2 "check for a recent csp_prvdr_add_dt (adddate) or a generic one " ;
	title3 "However, SwingBed providers can show up in new file without a recent date" ;
	title4 "checking file current quarter's extract - swing bed" ; 
	proc sql ; 
		select prvdr_num,
				fac_name,
				csp_prvdr_add_dt,
				orgnl_prtcptn_dt
		from wkbn.&in_curr_SB
		where prvdr_num in (select ccn
							from work.&pac._final
							where ccn not in (select ccn
												from work.&pac._prev_qrt
											)

								AND swing_bed = 1
				)
		;
	quit ; 
	title ; 
%end ; 	

	


	title2 "count of provider in prev Qrt not in current quarter" ; 
	title3 "num_not_in_curr expected to be 0" ; 
	title4 "checking file lib_prev.&in_prev." ; 
	proc sql ; 
		select count(*) as num_not_in_curr
			into :num_not_in_curr
			from lib_prev.&in_prev
			where ccn in 	(	select ccn
									from work.&pac._prev_qrt
									where ccn not in 	(	select ccn
																from work.&pac._final
														)
							)
		;
		
	
	quit ; 

%if &num_not_in_curr >0 %then %do ;

	proc sql ; 
		title4 "that should be investigated" ; 
		title5 "from prev assembled file " ; 
		select ccn,
				fac_name,
				state_cd,
				tiein
			from lib_prev.&in_prev
			where ccn in 	(	select ccn
									from work.&pac._prev_qrt
									where ccn not in 	(	select ccn
																from work.&pac._final
														)
							)
		;
	quit ; 

%end ; 

title ;

	/*********************** OUTPUT ************************************/
	
%put NOTE: ***** SAVING ; 
	/* save final set to workbench ; 
	* use macro with full final name ;
	* this has the report date built in ;*/ 


	data wkbn.&outfull ; 
		set work.&pac._final ; 
	run ; 


%put NOTE: END active code ; 
 