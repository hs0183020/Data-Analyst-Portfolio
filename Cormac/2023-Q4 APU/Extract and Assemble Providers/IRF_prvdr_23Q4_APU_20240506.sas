/******************* HEADER ***********************
	PROGRAM: 	IRF Provider Assembly

	CREATED BY:	Elizabeth Oliver (EO)

	PURPOSE:	This program combines provider files used in the most recent 
				APU reporting cycle as part of a larger process to produce APU Analytics.

	INPUTS:		This program has four (4) inputs.
				irf_prvdr_2xQx_(type)_2xxxxx (previous quarter's assembled file)
					the previous quarter's assembled file can be found on the workbench of the prior reporting cycle
				irf_prvdr_xtrct_XXXXXX (current quarter's extract - raw provider data)
					ALL provider XTRCT files 2020Q4 and newer can be found on the workbench of the current reporting cycle
				Source_file_name_2XQX_XXXX.xlsx Sheet = IRF_Provider (current control table)
					the current control table can be found in the workbench of the current reporting cycle
				cy202xposurbanrural - POS DEC 202x updates stored as sas data set 
					Souced in workbench path:
					/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/Static Files
				IRF_Active_Provider_List_20xx(month).txt (latest IRF Quarterly Active Provider File) --  found on workbench in "Active Provider Files"
					under Quarterly and Annual Reporting.
	METHOD:		This program contains the following subsections and methods.
				MACROS: 		assign libraries and define macro variables and parameters.
				IMPORTS: 		Import all data files needed and perform and cleaning necessary.
				COMBINING:		Combine all data sets, keep only fields needed to calculate historical OPD
				MANIPULATIONS:	Manipulate to calculate historical opd, and calculate tiein date
				COMBINE:		Combine the historical OPD set with the current quarters extract
								the original participation date from the current quarters extract will be the new current_OPD	
				EXTRADATA:		Combine with POS file to get urban rural indicator using the POS table as a hash object
				COMPARISONS:	Compare the results of the above steps with the previous qaurter's file using PROC COMPARE
				SAVING:			Write the final table to the workbench

	NEW for 2020Q3:
				This program will read the 2019 direct files from their home database directly.
				Previously, the files were written from pac_qrp_swgt_qies to pac_qrp_swgt then imported into caslib
				This was a throwback to the beginning days on hive when Liz did not know any better.
				She has evolved with time and now her code has too. ;)

				Also new, I am combining the POS fields using a hash object instead of a merge

	NEW for 20Q4 OR:
				The inputs for this program have been streamlined.
				The previous quarter's assembled provider file will be combined
				with the current quarter's extract to find the historical_OPD 
				(the oldest orgnl_prtcptn_dt on record). The current participation date
				will come from the current extract. Other methods for calculating tiein and
				finding the cbsa_urbn_rrl_ind remain the same.

	New for 21Q1 OR:
				All row subsetting involving dates have been removed. 
				We will no longer drop providers based on termination dates or tiein dates.
				


	TO UPDATE:	To update this program the macro variables at the top will need to be updated extensively.
				
				
	RECORD OF current quarter's UPDATES:
		07/30/2021	-	updated for use in 21Q1 apu reporting cycle
		08/19/2021	-	production input updates


	10/05/2021 - Updates for 2021 Q2 OR (Chid)
		- (lines 72-120) updated program parameters updated to reflect 2021 Q2 OR,
			             latest - directories, assembled provider file, provider extract,
					             POS file and control table.

	10/18/2021 - Production run for 2021 Q2 OR (Chid)
                   - (lines 558-573) QA -- Month of tie in by month of OPD
						---> added OPD_plus30_month column to make reading the table more straight forward
					         (Pam's suggestion from development)
					         
	11/4/2021	- Development for 2021-Q2 APU (Alex)
				Updated parameters
				Section signposting in notes
				Rejiggered titles in QA
				Expanded QA check in case where provider appears to have closed and reopened
				Tweaked some other QA
				
	11/5/2021 - Tweaked PROC COMPARE at the end of the program to use TRANSPOSE option. Didn't run new output or log.


	11/19/2021	- Production Run for 2021-Q2 APU (Chid)

	12/28/2021	- 	pre-Development for 2021-Q3 OR (EO)
				-	updated inputs and parameters
				-	removed chunk of code investigating 10T226 from Q2 APU (see that code for details)
				-	added a few more notes to log near end of code

	01/06/2022	- 	Development for 2021-Q3 OR (KAB)
				-	updated: in_curr = irf_prvdr_xtrct_220105

	01/19/2022	- 	Production run for 2021-Q3 OR (Chid)

	02/04/2022	- 	Development run for 2021-Q3 APU (Chid)

	02/08/2022	- 	updated POS file for 2021-Q3 Development run (Chid)

	02/18/2022	- 	Production run for 2021-Q3 APU (Chid)
				-   updated in_curr parameter (current quarter's extract)
				
	3/8/2022 (KAS)	- Pre-development for 2021-Q4
				- 	Updated inputs and parameters
	4/5/2022 (EO)	- development
				-	development inputs
				-	added a few notes to log in parameters section

	04/18/2022 - production run for 2021-Q$ OR (Kwame)
			   - updated in_curr parameter

	5/10/2022	- Development for 2021-Q4 APU (EO)
					- development inputs
					- cleared extraneous commenting
					- added statements to fix ccn length to minimize downstream messaging
					- cleaned up some QA
					- added common control table import
					- added lots of notes to the log
					- added check for ccn pattern from common macro
	5/23/2022	- production
					- prod inputs

	07/06/2022 (EO) - 22q1 or development
					- dev inputs and other parameters

	07/19/2022 EO	- 22q1 or production
					- inputs to prod data

	08/03/2022 (Chid) - Developement Run for 2022 Q1 APU
					  - additional use of common macros program in the parameter section
	08/18/2022 (EO) - production inputs

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

	11/03/2022 (EO) - 	development for 2022-Q2 APU
	
	11/22/2022 KAS	-	Production for 2022-Q2 APU

	01/05/2023 (CT) - 	development for 2022-Q3 OR

	01/17/2023 (EO)	-	prod for 22Q3 OR

	02/07/2023 (SS) - 	development for 2022-Q3 APU
                        Updated to use new POS file.
                        Changed titles for POS file check.
						Updated sort order for QA -- Compare revised historical OPD calculation with original method to use CCN, CAADS_DT

    02/21/2023 (CT) - 	Production run for 2022-Q3 APU

	03/23/2023 (SS) - 	Development Run for 2022-Q4 OR

    04/18/2023 (SS) - 	Production Run for 2022-Q4 OR

	05/04/2023 (SS) - 	Development Run for 2022-Q4 APU

    05/05/2023 (SS) - 	Removed code related to multi-org in the parameter section. 
                        Removed code related to multi-org in the output section.

	05/19/2023 (SS) - 	Production Run for 2022-Q4 APU

	07/13/2023 (EO) -	dev for 2023-Q1 OR
					-	added active provider file to grab opd for use as historical opd
					-	temporarily added code for what to do if provider is not on active provider file
						if that happens opd and tie in will be missing
						since tie in cannot be missing, we need to assign it a value
						the previous CMS decision to use 01JAN2018 may no longer
						be desired. The question is being brought to COR.
					-	the temporary code uses active provider file opd,
						unless the provider is not on the file, then it uses the CDR OPD
						the default tie in is only used if provider is missing opd in both files
					-	FOR NOW, all code that previously was needed to find
						historical OPD remains. Once we confirm new method, we can remove
					- 	code to combine and grab all needed variables no longer needed
						it was removed 

	7/19/2023 (EO)	-	prod run 23Q1 OR
					-	CMS decision to accept our suggestion on OPD when provider missing from
						active provider file or missing opd in all cases
					-	added length to ccn in current file 
						to avoid upstream trunctation messages

	08/14/2023 (SS) - 	Development Run for 2023-Q1 APU.
                    -   Updated the certification_date informat based on the new source data.

	08/18/2023 (SS) - 	Production Run for 2023-Q1 APU.

	10/11/2023 (SS) -   Development Run for 2023-Q2 OR.
                    -   Removed all code related to old method of finding historical OPD.
				    -   Added format for prvdr_num.

	10/18/2023 (CT) -   Production Run for 2023-Q2 OR

	10/19/2023 (CT) -   Production Run for 2023-Q2 OR (no rerun needed)
					- 	Deleted notes/titles implying we are waiting on the decision 
					    for handling active provider files missing OPD
		
	11/08/2023 (CT) -   Development Run for 2023-Q2 APU
					   (provider extracts copied from Q2 OR folder due to CDR refresh issue)		

	11/22/2023 (CT) -   Production Run for 2023-Q2 APU
					-	used the Nov 2023 Quarterly Active Provider Files

	01/04/2024 (HS) -   Development Run for 2023-Q3 OR

	01/17/2024 (HS) -   Production Run for 2023-Q3 OR

	02/13/2024 (SS) -   Development Run for 2023-Q3 APU

	03/14/2024 (SS) -   Production Run for 2023-Q3 APU
                        - Using new Active Provider file from folder 2024-02.
						- Updated informat for certification_date to yymmdd10.

	04/11/2024 (HS) -   Development Run for 2023-Q4 OR

	04/16/2024 (HS) -   Production Run for 2023-Q4 OR

	05/06/2024 (CT) -   Development Run for 2023-Q4 APU
					- 	Need to update the IRF Active Provider File for production run


******************* END HEADER **************************/

%put NOTE: MACROS AND PARAMETERS ;
* common macro set up ;
%let folder	= 2023-Q4 APU ;
%let cmn_prgm_dt = 20240422 ;

* common macro set up ; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Support Programs/APU_Common_Macros_&cmn_prgm_dt..sas";
%include cmacro;
	* setting ; 
	%let pac = IRF ; 

	* reporting cycle type OR=outreach vs APU ; 
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
	* general path to workbench ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ; 
	* current quarter specific folder ; 
	%let q_folder = &cmn_folder ;
	%put NOTE: &=q_folder;
	*current quarter's workbench path for data (input and output); 
	%let wkbn_path = &bench/&q_folder/Extract and Assemble Providers/Data ; 
	
	* path to active provider files ; 
	%let active_month = 2024_02 ; 
	%let active_dir = &bench/Active Provider Files/Quarterly Files/&active_month ; 
	%let active_file = IRF_Active_Provider_List_2024February.txt ; 
	%let active_path = &active_dir/&active_file ; 

	%put NOTE: previous cycle for provider assembly should reference most recently run report - either OR or APU ; 
	* previous cycle's folder ;
************* Need to update this parameter manually ***********;
	%let p_folder = 2023-Q4 OR; 
	*previous cycle's workbench path ;
	%let prev_path = &bench/&p_folder/Extract and Assemble Providers/Data  ;
	
	* workbench path for pos file ; 
	%LET POS_dir = &bench/Static Files ;

	 

	* libnames ;
	* library to current quarter ; 
	libname wkbn
			"&wkbn_path" ; 
	* library to previous cycle ; 
	libname lib_prev
			"&prev_path" ;
	libname posbn
			"&POS_dir" ;



***************** Need to manually update these two parameters **************;
	* input file information ; 

	* input filenames ; 
	* previous cycle's assembled file name ; 
	%let in_prev	= 	irf_prvdr_23q4_or_240416;
	
	* current quarter's provider extract ;  
	%let in_curr   =  irf_prvdr_xtrct_240502;
	 


	* control table ; 	
	* control table paths are automated in the common macro program ; 
	* here we just need the relevant sheet name ;
	%let c_sheet = IRF_Provider ; 

	* references for POS file ;
	%LET in_POS =  pos_2022_ur_byssa_20230125;

	%put NOTE: inputs summary ; 
	%put NOTE: provider data will be pulled from &in_curr for &pac providers ; 
	%put NOTE: control file sheet &c_sheet will be used ; 
	%put NOTE: current data will be combined with previous cycles data in &in_prev ; 
	%put NOTE: the POS file used will be &in_POS ; 
	%put NOTE: Historical OPD/Tie in will derive from the active provider file &active_file received in &active_month ; 

	

	* output tools ; 

	* report date assignment ; 
	%let report_date = %sysfunc(today() , yymmddn6.) ; 
	
	*final output naming ; 
	%let outfull = &pac._prvdr_&qrtyr._&type._&report_date ; 


	* fixed report values ; 

	* update as criteria changes ; 
	* date desired for default tie in if orgnl_prtcptn_dt is missing ;
	%let tieindefault = "01JAN2018"d ; 

 
	* dynamically create default date to flag termed providers ;  
	%let term_year = &cmn_term_year ;
	%put NOTE: &term_year ;
 
	%let in_scope_criteria = 
			%str((missing(trmntn_exprtn_dt)
				OR
			trmntn_exprtn_dt >= "01JAN&term_year."d)) ; 


	%put NOTE: in scope criteria is &in_scope_criteria ;


	%let ccn_irf = &ccn_pattern_irf ; 
	%put NOTE: ccn pattern to assign locally using common definition ; 
	%put NOTE: resolves to &=ccn_irf ; 


	 
/******************* END parameters ***********/
%put NOTE: END PROGRAM PARAMETERS ; 

%put NOTE: ***** IMPORT DATA ; 

	* import and clean control file ; 
	* the control file is used to subset desired columns ; 
	/* from common macro program */
	%set_control_file(
							setting		= &pac,
							ctrl_sheet	= &C_sheet ,
							ctrl_out 	= control);
	 
	
	* the control file contains the list of variable from the input we wish to keep ;
	* use proc sql to create a space delimited list into a macro variable ; 
	* this will be used in the import steps to subset to the desired variables ; 
	title	"QA  -- Control file";
	title2 	"Vars listed should be same as source_file_name in control file" ; 
	proc sql ;
	     select Source_File_Name
	     into :varlist separated by " "
	     	from work.control
	     ;
	quit; 
	
	* QA check that the macro varlist contains the same values as the source_file_name column in the control file;
	%PUT NOTE: are these separated by a space... &=varlist;


%put NOTE: import provider files ;
%put NOTE: previous quarter assembled file ; 
	* from the previous quarter's file we need ccn, historical_opd, and termination date...
		caads_dt is kept for QA purposes ; 
	* historical_opd is renamed to original participation date to match current extract ; 
	data work.&pac._prev_qrt (compress = no) ;
		set lib_prev.&in_prev (keep = 	ccn 
										historical_opd 
										trmntn_exprtn_dt 
										caads_dt) ; 
		rename historical_OPD = orgnl_prtcptn_dt ;
	run ; 
%put NOTE:  current quarter extract ; 
	* from the current quarter's extract we need variables on control table list only  ;
	%put NOTE: Adjusting CCN length - will throw warning message ;
	data work.&pac._curr_qrt (compress = yes) ;
		length prvdr_num $6. ; 
		format prvdr_num $6. ;
		set wkbn.&in_curr ; 
		keep &varlist ; 
	run ; 

 

%put NOTE: import active provider file ; 
	data work.active_prvdr_curr_raw  ;
	
		infile "&active_path"
		delimiter='09'x 
		missover 
		dsd 
		firstobs=1 ;
	
		informat CCN $6. ;
		informat facility_name $100. ; 
		informat address_line_1 $100. ; 
		informat address_line_2 $100. ;
		informat city $56. ; 
		informat state $4.  ; 
		informat zip_code $5. ; 
		informat county_name	$56. ; 
    	informat phone_number $10. ; 
    	informat ownership $10. ; 
    	informat certification_date yymmdd10.; 
    	informat provider_category_code $4. ;  
    	informat provider_category_description $50.  ; 
    	informat provider_category_subtype_code $4. ;
    	informat provider_category_subtype_desc $50. ;   

		format CCN $6. ;
		format facility_name $100. ; 
		format address_line_1 $100. ; 
		format address_line_2 $100. ;
		format city $56. ; 
		format state $4.  ; 
		format zip_code $5. ; 
		format county_name	$56. ; 
    	format phone_number $10. ; 
    	format ownership $10. ; 
    	format certification_date date9. ;
		format total_number_of_beds $4. ; 
    	format provider_category_code $4. ;  
    	format provider_category_description $50.  ; 
    	format provider_category_subtype_code $4. ;
    	format provider_category_subtype_desc $50. ; 

		input
		   	ccn $
    		facility_name $
    		address_line_1 $
    		address_line_2	$
    		city $
    		state $
    		zip_code $
    		county_name	$
    		phone_number $
    		ownership $
    		certification_date
			total_number_of_beds $
    		provider_category_code $
    		provider_category_description $
    		provider_category_subtype_code $
    		provider_category_subtype_desc $
			;

	run;

	data active_prvdr_curr_clean ; 
		
		set active_prvdr_curr_raw ; 
		
		keep 	ccn
				facility_name
				certification_date
				;
	
		rename certification_date = active_OPD ; 

	run ; 

%put NOTE: import additional files that will be needed later ; 
%put NOTE: import pos file ; 
	* pos file is cleaned upstream and can be read in directly without cleaning ;   
	data work.pos_clean (compress = no); 
		set posbn.&in_POS  ; 
	run ;  
	 
		*QA the above for num records and that variables are in correct format ; 
		title "Checking table pos_clean" ;
		title2 "num obs was 3177 for previous versions" ; 
		title3 "num obs is 3177  for 2022 version" ;
		 
		proc sql ;
			select count(*) as num_obs
				from work.pos_clean
			;
		quit ;  

		title2 "ssa variables should be char" ; 
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

%put NOTE: ***** COMBINE DATA SETS for provider files; 

%put NOTE: combine previous and current CDR provider data ; 
 

title "QA the active provider file for missing OPD" ;
proc sql ; 
	select n(ccn) as num_irfs_total,
		n(active_OPD) as num_with_OPD,
		nmiss(active_OPD) as num_miss_OPD
	from active_prvdr_curr_clean ; 
quit ; 

%put NOTE: combining active provider file with CURRENT CDR extract ; 

proc sort data = &pac._curr_qrt
			out = &pac._curr_qrt_sort ; 
	by prvdr_num ; 
run ; 

proc sort data = active_prvdr_curr_clean
			out = active_prvdr_curr_clean_sort ; 
	by ccn ; 
run ; 

data &pac._curr_active_combined_raw ; 

	merge &pac._curr_qrt_sort (rename= (prvdr_num = ccn) 
							in=in_cdr)
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
	select count(*) as num_irfs_total
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
	title3 "num in active provider file not in cdr" ; 
	select count(*) as num_miss_cdr
	from &pac._curr_active_combined_raw
	where cdr_prvdr = 0
			and
			act_prvdr = 1
	;
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
	;
quit ; 
title ; 

title "Investigate providers missing opd from active file but on both lists" ; 
title2 "They should all be subunits - this was a known problem" ; 
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
	
	if missing(historical_OPD)
		and
		act_prvdr = 0
	then historical_OPD = current_OPD ; 

	drop 
			act_prvdr
			cdr_prvdr
			;
run ; 

title "verify providers only missing historical opd are subunits" ;
title2 "those providers will be assigned the default tie in date" ;   
proc sql ;  
	select ccn,
			fac_name
	from &pac._curr_active_combined_clean
	where missing(historical_OPD)
	;
quit ; 
title; 

 
%put NOTE: calculating tiein ; 
	* tie in specfications can be found in criteria ;  
	data work.&pac._tiein (compress = no) ; 
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

	title "QA of tie in calculations -- table &pac._tiein" ;
	title2 "num obs stil same as input" ;  
	proc sql ; 
		select count(*) as num_obs 
			from work.&pac._tiein ; 
	quit ;

	title2 "historical_opd_plus30 and tiein should be formatted date 9." ; 
	proc sql ; 
		select name,
				type,
				format
			from dictionary.columns
			where libname = "WORK"
				and
				memname = upcase("&pac._tiein")
				and	
				index(format, "DATE") > 0
		;
	quit ; 
	
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
 

	title2	"QA -- Check to make sure day of Tie In is always 1";
	proc sql;
		select	distinct day(tieIn) as tieIn_Day_of_month
		from	work.&pac._tiein
		;
	quit;

	title2	"QA -- Month of tie in by month of OPD";
	title3	"Should follow the rule that tie in date is the first day of the next quarter unless OPD is within 30 days of the start of the new quarter";
	title4 "Where not missing hist opd" ; 

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

	title2	"QA -- Make sure year of Tie in is always either the same as OPD or the next year";
	title3	"If quarter of OPD is 1 or 2, delta year should be zero.";
	title4	"If quarter of OPD is 3, delta year may be 0 or 1.";
	title5	"If quarter of OPD is 4, delta year should be 1.";
	proc sql;
		select	distinct
				qtr(historical_opd) as Historic_OPD_Qtr,
				year(tieIn) - year(historical_opd) as Delta_Year
		from	work.&pac._tiein
		where not missing(historical_OPD)
		;
	quit;
	
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

title;

%put NOTE: code removed related to combining all sets ; 
%put NOTE: above methods keep all relevant and needed variables ; 


	title	"QA of clean dataset -- checking table &pac._tiein" ; 
	title2 	"num obs should be about same as curr qrt extract" ; 
	
	proc sql ;
		select count(*) as num_obs 
		from &pac._tiein
		;
	quit; 

	title2 "all needed variables should be present - except cbsa" ; 
	proc sql ;
		select name
		from dictionary.columns
		where libname = "WORK"
				and
				memname = upcase("&pac._tiein")
		;
	quit ; 

	* QA current OPD and historical determinations ;

	PROC FORMAT ;
		VALUE $misschar
			'' = 'Missing'
			other = 'Not missing' ;
		VALUE missnumb
			. = 'Missing'
			other = 'Not missing' ;
	RUN ;


	TITLE2 "Check Current and historical OPD and caads_dt - missings for OPD OK" ;

	PROC FREQ	DATA=&pac._tiein
				NLEVELS ;
		TABLE	caads_dt
				current_OPD
				historical_OPD
				/MISSING
							NOCOL ;
		FORMAT	caads_dt
				current_OPD
				historical_OPD
				missnumb. ;

	RUN ;
	
	title2 "there should be no missing tiein" ;
	proc freq data=&pac._tiein nlevels ; 
		table tiein / missing noprint ; 
		format tiein missnum. ; 
	run ; 
	
	TITLE2 "Spot check: dates display formatted" ; 

	PROC PRINT DATA=&pac._tiein (obs=5) ;
	    
	    VAR ccn 
	        current_OPD
			historical_OPD 
			tiein
			;

	RUN ;
	title ; 

%put NOTE: ***** COMBINE with extra data - POS file ; 
	data work.dates_pos (compress = yes) ; 
		length cbsa_urbn_rrl_ind $3 ; 
		if _N_ = 1 then 
			do ; 
				declare hash lookup(dataset:"work.pos_clean") ; 
				lookup.definekey("ssa_state_cd", "ssa_cnty_cd") ; 
				lookup.definedata("cbsa_urbn_rrl_ind") ; 
				lookup.definedone() ; 
				call missing(cbsa_urbn_rrl_ind) ; 
			end ; 
		set &pac._tiein ;
		RC = lookup.find() ; 
		drop RC 
			historical_OPD_plus30
			months_to_next_quarter
			next_quarter;  
	run ;

	* we want to see how many are missing cbsa_urbn_rrl_ind ;
	* this number should be monitored quarter to quarter ; 
	* an unexpected rise in this value may indicate a problem with the provider data ; 
	title "Checking table dates_pos" ; 
	title2	"Check number of missing CBSA";
	title3	"This has historically been zero for IRF";
	proc freq data = work.dates_pos ;
		table cbsa_urbn_rrl_ind / missing ;
	run ; 
	title ; 
	* if future quarters suddenly have missing values ; 
	* add in more QA ; 
	* see hospice code for QA steps to add ; 


	* clean up final set ; 
	* create in_scope variable ; 

%put NOTE: IN_SCOPE criteria are &in_scope_criteria ; 
%put NOTE: added upcase functions here for facility name, street address, and city ; 

	data work.&pac._final (compress = yes) ; 
		set work.dates_pos ; 

		if &in_scope_criteria
			then in_scope = 1;
		else if not &in_scope_criteria
			then in_scope = 0;

		fac_name 	= 	upcase(fac_name) ; 
		st_adr		=	upcase(st_adr) ; 
		city_name	=	upcase(city_name) ;
		
	run ; 

	title	"QA of final data set -- &pac._final";
	title2 "Check for missing values.";
	proc means data=work.&pac._final n nmiss min max;
	    var _numeric_;
		title3 "tiein CANNOT have missing values" ; 
		title4 "in_scope should NOT have missing values" ; 
	run;
	proc freq data=work.&pac._final nlevels;
	    tables _character_ / missing noprint;
		title3 "U/R can have missing levels" ;
		title4 "no other char should have missing levels" ; 
		title5 "nlevels of ccn should equal num obs" ;
	run;

	title "how many providers have a correctly formatted ccn" ;
	title2 "pattern checking &ccn_irf  " ; 

	proc sql;
		title3 "Checking final file" ;
		title4 "number correct CCNs" ; 
		select count(*) as num_correct_ccn
		from   work.&pac._final
		where 
				prxmatch("&ccn_irf", ccn) ; 
		title4 "number INcorrect CCNs" ; 
		select count(*) as num_INcorrect_ccn
		from   work.&pac._final
		where 
				~prxmatch("&ccn_irf", ccn) ; 
	quit ;
	title ; 

	title2 "how many providers have an in_scope of 0" ; 
	proc sql;
		title3 "Checking current_extract" ;	
			title4 "number expected classified as out of scope" ; 
		select count(*) as num_expected_OUT_of_scope
			from   &pac._curr_qrt
			where not( &in_scope_criteria
					) ; 
		title3 "Checking table &pac._final" ;
			title4 "number actually classified as out of scope" ; 
		select count(*) as num_OUT_of_scope
			from work.&pac._final  
			where in_scope = 0 ; 
	quit ;

	title2 "how many providers have an in_scope of 1" ; 
	proc sql;
		title3 "Checking current_extract -- &pac._curr_qrt" ;
			title4 "number expected classified as in scope" ; 
		select count(*) as num_expected_IN_scope
			from   &pac._curr_qrt
			where (&in_scope_criteria
					) ; 
		title3 "Checking table &pac._final" ;
			title4 "number actually classified as in scope" ; 
		select count(*) as num_IN_scope
			from work.&pac._final  
			where in_scope = 1 ; 
	quit ;

	title2 "checking creation of in_scope" ; 
	title3 "summary stats for term date by in_scope value" ;
	title4 "in_scope of 1 should have EITHER a missing term date" ; 
	title5 "or a term date >= 01JAN&term_year. " ; 
	title6 "in_scope of 0 should have a term date < 01JAN&term_year. " ; 

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
 

%put NOTE: compare final set to previous quarter assembled file ; 
%put NOTE: changes made to compare code ; 
	title 	"compare the final output for current quarter 	- base
					to the final output of previous quarter - compare " ; 
	title2 "date vars historical_OPD current_OPD and tiein are dropped" ; 
	proc compare    
				base = work.&pac._final (drop = caads_dt
												historical_OPD
												current_OPD
												tiein)
				compare = lib_prev.&in_prev (drop = caads_dt
													historical_OPD
													current_OPD
													tiein)
				listall
				transpose
				;
				id ccn;
	run; 

	title "compare providers that were previously in scope that are now not that were not termed" ; 
	title2 "they should all have a term date after previous reports default term" ; 
	title3 "no output here means no applicable investigation needed" ; 
	proc sql ; 
		select fin.ccn, 
				fin.fac_name,
				fin.historical_OPD,
				fin.current_OPD,
				fin.trmntn_exprtn_dt as curr_term label = "curr_term",
				prv.trmntn_exprtn_dt as prev_term label = "prev_term",
				fin.in_scope
		from &pac._final as fin
				left join
			lib_prev.&in_prev as prv
				on fin.ccn = prv.ccn
		where fin.in_scope = 0
			and prv.in_scope = 1
			and prv.trmntn_exprtn_dt ~= .
			
		;
	quit ; 
	title ; 


%put NOTE: investigate differences below of provider inclusion;

	

	title	"Checking current assembled file against previous";
	title2 "are the providers in curr qrt not in prev qrt new?" ; 
	title3 "check for a recent csp_prvdr_add_dt or a generic one if subunit" ; 
	title4 "a recent orgnl_prctptn_dt would explain as well" ; 
	title5 "checking current quarter's extract" ; 
	proc sql ; 
		select prvdr_num,
				fac_name,
				csp_prvdr_add_dt,
				orgnl_prtcptn_dt
			from wkbn.&in_curr
			where prvdr_num in 	(	select ccn
										from work.&pac._final
										where ccn not in 	(	select ccn
																	from work.&pac._prev_qrt
															)
								)
		;
	quit ; 

	title2 "count of provider in prev Qrt not in current quarter" ; 
	title3 "num_not_in_curr expected to be 0" ; 
	title4 "checking file lib_prev.&in_prev." ; 
	proc sql ; 
		select count(*) as num_not_in_curr
			from lib_prev.&in_prev
			where ccn in 	(	select ccn
									from work.&pac._prev_qrt
									where ccn not in 	(	select ccn
																from work.&pac._final
														)
							)
		;
	quit ; 

title;


 
/*********************** OUTPUT ************************************/
	
%put NOTE: ***** SAVING ; 
	/* save final set to workbench ; 
	* use macro with full final name ;
	* this has the report date built in ;*/ 


	data wkbn.&outfull ; 
		set work.&pac._final ; 
	run ; 


%put NOTE: END active code ; 
