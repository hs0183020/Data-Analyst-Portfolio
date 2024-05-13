/******************* HEADER ***********************
	PROGRAM: 	Hospice Provider Assembly
	CREATED BY:	Elizabeth Oliver (EO)


	PURPOSE:	This program combines provider files used in the most recent 
				APU reporting cycle as part of a larger process to produce APU Analytics.

	INPUTS:		This program has four (4) inputs.
				Hospice_prvdr_2xQx_21xxxx (previous quarter's assembled provider file)
					Previous Quarter's assembled provider file can be found on the workbench associated with that reporting cycle
				hospice_prvdr_xtrct_21XXXX (current quarter's extract - raw provider data)
					Provider XTRCT files can be found on the workbench
				Source_file_name_2XQX_XXXX.xlxl Sheet = Hosp_Provider (current quarter's control table)
					Sourced in workbench 
				cy202xposurbanrural - POS Dec 2020 update - now stored as a SAS data set
					Souced in workbench path:
					/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/Static Files

	METHOD:		This program contains the following subsections and methods.
				MACROS: 		assign libraries and define macro variables and parameters.
				IMPORTS: 		Import all data files needed and perform and cleaning necessary.
				COMBINING:		Combine all data sets, keep only fields needed to calculate historical OPD
				COMBINE:		Combine the historical OPD set with the current quarters extract
								the original participation date from the current quarters extract will be the new current_OPD	
				EXTRADATA:		Combine with POS file to get urban rural indicator using the POS table as a hash object
				COMPARE:		Compare results file to previous quarter's assembled file
				SAVING:			Write the final table to the workbench

	
	New for 21Q2 Hospice:	
				The inputs for this program have been simplified.
				We will use the assembled provider file from the previous quarter
				and the extract from the current quarter. We will use the combined file to 
				calculate the historical_OPD (oldest orgnl_prtcptn_dt on record). The current 
				extract will be used to find the current_OPD (orgnl_prtcptn_dt).
				Methods used previously for finding cbsa_rbn_rrl_ind remain the same. 


				We will no longer reduce the assembled provider file based on termination date.
				This will now be handled in downstream programming. There is a flag variable to denote inclusion in
				report based on termination date. 
				In_scope will be 1 when the provider's termination date is either missing 
				or the date falls outside of exclusion criteria. 
				In scope will be 0 when the provider's termination date
				is covered by exclusion criteria.

				This program also does not remove providers based on newness.

	TO UPDATE:	To update this program the macro variables at the top will need to be updated extensively.
		

	RECORD OF current quarter's UPDATES:
		07/30/2021	-	creation of program for Hospice Q2 apu reporting
		08/04/2021	-	used LTCH program as a base for updates to new method
					-	changed pac ref to hospice, removed all tiein logic and notes
					-	added newness criteria to in_scope variable criteria and QA
					-	changed QA to accomodate noise in in scope criteria
		08/05/2021	-	changed prvdr_intrnl_num to char to protect leading zero and to align with previous fix
		08/19/2021	-	production input updated
		11/5/2021	-	Updated for development 2021-Q2 APU
						Updated parameters
						Tweaked program comments
						Tweaked titles for QA and some QA steps


		11/19/2021  - Production run for 2021 Q2 APU

		02/25/2022	-	pre-development for 2021 Q4 Hospice APU
					-	updated parameters 
					-	added more notes to the log
		03/04/2022	-	production inputs


		05/5/2022	-	EO
					-	development for 2022 Q1 Hospice APU
					-	development parameters
					-	added common macro for importing control file
					-	added common macro for ccn pattern checks
					-	fixed length of ccn to eliminate errors in downsteam programming
					-	cleaned up commenting 
					-	cleaned up QA
		05/23/2022	-	EO
					-	prod inputs

		08/03/2022 (Chid) - Developement Run for 2022 Q2 Hospice APU
					  - additional use of common macros program in the parameter section
		08/18/2022 (EO) - prod inputs

		11/03/2022 (EO) - development for 2022-Q3 Hospice APU
					-	added an upcase function to all demographic fields that end up used down the line
						(facility name, street address, and city)
						it was noted after the 21Q4 report that some new providers were in the source database
						with lower case or prop case names and cities, etc.
						to standardize, we will upcase all fields where this is needed
					-	referenced ccn pattern to a local macro from the common
						body of program now references local
		11/3/2022 KAS - Revision by reviewer
					- We have 14 providers that are missing OPD. Our current logic considers these to be in-scope. We believe they probably should be treated as
					out-of-scope. Minor change made to ensure this.
		11/22/2022 KAS - Production for 2022-Q3 Hospice APU

		03/07/2023 EO - production for 2022-Q4 Hospice APU
					-	moved missing urban rural check to after final file
						so that it could be filtered for in scope only

		05/05/2023 SS - Development run for 2023-Q1 Hospice APU

		05/08/2023 SS - TERM_YEAR was incorrectly triggering as ILS Term Year.
                        Updated to use Hospice TERM Year.

		05/19/2023 EO - prod run for 23Q1 Hospice APU

		08/16/2023 SS - Dev updates for 23Q2 Hospice APU.
                      - Overhauled code to use Active Provide Files instead of Historical.

		08/21/2023 SS - prod run for 23Q2 Hospice APU.

		08/22/2023 CT - production run for 2023-Q2 APU (2023-Q1 ILS)
					  - replaced current OPD to historical OPD in the in_scope_criteria macro
						(Quarterly Active Provider file is the source of truth)

		08/30/2023 CT - production run for 2023-Q2 APU (2023-Q1 ILS) -- no re-run done
					  - fixed a note in the parameters section (noted from Liz's review)

		11/8/2023 (CT) - Development Run for 2023-Q3 APU (2023-Q2 ILS)
					    (provider extracts copied from Q2 APU (Q1 APU ILS) folder due to CDR refresh issue)

		11/22/2023 (CT) -   Production Run for 2023-Q3 APU (2023-Q2 ILS)
						-	used the October 2023 Quarterly Active Provider Files
						-   Added format for prvdr_num to avoid warning

		11/28/2023 (CT) -   Production re-run for 2023-Q3 APU (2023-Q2 ILS)
						- 	used a manual edited version (removed extra space from the row of CCN A11656) 
							of the October 2023 Quarterly Active Provider File

		01/08/2024 (HS) -   Development Run for 2023-Q4 APU (2023-Q3 ILS)

		01/09/2024 (HS) -   Development Run for 2023-Q4 APU (2023-Q3 ILS)
						-   Re-run using December 2023 Quarterly AP(Active Provider) File
						-   Corrections done on AP file line 5513 (CCN A11656), removed extra tab after address
						-   Corrections done on AP file line 2413 (CCN 341605), removed extra tab after address

		03/07/2024 (HS) -   Prod Run for Hospice 2023-Q4 APU

		05/08/2024 (CT) -   Development Run for 2024-Q1 APU (2023-Q4 APU ILS)
						-   Use March 2024 Quarterly Active Provider File
******************* END HEADER **************************/
%put NOTE: end header / start parameters ; 

***** MACROS AND PARAMETERS ;
%put NOTE: MACROS AND PARAMETERS ;
%let folder	= 2023-Q4 APU ;
%let cmn_prgm_dt = 20240422 ;

* common macro set up ; 
filename cmacro "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder/Support Programs/APU_Common_Macros_&cmn_prgm_dt..sas";
%include cmacro;

	* setting ; 
	%let pac 	 = Hospice ;
	%let short_pac = Hosp ; 
	* type should always be apu for hospice ;  
	%let type	 = APU ; 
	%let qtr = &cmn_q_hosp;
	%put NOTE: &=qtr;
	* report year ; 
	%let rpt_year = &cmn_yyyy_hosp ;
	%put NOTE: &=rpt_year;

	%let yy = &cmn_yy_hosp;
	%put NOTE: &=yy;
	%let qrtyr	 = &cmn_yy_hosp.&cmn_qq_hosp ;
	%put NOTE: &=qrtyr;
	
	* libnames etc ; 
	* general path to workbench -- should not change ; 
	%LET bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ; 
	* current quarter specific folder -- should not change (it's updated in common macro program); 
	%let q_folder = &cmn_folder ;
	%put NOTE: &=q_folder; 
	*current quarter's workbench path for data (input and output) --- should not change; 
	%let wkbn_path = &bench/&q_folder/Extract and Assemble Providers/Data ; 

	* path to active provider files ; 
	%let active_month = 2024_03 ; 
	%let active_dir = &bench/Active Provider Files/Quarterly Files/&active_month ; 
	%let active_file = 112689976_Active_HOSPC_032024.txt ; 
	%let active_path = &active_dir/&active_file ;

	* previous quarter's folder --- UPDATE EVERY REPORTING CYCLE;
************* Need to update this parameter manually ***********; 
	%let p_folder = 2023-Q4 Hospice; 
	*previous quarter's workbench path --- should not change;
	%let prev_path = &bench/&p_folder/Extract and Assemble Providers/Data ;
	* workbench path for new pos file  --- should not change; 
	%LET POS_dir = &bench/Static Files ;

	*libraries ; 
	* current quarter's path ; 
	libname wkbn
			"&wkbn_path" ; 
	* previous quarter's path ; 
	libname lib_prev
			"&prev_path" ; 
	* pos file path ; 
	libname posbn
			"&POS_dir" ;


	* input file information ;
************* Need to update these two parameters manually ***********; 
	* previous quarter's asembled provider file name ; 
	%let in_prev 	=	lib_prev.hospice_prvdr_23q4_apu_240307 ; 

	* current quarter's extract  ;
	%let in_curr   =  HOSPC_PRVDR_XTRCT_240502 ;  
	 

	* control table ; 
	* make certain this path references the most recent control table available ; 
	
	* path and table are referenced in common - just need sheet name here ; 
	* sheet name - should not change ; 
	%let c_sheet = &short_pac._Provider ; 


	* references for POS file ;
	*	This generally only updates once a year -- typically during Q3 or Q4 reporting	;
	%LET in_POS = pos_2022_ur_byssa_20230125 ;
	
	* output tools ; 

	* report date assignment ; 
	%let report_date = %sysfunc(today() , yymmddn6.) ; 
	
	*final output naming ; 
	%let outfull = &pac._prvdr_&qrtyr._&type._&report_date ; 

%put NOTE: parameters summary ;
 
	%put NOTE: &pac report will run for &qrtyr ; 

	%put NOTE: it will use the current extract of &in_curr ; 

	%put NOTE: and the previous assembled file of &in_prev -- the immediate prior assembled file for &pac ; 

	%put NOTE: and the POS file of &in_POS ; 

	%put NOTE: and a control file of with sheet &c_sheet ; 

    %put NOTE: Historical OPD/Tie in will derive from the active provider file &active_file received in &active_month ;

	%put NOTE: output will be named &outfull ; 

%put NOTE: fixed report values ; 


	* dynamically create default date to flag termed providers ;  
	%let term_year = &cmn_term_year_hosp ; 
	%put NOTE: &=term_year;
 
	
	%let in_scope_criteria = 
			%str((missing(trmntn_exprtn_dt)
				OR
			trmntn_exprtn_dt >= "01JAN&term_year."d)
				AND
			historical_OPD < "01APR&rpt_year."d
				AND
			~missing(historical_OPD)) ; 

	%put NOTE: in scope should be any provider without a term date or a term date after 01JAN&term_year. AND a historical_OPD < 01APR&rpt_year ;
	%put NOTE: in scope criteria resolves to  &in_scope_criteria ;

	%let ccn_hosp = &ccn_pattern_hosp ; 
	%put NOTE: ccn pattern to assign locally using common definition ; 
	%put NOTE: resolves to &=ccn_hosp ;

***** END PROGRAM PARAMETERS ; 
%put NOTE: end program parameters ; 

%put NOTE: ***** IMPORT DATA ; 

%put NOTE: import and clean control file ; 
	/* call import macro from common file */
	%set_control_file(
							setting= Hospice,
							ctrl_sheet= &c_sheet,
							ctrl_out = work.&pac._control);

	%put NOTE: create a macro of variable names needed ; 
	* the control file contains the list of variable from the raw extract we wish to keep ;
	* use proc sql to create a space delimited list into a macro variable ; 
	* this will be used in the import steps to subset to the desired variables ; 
	title	"QA of imported control file";
	title2	 "Vars listed should be same as source_file_name in control file" ;
	proc sql ;
	     select Source_File_Name
	     into :varlist separated by " "
	     from work.&pac._control
	     ;
	quit;
	
	title;
	
	* QA check that the macro varlist contains the same values as the source_file_name column in the control file;
	%PUT NOTE: are these separated by a space... &=varlist;


%put NOTE: import previous quarter assembled file;
	data work.&pac._prev_qrt ; 
		set &in_prev (keep = 			ccn 
										historical_OPD 
										current_opd 
										caads_dt) ;
		rename historical_opd = orgnl_prtcptn_dt ;
	run ; 	

%put NOTE: import current quarter extract ; 
    %put NOTE: correcting length of ccn - will throw a warning ;
	data work.&pac._curr_qrt_raw ; 
		length prvdr_num $6. ;
		format prvdr_num $6. ;
		set wkbn.&in_curr ; 
		keep &varlist ; 
	run ; 
	
	title	"QA -- Check provider number (CCN) formatting in this quarter's raw data";
	title2	"Check distinct CCN length and whether CCN matches the Hospice CCN pattern (&ccn_hosp)";
	title3	"CCN length should be 6 and pattern match should be 1";
	title4	"Assuming this check passes, there's a warning coming up in the log about 'multiple lengths specified for prvdr_num'";
	title5	"which you will be able to safely ignore";
	proc sql;
		select 	distinct
				lengthc(strip(prvdr_num)) as prvdr_num_Length,
				prxmatch("&ccn_hosp", prvdr_num) as CCN_Pattern_Match
		from	work.&pac._curr_qrt_raw
		;
	quit;
	title;
		
	
%put NOTE: fix attributes of prvdr_num and prvdr_intrnl_num ; 
	%put NOTE: If the QA check on provider number length and format passed, you can ignore the warning this step may throw...;
	data work.&pac._curr_qrt ; 

		LENGTH prvdr_num $ 6;


		set &pac._curr_qrt_raw ; 
	
		prvdr_intrnl_num_fix = PUT(prvdr_intrnl_num, 10.) ;
		
		drop prvdr_intrnl_num 
			; 

		rename 	prvdr_num = ccn
				prvdr_intrnl_num_fix = prvdr_intrnl_num 
				; 

	run ; 

%put NOTE: import active provider file ; 
	data work.active_prvdr_curr_raw  ;
	
		infile "&active_path"
		delimiter='09'x 
		missover 
		dsd 
		firstobs=2 ;
	
		informat CCN $6. ;
		informat facility_id $7.;
		informat facility_name $100. ; 
		informat city $56. ; 
		informat state $4.  ; 
		informat zip_code $5. ;
		informat address_line_1 $100. ; 
   	    informat phone_number $10. ; 
		informat county_name	$56. ; 
    	informat ownership $10. ; 
    	informat certification_date mmddyy10. ;
 

		format CCN $6. ;
		format facility_id $7.;
		format facility_name $100. ; 
		format city $56. ; 
		format state $4.  ; 
		format zip_code $5. ;
		format address_line_1 $100. ; 
   	    format phone_number $10. ; 
		format county_name	$56. ; 
    	format ownership $10. ; 
    	format certification_date date9. ;

		input
		   	ccn $
			facility_id $
    		facility_name $
    		city $
    		state $
    		zip_code $
    		address_line_1 $
			phone_number $
    		county_name	$
    		ownership $
    		certification_date
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
	 
	 
	data work.pos_clean (compress = no); 
		set posbn.&in_POS; 
		 
	run ; 
	 
	
		*QA the above  that variables are in correct format ; 
		title	"Checking import of POS table";
		
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

	

%put NOTE: ***** COMBINE CURRENT AND EXISTING PROVIDER DATA SETS ; 
	***** we will use the combined data set to find historical_opd 
			- the oldest orgnl_prtcptn_dt on file ; 
	* we keep only the two needed variables for finding historical_opd plus caads_dt for QA purposes; 
		
  
	data work.&pac._prev_curr_raw (compress = no);
   		set work.&pac._curr_qrt
			work.&pac._prev_qrt
			;
		 
		keep 	ccn
				orgnl_prtcptn_dt
				caads_dt /*we don't really need caads_dt here - it is for QA only */
				;
	run;

	title	"QA -- Combine current and previous quarters' files -- table &pac._prev_curr_raw" ;
	
	title2 "num obs should be reasonable for setting &pac."  ;
	proc sql ; 
			select count(*) as num_obs
				from work.&pac._prev_curr_raw
			; 
	quit;

	title2 "check that the var type is numeric for the two date variables";
	title3 "all two dates should be displayed" ; 
	title4 "format of dates should be date9" ; 
	proc sql ; 
		select name,
				format,
				type
			from dictionary.columns
			where libname = "WORK"
				and	
				memname = upcase("&pac._prev_curr_raw")
				and
				type = "num"
		;
	quit ; 

	
	proc freq data = work.&pac._prev_curr_raw;
	   		tables caads_dt;
            title2 "all caads dates in scope of data should be present" ;
	run;

	* check for missing values in ccn and original opd ; 
	proc sql ; 
		title2 "check for missing values in ccn and opd" ; 
		title3 "num_miss_ccn should be 0" ; 
		title4 "num_miss_opd should be close 0" ;
		select 	nmiss(ccn) as num_miss_ccn,
				nmiss(orgnl_prtcptn_dt) as num_miss_opd
			from &pac._prev_curr_raw
		;
	quit  ; 
	
	title ; 

%put NOTE: ***** DATA MANUPULATION ; 
	* we can now calculate historical_OPD ;

	* calculating historical OPD ; 
	* to find historical OPD we find the minimum orgnl_prtcptn_dt on file in all records for each provider ; 
	proc sql;
	    create table work.&pac._historical as
	    select    ccn,
                  min(orgnl_prtcptn_dt) as historical_OPD format = date9.
	    	from      work.&pac._prev_curr_raw
	    	group by ccn
	    ;
	quit;

	title	"QA -- Historical OPD calculation -- table &pac._historical" ;
	title2 	"num obs should be reasonable for setting - should be close to current extract" ;
	proc sql;
		select count(*) as num_obs
			from  work.&pac._historical
		;
	quit ; 

	title2 "historical_OPD should be formatted date9." ;

	proc sql;
		select 	name,
				type,
				format
			from dictionary.COLUMNS
			where	libname = "WORK"
							and memname = upcase("&pac._historical")	
					;
	quit;
	
	* check for duplicate ccn ; 
	proc sql ; 
		title2	"Make sure that there are no duplicate providers" ;
		title3	"n_prvdrs should be 1 and n_ccn should be obs number " ;
		SELECT	n_prvdrs,
				COUNT(*) AS n_ccn
			FROM
				(SELECT	ccn,
					COUNT(*) AS n_prvdrs
				FROM 	work.&pac._historical
				GROUP BY ccn)
			GROUP BY n_prvdrs 
		;
	quit ; 
	
	title ;

**15AUG2023 - Add  Active File OPD Related Checks**;

%put NOTE: compare previous methods of calculating historical OPD to active provider file OPD ; 

title "Quick Count of providers whose calculated historical OPD differs from Active provider file OPD" ; 
title2 "limited to providers that have not terminated" ; 
proc sql ; 
	select count(*) as num_diff_OPD
	from &pac._historical as full
		left join
		&pac._curr_qrt as curr
		on full.ccn = curr.ccn
		left join
		active_prvdr_curr_clean as act
		on full.ccn = act.ccn
		
	where full.historical_OPD ~= act.active_OPD
			and 
		missing(trmntn_exprtn_dt); 
quit ;

title ; 

%put NOTE: end all code related to previous historical OPD capture ; 

title "QA the active provider file for missing OPD" ;
proc sql ; 
	select n(ccn) as num_hospice_total,
		n(active_OPD) as num_with_OPD,
		nmiss(active_OPD) as num_miss_OPD
	from active_prvdr_curr_clean ; 
quit ; 

%put NOTE: ***** COMBINE Current OPD and Active Provider File ;
	* combine current_OPD (most current extract) and Active Provider sets ; 
	* the set &pac._curr_qrt and active_prvdr_curr_clean can be combined after sorting; 
	
	proc sort data = work.&pac._curr_qrt 
				out = work.current_sort ; 
		by ccn ; 
	run ; 
	
	proc sort data = active_prvdr_curr_clean
				out = active_prvdr_curr_clean_sort ; 
		by ccn ; 
	run ; 
	
	* keep only the two needed fields from the dates data set ; 
	* the original participation date from the current data set will be renamed as current_OPD ; 
	data &pac._curr_active_combined_raw ; 

		merge work.current_sort (in=in_cdr)
				active_prvdr_curr_clean_sort (keep = ccn 
											active_OPD 
										in=in_act)
				;
		by ccn  ;

		cdr_prvdr = in_cdr ; 
		act_prvdr = in_act ; 
		rename orgnl_prtcptn_dt = current_OPD ;
	run ;  

	title	"QA -- Check merge of current and Active Provider files";
	title2 "num obs should be reasonable for setting" ;
	proc sql ;
		select count(*) as num_obs 
			from &pac._curr_active_combined_raw
		;
	quit; 

	title2 "all needed variables should be present - except cbsa (this comes from the POS file)" ; 
	proc sql ;
		select name
			from dictionary.columns
			where libname = "WORK"
				and
				memname = upcase("&pac._curr_active_combined_raw")
		;
	quit ; 

	* QA current OPD and Active Provider OPD ;

	PROC FORMAT ;
		VALUE $misschar
			'' = 'Missing'
			other = 'Not missing' ;
		VALUE missnumb
			. = 'Missing'
			other = 'Not missing' ;
	RUN ;

	
	TITLE2 "Check Current and Active OPD and caads_dt - missings OK" ;
	PROC FREQ	DATA=work.&pac._curr_active_combined_raw 
				NLEVELS ;
		TABLE	caads_dt
				current_OPD
				active_OPD
				/MISSING
				NOCOL ;
		FORMAT	caads_dt
				current_OPD
				active_OPD
				missnumb. ;
	RUN ;
	
	TITLE2 "Spot check: dates display formatted" ;	
	PROC PRINT DATA=work.&pac._curr_active_combined_raw (obs=5) ;
	    
	    VAR ccn 
	        current_OPD
			active_OPD 
			
			;
		 
	RUN ;
	
	title ; 

title "QA combining data" ; 
title2 "providers in one list and not other" ; 
proc sql ; 
	select count(*) as num_hospices_total
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
title;

title "Investigate providers missing from Active Provider File that have not termed" ; 
proc sql  ; 
	select ccn,
			fac_name,
			current_OPD
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
title2 "There should be no output for Hospice" ; 
proc sql ; 
	select ccn,
			fac_name,
			current_OPD,
			active_opd,
			trmntn_exprtn_dt
	from &pac._curr_active_combined_raw 
	where missing(current_OPD)
			and
			missing(active_OPD)
			and 
			act_prvdr = 1
	;
quit ; 
title ; 

%put NOTE: clean up var names of curr active combined raw ; 
%put NOTE: a step here has been added to capture the original participation date from CDR if the provider is not on active provider file ; 
%put NOTE: that will need to be removed or amended based on decision of COR ; 

data &pac._curr_active_combined_cln ; 

	set &pac._curr_active_combined_raw (rename =
				(active_OPD = historical_OPD 
				/*orgnl_prtcptn_dt = current_OPD*/)) ;
	
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
title3 "that may need to change if CMS decision does not align" ;  
proc sql ;  
	select ccn,
			fac_name
	from &pac._curr_active_combined_cln
	where missing(historical_OPD)
	;
quit ; 
title; 

%put NOTE: ***** COMBINE with POS file ; 
	data work.dates_pos (compress = yes) ; 
		length cbsa_urbn_rrl_ind $3 ; 
		if _N_ = 1 then 
			do ; 
				declare hash lookup(dataset:"work.pos_clean") ; 
				lookup.definekey("ssa_state_cd","ssa_cnty_cd") ; 
				lookup.definedata("cbsa_urbn_rrl_ind") ; 
				lookup.definedone() ; 
				call missing(cbsa_urbn_rrl_ind) ; 
			end ; 
		set work.&pac._curr_active_combined_cln ;
		RC = lookup.find() ; 
		drop RC ;  
	run ; 




%put NOTE: prep final set ; 
%put NOTE:	***** Create in_scope variable ; 
%put NOTE: in scope criteria is evaluated as &in_scope_criteria ;
%put NOTE: upcase function to standardize naming patterns in source data ;  

	data work.&pac._final (compress = yes) ; 
		set work.dates_pos ; 
		
		fac_name 	= 	upcase(fac_name) ; 
		st_adr		=	upcase(st_adr) ; 
		city_name	=	upcase(city_name) ;


		if &in_scope_criteria
			then in_scope = 1;
		else in_scope = 0;

	run ; 

	title	"QA -- Create In_Scope Variable -- table &pac._final" ;
	title2 "Check for missing values.";

	proc means data=work.&pac._final n nmiss min max;
	    var _numeric_;
		title3 "in_scope should NOT have missing values" ;
		title4 "other num vars can have missing values" ;  
	run;
	proc freq data=work.&pac._final nlevels;
	    tables _character_ / missing noprint;
		title3 "U/R can have missing levels" ;
		title4 "no other char should have missing levels" ; 
		title5 "nlevels of ccn should equal num obs" ;
	run;

	* We want to see how many are missing cbsa_urbn_rrl_ind ;
	* this number should be monitored quarter to quarter ; 
	* an unexpected rise in this value may indicate a problem with the provider data ; 
	title	"QA -- Combined provider data with POS file -- table &pac._final";
	title2 "num missing Urban Rural in usually around 3-4" ; 
	title3 "if num miss urban Rural increases greatly further investigation may be needed" ; 
	proc sql ;
		select count(*) as num_miss_urbanRural
			from work.&pac._final 
			where missing(cbsa_urbn_rrl_ind)
					and in_scope = 1
		;
	quit  ; 
	
	title ; 

	title2 "how many providers have an in_scope of 0" ; 
	proc sql;
		title3 "Checking current_extract" ;	
		title4 "number expected classified as out of scope" ; 
		select count(*) as num_expected_OUT_of_scope
			from   &pac._curr_qrt
			where (( not missing(trmntn_exprtn_dt)
						AND	
					trmntn_exprtn_dt < "01JAN&term_year."d)
						OR
					orgnl_prtcptn_dt >= "01APR&rpt_year."d
						OR
					missing(orgnl_prtcptn_dt))
				;  
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
			where ((missing(trmntn_exprtn_dt)
						OR
					trmntn_exprtn_dt >= "01JAN&term_year."d)
						AND 
					orgnl_prtcptn_dt < "01APR&rpt_year."d
						and
					~missing(orgnl_prtcptn_dt)
					) 
				; 
		title3 "Checking table &pac._final" ;
		title4 "number actually classified as in scope" ; 
		select count(*) as num_IN_scope
			from work.&pac._final  
			where in_scope = 1 ; 
	quit ;


	title2 "summary stats for term date and current_opd by in_scope value" ;
	title3 "in_scope of 1 should have EITHER a missing term date" ; 
	title4 "or a term date >= 01JAN&term_year. " ;
	title5 "or a current_opd < 01APR&rpt_year. "; 
	title6 "in scope of 0 is checked more later" ; 
	title8 "checking table &pac._final" ; 
	proc tabulate data = work.&pac._final ; 
		class in_scope
			   
				/ missing;
		var trmntn_exprtn_dt
			current_opd	
				; 
		table in_scope ,
				trmntn_exprtn_dt * (n nmiss)
				trmntn_exprtn_dt * (min max) *f=date9.
				current_opd * (n nmiss)
				current_opd * (min max) *f=date9.
				;
		 		 
	run ; 

	proc sql ; 
		title2 "number of providers with in_scope of 0 who meet for newness" ; 
		select count(*) as num_newness_met
			from &pac._final
			where in_scope = 0 
					and 
					trmntn_exprtn_dt = . 
		; 
		title2 "min of current opd for those providers" ;
		title3 "min should be >= 01APR&rpt_year." ; 
		select min(current_opd) as min_opd_newness format = date9.
			from &pac._final
			where in_scope = 0 
					and 
					trmntn_exprtn_dt = . 
		;
	quit ; 

	proc sql ; 
		title2 "number of providers with in_scope of 0 who meet for term date" ; 
		select count(*) as num_termed_met
			from &pac._final
			where in_scope = 0 
					and 
					not missing(trmntn_exprtn_dt) 
		; 
		title2 "max of term date for those providers" ;
		title3 "max should be < 01JAN&term_year." ; 
		select max(trmntn_exprtn_dt) as max_opd_termed format = date9.
			from &pac._final
			where in_scope = 0 
					and 
					not missing(trmntn_exprtn_dt) 
		;
	quit ; 

	%put NOTE: **** Compare final results to previous quarters assembled file ; 
	
	
	title "compare the final output for current quarter - base
				to the final output of previous quarter - compare" ; 
	title2 "note to new reviewers --- this should raise lots of flags" ; 
	title3	"Contractors upstream from us are currently cleaning up iQIES data.";
	title4	"It is not unreasonable to see a lot of new providers or changes values -- name, location, termination date, etc.";
	proc compare    
				base = work.&pac._final (drop = caads_dt)
				compare = &in_prev (drop = caads_dt)
				listall
				transpose
				maxprint = (1000, 1000)
				;
				id ccn;
	run;

	title;
	
	title2 "are the providers in curr qrt not in prev qrt new?" ; 
	title3 "check for a recent csp_prvdr_add_dt or a generic one or a current_opd that excluded them from last year's report (>01APRpreviousyear)" ; 
	title4 "checking file current quarter's extract" ; 
	proc sql ; 
		select 	prvdr_num,
				csp_prvdr_add_dt,
				orgnl_prtcptn_dt
			from wkbn.&in_curr
			where prvdr_num in (select ccn
									from work.&pac._final
									where ccn not in 	(	select ccn
																from work.&pac._prev_qrt
														)
									AND in_scope = 1
								)
			order by year(orgnl_prtcptn_dt),
					month(orgnl_prtcptn_dt)
		;
	quit ; 

	title2 "Count of providers in prev_qrt not in curr_qrt" ;  
	proc sql ; 
		select count(*) as num_miss 
									from work.&pac._prev_qrt
									where ccn not in 	(	select ccn
																from work.&pac._final
														)
		;
	quit ; 
	

************* SAVING ******************************* ; 
%put NOTE: save final set to workbench ; 

	* use macro with full final name ;
	* this has the report date built in ; 

	data wkbn.&outfull ; 
		set work.&pac._final ; 
	run ; 


%put NOTE: end active code ; 
