/* LTCH APU assessment procssing only */
			

****************** END HEADER **********************/
%put NOTE: END Header / START Active Code ; 

%put NOTE: Include macros from the APU_Common_Macros program ; 

* workbench paths for workbench inputs AND output storage ; 
* path to main branch workbench --- should not need to be updated; 
%let bench = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting ;
* current quarter's folder --- update every reporting cycle; 
%let q_folder = 2023-Q4 APU ;

filename cmacro "&bench/&q_folder/Support Programs/APU_Common_Macros_20240422.sas";
%include cmacro;

%put NOTE: Set up CONTROL FLAGS ;

***** control flags ; 
* for inclusion of waivers ; 
* set to 1 for APU reporting and 0 for Outreach reporting ; 
%let has_waivers = 0; 

*	Flag to correct the BIMS PHQ Flaw	
	Set to 1 if you need to fix this issue
	;
%let fix_bims_phq = 1;

%put	NOTE: &=fix_bims_phq;

%if &fix_bims_phq = 1 %then %do ; 
	%let fix_start 	= 01OCT2022 ; 
	%let fix_end 	= 30SEP2023 ;
	
	%put NOTE: Fix bims will apply to target dates from &=fix_start to &=fix_end ; 

%end ;  

***** program parameters and libraries ; 
%put NOTE: parameters and libraries ; 	

%let pac = LTCH;

* ccn pattern definition for an LTCH ; 
%let ccn_LTCH = &ccn_pattern_LTCH ; 
%put NOTE: ccn pattern will resolve to &ccn_ltch ; 

%let yrqrt = &cmn_yy.&cmn_qq;
* report type (OR=Outreach vs APU) --- for file naming of output only ;  

%let type = &cmn_cycle; 

%put 	NOTE: Variables set by common macro program:
		&=yrqrt
		&=type
		;
	
* static folder names --- should not need to be updated; 
* asmt data folder ; 
%let asmt_folder = &bench/&q_folder/Extract and Assemble Assessments/Data ;  
* prvdr data folder ; 
%let prvdr_folder = &bench/2023-Q4 OR/Extract and Assemble Providers/Data ; 
* claims data folder ;
%let claims_folder = &bench/2023-Q4 OR/Extract and Tally Claims/Data ;

* setting apu folder ; 
%let apu_folder = &bench/&q_folder/LTCH APU and Summary/Data ; 

* output library ; 
libname out_bn
		"&apu_folder" ; 

* input libraries ; 
libname asmt_bn
		"&asmt_folder" ; 
libname prvdr_bn
		"&prvdr_folder" ;
libname cl_bn
		"&claims_folder" ;

* input file names - update each quarter;
     
*	Input files.	; 
*	This is FULL for APU, OR for OR	(assessment data file);
	%if "&type" = "OR" %then %do ; 
		%let full = OR ; 
	%end ; 
	%else %do ; 
		%let full = full ; 
	%end ; 	

%let in_assess 	= LTCH_ASMT_&FULL._&yrqrt._240424 ;
%let in_prov    = LTCH_PRVDR_&yrqrt._OR_240416 ;
%let in_claims 	= CLM_TALLY_LTCH_&yrqrt._240416 ; 


*	Control file -- stored as an Excel workbook. Common macro should direct to the most recent version by default...	;
* relevant control sheet - should not change ; 
%let in_control_sheet 	= LTCH_Assessment;

*	Outputs	;
%let report_date		= %sysfunc(today(), yymmddn6.);
%let out_apu        	= LTCH_&type._&yrqrt._&report_date;
%let out_asmt 			= LTCH_asmt_comply_&yrqrt._&report_date;
%let out_fails 			= LTCH_asmt_fails_&yrqrt._&report_date;

**** criteria values ;
*	Date range for assessments;
%let report_start = &cmn_report_start;
%let report_end = &cmn_report_end;
%put	NOTE: Report dates assigned using common macros
		&=report_start
		&=report_end
		;

*	Threshold for passing (1 = 100%)
	(This changes rarely if ever);
%let ltch_pass_assess = 0.7945;
%let ltch_pass_nhsn = 1.000;

* default date for tiein to signify OPD  - when reporting not required;
* tiein default is the first day of the quarter following the current reporting period ; 
%let tieindefault = &cmn_tiein_cutoff ;
%put	NOTE: Tie in default set using common macros
		&=tieindefault
		;

***** end program parameters ; 
%put NOTE: END parameters ; 

%put NOTE: Format used throughout program for QA ; 
*	Format, lets you lump all the providers with tie in prior to the exception date into a single group	;
proc format;
	value tie_applies
	low -< "&report_start"d = "Tie in not applicable"
	"&report_start"d - high = [date9.]
	;
	*	Format for reporting days -- we really care about 0 vs not zero for this qa	;
	value	qa_rd
		0 			= "0"
		0 <- high 	= "> 0"
		low -< 0 	= "Oopsie!"
		;
	picture nicepercent
		low - high = "009.99%";
run;

***** import and clean inputs ; 
%put NOTE: Start input loading and processing ; 

%put NOTE: read in and process control table ; 

/* 	import control table using a macro from the common macros program */ 
	%set_control_file   (setting = ltch,
						 ctrl_sheet	= &in_control_sheet ,
						 ctrl_out 	= ltch_control);
	 
	

	* control file QA;
	title	"QA Check -- Make sure that all element types in the control table are correctly classified";
	title2	"We want three freqs: goal and perform and missing. Anything else will cause problems.";
	proc freq	data = work.ltch_control;
				table	element_type / missing 
										nocum 
										nopercent;
	run;
	title;


%put NOTE: Set up arrays of performance and discharge items for logic processing ; 

	*	Macro arrays for control files	;
	*	We need one array for names, one array for element start date, and one array for element end date ;
	*   we also added a check for performance admissions elements, discharge elements separately;
	*   discharge elements can be planned, unplanned, or expired;
	*   thus we need an elements array for admit, goals, planned_dschrg, unplan_dschrg, and expired_dschrg;
	*   we also need a start and end date array for each of the above;
	*	So that's 15 arrays	;
%macro	make_goal_array(	fieldname =,
								macroname =,
								type =,
								source = &pac._control);

	select	&fieldname
			into :&macroname
			separated by " "
	from	&source
	where 	lowcase(element_type) = "%lowcase(&type)"
		
%mEnd make_goal_array;

%macro	make_performance_array(	fieldname =,
								macroname =,
								type =,
								source = &pac._control);

	select	&fieldname
			into :&macroname
			separated by " "
	from	&source
	where 	lowcase(&type) = "yes"
		
%mEnd make_performance_array;

%macro	make_assessment_array_date(	fieldname =,
									macroname =,
									type =,
									source = &pac._control);

	select	catt('"', put(&fieldname, date9.), '"d')
			into :&macroname
			separated by " "
	from	&source
	where 	lowcase(&type) = "yes"
		
%mEnd make_assessment_array_date;	

%macro	make_goal_array_date(	fieldname =,
									macroname =,
									type =,
									source = &pac._control);

	select	catt('"', put(&fieldname, date9.), '"d')
			into :&macroname
			separated by " "
	from	&source
	where 	lowcase(Element_Type) = "%lowcase(&type)"
		
%mEnd make_goal_array_date;

	* new in 20Q3 OR and beyond .... including APU reporting ; 
	* for assessments sources using assessment_iqies database there are different names for measures ;
	* compared to those used previously ;  
	* we have standardized the names between the two sources ; 
	* all arrays (other than dates) below will use the standardized_name ; 
	proc sql noprint;
	     %make_goal_array(	fieldname = Standardized_Name,
							macroname = goal_elements,
							     type = goal)
							;
	     %make_goal_array_date(	fieldname = Element_Active_Start,
							    macroname = goal_start,
									 type = goal)
							;
	     %make_goal_array_date(	fieldname = Element_Active_End,
							    macroname = goal_end,
									 type = goal)
							;
	     %make_performance_array( fieldname = Standardized_Name,
							      macroname = admit_elements,
							           type = required_admit)
							;
         %make_assessment_array_date( fieldname = Element_Active_Start,
									  macroname = admit_start,
									       type = required_admit)
							;							
	     %make_assessment_array_date( fieldname = Element_Active_End,
									  macroname = admit_end,
									       type = required_admit)
							;
         %make_performance_array( fieldname = Standardized_Name,
							      macroname = plan_dschrg_elements,
							           type = required_plan_dschrg)
							;
         %make_assessment_array_date( fieldname = Element_Active_Start,
									  macroname = plan_dschrg_start,
									       type = required_plan_dschrg)
							;							
	     %make_assessment_array_date( fieldname = Element_Active_End,
									  macroname = plan_dschrg_end,
									       type = required_plan_dschrg)
							;
         %make_performance_array( fieldname = Standardized_Name,
							      macroname = unplan_dschrg_elements,
							           type = required_unplan_dschrg)
							;
	     %make_assessment_array_date( fieldname = Element_Active_Start,
									  macroname = unplan_dschrg_start,
									       type = required_unplan_dschrg)
							;							
	     %make_assessment_array_date( fieldname = Element_Active_End,
									  macroname = unplan_dschrg_end,
									       type = required_unplan_dschrg)
							;
         %make_performance_array( fieldname = Standardized_Name,
							      macroname = expired_dschrg_elements,
							           type = required_expired_dschrg)
							;
	     %make_assessment_array_date( fieldname = Element_Active_Start,
									  macroname = expired_dschrg_start,
									       type = required_expired_dschrg)
							;							
	     %make_assessment_array_date( fieldname = Element_Active_End,
									  macroname = expired_dschrg_end,
									       type = required_expired_dschrg)
                            ;
	quit;

	* assign length to a macro variable ; 
	* this will be used in determining the length of array ; 
	* the data step that uses these just uses the "XXXX_elements" to assign length to each sub- arrays ; 
	%let admit_length = %sysfunc(countw(&admit_elements));
	%let plan_dschrg_length = %sysfunc(countw(&plan_dschrg_elements));
	%let unplan_dschrg_length = %sysfunc(countw(&unplan_dschrg_elements));
	%let expired_dschrg_length = %sysfunc(countw(&expired_dschrg_elements));
	%let goal_length = %sysfunc(countw(&goal_elements));
    

	%put &=admit_length;
	%put &=plan_dschrg_length;
	%put &=unplan_dschrg_length;
	%put &=expired_dschrg_length;
	%put &=goal_length;
	%put NOTE: &goal_elements;
	%put NOTE: &admit_elements;
	%put NOTE: &plan_dschrg_elements;
	%put NOTE: &unplan_dschrg_elements;
	%put NOTE: &expired_dschrg_elements;

	* end processing control file;
%put NOTE: End processing control file;


%put NOTE: Load Current Assessments for processing from workbench to work ;
* write assembled assessment file to work ; 	
* grab tie in from the provider file for new denominator logic ; 

PROC SQL ;
		CREATE TABLE work.&pac._asmt_raw AS
		select 	asmt.*,
				prv.tieIn,
			    prv.in_scope
		from	asmt_bn.&in_assess	asmt
					inner join
				prvdr_bn.&in_prov as prv
					on asmt.ccn = prv.ccn
			;
QUIT ;

title "file &pac._asmt_raw " ; 
title2 "min should be near &report_start and max should be near &report_end"  ; 
proc sql;
	select	
			min(trgt_dt) as min_trgt_dt format = date9.,
			max(trgt_dt) as max_trgt_dt format = date9.
	from	work.&pac._asmt_raw
	
	;
quit;
title ; 

	
********* process assessments base cases ; 
%put NOTE: process assessments base cases ;

data work.&pac._assessment_compliant;

	set &pac._asmt_raw end = eof; 
	
		*	Determine extent to which assessment contributes to the denominator;
		*	(In the case of LTCH, this is extremely easy -- all assessments contribute 1 to the denominator!)	;
		
	***** new denom logic added ; 
		* Determine Denominator: Denom=1 if after tieIn, otherwise 0 ;
		if tieIn < "&tieindefault"d then ltch_asmt_denom=1 ;
		else ltch_asmt_denom=0;

	*Hash table to capture perofrmance fields that are noncompliant	;
	*call missing(a0250_rsn_for_asmt_cd); /*This call statement is not needed for LTCH**/
	length fail_field $32;
	drop fail_field;
	if _n_ = 1 then do;
	
		declare hash failures();
		rc = failures.defineKey(	"orgnl_asmt_id", 
									"fail_field");
		rc = failures.defineData(	"ccn",
									"orgnl_asmt_id", 
									"a0250_rsn_for_asmt_cd", 
									"quarter",
									"trgt_dt", 
									"fail_field");
		rc = failures.defineDone();
		
	end;

	* open a do loop for numer if denom logic is met ; 
	if ltch_asmt_denom then do;

		*	Logic for the BIMS PHQ issue	;
		if &fix_bims_phq = 1 then do;

				/* now correcting for all records per CMS decision */
				/*	Instructions from Acumen:
				When C0400A_1=[-] AND C0400B_1=[-] AND C0400C_1=[-], treat dashes (“-“) in these items as skips (“^”) or another valid response.*/
				/* per CMS decision use "or" logic instead of and */
				if	c0400a_rcall_first_word_cd = "-"
					and "&fix_start"d <= trgt_dt <= "&fix_end"d
				then 
					c0400a_rcall_first_word_cd = "^";

				if	 c0400b_rcall_scnd_word_cd = "-"
					and "&fix_start"d <= trgt_dt <= "&fix_end"d
				then 
					c0400b_rcall_scnd_word_cd = "^";

				if	c0400c_rcall_thrd_word_cd = "-"
					and "&fix_start"d <= trgt_dt <= "&fix_end"d
				then 
					c0400c_rcall_thrd_word_cd = "^";
		end;/* end fix bims etc */

		*	Determine compliance over performance measures;
		*   there are performance measures for admits and discharges;
		*   performance measures for discharge vary based on type of discharge - all discharges checked for some - only complete stay checked for others;
		*	A dash indicates failure to comply. Any non-dash -- including the skip pattern, ^ -- is compliant	;
	
		length 	admit_check 
				plan_dschrg_check
				unplan_dschrg_check
				expired_dschrg_check
				goal_check
				$500
				;
		*	Arrays for all admit items -- field name, start date, end date	;
		IF a0250_rsn_for_asmt_cd = "01" then do;
	          array admit_elements{&admit_length} &admit_elements;
	          array admit_start{&admit_length} _temporary_ (&admit_start);
	          array admit_end{&admit_length} _temporary_ (&admit_end);
	
	         *	Loop over all admit  elements. Add element's value to admit_check if the element's active date includes the assessment's
		        trgt_dt	;
	         *	We use admit_check to create a concatenated field containing all of the elements reviewed. This is for ease of QA.
		        The determination as to whether the assessment passes or not is determined by the value in the admit_dash field	;
	         admit_dash = 0;
	         do admt = 1 to &admit_length;
		        if admit_start[admt] <= trgt_dt <= admit_end[admt] then do;
			       admit_check = catt(admit_check, admit_elements[admt]);
			       if admit_elements[admt] = "-" then DO;
							admit_dash = admit_dash + 1 ;
							*	Add the field to the failures hash table	;
							fail_field = vname(admit_elements[admt]);
							rc = failures.add();
					END;
		        end;
	         end;
	         if admit_dash > 0 then admit_pass = 0;
	         else admit_pass = 1;
		end ; /* end for admissions */
		/* now do goals */
		IF a0250_rsn_for_asmt_cd = "01" then do;
             *	Determine compliance over goal measures. At least one goal must be non-dash. We also don't give credit for skip patterns, here.	;
	         *	Assemble check field -- process is similar to process for performance	;
	         array	goal_elements{&goal_length} &goal_elements;
	         array	goal_start{&goal_length} _temporary_ (&goal_start);
	         array	goal_end{&goal_length}	_temporary_ (&goal_end);

	         goal_non_dash = 0 ;

	         do goal = 1 to &goal_length;
		        if goal_start[goal] <= trgt_dt <= goal_end[goal] then do;
			       goal_check = catt(goal_check, goal_elements[goal]);
			       if goal_elements[goal] not in("-", "^") then do ; 
						goal_non_dash = (goal_non_dash + 1);
					end ; 
		        end ; 
			end ; 
				if ~("01JAN2023"d<= trgt_dt <= "30SEP2023"d) then do;
					goal_non_dash = . ;
				end;
			 if goal_non_dash = . then goal_pass = . ;
             else if goal_non_dash > 0  then goal_pass = 1;
	         else if goal_non_dash = 0 then do;
				goal_pass = 0;
				fail_field = "Goal_Fail" ;
				rc = failures.add();
			 end;
		end;/* end for admissions goals */

		*	Array for plan_discharges items -- field name, reuse start data and end date from above	;
		IF a0250_rsn_for_asmt_cd = "10" then do;
	        array plan_dschrg_elements {&plan_dschrg_length} &plan_dschrg_elements;
	        array plan_dschrg_start{&plan_dschrg_length} _temporary_ (&plan_dschrg_start);
	        array plan_dschrg_end{&plan_dschrg_length} _temporary_ (&plan_dschrg_end);
	
	        *	Loop over plan_dschrg  elements. Add element's value to plan_dschrg_check if the element's active date includes the assessment's
		        trgt_dt	;
	        *	We use plan_dschrg_check to create a concatenated field containing all of the elements reviewed. This is for ease of QA.
		        The determination as to whether the assessment passes or not is determined by the value in the all_dschrg_dash field	;
	        plan_dschrg_dash = 0;
	        do plds = 1 to &plan_dschrg_length;
		       if plan_dschrg_start[plds] <= trgt_dt <= plan_dschrg_end[plds] then do;
			      plan_dschrg_check = catt(plan_dschrg_check, plan_dschrg_elements[plds]);
			      if plan_dschrg_elements[plds] = "-" then DO;
							plan_dschrg_dash = plan_dschrg_dash + 1;
							*	Add the field to the failures hash table	;
							fail_field = vname(plan_dschrg_elements[plds]);
							rc = failures.add();
				  END;
		       end;
	        end;
	        if plan_dschrg_dash > 0 then plan_dschrg_pass = 0;
	        else plan_dschrg_pass = 1;
		end;/* end for planned discharge */

		*	Array for unplan_discharges items -- field name, reuse start date and end date from above	;
		IF a0250_rsn_for_asmt_cd = "11" then do;
	        array unplan_dschrg_elements {&unplan_dschrg_length} &unplan_dschrg_elements;
	        array unplan_dschrg_start{&unplan_dschrg_length} _temporary_ (&unplan_dschrg_start);
	        array unplan_dschrg_end{&unplan_dschrg_length} _temporary_ (&unplan_dschrg_end);
	
	        *	Loop over unplan_dschrg  elements. Add element's value to unplan_dschrg_check if the element's active date includes the assessment's
		        trgt_dt	;
	        *	We use unplan_dschrg_check to create a concatenated field containing all of the elements reviewed. This is for ease of QA.
		        The determination as to whether the assessment passes or not is determined by the value in the all_dschrg_dash field	;
	        unplan_dschrg_dash = 0;
	        do upds = 1 to &unplan_dschrg_length;
		       if unplan_dschrg_start[upds] <= trgt_dt <= unplan_dschrg_end[upds] then do;
			      unplan_dschrg_check = catt(unplan_dschrg_check, unplan_dschrg_elements[upds]);
			      if unplan_dschrg_elements[upds] = "-" then DO;
							unplan_dschrg_dash = unplan_dschrg_dash + 1;
							*	Add the field to the failures hash table	;
							fail_field = vname(unplan_dschrg_elements[upds]);
							rc = failures.add();
				  END;
		       end;
	        end;
	        if unplan_dschrg_dash > 0 then unplan_dschrg_pass = 0;
	        else unplan_dschrg_pass = 1;
		end;/* end unplanned discharge */

		*	Array for expired_discharges items -- field name, reuse start data and end date from above	;
		IF a0250_rsn_for_asmt_cd = "12" then do;
	        array expired_dschrg_elements {&expired_dschrg_length} &expired_dschrg_elements;
	        array expired_dschrg_start{&expired_dschrg_length} _temporary_ (&expired_dschrg_start);
	        array expired_dschrg_end{&expired_dschrg_length} _temporary_ (&expired_dschrg_end);
	
	        *	Loop over expired_dschrg  elements. Add element's value to expired_dschrg_check if the element's active date includes the assessment's
		        trgt_dt	;
	        *	We use expired_dschrg_check to create a concatenated field containing all of the elements reviewed. This is for ease of QA.
		        The determination as to whether the assessment passes or not is determined by the value in the expired_dschrg_dash field	;
	        expired_dschrg_dash = 0;
	        do exds = 1 to &expired_dschrg_length;
		       if expired_dschrg_start[exds] <= trgt_dt <= expired_dschrg_end[exds] then do;
			      expired_dschrg_check = catt(expired_dschrg_check, expired_dschrg_elements[exds]);
			      if expired_dschrg_elements[exds] = "-" then DO;
							expired_dschrg_dash = expired_dschrg_dash + 1;
							*	Add the field to the failures hash table	;
							fail_field = vname(expired_dschrg_elements[exds]);
							rc = failures.add();
				  END;
		       end;
	        end;
	        if expired_dschrg_dash > 0 then expired_dschrg_pass = 0;
	        else expired_dschrg_pass = 1;
		end;/* end expired discharge */
	
		*	for admit If the assessment passes performance AND goal, it's a pass!;
		* performance matters whether the record is a admit, plan_dschrg, unplan_dschrg, or expired_dschrg;
		*numer for admissions;
		if a0250_rsn_for_asmt_cd = "01"
                   AND admit_pass in (1, . )
                   AND goal_pass in (1, . )
            then LTCH_asmt_numer = 1;

		*numer for planned discharge;
		else if a0250_rsn_for_asmt_cd = "10" 
                   AND plan_dschrg_pass = 1
            then LTCH_asmt_numer = 1;

		*numer for unplan discharge;
		else if a0250_rsn_for_asmt_cd = "11" 
                   AND unplan_dschrg_pass = 1
            then LTCH_asmt_numer = 1;  

		*numer for expired discharge;
		else if a0250_rsn_for_asmt_cd = "12" 
                   AND expired_dschrg_pass = 1
            then LTCH_asmt_numer = 1;  
		else LTCH_asmt_numer = 0;

	* end if denom logic ; 
	end ; 	

	*	Output the failures hash table to file	;
	if eof then	rc = failures.output(dataset:"work.ltch_assessment_failures");

		*	Clean up	;
		drop    admt
				goal	
				plds
				upds
				exds
				rc
			     ;
run;

 
	** QA Assessments-Level Processing Logic ;
%put NOTE:	QA Assessments-Level Processing Logic;

	title	"QA -- Assessment-level processing -- work.&pac._assessment_compliant";
	title2	"Denominator value versus Tie In";
	proc freq	data = work.&pac._assessment_compliant;
		format	tieIn tie_applies.;
		table	tieIn * ltch_asmt_denom / missing 
											nocum 
											nocol 
											nopercent 
											norow;
	run;
	title;

	proc format;
		value	$rsn_for_asmt
			"01" = "Admission (includes Admission and Goals)"
			"10" = "Planned Discharge"
			"11" = "Unplanned Discharge"
			"12" = "Expired Discharge"
			;
	run;
	
	title2	"Diagnostic -- Breakdown of failure points";
	title3	"Only relevant pass conditions should be evaluated as 0/1. All others should be missing.";
	proc tabulate	data = work.&pac._assessment_compliant;
		class	a0250_rsn_for_asmt_cd
				admit_pass
				plan_dschrg_pass
				unplan_dschrg_pass
				expired_dschrg_pass
				goal_pass
				ltch_asmt_numer
				/
				missing
				;
		format	a0250_rsn_for_asmt_cd	$rsn_for_asmt.
				;
		table	a0250_rsn_for_asmt_cd
				,
				ltch_asmt_numer
				,
				admit_pass * N = ""
				plan_dschrg_pass * N = ""
				unplan_dschrg_pass * N = ""
				expired_dschrg_pass * N = ""
				goal_pass * N = ""
				;
		where	ltch_asmt_denom > 0;
	run;
	title;

%if &fix_bims_phq = 1 and "&type" = "APU" %then %do;
	%put NOTE: Checks for BIMS and PHQ correction logic	;
	title	"QA: Checking outcomes of BIMS and PHQ correction logic";
	
	
	proc sql;
		title2	"Initial number of assessments with C0400A-C Admit All Dashes - by quarter";
		select count(*) as initial_all_fail, 
				a0250_rsn_for_asmt_cd,
				quarter
		from	work.&pac._asmt_raw
		where	
				(c0400a_rcall_first_word_cd = "-"
				or c0400b_rcall_scnd_word_cd = "-"
				or c0400c_rcall_thrd_word_cd = "-")
				and tieIn < "&tieindefault"d
		group by a0250_rsn_for_asmt_cd,
				quarter
				;
		title2	"Number of assessments with C0400A-C Admit All Dashes After Fix Applied - by quarter";
		select count(*) as corrected_all_fail,
				quarter
		from	work.&pac._assessment_compliant
		where	(c0400a_rcall_first_word_cd = "-"
				or c0400b_rcall_scnd_word_cd = "-"
				or c0400c_rcall_thrd_word_cd = "-")
				and ltch_asmt_denom > 0
		group by quarter
		;
	quit;

	title;
	
%end;
	
%put NOTE: added code related to goal fail - there should be no Goal gails 2023-Q4 and beyond ; 

title "Checking failure reasons in data - work.ltch_assessment_failures " ; 
title2 "There should be no Goal related Failures for 2023-Q4 and beyond" ; 
proc sql ; 
	select count(*) as num_Goal_Fails,
		quarter
	from work.ltch_assessment_failures
	where Fail_field = "Goal_Fail" 
	group by quarter
	;
quit ; 
title ; 



********	
	!!!!!!!!!!!!!!!!
	Output	;
%put NOTE: write final file to workbench ; 

* macro out_apu used to name to workbench because it includes report date;

data out_bn.&out_fails;
	set work.ltch_assessment_failures;
run;

data	out_bn.&out_asmt;
	set	work.&pac._assessment_compliant;
run;

/**************************************END ACTIVE CODE ***************************************************/	

%put NOTE: END ACTIVE CODE ; 
