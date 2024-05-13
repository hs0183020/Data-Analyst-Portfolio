/*	This program contains macros used across multiple settings for assessment assembly.

	Created 5/6/2022 -- KAS
	
	Update 5/11/2022 -- KAS
	- Tweaked %deduplicate_asmt so that it will run for all settings
	- Added subset_hist
	
	Update 5/13/2022 -- KAS
	- Added macro that imports assessments
	- parameterized provider number in dedup macro

	Update 5/27/2022 -- EO
	- post production (Q4 APU) to a title that is wrong and gives the impression of a problem
	
	Updated 10/12/2022 -- EO
	- development run for 22Q2 Outreach
		- new potential issue in irf where there are duplicate org assessments in the main table
			that are missing the correction number
			the sort order on the limiting import step might pose a problem in the future
			the submsn_dt sort was moved ahead of crctn_num in by statment
	
	updated 01/04/2023 -- EO
	- development for 22Q3 Outreach
		- no changes
		
	Updated 2/7/2023 -- KAS
	- Added characterization of records dropped

	updates 5/8/2023 -- EO
	- development 22Q4 APU
		- no changes

	updates 7/13/2023 -- CT
	- development 2023-Q1 OR
		- no changes

	updates 8/16/2023 -- CT
	- development 2023-Q1 APU
		- no changes

	updates 10/17/2023 -- SS
	- development 2023-Q2 OR
		- no changes

	updates 11/09/2023 -- SS
	- development 2023-Q2 APU
		- no changes

	updates 12/13/2023 -- EO
	- added code to exclude "X" assessments 
		as well as "I" from historical table and main

	updates 01/09/2024 -- SS
	- development 2023-Q3 OR
		- no changes

	updates 04/12/2024 -- SS
	- development 2023-Q4 OR
		- no changes

	updated 04/12/2024 -- EO
	- changes the deduplicated macro to explicitly call comparison variables needed
		instead of all variables, this was necessitated in SNF due to variable changes
		from Q3 to Q4. Since this happens occassionally, made sense to change.

	updates 05/09/2024 -- HS
	- development 2023-Q4 APU
		- no changes
*/

/*	Macro to import assessments	*/
%MACRO import_asmt (current_asmt = ,
					cutoff_dt = ,
					columns = ,
					submit_dt = ,
					out =,
					drop_out = &out._drop,
					dt_flag = 0 /* Set this to 1 if trgt_dt and submsn_dt are datetime instead of date	*/) ;

	/* Pull most recent records with only the needed columns from assessment extract */
	PROC SORT	DATA = &current_asmt 
				OUT = &out._sort ;
		BY	orgnl_asmt_id
			submsn_dt
			crctn_num
			 ;
	RUN ;
	
	DATA	&out
			&drop_out
			;
		SET	&out._sort		
			%if &dt_flag = 0 %then %do;
				(WHERE =(&submit_dt <= "&cutoff_dt"d))
			%end;
			%else %if &dt_flag = 1 %then %do;
				(WHERE =(DATEPART(&submit_dt) <= "&cutoff_dt"d))
			%end;
			;
		KEEP	&columns ;
		BY	orgnl_asmt_id
			submsn_dt
			crctn_num
			 ;
		%if &dt_flag = 1 %then %do;
			trgt_dt = DATEPART(trgt_dt);
			submsn_dt = DATEPART(submsn_dt) ;
			FORMAT	trgt_dt
					submsn_dt DATE9. ;
		%end;
		IF last.orgnl_asmt_id then output &out;
		else output &drop_out;
	RUN ;
	
	title	"Importing from &current_asmt";
	title2	"N records in source file";
	proc sql;
		select count(*) as source_file_n_records format = comma11.
		from &current_asmt 
		;
	quit;
	
	title2	"N records in output file (&out)";
	proc sql;
		select count(*) as imported_file_n_records format = comma11.
		from &out
		;
	quit;
	
	title2	"N Records dropped due to submission date";
	proc sql;
		select count(*) as submission_too_late format = comma11.
		from &current_asmt
 		%if &dt_flag = 0 %then %do;
				WHERE &submit_dt > "&cutoff_dt"d
			%end;
			%else %if &dt_flag = 1 %then %do;
				WHERE DATEPART(&submit_dt) > "&cutoff_dt"d
			%end;
		;
	quit;
	
	title2	"N Records Dropped Due to Duplication";
	proc sql;
		select count(*) as duplicated_orgnl_asmt_id format = comma11.
		from	&drop_out
		;
	quit;
	
	
	title;
	
%MEND import_asmt ;
	
/*	Macro to deduplicate assessments based upon the prior APU quarter's assembled file	*/
%MACRO deduplicate_asmt (prev_asmt = ,
						curr_asmt = ,
						type = ,
						setting = ,
						pac_table = ,
						prev_quarter =,
						quarter = ,
						varlist_sql =,
						out_file = ,
						ccn_current =,
						ccn_old = ccn,
						prv_internal = prvdr_intrnl_num);
	

	title	"Deduplicating assessments from &curr_asmt that represent modifications from &prev_quarter" ;
	title2	"Count of assessments from &curr_asmt that represent modifications from &prev_quarter" ;
	proc sql ;
		select	count(*) as num_duplicates_&type
		into: num_duplicates_&type
		from	&curr_asmt
			where 	orgnl_asmt_id in(select orgnl_asmt_id from &prev_asmt) ;
	quit ;

	/*	This stretch of code executes duplicate verification and de-duplication logic only executes if there are duplicates 
	*	If there are no duplicates, skips to outputting _clean data */
	%if &&num_duplicates_&type = 0 %then %do ;
		%put	NOTE: No duplicate assessments from &prev_quarter submitted. Code above not run. No need to de-duplicate.  ;
		
		data	&out_file;
			set &curr_asmt;
		run;
		
	%end ;	
	
	%if &&num_duplicates_&type > 0 %then %do ;
		%put NOTE: Duplicated from &prev_quarter found. Deduplicating.	;
	/* create subset of duplicated assesments in current quarter to compare to last quarter */
		proc sql ;
			create table	work.&setting._duplicate_&type._&quarter as
			select	
					&ccn_current,
					orgnl_asmt_id,
					crctn_num, 
					trgt_dt,
					submsn_dt, 		
					&prv_internal
			from	&curr_asmt
			where 	orgnl_asmt_id in(select orgnl_asmt_id from &prev_asmt)
			order by	orgnl_asmt_id,
						submsn_dt ;
		quit ;

	/* pull assessments from previous quarter that appear in current quarter */
		proc sql ;
			create table	work.&setting._duplicate_&type._&prev_quarter as
			select
					&ccn_old,
					orgnl_asmt_id,
					crctn_num, 
					trgt_dt,
					submsn_dt, 		
					&prv_internal
				from	&prev_asmt
					where 	orgnl_asmt_id in(select orgnl_asmt_id from &curr_asmt)
				order by	orgnl_asmt_id,
							submsn_dt ;
		quit ;

	/* characterize the duplicate records - verify that they are modifications */
		proc sql ;
			create table work.&setting._compare_&type._duplicates as
			select	prev.orgnl_asmt_id,
					prev.&ccn_old as prev_ccn,
					prev.crctn_num as prev_crctn_num, 
					prev.trgt_dt as prev_trgt_dt,
					prev.submsn_dt as prev_submit, 		
					prev.&prv_internal as prev_prvdr_intrnl_num,
					curr.&ccn_current as curr_ccn, 
					curr.crctn_num as curr_crctn_num,
					curr.trgt_dt as curr_trgt_dt,
					curr.submsn_dt as curr_submit,		
					curr.&prv_internal as curr_prvdr_intrnl_num					
			from	work.&setting._duplicate_&type._&prev_quarter prev
					inner join
					work.&setting._duplicate_&type._&quarter curr
				on	prev.orgnl_asmt_id = curr.orgnl_asmt_id ;
		quit ;

		title2	"Check assessments from &curr_asmt with doubled asmt_ids --- Confirm they are modifications from &prev_quarter" ;	
		title3	"Records will differ in correction number" ;
		proc freq data = work.&setting._compare_&type._duplicates ;
			tables	prev_crctn_num * curr_crctn_num/nocum nopercent nocol norow ;
		run ;

		title3	"Compare records' provider and patient IDs";
		title4	"Low differences in provider IDs expected." ;
		proc sql ;
			select	count(*) as diff_fac_intrnl_id
			from	work.&setting._compare_&type._duplicates 
				where prev_prvdr_intrnl_num ~= curr_prvdr_intrnl_num ;
			select	count(*) as diff_ccns
			from	work.&setting._compare_&type._duplicates 
				where prev_ccn ~= curr_ccn ;
		quit ;

		/* output --- without duplicates from previous quarter */
		proc sql ;
			create table &out_file as
			select	*
			from	&curr_asmt
				where	orgnl_asmt_id not in (select distinct orgnl_asmt_id from &prev_asmt)
			order by	orgnl_asmt_id,
						crctn_num,
						submsn_dt ;
		quit ;

		/* QA the de-duplication */
		title2	"Check that &report_qrtr. &type. row counts are as expected post de-duplication" ;
		title3 	"base count minus duplicates_removed = count cleaned obs" ;
        proc sql ;
			select count(*) as base_count
			from	&curr_asmt ;
	
			select	count(*) as num_&prev_quarter._duplicates_removed
			from	work.&setting._duplicate_&type._&quarter ;

			select count(*) as count_cleaned_obs 
			from	&out_file ; 
		quit ;

	%end ;
	title ;

		
%MEND deduplicate_asmt ;

/*	Macro to subset the history table to include only 
				1. Records that don't have an original assessment ID in the assessment table
				2. Records that have not been inactivated ;	*/
				
%MACRO subset_hist (hist_table = ,
					asmt_table = ,
					pac_table = ,
					report_qrtr = ,
					quarter = ,
					asmt_setting =,
					out = ,
					ccn =
					) ;

	PROC SQL ;
		CREATE TABLE &out AS
		SELECT	*
		FROM	&hist_table
		WHERE	orgnl_asmt_id NOT IN (	SELECT DISTINCT orgnl_asmt_id 
										FROM &asmt_table)
				AND orgnl_asmt_id NOT IN (	SELECT DISTINCT orgnl_asmt_id 
											FROM &hist_table 
											WHERE &pac_table._crctn_stus_cd IN ("I", "X"))
		ORDER BY	orgnl_asmt_id,
					crctn_num,
					submsn_dt ;
	QUIT ;

	/* QA subsetting of history file */
	title	"Subsetting &report_qrtr historical assessments" ;
	proc sql ;
	
		title2	"Total number of records in the history table";
		select count(*) as base_count
		from	&hist_table ;
			
		title2	"Removed records";
		title3	"Total number of records in history table that are modifications of a record in the current assessment table";
		select	count(*) as num_&quarter._modfications_removed
		into :num_modfications
		from	&hist_table
			where	orgnl_asmt_id in (select distinct orgnl_asmt_id from &asmt_table);

		title3	"Total number of records in history table that are modifications of an inactivated assessment";
		select	count(*) as num_&quarter._inactivs_removed
		into :num_inactivs
		from	&hist_table 
		where	orgnl_asmt_id in (select distinct orgnl_asmt_id from &hist_table where &pac_table._crctn_stus_cd IN ("I", "X"))
				and orgnl_asmt_id not in (select distinct orgnl_asmt_id from &asmt_table) ;

		title3	"Total number of records removed";
		select	distinct (&num_modfications+ &num_inactivs) as total_asmt_removed
		from &hist_table ;

		title2	"Total number of records remaining in historical table";
		select count(*) as count_cleaned 
		/* create a macro for number of historical records (num_hist_asmt)*/
		into	:num_hist_asmt
		from	&out ; 
	quit ;

	
	%if &num_hist_asmt = 0 %then %do ;
		%put NOTE: no assessments from history table. Code below not run ;
	%end ;

	%if &num_hist_asmt > 0 %then %do ;
		title2	"More QA of work.&asmt_setting._hist_&report_qrtr._subset" ;
		title3	"Are there original submissions in the history extracts?" ;
		title4	"Count assessments in  that are NOT in &asmt_table" ;
		title5	"Should match count_cleaned above" ;
		proc sql ;
			select	count(*) as num_hist_unique,
					&pac_table._crctn_stus_cd
			from	&out
			where orgnl_asmt_id not in (select orgnl_asmt_id from &asmt_table)
			group by	&pac_table._crctn_stus_cd	
			;
		quit ;

		/*
		title2	"What is the impact of these records?" ;
		title3	"How many providers and how many additional assesments per provider?" ;
		proc sql ;
			select	hist.&ccn,
					hist.num_hist_asmt,
					asmt.num_curr_asmt,
					(hist.num_hist_asmt + asmt.num_curr_asmt) as num_new_total_asmt	
			from	(select	&ccn,
							count(*) as num_curr_asmt
					from	&asmt_table
					group by &ccn) asmt

					inner join

					(select	&ccn,
							count(*) as num_hist_asmt
					from	&out
					group by &ccn) hist
				on hist.&ccn = asmt.&ccn
			;
		quit ;
		title ;
		*/
	%end ;
	title;
	
%MEND subset_hist ;

/* Macro to QA merge of assessment and history tables */
%macro	check_hist_merge	(table=,
							 ancestor =,
							 history =,
							 pac =)	;
												
	TITLE	"QA -- Check combined Assessment and History Table &table";
	TITLE2	"Number of records in ancestor assessment table &ancestor";	
	PROC SQL;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS Ancestor_Assessments
		INTO :Ancestor_Assess
		FROM &ancestor ;
	QUIT ;

	TITLE2 "Number of records in ancestor history table &history";
	PROC SQL;
		SELECT COUNT(DISTINCT orgnl_asmt_id) AS Ancestor_History
		INTO :Ancestor_History
		FROM	&history ;
	QUIT ;

	TITLE2	"Combined record count";
	PROC SQL;
		SELECT DISTINCT(&ancestor_assess + &ancestor_history) AS Combined_Records
		FROM	&ancestor
		;
	QUIT ;	
	TITLE ;

	/*	Check for duplicate orgnl_asmt_id in final file	*/
	TITLE	"QA -- Check for duplicate original assessment ID in &table";
	PROC SQL;
		SELECT	N_Records,
			COUNT(*) AS Orgnl_asmt_ids
		FROM
			(SELECT	orgnl_asmt_id,
					COUNT(*) AS N_Records
			FROM	&table
			GROUP BY	orgnl_asmt_id)
		GROUP BY N_Records;
	QUIT ;
	TITLE ;

	/* Double check that there are no suspicious drops in records */
	/* Unclear why, but IRF assessments use crctn_cd=O where data dictionary indicates C */
	TITLE "QA &table" ;
	TITLE2 "Frequency of crctn_num and &pac.._crctn_stus_cd are reasonable" ;
	TITLE3 "Should not have inactivated/inactivation request records (I or X)" ;
	TITLE4 "For SNF Any remaining X and I assessments will be eliminated seperately" ; 
	PROC FREQ	DATA = &table ;
		TABLE	crctn_num
				&pac._crctn_stus_cd /NOCUM ;
		TABLE	crctn_num*&pac._crctn_stus_cd /NOCOL NOPERCENT NOCUM ;
	RUN ;
	title ; 
%mEnd check_hist_merge ;

/*	Macro checks to see if there is a hypothetical issue with correction numbers missing in history table and causing sort order problems.	*/
%macro missing_crctn_issue(	lib = lib_asmt,
							table =);

	proc sql;
		create table work.missing_crctn as
		select 	orgnl_asmt_id,
				submsn_dt,
				crctn_num
		from	&lib..&table
		where	crctn_num is null
		;
	quit;
	
	proc sql noprint;
		select count(*)
		into :n_missing_crctn
		from work.missing_crctn
		;
	quit;
		
	%if &n_missing_crctn = 0 %then %do;
		%put NOTE: No cases where an orgnl_asmt_id in history table is missing a correction number. Check will not be run.	;	
	%end;
	
	%else %do;
		title	"For &lib..&table, orgnl_asmt_id missing correction number";
		title2	"Checking for orgnl_asmt_id - submsn_dt pairs in which at least one record is missing correction number";
		proc sql;
			select	Records,
					count(*) as oai_submsn_pairs
			from
				(
				select	orgnl_asmt_id,
						submsn_dt,
						count(*) as Records
				from	lib_asmt.&table
				where orgnl_asmt_id in (select orgnl_asmt_id from work.missing_crctn)
				group by 	orgnl_asmt_id,
							submsn_dt
				)
			group by Records
			;
		quit;
		title;
	%end;
	
%mEnd 	missing_crctn_issue;
