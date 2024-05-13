/*
    purpose:
		This program scrapes the relevant data from FEMA websites and outputs
		a sas data file with the relevant state and counties.

        Following a FEMA declared Natural Disaster, counties eligible for public assistance under
		PA-A and PA-B in a given area (state specific) are exempt from APU reporting
		for a specific period of time.

        The data are listed on the FEMA websites - following the pattern below:
            https://www.fema.gov/disaster/XXXX/designated-areas

		This program accesses the FEMA designated areas website(s) and obtains the applicable counties

	input(s):
			parameter table (tab delimited):
				Column 1: waiver name (for each applicable disaster declaration)
				Column 2: state abbreviation (for each applicable disaster area)
				Column 3: FEMA id number (per url)
	output(s):
		<file_name>_<waiver-reason>_<yr-qrt>_<report-date>.sas7bdat
		

	updates:
		12/27/2021 - initial commit
		01/12/2022 - added matching on regular expressions and macro to loop over states
		01/13/2022 - added merge, sort, deduplicate logic, and comments
		01/14/2022 - modified comments
		01/19/2022 - modified comments and added variable and static parameter sections
		01/25/2022 - modified i/o logic, parameters, comments
		01/26/2022 - modified i/o logic, parameters, comments
				   - removed leading whitespace from counties column
				   - modified output file name to include waiver reason, report cycle (YYQQ), and run date (YYMMDD)
		02/01/2022 - added code to initialize work.combined before appending work.parse to it
				   - changed comment styles to follow SOP guidelines
				   - added data sets to get_exempt_counties() for debugging
				   - perform garbage collection on additional data sets
				   - added QA to show tally of records by state for pre/post deduplication
		02/02/2022 - refactor get_exempt_counties() macro to accepts state, FEMA disaster id, and waiver as parameters
		02/03/2022 - created flags and macros for export, garbage collection, and inline QA steps
			       - moved inline QA to execute before export
				   - moved deduplicate, sort, and reorder steps outside the do loop in main()
				   - removed redundant getvarc funtion calls
				   - changed output from an Excel workbook to a SAS data set
				   - added parameter table to QA
				   - added "file_name" to avoid overwriting "output_filename"
		02/07/2022 - updated for 2021-Q3 development cycle
				   - changed %let qrt = Q2 to %let qrt = Q3
				   - changed %let garbage_collection_flag = 0 for debugging purposes
				   
		03/08/2022	- Updated for 2021-Q4 OR pre-development
					- Updated parameters

		05/09/2022 	- Updated parameters for 2021-Q4 APU Development
		(Chid)		
		05/25/2022	- (EO) ran for 2021-Q4 production

		11/04/2022	-	development for 2022-Q2 APU reporting
					-	extensive updates to header
					-	stripped some control flag structures, mostly in "garbage collection," possibly others
					-	updated deduplication method used, with the intention 
						that this program can be used for multiple waivers with potentially overlapping states
						was a proc sort dedup with all, changed to explicitly list var order of interest
					-	stripped some parameters to make the nameing more flexible
					-	changed bench location of output to be waivers subfolder
					-	post run change
						a county name in a memo has a hyphen and the scrapper is 
						dropping everything after it which is F ing the downstream logic
					-	added tag to interim datasets, it was reusing data set names
						and it was hard to find problems

		11/23/2022	-	production run 22q2 apu

		02/08/2023	-	development for 22Q3 APU
					-	no substantive changes

		02/21/2023  -   (SS) production run 22Q3 APU

		05/05/2023	-	(EO) development for 22Q4 ILS / 23Q1 Hospice
					-	Added MS tornado waiver DR 4697

		05/22/2023  -   (SS) production run 22Q4 ILS / 23Q1 Hospice

		08/24/2023  -   (SS) production run 23Q1 ILS / 23Q2 Hospice
					-   Added Hawaii Fires Waiver and removed FY24 Waivers.

		11/14/2023  -   (SS) development run 23Q2 ILS / 23Q3 Hospice

		11/29/2023  -   (CT) production run 2023-Q2 APU ILS / 2023-Q3 APU Hospice

		02/13/2024	-	(EO) dev run 23Q3 ILS

		03/14/2024	-	(SS) prod run 23Q3 ILS

		05/08/2024	-	(HS) prod run 23Q4 ILS

*/

%put NOTE: Start of variable and static parameter section ;

/* variable program parameters */
%let year = 2023; 
%let qrt = Q4; 
%let cycle = APU;
%let file_name = wvr_cty;

/* flag parameters */

%let export_flag = 1;
%let inline_qa_flag = 1;

/* create tab-delimited parameter table */
/* col 1: waiver name */
/* col 2: state abbreviation */
/* col 3: FEMA id number (per url) */
data work.parameter_table;
	length
		waiver $256
        state $2
        fema_id $4
	;
    infile datalines
        dlm = '09'x;  /* sets the delimiter as a tab */
    input waiver $
		state $
		fema_id $
	;		
    datalines;
Fires_HI	HI	4724
;
run;

/* static program parameters */
%let bench_path = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting;
%let subfolder = Assemble Waivers/Data;

/* workbench logic */
%let cycle_folder = &year-&qrt &cycle;
%let report_date = %sysfunc(today() , yymmdd6.);
%let yrqrt = %sysfunc(cats(%substr(&year, 3, 2), &qrt));
%let output_filename = &file_name._FEMA_&yrqrt._&report_date;

libname wkbn "&bench_path/&cycle_folder/&subfolder";

/* create file reference */
filename resp temp;

%put NOTE: End of variable and static parameter section;


/* get exempt counties for individual state and individual waiver type */
%macro get_exempt_counties(waiver=, state=, fema_id=);
	%let url = "https://www.fema.gov/disaster/&fema_id/designated-areas";

	/* get the html response from FEMA website */
	proc http
		method="GET"
		url=&url
		out=resp;
	run;

	/* put html response in dataset */
	data work.exempt_raw_&waiver;
		infile resp length=len lrecl=32767;
		input line $varying32767. len;
		linenum = _n_;
	run;

	/* create dataset that only contains lines with appropriate counties */
	data work.parse_&waiver;
		set work.exempt_raw_&waiver;
		where prxmatch("\<p><b>PA-[AB]</b></p>\", line);
		rx_li = prxparse("/(<li>)?([A-Za-z-.\(\)]\s*)+(<\\li>)?/");  /* match list tags */
		length county $50;
		start = 1;
		stop = length(line);
		call prxnext(rx_li, start, stop, line, position, length);
		do while (position > 0);
			state = symget('state');
			waiver = symget('waiver');
			county = substr(line, position, length);
			/* only collect lines with list tag */
			if index(county, '<li>') > 0 then
				do;
					is_county = 1;  /* debug */
					/* strip leading blanks, trailing blanks and multiple blanks */
					county = strip(compbl(prxchange('s/<[^>]*>/ /', -1, county)));  
					output;
				end;
			else is_county = 0;  /* debug */
			call prxnext(rx_li, start, stop, line, position, length);
		end;
	run;

	/* keep only state, county, and waiver columns; */
	data work.state_county_&waiver;
		set work.parse_&waiver(keep = state county waiver);
	run;
%mend get_exempt_counties;


/* output all states appended state, county, waiver dataset */
%macro export_data;
	
	data wkbn.&output_filename;
		set work.waivers; 
	run ;
%mend export_data;


%macro inline_qa;
	/* inline qa: show parameter table */
	title "Parameter Table"; 
	proc print data = work.parameter_table;
	run;

	/* inline qa: show total number of counties, including duplicates */
	title "Total Number of Counties (Including Duplicates)"; 
	proc sql;
		select count(*) as total_counties_pre_dedup
		from work.combined;
	quit;

	/* inline qa: show total number of deduplicated counties */
	title "Total Number of Counties (Deduplicated)"; 
	title2 "Expecting total_counties_post_dedup <= total_counties_pre_dedup"; 
	proc sql;
		select count(*) as total_counties_post_dedup
		from work.waivers;
	quit;

	/* inline qa: show total number of counties, including duplicates, grouped by state */
	title "Total Number of Counties Grouped by State and Waiver (Including Duplicates)"; 
	proc sql;
		select waiver, 
				state, 
				count(*) as num_counties_pre_dedup
		from work.combined
		group by state, waiver
		order by state asc;
	quit;

	/* inline qa: show total number of deduplicated counties, grouped by state */
	title "Total Number of Counties Grouped by State and Waiver (Deduplicated)";
	title2 "Expecting num_counties_post_dedup <= num_counties_pre_dedup";
	proc sql;
		select waiver, 
				state, 
				count(*) as num_counties_post_dedup
		from work.waivers
		group by state, waiver
		order by state asc;
	quit;
%mend inline_qa;


%macro main(parameter_table=, export_ind=, inline_qa_ind=);
	%let dsid = %sysfunc(open(&parameter_table));  /* open file */
	%let records = %sysfunc(attrn(&dsid, nobs));  /* get record count */
	%syscall set(dsid);  /* use syscall set to set up macro variables with same names as columns in the parameter table */

	/* loop through each record in the parameter table */
	%do loop = 1 %to &records;
		%let rc = %sysfunc(fetchobs(&dsid, &loop)); /* fetch observations for current record */

		%get_exempt_counties(waiver=&waiver, state=&state, fema_id=&fema_id);

		/* append work.combined if it exists and initialize it if it doesn't exist */
		%if %sysfunc(exist(work.combined)) %then
			%do;
				/* append individual states data set to that of all states */
				proc append base=work.combined data=work.state_county_&waiver;
				run;
			%end;
		%else
			%do;
				/* initialize work.combined */
				data work.combined;
					set work.state_county_&waiver;
				run;
			%end;
	%end;

	/* remove duplicate records */
	proc sort data=work.combined out=work.dedup nodupkey;
		by 	waiver
			state
			county
			;
	run;
	
	/* sort dataset by state */
	proc sort data=work.dedup out=work.sort_by_state;
		by state;
	run;

	/* reorder columns by (state, county, waiver) */
	data work.waivers;
		retain state county waiver;
		set work.sort_by_state;
	run;

	%let dsid = %sysfunc(close(&dsid));  /* close file */


	/*** conditional macro execution by indicator flags ***/

	/* conduct inline QA if idicated */
	%if &inline_qa_ind = 0 %then
		%do;
			%put %sysfunc(dequote("NOTE: Not conducting QA"));
		%end;
	%else
		%do;
			%put %sysfunc(dequote("NOTE: Conducting QA"));
			%inline_qa;  /* conduct inline QA */
		%end;

	/* export data if indicated */
	%if &export_ind = 0 %then
		%do;
			%put %sysfunc(dequote("NOTE: Not exporting data"));
		%end;
	%else
		%do;
			%put %sysfunc(dequote("NOTE: Exporting data"));
			%export_data; /* export all states appended state, county, waiver dataset to .xlsx file */
		%end;

	
%mEnd main;


%main(
	parameter_table=work.parameter_table,
	export_ind=&export_flag,
	inline_qa_ind=&inline_qa_flag);