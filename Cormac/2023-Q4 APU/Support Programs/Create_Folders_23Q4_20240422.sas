/*	Program creates directory structure for the quarterly reporting, including subfolders.

	Run once at the beginning of development. It shouldn't hurt to re-run, but it shouldn't need to be re-run, either.
	In the event that you need to add further folders after initial creation, you're probably better adding them on the
	fly manually. You can then update the program for future quarterly reporting.
	
	There isn't any in-line QA -- QA is to check for the creation of the folders specified. Proof is in the pudding.
	
	Created 6/23/2021 -- KAS
	

Updates to program:

	Updated 7/28/2021 -- 	Updated for 2021 Q1 APU. Changed the root folder, dropped the Documentation folder since we weren't using it,
							and only included outputs for APU + Summary folders since these aren't used elsewhere. Also, added folder for
							Hospice.
							
	Updated 9/9/2021 -- Updated for 2021 Q2 OR.
	
	Updated 11/2/2021 -- Updated for 2021 Q2 APU
							
		-- KAS

	updated 12/15/2021 (EO) --- updated for 2021 Q3 OR
			-	added a subfolder for support programs
			-	added note about top-level folder existence requirements
	updated 04/08/2022 (EO) --- updated for 2021 Q4 APU
			-	added a run_hospice control flag at the top so that we
				do not need to keep commenting that sections out
			-	parameterized the core folder for ease of reading
	Updated 7/28/2022 (KAS) --
			- Added waiver folder
			- Added a control flag so you can determine whether or not to create the waiver folder
	
	updated 01/11/2022 (CT) --- updated for 2022-Q2 APU
			- added logs and results subfolder for support programs
			
	Updated 1/3/2023 (KAS)
			- Updated for 2022-Q3 OR

	updated 1/31/2023 (EO)
			-	for 22Q3 APU

	updated 05/01/2023 (SS)
			- 	for 22Q4 APU

	updated for 23Q1 apu

	updated for 23Q3 OR

	updated for 23Q4 OR

	updated for 23q4 apu
	
*/
%put NOTE: updates needed to these two parameters for each run ; 
%put NOTE: The folder must already exist on the workbench in the path desired below; 

%let folder 		= 	2023-Q4 APU ; 

*	Set this to 1 if you need to create folders for Hospice this reporting cycle, else set to 0	;
%let run_hospice 	= 	1 ; 
*	Set this to 1 if you need a folder for waiver assembly, else set to 0	;
%let waivers = 1;

*	Macro variable for the quarterly reporting folder (or other top-level folder)	;

%let qtr_dir = /workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/&folder.;

*	Macro variable with the standard list of subdirectories to create -- delimiter is a | (pipe)	;
%let subdirs_std = Data|Logs and Results|Programs;

*	This macro makes a directory. It will be called by other macros.
	Basically just packaging dcreate, really.
	;
%macro makedir(	dir =,
				parent =);

		%let rc = %sysfunc(dcreate(&dir, &parent));
	
%mEnd makedir;

*	This macro creates a program-level folder as well as subfolders.
	You can override the subdirectories to be created -- submit them as a pipe-delimited list.
	;
	
%macro make_structure(	dir =,
						parent = &qtr_dir,
						subdirs = &subdirs_std);
						
						
	/*	Create the folder	*/
	%makedir(	dir = &dir,
				parent = &parent)
				
	/*	Create its subfolders	*/
	/*	Initialize	*/
	%let subdir = 1;
	%let len_subdir = %length(%scan(&subdirs, &subdir, |));
	
	/*	Loop	*/
	%do %while(&len_subdir > 0);
	
		%let make_subdir = %scan(&subdirs, &subdir, |);
		%makedir(	dir = &make_subdir,
					parent = &parent/&dir)
	
	%let subdir = %eval(&subdir + 1);
	%let len_subdir = %length(%scan(&subdirs, &subdir, |));
	
	%end;
						
						
%mEnd make_structure;

	*	No subdirectories for the control table folder...	;
	%make_structure(dir = Control Table,
				subdirs =)
	;				
	%make_structure(dir = Assemble NHSN)
	;
	%make_structure(dir = Extract and Tally Claims)
	;
	%make_structure(dir = Extract and Assemble Assessments)
	;
	%make_structure(dir = Extract and Assemble Providers)
	;

	%make_structure(dir = IRF APU and Summary,
				subdirs = &subdirs_std.|Outputs)
	;
	%make_structure(dir = LTCH APU and Summary,
				subdirs = &subdirs_std.|Outputs)
	;
	%make_structure(dir = SNF APU and Summary,
				subdirs = &subdirs_std.|Outputs)
	;	
	*	No subdirectories for the support programs folder...	;
	%make_structure(dir = Support Programs,
				subdirs = Logs and Results)		
	;
%if &run_hospice = 1 %then %do ; 
	%make_structure(dir = Hospice APU and Summary,
				subdirs = &subdirs_std.|Outputs)	
%end ;

%if &waivers = 1 %then %do;
	%make_structure(dir = Assemble Waivers)
%end;	