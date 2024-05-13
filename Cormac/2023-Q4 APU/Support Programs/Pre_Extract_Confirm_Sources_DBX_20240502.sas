/*	Purpose of this program is to quickly check key metrics from tables we extract from the CDR for APU reporting. This program answers
	the question, "do we have enough confidence that the data have actually been updated to start running our extracts." It is not intended to
	be a comprehensive data check -- these checks are run during extraction and/or assembly.
	
	Program Name: Pre_Extract_Confirm_Sources_Updated_

	Tables checked:
	
		IRF_ASMT_FED		-- IRF assessments
		LTCH_ASMT_FED		-- LTCH assessments
		MDS_ASMT_FED		-- MDS (SNF) assessments
		HOSPC_ASMT_FED		-- Hospice assessments
		csp_prvdr_cmn		-- Providers for IRF, LTCH, and Hospice
		natl_facility_mds	-- Providers for SNF
		
		
	We don't really need to check claims (they update on a weekly cycle and are used by enough other contractors that if there was a problem with
	them, we would hear about it) or the History tables (we'll make the assumption that if the assessment tables are updated, the history tables
	are likely to have been updated, too. We have QA later in the process to catch issues with this in any event -- remember that the point of
	this program is to quickly tell us whether it's probably OK to move forward with extraction or not.)
	
	This program does not produce output -- just check the results and confirm that all key dates seem reasonable.
	
	Created 8/19/2021 -- KAS

	updated 02/01/2022 -- EO
		- 	added year filters on key variables for each table
			this should allow for more efficient and faster processing
	updated 06/30/2022 -- EO	
	
	updated 07/05/2023 -- EO
		-	updated year filter for new reporting year

	updated 10/03/2023 -- EO
		-	changes to each call to link through DBX 
			using the ISG supplied link macro hive exec sql
			with an explicit connection

	updated 11/07/2023 -- EO
		- no changes for 23Q2 APU

	updated 01/02/2024 -- EO
		- changed date call to DBX needed syntax

	updated 04/03/2024 -- CT
		- updated for 2023-Q4 OR
		- changed the check field for natl_facility_mds
		  from sys_ld_id to st_prepd_dt based on ISG/eSimplicity guidance
	updated 04/04/2024 -- EO
		- updated SNF prvdr table title to reflect change in var
		- updated SNF prvdr subsetting variable as well

	updated 05/02/2024 -- CT
		- no changes for 2024-Q4 APU
*/	
	
* if desired --- update Q1 of each reporting year ; 
* can be left on JAN 01 whether or not you update the year ; 
%LET fy_start_date = 2023-01-01 ;

title	"IRF Assessment -- IRF_ASMT_FED";
title2	"Max trgt_dt and submsn_dt should be close to the date CDR pulled data from iQIES.";

%hive_exec_sql( %STR(

           select max(trgt_dt) as max_trgt_dt,
				max(submsn_dt) as max_submsn_dt
		from	assessment_iqies.IRF_ASMT_FED
		WHERE	DATE("&fy_start_date") <= trgt_dt),

use_dbx=Y);
title;



title	"LTCH Assessment -- LTCH_ASMT_FED";
title2	"Max trgt_dt and submsn_dt should be close to the date CDR pulled data from iQIES.";

%hive_exec_sql( %STR(

           select max(trgt_dt) as max_trgt_dt,
				max(submsn_dt) as max_submsn_dt
		from	assessment_iqies.LTCH_ASMT_FED
		WHERE	DATE("&fy_start_date") <= to_date(trgt_dt, 'YYYY-MM-DD')),

use_dbx=Y);
title;

title	"SNF Assessment -- MDS_ASMT_FED";
title2	"Max trgt_dt and submsn_dt should be close to the date CDR pulled data from iQIES.";

%hive_exec_sql( %STR(

           select max(trgt_dt) as max_trgt_dt,
				max(submsn_dt) as max_submsn_dt
		from	assessment_iqies.MDS_ASMT_FED
		WHERE	DATE("&fy_start_date") <= to_date(trgt_dt, 'YYYY-MM-DD')),

use_dbx=Y);
title;

title	"Hospice Assessment -- HOSPC_ASMT_FED";
title2	"Max trgt_dt and submsn_dt should be close to the date CDR pulled data from iQIES.";

%hive_exec_sql( %STR(

           select max(trgt_dt) as max_trgt_dt,
				max(submsn_dt) as max_submsn_dt
		from	assessment_iqies.HOSPC_ASMT_FED
		WHERE	DATE("&fy_start_date") <= to_date(trgt_dt, 'YYYY-MM-DD')),

use_dbx=Y);
title;

title	"Providers for IRF/LTCH/Hospice -- CSP_PRVDR_CMN";
title2	"Max update date should be close to the date CDR pulled data from iQIES.";

%hive_exec_sql( %STR(

           select max(updt_dt) as max_update_dt
		from	provider_iqies.csp_prvdr_cmn
		WHERE	DATE("&fy_start_date") <= to_date(updt_dt, 'YYYY-MM-DD')),

use_dbx=Y);

title;

title	"Providers for SNF -- NTL_FACILITY_MDS";
title2	"Max st_prepd_dt should be the date CDR pulled data from iQIES";
%hive_exec_sql( %STR(

           select max(st_prepd_dt) as max_st_prepd_dt
		from	assessment_iqies.natl_facility_mds
		WHERE	DATE("&fy_start_date") <= to_date(st_prepd_dt, 'YYYY-MM-DD')),

use_dbx=Y);
title;
