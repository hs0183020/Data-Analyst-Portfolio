/* 

CMS has requested a listing of providers with dashes entered in the CAMS fields for Q4

*/

libname irf_Q4 "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/IRF APU and Summary/Data" ; 
libname irf_OR "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 OR/IRF APU and Summary/Data" ; 
libname irf_Q3 "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q3 APU/IRF APU and Summary/Data" ; 


%let in_fail_fields = IRF_ASMT_FAILS_23Q4_240425 ;
%let in_q3 = IRF_APU_23Q3_240325 ;
%let in_or = IRF_OR_23Q4_240418 ; 

%let report_date = %sysfunc(today(), yymmddd8.) ; 
%let out_name = IRF_CAMS_Fails_Provider_List_&report_date ; 
%let out_sheet = CAMSList ; 

%put NOTE: get ccns of providers with any cams items dashed ; 

proc sql ; 
	create table list_ccn as
	select ccn,
		count(distinct orgnl_asmt_id) as records_dashed_cams
	from irf_q4.&in_fail_fields
	where fail_field in
		("mentl_stus_chg_dschrg_cd"
			, "dfclty_attntn_dschrg_cd"
			, "thnkg_disorgnz_dschrg_cd"
			, "lvl_conscs_dschrg_cd"
		)
	group by ccn;
quit ;

%put NOTE: grab fac name and overall performance ; 
proc sql ; 
	create table full_list as
	select apu.ccn,
		apu.fac_name label = "Facility Name",
		apu.irf_asmt_numer as Q3_numerator label = "Q1-Q3 Numerator",
		apu.irf_asmt_denom as Q3_Denom label = "Q1-Q3 Denominator",
		apu.irf_asmt_comply_rate as Q3_compliance label = "Q1-Q3 Percent Compliance",
		apu.meet_apu_best as Q3_Best_Meet_APU label = "Q1-Q3 Best Meet APU",
		ora.irf_asmt_numer as Q4_OR_numerator label = "Q4 Outreach Numerator",
		ora.irf_asmt_denom as Q4_OR_Denom label = "Q4 Outreach Denominator",
		ora.irf_asmt_comply_rate as Q4_OR_compliance label = "Q4 Outreach Percent Compliance",
		ora.meet_apu_best as Q4_OR_Meet_APU label = "Q4 Outreach Initial Meet APU",
		list.records_dashed_cams as Q4_CAMS_Dash_Records label = "Q4 Outreach Records with CAMS dashes"
	from list_ccn as list
		left join
		irf_Q3.&in_q3 as apu
		on list.ccn = apu.ccn
		left join
		irf_or.&in_or as ora
		on list.ccn = ora.ccn
	;
quit ; 

* mac list ; 
	* since I need the labels to output as column headers I will use an excel engine ; 
libname outlist xlsx "/workspace/workbench/pac_qrp_swingtech/data/SAS/Quarterly and Annual Reporting/2023-Q4 APU/IRF APU and Summary/Outputs/&out_name..xlsx" ; 


data outlist.&out_sheet. (dblabel=yes)  ;
	set full_list ; 
run ; 
