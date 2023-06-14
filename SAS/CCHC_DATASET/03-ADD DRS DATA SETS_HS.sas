* Written by: 	P.Sanchez ;
* Date: 		3/31/2015 ;
* Description:  Appending BASELINE, 5YR, 10YR, DRS data sets. This program will not be used 
				after DRS is appended into the CCHC ACCESS DB.;
* Update(s):	
	04.01.15	PS	-Updating STUDY.
	04.28.15	PS	-Updating Town/Location.
	05.08.15	PS	-Including Pediatric records. 
	05.13.15	PS	-Renaming FBG3 to FBG_CRL.
	05.21.15	PS	-Updating paths only.
	06.24.15	PS	-Removing hard coding.
	06.25.15	PS	-Using U instead of Y (same path).
	07.06.15	PS	-Using SQL instead of 'merge'. 
	07.08.15	PS	-Renamed FBG3 to FBG_CRL for DRS CRL data set.
	07.29.15	PS	-Removing 'pediatric' code.
	07.07.16	ES	-Optimizing code and updating new tables
	07.12.16	ES	-Cleaned Code and optimize also keep consistency over naming Folders in 
					  all programs.
******************************************************************************************;

DATA CS1; LENGTH STUDY $30.; LENGTH LOCATION $20.; SET RAW.contactschedulingpat; FORMAT STUDY $30.; FORMAT LOCATION $20.; *DROP XPLACE;
*XPLACE = SUBSTR(RRID,1,1);
*TOWN = .;
*LOCATION = '            ';
*PEDIATRIC = 'NO '; *REMOVED BY PS (7.29.15);
*IF STUDY = 'PEDIATRIC' THEN PEDIATRIC = 'YES'; *REMOVED BY PS (7.29.15);
*CHECK WITH ISRAEL (MAY USE 'LOC_CITY'). PS 7.6.15;
IF TOWN = 1 THEN LOCATION = 'BROWNSVILLE';
IF TOWN = 2 THEN LOCATION = 'HARLINGEN'; 
IF TOWN = 3 THEN LOCATION = 'LAREDO';
*IF ANNUAL IN (0 1) THEN STUDY = 'RISK      ';
*IF SUBSTR(RRID,1,1) = 'R' OR SUBSTR(RRID,2,1) = 'R' THEN DELETE;
IF INDEX(UPCASE(STUDY), 'REFUSAL') <> 0 THEN DELETE;
IF BDVISIT IN ('-1' '') THEN DELETE;
IF RRID IN ('-1' '' ) THEN DELETE;
IF VISIT = . THEN DELETE;
*TEMPORARY FOR LAREDO MISSING STUDY;
*IF STUDY = '' AND VISIT = 1 THEN STUDY = 'BASELINE'; 
*IF VISIT = 1 THEN STUDY = 'BASELINE'; *REMOVED BY PS (7.29.15);
run;
DATA CS2; LENGTH STUDY $30.; LENGTH LOCATION $20.; SET DRS1.contact_a; FORMAT STUDY $30.; FORMAT LOCATION $20.;
STUDY = 'RISK';
TOWN = 1;
LOCATION = 'BROWNSVILLE'; 
RUN;
DATA RAW.contactschedulingpat_all CS; SET CS1 CS2;
PROC SORT; BY BDVISIT;
RUN;

DATA CRL_OTHER; SET RAW.CRLDATA (RENAME=(FBG3=FBG_CRL DATE_FBG3=DATE_FBG_CRL));
drop visit bdvisit;
PROC SORT; BY LABID;
RUN;


DATA RISK_CRL1; SET DRS1.CRLDATA (RENAME=(FBG3=FBG_CRL DATE_FBG3=DATE_FBG_CRL));
PROC SORT; BY BDVISIT; run;

DATA RISK_HEMAT; SET DRS1.hemat_a (RENAME=(H_DATE=DATE_CRL H_TIME=CRL_TIME)); LABEL DATE_CRL = "DATE_CRL" CRL_TIME = "CRL_TIME"; PROC SORT; BY BDVISIT; run; 

DATA RISK_CRL2; MERGE RISK_CRL1 (IN=A) RISK_HEMAT; IF A; BY BDVISIT; PROC SORT; BY BDVISIT; RUN;

DATA MY_KEYS; SET CS; KEEP RRID KEY8 KEY7; PROC SORT NODUPKEY; BY RRID; RUN;

DATA CRL_RISK; MERGE RISK_CRL2 (IN = A) MY_KEYS; IF A; BY RRID; drop visit bdvisit; RUN;

PROC SORT DATA = CRL_RISK; BY LABID; RUN;


data CRL_Final_Set;
set CRL_FL.final_crl_results_all;
run;

proc sql;
create table CRL_NeedSMerge as
select * from CRL_Final_Set 
where labid in (select labid  
from CRL_OTHER where labid in (select labid from CRL_Final_Set));
quit;

proc sql;
create table CRL_NOTIN as
select * from CRL_Final_Set 
where labid not in (select labid from CRL_OTHER);
quit;

data CRL_NeedSMerge;
drop STAFFID CHOL1_FLAG CHLR_FLAG HDLC_FLAG LDLCALC_FLAG TRIG_FLAG GPT_FLAG GOT_FLAG LALB_FLAG ALK_FLAG BUN_FLAG
TBIL_FLAG CO2_FLAG CALC_FLAG CHL_FLAG CREA_FLAG GFR_FLAG FBG3_FLAG POT_FLAG SOD_FLAG TP_FLAG MCGLUC_FLAG GHB_FLAG
BASO_NO_FLAG EOS_NO_FLAG HCT_FLAG HGB_FLAG LY_NO_FLAG MCH_FLAG MCHC_FLAG MCV_FLAG MPV_FLAG MO_NO_FLAG NEUT_NO_FLAG
PLT_FLAG RBC_FLAG RDW_FLAG WBC_FLAG CALC_WBC DATE_N_TIME bdvisit visit;
*rename FBG3=FBG1;
set CRL_NeedSMerge;
run;

data CRL_NOTIN;
drop STAFFID CHOL1_FLAG CHLR_FLAG HDLC_FLAG LDLCALC_FLAG TRIG_FLAG GPT_FLAG GOT_FLAG LALB_FLAG ALK_FLAG BUN_FLAG
TBIL_FLAG CO2_FLAG CALC_FLAG CHL_FLAG CREA_FLAG GFR_FLAG FBG3_FLAG POT_FLAG SOD_FLAG TP_FLAG MCGLUC_FLAG GHB_FLAG
BASO_NO_FLAG EOS_NO_FLAG HCT_FLAG HGB_FLAG LY_NO_FLAG MCH_FLAG MCHC_FLAG MCV_FLAG MPV_FLAG MO_NO_FLAG NEUT_NO_FLAG
PLT_FLAG RBC_FLAG RDW_FLAG WBC_FLAG CALC_WBC DATE_N_TIME bdvisit visit;
*rename FBG3=FBG1;
set CRL_NOTIN;
IF SUBSTR(LABID,1,2) NOT IN ('BA' 'BL' '5Y' '10' '15' ) THEN DELETE;
run;

data CRL_NOTIN2;
set CRL_NOTIN;
new = input(STAFFID, $char90.);
drop STAFFID;
rename new=STAFFID;
run;

data CRL_OTHER2;
  update CRL_OTHER CRL_NeedSMerge;
  by labid;
run;

data CRL_OTHER3;
set CRL_OTHER2 CRL_NOTIN2;
proc sort; by labid;
run;

DATA CRL_DATA_ALL;
set CRL_OTHER3 CRL_RISK;
if LABID = "" then delete;
proc sort; by labid;
run;

data raw.crl_data_all;
set crl_data_all;
run;



