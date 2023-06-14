

OPTIONS NOCENTER;
*%let path = \\uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core;
%include "&path\MSAccess\FINAL DATA\BASELINE\RENAME.sas";

DATA ReFormated_CCHC; SET FINAL.CCHC; 
RENAME %All_Rename_Var;
RUN;


data ReFormated_CCHC1;drop BASELINE_mm RH60mm RH90mm FMDPCTCHANGE;
set ReFormated_CCHC;
run;

/*Corrections needed added 03/28/2022*/
data final.CCHC;set ReFormated_CCHC1;
/*Corrections needed added 03/28/2022 
same 9 persons different rrid*/
if rrid='BD1292' then rrid='BD0172';
if rrid='BD0647' then rrid='BD0165';
if rrid='BD2128' then rrid='BD0238';
if rrid='BD2360' then rrid='BD0353';
if rrid='BD3239' then rrid='BD2575';
if rrid='BD1951' then rrid='BD0503';
if rrid='BD2087' then rrid='BD1917';
if rrid='BD2089' then rrid='BD1914';
if rrid='BD0647' then rrid='BD0165';

/*Correct genders 03/28/2022*/
if rrid='BD1732' then gender=1;
if rrid='BD1699' then gender=1;
if rrid='BD2783' then gender=2;
proc sort; by rrid visit;
run;
/*
proc contents data=ReFormated_HLCC
out=HLCC_ProcC(keep=VARNUM NAME LABEL) noprint;  
proc sort; by VARNUM;
run;       

data same; set HLCC_ProcC;
if name = label or label = '' then output;
run; 

data calc; set HLCC_ProcC;
if label =: "EAN" then output;
run; 
*/

