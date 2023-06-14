* Written by: 	P.Sanchez ;
* Date: 		4/8/2015 ;
* Description:  Creates data set 'other_studies' which contains all RRIDs and the substudies
				they are in.;
* Update(s):	
	05.21.15	PS	-Updated paths only.
	05.29.15	PS	-Updated PATH data set.
	05.29.15	ES	-Optimized, reduced.
	11.12.15	ES	-Change paths
	07.11.16	ES	-Keep consistency over naming Folders in all programs.
;
OPTIONS NOFMTERR;
/*
%let pathmsa = U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess;
LIBNAME LIBRARY "\\uthouston.edu\uthsc\sph\Research\Studies Data\SAS Formats";
LIBNAME FINAL "&pathmsa\FINAL DATA\BASELINE";
LIBNAME OB "&pathmsa\FINAL DATA\OB";
LIBNAME CAB "&pathmsa\FINAL DATA\CAB";
LIBNAME DRS "&pathmsa\FINAL DATA\DRS";
LIBNAME BRO "&pathmsa\TEMPDATA\NEWCCHC";
LIBNAME PACE "&pathmsa\Final Data";
*/
************************************************************************************;
/*Data*/
%LET DATA1= PREG1; 				%LET DATA2 = C1;					%LET DATA3 = D1;	
%LET DATA4 = B1;				%LET DATA5 =P1;					
/*Set*/
%LET SET1= OB.obmain;			%LET SET2 = CAB.all_cabparticipants;
%LET SET3 = DRS.patient_alldrs_iz;	%LET SET4 = BRO.contact_a;			%LET SET5=PACE.PACEIDS;
/*Survey*/
%LET SURVEY1= OB_SURVEY;		%LET SURVEY2 = CAB_SURVEY;
%LET SURVEY3 = DRS_SURVEY;		%LET SURVEY4 = FYR_SURVEY;		%LET SURVEY5 = PACE_SURVEY;
/*Study*/
%LET STUDY1= ; 					%LET STUDY2 = ;						%LET STUDY3 = ;
%LET STUDY4 = STUDY;			%LET STUDY5=;
/*Out*/
%LET OUT1= PREG2; 				%LET OUT2 = C2;						%LET OUT3 = D2;
%LET OUT4 = B2;					%LET OUT5 = P2;
/*Flag*/
%LET FLAG1= 2; 					%LET FLAG2 = 2;						%LET FLAG3 = 2;
%LET FLAG4 = 1;					%LET FLAG5 =2;

%MACRO GET_DATA(DATA,SET,RRID,T2,T3,OUT,FLAG);
DATA &DATA; SET &SET; KEEP &RRID &T2 &T3; &T2 = 1; 
%IF &FLAG EQ 1 %THEN
	%DO;
	IF STUDY = 'FIVE YEAR';
	%END;
RUN;
PROC SORT DATA=&DATA OUT=&OUT 
%IF &FLAG EQ 1 %THEN
	%DO;
	(DROP = STUDY)
	%END;
NODUPKEY;
BY RRID; RUN;
%MEND GET_DATA;

%MACRO PROC_DATA;
%DO i=1 %TO 5;
%GET_DATA(&&DATA&i.,&&SET&i.,RRID,&&survey&i.,&&study&i.,&&OUT&i.,&&FLAG&i.);
%END;
%MEND;
%MACRO MERGE_SET_SORT_FINAL;
DATA M1; MERGE PREG2 C2 D2 B2 P2; BY RRID; RUN;

*FORMAT FYR_SURVEY DRS_SURVEY PACE_SURVEY CAB_SURVEY OB_SURVEY COHORF.;
DATA M2; SET M1; IF PACE_SURVEY = . THEN PACE_SURVEY = 0; 
PROC SORT DATA=M2 OUT=M3 NODUPKEY; BY RRID; 
RUN; *0 DUPES;

DATA OTHER_STUDIES FINAL.OTHER_STUDIES; SET M3; RUN; 
%MEND;

*START PROGRAM;
%PROC_DATA;
%MERGE_SET_SORT_FINAL;
RUN;
*END PROGRAM;


/* FINAL RESULTS
NOTE: The data set FINAL.OTHER_STUDIES has 2686 observations and 6 variables. 4.9.15
NOTE: The data set FINAL.OTHER_STUDIES has 3131 observations and 6 variables. 4.28.15
NOTE: The data set FINAL.OTHER_STUDIES has 3131 observations and 6 variables. 5.5.15
NOTE: The data set FINAL.OTHER_STUDIES has 3132 observations and 6 variables. 5.7.15
NOTE: The data set FINAL.OTHER_STUDIES has 3139 observations and 6 variables. 5.28.15
NOTE: The data set FINAL.OTHER_STUDIES has 3187 observations and 6 variables. 2.18.16
*/
