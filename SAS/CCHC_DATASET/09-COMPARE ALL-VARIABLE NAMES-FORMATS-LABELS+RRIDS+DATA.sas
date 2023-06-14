* Written by:	P.Sanchez;
* Date:			10/24/2013;
* Description: 	Compares YDrive data set with Server8 data set. 
				Checks for differences in Labels, Variable names, Formats.
Description: Compares YDrive data set with Server8 data set. Checks for differences in data.;
* Update(s):	
	08/12/14	PS	Update folder.
	08/27/14	PS	Updated paths only.	
	11/06/14	PS	Adding other programs.

* Date: 10/24/2013;
* UPDATE(S)
	08/28/14	PS	Updating paths only.
	11/05/14	PS	Merging 'comparing rrids' and 'comparing data' (due to slow server).
	11/06/14	PS	Adding other programs.
	06/25/15	PS	Using libnames U instead of Y.
	06/15/15	ES	OPTIMIZED, REDUCED, CLEANED.
	02/18/16	ES	Fixed Bug.
	07.11.16	ES	-Keep consistency over naming Folders in all programs.
;
OPTIONS NOCENTER;

%MACRO DATA_FREQ(DATA1,SET1,TABLE1,TABLE2,TABLE3);
DATA &DATA1; SET &SET1; RUN;
*PROC FREQ DATA=&DATA1; 
*TABLE &TABLE1 &TABLE2 &TABLE3;
RUN;
%MEND;

%DATA_FREQ(OLDCCHC,OLDY.CCHC,FYR_SURVEY,FYRS_SURVEY,);

%DATA_FREQ(NEWCCHC,FINAL.CCHC,LASTRESULT,,);

%MACRO SORT_DATA(DATA1,OUT1,BY1,BY2,BY3,BY4);
PROC SORT DATA=&DATA1 OUT=&OUT1; BY &BY1 &BY2 &BY3 &BY4; RUN;
%MEND;

%SORT_DATA(OLDCCHC,BASE_OLD_YDRIVE,VISIT,RRID,STUDY,LABID);

%SORT_DATA(NEWCCHC,COMPARE_NEW_S8,VISIT,RRID,STUDY,LABID);

%MACRO PROC_CONTENTS(DATA1,OUT1,DATA2,set1,set2,keep1,keep2,keep3,keep4,keep5);
PROC CONTENTS DATA=&DATA1 OUT=&OUT1 NOPRINT; RUN;
DATA &DATA2; SET &set1 &set2; 
	KEEP &keep1 &keep2 &keep3 &keep4; &keep2 = LABEL; &keep3 = FORMAT;
NAME=UPCASE(NAME);
&keep1=NAME;
RUN;
PROC SORT; BY NAME; RUN;
%MEND;

%PROC_CONTENTS(BASE_OLD_YDRIVE,YDRIVE1,YDRIVE2,YDRIVE1,,YNAME,YLABEL,YFORMAT,NAME,);

%PROC_CONTENTS(COMPARE_NEW_S8,S8A,S8B,S8A,,S8NAME,S8LABEL,S8FORMAT,NAME,);

DATA M1; MERGE YDRIVE2 S8B; BY NAME; RUN;

DATA M2; SET M1;
WHERE YNAME = '' OR S8NAME = '' OR (YFORMAT NE S8FORMAT) OR (YLABEL NE S8LABEL);
PROC SORT; BY YNAME;
RUN;


%MACRO TITLE_PRINT(TITLE1,DATA1,W1,NE1,AND1,EQUAL1,VAR1,VAR2,VAR3,VAR4,VAR5);
TITLE &TITLE1;
PROC PRINT DATA=&DATA1; 
WHERE &W1 NE &NE1 AND &AND1 = &EQUAL1; VAR &VAR1 &VAR2 &VAR3 &VAR4 &VAR5; RUN;
%MEND;

%TITLE_PRINT('1A. IDENTIFY CHANGES IN VARIABLES (DELETED VARIABLES)',M2,YNAME,S8NAME,S8NAME,'',NAME,YNAME,YLABEL,S8NAME,S8LABEL);
%TITLE_PRINT('1B. IDENTIFY CHANGES IN VARIABLES (NEW VARIABLES)',M2,YNAME,S8NAME,YNAME,'',NAME,YNAME,YLABEL,S8NAME,S8LABEL);
%TITLE_PRINT('2. IDENTIFY CHANGES IN FORMATS',M2,YFORMAT,S8FORMAT,YNAME,S8NAME,NAME,YFORMAT,S8FORMAT,,);
%TITLE_PRINT('3. IDENTIFY CHANGES IN LABELS',M2,YLABEL,S8LABEL,YNAME,S8NAME,NAME,YLABEL,S8LABEL,,);

DATA N1; SET M1; DROP YLABEL YNAME YFORMAT S8FORMAT;
IF S8NAME = '' THEN DELETE;
IF S8LABEL = S8NAME;
RUN;
/*
TITLE '4. IDENTIFY VARIABLES WHERE NAME = LABEL';
PROC PRINT DATA=N1; RUN;
*/
%MACRO COMPARE_RRIDS_SORT(DATA1,SET1,k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,c1,FLAG);
DATA &DATA1;
SET &SET1; 
KEEP &k1 &k2 &k3 &k4 &k5 &k6 &k7 &k8 &k9 &k10 &k11 &k12 &k13 ; 
&c1 = "&c1"; 
&c1._VISIT = VISIT;
&c1._STUDY = STUDY;
&c1._BDVISIT = BDVISIT;
&c1._LABID = LABID;
&c1._INTERVIEW_DATE = INTERVIEW_DATE;
%IF &FLAG = 1 %THEN
	%DO;
	FORMAT BASE_INTERVIEW_DATE DATE9.;
	%END;
%ELSE %DO; COMP_LASTRESULT=LASTRESULT; %END;
PROC SORT; BY RRID VISIT INTERVIEW_DATE STUDY;
RUN;
%MEND COMPARE_RRIDS_SORT;

%COMPARE_RRIDS_SORT(BASE1,BASE_OLD_YDRIVE,STUDY,BDVISIT,RRID,LABID,BASE_VISIT,VISIT,INTERVIEW_DATE,BASE_INTERVIEW_DATE,BASE,BASE_STUDY,BASE_BDVISIT,BASE_LABID,,BASE,1);

%COMPARE_RRIDS_SORT(COMP1,COMPARE_NEW_S8,STUDY,BDVISIT,RRID,LABID,COMP_VISIT,VISIT,INTERVIEW_DATE,COMP_INTERVIEW_DATE,COMP,COMP_STUDY,COMP_BDVISIT,COMP_LABID,COMP_LASTRESULT,COMP,2);

DATA MA1; MERGE BASE1 COMP1; BY RRID VISIT INTERVIEW_DATE STUDY; RUN;

DATA MA2; SET MA1; WHERE BASE = '' OR COMP = '' OR BASE_LABID NE COMP_LABID; 
FORMAT BASE_INTERVIEW_DATE COMP_INTERVIEW_DATE DATE9.;
RUN;

TITLE '5. IDENTIFY DIFFERENCES IN RRIDS FROM THE DATA SET IN THE YDRIVE (BASE) AND IN SERVER8 (COMPARE)';
PROC PRINT DATA=MA2; 
VAR
BASE COMP RRID LABID INTERVIEW_DATE 
BASE_VISIT BASE_STUDY BASE_BDVISIT BASE_LABID BASE_INTERVIEW_DATE
COMP_VISIT COMP_STUDY COMP_BDVISIT COMP_LABID COMP_INTERVIEW_DATE COMP_LASTRESULT;
RUN;

PROC PRINT DATA=MA2; WHERE BASE_BDVISIT NE COMP_BDVISIT AND BASE_BDVISIT NE '' 
						AND COMP_BDVISIT NE '';
RUN;

*COMPARING DATA***;
PROC SORT DATA=BASE_OLD_YDRIVE; BY TOWN RRID VISIT LABID STUDY BDVISIT ANNUAL; RUN;
PROC SORT DATA=COMPARE_NEW_S8; BY TOWN RRID VISIT LABID STUDY BDVISIT /*ANNUAL*/; RUN;

TITLE '7. CHECKS FOR DIFFERENCES IN DATA FROM THE DATA SET IN THE YDRIVE (BASE) AND SERVER8 (COMPARE)';
PROC COMPARE 
BASE= BASE_OLD_YDRIVE 
(DROP=DSDATE 
age_today length_diabetic        
today_yr
/*TEMPORARY REMOVAL OF SOME VARIABLES (PS 7.31.15)
URNCOLLECTED*/
/*
CTTIME1 CTTIME2 CTTIME3 CTTIME4 CTTIME5 TIMEOGTT CO1TIME
CO2TIME CO3TIME CO4TIME CO5TIME CO6TIME CO7TIME CO8TIME CO9TIME CO10TIME CO11TIME CO12TIME
CO13TIME CO14TIME ARRTIME DEPTIME DATEOGTT CO1DATE CO6DATE date_patresult
*/
BP_examdate
TODAYTIM EAT_TIME SCHDATE SCHTIME EDTA2 FACOMM FAHRTATC FACORAGC
   FACARAGC BIOSIBSC exit_examdate FASTDATE FASTTIME DATE_CREA
      DATE_CRP DATE_HDLC DATE_LDLCALC DATE_URIC 
      IDDCOMMENTS TIMESUPI TIMEBIO FAMCOMMENTS
MICROARRAY EDTA3 STAFFID
      DATE_CRL DATE_FBG_CRL CBC_DIFF DATE_ALK DATE_ALP DATE_BUN DATE_CALC DATE_CHL DATE_CHLR
      DATE_CHOL1 DATE_CO2 DATE_DBIL DATE_DLDL DATE_GFR DATE_GHB DATE_GOT DATE_GPT DATE_LALB
      DATE_MCGLUC DATE_POT DATE_SOD DATE_TBIL DATE_TP DATE_TRIG EDTA_2ML MUSTARD_5ML 
      H_TIME MEDCOMM TIMEW BPWELCH_EXAMDATE TIME  

HEIGHT
/*VARIABLES CREATED BY PROGRAM*/
    C_hhincmth C_hhincyr
  BMI1
   BMI BMIGR Obese_cat   
      OBESE30 OBESE40 
PACE_SURVEY FYR_SURVEY
OB_SURVEY w_date w

/*THESE VARIABLES SHOULD REMAIN CONSTANT*/
/*YROB*/
/*Patient_Type */
   FFBIRST


/*COMMENTS  /* Commented "comments" because of error ES 07/11/16*/ 
/*THIS VARIABLE SHOULD ALREADY EXIST AS IDDCOMMENTS, FAMCOMMENTS, 
   DIABCOMM, OR ANOTHER 'COMMENTS' VARIABLE - SHOULD BE OK TO DROP*/


/*ISRAEL ADDED NEW INSULIN DATA 
ins     
                     HOMA_IR          
                   HOMA_beta          
             HOMAIR_ABNORMAL          
                INSULIN_DATE  
*/

)
COMPARE= COMPARE_NEW_S8
OUTNOEQUAL OUTBASE OUTCOMP OUTDIF out=cd1
TRANSPOSE BRIEF MAXPRINT = 3200; 
ID TOWN RRID VISIT LABID STUDY BDVISIT ANNUAL; 
WHERE LABID NE '';
RUN;

/*
PROC SORT DATA=CD1; BY BDVISIT; RUN;

PROC TRANSPOSE DATA=CD1 OUT=CD2; BY BDVISIT; ID _TYPE_; VAR _ALL_; RUN;

DATA CD3; SET CD2; 
COMPARE = STRIP(COMPARE);
BASE = STRIP(BASE);
IF COMPARE = BASE THEN DELETE; 
IF COMPARE IN ('.' '') AND BASE IN ('.' '') THEN DELETE;
RUN;

DATA CD4; SET CD3; IF BASE NE ''; 
IF _NAME_ IN ('_TYPE_' '_OBS_' 'STUDY') THEN DELETE;
IF COMPARE = '.' THEN DELETE;
RUN;
PROC SORT; BY BDVISIT; RUN;

DATA CD5; SET CD4; IF _NAME_ = 'RRIDTIMES' AND COMPARE = ''; RUN;
DATA CD6; SET CD2; IF _NAME_ = 'RRIDTIMES' AND BASE = '' AND COMPARE NE ''; RUN;
*/
