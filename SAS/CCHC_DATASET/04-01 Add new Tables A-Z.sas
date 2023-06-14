/*
Created by 	Emmanuel Santillana
Date:07/11/16
Purpose:
Adds new tables into the current program, drops columns key7,key8, and deletes
	bdvisits = blank
Keeps the format of naming on original program 04-Convert Data sets+Merge Lab
	Data.sas
Output: Dataset named A_B_Merged in Final folder, then on program 06 it gets 
		added into whole program.
*/

*	tables that will be processed		;
%let t1 = A_majdepep;		%let t2 = B_Dysthymia;		%let t3 = C_Suicidality;
%let t4 = D_Hypomanicep;	%let t5 = E_PanicDisorder;	%let t6 = F_Agoraphobia;
%let t7 = G_Sepanxdis;		%let t8 = H_Socialphobia;	%let t9 = I_Specificphobia;
%let t10 = J_OCD;			%let t11 = K_PTSD;			%let t12 = L_Alcoholabuse;
%let t13 = M_Napasud;		%let t14 = N_TicDisorders;	%let t15 = O_ADHD;
%let t16 = P_Conductdis;	%let t17 = Q_Oppositional;	%let t18 = R_psychoticmood;
%let t19 = S_Anorexia;		%let t20 = T_Bulimia;		%let t21 = U_Genanx;
%let t22 = V_Adjustment;	%let t23 = W_ruleout;		%let t24 = X_Pervasive;
/*		Pediatric forms pending		*/
%let t25= cabspd;			%let t26 = cesdpd;			%let t27 = saspd;
/*		Pending Impact				*/
%let t28= ImpactEan;		%let t29 = ImpactSp;

*	proc SQL alias for each table	;
%let L1 = A;				%let L2 = B;				%let L3 = C;
%let L4 = D;				%let L5 = E;				%let L6 = F;
%let L7 = G;				%let L8 = H;				%let L9 = I;
%let L10 = J;				%let L11 = K;				%let L12 = L;
%let L13 = M;				%let L14 = N;				%let L15 = O;
%let L16 = P;				%let L17 = Q;				%let L18 = R;				
%let L19 = S;				%let L20 = T;				%let L21 = U;				
%let L22 = V;				%let L23 = W;				%let L24 = X;
%let L25 = cspd;			%let L26 = cespd;			%let L27 = sapd;
%let L28 = ean;				%let L29 = segpres;
*	deletes blank bdvisit, drops key7 and key8 for everytable;
%macro clean(table);
data RAW.&table._converted &table._converted; set RAW.&table; drop key7 key8;
if bdvisit= '' then delete;
%mend clean;
*	Loop to run all 29 tables and clean them all;
%macro cleandata();
%do i=1 %to 29;
%clean(&&t&i.);
%end;
%mend cleandata;
*	Merges all tables previously cleaned into a A_B_Merged;
%macro mergedata();
proc sql noprint;
create table RAW.Pediatric_Data_Merged as
select *
from A_majdepep_converted A left join B_dysthymia_converted B
/* ON A.BDVisit=B.BDVISIT and A.RRID=B.RRID */
ON A.BDVisit=B.BDVISIT
/*	Repeat left join for all the tables	*/
	%do	j=3 %to 27;
	left join &&t&j.._converted as &&L&j.
	/* ON A.bdvisit = &&L&j...bdvisit and A.rrid=&&l&j...rrid */
	ON A.bdvisit = &&L&j...bdvisit
	%end;
;quit;
run;
%mend mergedata;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++		Start of Program		++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

*step 1;	%cleandata;

			*	format date in table	;
*step 2;	data RAW.V_ADJUSTMENT_converted V_ADJUSTMENT_converted; set RAW.V_ADJUSTMENT; drop key7 key8;
			if bdvisit= '' then delete;
			if idstressor ne "" then DTSTRESSOR = DTSTRESSOR/86400;
			else DTSTRESSOR = .;
			format DTSTRESSOR Date9.;
			if year(DTSTRESSOR) < 1920 then DTSTRESSOR = .;
			run;

*step 3;	%mergedata;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++		End of Program			++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
