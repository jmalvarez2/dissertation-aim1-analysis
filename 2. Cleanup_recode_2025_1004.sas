/*==================================================================================================================================================*/
/*Pupose: Recode and create variables for Aim 1*/

/*Created by: Juan M. Alvarez*/

/*Date created: 06/25/2025*/
/*Date edited: 10/04/2025*/

/*Input dataset: diss.aim1*/
/*Output dataset: diss.aim1_v5*/

/*Output table: Table 1: Demographic and health characteristics of the study population with detectable urinary phthalates and polycyclic aromatic 
                hydrocarbons, and serum per- and polyfluoroalkyl substances in the National Health and Nutrition Examination Survey (NHANES), 2011-2012  
                (N = 1,404) */
/*==================================================================================================================================================*/

libname DISS "H:\UTHealth Dissertation\SAS Datasets"; 
RUN;

/*==================================================================================================================================================*/
/*Recoding and constructing variables for Aim 1

/*Recode education (youth and adult) and race/ethnicity*/

proc freq data = diss.aim1_2025_0615;
table educ_youth educ_adult;
run;

data rec;
 set diss.aim1;
	edu_youth_new =.;
		if educ_youth = 4 or educ_youth = 5 or educ_youth = 6 or educ_youth = 7 or educ_youth = 8 then edu_youth_new = 1;
		else if educ_youth = 9 or educ_youth = 10 or educ_youth = 11 or educ_youth = 12 then edu_youth_new = 2;
		else if educ_youth GE 13 then edu_youth_new = 3;
		else if educ_youth = . then edu_youth_new = .;

	edu_adult_new = .;
		if educ_adult = 1 or educ_adult = 2 or educ_adult = 3 then edu_adult_new = 1;
		else if educ_adult = 4 or educ_adult = 5 then edu_adult_new = 2;
		else if educ_adult = 9 then edu_adult_new = 3;
		else if educ_adult = . then edu_adult_new = .;

	racethnic = .;
		if ethnicity = 1 or ethnicity = 2 then racethnic = 1;
		else if ethnicity = 3 then racethnic = 2;
		else if ethnicity = 4 then racethnic = 3;
		else if ethnicity = 5 then racethnic = 4;

	education =.;
		if educ_youth = 4 or educ_youth = 5 or educ_youth = 6 or educ_youth = 7 or educ_youth = 8 or educ_adult = 1 then education = 1;
		else if educ_youth = 9 or educ_youth = 10 or educ_youth = 11 or educ_youth = 12 or educ_adult = 2 then education = 2;
		else if educ_youth = 13 or educ_youth = 14 or educ_adult = 3 then education = 3;
		else if educ_youth = 15 or educ_adult = 4 then education = 4;
		else if educ_adult = 5 then education = 5;
		else education = 6;
run;

proc freq data = rec;
	table educ_youth*edu_youth_new educ_adult*edu_adult_new ethnicity*racethnic education/list missing;
run;

/*Construct Uric acid to albumin ratio*/
data rec1;
 set rec;
	UAR = uric_acid/albumin;
run;

proc univariate data = rec1;
 var UAR;
run;

/*Find average of systolic and diastolic blood pressure*/
data rec2;
 set rec1;
	SBP_avg = mean(SBP_1, SBP_2, SBP_3, SBP_4);
	DBP_avg = mean(DBP_1, DBP_2, DBP_3, DBP_4);
run;

proc univariate data = rec2;
var sbp_avg dbp_avg;
run;

/*---------------------------------------------------------------------*/
/*Hypertension (yes/no)* Flyn et al. for children; 2025 AHA for adults*/

data rec9;
set rec2;

htn = .;

 if 13 <= age <=17 then do;
 if SBP_avg GE 130 or DBP_avg >= 80 then htn = 1;
 else htn = 0;
end;
 if age = 12 then do;
 if SBP_avg GE 130 or DBP_avg GE 80 then htn = 1;
 else htn = 0;
end;
 if age GE 18 then do;
 if SBP_avg GE 140 or DBP_avg GE 90 then htn = 1;
 else htn = 0;
end;

run;

proc freq data = rec9;
table htn;
run;

/*---------------------------------------------------------------------*/
/*Variable to determine if total cholesterol is high. Schefelker et al. 2022 for children*/
data rec10;
 set rec9;

 high_chol =.;

 if age LE 19 then do;
 if tot_chol GE 170 then high_chol = 1;
 else high_chol = 0;
end;
 if age > 19 then do;
 if tot_chol GE 200 then high_chol = 1;
 else high_chol = 0;
end;
run;

proc freq data = rec10;
	table high_chol/list missing;
run;

/*Create cardiovascular disease risk variable*/

data rec11;
 set rec10;

CVD_risk = 0;
if htn = 1 or high_chol = 1 then CVD_risk = 1;
run;

proc freq data = rec11;
table cvd_risk CVD_risk*htn*high_chol/list missing;
run;


data rec12; 
 set rec11;
	if 12 <= age <= 19 then age_strata = 1;
	else if 20 <= age <= 39 then age_strata = 2;
	else if 40 <= age <= 64 then age_strata = 3;
	else if 65 <= age <= 80 then age_strata = 4;
run;

proc freq data = rec12;
	table age_strata/missing;
run;

/*Construct categorical BMI for children and teens 2 through 19, and adults*/
/*According to CDC guidelines*/

proc univariate data = rec12;
 where group = 1;
	var BMI;
	output out=percentiles pctlpts=5 pctlpts=85 pctlpts=95 pctlpre=P_;
run;

data rec13;
	set rec12;

BMI_child = .;
if group = 1 and BMI < 16.9 then BMI_child = 1; /*Underweight*/
else if group = 1 and 16.9 LE BMI < 30.9 then BMI_child = 2; /*Healthy Weight*/
else if group = 1 and 30.9 LE BMI < 38.8 then BMI_child = 3; /*Overweight*/
else if group = 1 and BMI >= 38.8 then BMI_child = 4; /*Obesity*/

BMI_adult =.;
if group = 2 and BMI < 18.5 then BMI_adult = 1; /*Underweight*/
else if group = 2 and 18.5 LE BMI < 25 then BMI_adult = 2; /*Healthy Weight*/
else if group = 2 and 25 LE BMI < 30 then BMI_adult = 3; /*Overweight*/
else if group = 2 and BMI GE 30 then BMI_adult = 4; /*Obesity*/
run;

data rec14;
	set rec13;

BMI_new = .;
if BMI_child = 1 or BMI_adult = 1 then BMI_new = 1;
else if BMI_child = 2 or BMI_adult = 2 then BMI_new = 2;
else if BMI_child = 3 or BMI_adult = 3 then BMI_new = 3;
else if BMI_child = 4 or BMI_adult = 4 then BMI_new = 4;
run;

proc freq data = rec14;
table BMI_child*BMI_adult*BMI_new/list missing;
run;

proc freq data = rec14;
	where group = 1;
	table BMI_child/missing;
run;

proc freq data = rec14;
	where age_strata NE 1;;
	table BMI_adult/missing;
run;

/*Construct cutpoints to identify smokers vs non-smokers*/
/*Based on Benowitz et al., 2009*/

data rec15;
 set rec14;
smoker = .;
if cotinine GE 3.00 then smoker = 1;
else if cotinine < 3.00 then smoker = 0;
run; 

proc freq data = rec15;
table smoker/list missing;
run;

/*Age (categorical)*/

data rec16;
 set rec15;

age_new = .;
if age < 23 then age_new = 1;
else if 23 LE age < 40 then age_new = 2;
else if 40 LE age < 60 then age_new = 3;
else if age GE 60 then age_new = 4;
run;

proc freq data = rec16;
table age_new/missing;
run;

proc sort data = rec16;
by CVD_risk;
run;

proc freq data = rec16;
by CVD_risk;
table age_new age_strata gender educ_youth educ_adult ethnicity
	  diabetes BMI_adult BMI_child htn high_chol smoker/missing;
run;

proc univariate data = rec16;
by cvd_risk;
var age bmi cotinine SBP_avg DBP_avg tot_chol albumin uric_acid UAR;
run;

proc means data = rec16 mean std;
by cvd_risk;
var PFHxS PFNA PFDE MBzP MCPP MEHHP MEHP MEOHP MEP MnBP MiBP NAP1 NAP2 OHFlu2 OHFlu3 OHPHE1 PYR1;
run;

/*====================================================================================================*/
/*Recode diabetes: 'Borderline' diabetes to 'Yes'*/

data rec17;
 set rec16;
	diabetes_new =.;
	if diabetes = 3 then diabetes_new = 1;
	else if diabetes = 1 then diabetes_new = 1;
	else if diabetes = 2 then diabetes_new = 2;
run;

proc freq data = rec17;
table diabetes*diabetes_new/list missing;
run;

/*====================================================================================================*/
/*Categorize PIR*/

data rec18;
set rec17;

PIR_new = .;
if PIR < 1.3 then PIR_new = 1;
else if 1.3 <= PIR <= 3.5 then PIR_new = 2;
else if PIR > 3.5 then PIR_new = 3;
run;

proc freq data = rec18;
table PIR_new;
run;

/*====================================================================================================*/
/*Categorize glucose levels*/

data rec19;
set rec18;

glu_new = .;
if glu < 126 then glu_new = 0;
else if glu GE 126 then glu_new = 1;
run;

proc freq data = rec19;
table glu_new;
run;

/*====================================================================================================*/
/*Categorize cotinine levels*/

data rec20;
set rec19;

cot_new = .;
if cotinine < 0.015 then cot_new = 1;
else if 0.015 <= cotinine <= 2 then cot_new = 2;
else if cotinine GE 2 then cot_new = 3;
run;

proc freq data = rec20;
table cot_new/list missing;
run;

data rec21;
set rec20;

cot_new2 = .;
if age GE 20 and cotinine > 3.8 then cot_new2 = 1;
else if age GE 20 and cotinine LE 3.8 then cot_new2 = 2;

cot_new3 = .;
if  age < 20 and cotinine > 2.99 then cot_new3 = 1;
else if age < 20 and cotinine LE 2.99 then cot_new3 = 2;
run;

data rec22;
set rec21;

smk_status = .;
if cot_new2 = 1 or cot_new3 = 1 then smk_exp = 1;
else if cot_new2 = 2 or cot_new3 = 2 then smk_exp = 2;
run;

proc freq data = rec22;
table cot_new cot_new2 cot_new3 smk_exp;
run;

/*====================================================================================================*/
/*Create smoking status varible*/

proc freq data = rec22;
table SMK100 SMK_NOW SMK_30;
run;

data rec23;
set rec22;

smk_status = .;
if SMK100 = 2 then smk_status = 1; /*Never*/
else if SMK100 = 1 and SMK_NOW = 3 then smk_status = 2; /*Former*/
else if SMK100 = 1 and SMK_NOW = 1 or SMK_NOW = 2 then smk_status = 3; /*Current*/
run;

proc freq data = rec23;
table smk_status/list missing;
run;

data rec24;
set rec23;

smk_status2 = .;
if smk_status = 1 or smk_status = 2 then smk_status2 = 1;
else if smk_status = 3 then smk_status2 = 2;
else if smk_status = . then smk_status2 = .;
run;

proc freq data = rec24;
table smk_status*smk_status2/list missing;
run;

/*====================================================================================================*/
/*Create albuminuria variable*/

data rec25;
set rec24;

creat_new = creatinine / 1000;

creat_album = albumin / creat_new;
run;

proc univariate data = rec25;
var creatinine albumin creat_album;
run;

data rec26;
set rec25;

albumin_new = .;
if creat_album < 30 then albumin_new = 1;
else if 30 <= creat_album <= 300 then albumin_new = 2;
else if creat_album > 300 then albumin_new = 3;
run;

data rec27;
set rec26;

albumin_new2 = .;
if albumin_new = 2 or albumin_new = 3 then albumin_new2 = 1;
else if albumin_new = 1 then albumin_new2 = 0;
run;

proc freq data = rec27;
table albumin_new*albumin_new2/list missing;
run;

/*====================================================================================================*/
/*Create hyperuricemia variable*/

data rec28;
set rec27;

hyperuric = .;
if gender = 1 and uric_acid GE 7 then hyperuric = 1; /*Elevated*/
else if gender = 1 and uric_acid < 7 then hyperuric = 0; /*Normal*/

if gender = 2 and uric_acid GE 6 then hyperuric = 1; /*Elevated*/
else if gender = 2 and uric_acid < 6 then hyperuric = 0; /*Normal*/

run;

proc freq data = rec28;
table hyperuric/list missing;
run;

proc freq data = rec28;
table hyperuric*albumin_new2/list missing;
run;

data diss.aim1_2025_0713;
 set rec28;
run; 

/*==================================================================================================================================================*/
/*Drop unused variables*/

data temp;
set diss.aim1_2025_0713;

drop alcohol_12 alcohol_12_dmy alcohol_12_life alcohol_1yr alcohol_freq alcohol_life angina bpq150a bpq150b bpq150c bpq150d citizen cor_heart_disease food
	 hbp_yn hdl_yn height hg_u hg_u_comm hh_size hrt_att hrt_fail mcq220 pad615 pad630 pad645 pad660 pad675 paq610 paq625 paq640 paq655 paq670 rhd143
	 smknow_num smknow_pcks smk_30 smk_30_pcks smk_now smk_reg stroke waist_circ weight educ_adult educ_youth lbdhdd lbdhddsi lbdtcsi lbxhdd lbxtc;
run;

data diss.aim1_v2;
set temp;
run;

/*============================================*/
/*Re-code albuminuria (08/10/2025)*/

data okay;
set diss.aim1_v2;

albuminuria = .;
if albumin < 3.4 then albuminuria = 1;
else if albumin GE 3.4 then albuminuria = 0;
run;

data diss.aim1_v3;
set okay;
run;

/*============================================*/
/*Log transform biomarkers and creatinine (09/20/2025)*/

/*Natural log-transform uric acid and albumin*/

data transform;
set diss.aim1_v3;
	uric_log = log(uric_acid);
	albumin_log = log(albumin);
	creatinine_log = log (creatinine);
run;

proc reg data = transform;
	model creatinine_log = age racethnic gender bmi;
	output out=transform2 p=plogC;
run;

data diss.aim1_v4;
	set transform2;
run;

proc sort data = transform2;
by seqn;
run;

/*Merge urinary albumin and creatinine*/

data merged;
  merge transform2(in=a) alb_cr_g(in=b);
  by seqn;
  if a and b;
run;

data merged2;
set merged;
rename urxums = albumin_urine urxucr = creatinine_urine urdact = album_creat_ratio;
drop albumin_new albumin_new2 creat_album creat_new albuminuria albumin_log creatinine_log plogC cot_new2 cot_new3 smk_status smk_exp smk_status2 
	 creat_new urxuma urxcrs pb_u pb_u_comm albumin UAR;
run;

data merged3;
set merged2;
albumin_mgdL = albumin_urine /10;
run;

/*============================================*/
/*Log transform biomarkers and creatinine (10/04/2025)*/

/*Natural log-transform uric acid and albumin*/

data merged4;
set merged3;
	albumin_log = log(albumin_mgdL);
	creatinine_log = log (creatinine_urine);
run;

proc reg data = merged4;
	model creatinine_log = age racethnic gender bmi;
	output out=merged5 p=plogC;
run;

data merged6;
set merged5;
albuminuria = .;
if albumin_mgdl < 30 then albuminuria = 0;
else if albumin_mgdl GE 30 then albuminuria = 1;
run;

proc sort data = merged6;
by CVD_risk;
run;

proc freq data = merged6;
table albuminuria*cvd_risk;
run;

data diss.aim1_v5;
set merged6;
run;

proc univariate data = merged6;
var plogC;
run;
