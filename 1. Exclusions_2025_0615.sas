/*==================================================================================================================================================*/
/*Pupose: Exclude missing information and other criteria and determine final analytic sample for Aim 1*/

/*Created by: Juan M. Alvarez*/

/*Date created: 06/15/2025*/
/*Date edited: 10/25/2025*/

/*Input dataset: H:\UTHealth Dissertation\UTHealth Dissertation\SAS Datasets\DISS.DISS_ALLVARS_RAW_2025_0316*/
/*Output datasets: diss.aim1*/
/*==================================================================================================================================================*/

libname DISS "H:\UTHealth Dissertation\UTHealth Dissertation\SAS Datasets"; /*Edit path*/
run;

proc contents data = DISS.DISS_ALLVARS_RAW_2025_0316; 
run;

data edit; 
 set DISS.DISS_ALLVARS_RAW_2025_0316;
	if age < 20 then group = 1;
	else if age GE 20 then group = 2;
run;

/*==================================================================================================================================================*/
/*Exclusions for Aim 1*/
/*==================================================================================================================================================*/

/*==================================================================================================================================================*/
																/*Exposures*/

data edit1; 
 set edit;
	if mpah = . or pfde = . or pfhxs = . or pfna = . or pfua = . then delete; /*56,556*/
	if dinp = . or mbzp = . or mcpp = . or mehhp = . or mehp = . or meohp = . or mep = . or mibp = . or mnbp = . then delete; /*52,403*/
	if nap1 = . or nap2 = . or OHFlu3 =. or OHFlu2 = . or OHPHE1 = . or PYR1 = . then delete; /*53,193*/
run;

/*N = 1,856 (78,456 observations missing any EDC)*/

/*==================================================================================================================================================*/
													/*Demographic/Lifestyle variables*/

data edit2;
 set edit1;
	if gender  = . then delete; /*0 missing*/
	if age = . then delete; /*0 missing*/
	if educ_youth = . and educ_adult = . then delete; /*0 missing*/
	if diabetes = . then delete; /*0 missing*/
	if PIR = . then delete; /*171 missing*/
	if creatinine = . then delete; /*1 missing*/
	if triglyceride = . then delete; /*4 missing*/
	if cotinine = . then delete; /*0 missing*/
	if BMI = . then delete; /*28 missing*/
run;

/*1,657 (199 observations missing demographic/lifestyle variables) */

/*==================================================================================================================================================*/
													        /*Previous diagnoses or pregnancy status*/

proc freq data = edit2;
table rhd143 mcq220;
run;

data edit3;
set edit2;

	if hrt_fail = 1 or cor_heart_disease = 1 or angina = 1 or hrt_att = 1 or stroke = 1 then delete; /*114 with previous CVD diagnoses*/
	if RHD143 = 1 then delete; /*11 pregnant women*/
	if MCQ220 = 1 then delete; /*104 cancer diagnosis*/
run;

/*1,451 (206 observations with previous diagnoses or pregnant at time of survey) */

/*==================================================================================================================================================*/
														      /*Mediators*/

data edit4;
 set edit3;
	if albumin = . then delete; /*0 missing*/
	if uric_acid = . then delete; /*0 missing*/
run;

/*N = 1,451 (0 observations missing albumin or uric acid measurements*/

/*==================================================================================================================================================*/
																/*Outcomes*/

data edit5;
 set edit4;
	if dbp_1 = . and dbp_2 = . and dbp_3 = . and dbp_4 = . then delete; /*47 missing*/
	if sbp_1 = . and sbp_2 = . and sbp_3 = . and sbp_4 = . then delete; /*47 missing*/
	if tot_chol = . then delete; /*0 missing*/
run;

/*N = 1,404 (47 observations missing blood pressure or total cholesterol measurements*/

data diss.aim1;
 set edit5;
run;

/*==================================================================================================================================================*/
/*Final analytic sample for Aim 1: N = 1,404*/
/*==================================================================================================================================================*/
