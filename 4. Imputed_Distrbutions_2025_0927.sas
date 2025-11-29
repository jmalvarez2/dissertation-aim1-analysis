/*==================================================================================================================================================*/
/*Purpose: Import imputed dataset and adjust urinary concentrations to creatinine and create categorical variables. Recreate UAR
		   variable*/

/*Created by: Juan M. Alvarez*/

/*Date created: 09/27/2025*/
/*Date edited: 10/04/2025*/

/*Input dataset: H:\UTHealth Dissertation\SAS Datasets\aim1_imputed_2025_1004.xlsx*/
/*Output datasets: aim.imputed_aim1_2025_1004*/
/*==================================================================================================================================================*/

libname aim "H:\UTHealth Dissertation\SAS Datasets";
run;

proc import datafile="H:\UTHealth Dissertation\SAS Datasets\aim1_imputed_2025_1004.xlsx"
	out=imputed
	dbms=xlsx
	replace;
	getnames=yes;
run;

/*==================================================================================================================================================*/
/*STEP 2: Creatinine-adjusted concentrations*/

/*Creatinine adjusted based on methods employed by O'Brien et al., 2016 (urinary concentrations)*/

data imputed2;
 set imputed;
	pC=exp(plogC);
	Cratio=creatinine/pC;

	/*Phthalates*/
	MBzP_adj = (MBzP/Cratio) * 0.01;
	MCPP_adj = (MCPP/Cratio) * 0.01;
	MEHHP_adj = (MEHHP/Cratio) * 0.01;
	MEHP_adj = (MEHP/Cratio) * 0.01;
	MEOHP_adj = (MEOHP/Cratio) * 0.01;
	MEP_adj = (MEP/Cratio) * 0.01;
	MnBP_adj = (MnBP/Cratio) * 0.01;
	MiBP_adj = (MiBP/Cratio) * 0.01;

	/*PAHs*/
	NAP1_adj = (NAP1/Cratio) * 0.01;
	NAP2_adj = (NAP2/Cratio) * 0.01;
	OHFlu2_adj = (OHFlu2/Cratio) * 0.01;
	OHFlu3_adj = (OHFlu3/Cratio) * 0.01;
	OHPHE1_adj = (OHPHE1/Cratio) * 0.01;
	PYR1_adj = (PYR1/Cratio) * 0.01;
run;

proc univariate data = imputed2;
var MBzP_adj MCPP_adj MEHHP_adj MEHP_adj MEOHP_adj MEP_adj MnBP_adj MiBP_adj NAP1_adj NAP2_adj OHFlu2_adj OHFlu3_adj OHPHE1_adj PYR1_adj;
run;

/*==================================================================================================================================================*/
/*STEP 4: Log-transfomed creatinine-adjusted concentrations*/

data imputed3; 
 set imputed2;
	MBzP_log_adj=log(MBzP_adj);
	MCPP_log_adj=log(MCPP_adj);
	MEHHP_log_adj=log(MEHHP_adj);
	MEHP_log_adj=log(MEHP_adj);
	MEOHP_log_adj=log(MEOHP_adj);
	MEP_log_adj=log(MEP_adj);
	MnBP_log_adj=log(MnBP_adj);
	MiBP_log_adj=log(MiBP_adj);
	NAP1_log_adj=log(NAP1_adj);
	NAP2_log_adj=log(NAP2_adj);
	OHFlu2_log_adj=log(OHFlu2_adj);
	OHFlu3_log_adj=log(OHFlu3_adj);
	OHPHE1_log_adj=log(OHPHE1_adj);
	PYR1_log_adj=log(PYR1_adj);
run;

/*==================================================================================================================================================*/
/*STEP 5: Create quartiles for log-transformed, adjusted EDC concentrations*/

proc means data = imputed3 p25 p50 p75;
var PFHxS_log PFNA_log PFDE_log MBzP_log_adj MCPP_log_adj MEHHP_log_adj MEHP_log_adj 
    MEOHP_log_adj MEP_log_adj MnBP_log_adj MiBP_log_adj NAP1_log_adj NAP2_log_adj 
    OHFlu2_log_adj OHFlu3_log_adj OHPHE1_log_adj PYR1_log_adj;
run;

data imputed4;
    set imputed3;

    /* PFAS */
    if PFHxS_log <= -0.3638693 then PFHxS_log_quart = 1;
    else if PFHxS_log <= 0.1739533 then PFHxS_log_quart = 2;
    else if PFHxS_log <= 0.7395507 then PFHxS_log_quart = 3;
    else PFHxS_log_quart = 4;

    if PFNA_log <= -0.5108256 then PFNA_log_quart = 1;
    else if PFNA_log <= -0.1165338 then PFNA_log_quart = 2;
    else if PFNA_log <= 0.2851789 then PFNA_log_quart = 3;
    else PFNA_log_quart = 4;

    if PFDE_log <= -2.0402208 then PFDE_log_quart = 1;
    else if PFDE_log <= -1.6607312 then PFDE_log_quart = 2;
    else if PFDE_log <= -1.1711830 then PFDE_log_quart = 3;
    else PFDE_log_quart = 4;

    /* Phthalates */
    if MBzP_log_adj <= -3.6907161 then MBzP_log_adj_quart = 1;
    else if MBzP_log_adj <= -3.0868128 then MBzP_log_adj_quart = 2;
    else if MBzP_log_adj <= -2.4501499 then MBzP_log_adj_quart = 3;
    else MBzP_log_adj_quart = 4;

    if MCPP_log_adj <= -4.3014397 then MCPP_log_adj_quart = 1;
    else if MCPP_log_adj <= -3.7556040 then MCPP_log_adj_quart = 2;
    else if MCPP_log_adj <= -2.9906379 then MCPP_log_adj_quart = 3;
    else MCPP_log_adj_quart = 4;

    if MEHHP_log_adj <= -3.0376279 then MEHHP_log_adj_quart = 1;
    else if MEHHP_log_adj <= -2.5503835 then MEHHP_log_adj_quart = 2;
    else if MEHHP_log_adj <= -2.0036023 then MEHHP_log_adj_quart = 3;
    else MEHHP_log_adj_quart = 4;

    if MEHP_log_adj <= -5.0530894 then MEHP_log_adj_quart = 1;
    else if MEHP_log_adj <= -4.2849777 then MEHP_log_adj_quart = 2;
    else if MEHP_log_adj <= -3.5965235 then MEHP_log_adj_quart = 3;
    else MEHP_log_adj_quart = 4;

    if MEOHP_log_adj <= -3.4482126 then MEOHP_log_adj_quart = 1;
    else if MEOHP_log_adj <= -2.9872990 then MEOHP_log_adj_quart = 2;
    else if MEOHP_log_adj <= -2.4776963 then MEOHP_log_adj_quart = 3;
    else MEOHP_log_adj_quart = 4;

    if MEP_log_adj <= -1.7575975 then MEP_log_adj_quart = 1;
    else if MEP_log_adj <= -0.8839214 then MEP_log_adj_quart = 2;
    else if MEP_log_adj <= 0.0722373 then MEP_log_adj_quart = 3;
    else MEP_log_adj_quart = 4;

    if MnBP_log_adj <= -2.9939054 then MnBP_log_adj_quart = 1;
    else if MnBP_log_adj <= -2.3762978 then MnBP_log_adj_quart = 2;
    else if MnBP_log_adj <= -1.8254715 then MnBP_log_adj_quart = 3;
    else MnBP_log_adj_quart = 4;

    if MiBP_log_adj <= -3.2271385 then MiBP_log_adj_quart = 1;
    else if MiBP_log_adj <= -2.6751786 then MiBP_log_adj_quart = 2;
    else if MiBP_log_adj <= -2.1183625 then MiBP_log_adj_quart = 3;
    else MiBP_log_adj_quart = 4;

    /* PAHs */
    if NAP1_log_adj <= 1.8590794 then NAP1_log_adj_quart = 1;
    else if NAP1_log_adj <= 2.5807914 then NAP1_log_adj_quart = 2;
    else if NAP1_log_adj <= 3.6563493 then NAP1_log_adj_quart = 3;
    else NAP1_log_adj_quart = 4;

    if NAP2_log_adj <= 3.1387936 then NAP2_log_adj_quart = 1;
    else if NAP2_log_adj <= 3.8287436 then NAP2_log_adj_quart = 2;
    else if NAP2_log_adj <= 4.5088414 then NAP2_log_adj_quart = 3;
    else NAP2_log_adj_quart = 4;

    if OHFlu2_log_adj <= 0.2677762 then OHFlu2_log_adj_quart = 1;
    else if OHFlu2_log_adj <= 0.7427765 then OHFlu2_log_adj_quart = 2;
    else if OHFlu2_log_adj <= 1.4449675 then OHFlu2_log_adj_quart = 3;
    else OHFlu2_log_adj_quart = 4;

    if OHFlu3_log_adj <= -0.7843016 then OHFlu3_log_adj_quart = 1;
    else if OHFlu3_log_adj <= -0.2682891 then OHFlu3_log_adj_quart = 2;
    else if OHFlu3_log_adj <= 0.5427314 then OHFlu3_log_adj_quart = 3;
    else OHFlu3_log_adj_quart = 4;

    if OHPHE1_log_adj <= -0.2390705 then OHPHE1_log_adj_quart = 1;
    else if OHPHE1_log_adj <= 0.1846950 then OHPHE1_log_adj_quart = 2;
    else if OHPHE1_log_adj <= 0.6669905 then OHPHE1_log_adj_quart = 3;
    else OHPHE1_log_adj_quart = 4;

    if PYR1_log_adj <= -0.3617796 then PYR1_log_adj_quart = 1;
    else if PYR1_log_adj <= 0.1138930 then PYR1_log_adj_quart = 2;
    else if PYR1_log_adj <= 0.6930870 then PYR1_log_adj_quart = 3;
    else PYR1_log_adj_quart = 4;
run;

proc freq data=imputed4;
    tables MBzP_log_adj_quart--PYR1_log_adj_quart / nocum nopercent;
run;

proc contents data=imputed4;
run;

data imputed5;
 set imputed4;

drop PFHxS_comm PFNA_comm PFDE_comm MBzP_comm MCPP_comm MEHHP_comm MEHP_comm MEOHP_comm MEP_comm MnBP_comm MiBP_comm NAP1_comm NAP2_comm 
	 OHFlu2_comm OHFlu3_comm OHPHE1_comm PYR1_comm ind_MBzP ind_MCPP ind_MEHHP ind_MEHP ind_MEOHP ind_MEP ind_MiBP ind_MnBP ind_NAP1 ind_NAP2 ind_OHFlu2
	 ind_OHFlu3 ind_OHPHE1 ind_PFDE ind_PFHxS ind_PFNA ind_PYR1 BPAARM BPACSZ BPAEN1 BPAEN2 BPAEN3 BPAEN4 BPXCHR BPXDAR BPXDB BPXML1 BPXPLS BPXPTY
	 BPXPULS BPXSAR CD_U CD_U_COMM DINP DINP_COMM MPAH MPAH_COMM PEASCCT1 PEASCST1 PEASCTM1 PFUA PFUA_COMM;
run;

proc sort data = imputed5; by seqn; run;

/*Import blood albumin variable to construct UAR variable*/
data uar;
  merge imputed5(in=a) 
        biopro_g(keep=seqn LBXSAL rename=(LBXSAL=albumin_blood) in=b);
  by seqn;
  if a and b; /* keep only rows present in both datasets */
run;

data imputed6;
set uar;

UAR = uric_acid / albumin_blood;
run;

/*Create permanent dataset*/

data aim.imputed_aim1_2025_1004;
set imputed6;
run;
