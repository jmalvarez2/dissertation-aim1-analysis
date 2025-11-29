/*Purpose: Variable selection for imputation and multivariate model adjustments */

libname DISS "H:\UTHealth Dissertation\SAS Datasets"; 
RUN;

proc contents data = diss.aim1_v4;
run;

/*====================================================================================================================*/
/*====================================================================================================================*/

/*Continuous variables along with categorical variables*/

title 'Stepwise Regression on Cardiovascular Disease Risk';
proc logistic data=diss.aim1_v4 outest=test covout;
	model CVD_risk(event='1')= gender age age_new age_strata racethnic education pir pir_new diabetes_new bmi bmi_new cotinine cot_new plogC 
		/	selection=stepwise
			slentry=0.25
			slstay=0.30
			details
			lackfit;
	output out=pred p=phat lower=lcl upper=ucl
		predprob=(individual crossvalidate);
	ods output Association=Association;
run;

/*====================================================================================================================*/
/*====================================================================================================================*/

title 'Backward Elimination on Cardiovascular Disease Risk';
proc logistic data=diss.aim1_v4;
	model CVD_risk(event='1')= gender age age_new age_strata racethnic education pir pir_new diabetes_new bmi bmi_new cotinine cot_new plogC 
		/	selection=backward fast slstay=0.30
			ctable pprob=(0 to 1 by 0.05);
run;

/*====================================================================================================================*/
/*====================================================================================================================*/

title 'LASSO Elimination on Cardiovascular Disease Risk';
proc glmselect data = diss.aim1_v4;
  class gender age_new age_strata racethnic education pir_new diabetes_new BMI_new cot_new;
  	model CVD_risk = gender age age_new age_strata racethnic education pir pir_new diabetes_new bmi bmi_new cotinine cot_new plogC
		/ selection=lasso (choose=aic stop=none)
		  stats=all;
run;

/*====================================================================================================================*/
/*====================================================================================================================*/

title 'HPCENSELECT GROUP LASSO Elimination on Cardiovascular Disease Risk';
proc hpgenselect data=diss.aim1_v4 lassorho=.80 lassosteps=30;
  class gender age_new age_strata racethnic education pir_new diabetes_new BMI_new cot_new;
      model CVD_risk (descending) = gender age age_new age_strata racethnic education pir pir_new diabetes_new bmi bmi_new cotinine cot_new plogC / distribution=binary;
SELECTION METHOD=lasso (choose=aic stop=none) DETAILS=all;     
run;

/*====================================================================================================================*/
/*====================================================================================================================*/

title 'Forward Elimination on Cardiovascular Disease Risk';
proc hplogistic data=diss.aim1_v4;
  class gender age_new age_strata racethnic education pir_new diabetes_new BMI_new cot_new;
      model CVD_risk (descending) = gender age age_new age_strata racethnic education pir pir_new diabetes_new bmi bmi_new cotinine cot_new plogC ;
SELECTION METHOD=forward (select=aic choose=aic stop=none);     
run;

/*From GLMSELECT (LASSO Elimination) variables selected include:
  age, age_strata, age_new, race/ethnicity, diabetes_new, BMI_new, log-adjusted creatinine, education, cotinine, BMI, PIR, and gender*/
