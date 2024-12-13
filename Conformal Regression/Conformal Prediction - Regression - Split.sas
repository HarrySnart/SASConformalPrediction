/* 
This script attempts to implement Split Conformal Prediction as demonstrated in the blog
"Tidymodels and Conformal Prediction" found here: https://theclarkeorbit.github.io/tidymodels-and-conformal-prediction.html

We import a partially prepared dataset on indian trade and perform some basic data preparation for modelling then train
a Gradient Boosting Regression model on all covariates in order to predict "value" which is the 
trade value for exports and imports in millions of USD

We then perfrom Split Conformal Prediction to generate a prediction band with 90% coverage using 
a nonparametric post-training method. 

 */

cas casauto;

/* import data */
proc import datafile='/export/home/users/sukhsn/ConformalPrediction/Regression/india_trade_data.csv' dbms=csv out=trade;run;

proc print data=trade(obs=5);run;

/* preprocess the data */
data casuser.trade;
set trade;
drop country;
if country ne 'NA' or gdp ne . ;/*remove missing values*/
value = log(value+1); 
gdp = log(gdp);
population = log(population);
/* note in R blog they also normalise the covariates */
run; 

/* partition data with stratified sampling on value*/
proc partition data=CASUSER.TRADE partind samppct=60 samppct2=20;
	output out=casuser.trade_part partindname=partition;
run;

data casuser.trade_part;
set casuser.trade_part;
length part $5;
if partition=0 then part='val';
if partition=1 then part='train';
if partition=2 then part='test';
run;

proc sql;
select distinct part,count(*) as cnt
from casuser.trade_part group by part;quit;

data casuser.train;
set casuser.trade_part;
where part='train';
drop part partition;
run;

data casuser.test;
set casuser.trade_part;
where part='test';
drop part partition;
run;

data casuser.val;
set casuser.trade_part;
where part='val';
drop part partition;
run;

/* fit linear regression model */
filename sfile 
	'/export/home/users/sukhsn/ConformalPrediction/Regression/reg_model.sas';

proc regselect data=CASUSER.TRAIN;
	class trade_direction;
	model value=trade_direction year gdp population /;
	code file=sfile;
run;

filename sfile CLEAR;

/* 
training...
RMSE = 1.4957
R-Squared =  0.721

results look similar to R model. I've noticed that year should be a categorical variable not interval.

 */

/* score the test datast */

data casuser.reg_score_tst;
set casuser.test;
%include '/export/home/users/sukhsn/ConformalPrediction/Regression/reg_model.sas';
run;

/* calculate fit stats on test dataset */
proc assess data=CASUSER.REG_SCORE_TST nbins=10;
	target value;
	input P_value;
	ods output LIFTREGInfo=WORK._lift_temp;
run;

proc sgplot data=WORK._lift_temp noautolegend;
	title 'Regression Lift Chart (Target = value)';
	xaxis label='Population percentile';
	yaxis label='Response';
	series x=depth y=meanT /lineattrs=(color=very_light_blue);
	band x=depth lower=minT upper=maxT / transparency=0.5 legendlabel='Target' 
		name="tband" fillattrs=(color=very_light_blue);
	series x=depth y=meanP /lineattrs=(color=dark_green);
	band x=depth lower=minP upper=maxP / transparency=0.5 legendlabel='Predicted' 
		name="pband" fillattrs=(color=dark_green);
	keylegend "tband" "pband" / across=4 noborder position=TopRight 
		location=inside;
run;


/* fit stats on test:

rmse = 1.51
rsq = 0.71
mae = 1.14

marignally worse than R blog, but explained by different treatment of 'Year'
 */


/* train a gradient boosting regression */

proc gradboost data=CASUSER.TRAIN outmodel=casuser.gb_model;
	target value / level=interval;
	input year gdp population / level=interval;
	input trade_direction / level=nominal;
	autotune tuningparameters=(ntrees samplingrate vars_to_try(init=100) 
		learningrate lasso ridge) objective=ase fraction=0.3 maxtime=%sysevalf(60*60);
	savestate rstore=casuser.gb_score_model;
	id _all_;
run;

/* predict test dataset with grad boost */
proc astore;
	score data=CASUSER.TEST out=casuser.gb_score_tst rstore=CASUSER.GB_SCORE_MODEL;
run;

proc assess data=CASUSER.GB_SCORE_TST nbins=10;
	target value;
	input P_value;
run;

/* 

rmse=1.05
rsq=0.86
mae=0.75

unsurprisingly, this is better than the r model as we've done autotuning.

 */


/* plot actual v pred with line of best fit */
ods graphics / noborder;
proc sgplot data=casuser.gb_score_tst;
reg x=value y=P_value / filledoutlinedmarkers  nolegfit ;

title 'Actual v Predicted Value';
run;

/* implement conformalised prediction

n.b. may be different depending on whether the model is an astore,datastep or model table
 */

/* split conformal recipe:

1 - split data and get row counts
2 - train model 
3 - predict validation dataset
4 - calc residuals on validation dataset
5 - calculate the conformal quantile,q


 */


/* BEGIN CONFORMAL PREDICTION */

/* get row count of validation dataset */
proc sql;select count(*) into :val_cnt from casuser.val;quit;

/* score validation dataset */
proc astore;
	score data=CASUSER.VAL out=casuser.gb_score_val rstore=CASUSER.GB_SCORE_MODEL;
run;

/* calculate residuals */
data resid;
set casuser.gb_score_val;
residual = abs(value - P_value);
run;

/* sort residuals */
proc sort data=resid;by  residual;run;

data resid_smallest;
set resid;
rowid + 1;
keep rowid residual;
run;

proc sort data=resid;by descending residual;run;

data resid_biggest;
set resid;
rowid + 1;
keep rowid residual;
run;

/* calculate q */

%let alpha=0.1;

data _null_;
q = round((1-&alpha.)*(&val_cnt. + 1));
call symput('q',q);
run;

%put &q.;

/* find qth residuals */

proc sql;select residual into :c_lower from resid_smallest  where rowid=&q.;quit;

proc sql;
select residual into :c_upper from resid_biggest  where rowid=&q.;quit;

/* calculate upper and lower bounds for test dataset */

data casuser.gb_score_tst_cp;
set casuser.gb_score_tst;
P_Upper = P_Value+&c_lower.;
P_Lower = value  - &c_lower.;/*changed from lower*/
run;

/* END CONFORMAL PREDICTION */


/* visualize upper and lower bounds */

proc sgplot data=casuser.gb_score_tst_cp noautolegend;
scatter x=value y = p_value;
band lower=p_lower upper=value x=value  / transparency=0.7  type=step noextend ;
band lower=p_value upper=p_upper x=p_value / transparency=0.7  type=step noextend; 
reg x=value y=p_value / nomarkers curvelabel='Regression' ;
reg x=value y=p_lower / nomarkers curvelabel='CP Lower Bound';
reg x=value y=p_upper / nomarkers curvelabel='CP Upper Bound';

yaxis min=-3 max=12;
xaxis min=0 max=12;
title 'Conformal Bands for Gradient Boosting Regression';
footnote 'This plot shows the relationship between actual and predicted values for the regression model and the coverage  of the conformal interval';
run;

/* check coverage */

data coverage;
set casuser.gb_score_tst_cp;
if p_lower le value and value le p_upper then within_band=1;
else within_band = 0;
run;

title 'Coverage of Conformal Bands';
proc sql;
select sum(within_band) as within_band,count(*) as total, sum(within_band)/count(*) as coverage format percent. from coverage;quit;

proc print data=casuser.gb_score_tst_cp(obs=15) l;
title 'Regression Point Estimates with Conformal Prediction Interval on Hold-Out Dataset';
var year trade_direction gdp population value p_lower p_value p_upper;
footnote1 'Gradient Boosting Regression on Indian Economic Trade Data';
footnote2 'Upper and Lower bounds of Prediction interval calculated using Conformal Prediction with 90% Coverage';
label year='Year' trade_direction = 'Trade Type' gdp='Log GDP of Trade Country'
population = 'Log Population of Trade Country' value='Observed Trade Amount ($M)'
p_value = 'Predicted trade value ($M)'
p_lower='Conformal Lower Bound ($M)'
p_upper='Conformal Upper Bound ($M)';
run;

/* on monday - add copyvars to gradboost statement for country

create boxplot / violin plot for confidence of trade amount by import/export by country
 */
