/* create temporary score file for quantile regression model. */

/* get work path */
%let wpath=%sysfunc(pathname(work));


/* create data */
data casuser.class;
set sashelp.class;
run;

/* create score file in work path */
filename sfile "&wpath/score_qr.sas" ; /*need to guarantee uniqueness*/

/* create model */
proc qtrselect data=CASUSER.CLASS;
	class Sex;
	model Height=Sex Age Weight / quantiles=(0.05 0.5 0.95);
	code file="&wpath/score_qr.sas";
run;

/* score table */
data casuser.class_score;
set casuser.class;
%include "&wpath/score_qr.sas";
run;

/* delete score file */
%macro check(file);
%if %sysfunc(fileexist(&file)) ge 1 %then %do;
   %let rc=%sysfunc(filename(temp,&file));
   %let rc=%sysfunc(fdelete(&temp));
%end; 
%else %put The file &file does not exist;
%mend check; 

%check("&wpath/score_qr.sas");

/* clear file name */
filename sfile CLEAR;

