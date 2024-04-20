/**********************************************************************
This code is FM regression with model: &lhs_var = &rhs_vars.
Inputs:
    input data: &data
    input c-s level variable: &by_id (ex. &by_id)
    input date variable: date
    input LHS var: &lhs_var
    input RHS vars: &rhs_vars
Outputs:
    output data: &out_full, contains detail stats for each var;
                 &out_short, contains only estimate, tstat, adjrsq, nobs;

Syntax: %fmreg(data=,out_full=,out_short=,by_id=,lhs_var=,rhs_vars=)
**********************************************************************/
%macro fmreg(data=,out_full=,out_short=,by_id=,lhs_var=,rhs_vars=,w=);
data temp111;
    set &data (keep = &by_id code date &lhs_var &rhs_vars &w);
    run;
    
** 1. run cross-sectional OLS regressions;
proc sort data=temp111; by &by_id date;run;

%if "&w" ne "" %then %do;
    ods listing close;
    ods output parameterestimates= FB;
    proc reg data=temp111 outest=adjrsq;
        by &by_id date;
        model &lhs_var = &rhs_vars / adjrsq vif;
        weight &w;
        quit;
    ods listing;
%end;
%else %do;
    ods listing close;
    ods output parameterestimates= FB;
    proc reg data=temp111 outest=adjrsq;
        by &by_id date;
        model &lhs_var = &rhs_vars / adjrsq vif;
        quit;
    ods listing;
%end;


** 2. Newey-West lag of 4;
proc sort data=FB; by &by_id variable; run;

%let lags = 4;
ods output parameterestimates = out1;
ods listing close;
proc model data = FB;
    by &by_id variable;
    instruments / intonly;
    estimate= a;
    fit estimate / gmm kernel=(bart,%eval(&lags+1),0) vardef=n; run;
    quit;
ods listing;

data out1;
    set out1(drop=esttype parameter);
    by &by_id;
    *where variable ~= "Intercept";
    run;
    
*proc print data=out1;run;

** 3. total number of firms per cross-section;
proc means data=temp111 noprint;
  var date;
  by &by_id date;
  output out=nobs_tot n=nobs_tot;
  run;
  
** 4. adjusted r-square & number of observation used;
data adjrsq;
  merge nobs_tot(keep=&by_id date nobs_tot) adjrsq;
  by &by_id date;
  nobs_reg=_P_ + _EDF_;
  pct_nobs_used_for_reg = nobs_reg / nobs_tot;
  run;
  
proc sort data=adjrsq; by &by_id; run;
proc means data=adjrsq noprint;
  output out=out2 mean=;
  var _adjrsq_ nobs_reg nobs_tot pct_nobs_used_for_reg;
  by &by_id;
  run;

*proc print data=out2;run;

** 5. VIF;
proc sort data=FB; by &by_id variable; run;
proc means data=FB noprint;
  output out=out3 mean=;
  var varianceinflation;
  by &by_id variable;
  run;
  
data out3;
  set out3(drop=_type_ _freq_ rename=(varianceinflation=vif));
  by &by_id variable;
  *where variable ~= "Intercept";
  run;
  
*proc print data=out3;run;

** 6. together;
data out12;
  merge out1
        out2(keep=&by_id _adjrsq_ nobs_reg nobs_tot pct_nobs_used_for_reg rename=(_adjrsq_=adjrsq));
  by &by_id;
  run;

proc sort data=out12;by &by_id variable;run;
data &out_full;
  merge out12 out3;
  by &by_id variable;
  run;

data out22; 
    set out2(keep=&by_id _adjrsq_ rename=(_adjrsq_=estimate));
    variable = "z_adjrsq";
    run;

data out23;
    set out2(keep=&by_id nobs_reg rename=(nobs_reg=estimate));
    variable = "zz_nobs_reg";
    run;

data &out_short;
    length variable $20.;
    set out1(keep = &by_id estimate tvalue variable) out22 out23;
    run;
    
proc sort data=&out_short;by &by_id variable;run;
    
%mend;
