/* Rolling FM regression predicting out-of-sample returns
This code is for panel data with monthly frequency date (calmonth) for each stock (code).
this code uses FM regression with a rolling window (n) starting from (start_date) to (end_date)
to predict expected return (r_pred). Loadings from one window is used to predict return for the 
first month after the end of the window (out-of-sample prediction). No need to worry about 
standard errors since not used in prediction.

SYNTAX: %roll_fm_pred(&data, &by_id, &n, &date, &win_beg, &win_end, &rhs_vars)
INPUTS:
    input dataset: &data
    input regression level vars (cross-section): &by_id (region, etc. assumed CHAR format)
    input window size (number of months): &n
    input date variable: &date
    input date for the first estimation (n months before this date is the first window): &est_beg
    input date for the last estimation (assuming ending date of sample): &est_end
        note: &date, &est_beg, &est_end must have SAS date format at monthly end: ex. 14feb2004.
        note: the window for the first prediction should then be: intnx("month",&est_beg,-&n,"E") to intnx("month",&est_beg,-1,"E")
    input LHS variable for regression: &lhs_var
    input RHS variables (assuming reg model: ret = RHS_vars): &rhs_vars
OUTPUTS:
    output dataset: &out
    predicted return: r_pred
    other output variables in &out: &by_id
*/


*********************************************;

%macro rollingfmregpredict(data=,out=,by_id=,n=,date=,est_beg=,est_end=,lhs_var=, rhs_vars=);

* prepare data for regression;
proc sort data=&data;by &by_id &date; run;
data temp;
    set &data (keep = &by_id code &date &lhs_var &rhs_vars);
    run;

data &out;
    set &data (keep = &by_id code &date &lhs_var);
    r_pred = .;
    nobs_reg = .;
    if _n_>=1 then delete;
    run;

* number of firms each cross-section;
proc means data = temp noprint;
  var &date;
  by &by_id &date;
  output out=nobs_tot n=nobs_tot;
  run;
    
* define rolling window;
%macro rollwindow;
%let b=%sysfunc(inputn(&est_beg,date9.));
%let e=%sysfunc(inputn(&est_end,date9.));
%LET T = %sysfunc(intck(month, &b, &e));

%do i = 0 %to &T;

    * FM rolling regression;
    proc reg data=temp outest=regout noprint;
        where &date between intnx("month","&est_beg"d, &i-&n, "E") and intnx("month", "&est_beg"d, &i-1, "E"); *window size n looking back;
        model &lhs_var = &rhs_vars / adjrsq;
        by &by_id &date;
        run;
    
    * if regout is empty then proceed to next loop;
    data regout;
      retain empty 1;
      set regout;
      run;
      
    proc sql;
      select count(*) into: obs from regout;
      quit;
    
    %if &obs=0 %then %goto continue;
    
    data regout;
      set regout(keep=&by_id &date intercept &rhs_vars _P_ _EDF_);
      by &by_id;
      nobs_reg = _P_ + _EDF_;
      run;
      
    proc means data=regout mean noprint;
        var intercept &rhs_vars nobs_reg;
        by &by_id;
        output out=loadings (drop = _freq_ _type_) mean= ;
        run;
    
    data loadings1;
        set loadings(drop=nobs_reg);
        _model_="r_pred";
		    _type_="PARMS";
		    run;
    
    data nobs_reg;
      set loadings(keep=&by_id nobs_reg);
      by &by_id;
      date = intnx("month","&est_beg"d, &i, "E");
      run;
      
    * use loadings from rolling regression to predict next month return;
    proc score data=temp score=loadings1 out=r_pred type=parms;
        var &rhs_vars;
        by &by_id;
        run;
    
    data r_pred;
        set r_pred (keep = &by_id code &date &lhs_var r_pred);
        by &by_id;
        where &date = intnx("month","&est_beg"d, &i, "E");
        run;
    
    * append all predictions;
    data r_pred;
      merge r_pred nobs_reg;
      by &by_id;
      run;
      
    proc append base=&out data=r_pred force;run;

    %continue:
%end;

* merge predicted returns with number of firms each cross-section;
proc sort data=&out;by &by_id &date;run;
data &out;
  merge &out (in=in1) nobs_tot(in=in2 keep=&by_id &date nobs_tot);
  by &by_id &date;
  if in1=1;
  run;


%mend;
%rollwindow;

%mend;
