/* Rolling FM regression predicting out-of-sample earnings (monthly frequency)
This is for panel data with monthy frequency date (date) for each stock (code).
this uses FM regression with a rolling window (n) starting from (start_year) to (end_year)
to predict expected earnings (ret_hat). Loadings from one window is used to predict return for the 
five years after the end of the window (out-of-sample prediction). A gap can exist between the 
end-of-est-window and the year to predict. For example, years from t-10 to t-1 can be used to predict
earnings in year t+2. No need to worry about standard errors since not used in prediction.

SYNTAX: %mroll_pred(&data_reg, &data_score, &out, code, &n, date, &est_beg, &est_end, &rhs_vars)
INPUTS:
    input dataset for regression to estimate coeff: &data_reg (LHS and RHS vars all winsorized so coeff not biased by extremes)
    input dataset for proc score to estimate forecast: &data_score (LHS and RHS vars not winsorized so forecasts remain variation)
    input stock code: code
    input window size (number of years): &n
    input year for the first estimation (n years before this date is the first window): &est_beg
    input year for the last estimation (assuming ending year of sample): &est_end
        note: date, &est_beg, &est_end must be years: ex.2004.
        note: the window for the first prediction should then be: &est_beg-&n to &est_beg
    input LHS variable for regression: &lhs_var
    input RHS variables (assuming reg model: ret = RHS_vars): &rhs_vars
OUTPUTS:
    output dataset: &out, including predicted ret (ret_hat), code
                    coeff, including time-series average of coeff and t-stat 
*/


**********************************************************************************************************;
%macro mroll_pred(data_reg=,data_score=,out=,n=,est_beg=,est_end=,lhs_var=,rhs_vars=,weight=);

    * prepare data for regression;
    proc sort data=&data_reg out=pred(keep = code date &lhs_var &rhs_vars &weight); by date; run;
    proc sort data=&data_score out=pred1(keep = code date &lhs_var &rhs_vars); by date; run;
    
    data &out;
        length code $12.; *max length for ds code;
        code = "";
        date = .;
        format date MMDDYY10.;
        &lhs_var._hat = .;
        if _n_>=1 then delete;
        run;
    
    data coeff;
        date = .;
        format date MMDDYY10.;
        Intercept = .;
        _adjrsq_ = .;
        %do v = 1 %to %sysfunc(countw(&rhs_vars), %str( ));
    	    %let var = %scan(&rhs_vars, &v, %str( ));
          &var = .;
        %end;
        if _n_>=1 then delete;
        run;
        
    * define rolling window;
    %LET T = %sysfunc(intck(month, &est_beg, &est_end)); *number of month in between;
    %do i = 0 %to &T;
    
      * FM rolling regression;
        proc reg data=pred outest=regout noprint;
            where date between intnx("month", &est_beg, &i-&n, "e") and intnx("month", &est_beg, &i-1, "e"); *window size n looking back;
            &lhs_var._hat: model &lhs_var = &rhs_vars / adjrsq;
            by date;
            %if "&weight" ne "" %then %do;
              weight &weight;
            %end;
            run;
      
      * if regout is empty then proceed to next loop;
      data regout;
        retain empty 1;
        set regout;
        run;
        
      proc sql noprint;
        select count(*) into: obs from regout;
        quit;
      
      %if &obs=0 %then %goto continue;
      
      * use loadings from rolling regression to predict next year earnings;        
      proc means data=regout mean noprint;
          var intercept &rhs_vars _adjrsq_;
          output out=out(drop = _freq_ _type_) mean= ;
          run;
      
      data out1;
          set out(drop=_adjrsq_);
          _model_="&lhs_var._hat";
  		    _type_="PARMS";
  		    run;
      
      data pred2;
        set pred1;
        where date = intnx("month", &est_beg, &i, "e");
        run;

      proc score data=pred2 score=out1 out=ib(keep = code date &lhs_var._hat) type=parms;
          var &rhs_vars;
          run;

      data ib;
        set ib;
        date = intnx("month", &est_beg, &i, "e");
      run;
      
      * append all predictions;
      proc append base=&out data=ib force;run;
      
      * collect coeff and t-stats;
      data out2;
        set out;
        date = intnx("month", &est_beg, &i, "e");
        run;
      
      * append all coeff and t-stats;
      proc append base=coeff data=out2 force;run;
      
      %continue:
    %end;

%mend;
