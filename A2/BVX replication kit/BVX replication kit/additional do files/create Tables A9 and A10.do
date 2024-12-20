
/*======================================================================

	Create Tables A9 and A10 (Panel B)
	
	Robustness analysis for timing analysis: around Narrative Crises

========================================================================*/


foreach type1 in peak_B peak_N trough_bankcredit trough_corpcredit {
	use "$root/data/additional data/Timing of stocks and bonds.dta", clear
	gen year_narrative_`type1' = narrative_year
	keep country `type1'_year `type1'_month year_narrative_`type1'
	keep if `type1'_year!=.
	save "$root/data/temp/list_`type1'_narrative", replace
}


use "$root/data/additional data/Timing of stocks and bonds.dta", clear
gen RR_tm = ym(RR_year, RR_month)
gen narrative_tm = ym(narrative_year, narrative_month)
keep country narrative_* RR_*
rename narrative_year year_narrative
rename narrative_month month_narrative
drop if year_narrative==.
save "$root/data/temp/narrative_dates_narrative", replace


use "$root/data/additional data/Narrative Crisis List, Panics List, and BVX List.dta", clear
rename year_bankeqdecline year_revised
gen panic_month = month(date(panic_year_month,"MY"))
gen panic_year = year(date(panic_year_month,"MY"))
gen panic_tm = ym(panic_year, panic_month)
gen narrative_year = jointlist
keep country narrative_year panic panic_tm panic_year panic_month
format %tm panic_tm
drop if panic==0 | panic_tm==.
replace country="UK" if country=="U.K."
replace country="US" if country=="U.S."
rename narrative_year year_narrative
drop if year_narrative==.
save "$root/data/temp/panic_dates_narrative", replace








/*
* calculate future peak of credit spreads (both the year-month of peak and the amount of the increase)
* calculate when credit spreads spike by 1% or 2% relative to their previous trough

* Note: This code here takes about 10-20 minutes to run. Can be uncommented.


use "$root/data/master_data_monthly.dta", clear

gen date = dofm(tm)
gen year = year(date)
gen month = month(date)
drop if year<1870
drop date

foreach type1 in peak_B peak_N trough_bankcredit trough_corpcredit {
	rename (year month) (`type1'_year `type1'_month)
	merge 1:1 country `type1'_year `type1'_month using "$root/data/temp/list_`type1'_narrative"
	sort _merge country `type1'_year `type1'_month
	by _merge: gen `type1' = _n if _merge==3
	drop _merge 
	rename (`type1'_year `type1'_month) (year month) 
}
sort country year month
rename (trough_bankcredit trough_corpcredit) (trough_sB trough_sC)


foreach type1 in "B" "C" {
	
	gen s`type1'_trough_value=.
	gen s`type1'_peak_value=.
	gen s`type1'_peak_tm=.
	gen s`type1'_1pctspike_tm=.
	gen s`type1'_2pctspike_tm=.
	gen s`type1'_10spike_tm=.
	gen s`type1'_25spike_tm=.
	gen s`type1'_50spike_tm=.
	gen s`type1'_99spike_tm=.
	gen s`type1'_1sdspike_tm=.
	

	local nn_end = 42
	if "`type1'"=="B" local nn_end = 42
	if "`type1'"=="C" local nn_end = 21
	
	
	forval nn = 1/`nn_end' {
		gen s`type1'_value = sB if trough_s`type1'==`nn'
		replace s`type1'_value = s`type1' if s`type1'_value[_n-1]!=. & s`type1'_value[_n-48]==.
		gen s`type1'_value_past = s`type1' if trough_s`type1'==`nn'
		forval qq = 1/120 {
			quietly by country: replace s`type1'_value_past = s`type1' if s`type1'_value_past[_n+1]!=.
		}
		egen stdev_s`type1'_value_temp = sd(s`type1'_value_past)
		
		egen s`type1'_max_value_temp = max(s`type1'_value)
		gen s`type1'_max_tm_temp2 = tm if s`type1'_max_value_temp==s`type1'_value
		egen s`type1'_max_year_temp = min(s`type1'_max_tm_temp2)
		replace s`type1'_peak_value = s`type1'_max_value_temp if trough_s`type1'==`nn'
		replace s`type1'_peak_tm = s`type1'_max_year_temp if trough_s`type1'==`nn'
		replace s`type1'_trough_value = s`type1'_value if trough_s`type1'==`nn'
		gen s`type1'_trough_value_temp = s`type1'_trough_value  if trough_s`type1'==`nn'
		replace s`type1'_trough_value_temp = s`type1'_trough_value_temp[_n-1] if s`type1'_trough_value_temp[_n-1]!=. & s`type1'_trough_value_temp[_n-48]==.
		
		gen s`type1'_1pctspike_temp2 = tm if inrange(s`type1'_value - s`type1'_trough_value_temp,0.01,9999) 
		egen s`type1'_1pctspike_temp = min(s`type1'_1pctspike_temp2)
		replace s`type1'_1pctspike_tm = s`type1'_1pctspike_temp if trough_s`type1'==`nn'
			
		gen s`type1'_2pctspike_temp2 = tm if inrange(s`type1'_value - s`type1'_trough_value_temp,0.02,9999) 
		egen s`type1'_2pctspike_temp = min(s`type1'_2pctspike_temp2)
		replace s`type1'_2pctspike_tm = s`type1'_2pctspike_temp if trough_s`type1'==`nn'
		
		foreach pct1 in "10" "25" "50" "99" {	
			gen s`type1'_`pct1'spike_temp2 = tm if inrange((s`type1'_value - s`type1'_trough_value_temp)/(s`type1'_max_value_temp - s`type1'_trough_value_temp),0.`pct1',9999) 
			egen s`type1'_`pct1'spike_temp = min(s`type1'_`pct1'spike_temp2)
			replace s`type1'_`pct1'spike_tm = s`type1'_`pct1'spike_temp if trough_s`type1'==`nn'	
		}
			
		gen s`type1'_1sdspike_temp2 = tm if inrange((s`type1'_value - s`type1'_trough_value_temp)/stdev_s`type1'_value_temp,1,9999)
		egen s`type1'_1sdspike_temp = min(s`type1'_1sdspike_temp2)
		replace s`type1'_1sdspike_tm = s`type1'_1sdspike_temp if trough_s`type1'==`nn'
			
		drop s`type1'_value s*_max_value_temp s*_max_tm_temp2 s*_max_year_temp *_temp* *temp s*_value_past stdev_s*_value_temp
		format %tm s*_peak_tm s*spike_tm
		

	}
}

drop if trough_sB==. & trough_sC==.

keep country tm year year_narrative* month trough_sB trough_sC s*_trough_value s*_peak_value s*_peak_tm s*spike_tm


save "$root/data/temp/credit_spread_peaks - around Narrative Crises", replace
*/







/*
* calculate when stocks decline by 30%
* calculate future trough of stocks (both the year-month of the trough and the value of the pk-to-trough decline)

* Note: This code here takes about 20-30 minutes to run. Can be uncommented.



use "$root/data/master_data_monthly.dta", clear
gen date = dofm(tm)
gen year = year(date)
gen month = month(date)
drop if year<1870
drop date

foreach type1 in peak_B peak_N trough_bankcredit trough_corpcredit {
	rename (year month) (`type1'_year `type1'_month)
	merge 1:1 country `type1'_year `type1'_month using "$root/data/temp/list_`type1'_narrative"
	sort _merge country `type1'_year `type1'_month
	by _merge: gen `type1' = _n if _merge==3
	drop _merge 
	rename (`type1'_year `type1'_month) (year month) 
}
sort country year month

* calculate future trough of stocks (both year-month & value)
* calculate when stocks decline by 30% 

foreach type1 in "B" "N" {
	gen `type1'_min_value=.
	gen `type1'_min_tm=.
	
	forval dcl = 20(5)60 {
		gen `type1'_`dcl'decline_tm=.
		gen `type1'_`dcl'decline_sB=.
		gen `type1'_`dcl'decline_sC=.
	}
	
	
	if "`type1'"=="B" local nn_end = 155
	if "`type1'"=="N" local nn_end = 152
	
	forval nn = 1/`nn_end' {
		
		gen `type1'_returns = r`type1' if peak_`type1'==`nn'
		replace `type1'_returns = 0 if peak_`type1'==`nn' & r`type1'==.
		replace `type1'_returns = r`type1' if `type1'_returns[_n-1]!=. & `type1'_returns[_n-48]==.
		gen cumul_`type1'_returns = sum(`type1'_returns)
		replace cumul_`type1'_returns = . if `type1'_returns ==.
		
		egen `type1'_min_value_temp = min(cumul_`type1'_returns)
		gen `type1'_min_tm_temp2 = tm if `type1'_min_value_temp ==cumul_`type1'_returns
		egen `type1'_min_year_temp = min(`type1'_min_tm_temp2)
		replace `type1'_min_value = `type1'_min_value_temp if peak_`type1'==`nn'
		replace `type1'_min_tm = `type1'_min_year_temp if peak_`type1'==`nn'
		
		forval dcl = 20(5)60 {
			gen `type1'_`dcl'decline_temp2 = tm if cumul_`type1'_returns<-0.`dcl'
			egen `type1'_`dcl'decline_temp = min(`type1'_`dcl'decline_temp2)
			replace `type1'_`dcl'decline_tm = `type1'_`dcl'decline_temp if peak_`type1'==`nn'
			
			gen `type1'_`dcl'decline_sB_temp2 = sB if cumul_`type1'_returns<-0.`dcl'
			replace `type1'_`dcl'decline_sB_temp2 = . if `type1'_`dcl'decline_temp!=`type1'_`dcl'decline_temp2
			egen `type1'_`dcl'decline_sB_temp = min(`type1'_`dcl'decline_sB_temp2)
			replace `type1'_`dcl'decline_sB = `type1'_`dcl'decline_sB_temp if peak_`type1'==`nn'
			local dclminus5 = `dcl'-5
			if `dcl'>20 replace `type1'_`dcl'decline_sB = `type1'_`dclminus5'decline_sB if `type1'_`dcl'decline_sB<`type1'_`dclminus5'decline_sB & `type1'_`dclminus5'decline_sB!=.
			
			gen `type1'_`dcl'decline_sC_temp2 = sC if cumul_`type1'_returns<-0.`dcl'
			replace `type1'_`dcl'decline_sC_temp2 = . if `type1'_`dcl'decline_temp!=`type1'_`dcl'decline_temp2
			egen `type1'_`dcl'decline_sC_temp = min(`type1'_`dcl'decline_sC_temp2)
			replace `type1'_`dcl'decline_sC = `type1'_`dcl'decline_sC_temp if peak_`type1'==`nn'
			if `dcl'>20 replace `type1'_`dcl'decline_sC = `type1'_`dclminus5'decline_sC if `type1'_`dcl'decline_sB<`type1'_`dclminus5'decline_sC & `type1'_`dclminus5'decline_sC!=.
			
		}
	
		drop `type1'_returns cumul_`type1'_returns `type1'_min_value_temp `type1'_min_tm_temp2 `type1'_min_year_temp *_temp*
	
	
	}
	
	format %tm *_min_tm *decline_tm
}

drop if peak_B==. & peak_N==.


keep country year_narrative* tm year month peak_B peak_N B_min_value B_min_tm B_20decline_tm B_25decline_tm ///
B_30decline_tm B_35decline_tm B_40decline_tm B_45decline_tm B_50decline_tm B_55decline_tm ///
B_60decline_tm N_min_value N_min_tm N_20decline_tm N_25decline_tm N_30decline_tm N_35decline_tm ///
N_40decline_tm N_45decline_tm N_50decline_tm N_55decline_tm N_60decline_tm B_*decline_s*



save "$root/data/temp/stock_declines - around Narrative Crises", replace
*/








* Create Tables A9 and A10 (Panel B)


use "$root/data/temp/stock_declines - around Narrative Crises", clear

gen year_narrative = year_narrative_peak_B
replace year_narrative = year_narrative_peak_N if year_narrative==.
drop year_narrative_*
order year_narrative

gen peak_B_tm = tm if peak_B!=.
gen peak_B_year = year if peak_B!=.
gen peak_B_month = month if peak_B!=.
gen peak_N_tm = tm if peak_N!=.
gen peak_N_year = year if peak_N!=.
gen peak_N_month = month if peak_N!=.
order country year_narrative peak_B_* peak_N_*
drop tm year month

collapse (lastnm) peak_* B_min_value B_min_tm B_*decline* N_min_value N_min_tm N_*decline_tm, by(country year_narrative)
replace B_min_tm = . if B_min_value==.
replace N_min_tm = . if N_min_value==.

save "$root/data/temp/stock_declines_v2 - around Narrative Crises", replace



capture file close _all
file open myfile using "$root/output/Tables A9 and A10 - around Narrative Crises.txt", write text replace



** calculate time difference between bank30decline and nonfin30decline

gen diff30decline = N_30decline_tm - B_30decline_tm
replace diff30decline = 12 if B_30decline_tm!=. & N_30decline_tm==. & peak_N!=.
replace diff30decline = -12 if N_30decline_tm!=. & B_30decline_tm==. & peak_B!=.
summ diff30decline
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff30decline>0 & diff30decline!=.
local pos1 = r(N)
count if diff30decline==0 & diff30decline!=.
local zero1 = r(N)
count if diff30decline<0 & diff30decline!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)

file write myfile "Table A9 (Panel B: columns 1-3)" _n _n

display "30% bank decline vs. 30% nonfin decline"
display "`mean1' `tstat1' `N'"
display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"

file write myfile "`mean1' `tstat1' `N' " 
file write myfile "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' " 
file write myfile "30% bank decline vs. 30% nonfin decline" _n





** calculate time difference between bank_peak and nonfin_peak

gen diff_peaks = peak_N_tm - peak_B_tm
replace diff30decline = 12 if peak_B_tm!=. & peak_N_tm==.
replace diff30decline = -12 if peak_N_tm!=. & peak_B_tm==.
summ diff_peaks
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff_peaks>0 & diff_peaks!=.
local pos1 = r(N)
count if diff_peaks ==0 & diff_peaks!=.
local zero1 = r(N)
count if diff_peaks <0 & diff_peaks!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)


display "bank stock peak vs. nonfin stock peak"
display "`mean1' `tstat1' `N'"
display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"

file write myfile  "`mean1' `tstat1' `N' "
file write myfile  "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
file write myfile  "bank stock peak vs. nonfin stock peak" _n




** calculate time from peak to trough for bank stocks

gen diff_pktotr = B_min_tm - peak_B_tm if B_min_tm!=. & peak_B_tm!=.
summ diff_pktotr
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff_pktotr>=24 & diff_pktotr!=.
local pos1 = r(N)
count if diff_pktotr<24 & diff_pktotr!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)


display "time from peak to trough for bank stocks"
display "`mean1' `tstat1' `N'"
display "`pos1' `neg1' `pctpos' `bernoulli2'"


file write myfile  "`mean1' `tstat1' `N' "
file write myfile  "`pos1' XXX `neg1' `pctpos' `bernoulli2' "
file write myfile  "time from peak to trough for bank stocks" _n






** SUBSAMPLES: calculate time difference between bank30decline and nonfin30decline

gen EM = 1 if country=="Argentina" | country=="Brazil" | country=="Chile" | country=="Colombia" | ///
			country=="Czech" | country=="Greece" | country=="Hungary" | ///
            country=="Hong Kong" |  country=="India" | country=="Indonesia" | country=="Israel" | country=="Korea" | ///
			country=="Malaysia" | country=="Mexico" | country=="Peru" | country=="Philippines" | ///
			country=="Russia" | country=="South Africa" | country=="Taiwan" | country=="Thailand" | ///
			country=="Turkey" | country=="Venezuela" 
replace EM = 0 if EM==.

file write myfile _n "Table A10 (Panel B: columns 1-5)" _n _n

foreach subsample1 in "inrange(year_narrative,1870,1939)==0" "inrange(year_narrative,1940,2016)==0"  ///
"inrange(year_narrative,1940,2016)==0 | EM==0" "inrange(year_narrative,1940,2016)==0 | EM==1" "inrange(year_narrative,1940,2005)==0 | EM==1" { 
	capture drop diff30decline
	gen diff30decline = N_30decline_tm - B_30decline_tm 
	replace diff30decline = 12 if B_30decline_tm!=. & N_30decline_tm==. & peak_N!=.
	replace diff30decline = -12 if N_30decline_tm!=. & B_30decline_tm==. & peak_B!=.
	replace diff30decline = . if `subsample1'
	summ diff30decline
	local N = r(N)
	local mean1 = r(mean)
	local sd1 = r(sd)
	local tstat1 = `mean1' / `sd1' * sqrt(`N')
	count if diff30decline>0 & diff30decline!=.
	local pos1 = r(N)
	count if diff30decline==0 & diff30decline!=.
	local zero1 = r(N)
	count if diff30decline<0 & diff30decline!=.
	local neg1 = r(N)
	local pctpos = `pos1'/(`pos1' + `neg1')
	local nonzero1 = (`pos1' + `neg1')
	bitesti `nonzero1' `pos1' 0.5
	local bernoulli2 = r(p_u)


	display "EXCLUDE: `subsample1'"
	display "`mean1' `tstat1' `N'"
	display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"
	

	file write myfile "`mean1' `tstat1' `N' "
	file write myfile "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
	file write myfile "EXCLUDE: `subsample1'" _n
}




** calculate time difference between bank30decline and RRstartdate

merge 1:1 country year_narrative using "$root/data/temp/narrative_dates_narrative"

capture drop diff30decline
gen diff30decline = RR_tm - B_30decline_tm
gen date_B_30decline = dofm(B_30decline_tm)
gen year_B_30decline = year(date_B_30decline)
replace diff30decline = (RR_year - year_B_30decline)*12 if diff30decline==.
summ diff30decline
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff30decline>0 & diff30decline!=.
local pos1 = r(N)
count if diff30decline==0 & diff30decline!=.
local zero1 = r(N)
count if diff30decline<0 & diff30decline!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)


file write myfile _n "Table A9 (Panel A: columns 2-3)" _n _n

display "30% bank decline vs. RR narrative start"
display "`mean1' `tstat1' `N'"
display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"

file write myfile "`mean1' `tstat1' `N' "
file write myfile "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
file write myfile "30% bank decline vs. RR narrative start" _n




** calculate time difference between bank30decline and earliest narrative start date

capture drop diff30decline
gen diff30decline = narrative_tm - B_30decline_tm
replace diff30decline = (year_narrative - year_B_30decline)*12 if diff30decline==.
summ diff30decline
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff30decline>0 & diff30decline!=.
local pos1 = r(N)
count if diff30decline==0 & diff30decline!=.
local zero1 = r(N)
count if diff30decline<0 & diff30decline!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)

display "30% bank decline vs. earliest narrative start"
display "`mean1' `tstat1' `N'"
display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"

file write myfile  "`mean1' `tstat1' `N' "
file write myfile  "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
file write myfile  "30% bank decline vs. earliest narrative start" _n






** calculate time difference between bank30decline and panic date

use "$root/data/temp/stock_declines_v2 - around Narrative Crises", clear
merge 1:1 country year_narrative using "$root/data/temp/panic_dates_narrative"
** note that _merge==1 are events without Panics
** note that _merge==2 are events without BE Declines

capture drop diff30decline
gen diff30decline = panic_tm - B_30decline_tm
gen date_B_30decline = dofm(B_30decline_tm)
gen year_B_30decline = year(date_B_30decline)
summ diff30decline
local N = r(N)
local mean1 = r(mean)
local sd1 = r(sd)
local tstat1 = `mean1' / `sd1' * sqrt(`N')
count if diff30decline>0 & diff30decline!=.
local pos1 = r(N)
count if diff30decline==0 & diff30decline!=.
local zero1 = r(N)
count if diff30decline<0 & diff30decline!=.
local neg1 = r(N)
local pctpos = `pos1'/(`pos1' + `neg1')
local nonzero1 = (`pos1' + `neg1')
bitesti `nonzero1' `pos1' 0.5
local bernoulli2 = r(p_u)


file write myfile _n "Table A9 (Panel A: columns 1,5,4,7,6)" _n _n

display "30% bank decline vs. Panic month"
display "`mean1' `tstat1' `N'"
display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"

file write myfile "`mean1' `tstat1' `N' "
file write myfile "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
file write myfile "30% bank decline vs. Panic month" _n




use "$root/data/temp/credit_spread_peaks - around Narrative Crises", clear

gen year_narrative = year_narrative_trough_bankcredit
replace year_narrative = year_narrative_trough_corpcredit if year_narrative==.
drop year_narrative_*
order year_narrative

gen trough_bankcredit_tm = tm if trough_sB!=.
gen trough_bankcredit_year = year if trough_sB!=.
gen trough_bankcredit_month = month if trough_sB!=.
gen trough_corpcredit_tm = tm if trough_sC!=.
gen trough_corpcredit_year = year if trough_sC!=.
gen trough_corpcredit_month = month if trough_sC!=.

order country year_narrative trough* sB* sC*
drop tm year month

collapse (lastnm) trough* sB* sC*, by(country year_narrative)
replace sB_peak_tm = . if sB_peak_value ==.
replace sC_peak_tm = . if sC_peak_value ==.

merge 1:1 country year_narrative using "$root/data/temp/stock_declines_v2 - around Narrative Crises"
sort country year_narrative
drop _merge



** calculate time difference between bank30decline and credit spread spike
		
foreach type1 in B C {
	foreach creditvar in s`type1'_1pctspike_tm s`type1'_2pctspike_tm { 
		capture drop diff30decline
		gen diff30decline = `creditvar' - B_30decline_tm
		replace diff30decline = 12 if B_30decline_tm!=. & `creditvar' ==. & trough_s`type1'!=.
		replace diff30decline = -12 if `creditvar'!=. & B_30decline_tm==. & peak_B!=.
		summ diff30decline
		local N = r(N)
		local mean1 = r(mean)
		local sd1 = r(sd)
		local tstat1 = `mean1' / `sd1' * sqrt(`N')
		count if diff30decline>0 & diff30decline!=.
		local pos1 = r(N)
		count if diff30decline==0 & diff30decline!=.
		local zero1 = r(N)
		count if diff30decline<0 & diff30decline!=.
		local neg1 = r(N)
		local pctpos = `pos1'/(`pos1' + `neg1')
		local nonzero1 = (`pos1' + `neg1')
		bitesti `nonzero1' `pos1' 0.5
		local bernoulli2 = r(p_u)
		
		display "bank30decline vs. `creditvar'"
		display "`mean1' `tstat1' `N'"
		display "`pos1' `zero1' `neg1' `pctpos' `bernoulli2'"
		
		file write myfile  "`mean1' `tstat1' `N' "
		file write myfile  "`pos1' `zero1' `neg1' `pctpos' `bernoulli2' "
		file write myfile  "bank30decline vs. `creditvar'" _n
	}
}



file close myfile
*/
