clear all

cd "/Users/shagunkhetan/Desktop/Data"

********************************************
**********//Regressions
********************************************

//>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** Treatment JIO Control ALL
//>>>>>>>>>>>>>>>>>>>>>>>>>>>

import delimited "final_panel.csv", clear
encode district_name, gen(district_id)
keep if year == 2016 | year ==2018
destring mean_nightlight, gen(nightlight)

drop if t_c_stata == 0
gen treat = (t_c_stata == 1)
gen post = (year == 2018)
gen did = treat*post
gen treat_int = ppc_tj/100
gen did_int = treat_int*post

/*
READ SCORE
*/

//no controls
reg read_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col1

//controls
reg read_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col2

//controls and fixed effects
reghdfe read_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col3


//treatment intensity
reghdfe read_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col4


/*
MATH SCORE
*/

//no controls
reg math_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col5

//controls
reg math_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col6

//controls and fixed effects
reghdfe math_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col7

//treatment intensity
reghdfe math_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col8


esttab col1 col2 col3 col4 col5 col6 col7 col8 using "didall.tex", replace ///
	booktabs ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("{\footnotesize Average Treatment Effect of Jio 4G Shock(Main Specification)}") ///
	rename(post "Post" treat "Treat" did "DiD" treat_int "Treat Intensity" did_int "DiD Intensity" mobile "Mobile" hh_size "HH Size" class "Class" oos_rate "Drop Rate" nightlight "Nightlights") ///
	drop(_cons) ///
	stats(N r2 controls fe, label("Observations" "$R^2$" "Controls" "FE")) ///
	note("\textit{Notes:} Standard errors clustered at the district level.") ///
	se(4)


//>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** Treatment JIO Control JIO
//>>>>>>>>>>>>>>>>>>>>>>>>>>>

import delimited "final_panel.csv", clear

keep if year == 2016 | year == 2018
destring mean_nightlight, gen(nightlight)
encode district_name, gen(district_id)
drop if t_c_stat == 0

gen treat = (t_c_stat == 1)
gen post = (year == 2018)
gen did = treat*post
gen treat_int = ppc_tj/100
gen did_int = treat_int*post


/*
READ SCORE
*/

//no controls
reg read_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col1

//controls
reg read_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col2

//controls and fixed effects
reghdfe read_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col3

//treatment intensity
reghdfe read_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col4

/*
MATH SCORE
*/

//no controls
reg math_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col5

//controls
reg math_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col6

//controls and fixed effects
reghdfe math_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col7

//treatment intensity
reghdfe math_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col8

esttab col1 col2 col3 col4 col5 col6 col7 col8 using "didjio.tex", replace ///
	booktabs ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("{\footnotesize Average Treatment Effect of Jio 4G Shock(Treatment Jio, Control Jio)}") ///
	rename(post "Post" treat "Treat" did "DiD" treat_int "Treat Intensity" did_int "DiD Intensity" mobile "Mobile" hh_size "HH Size" class "Class" oos_rate "Drop Rate" nightlight "Nightlights") ///
	drop(_cons) ///
	stats(N r2 controls fe, label("Observations" "$R^2$" "Controls" "FE")) ///
	note("\textit{Notes:} Standard errors clustered at the district level.") ///
	se(4)


//>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** Treatment ALL Control ALL
//>>>>>>>>>>>>>>>>>>>>>>>>>>>

import delimited "final_panel.csv", clear

keep if year == 2016 | year == 2018
destring mean_nightlight, gen(nightlight)
encode district_name, gen(district_id)
drop if ta_c_stata == 0

gen treat = (ta_c_stata == 1)
gen post = (year == 2018)
gen did = treat*post
gen treat_int = ppc_tj/100
gen did_int = treat_int*post


/*
READ SCORE
*/

//no controls
reg read_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col1

//controls
reg read_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col2

//controls and fixed effects
reghdfe read_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col3

//treatment intensity
reghdfe read_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col4

/*
MATH SCORE
*/

//no controls
reg math_code post treat did, vce(cluster district_id)
estadd local controls "No"
estadd local fe "No"
eststo col5

//controls
reg math_code post treat did mobile hh_size class oos_rate nightlight, vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "No"
eststo col6

//controls and fixed effects
reghdfe math_code treat did mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col7

//treatment intensity
reghdfe math_code treat_int did_int mobile hh_size class oos_rate nightlight, absorb(year state_code) vce(cluster district_id)
estadd local controls "Yes"
estadd local fe "Yes"
eststo col8

esttab col1 col2 col3 col4 col5 col6 col7 col8 using "didtall.tex", replace ///
	booktabs ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("{\footnotesize Average Treatment Effect of Jio 4G Shock(Treatment All Control All)}") ///
	rename(post "Post" treat "Treat" did "DiD" treat_int "Treat Intensity" did_int "DiD Intensity" mobile "Mobile" hh_size "HH Size" class "Class" oos_rate "Drop Rate" nightlight "Nightlights") ///
	drop(_cons) ///
	stats(N r2 controls fe, label("Observations" "$R^2$" "Controls" "FE")) ///
	note("\textit{Notes:} Standard errors clustered at the district level.") ///
	se(4)

clear

	

