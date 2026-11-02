clear all
set maxvar 15000
use "/Users/shagunkhetan/Desktop/Data/IAHR74DT/IAHR74FL.DTA"

rename hv243a phone
rename shdistri district 
rename hv024 state

// tab phone, nol
// tab district, nol

gen wt = hv005/ 1000000
sum wt

collapse (mean) phone [aw = wt], by(state district) 
codebook phone
br


export delimited using "nfhs_phonepen.csv", replace

