/*
Codefile 3
     Vice Versa: The Decoupling of Content and Topic Heterogeneity in Collusion Research
     W. Benedikt Schmal, Research Group Lead at Walter Eucken Institute, Freiburg (DE)
     schmal@eucken.de      https://sites.google.com/view/wbschmal
     Last changes: November 20, 2023
*/

* General Settings
clear all
set more off
set scheme s1mono
cd"YOUR PATH"

import delim var_data.csv, clear // import data exported from "STM_code_main.R"


* 3.5 Reciprocal topic prevalence over time
*** VECTOR AUTOREGRESSIVE MODEL

ren datayear year

forval j = 1/21 {
	gen lt`j' = log(topic`j')
	bys year: egen mlt`j' = mean(lt`j')
	bys year: egen m`j' = mean(topic`j')
	bys year: gen lm`j' = log(m`j')
}
	
cap drop ext rul welf attma act inta out

* Exogenous topics:
gen ext = topic1 + topic2 + topic6
* Competition analysis topics:
gen rul = topic4 + topic7 + topic11 + topic12 + topic18 + topic19

* generate logarithmic variables
foreach j in ext rul {
	bys year: egen m_`j' = mean(`j')
	gen ml_`j' = log(m_`j')
}

cap drop count
bys year: gen count = _n
drop if count > 1

tsset year



*** START HERE with VAR regs *******
qui var ml_ext ml_rul, lags(1/3)
varsoc ml_ext ml_rul, maxlag(2)
varsoc ml_ext ml_rul, maxlag(3)
varstable, graph dlabel title("Eigenvalues of the VAR")
graph export unitcircle_var.png, replace
varlmar
varnorm

*Johannsen Test
varsoc ml_ext ml_rul, maxlag(2) // >> 2 Lags suggested
varsoc ml_ext ml_rul, maxlag(3) // >> 2 Lags suggested
vecrank ml_ext ml_rul, lags(2)  // >> Cointegration rank 1 for main specification of VAR
vecrank ml_ext ml_rul, lags(3)  // >> No cointegration any longer. Toda & Yamamoto (1995) works

* Engle-Granger Manual Test for Cointegration:
reg ml_ext ml_rul
	eststo reg1
	esttab reg1, se tex
predict resid, r
dfuller resid

* Granger Causality
qui var ml_ext ml_rul, lags(1/3)
vargranger


* generate IRF and FEVD plots for Figure 12 in the main text
qui var ml_ext ml_rul, lags(1/3)
irf create VAR, step(8) set(myirf1, replace)

irf graph oirf, impulse (ml_rul) response(ml_ext) yline(0) ysc(r(-.75 0.5))
graph export irf_rul_to_ext_var.png, replace

irf graph oirf, impulse (ml_ext) response(ml_rul) yline(0) ysc(r(-.75 0.5))
graph export irf_ext_to_rul_var.png, replace

irf graph fevd, impulse (ml_ext) response(ml_rul) yline(0) ysc(r(-.75 0.5))
graph export fevd_ext_to_rul_var.png, replace

irf graph fevd, impulse (ml_rul) response(ml_ext) yline(0) ysc(r(-.75 0.5))
graph export fevd_rul_to_ext_var.png, replace

irf table fevd, impulse (ml_rul) response(ml_ext) stderror level(95)


**** END OF CODE FILE ****
