**# TABLE 1
///////////////////////////////////////////////////////////////////////////////
///////    START PREPARING TABLE 1
///////////////////////////////////////////////////////////////////////////////


cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
use sle_cross_sectional, clear

cap drop ISNRPS_class2 
recode ISNRPS_class (1 2 = 1 "3 or 3+5") (3 4 = 2 "4 or 4+5"), gen(ISNRPS_class2)

cap drop N_LOWC3
gen N_LOWC3 = LOWC3
	gen eGFR=.
	replace eGFR=141*            (sCr/0.9)^(cond(sCr<=0.9,-0.411, -1.209))*0.993^age if sex==1 & ethnicity!=1
	replace eGFR=141*1.018*      (sCr/0.7)^(cond(sCr<=0.7,-0.329, -1.209))*0.993^age if sex==0 & ethnicity!=1
	replace eGFR=141*1.159*      (sCr/0.9)^(cond(sCr<=0.9,-0.411, -1.209))*0.993^age if sex==1 & ethnicity==1
	replace eGFR=141*1.018*1.159*(sCr/0.7)^(cond(sCr<=0.7,-0.329, -1.209))*0.993^age if sex==0 & ethnicity==1
	label var eGFR "Time-varying eGFR (mL/min/1.73m2) by CKD-EPI"

qui: table (var) (LOWC3) , ///
		stat(count N_LOWC3) ///
		stat(count ageatbiopsy) stat(mean ageatbiopsy) stat(sd ageatbiopsy) ///
		stat(fvfrequency sex ethnicity obesity diabetes hypertension CADorstrokeorPAD heartfailure cancer) ///
		stat(fvpercent sex ethnicity obesity diabetes hypertension CADorstrokeorPAD heartfailure cancer) ///
        stat(count sCr eGFR proteinuria C3 C4 ) ///
        stat(mean sCr eGFR proteinuria C3 C4) ///
        stat(sd sCr eGFR proteinuria C3 C4) ///
		stat(fvfrequency ISNRPS_class2 APL TMA) ///
		stat(fvpercent ISNRPS_class2 APL TMA) ///
		stat(count LDHn220 haptoglobinn36 platelets ggs AI CI IFTA ah) ///
		stat(mean LDHn220 haptoglobinn36 platelets ggs AI CI IFTA ah) ///
		stat(sd LDHn220 haptoglobinn36 platelets ggs AI CI IFTA ah) ///
		stat(fvfrequency  pdn hcq csa tac cyc aza mmf rtx belimumab none) ///
		stat(fvpercent pdn hcq csa tac cyc aza mmf rtx belimumab none) ///
        nformat(%3.0f count) ///
        nformat(%3.1f mean sd) ///
		nformat(%3.1f fvfrequency) ///
		nformat(%3.1f fvpercent)
		

// Totals
collect remap LOWC3[.m]=total
collect label dim total "All Patients"
collect style header total, title(label) level(hide)
               
		
// Change the label of LOWC3
collect label dim LOWC3 "PI-LowC3", modify
		
// don't show stat description along rows
collect style header result, level(hide)
* collect preview

// put frequency and percent in the same column as mean and sd, respectively
collect recode result fvfrequency = mean fvpercent = sd
collect layout (var) (total#result LOWC3[0 1]#result)



// display percent (i.e. sd column) as %
foreach x in 	sex ethnicity obesity diabetes hypertension CADorstrokeorPAD ///
	heartfailure cancer ISNRPS_class2 APL TMA  ///
	pdn hcq csa tac cyc aza mmf rtx belimumab none  {
	collect style cell result[sd]#var[LOWC3 `x'], sformat("%s%%")
	 }
	 

// display SD within brackets
collect style cell result[sd]#var[ageatbiopsy sCr eGFR proteinuria C3 C4 LDHn220 haptoglobinn36 platelets ggs AI CI IFTA ah], sformat("(%s)")



// display frequency (i.e. mean column) as integer
collect style cell result[mean]#var[sex ethnicity obesity diabetes hypertension CADorstrokeorPAD ///
	heartfailure cancer ISNRPS_class2 APL TMA  ///
	pdn hcq csa tac cyc aza mmf rtx belimumab non], nformat(%4.0f)



// calculate-save P values for continuous vars and tag vars
foreach x in ageatbiopsy sCr eGFR proteinuria C3 C4 LDHn220 haptoglobinn36 platelets ggs AI CI IFTA ah {
	qui: collect r(p), tag(var[`x']): ranksum `x', by(LOWC3)
	 }

	 
// calculate-save P values for categorical vars and tag vars
foreach x in sex ethnicity obesity diabetes hypertension CADorstrokeorPAD ///
	heartfailure cancer ISNRPS_class2 APL TMA  ///
	pdn hcq csa tac cyc aza mmf rtx belimumab none {
	local pval = chi2tail(r(df), r(chi2_adj))
	qui: collect r(p_exact), tag(var[`x']): tab LOWC3 `x', exact
	 }	 



// attach columns for result levels p and p_exact
collect layout (var) (total#result LOWC3[0 1]#result result[p p_exact])

// recode var levels to get P valu in the first row of categorical vars
foreach x in sex ethnicity obesity diabetes hypertension CADorstrokeorPAD ///
	heartfailure cancer  APL TMA  ///
	pdn hcq csa tac cyc aza mmf rtx belimumab none  {
	collect recode var `x' = 0.`x', fortags(result[p_exact])
	 }

collect recode var ISNRPS_class2 = 1.ISNRPS_class2, fortags(result[p_exact])
	 
	

// display P values with three digits
collect style cell result[p p_exact], nformat(%4.3f)

// label p-values results as "P value"
collect label levels result p "P value", modify
collect label levels result p_exact "P value", modify
collect style header result[p], level(label)
collect style header result[p_exact], level(label)

// display P value from the two type of stat test in a single column
collect recode result p_exact = p 


// label vars
collect label levels var N_LOWC3 "Number of patients"
collect label levels var ageatbiopsy "Age, yrs", modify
collect label levels var sex "Sex", modify
collect label levels var ethnicity "Ethnicity", modify
collect label levels var obesity "Obesity", modify
collect label levels var diabetes "Diabetes", modify
collect label levels var hypertension "Hypertension", modify
collect label levels var CADorstrokeorPAD "Cardio- or Cerebro-vascular Disease", modify
collect label levels var heartfailure "Heart Failure", modify
collect label levels var cancer "Cancer", modify
collect label levels var obesity "Obesity", modify
collect label levels var sCr "sCr, mg/dL", modify
collect label levels var eGFR "eGFR, mL/min/1.73m2", modify
collect label levels var proteinuria "Proteinuria, g/day", modify
collect label levels var C3 "C3, mg/dL", modify
collect label levels var C4 "C4, mg/dL", modify
collect label levels var ISNRPS_class2 "ISN/RPS Class", modify
collect label levels var APL "APL", modify
collect label levels var TMA "TMA", modify
collect label levels var LDHn220 "LDH, IU/L", modify
collect label levels var haptoglobinn36 "Haptoglobin, mg/dL", modify
collect label levels var platelets "Platelets (x 1000/mm3)", modify
collect label levels var ggs "GGS score", modify
collect label levels var AI "AI score", modify
collect label levels var CI "CI score", modify
collect label levels var IFTA "IFTA score", modify
collect label levels var ah "AH score", modify
collect label levels var pdn "Prednisone", modify
collect label levels var hcq "Hydroxychloroquine", modify
collect label levels var csa  "Cyclosporine", modify
collect label levels var tac "Tacrolimus", modify
collect label levels var cyc "Cyclophosphamide", modify
collect label levels var aza "Azathioprine", modify
collect label levels var mmf "Mycophenolate", modify
collect label levels var rtx "Rituximab", modify
collect label levels var belimumab "Belimumab", modify
collect label levels var none "None", modify

// label values
foreach x  in obesity  diabetes ar hypertension CADorstrokeorPAD heartfailure ///
		cancer obesity APL TMA pdn  hcq csa  tac  cyc aza mmf rtx ///
		belimumab  none   {
		collect label values  `x' 0 "No" 1 "Yes", modify
		}
collect label values sex 0 "Female" 1 "Male", modify
collect label values ethnicity 0 "White" 1 "African-American" 2 "Hispanic" 3 "Asian" 4 "Other/Not specified", modify
collect label values LOWC3 0 "No" 1 "Yes"



// set the layout
collect label list LOWC3, all
// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header foreign
collect style cell cell_type[column-header]#LOWC3, border(bottom, pattern(single))



collect style row stack, nobinder 

collect notes, clear
collect notes "PI-LowC3, prolonged or persistent isolated low serum C3; sCr, serum creatinine; eGFR, estimated glomerular filtration rate; ISN/RPS, International Society of Nephrology and the Renal Pathology Society;  APL, antiphospholipid syndrome; TMA, thrombotic microangiopathy; GS, glomerulosclerosis; AI, activity index; CI, chronicity index; IFTA, interstitial fibrosis and tubular atrophy; AH, arteriolar hyalinosis."
collect notes 0: "Baseline patient characteristics. Baseline laboratory characteristics refer to 6 month post-biopsy."


// export tables
collect export Table1.html, replace
collect export Table1.txt, replace
collect export Table1.docx, replace
collect export Table1.tex, replace

///////////////////////////////////////////////////////////////////////////////
**# ///////    END PREPARING TABLE 
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
**#/////  START SURVIVAL ANALYSIS
///////////////////////////////////////////////////////////////////////////////

*-------------------------------------------------------------------------------
* Start Follow-up description
*-------------------------------------------------------------------------------

clear
cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
use sle_cross_sectional, clear
stset time_fup, fail(outcome == 1 2) origin(time 0) enter(time  0.49) id(id)
stdes
tab outcome if _d == 1
centile _t, c(25 50 75)
centile _t if outcome == 1, c(50)
centile _t if outcome == 2, c(50)
cap drop surv lb ub
sts gen surv =s, by(LOWC3)
sts gen lb = lb(s), by(LOWC3)
sts gen ub = ub(s), by(LOWC3)
table (LOWC3) ( ) if _t <=5 , stat(min surv lb ub) nototal nformat(%4.3f min lb ub)
cap drop surv lb ub
sts gen surv =s, by(TMA)
sts gen lb = lb(s), by(TMA)
sts gen ub = ub(s), by(TMA)
table (TMA) ( ) if _t <=5 , stat(min surv lb ub) nototal nformat(%4.3f min lb ub)


*-------------------------------------------------------------------------------
* End Follow-up description
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
**# START ANALYSIS LOWC3
*-------------------------------------------------------------------------------
clear
cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
use sle_cross_sectional, clear

stset time_fup, fail(outcome == 1 2) origin(time 0) enter(time  0.49) id(id)
preserve
gen status = _d
rename time_fup survtime
save sle_status_cross_sectional, replace
restore




	sts test LOWC3
	
	qui stcox i.LOWC3 
	lincom 1.LOWC3, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_cHR = string(`r(estimate)', "%3.2f")
	local s_cLB = string(`r(lb)', "%3.2f")
	local s_cUB = string(`r(ub)', "%3.2f")
	local s_cPVAL = string(`r(p)', "%4.3f")
	
	qui stcox i.LOWC3 ///
	c.ageatbiopsy i.sex i.black i.hypertension i.cyc i.mmf, ///
	nolog
	lincom 1.LOWC3, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_aHR = string(`r(estimate)', "%3.2f")
	local s_aLB = string(`r(lb)', "%3.2f")
	local s_aUB = string(`r(ub)', "%3.2f")
	local s_aPVAL = string(`r(p)', "%4.3f")



#delimit ;
global c3stuff  "risktable(, title("N at risk", size(*.7))) 
	  risktable(, rowtitle("Others") color("navy") group(#1) size(*.7)) 
	  risktable(, rowtitle("PI-LowC3") color("maroon")  group(#2) size(*.7))  
      plot1opts(lcolor("navy") lwidth(*1.0) lpattern(dash)) 
	  plot2opts(lcolor("maroon")  lwidth(*1.0) lpattern(solid))  
	  legend(cols(1) position(3) symxsize(10) rowgap(0.5) size(*.7) 
	  lstyle(none) lab(1 "Others") 
	  lab(2 "PI-LowC3") 
	  order(1 2) 
	  )   
	  ysc(range(.75 1)) 
	  ylab(0 "0" .20 "20" .40 "40" .60 "60" .80 "80"  1 "100", 
	  angle(horizontal) labsize(*.7) grid ) 
	  xsc(titlegap(2)) 
	  xlab(0 0.5 `" " " "category" "definition" "(6 months)" "' 
	  1(1)10, format(%3.1f) labsize(*.7))";
#delimit cr
	
	sts graph, by(LOWC3) tmax(10) $c3stuff scheme(s1mono)  xsize(*1.3) ///
	ytitle("Survival (%)") xtitle("Time from biopsy") title("") ///
	text(.17 3 "Crude    HR = `s_cHR' (95%CI: `s_cLB' to `s_cUB'; P= `s_cPVAL')", size(*0.7)) ///
	text(.1 3 "Adjusted HR = `s_aHR' (95%CI: `s_aLB' to `s_aUB'; P= `s_aPVAL')", size(*0.7))
    cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
	graph export "FIGURE 1.png", replace
	graph export "FIGURE 1.pdf", replace
	
	
	
	qui stcox i.TMA 
	lincom 1.TMA, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_cHR = string(`r(estimate)', "%3.2f")
	local s_cLB = string(`r(lb)', "%3.2f")
	local s_cUB = string(`r(ub)', "%3.2f")
	local s_cPVAL = string(`r(p)', "%4.3f")
	
	qui stcox i.TMA ///
	c.ageatbiopsy i.sex i.black i.hypertension i.cyc i.mmf, ///
	nolog
	lincom 1.TMA, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_aHR = string(`r(estimate)', "%3.2f")
	local s_aLB = string(`r(lb)', "%3.2f")
	local s_aUB = string(`r(ub)', "%3.2f")
	local s_aPVAL = string(`r(p)', "%4.3f")



#delimit ;
global c3stuff  "risktable(, title("N at risk", size(*.7))) 
	  risktable(, rowtitle("Others") color("navy") group(#1) size(*.7)) 
	  risktable(, rowtitle("TMA") color("maroon")  group(#2) size(*.7))  
      plot1opts(lcolor("navy") lwidth(*1.0) lpattern(dash)) 
	  plot2opts(lcolor("maroon")  lwidth(*1.0) lpattern(solid))  
	  legend(cols(1) position(3) symxsize(10) rowgap(0.5) size(*.7) 
	  lstyle(none) lab(1 "Others") 
	  lab(2 "TMA") 
	  order(1 2) 
	  )   
	  ysc(range(.75 1)) 
	  ylab(0 "0" .20 "20" .40 "40" .60 "60" .80 "80"  1 "100", 
	  angle(horizontal) labsize(*.7) grid ) 
	  xsc(titlegap(2)) 
	  xlab(0 0.5 `" " " "category" "definition" "(6 months)" "' 
	  1(1)10, format(%3.1f) labsize(*.7))";
#delimit cr
	
	sts graph, by(TMA) tmax(10) $c3stuff scheme(s1mono)  xsize(*1.3) ///
	ytitle("Survival (%)") xtitle("Time from biopsy") title("") ///
	text(.17 3 "Crude    HR = `s_cHR' (95%CI: `s_cLB' to `s_cUB'; P= `s_cPVAL')", size(*0.7)) ///
	text(.1 3 "Adjusted HR = `s_aHR' (95%CI: `s_aLB' to `s_aUB'; P= `s_aPVAL')", size(*0.7))
	graph export sle_only_tma_crude_km.png, replace
	
	
stset time_fup, fail(outcome == 1 2) origin(time 0) enter(time  0) id(id)
preserve
gen status = _d
rename time_fup survtime
save sle_status_cross_sectional, replace
restore


	
	qui stcox i.TMA 
	lincom 1.TMA, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_cHR = string(`r(estimate)', "%3.2f")
	local s_cLB = string(`r(lb)', "%3.2f")
	local s_cUB = string(`r(ub)', "%3.2f")
	local s_cPVAL = string(`r(p)', "%4.3f")
	
	qui stcox i.TMA ///
	c.ageatbiopsy i.sex i.black i.hypertension i.cyc i.mmf, ///
	nolog
	lincom 1.TMA, hr cformat(%3.2f) sformat(%3.2f) pformat(%4.3f)
	local s_aHR = string(`r(estimate)', "%3.2f")
	local s_aLB = string(`r(lb)', "%3.2f")
	local s_aUB = string(`r(ub)', "%3.2f")
	local s_aPVAL = string(`r(p)', "%4.3f")



#delimit ;
global c3stuff  "risktable(, title("N at risk", size(*.7))) 
	  risktable(, rowtitle("non-TMA") color("navy") group(#1) size(*.7)) 
	  risktable(, rowtitle("TMA") color("maroon")  group(#2) size(*.7))  
      plot1opts(lcolor("navy") lwidth(*1.0) lpattern(dash)) 
	  plot2opts(lcolor("maroon")  lwidth(*1.0) lpattern(solid))  
	  legend(cols(1) position(3) symxsize(10) rowgap(0.5) size(*.7) 
	  lstyle(none) lab(1 "non-TMA") 
	  lab(2 "TMA") 
	  order(1 2) 
	  )   
	  ysc(range(.75 1)) 
	  ylab(0 "0" .20 "20" .40 "40" .60 "60" .80 "80"  1 "100", 
	  angle(horizontal) labsize(*.7) grid ) 
	  xsc(titlegap(2)) 
	  xlab(0 0.5 `" " " "category" "definition" "(6 months)" "' 
	  1(1)10, format(%3.1f) labsize(*.7))";
#delimit cr
	
	sts graph, by(TMA) tmax(10) $c3stuff scheme(s1mono)  xsize(*1.3) ///
	ytitle("Survival (%)") xtitle("Time from biopsy") title("") ///
	text(.17 3 "Crude    HR = `s_cHR' (95%CI: `s_cLB' to `s_cUB'; P= `s_cPVAL')", size(*0.7)) ///
	text(.1 3 "Adjusted HR = `s_aHR' (95%CI: `s_aLB' to `s_aUB'; P= `s_aPVAL')", size(*0.7))
	graph export "FIGURE 2.png", replace
	graph export "FIGURE 2.pdf", replace
	
*-------------------------------------------------------------------------------
**# END ANALYSIS LOWC3
*-------------------------------------------------------------------------------
	
///////////////////////////////////////////////////////////////////////////////
**#/////  END SURVIVAL ANALYSIS
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
**#///// START COMPETING RISK ANALYSIS
///////////////////////////////////////////////////////////////////////////////

*-------------------------------------------------------------------------------
**# START CRUDE COMP RISK ANALYSIS LOWC3
*-------------------------------------------------------------------------------

cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
use sle_cross_sectional, clear
stset time_fup, fail(outcome == 2) enter(time 0.5) id(id)
cap drop  CumInc* SError High Low
stcompet CumInc = ci SError = se High = hi Low = lo, ///
	by(LOWC3) compet1(1)

gen CumInc_1 = CumInc if outcome == 1
gen CumInc_2 = CumInc if outcome == 2


stpepemori LOWC3, compet(1)

capture program drop _ADDID
program define _ADDID
version 17.0
count
 di r(N)
 local N = r(N)
 qui summ id
 local last_id = r(max)
 local new_record = `N'+1
 set obs `new_record'
 replace id= `last_id' +1 in l
end

preserve
foreach _ci in 1 2  {
	foreach _c3grp in 0 1  {
	qui summ CumInc_`_ci' if CRP_C3 == `_c3grp'
	local ci10_`_ci'_`_c3grp' = r(max)
	di  %4.3f "---->for outcome = `_ci', and group = `_c3grp' maximun incidence is `ci10_`_ci'_`_c3grp''"
	_ADDID
	replace CumInc_`_ci' = 0 in l
	replace _t = 0.5 in l
	replace LOWC3 = `_c3grp' in l
	 }
 }

label define LOWC3 0 "Others"  1 "PI-LowC3"
label values LOWC3 LOWC3
tw line CumInc_* _t if _t <= 10, ///
by(LOWC3, rows(1) note(" ") title(, size(*0.8))) ///
connect(J J) ///
lcolor(black maroon) ///
lwidth(*1.5 *1.5) ///
lpatter(solid dash) ///
sort ///
ytitle("Cumulative Incidence (%)") ///
ylabel(0 "0" .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60" .7 "70" , angle(horizontal) grid format(%3.0f)) ///
ymtick(0(0.05).7, grid) ///
 yscale(titlegap(2)) ///
xtitle("Time form Biopsy (Years)") xscale(titlegap(2)) ///
xlabel(0.5 `" "0.5" "(6mo)"  "' 2.5(2.5)10) xsc(range(0 10))  ///
legend(order( 1 "Death" 2 "ESKD") ///
size(*0.6) bmargin(t=5) pos(12) rows(1)) ///
scheme(s1mono)
graph export "FIGURE 3.png", replace
graph export "FIGURE 3.pdf", replace

restore

*-------------------------------------------------------------------------------
**# END CRUDE COMP RISK ANALYSIS LOWC3
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
**# START ADJUSTED COMP RISK ANALYSIS LOWC3
*-------------------------------------------------------------------------------

cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
use sle_cross_sectional, clear
stset time_fup, fail(outcome == 2) enter(time 0.5) id(id)
stcrreg i.LOWC3 c.ageatbiopsy i.sex i.black i.hypertension, compete(outcome == 1)
test _b[1.LOWC3] = 0
lincom 1.LOWC3, cformat(%3.2f) pformat(%4.3f) sformat(%3.2f) hr
stcrreg i.LOWC3 c.ageatbiopsy i.sex i.black i.hypertension i.cyc i.mmf, compete(outcome == 1)
test _b[1.LOWC3] =  0
lincom 1.LOWC3, cformat(%3.2f) pformat(%4.3f) sformat(%3.2f) hr

*-------------------------------------------------------------------------------
**# END ADJUSTED COMP RISK ANALYSIS LOWC3
*-------------------------------------------------------------------------------


///////////////////////////////////////////////////////////////////////////////
///// END COMPETING RISK ANALYSIS
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
**#/////  START LONGITUDINAL ANALYSIS
///////////////////////////////////////////////////////////////////////////////
* Note: one patient with event not matched with eGFR file and dropped
cd "C:\Documenti\Manenti\LES\JOHNS HOPKINS"
*-- start creating consistent Longitudinal Survival datasets
use sle_global, clear
    count if  time_elapsed < 0.5
    list id time_elapsed LOWC3 outcome eGFR if time_elapsed < 0.5 in 500/600
	drop if time_elapsed < 0.5
	preserve 
	keep  id time_elapsed eGFR CRP_C3 LOWC3 ///
			ageatbiopsy sex black ///
			diabetes hypertension cyc mmf
	rename time_elapsed time
	save sle_05_long, replace
	restore
	
use sle_status_cross_sectional, clear
	drop if missing(_t)
	unique id
	merge 1:m id using sle_05_long
	list id LOWC3 outcome if _merge != 3
	* beware  id = 201 LOWC3 = 1  outcome = Death cannot be merged with sle_05_long
	keep if _merge == 3
	unique id
	preserve
	drop if time > survtime
	keep id CRP_C3 LOWC3 eGFR time ageatbiopsy sex  black diabetes  hypertension cyc mmf
	save sle_05_long, replace 
	restore 
	bysort id (time): drop if _n > 1
	drop time eGFR
	save, replace

use sle_05_long, clear
	unique id
	use sle_status_cross_sectional, clear
	unique id

use sle_status_cross_sectional
	drop _merge
	merge 1:m id using sle_05_long
	drop if time > survtime
	keep id CRP_C3 LOWC3 eGFR time ageatbiopsy sex  black diabetes  hypertension cyc mmf
	save sle_05_long, replace 
	unique id
	
*-- end creating consistent Longitudinal Survival datasets

	
	use sle_05_long, clear
	* random intercept 
	mixed eGFR ///
	i.LOWC3##c.time c.ageatbiopsy i.sex i.black ///
	i.hypertension i.cyc i.mmf ///
	|| id: ,  reml dfmethod(kroger) nolog
	
	* random coefficient
	mixed eGFR ///
	i.LOWC3##c.time c.ageatbiopsy i.sex i.black ///
	i.hypertension i.cyc i.mmf ///
	|| id: time, cov(unstr) reml dfmethod(kroger) nolog
	
	
	qui margins, at(time = (0(1)10)) over(LOWC3)
	marginsplot,  xdimension(time)  plotdimension(LOWC3) noci ///
	recast(line) ///
	plot1opts(lwidth(*3) lcolor(black) lpatter(dash))  ///
	plot2opts(lwidth(*3) lcolor(navy))  ///
	ylabel(0(30)120, angle(horizontal) grid) ////
	xtitle(Time since Biopsy) ///
	ytitle("eGFR (mL/min/1.73m{sup:2})") ///
	title("Predicted eGFR Traectories") ///
	scheme(s1mono)
	cap graph export lowc3_sle_3_mean_egfr_traj_adj.png, replace
	
	qui margins, at(time = (0(1)10)) over(LOWC3)
	marginsplot,  xdimension(time)  plotdimension(LOWC3) noci ///
	recast(line) ///
	plot1opts(lwidth(*3) lcolor(black) lpatter(dash))  ///
	plot2opts(lwidth(*3) lcolor(navy))  ///
	addplot(scatter eGFR time if LOWC3 == 0, ///
				msymbol(o) msize(*0.2) mcolor(black%10) || ///
			scatter eGFR time if LOWC3 == 1, ///
				msymbol(o) msize(*0.2) mcolor(navy%10) 		|| ///
				) ///
	ylabel(0(30)180, angle(horizontal) grid) ////
	xtitle(Time since Biopsy) ///
	ytitle("eGFR (mL/min/1.73m{sup:2})") ///
	title("Predicted eGFR Traectories") ///
	legend(off) scheme(s1mono)
	cap graph export lowc3_sle_3_ind_egfr_traj_adj.png, replace

	
///////////////////////////////////////////////////////////////////////////////
/////  END LONGITUDINAL ANALYSIS
///////////////////////////////////////////////////////////////////////////////


	
	
	


