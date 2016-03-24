
clear
set more off
/*
global dofile "C:\Users\Jessica-laptop\Documents\Rothstein"
global data "C:\Users\Jessica-laptop\Documents\Rothstein\data"
global output "C:\Users\Jessica-laptop\Documents\Rothstein\output"

cd $dofile
*/

cd "C:\Users\Jessica-laptop\Documents\EC300"

global data "C:\Users\Jessica-laptop\Documents\EC300\dta"
use "$data\import93.dta"

keep if interv==1

* analyze federal minimum wage for now; ie, one minimum wage

* compute hourly earnings
gen laborweeks = wkswork
replace laborweeks = laborweeks+lkweeks if lkweeks<. & wkswork<.
replace laborweeks = lkweeks if wkswork==.

gen emprate = wkswork/laborweeks
replace emprate = 0 if wkswork==. & lkweeks==.

*gen mwage = wsal_val/wkswork*hrslastyr
* use hrlywg - above is more of an average wage
replace hrlywg = . if hrlywg<0



* TAXSIM needs these variables: year state mstat depx agex pwages swages dividends ///
* otherprop pensions gssi transers rentpaid protax otheritem childcare ui depchild mortgage stcg ltcg
gen year=1993
gen taxyear=year-1


*Make needed variables: hourly wage, worker, female
	
gen wsearn=wsal_val
gen frseearn=frse_val 
gen sempearn=semp_val 

gen seearn=frseearn+sempearn
replace seearn=0 if seearn<0

gen wage=(wsearn+seearn)/(wkswork*hrslastyr) if wsearn+seearn>0 & wkswork>0 & hrslastyr>0
label var wage "Hourly wage last year (nominal)"

replace hrlywg = wage if hrlywg==. & wage<.


*Worker -- reasonable wage, annual earnings > 100
*Workers earn between 2 and 100 dollars an hour
gen worker=(wage<=100 & wage>=2 & (wsearn+seearn)>100)

   
gen female=(sex==2)
gen married=(maritalst>=1 & maritalst<=3)
*!!HH col 57: 3 - not identified
gen byte inmsa=(metro==1 | metro==3)
*gen inmsa=(metro==1)

*Will need education levels for tables later
*Jesse has more obs here bc he includes children education levels
recode educ (31=0) (32=4) (33=6) (34=8) (35=9) (36=10) (37 38=11) (39=12) ///
(40 41=13)(42=14) (43=16) (44=17) (45=18) (46=20) (else=.), test

gen adult = 1 if age>25 & age<65
*gen elder = (age>64)

 gen div = div_val
 gen op = int_val + rnt_val + alimony
 gen pen=ret1_val + ret2_val + sur1_val + sur2_val
 gen ss=ss_val
 gen trans=wc_val + ssi_val + welfare + vet_val + di1_val + di2_val + ed_val + childsupp
 gen un=ui_val
 *gen depchild = dep
 
   replace ss       =ss     +oi_val if oi_src==1 
   replace pen      =pen     +oi_val if oi_src==2
   replace trans     =trans    +oi_val if oi_src==3
   replace tran     =trans    +oi_val if oi_src==4
   replace op      =op     +oi_val if oi_src==5
   replace div      =div     +oi_val if oi_src==6
   replace op      =op     +oi_val if oi_src==7
   replace op      =op     +oi_val if oi_src==8
   replace trans     =trans    +oi_val if oi_src==9
   replace trans    =trans    +oi_val if oi_src==10
   replace un       =un      +oi_val if oi_src==11
   replace trans     =trans    +oi_val if oi_src==12
   replace pen      =pen     +oi_val if oi_src==13
   replace op      =op     +oi_val if oi_src==19

gen dividends = div
gen otherprop = op
gen pensions = pen
gen gssi = ss
gen transfers = trans
gen ui = un   
gen rentpaid=0
gen proptax=0
gen otheritem=0
gen childcare=0

gen kid = 1 if age<24
tab kid famrelp
* any kids that are heads of households?! YES ottoke???????????
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

* Calculating number of children!! must save this in a separate file bc ow, lots of children get dropped when I drop in line 109
 tempfile eic
 save `eic'
 

 use `eic'
 tempfile kids
 
replace kid=0 if age>18 & (enrlw!=1 & enrlw!=2)

*!!!
* if head of household is a kid, make them part of first family
    replace famid=1 if (famrelp==0 | famrelp==1 | famrelp==2) & (expandrel==5 | expandrel==6 | expandrel==7 | expandrel==9 | expandrel==10 | expandrel==11)

gen kid6 = (age<=6)
gen kid16 = (age<16)				  

collapse (sum) nkid=kid dep=kid16, by(year mis hhseqnum famid)
save `kids'


use `eic'
*need separate file to collapse workers, man income, before removing men
tempfile income

keep if famrelp==1 | famrelp==2 | famrelp==0
tab famrelp
keep if famrelp==1 | famrelp==2
*21k differences with rothstein bc famrelp==0 dropped


egen fem_taxsim_ws=rowtotal(wsal_val semp_val frse_val) if female==1
egen man_taxsim_ws=rowtotal(wsal_val semp_val frse_val) if female==0
sort year mis hhseqnum famid female

*identify couples, single women, single men
by year mis hhseqnum famid: gen husage = age[_N-1]
by year mis hhseqnum famid: gen headnum = _N
tab husage
tab headnum

collapse (sum) nworkers=worker fem_taxsim_ws man_taxsim_ws (max) couple=married husage, by(year mis hhseqnum famid)
tab husage

merge 1:1 year mis hhseqnum famid using `kids', gen(kidmerge)

save `income'

use `eic'

sort year mis hhseqnum famid female
*drop if female==0 & famrelp==1
keep if female==1 | kid==1
* dropped all male head of households; the rest should be secondary earners
gen reffemale = 1 if female ==1 & famrelp==1 | famrelp==2 | famrelp==0
label var reffemale "Our reference person is female"

merge m:1 year mis hhseqnum famid using `income', gen(fammerge)
tab fammerge
drop fammerge
tab kidmerge
drop kidmerge



*Create labor market segregation

gen lths = (educ<12)
gen hsgd = (educ ==12)
gen smcol = (educ>12 & educ <16) 
gen college =(educ>=16 & educ<.)

gen edumark = lths+2*hsgd+3*smcol+4*college
replace edumark=1 if edumark==0

gen age_int = floor(age/5)

codebook age_int
replace age_int=1 if age_int<=0
*replace age_int=9 if age_int>=10 & age_int<.
* only three states with higher state min wages in 1992: http://www.dol.gov/whd/state/stateminwagehis.htm

xtile lm3=wage if hrlywg>4.25, nquantiles(19)
replace lm3=0 if wage==. | worker~=1
*!! lm4 is directly comparable with Jesse!!

*should I use hrlywg??
xtile lm4=wage, nquantiles(20)
replace lm4=0 if wage==. | worker~=1

gen lm3_w = lm3*1000 + 100*(married==1) + 10*(nkid>0) + (kid==1)
replace lm3_w=0 if wage==. | worker~=1

gen lm4_w = lm4*1000 + 100*(married==1) + 10*(nkid>0) + (kid==1)
replace lm4_w=0 if wage==. | worker~=1

drop if age<16
****** how to do a combined analysis?!?!! min wage needs kids bc they could possibly work. 
* I think i'm fine actually.... why do I have separate file for min wage...?
table lm4 married* nkid

save "$data\cpsprep93.dta", replace

*Select families/filing units. For each family, need to know total income, # kids, couple
* See http://www.irs.gov/Credits-&-Deductions/Individuals/Earned-Income-Tax-Credit/Do-I-Qualify-for-Earned-Income-Tax-Credit-EITC
* start creating file for TAXSIM! need one representative/observation for tax filing unit. looking for women as family representatives

*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

use `eic'
tempfile familystats
*don't need to control for age; TAXSIM knows if they are eligible for EITC!
keep if famrelp==1 | famrelp==2 | famrelp==0 | kid==1
*keep if famrelp==1 | famrelp==2

sort year mis hhseqnum famid female

*identify couples, single women, single men
by year mis hhseqnum famid: keep if _n==_N
rename female reffemale
label var reffemale "Our reference person is female"
*rename male refmale

* took out inc_val
*sort year mis hhseqnum famid
merge 1:1 year mis hhseqnum famid using `kids', gen(kidmerge)
merge 1:1 year mis hhseqnum famid using `income', gen(fammerge)
replace nkid =0 if kidmerge==1
replace dep = 0 if kidmerge==1


*If either is him/herself a qualifying child (right age and living with relatives), can't claim;
 gen qualkid=1 if age<=18 
 replace qualkid = 1 if age<24 & enrlw==1 
 replace qualkid = 1 if age<24 & enrlw==2
 replace qualkid=0 if expandrel<5 | expandrel==8 | expandrel>=12
*keep if qualkid~=1 
*drop if qualkid==1

gen id = _n



*Create labor market segregation
* Not sure needs to be in this dofile; but I put it here to match his estimates

gen lths = (educ<12)
gen hsgd = (educ ==12)
gen smcol = (educ>12 & educ <16) 
gen college =(educ>=16 & educ<.)

gen edumark = lths+2*hsgd+3*smcol+4*college
replace edumark=1 if edumark==0

gen age_int = floor(age/5) - 3
* minus 3 b/c our householders will be at least 18

codebook age_int
replace age_int=1 if age_int<=0
replace age_int=9 if age_int>=10 & age_int<.

gen lm1=edumark*10 + age_int

*Recoded to match Jesse
*gen lm2 = (state*10+inmsa)*10+(educ>12)

recode st_cen (11 13=12) (55=54) (15 16=14) (72=64) (83=81) (51=52) (87=88), into(newst) copyrest
*Note: Counting "not identified" as non-metropolitan
gen stmet=newst*10 + (metro==1)
gen educgrp2=lths + hsgd + 2*(smcol+college)
gen lm2=stmet*10+educgrp2

xtile lm4=wage, nquantiles(20)
replace lm4=0 if wage==. | worker~=1

xtile lm3=wage if hrlywg>4.25, nquantiles(19)
replace lm3=0 if wage==. | worker~=1

* separating single and married women
*drop if reffemale !=1

gen lm1_w = (edumark*10 + age_int)*10 + (married==1)
gen lm2_w = (lm2)*10+ (married==1)
gen lm3_w = lm3*1000 + 100*(married==1) + 10*(nkid>0) + (kid==1)
replace lm3_w=0 if wage==. | worker~=1

gen lm4_w = lm4*1000 + 100*(married==1) + 10*(nkid>0) + (kid==1)
replace lm4_w=0 if wage==. | worker~=1

save `familystats'

* TAXSIM needs these variables: year state mstat depx agex pwages swages dividends ///
* otherprop pensions gssi transers rentpaid protax otheritem childcare ui depchild mortgage stcg ltcg
* see help taxsim9 

 merge m:1 st_cens using "$data\statecodes.dta", generate(statemerge)
 rename state stname
 rename st_soi state
 gen mstat = 1 +(married==1) + (married!=1 & nkid>0)
 gen depx = nkid
 gen agex = (age>64) + (husage>64 & husage<.)
 gen pwages = fem_taxsim_ws if reffemale==1
 replace pwages = man_taxsim_ws if reffemale==0
 gen swages = fem_taxsim_ws+man_taxsim_ws-pwages
 *gotta make sure no missing data, negative data gets fed into TAXSIM!
 replace pwages=0 if pwages==.
 replace swages=0 if swages==.
 replace swages=max(swages+pwages, 0) if pwages<0
 replace pwages=0 if pwages<0
 replace pwages=max(pwages+swages, 0) if swages<0
 replace swages=0 if swages<0
 
 

 
 foreach v in mstat depx agex dividends pensions gssi transfers ui otherprop {
    quietly replace `v'=0 if `v'==. | `v'<0
    }


*Now duplicate data to estimate tax with no earnings( ie if they choose to not work)

expand 2
sort id
by id: gen dupnum=_n
* Just looking at counterfactual for women who work:
drop if dupnum==2 & reffemale~=1
drop if dupnum==2 & (pwages<=0 | pwages==. | fem_taxsim_ws<=0)
replace pwages=0 if dupnum==2

taxsim9, full replace
rename fiitax fedtax
   rename siitax sttax
   rename frate fedmtr
   rename srate stmtr
   rename ficar ficamtr
rename v10 fedagi
rename v11 uiinagi
rename v12 ssinagi
rename v13 zba
rename v14 persexempt
rename v15 exemptout
rename v16 deductout
rename v17 deductallow
rename v18 fedtaxy
rename v19 fedregtax
rename v20 exempttax
rename v21 gencred
rename v22 kidcred
rename v23 kidcred_ref
rename v24 carecred
rename v25 eic
rename v26 amtinc
rename v27 amt
rename v28 fedtaxnet
rename v29 fica2
rename v30 stinc
rename v31 strent
rename v32 stagi
rename v33 stexempt
rename v34 stdeductstd
rename v35 stdeductitem
rename v36 sttaxinc
rename v37 stpropcred
rename v38 stcarecred
rename v39 stgencred
rename v40 sttotcred
rename v41 stbracket


keep id dupnum fedagi fedtax fedmtr eic pwages

reshape wide fedagi fedtax fedmtr eic pwages, i(id) j(dupnum)
foreach v in fedagi fedtax fedmtr eic pwages {
	rename `v'1 `v'_act
	rename `v'2 `v'_nowork
     }

label var fedtax_act "TAXSIM: Federal tax (1992 schedule)"
label var fedmtr_act "TAXSIM: Federal MTR (1992 schedule)"
label var fedagi_act "TAXSIM: Federal AGI (1992 schedule)"
label var eic_act "TAXSIM: EITC (1992 schedule, fed only?)"
label var fedtax_nowork "TAXSIM: Fed. tax (1992 sched), no f work"
label var fedmtr_nowork "TAXSIM: Fed. MTR (1992 sched), no f work"
label var fedagi_nowork "TAXSIM: Fed. AGI (1992 sched), no f work"
label var eic_nowork "TAXSIM: EITC (1992 sched, fed only?), no f work"

gen fedatr=(eic_nowork-eic_act)/pwages_act if pwages_act>0 & pwages_nowork<.

*!!! Using Jesse's code to run his EITC simulation
 merge 1:1 id using `familystats'
 gen taxsim_ws=fem_taxsim_ws+man_taxsim_ws
 gen taxsim_ws_nowork=man_taxsim_ws if reffemale==1 & fem_taxsim_ws>0
 
 preparemarchdata_simeitc eic mtr, earnings(taxsim_ws) agi(fedagi_act) nkids(nkid) schedule(taxyear) debug prefix(sim1)

   rename sim1range range
   rename sim1useagi useagi

 preparemarchdata_simeitc s_eic_nowork mtr_nowork, earnings(taxsim_ws_nowork) agi(fedagi_nowork) nkids(nkid) schedule(taxyear) debug prefix(sim2)

   rename sim2range range_nowork
   rename sim2useagi useagi_nowork

   drop sim1* sim2*

 foreach v in s_eic mtr range useagi {
	replace `v'_nowork=`v' if taxsim_ws_nowork==.
   	}

label var eic "Simul EITC (1992 sched.)"
label var mtr "Simul EITC MTR (1992 sched.)"
label var range "Simul EITC range (1992 sched.)"
label var s_eic_nowork "Simul EITC (1992 sched.), no f work"
label var mtr_nowork "Simul EITC MTR (1992 sched.), no f work"
label var range_nowork "Simul EITC range (1992 sched.), no f work"

label define range 0 "Inelig." 1 "Phase-in" 2 "Plateau" 3 "Phase-out" 4 "too high"
	label values range range
	label values range_nowork range
	
gen atr=(s_eic_nowork-eic)/fem_taxsim_ws if fem_taxsim_ws>100 & fem_taxsim_ws<.

*!! reconcile differences
gen diff=eic-eic_act
* Taxsim sysmetically overestimates ETIC credit; doesn't happen in Jesse's code
* Our final samples are slightly different - I have more observations. Unsure why. 

gen byte isdiffeic=(abs(diff)>10 & diff<.)
tab isdiffeic, m

*Basically, need new EITC program to be same cost as old
sum eic [aw=marchwt] if reffemale==1
gen eic_oldtotal = r(sum)
*only want marginal change, so not:
*gen eic_perctinc = (eic_oldtotal+1)/eic_oldtotal
gen eic_perctinc = 1e8/eic_oldtotal

* What is new MTR? EITC MTR is just change in income over current income
* MTR = (marginal $ increase in working 1 more unit)/(current income)
* new MTR = (marginal $*dcredit increase in working 1 more unit)/current income = dcredit*MTR

*ATR = (eicnowork-eic)/(current income)
* new ATR = (dcredit)*(eicnowork-eic)/(current income)-- since entire schedule gets same percentage increase

gen deic_mtr = eic_perctinc*mtr
gen deic_atr = eic_perctinc*atr
gen deic = eic_perctinc*eic

replace deic_mtr=0 if qualkid==1
replace deic_atr = 0 if qualkid==1
replace deic = 0 if qualkid==1
replace eic==0 if qualkid==1

save "$data\cpsprep.dta", replace
*log close

