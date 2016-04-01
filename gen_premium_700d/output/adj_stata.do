*adjacent

clear all
set more off
*cd "F:\Dropbox\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"
*xin path
cd "C:\Users\xin_chen\Dropbox\urap_programming\yi\gen_premium_new\output"
import delimited "adjacent.csv"

xtset item_id
xtreg premrel diff, fe vce(cl item_id)
outreg2 using doubleref_deltap_deltan.tex, replace ctitle(rel B1 and B5 Model 1) 
xtreg premrel diff if diff==1 | diff == 2, fe vce(cl item_id)
xtreg premrel diff if diff>0 & diff <= 3, fe vce(cl item_id)
