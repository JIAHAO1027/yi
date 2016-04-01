clear all
set more off
*cd "F:\Dropbox\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"
*xin path
cd "C:\Users\xin_chen\Dropbox\urap_programming\yi\gen_premium\output"
import delimited "numofacc_and_premium_700D_adjacent_0229.csv"

xtset item_id
xtreg prem_adjacent diff, fe vce(cl item_id)
outreg2 using adj_deltap_deltan.tex, replace ctitle(Adj Model 1)
