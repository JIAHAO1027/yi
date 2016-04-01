clear all
set more off
*cd "F:\Dropbox\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"
*xin path
cd "C:\Users\xin_chen\Dropbox\urap_programming\yi\gen_premium\output"
import delimited "prem_share.csv"

xtset item_id
xtreg premrel1 diff, fe vce(cl item_id)

*not significant
**denotes we replace diff w diff_minus_a
**xtreg premrel1 diff_minus_a, fe vce(cl item_id)
*add in control bundle_index
*xtreg premrel1 diff bundle_index, fe vce(cl item_id)
outreg2 using deltap_deltan.tex, replace ctitle(Model 1)

*LHS = bundle_sales
* premrel1 not significant
*xtreg bundle_sales premrel1 diff , fe vce(cl item_id)
**xtreg bundle_sales premrel1 diff_minus_a , fe vce(cl item_id)
*xtreg bundle_sales premrel1 diff bundle_price, fe vce(cl item_id)
**xtreg bundle_sales premrel1 diff_minus_a bundle_price, fe vce(cl item_id)

*outreg2 using sales_premium.tex, replace ctitle(Model)

