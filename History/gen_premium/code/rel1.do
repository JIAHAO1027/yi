clear all
set more off
*cd "F:\Dropbox\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"
*xin path
cd "C:\Users\xin_chen\Dropbox\urap_programming\yi\gen_premium_new\output"
import delimited "700d_relative_to_bundle_1.csv"
*fixme how to append second data set
*append using 750d_relative_to_bundle_1.csv

xtset item_id
xtreg premrel diff, fe 
outreg2 using rel1_deltap_deltan.tex, replace ctitle(Premium rel B1) alpha(0.15) //label
xtreg premrel diff, fe vce(cl item_id)
outreg2 using rel1_deltap_deltan.tex, append ctitle(Premium rel B1) addtext(Clutered at the item_id level, Yes) alpha(0.15) //label
xtreg premrel diff cc, fe //vce(cl item_id)
outreg2 using rel1_deltap_deltan.tex, append ctitle(Premium rel B1)  alpha(0.15) //label
xtreg premrel diff uv, fe
xtreg premrel diff cc uv, fe
xtreg premrel diff cc uv xb, fe
*xtreg premrel diff bd bj bu cc dc dk ej fd fh fw gj hj jb jc jd jq mo nd pz qc qt rg sj sp uv wd wj xb xn yd yk zg, fe vce(cl item_id)
outreg2 using rel1_deltap_deltan.tex //, replace ctitle(Model 1)
