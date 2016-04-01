clear all
set more off
*cd "D:\Dropbox\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"
*xin path
cd "C:\Users\xin_chen\Dropbox\STIMULATING_Research_ Ideas and Inspirations_\Bundle_Pricing_joint_Zach\iPad Air 2 data\Output"

*import delimited "item_bundle_merged.csv"
import delimited "share_and_premium.csv"


*how to create a variable
gen withitemshare = item_bundlesales/ itemsales
gen rbundleid = bundleid / numberofbundles
gen rid = floor(rbundleid*5)

stripplot totalprice, vertical bar over( bundleid)
graph export totalprice_bundle.png

stripplot bundleprice, vertical bar over( bundleid)
graph export bundleprice_bundle.png

stripplot item_bundlesales, vertical bar over( bundleid)
graph export sales_bundle.png

stripplot withitemshare, vertical bar over( bundleid)
graph export share_bundle.png

*fixed effects regression
xtset itemid
xtreg totalprice numberofaccessory bundleid, fe vce(cl itemid)
outreg2 using xtregresults.tex, replace ctitle(Model 1)
est store price1
xi: xtreg totalprice numberofaccessory i.bundleid, fe vce(cl itemid)
est store price2
esttab price1 price2 using price.csv, r2 nogap replace

xtreg item_bundlesales numberofaccessory totalprice, fe vce(cl itemid)
est store sales1
xi: xtreg item_bundlesales numberofaccessory totalprice i.bundleid, fe vce(cl itemid)
est store sales2
esttab sales1 sales2 using sales.csv, r2 nogap replace

xtreg withitemshare numberofaccessory totalprice, fe vce(cl itemid)
est store share1
xi: xtreg withitemshare numberofaccessory totalprice i.bundleid, fe vce(cl itemid)
est store share2
esttab share1 share2 using share.csv, r2 nogap replace
xi: xtreg totalprice i.numberofaccessory i.bundleid, fe vce(cl itemid)
