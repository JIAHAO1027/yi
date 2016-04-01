library(dplyr)
setwd("C:/Users/xin_chen/Dropbox/urap_programming")
df.prem = read.csv("./yi/gen_premium/output/numofacc_and_premium_700D_prem_minus_a_0225.csv")
df.share = read.csv("./Jiazhen/share_filtered/output/share_700d_20160229.csv",stringsAsFactors = F)


df.prem$bundle_index =as.character(df.prem$bundle_index)
str(df.share$bundle_index)
#check if the keys to join have the same data type
str(df.share$bundle_index[1]) == str(df.prem$bundle_index[1])
df.prem_share = left_join(df.prem, df.share, by = c("item_id","bundle_index"))
# df.prem_share = inner_join(df.prem, df.share, by = c("item_id","bundle_index"))

#check the left_join is correct
dim(df.prem)[1] == dim(df.prem_share)[1]

#remove NA; NA = 0
df.prem_share$bundle_sales[is.na(df.prem_share$bundle_sales)] <- "."
# df.prem_share$share = as.numeric(df.prem_share$share)

df.prem_share$share[is.na(df.prem_share$share)] <- "."
write.csv(df.prem_share,"./prem_join_share/output/prem_share.csv")
df.prem_share_subset_by_num_of_acc_le5 = subset(df.prem_share,df.prem_share$num_of_acc_rel <= 5)

write.csv(df.prem_share_subset_by_num_of_acc_le5,"./yi/gen_premium/output/prem_share_le5.csv")
write.csv(df.prem_share,"./yi/gen_premium/output/prem_share.csv")

