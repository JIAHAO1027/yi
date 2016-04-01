### fixme please add comments on what num_of_acc_no_price is

library(DataComputing)
setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
# setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list)
Price<-read.csv(filename_dict) %>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)

#fixme: remove all item_ids that hav only 1 bundle

# Set all "unknown" or lost Price = 0
i=1
while(i<length(Price$Avg..Price.Esitimate))
{
  if(Price$Avg..Price.Esitimate[i]==""||Price$Avg..Price.Esitimate[i]=="unknown"||Price$Avg..Price.Esitimate[i]=="Unknown")
    Price$Avg..Price.Esitimate[i]=0
  i=i+1
}

Camera$dict_yi_index<-as.character(Camera$dict_yi_index)
Price$index_final<-as.character(Price$index_final)
Price$Avg..Price.Esitimate<-as.numeric(Price$Avg..Price.Esitimate)
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_yi_index"="index_final"))
result<-CameraWPrice%>%
  group_by(ItemID.LIsting_id.,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_yi_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('ԭװ',grepl,Accessory_title))),
            num_of_acc_no_price=sum(Avg..Price.Esitimate==0),
            bundle_price=mean(bundle_price),
            price_ref=sum(Avg..Price.Esitimate))
result[is.na(result)]<-0
result<-result%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         num_of_acc_rel=total-first(total),
         premium_percent=(bundle_add_on-price_ref_rel)/bundle_price,
         ind_firstbundle_noacc=first(total)==0)

names(result)[names(result) == "ItemID.LIsting_id."] = "item_id"
# names(df_lst)[names(df_lst) == "ItemID.LIsting_id."] = "item_id"

setwd("../output")
write.csv(result, file = "premium_num_of_acc.csv")

#par(mar = rep(2, 4))
# plot(result$num_of_acc_no_price, result$premium)
