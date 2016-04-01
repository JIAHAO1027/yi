library(DataComputing)
setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
# setwd("F:/Dropbox/urap_programming/yi/gen_premium")
#filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
date <- paste(format(Sys.Date(), "%Y-%m-%d"), "csv", sep = ".")

#camera_name="750d"
#filename_list="Feb062016_750D_CombineList.csv"
camera_name="700d"
filename_lst="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
filename_dict="dict_700d_750d_Feb082016.csv"


setwd("./input")
Camera<-read.csv(filename_lst)
Price<-read.csv(filename_dict)%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
# summary(Price$Avg..Price.Esitimate)
# str(Price$Avg..Price.Esitimate)

# Set all "unknown" or lost Price = 0
i=1
while(i<length(Price$Avg..Price.Esitimate))
{
  if(Price$Avg..Price.Esitimate[i]==""||Price$Avg..Price.Esitimate[i]=="unknown"||Price$Avg..Price.Esitimate[i]=="Unknown")
    Price$Avg..Price.Esitimate[i]=0
  i=i+1
}

Camera$dict_index<-as.character(Camera$dict_index)
Price$index_final<-as.character(Price$index_final)
Price$Avg..Price.Esitimate<-as.numeric(Price$Avg..Price.Esitimate)
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_index"="index_final"))
result<-CameraWPrice%>%
  group_by(ItemID.LIsting_id.,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('ԭװ',grepl,Accessory_title))),
            num_of_acc_no_price=sum(Avg..Price.Esitimate==0),
            bundle_price=mean(bundle_price),
            price_ref=sum(Avg..Price.Esitimate*(1-gift_or_not))
            ,price_ref_notadjustingforgift =sum(Avg..Price.Esitimate)
            )
result[is.na(result)]<-0
result<-result%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         price_ref_rel_notadjustingforgift=price_ref_notadjustingforgift-first(price_ref_notadjustingforgift),
         num_of_acc_rel=total-first(total),
         #num_of_acc adjusting for gift 
         num_of_acc=nongift-first(nongift),
         premium = bundle_add_on-price_ref_rel,
         
         premium_notadjustingforgift = bundle_add_on-price_ref_rel_notadjustingforgift,
         premium_percent_notadjustingforgift=(bundle_add_on-price_ref_rel_notadjustingforgift)/bundle_price,
         
         ind_firstbundle_noacc=first(total)==0)


###graphs
boxplot(premium ~ num_of_acc_rel, data=result, main="Premium v Num_of_acc", 
        xlab="Number of Accessories", ylab="Premium")

#boxplot(premium ~ num_of_acc, data=result, main="Premium v Num_of_acc", 
        # xlab="Number of Accessories", ylab="Premium")

boxplot(premium ~ bundle_index, data=result, main="Premium by Bundle index", 
        xlab="bundle_index", ylab="Premium")

# boxplot(premium_notadjustingforgift ~ bundle_index, data=result, main="Premium by Bundle index", 
#         xlab="bundle_index", ylab="Premium")

setwd("../output")
# ####fixme
# outputfilename = paste(paste(camera_name,under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
# ##adding hours and minutes
# #outputfilename = paste(paste(camera_name,under_s,sep='') ,format(Sys.time(),"%Y-%m-%d-%h-%m"),".csv",sep='')
# # write.csv(df_lst_join_dict, "Output\outputfilename")
# write.csv(df_lst_join_dict, outputfilename)

write.csv(result,file = "numofacc_and_premium_750D_yi.csv")

