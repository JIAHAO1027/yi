library(DataComputing)
# setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list)
Price<-read.csv(filename_dict)%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)

CameraWPrice<-Camera%>%left_join(Price,by=c("dict_index"="index_final"))
result<-CameraWPrice%>%
  group_by(ItemID.LIsting_id.,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_index)))),
            bundle_price=mean(bundle_price),
            optimal_price=sum(Avg..Price.Esitimate))%>%
  arrange(ItemID.LIsting_id.,bundle_index)
i=2
n=1
while(i<length(result$ItemID.LIsting_id.))
{
  if(result$ItemID.LIsting_id.[i]==result$ItemID.LIsting_id.[i-1])
  {
    result$optimal_price[i]<-result$optimal_price[i]+result$bundle_price[i-n];
    n=n+1
  }
  else n=1
  i=i+1
}
result<-result%>%mutate(premium_percent=(bundle_price-optimal_price)/bundle_price)

setwd("../output")
write.csv(result,file = "result2.csv")
