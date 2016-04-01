library(DataComputing)
# setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi")
setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")

setwd("./input")
Camera<-read.csv("lst_700D_W_B0.csv")
Price<-read.csv("canon_price.csv")
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
 setwd("./output")
write.csv(result,file = "result3.csv")

