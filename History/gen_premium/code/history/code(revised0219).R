library(DataComputing)
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list)
Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)


colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

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
  group_by(item_id,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('ԭװ',grepl,Accessory_title))),
            num_of_acc_no_price=sum(Avg..Price.Esitimate==0),
            bundle_price=mean(bundle_price),
            price_ref=sum(Avg..Price.Esitimate),
            price_ref_adjgift=sum(Avg..Price.Esitimate*(1-gift_or_not)))
result[is.na(result)]<-0
result<-result%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         num_of_acc_rel=total-first(total),
         premium=bundle_add_on-price_ref_rel,
         premium_percent=(bundle_add_on-price_ref_rel)/bundle_price,
         ind_firstbundle_noacc=first(total)==0)
result2<-result%>%
  filter(bundle_index!=0)%>%
  mutate(premref1=premium-first(premium))

deltan<-read.csv("relative_delta_with_bundle1.csv")
colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
result3<-result2%>%left_join(deltan,by=c("item_id"="item_id","bundle_index"="bundle_index"))
result3$diff=as.character(result3$diff)
result3$diff=as.numeric(result3$diff)
result3$diff[is.na(result3$diff)]=0

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_yi_0219.csv")

setwd("./graph")
#fixme automate the plot saving
# dev.on()
plot(result3$diff,result3$premref1,title(main="plot: delta_n ~ premref1"),xlab="delta_n",ylab="premref1")
boxplot(result3$premref1~result3$diff,xlab="delta_n",ylab="premref1",main="Premium rel B1 vs. delta n")
boxplot(result3$premref1~result3$bundle_index,xlab="bundle_index",ylab="premref1",main="Premium rel B1 vs. bundle_index")
model=lm(result3$premref1~result3$diff + factor(result3$item_id) -1 )
summary(model)
