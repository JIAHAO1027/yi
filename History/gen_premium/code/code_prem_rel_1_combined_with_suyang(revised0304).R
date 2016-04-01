library(DataComputing)
library(rPython)
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("/Users/suyanglu/Dropbox/urap_programming/yi/gen_premium/input")
filename_list="lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao.csv"
Camera<-read.csv(filename_list)
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"
Price<-read.csv(filename_dict,encoding = "UTF-8") %>% select(index_final,Avg..Price.Esitimate)

setwd("/Users/suyanglu/Desktop")
python.load("gen_deln_new.py")
setwd("/Users/suyanglu/Dropbox/urap_programming/suyang/gen_deln_adjacent")
filename_diff="gen_deln.csv"
deltan<-read.csv(filename_diff)


Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

Benchmark=Camera%>%
  group_by(item_id)%>%
  summarise(benchmark=first(bundle_price))

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
            num_of_yuanzhuang=sum(as.vector(sapply('????',grepl,Accessory_title))),
            num_of_acc_no_price=sum(Avg..Price.Esitimate==0),
            bundle_price=mean(bundle_price),
            price_ref=sum(Avg..Price.Esitimate),
            price_ref_adjgift=sum(Avg..Price.Esitimate*(1-gift_or_not)))
###
result[is.na(result)]<-0
result<-result%>%group_by(item_id)%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         num_of_acc_rel=total-first(total),
         premium=bundle_add_on-price_ref_rel,
         premium_percent=(bundle_add_on-price_ref_rel)/bundle_price,
         ind_firstbundle_noacc=first(total)==0)
#####

deltan2<-deltan%>%left_join(result, by=c("item_id"="item_id", "bundle_index2"="bundle_index"))%>%select(item_id, bundle_index1, bundle_index2, premium)
colnames(deltan2)[colnames(deltan2)=="premium"] <- "premium2"


colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
result3<-result%>%left_join(deltan,by=c("item_id"="item_id","bundle_index"="bundle_index1"))

result3%>%left_join(deltan2, by=c("item_id"="item_id","bundle_index"="bundle_index1","bundle_index2"="bundle_index2"))

result2<-result%>%
  filter(bundle_index!=0)%>%
  mutate(premrel=premium-first(premium))

result2=result2%>%
  left_join(Benchmark,by=c("item_id"="item_id"))%>%
  filter(bundle_index!=1)%>%
  mutate(premrel1_perc=premrel1/benchmark)


colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
result3<-result%>%left_join(deltan,by=c("item_id"="item_id","bundle_index"="bundle_index1"))
result3$diff=as.character(result3$diff)
result3$diff=as.numeric(result3$diff)
result3$diff[is.na(result3$diff)]=0

result2<-result3%>%
  filter(bundle_index!=0)%>%
  mutate(premrel=premium-(premium))

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_premrel1_0229.csv")

setwd("./graph")

png(file="histogram of price of bundle 0.png",bg="transparent")
hist(result3$benchmark,main="histogram of price of bundle 0",xlab="price of bundle 0")
dev.off()

png(file="histogram of delta n.png",bg="transparent")
hist(result3$diff,main="histogram of delta n",xlab="delta n",breaks=22)
dev.off()

png(file="histogram of bundle index.png",bg="transparent")
hist(result3$bundle_index,main="histogram of bundle index",xlab="bundle index",breaks=8)
dev.off()

png(file="histogram of prem rel 1(2 col).png",bg="transparent")
hist(result2$premrel1,breaks=1,main="2 column histogram of prem rel 1",xlab="prem rel 1")
dev.off()

png(file="histogram of prem rel 1.png",bg="transparent")
hist(result2$premrel1,breaks=25,main="histogram of prem rel 1",xlab="prem rel 1")
dev.off()

png(file="histogram of prem rel 1 perc.png",bg="transparent")
hist(result2$premrel1_perc,main="histogram of percentage of prem rel 1",xlab="percentage of prem rel 1")
dev.off()

png(file="boxplot- delta_n~bundle_index.png",bg="transparent")
boxplot(result3$result3$diff~bundle_index,main="Delta n vs Bundle Index",xlab="delta_n",ylab="bundle_index")
dev.off()

png(file="boxplot- delta_n~premrel1.png",bg="transparent")
boxplot(result3$premrel1~result3$diff,xlab="delta_n",ylab="premrel1",main="Premium rel B1 vs. delta n")
dev.off()

png(file="boxplot- bundle_index~premrel1.png",bg="transparent")
boxplot(result3$premrel1~result3$bundle_index,xlab="bundle_index",ylab="premrel1",main="Premium rel B1 vs. bundle index")
dev.off()

result4=result3%>%filter(premrel1>-500&premrel1<500)
png(file="boxplot- bundle_index~premrel1(zoom).png",bg="transparent")
boxplot(result4$premrel1~result4$bundle_index,xlab="bundle_index",ylab="premrel1",main="Premium rel B1 vs. bundle index")
dev.off()

model1=lm(result3$premrel1~result3$diff + factor(result3$item_id) -1 )
summary(model1)

model2=lm(result3$premrel1~result3$bundle_index+ factor(result3$item_id) -1 )
summary(model2)
