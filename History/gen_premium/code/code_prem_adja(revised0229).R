library(DataComputing)
setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
#setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao.csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"
filename_diff="gen_deln_adjacent.csv"

setwd("./input")
Camera<-read.csv(filename_list)
Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
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
  mutate(premrel1=premium-first(premium))

i=1
while(i<length(result2$item_id))
{
  if(result2$item_id[i]==result2$item_id[i+1])
    result2$prem_adjacent[i]=result2$premium[i+1]-result2$premium[i]
  i=i+1
}

deltan<-read.csv(filename_diff,stringsAsFactors = FALSE)
colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
deltan$bundle_index=substr(deltan$bundle_index,start=2,stop=nchar(deltan$bundle_index)-1)
deltan=deltan%>%separate(bundle_index,into=c("bundleindexlow","bundleindexhigh"),sep=",")
deltan$bundleindexlow=as.numeric(deltan$bundleindexlow)
deltan$bundleindexhigh=as.numeric(deltan$bundleindexhigh)

result2=result2%>%
  left_join(Benchmark,by=c("item_id"="item_id"))%>%
  mutate(prem_adjacent_perc=prem_adjacent/benchmark)

result3<-result2%>%inner_join(deltan,by=c("item_id"="item_id","bundle_index"="bundleindexlow"))
result3$diff=as.character(result3$diff)
result3$diff=as.numeric(result3$diff)
result3$diff[is.na(result3$diff)]=0

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_adjacent_0229.csv")

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

png(file="histogram of prem_adjacent(2 col).png",bg="transparent")
hist(result2$prem_adjacent,breaks=1,main="2 column histogram of prem_adjacent",xlab="prem_adjacent")
dev.off()

png(file="histogram of prem_adjacent.png",bg="transparent")
hist(result2$prem_adjacent,breaks=20,main="histogram of prem_adjacent",xlab="prem_adjacent",breaks=20)
dev.off()

png(file="histogram of prem_adjacent perc.png",bg="transparent")
hist(result2$prem_adjacent_perc,main="histogram of percentage of prem_adjacent",xlab="percentage of prem_adjacent")
dev.off()

png(file="boxplot- delta_n~bundle_index.png",bg="transparent")
boxplot(result3$result3$diff~bundle_index,main="Delta n vs Bundle Index",xlab="delta_n",ylab="bundle_index")
dev.off()

png(file="boxplot- delta_n~prem_adjacent.png",bg="transparent")
boxplot(result3$prem_adjacent~result3$diff,xlab="delta_n",ylab="prem_adjacent",main="Prem_adjacent vs. delta n")
dev.off()

png(file="boxplot- bundle_index~prem_adjacent.png",bg="transparent")
boxplot(result3$prem_adjacent~result3$bundle_index,xlab="bundle_index",ylab="prem_adjacent",main="Prem_adjacent vs. bundle index")
dev.off()

model1=lm(result3$prem_adjacent~result3$diff + factor(result3$item_id) -1 )
summary(model1)

model2=lm(result3$prem_adjacent~result3$diff + result3$bundle_index+ factor(result3$item_id) -1 )
summary(model2)

