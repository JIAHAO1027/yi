
###########generate premium rel to Bundle 1 not considering those acc in bundle 1 but not in bundle j, referred to as "a" in the code 

##########

library(DataComputing)
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
Camera$bundle_index=as.numeric(Camera$bundle_index)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

###perc_premium reference denominator
###For fist attempt, use bundle 0 price as the benchmark
Benchmark=Camera%>%
  group_by(item_id)%>%
  summarise(benchmark=first(bundle_price))

Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
Camera[,27:50]=0
colnames(Camera)[32]="item_in_bundle2"
colnames(Camera)[33]="item_in_bundle3"
colnames(Camera)[34]="item_in_bundle4"
colnames(Camera)[35]="item_in_bundle5"
colnames(Camera)[36]="item_in_bundle6"
colnames(Camera)[37]="item_in_bundle7"
colnames(Camera)[38]="item_in_bundle8"
colnames(Camera)[39]="item_in_bundle9"
colnames(Camera)[40]="item_in_bundle10"

colnames(Camera)[42]="price_item_in_bundle2"
colnames(Camera)[43]="price_item_in_bundle3"
colnames(Camera)[44]="price_item_in_bundle4"
colnames(Camera)[45]="price_item_in_bundle5"
colnames(Camera)[46]="price_item_in_bundle6"
colnames(Camera)[47]="price_item_in_bundle7"
colnames(Camera)[48]="price_item_in_bundle8"
colnames(Camera)[49]="price_item_in_bundle9"
colnames(Camera)[50]="price_item_in_bundle10"


Camera=Camera%>%filter(bundle_index!=0)

i=1
while(i<length(Camera$item_id))
{
  if(Camera$bundle_index[i]==1)
  {
    j=i+1
    while(Camera$item_id[j]==Camera$item_id[i]&j<length(Camera$item_id))
    {
      if(Camera$dict_index[j]==Camera$dict_index[i])
      {
        Camera[i,(30+Camera$bundle_index[j])]=1
      }
      j=j+1
    }
  }
  i=i+1
}


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
CameraWPrice[,32:40]=(1-CameraWPrice[,32:40])
CameraWPrice[,42:50]=CameraWPrice[,32:40]*CameraWPrice$Avg..Price.Esitimate

Total_n_of_a<-CameraWPrice%>%
  filter(bundle_index==1)%>%
  group_by(item_id)%>%
  summarise(sum(item_in_bundle2),sum(item_in_bundle3),sum(item_in_bundle4),
            sum(item_in_bundle5),sum(item_in_bundle6),sum(item_in_bundle7),
            sum(item_in_bundle8),sum(item_in_bundle9),sum(item_in_bundle10))

Total_price_of_a<-CameraWPrice%>%
  filter(bundle_index==1)%>%
  group_by(item_id)%>%
  summarise(sum(price_item_in_bundle2),sum(price_item_in_bundle3),sum(price_item_in_bundle4),
            sum(price_item_in_bundle5),sum(price_item_in_bundle6),sum(price_item_in_bundle7),
            sum(price_item_in_bundle8),sum(price_item_in_bundle9),sum(price_item_in_bundle10))

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

result2=result2%>%
  left_join(Benchmark,by=c("item_id"="item_id"))


i=1
while(i<=length(result2$item_id))
{
  r=Total_price_of_a$item_id==result2$item_id[i]
  result2$premium_minus_a[i]=result2$premrel1[i]-Total_price_of_a[r,result2$bundle_index[i]]
  i=i+1
}
result2$premium_minus_a=as.numeric(result2$premium_minus_a)
result2<-result2%>%
  filter(bundle_index!=1)%>%
  mutate(premium_minus_a_perc=premium_minus_a/benchmark)

deltan<-read.csv("new_diff_1_2.csv")
colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
result3<-result2%>%left_join(deltan,by=c("item_id"="item_id","bundle_index"="bundle_index"))
result3$diff=as.character(result3$diff)
result3$diff=as.numeric(result3$diff)
result3$diff[is.na(result3$diff)]=0

i=1
while(i<=length(result3$item_id))
{
  rr=Total_n_of_a$item_id==result3$item_id[i]
  result3$diff_minus_a[i]=result3$diff[i]-Total_n_of_a[rr,result3$bundle_index[i]]
  i=i+1
}
result3$diff_minus_a=as.numeric(result3$diff_minus_a)

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_prem_minus_a_0223.csv")

setwd("./graph")

png(file="histogram of price of bundle 0.png",bg="transparent")
hist(result3$benchmark,main="histogram of price of bundle 0",xlab="price of bundle 0")
dev.off()

png(file="histogram of delta n minus a.png",bg="transparent")
hist(result3$diff_minus_a,main="histogram of delta n minus a",xlab="delta n",breaks=20)
dev.off()

png(file="histogram of bundle index.png",bg="transparent")
hist(result3$bundle_index,main="histogram of bundle index",xlab="bundle index",breaks=8)
dev.off()

png(file="histogram of prem rel 1 minus a(2 col).png",bg="transparent")
hist(result3$premium_minus_a,breaks=1,main="2 column histogram of prem rel 1 minus a",xlab="prem rel 1 minus a")
dev.off()

png(file="histogram of prem rel 1 minus a.png",bg="transparent")
hist(result3$premium_minus_a,main="histogram of prem rel 1 minus a",xlab="prem rel 1 minus a")
dev.off()

png(file="histogram of prem rel 1 minus a perc.png",bg="transparent")
hist(result3$premium_minus_a_perc,main="histogram of percentage of prem rel 1 minus a",xlab="percentage of prem rel 1 minus a")
dev.off()

png(file="boxplot- delta_n_minus_a~bundle_index.png",bg="transparent")
boxplot(result3$bundle_index~result3$diff_minus_n,main="Delta n minus avs Bundle Index",xlab="delta_n_minus_a",ylab="bundle_index")
dev.off()

png(file="boxplot- delta_n_minus_a ~premrel1_minus_a.png",bg="transparent")
boxplot(result3$premium_minus_a ~result3$diff_minus_a,xlab="delta_n_minus_a ",ylab="premrel1_minus_a ",main="Premium rel B1_minus_a vs. delta n_minus_a ")
dev.off()

png(file="boxplot- bundle_index~premium_minus_a.png",bg="transparent")
boxplot(result3$premium_minus_a~result3$bundle_index,xlab="bundle_index",ylab="premium_minus_a",main="premium_minus_a vs. bundle_index")
dev.off()

model1=lm(result3$premium_minus_a ~result3$diff_minus_a + factor(result3$item_id) -1 )
summary(model1)

model2=lm(result3$premium_minus_a ~result3$bundle_index+ factor(result3$item_id) -1 )
summary(model2)
