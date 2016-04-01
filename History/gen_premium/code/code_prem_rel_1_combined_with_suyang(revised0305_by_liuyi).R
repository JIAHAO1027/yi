library(DataComputing)
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="new_lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao( 2016-03-05).csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"
df=Camera%>%
  select(item_id,bundle_index,dict_index)%>%
  filter(bundle_index!=0)

deltan=data.frame()
i=1
while(i<=length(df$item_id))
{
  count_n=df%>%
    filter(item_id==item_id[i]&bundle_index==bundle_index[i])%>%
    tally()
  n=count_n[1,1]
  j=i+n
  a=as.numeric(c(df$item_id[i],df$bundle_index[i],df$bundle_index[j],n))
  while(df$item_id[j]==df$item_id[i]&j<=length(df$item_id))
  {
    t=0
    for(k in i:(i+n-1))
      if(df$dict_index[j]==df$dict_index[k]) t=t+1
      if(t==0)
        a=a+c(0,0,0,1)
      else a=a+c(0,0,0,-1)
      if(df$bundle_index[j]!=df$bundle_index[j+1])
      {
        deltan=rbind(deltan,a)
        a=as.numeric(c(df$item_id[i],df$bundle_index[i],df$bundle_index[j+1],n))
      }
      j=j+1
  }
  i=i+n
}
colnames(deltan)=c("item_id","bundle_index1","bundle_index2","diff")

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
###
result[is.na(result)]<-0
result<-result%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         num_of_acc_rel=total-first(total),
         premium=bundle_add_on-price_ref_rel,
         premium_percent=(bundle_add_on-price_ref_rel)/bundle_price,
         ind_firstbundle_noacc=first(total)==0)%>%
  filter(bundle_index!=0)

result2=data.frame()

i=1
while(i<=length(result$item_id))
{
  j=i+1
  while(j<=length(result$item_id)&result$item_id[j]==result$item_id[i])
  {
    result2=rbind(result2,
                  c(result$item_id[j],
                    result$bundle_index[i],
                    result$bundle_index[j],
                    result$premium[j]-result$premium[i]))
    j=j+1
  }
  i=i+1
}  

colnames(result2)=c("item_id","bundle_index1","bundle_index2","premrel")


# result2=result2%>%
#   left_join(Benchmark,by=c("item_id"="item_id"))%>%
#   filter(bundle_index!=1)%>%
#   mutate(premrel_perc=premrel/benchmark)


result3<-result2%>%left_join(deltan,by=c("item_id"="item_id","bundle_index1"="bundle_index1","bundle_index2"="bundle_index2"))

result3$diff[is.na(result3$diff)]=0

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_premrelToEachOther_0305.csv")

setwd("./graph")

# png(file="histogram of price of bundle 0.png",bg="transparent")
# hist(result3$benchmark,main="histogram of price of bundle 0",xlab="price of bundle 0")
# dev.off()

png(file="histogram of delta n.png",bg="transparent")
hist(result3$diff,main="histogram of delta n",xlab="delta n",breaks=20)
dev.off()

# png(file="histogram of bundle index.png",bg="transparent")
# hist(result3$bundle_index,main="histogram of bundle index",xlab="bundle index",breaks=8)
# dev.off()

png(file="histogram of prem rel(2 col).png",bg="transparent")
hist(result3$premrel,breaks=1,main="2 column histogram of prem rel to each other",xlab="prem rel to each other")
dev.off()

png(file="histogram of prem rel.png",bg="transparent")
hist(result3$premrel,breaks=20,main="histogram of prem rel to each other",xlab="prem rel to each other")
dev.off()

# png(file="histogram of prem rel 1 perc.png",bg="transparent")
# hist(result2$premrel1_perc,main="histogram of percentage of prem rel 1",xlab="percentage of prem rel 1")
# dev.off()
# 
# png(file="boxplot- delta_n~bundle_index.png",bg="transparent")
# boxplot(result3$result3$diff~bundle_index,main="Delta n vs Bundle Index",xlab="delta_n",ylab="bundle_index")
# dev.off()

png(file="boxplot- delta_n~premrel.png",bg="transparent")
boxplot(result3$premrel~result3$diff,xlab="delta_n",ylab="premrel",main="Premium rel to each other vs. delta n")
dev.off()
# 
# png(file="boxplot- bundle_index~premrel1.png",bg="transparent")
# boxplot(result3$premrel1~result3$bundle_index,xlab="bundle_index",ylab="premrel1",main="Premium rel B1 vs. bundle index")
# dev.off()
# 
# result4=result3%>%filter(premrel1>-500&premrel1<500)
# png(file="boxplot- bundle_index~premrel1(zoom).png",bg="transparent")
# boxplot(result4$premrel1~result4$bundle_index,xlab="bundle_index",ylab="premrel1",main="Premium rel B1 vs. bundle index")
# dev.off()

model1=lm(result3$premrel~result3$diff + factor(result3$item_id) -1 )
summary(model1)
# 
# model2=lm(result3$premrel1~result3$bundle_index+ factor(result3$item_id) -1 )
# summary(model2)
