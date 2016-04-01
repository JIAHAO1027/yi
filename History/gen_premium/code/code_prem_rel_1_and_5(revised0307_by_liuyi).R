library(DataComputing)
library(lfe)
####################################################fixme
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium_new")
setwd("/Users/suyanglu/Dropbox/urap_programming/yi/gen_premium_new")
filename_list="lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao_0306.csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"
filename_regression="regresstion_data.csv"
df_input="transec_700d_cleaned_20160215.csv"

setwd("./input")
df_input = read.csv(filename_transaction,head=TRUE)
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

#Genarate delta n
df=Camera%>%
  select(item_id,bundle_index,dict_yi_index)%>%
  filter(bundle_index!=0)

##i,j, k index the row numbers for columns bundle_index1, bundle_index2, dict_yi_index resp.
df.deltan=data.frame()
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
      if(df$dict_yi_index[j]==df$dict_yi_index[k]) t=t+1
    if(t==0)
      a=a+c(0,0,0,1)
    else a=a+c(0,0,0,-1)
    if(df$bundle_index[j]!=df$bundle_index[j+1]|(j+1)>length(df$item_id))
    {
      df.deltan=rbind(df.deltan,a)
      a=as.numeric(c(df$item_id[i],df$bundle_index[i],df$bundle_index[j+1],n))
    }
    j=j+1
  }
  i=i+n
}
colnames(df.deltan)=c("item_id","bundle_index1","bundle_index2","diff")



#Set a benchmark as denominator for "prem_perc"
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

#Generate premium and a lot of indicators
Camera$dict_yi_index<-as.character(Camera$dict_yi_index)
Price$index_final<-as.character(Price$index_final)
Price$Avg..Price.Esitimate<-as.numeric(Price$Avg..Price.Esitimate)
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_yi_index"="index_final"))
result<-CameraWPrice%>%
  group_by(item_id,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_yi_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('????',grepl,Accessory_title))),
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
         ind_firstbundle_noacc=first(total)==0)%>%
  filter(bundle_index!=0)

#Genarate a 
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

result3<-result2%>%left_join(df.deltan,by=c("item_id"="item_id","bundle_index1"="bundle_index1","bundle_index2"="bundle_index2"))
result3$diff[is.na(result3$diff)]=0
result3=result3%>%
  filter(premrel>-500&premrel<500)



### add share

df_input<-df_input %>% select(item_id, amount, color_category_ind_18_55, bundle_index)
#group the cells
grouped_sales <- df_input %>% group_by(item_id) %>% summarize(item_sales=sum(amount))

grouped_bundle <- df_input %>% group_by(item_id, bundle_index) %>% summarize(bundle_sales=sum(amount))
grouped_f_sales <- df_input%>%group_by(item_id, color_category_ind_18_55) %>% filter(color_category_ind_18_55==1)%>%summarize(filterted_item_sales=sum(amount))

grouped_f_bundle = df_input %>%group_by(item_id, bundle_index, color_category_ind_18_55)%>%filter(color_category_ind_18_55==1)%>%summarize(filtered_bundle_sales=sum(amount))
#grouped_f_bundle =grouped_f_bundle%>%select(item_id, bundle_index,filtered_bundle_sales)

#merge
df1 = data.frame(grouped_sales)
df2 = data.frame(grouped_bundle)
s1 <- merge(df1,df2,by="item_id")
df3 <- data.frame(grouped_f_sales%>%select(item_id,filterted_item_sales ))
df4 <- data.frame(grouped_f_bundle%>%select(item_id, bundle_index,filtered_bundle_sales))
s1 = left_join(s1, df3, by='item_id')
s1 = left_join(s1, df4, by=c("item_id", "bundle_index"))
#share function
count<-function(x){
  return(x['bundle_sales'] / x['item_sales'])
}

f_count<-function(x){
  return(x['filtered_bundle_sales'] / x['filterted_item_sales'])
}
#counting share
s1['share'] <- count(s1)
s1['filtered_item_share'] <- f_count(s1)
# s1
s1[is.na(s1)]<-"."
s1$bundle_index<-as.numeric(s1$bundle_index)
result3<-result3%>%left_join(s1, by=c("item_id"="item_id", "bundle_index2"="bundle_index"), copy=TRUE)

setwd("../output")
write.csv(result3,file = "all.csv")
write.csv(s1,file="share_filtered.csv")




#1. Relative to Bundle 1 (with Jiahao's indicator for each "CC","DC",...)
setwd("../input")
result4=result3%>%
  filter(bundle_index1==1)
regression=read.csv(filename_regression,stringsAsFactors = FALSE)
regression[,-c(2,3)][regression[,-c(2,3)]>0]=1
result4=result4%>%
  left_join(regression[,-1],by=c("item_id"="ItemID","bundle_index2"="index"))
setwd("../output/1.relative_to_bundle_1")
write.csv(result4,file = "relative_to_bundle_1.csv")

png(file="1.histogram of delta n.png",bg="transparent")
hist(result4$diff,main="histogram of delta n",xlab="delta n",breaks=20)
dev.off()

png(file="2.histogram of prem rel to bundle 1(2 col).png",bg="transparent")
hist(result4$premrel,breaks=1,main="2 column histogram of prem rel to bundle 1",xlab="prem rel to bundle 1")
dev.off()

png(file="3.histogram of prem rel to bundle 1.png",bg="transparent")
hist(result4$premrel,breaks=20,main="histogram of prem rel to bundle 1",xlab="prem rel to bundle 1")
dev.off()

png(file="4.boxplot- bundle_index~premrel1.png",bg="transparent")
boxplot(result4$premrel~result4$bundle_index2,xlab="bundle_index",ylab="prem rel to bundle 1",main="Premium rel to bundle 1 vs. bundle index")
dev.off()

png(file="5.boxplot- delta_n~premrel1.png",bg="transparent")
boxplot(result4$premrel~result4$diff,xlab="delta n",ylab="prem rel to bundle 1",main="Premium rel to bundle 1 vs. delta n")
dev.off()


#2. Relative to Bundle 1(for B2~B5) and Bundle 5(for B6~B8)
setwd("../2.relative_to_bundle_1_and_5")
result5=result3%>%
  filter((bundle_index1==1&bundle_index2<=5)|bundle_index1==5)
write.csv(result5,file = "relative_to_bundle_1_and_5.csv")

png(file="1.histogram of delta n.png",bg="transparent")
hist(result5$diff,main="histogram of delta n",xlab="delta n",breaks=20)
dev.off()

png(file="2.histogram of prem rel to bundle 1 and 5(2 col).png",bg="transparent")
hist(result5$premrel,breaks=1,main="2 column histogram of prem rel to bundle 1 and 5",xlab="prem rel to bundle 1 and 5")
dev.off()

png(file="3.histogram of prem rel to bundle 1 and 5.png",bg="transparent")
hist(result5$premrel,breaks=20,main="histogram of prem rel to bundle 1 and 5",xlab="prem rel to bundle 1 and 5")
dev.off()

png(file="4.boxplot- bundle_index~premrel1and5.png",bg="transparent")
#I will fix this to 2 colors this afternoon
boxplot(result5$premrel~result5$bundle_index2,xlab="bundle_index",ylab="prem rel to bundle 1 and 5",main="Premium rel to bundle 1 and 5 vs. bundle index")
dev.off()

png(file="5.boxplot- delta_n~premrel1and5.png",bg="transparent")
boxplot(result5$premrel~result5$diff,xlab="delta n",ylab="prem rel to bundle 1 and 5",main="Premium rel to bundle 1 and 5 vs. delta n")
dev.off()


#3. Adjacent
setwd("../3.adjacent")
result6=result3%>%
  filter(bundle_index1==bundle_index2-1)
write.csv(result6,file = "adjacent.csv")

png(file="1.histogram of delta n.png",bg="transparent")
hist(result6$diff,main="histogram of delta n",xlab="delta n",breaks=20)
dev.off()

png(file="2.histogram of prem adjacent.png",bg="transparent")
hist(result6$premrel,breaks=1,main="2 column histogram of prem adjacent",xlab="prem adjacent")
dev.off()

png(file="3.histogram of prem adjacent.png",bg="transparent")
hist(result6$premrel,breaks=20,main="histogram of prem adjacent",xlab="prem adjacent")
dev.off()

png(file="4.boxplot- bundle_index2~prem_adjacent.png",bg="transparent")
boxplot(result6$premrel~result6$bundle_index2,xlab="bundle_index2",ylab="prem adjacent",main="Premium adjacent vs. bundle index")
dev.off()

png(file="5.boxplot- delta_n~prem_adjacent.png",bg="transparent")
boxplot(result6$premrel~result6$diff,xlab="delta n",ylab="prem adjacent",main="Premium adjacent vs. delta n")
dev.off()
