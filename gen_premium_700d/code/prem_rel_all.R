camera_name="700d"
under_s = "_"

# library(DataComputing)
# install.packages("scales")

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

####################################################fixme
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium_700d")
#setwd("F:\\Dropbox\\urap_programming\\yi\\gen_premium_700d")
setwd("/Users/suyanglu/Dropbox/urap_programming/yi/gen_premium_700d")

#updateme
filename_list="lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao_0306.csv"
filename_dict="dict_700d_750d_Feb082016_zemin0309.csv"
filename_regression="regresstion_data.csv"
df_input="transec_700d_cleaned_20160215.csv"
file_name_category="good_format_dictIdx_table_pr_fk_20160314.csv"
filename_regression="regresstion_data.csv"

setwd("./input")
df_input = read.csv(df_input,head=TRUE)
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
Price<-read.csv(filename_dict,encoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
regression=read.csv(filename_regression,stringsAsFactors = FALSE)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"
Camera=Camera%>%filter(item_id!=45410114111)	

#updateme
df.category=read.csv(file_name_category,encoding="UTF-8",stringsAsFactors = FALSE)
df.category_rel_b0=read.csv(file=filename_regression,stringsAsFactors = FALSE)


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

#######Suyang#########
Priceofbundle0<-Camera%>%select(item_id, bundle_index, bundle_price)%>%filter(bundle_index==0)%>%select(-bundle_index)
colnames(Priceofbundle0)[colnames(Priceofbundle0)=="bundle_price"] <- "bundle_price_0"
Priceofbundle1<-Camera%>%select(item_id, bundle_index, bundle_price)%>%filter(bundle_index==1)%>%select(-bundle_index)
colnames(Priceofbundle1)[colnames(Priceofbundle1)=="bundle_price"] <- "bundle_price_1"

# Set all "unknown" or lost Price = 0
for(i in 1:length(Price$Avg..Price.Esitimate))
  if(Price$Avg..Price.Esitimate[i]==""|
     Price$Avg..Price.Esitimate[i]=="unknown"|
     Price$Avg..Price.Esitimate[i]=="Unknown"|
     is.na(Price$Avg..Price.Esitimate[i]))
    Price$Avg..Price.Esitimate[i]=0



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
  filter(bundle_index!=0)%>%
  filter(num_of_acc_rel!=0)







#Genarate a data frame "all"
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


#updateme
#comment this chunk out#############
result3=result3%>%
  left_join(df.category,by=c("item_id"="item_id","bundle_index1"="bundle_index_1","bundle_index2"="bundle_index_2"))
###################################


df.bundleprice=result%>%
  select(item_id,bundle_index,bundle_price)
colnames(df.bundleprice)[colnames(df.bundleprice)=="bundle_price"]="bundle_price1"

result3=result3%>%
  left_join(df.bundleprice,by=c("item_id"="item_id","bundle_index1"="bundle_index"))

colnames(df.bundleprice)[colnames(df.bundleprice)=="bundle_price1"]="bundle_price2"
            
result3=result3%>%
  left_join(df.bundleprice,by=c("item_id"="item_id","bundle_index2"="bundle_index"))%>%
  mutate(rel_cost_deln=bundle_price2-bundle_price1-premrel)


#####Suyang#####

result3<-result3%>%left_join(Priceofbundle0, by=c("item_id"="item_id"))
result3<-result3%>%left_join(Priceofbundle1, by=c("item_id"="item_id"))
result3<-result3%>%mutate(perc_premrel_relB0=premrel/bundle_price_0)%>%mutate(perc_premrel_relB1=premrel/bundle_price_1)


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
s11<-s1%>%select(item_id, item_sales)
s11<-unique(s11)
s12<-s1%>%select(-item_sales)
s12<-unique(s12)
# s1

n0<-s1 %>%filter(bundle_index==0)%>%summarise(Bundle0=sum(share))
n1<-s1 %>% filter(bundle_index==1)%>%summarise(B1=sum(share))
n2<-s1 %>% filter(bundle_index==2)%>%summarise(B2=sum(share))
n3<-s1 %>% filter(bundle_index==3)%>%summarise(B3=sum(share))
n4<-s1 %>%filter(bundle_index== 4)%>%summarise(B4=sum(share))
n5<-s1 %>% filter(bundle_index==5)%>%summarise(B5=sum(share))
n6<-s1 %>% filter(bundle_index==6)%>%summarise(B6=sum(share))
n7<-s1 %>% filter(bundle_index==7)%>%summarise(B7=sum(share))
n8<-s1 %>% filter(bundle_index==8)%>%summarise(B8=sum(share))
sum<-sum(n0,n1,n2,n3,n4,n5,n6,n7,n8)
Bundle0<-n0/sum
B1<-n1/sum
B2<-n2/sum
B3<-n3/sum
B4<-n4/sum
B5<-n5/sum
B6<-n6/sum
B7<-n7/sum
B8<-n8/sum

other0_1_8<-data.frame(Bundle0, Bundle1to8=sum(B1,B2,B3,B4,B5,B6,B7,B8))
other0_1_8<-other0_1_8%>%gather(bundle, value)
other0_1_2<-data.frame(B1, B2, B3,B4,B5,B6,B7,B8)
other0_1_2<-other0_1_2%>%gather(bundle, value)


setwd("../output")
setwd("./0_relative_to_bundle_0")
setwd("./graph_share")

png(file="Avg_Bundle_Share1.png")
other0_1_8$value <- 100*other0_1_8$value
p<-barplot(other0_1_8$value, main="Bundle Share", col= "white", ylim=c(0,70), xlab="Bundle Index", ylab="Bundle Share", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
labs <- paste(round(other0_1_8$value, 2), "%", sep="")
text(x = p, y = other0_1_8$value, label =labs, pos = 3, cex = 1)
axis(1, at=p, labels=other0_1_8$bundle, tick=FALSE, cex.axis = 1)
dev.off()

png(file="Avg_Bundle_Share2.png")                        
other0_1_2$value<-round(other0_1_2$value, 4)
other0_1_2$value <- 100*other0_1_2$value
q<-barplot(other0_1_2$value, main="Ave Bundle Share", col= "white", ylim=c(0,20), xlab="Bundle Index", ylab="Average Share", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
labs <- paste(round(other0_1_2$value, 2), "%", sep="")
text(x = q, y = other0_1_2$value, label =labs, pos = 3, cex = 1)
axis(1, at=q, labels=other0_1_2$bundle, tick=FALSE)
dev.off()




s12$bundle_index<- as.numeric(s1$bundle_index)
s12$filtered_item_share<-as.numeric(s1$filtered_item_share)

result3<-result3%>%left_join(s11, by=c("item_id"="item_id"), copy=FALSE)
result3<-result3%>%left_join(s12, by=c("item_id"="item_id", "bundle_index2"="bundle_index"), copy=FALSE)
result3<-unique(result3)
result3<-result3%>%ifelse(is.na(item_sales),item_sales==".", item_sales==item_sales)
result3[is.na(result3)]<-"0"
result6<-result3
result<-result%>%left_join(s11, by=c("item_id"="item_id"), copy=FALSE)%>%left_join(s12, by=c("item_id"="item_id", "bundle_index"="bundle_index"), copy=FALSE)
result_0<-result
result_0$filtered_bundle_sales<-as.numeric(result_0$filtered_bundle_sales)
result[is.na(result)]<-"0"





#0. Relative to Bundle 0

setwd("../")
setwd("../")
outputfilename = paste(paste(camera_name,under_s,"relative_to_bundle_0",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result_0,file=outputfilename)

setwd("./0_relative_to_bundle_0")

png(file="1_histogram_of_delta_n_rel_to_b0.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25))+geom_bar(aes(num_of_acc_rel))+labs(title="Histogram of Delta n",x="Delta n")
dev.off()

png(file="1_z_histogram_of_delta_n_rel_to_b0_in_num_of_acc_relerent_bundle_index_rel_to_b0.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25))+geom_bar(aes(num_of_acc_rel))+labs(title="Histogram of Delta n",x="Delta n")+ facet_wrap(~bundle_index, nrow =3)
dev.off()

png(file="2_boxplot_delta_n_bundle_index_rel_to_b0.png",bg="transparent")
boxplot(result$num_of_acc_rel~result$bundle_index,xlab="Bundle Index 2",ylab="Delta n",main="Delta n vs. Bundle Index 2",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="3_histogram_of_prem_rel_to_b0_2_col.png",bg="transparent")

result%>%ggplot()+theme(text = element_text(size=25))+
  geom_histogram(bins=2,boundary=0,aes(x=premium,y=(..density..)*2000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B0",x="Premium rel to B0",y="Percentage")

dev.off()

png(file="3_z_histogram_of_prem_rel_to_b0_2_col_in_num_of_acc_relerent_bundle_index.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+
  geom_histogram(bins=2,boundary=0,aes(x=premium,y=(..density..)*2000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B0",x="Premium rel to B0",y="Percentage")+
  facet_wrap(~bundle_index, nrow =3)
dev.off()


png(file="4_histogram_of_prem_rel_to_b0.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premium),color="white")+labs(title="Histogram of Premium rel to B0",x="Premium rel to B0")
dev.off()

png(file="4_z_histogram_of_prem_rel_to_b0_in_num_of_acc_relerent_bundle_index.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premium),color="white")+labs(title="Histogram of Premium rel to B0",x="Premium rel to B0")+ facet_wrap(~bundle_index, nrow =3)
dev.off()

png(file="5_histogram_of_prem_rel_to_b0_zoom_in_.png",bg="transparent")
result%>%
  filter(premium>=-200&premium<=200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premium),color="white")+labs(title="Histogram of Premium rel to B0",x="Premium rel to B0")
dev.off()

png(file="5_histogram_of_prem_rel_to_b0_zoom_in_in_num_of_acc_relerent_bundle_index.png",bg="transparent")
result%>%
  filter(premium>=-200&premium<=200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premium),color="white")+labs(title="Histogram of Premium rel to B0",x="Premium rel to B0")+ facet_wrap(~bundle_index, nrow =3)
dev.off()


png(file="6_boxplot_bundle_index_prem_rel_to_b0.png",bg="transparent")
boxplot(result$premium~result$bundle_index,xlab="Bundle Index",ylab="Premium rel to B0",main="Premium rel to B0 vs. Bundle Index", cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="7_boxplot_delta_n_prem_rel_to_b0.png",bg="transparent")
boxplot(result$premium~result$num_of_acc_rel,xlab="Delta n",ylab="Premium rel to B0",main="Premium rel to B0 vs. Delta n",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="7_z_histogram_bundle_price.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(bundle_price),color="white")+labs(title="Histogram of Bundle Price",x="Bundle Price")
dev.off()

png(file="7_z_histogram_bundle_price_in_different_bundle_index.png",bg="transparent")
result%>%ggplot()+theme(text = element_text(size=20),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(bundle_price),color="white")+labs(title="Histogram of Bundle Price",x="Bundle Price")+ facet_wrap(~bundle_index, nrow =3)
dev.off()


png(file="7_zz_histogram_bundle_price_zoom_in.png",bg="transparent")
result%>%
  filter(bundle_price>2800&bundle_price<4200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(bundle_price),color="white")+labs(title="Histogram of Bundle Price",x="Bundle Price")
dev.off()

png(file="7_zzz_histogram_bundle_price_in_different_bundle_index_zoom_in.png",bg="transparent")
result%>%
  filter(bundle_price>2800&bundle_price<4200)%>%
  ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(bundle_price),color="white")+labs(title="Histogram of Bundle Price",x="Bundle Price")+ facet_wrap(~bundle_index, nrow =3)
dev.off()


png(file="7_zzzz_boxplot_bundle_price_bundle_index.png",bg="transparent")
boxplot(result$bundle_price~result$bundle_index,xlab="Bundle Index",ylab="Bundle Price",main="Bundle Price vs. Bundle Index",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

df.category_rel_b0=Camera%>%
  filter(bundle_index!=0)%>%
  mutate(Type_of_acc=substr(dict_yi_index,1,2))%>%
  group_by(Type_of_acc)%>%
  summarise(Count=n())
  
png(file="8_1_barchart_of_type_of_acc_rel_to_B0.png")
ggplot(df.category_rel_b0)+theme(text = element_text(size=25))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Acc Types rel to B0")
dev.off()

png(file="8_1_z_barchart_of_type_of_acc_important_rel_to_B0.png")
result02=df.category_rel_b0%>%filter(Count>200)
ggplot(result02)+geom_bar(aes(x = reorder(Type_of_acc,desc(Count)),y=Count),stat="identity")+labs(title = "Barchart of Important Acc Types rel to B0",x="Type of Acc")+theme(text = element_text(size=25))
dev.off()


#select a subset of variables for barchart
png(file="8_2_barchart_of_type_of_acc_important_rel_to_B0.png")
result02=df.category_rel_b0%>%filter(Type_of_acc=="XB"|Type_of_acc=="CC"|Type_of_acc=="UV")
ggplot(result02)+geom_bar(aes(x = reorder(Type_of_acc,desc(Count)),y=Count),stat="identity")+labs(title = "Barchart of Important Acc Types rel to B0",x="Type of Acc")+theme(text = element_text(size=25))
dev.off()

result03=Camera%>%
  filter(bundle_index!=0)%>%
  mutate(Type_of_acc=substr(dict_yi_index,1,2))%>%
  group_by(bundle_index,Type_of_acc)%>%
  summarise(Count=n())%>%
  filter(Type_of_acc=="XB"|Type_of_acc=="CC"|Type_of_acc=="UV")
png(file="8_3_barchart_of_type_of_acc_important_in_different_bundle_index_rel_to_B0.png")
ggplot(result03)+geom_bar(aes(x = reorder(Type_of_acc,desc(Count)),y=Count),stat="identity")+labs(title = "Barchart of Important Acc Types rel to B0",x="Type of Acc")+facet_wrap(~bundle_index, nrow =3)+theme(text = element_text(size=25),axis.text=element_text(size=15))
dev.off()

##########################Suyang############################
setwd("./graph_share")
png(file="bundle_index_vs_bundle_share.png",bg="transparent")
boxplot(result_0$share~result_0$bundle_index,xlab="Bundle Index",ylab="Bundle Share", main="Bundle Index vs Bundle Sales", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
dev.off()

png(file="bundle_index_vs_bundle_sale.png", bg="transparent")
boxplot(result_0$bundle_sales~result_0$bundle_index, xlab="Bundle Index",ylab="Bundle Sales", main="Bundle Index vs Bundle Sales", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
dev.off()


png(file="filtered_bundle_share_vs_bundle_index.png",bg="transparent")
boxplot(result_0$filtered_item_share~result_0$bundle_index,xlab="Bundle Index",ylab="Filtered Bundle Share", main="Bundle Index vs Bundle Sales", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
dev.off()

png(file="filtered_bundle_sale_vs_bundle_index.png", bg="transparent")
boxplot(result_0$filtered_bundle_sales~result_0$bundle_index, xlab="Bundle Index",ylab="Filtered Bundle Sales", main="Bundle Index vs Bundle Sales", cex.lab=1.6, cex.axis=1.1, cex.main=2, cex.sub=1)
dev.off()

setwd("../")
setwd("../")
outputfilename = paste(paste(camera_name,under_s,"all",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result3,file = outputfilename)

outputfilename = paste(paste(camera_name,under_s,"share_filtered",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result6,file=outputfilename)

outputfilename = paste(paste(camera_name,under_s,"onlyshare_filtered",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(s1,file=outputfilename)

#fixme
#df.error=result3%>%filter(diff==0)%>%select(item_id,bundle_index1,bundle_index2)
#write.csv(df.error,file="error_item_id.csv")

png(file="share_bundle_index.png")
boxplot(s1$share~s1$bundle_index,xlab="bundle_index", ylab="share", main="share~bundle_index",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()
png(file="share_bundle_index_highlighted.png")
boxplot(s1$share~s1$bundle_index,xlab="bundle_index", ylab="share", main="share~bundle_index", col=ifelse(s1$item_id==16831203027, "red", "white"),cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()




#1. Relative to Bundle 1 
result4=result3%>%
  filter(bundle_index1==1)

setwd("../output")
outputfilename = paste(paste(camera_name,under_s,"relative_to_bundle_1",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result4,file = outputfilename)


setwd("./1_relative_to_bundle_1")

png(file="1_histogram_of_delta_n_rel_to_bundle_1.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")
dev.off()

png(file="1_z_histogram_of_delta_n_in_different_bundle_index_rel_to_bundle_1.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="2_boxplot_delta_n_bundle_index2_rel_to_bundle_1.png",bg="transparent")
boxplot(result4$diff~result4$bundle_index2,xlab="Bundle Index 2",ylab="Delta n",main="Delta n vs. Bundle Index 2",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="3_histogram_of_prem_rel_to_bundle_1_2_col.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B1",x="Premium rel to B1",y="Percentage")
  
#   +geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="bin",colour="darkgreen") 
# 

dev.off()

png(file="3_z_histogram_of_prem_rel_to_bundle_1_2_col_in_different_bundle_index.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B1",x="Premium rel to B1",y="Percentage")+ 
  facet_wrap(~bundle_index2, nrow =3)
dev.off()


png(file="4_histogram_of_prem_rel_to_bundle_1.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1",x="Premium rel to B1")
dev.off()

png(file="4_z_histogram_of_prem_rel_to_bundle_1_in_different_bundle_index.png",bg="transparent")
result4%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1",x="Premium rel to B1")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="5_histogram_of_prem_rel_to_bundle_1_zoom_in_.png",bg="transparent")
result4%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1",x="Premium rel to B1")
dev.off()

png(file="5_histogram_of_prem_rel_to_bundle_1_zoom_in_in_different_bundle_index.png",bg="transparent")
result4%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1",x="Premium rel to B1")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()


png(file="6_boxplot_bundle_index_prem_rel_to_bundle_1.png",bg="transparent")
boxplot(result4$premrel~result4$bundle_index2,xlab="Bundle Index",ylab="Premium rel to B1",main="Premium rel to B1 vs. Bundle Index",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="7_boxplot_delta_n_prem_rel_to_bundle_1.png",bg="transparent")
boxplot(result4$premrel~result4$diff,xlab="Delta n",ylab="Premium rel to B1",main="Premium rel to B1 vs. Delta n",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

result41=result4%>%select(BD,BJ,BU,CC,CD,DC,DK,DZ,EJ,FC,FD,FH,FP,FW,GJ,GP,GS,HJ,JB,JC,JD,JG,JQ,JS,JZ,LJ,LP,MO,MS,MZ,N.,ND,PZ,QC,QT,RG,SJ,SP,TZ,UV,WD,WJ,XB,XN,YD,YK,ZG)
result42=cbind(rownames(as.data.frame(colSums(result41))),as.data.frame(colSums(result41)))
colnames(result42)=c("Type_of_acc","Count")
png(file="8_1_barchart_of_type_of_acc_rel_to_bundle_1.png")
ggplot(result42)+theme(text = element_text(size=25),axis.text=element_text(size=10))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Acc Types in Delta n")
dev.off()

png(file="8_2_barchart_of_type_of_acc_important_rel_to_bundle_1.png")
result42=result42%>%filter(result42$Count>100)
ggplot(result42)+theme(text = element_text(size=25))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n")
dev.off()

png(file="8_2_z_barchart_of_type_of_acc_important_rel_to_bundle_1.png")
result42=result42%>%filter(result42$Count>100&Type_of_acc!="DC"&Type_of_acc!="SJ")
ggplot(result42)+theme(text = element_text(size=25))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n")
dev.off()

result43=result4%>%
  group_by(bundle_index2)%>%
  summarise(CC=sum(CC),UV=sum(UV),XB=sum(XB))%>%
  gather(key=Type_of_acc,value=Count,CC,UV,XB)
png(file="8_3_z_barchart_of_type_of_acc_important_in_different_bundle_index_rel_to_bundle_1.png")
ggplot(result43)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n in Different Bundle Index")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

result43=result4%>%
  group_by(bundle_index2)%>%
  summarise(CC=sum(CC),DC=sum(DC),SJ=sum(SJ),UV=sum(UV),XB=sum(XB))%>%
  gather(key=Type_of_acc,value=Count,CC,DC,SJ,UV,XB)
png(file="8_3_barchart_of_type_of_acc_important_in_different_bundle_index_rel_to_bundle_1.png")
ggplot(result43)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n in Different Bundle Index")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()



# png(file="9_1_histogram_of_num_of_fake_1")
# result41=result4%>%
#   group_by(bundle_index2,num_of_fake_1)%>%
#   summarize(count=n())%>%
#   filter(!is.na(num_of_fake_1))
# ggplot(result41)+geom_bar(aes(num_of_fake_1,count), stat="identity")+labs(title = "Histogram of num_of_fake_1")
# dev.off()
# 
# png(file="9_2_histogram_of_num_of_fake_1_facet_by_bundle_index2")
# ggplot(result41)+geom_bar(aes(num_of_fake_1,count), stat="identity")+facet_wrap(~bundle_index2, nrow = 3) + labs(title = "Histogram of num_of_fake_1")
# dev.off()
# 
# png(file="9_3")  ####################################fixme################################
# 
# png(file="9_histogram_of_num_of_fake_2")
# result42=result4%>%
#   group_by(num_of_fake_2)%>%
#   summarize(count=n())%>%
#   filter(!is.na(num_of_fake_2))
# ggplot(result42)+geom_bar(aes(num_of_fake_2,count), stat="identity")+labs(title = "Histogram of num_of_fake_1")
# dev.off()




#2. Relative to Bundle 1(for B2~B5) and Bundle 5(for B6~B8)
setwd("..")
result5=result3%>%
  filter((bundle_index1==1&bundle_index2<=5)|bundle_index1==5)

outputfilename = paste(paste(camera_name,under_s,"relative_to_bundle_1_and_5",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result5,file = outputfilename)

setwd("./2_relative_to_bundle_1_and_5")

png(file="1_histogram_of_delta_n_rel_to_bundle_1_and_5.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")
dev.off()

png(file="1_z_histogram_of_delta_n_in_different_bundle_index_rel_to_bundle_1_and_5.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="2_boxplot_delta_n_bundle_index2_rel_to_bundle_1_and_5.png",bg="transparent")
boxplot(result5$diff~result5$bundle_index2,xlab="Bundle Index 2",ylab="Delta n",main="Delta n vs. Bundle Index 2",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="3_histogram_of_prem_rel_to_bundle_1_and_5_2_col.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5",y="Percentage")
dev.off()

png(file="3_z_histogram_of_prem_rel_to_bundle_1_and_5_2_col_in_different_bundle_index.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5",y="Percentage")+ 
  facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="4_histogram_of_prem_rel_to_bundle_1_and_5.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5")
dev.off()

png(file="4_z_histogram_of_prem_rel_to_bundle_1_and_5_in_different_bundle_index.png",bg="transparent")
result5%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="5_histogram_of_prem_rel_to_bundle_1_and_5_zoom_in_.png",bg="transparent")
result5%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5")
dev.off()

png(file="5_histogram_of_prem_rel_to_bundle_1_and_5_zoom_in_in_different_bundle_index.png",bg="transparent")
result5%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium rel to B1 and B5",x="Premium rel to B1 and B5")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()


png(file="6_boxplot_bundle_index_prem_rel_to_bundle_1_and_5.png",bg="transparent")
boxplot(result5$premrel~result5$bundle_index2,xlab="Bundle Index",ylab="Premium rel to B1 and B5",main="Premium rel to B1 and B5 vs. Bundle Index",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="7_boxplot_delta_n_prem_rel_to_bundle_1_and_5.png",bg="transparent")
boxplot(result5$premrel~result5$diff,xlab="Delta n",ylab="Premium rel to B1 and B5",main="Premium rel to B1 and B5 vs. Delta n",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

result51=result5%>%select(BD,BJ,BU,CC,CD,DC,DK,DZ,EJ,FC,FD,FH,FP,FW,GJ,GP,GS,HJ,JB,JC,JD,JG,JQ,JS,JZ,LJ,LP,MO,MS,MZ,N.,ND,PZ,QC,QT,RG,SJ,SP,TZ,UV,WD,WJ,XB,XN,YD,YK,ZG)
result52=cbind(rownames(as.data.frame(colSums(result51))),as.data.frame(colSums(result51)))
colnames(result52)=c("Type_of_acc","Count")
png(file="8_1_barchart_of_type_of_acc_rel_to_bundle_1_and_5.png")
ggplot(result52)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Acc Types in Delta n")
dev.off()

png(file="8_2_barchart_of_type_of_acc_important_rel_to_bundle_1_and_5.png")
result52=result52%>%filter(result52$Count>80)
ggplot(result52)+theme(text = element_text(size=25))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n")
dev.off()

result53=result5%>%
  group_by(bundle_index2)%>%
  summarise(CC=sum(CC),DC=sum(DC),SJ=sum(SJ),UV=sum(UV),XB=sum(XB))%>%
  gather(key=Type_of_acc,value=Count,CC,DC,SJ,UV,XB)
png(file="8_3_barchart_of_type_of_acc_important_in_different_bundle_index_rel_to_bundle_1_and_5.png")
ggplot(result53)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n in Different Bundle Index")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()


#3. Adjacent
setwd("..")
result6=result3%>%
  filter(bundle_index1==bundle_index2-1)

outputfilename = paste(paste(camera_name,under_s,"adjacent",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result6,file = outputfilename)

setwd("./3_adjacent")


png(file="1_histogram_of_delta_n_adjacent.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")
dev.off()

png(file="1_z_histogram_of_delta_n_in_different_bundle_index_adjacent.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(diff))+labs(title="Histogram of Delta n",x="Delta n")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="2_boxplot_delta_n_bundle_index2_adjacent.png",bg="transparent")
boxplot(result6$diff~result6$bundle_index2,xlab="Bundle Index 2",ylab="Delta n",main="Delta n vs. Bundle Index 2",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="3_histogram_of_prem_adjacent_2_col.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium Adjacent",x="Premium Adjacent",y="Percentage")
dev.off()

png(file="3_z_histogram_of_prem_adjacent_2_col_in_different_bundle_index.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+
  geom_histogram(bins=2,boundary=0,aes(x=premrel,y=(..density..)*1000),color="white")+
  scale_y_continuous(labels = percent)+ 
  labs(title="Histogram of Premium Adjacent",x="Premium Adjacent",y="Percentage")+ 
  facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="4_histogram_of_prem_adjacent.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium Adjacent",x="Premium Adjacent")
dev.off()

png(file="4_z_histogram_of_prem_adjacent_in_different_bundle_index.png",bg="transparent")
result6%>%ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium Adjacent",x="Premium Adjacent")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()

png(file="5_histogram_of_prem_adjacent_zoom_in_.png",bg="transparent")
result6%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium Adjacent",x="Premium Adjacent")
dev.off()

png(file="5_histogram_of_prem_adjacent_zoom_in_in_different_bundle_index.png",bg="transparent")
result6%>%
  filter(premrel>=-200&premrel<=200)%>%
  ggplot()+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_histogram(bins=30,aes(premrel),color="white")+labs(title="Histogram of Premium Adjacent",x="Premium Adjacent")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()


png(file="6_boxplot_bundle_index_prem_adjacent.png",bg="transparent")
boxplot(result6$premrel~result6$bundle_index2,xlab="Bundle Index",ylab="Premium Adjacent",main="Premium Adjacent vs. Bundle Index",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

png(file="7_boxplot_delta_n_prem_adjacent.png",bg="transparent")
boxplot(result6$premrel~result6$diff,xlab="Delta n",ylab="Premium Adjacent",main="Premium Adjacent vs. Delta n",cex.lab=1.6, cex.axis=1, cex.main=2, cex.sub=1)
dev.off()

result61=result6%>%select(BJ,BU,CC,CD,DC,DK,DZ,EJ,FC,FD,FH,FP,FW,GJ,GP,GS,HJ,JB,JC,JD,JG,JQ,JS,JZ,LJ,LP,MO,MS,MZ,N.,ND,PZ,QC,QT,RG,SJ,SP,TZ,UV,WD,WJ,XB,XN,YD,YK,ZG)
result62=cbind(rownames(as.data.frame(colSums(result61))),as.data.frame(colSums(result61)))
colnames(result62)=c("Type_of_acc","Count")
png(file="8_1_barchart_of_type_of_acc_adjacent.png")
ggplot(result62)+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Acc Types in Delta n")+theme(text = element_text(size=25))
dev.off()

png(file="8_2_barchart_of_type_of_acc_important_adjacent.png")
result62=result62%>%filter(result62$Count>50)
ggplot(result62)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n")
dev.off()

result63=result6%>%
  group_by(bundle_index2)%>%
  summarise(CC=sum(CC),DC=sum(DC),SJ=sum(SJ),UV=sum(UV),XB=sum(XB))%>%
  gather(key=Type_of_acc,value=Count,CC,DC,SJ,UV,XB)
png(file="8_3_barchart_of_type_of_acc_important_in_different_bundle_index_adjacent.png")
ggplot(result63)+theme(text = element_text(size=25),axis.text=element_text(size=15))+geom_bar(aes(Type_of_acc,Count),stat="identity")+labs(title = "Barchart of Important Acc Types in Delta n in Different Bundle Index")+ facet_wrap(~bundle_index2, nrow =3)
dev.off()
