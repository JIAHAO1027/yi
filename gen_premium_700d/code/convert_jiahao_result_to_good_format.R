camera_name="700d"
under_s = "_"

library(DataComputing)
library(lfe)
####################################################fixme
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium_700d")
setwd("F:\\Dropbox\\urap_programming\\yi\\gen_premium_700d")
#setwd("/Users/suyanglu/Dropbox/urap_programming/yi/gen_premium_700d")

#updateme
filename_list="lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao_0306.csv"
filename_dict="dict_700d_750d_Feb082016_zemin0309.csv"
filename_regression="regresstion_data.csv"
df_input="transec_700d_cleaned_20160215.csv"
file_name_category="dictIdx_table_pr_fk_20160314.csv"


setwd("./input")
df.category=read.csv(file_name_category,encoding="UTF-8",stringsAsFactors = FALSE)
colnames(df.category)[colnames(df.category)=="X.U.FEFF.ItemID"] <- "item_id"
df.category$two_bundle_index=substr(df.category$two_bundle_index,start=2,stop=nchar(df.category$two_bundle_index)-1)
df.category=df.category%>%separate(two_bundle_index,into=c("bundle_index_1","bundle_index_2"),sep=",")
df.category$bundle_index_1=as.numeric(df.category$bundle_index_1)
df.category$bundle_index_2=as.numeric(df.category$bundle_index_2)

df.category$price=as.numeric(df.category$price)
df.category$price[is.na(df.category$price)]=0

df.category=df.category%>%filter(dictIdx_top2!="")

df.category2=df.category%>%
  select(item_id,bundle_index_1,bundle_index_2,deltaN,dictIdx_top2,X)%>%
  group_by(item_id,bundle_index_1,bundle_index_2,deltaN,dictIdx_top2)%>%
  summarise(total=n())
df.category3=df.category2%>%spread(key=dictIdx_top2,value=total)
df.category3[is.na(df.category3)]=0
df.category4=df.category3
df.category4[,-c(1,2,3,4)][df.category4[,-c(1,2,3,4)]>0]=1
colnames(df.category4)=paste("ind",colnames(df.category4),sep="_")
df.category3=df.category3%>%
  left_join(df.category4,by=c("item_id"="ind_item_id","bundle_index_1"="ind_bundle_index_1","bundle_index_2"="ind_bundle_index_2","deltaN"="ind_deltaN"))

df.category5=df.category%>%
  filter(dictIdx_top2=="CC",first==1)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(price_of_CC_1=mean(price))
df.category3=df.category3%>%
  left_join(df.category5,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))

df.category5=df.category%>%
  filter(dictIdx_top2=="CC",first==2)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(price_of_CC_2=mean(price))
df.category3=df.category3%>%
  left_join(df.category5,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))

df.category5=df.category%>%
  filter(dictIdx_top2=="UV",first==1)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(price_of_UV_1=mean(price))
df.category3=df.category3%>%
  left_join(df.category5,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))

df.category5=df.category%>%
  filter(dictIdx_top2=="UV",first==2)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(price_of_UV_2=mean(price))
df.category3=df.category3%>%
  left_join(df.category5,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))

df.category6=df.category%>%
  filter(first==1)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(num_of_fake_1=sum(fake_brand_id))
df.category3=df.category3%>%
  left_join(df.category6,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))

df.category6=df.category%>%
  filter(first==2)%>%
  group_by(item_id,bundle_index_1,bundle_index_2)%>%
  summarise(num_of_fake_2=sum(fake_brand_id))
df.category3=df.category3%>%
  left_join(df.category6,by=c("item_id"="item_id","bundle_index_1"="bundle_index_1","bundle_index_2"="bundle_index_2"))


write.csv(df.category3,file = "good_format_dictIdx_table_pr_fk_20160314.csv")
