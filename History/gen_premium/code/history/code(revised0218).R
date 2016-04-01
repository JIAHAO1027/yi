library(DataComputing)
# setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
#filename_list="Feb062016_750D_CombineList.csv"
#filename_dict="dict_700d_750d_Feb082016.csv"
filename_dict="dict_700d_price_youyi.csv"

setwd("./input")
Camera<-read.csv(filename_list)
Price<-read.csv(filename_dict)%>%select(index_final,price_lower_bound,price_upper_bound)
Price$price_lower_bound<-as.character(Price$price_lower_bound)

# Set all "unknown" or lost Price = 0
i=1
while(i<length(Price$price_lower_bound))
{
  if(Price$price_lower_bound[i]==""||Price$price_lower_bound[i]=="unknown"||Price$price_lower_bound[i]=="Unknown")
    Price$price_lower_bound[i]=0
  i=i+1
}

Price$price_upper_bound<-as.character(Price$price_upper_bound)

# Set all "unknown" or lost Price = 0
i=1
while(i<length(Price$price_lower_bound))
{
  if(Price$price_lower_bound[i]==""||Price$price_lower_bound[i]=="unknown"||Price$price_lower_bound[i]=="Unknown")
    Price$price_lower_bound[i]=0
  i=i+1
}


Camera$dict_yi_index<-as.character(Camera$dict_yi_index)
Price$index_final<-as.character(Price$index_final)
Price$price_lower_bound<-as.numeric(Price$price_lower_bound)
Price[is.na(Price)]<-0
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_yi_index"="index_final"))
CameraWPrice[is.na(CameraWPrice)]<-0
result<-CameraWPrice%>%
  group_by(ItemID.LIsting_id.,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_yi_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_yi_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('ԭװ',grepl,Accessory_title))),
            num_of_acc_no_price=sum(price_lower_bound==0),
            bundle_price=mean(bundle_price),
            price_ref=sum(price_lower_bound*(1-gift_or_not)))
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

setwd("../output")
write.csv(result3,file = "numofacc_and_premium_700D_youyilow.csv")

