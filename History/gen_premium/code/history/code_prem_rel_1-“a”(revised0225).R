# ##Xin troubleshooting
# source('C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium/code/code_prem_rel_1-“a�?(revised0225).R', encoding = 'CP936')
# Error in file(filename, "r", encoding = encoding) : 
#   cannot open the connection
# In addition: Warning message:
#   In file(filename, "r", encoding = encoding) :
#   cannot open file 'C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium/code/code_prem_rel_1-': No such file or directory
##error msgs esp we still need to fix why DataComputing does not perform satisfactorily here and always gives error and warnings
###########This code generates premium rel to Bundle 1 not considering those acc in bundle 1 but not in bundle j, referred to as "a" in the code 

#combine the 700d and 750d lst data into one input file and add col $camera_name = 700d, 750d resp. as the last column
# fixme add in the following variables sum of the "diff" acc prices 
##########
library(dplyr)
library(ggplot2)
# library(DataComputing)
setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/gen_premium")
# setwd("F:/Dropbox/urap_programming/yi/gen_premium")
filename_list="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
#filename_list="Feb062016_750D_CombineList.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

setwd("./input")
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
Camera$bundle_index=as.numeric(Camera$bundle_index)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

###perc_premium reference denominator
##fixme play around w other benchmarks for perc_prem
###For fist attempt, use bundle 0 price as the benchmark
Benchmark = Camera%>%
  group_by(item_id)%>%
  summarise(benchmark=first(bundle_price))

Price<-read.csv(filename_dict,stringsAsFactors = FALSE)%>%select(index_final,Avg..Price.Esitimate)

Camera=Camera%>%filter(bundle_index!=0)
Camera_ind=Camera%>%select(item_id,bundle_index,dict_index)

Camera_ind[,4:30]=0
colnames(Camera_ind)[12]="ind_item_in_bundle2"
colnames(Camera_ind)[13]="ind_item_in_bundle3"
colnames(Camera_ind)[14]="ind_item_in_bundle4"
colnames(Camera_ind)[15]="ind_item_in_bundle5"
colnames(Camera_ind)[16]="ind_item_in_bundle6"
colnames(Camera_ind)[17]="ind_item_in_bundle7"
colnames(Camera_ind)[18]="ind_item_in_bundle8"
colnames(Camera_ind)[19]="ind_item_in_bundle9"
colnames(Camera_ind)[20]="ind_item_in_bundle10"

colnames(Camera_ind)[22]="price_item_in_bundle2"
colnames(Camera_ind)[23]="price_item_in_bundle3"
colnames(Camera_ind)[24]="price_item_in_bundle4"
colnames(Camera_ind)[25]="price_item_in_bundle5"
colnames(Camera_ind)[26]="price_item_in_bundle6"
colnames(Camera_ind)[27]="price_item_in_bundle7"
colnames(Camera_ind)[28]="price_item_in_bundle8"
colnames(Camera_ind)[29]="price_item_in_bundle9"
colnames(Camera_ind)[30]="price_item_in_bundle10"


i=1
while(i<length(Camera_ind$item_id))
{
  if(Camera_ind$bundle_index[i]==1)
  {
    j=i+1
    while(Camera_ind$item_id[j]==Camera_ind$item_id[i]&j<length(Camera_ind$item_id))
    {
      if(Camera_ind$dict_index[j]==Camera_ind$dict_index[i])
      {
        Camera_ind[i,(10+Camera_ind$bundle_index[j])]=1
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

Price$Avg..Price.Esitimate<-as.numeric(Price$Avg..Price.Esitimate)
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_index"="index_final"))

CameraWPrice_ind<-Camera_ind%>%left_join(Price,by=c("dict_index"="index_final"))
CameraWPrice_ind[,12:20]=(1-CameraWPrice_ind[,12:20])
CameraWPrice_ind[,22:30]=CameraWPrice_ind[,12:20]*CameraWPrice_ind$Avg..Price.Esitimate

Total_n_of_a<-CameraWPrice_ind%>%
  filter(bundle_index==1)%>%
  group_by(item_id)%>%
  summarise("2"=sum(ind_item_in_bundle2),
            "3"=sum(ind_item_in_bundle3),
            "4"=sum(ind_item_in_bundle4),
            "5"=sum(ind_item_in_bundle5),
            "6"=sum(ind_item_in_bundle6),
            "7"=sum(ind_item_in_bundle7),
            "8"=sum(ind_item_in_bundle8),
            "9"=sum(ind_item_in_bundle9),
            "10"=sum(ind_item_in_bundle10))
Total_n_of_a_narrow=Total_n_of_a%>%
  gather(key=bundle_index,value=total_n_of_a,2,3,4,5,6,7,8,9,10)
Total_n_of_a_narrow$bundle_index=as.numeric(Total_n_of_a_narrow$bundle_index)

Total_price_of_a<-CameraWPrice_ind%>%
  filter(bundle_index==1)%>%
  group_by(item_id)%>%
  summarise("2"=sum(price_item_in_bundle2),
            "3"=sum(price_item_in_bundle3),
            "4"=sum(price_item_in_bundle4),
            "5"=sum(price_item_in_bundle5),
            "6"=sum(price_item_in_bundle6),
            "7"=sum(price_item_in_bundle7),
            "8"=sum(price_item_in_bundle8),
            "9"=sum(price_item_in_bundle9),
            "10"=sum(price_item_in_bundle10))
Total_price_of_a_narrow=Total_price_of_a%>%
  gather(key=bundle_index,value=total_price_of_a,2,3,4,5,6,7,8,9,10)
Total_price_of_a_narrow$bundle_index=as.numeric(Total_price_of_a_narrow$bundle_index)

Total_info_of_a=Total_n_of_a_narrow%>%
  left_join(Total_price_of_a_narrow,by=c("item_id"="item_id","bundle_index"="bundle_index"))

result<-CameraWPrice%>%
  group_by(item_id,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            total_nofw=total-sum(as.vector(sapply('FW',grepl,dict_index))),
            nongift=sum(Accessory_title!=""&gift_or_not!=1),
            nongift_nofw=sum(Accessory_title!=""&gift_or_not!=1&!(as.vector(sapply('FW',grepl,dict_index)))),
            branded=sum(Accessory_title!=""&is.na(brand)==FALSE),
            branded_nofw=sum(Accessory_title!=""&is.na(brand)==FALSE&!(as.vector(sapply('FW',grepl,dict_index)))),
            num_of_yuanzhuang=sum(as.vector(sapply('原装',grepl,Accessory_title))),
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
         ind_firstbundle_noacc=first(total)==0,
         premrel1=premium-first(premium)
         #fixme run this line
         #,perc_premrel1 = premrel1/benchmark
         )%>%
  left_join(Benchmark,by=c("item_id"="item_id"))%>%
  left_join(Total_info_of_a,by=c("item_id"="item_id","bundle_index"="bundle_index"))%>%
  filter(bundle_index!=1)%>%
  mutate(premium_minus_a=premrel1-total_price_of_a,
         premium_minus_a_perc=premium_minus_a/benchmark)

deltan<-read.csv("new_diff_1_2.csv",stringsAsFactors = FALSE)
colnames(deltan)[colnames(deltan)=="ItemID.LIsting_id."] <- "item_id"
deltan$diff=as.numeric(deltan$diff)
result<-result%>%
  left_join(deltan,by=c("item_id"="item_id","bundle_index"="bundle_index"))%>%
  mutate(diff_minus_a=diff-total_n_of_a)

setwd("../output")
write.csv(result,file = "numofacc_and_premium_700D_prem_minus_a_0225.csv")

setwd("./graph")

png(file="histogram of price of bundle 0.png",bg="transparent")
hist(result$benchmark,main="histogram of price of bundle 0",xlab="price of bundle 0")
dev.off()

png(file="histogram of delta n minus a.png",bg="transparent")
hist(result$diff_minus_a,main="histogram of delta n minus a",xlab="delta n",breaks=20)
dev.off()

png(file="histogram of bundle index.png",bg="transparent")
hist(result$bundle_index,main="histogram of bundle index",xlab="bundle index",breaks=8)
dev.off()

png(file="histogram of prem rel 1 minus a(2 col).png",bg="transparent")
hist(result$premium_minus_a,breaks=1,main="2 column histogram of prem rel 1 minus a",xlab="prem rel 1 minus a")
dev.off()

png(file="histogram of prem rel 1 minus a.png",bg="transparent")
hist(result$premium_minus_a,main="histogram of prem rel 1 minus a",xlab="prem rel 1 minus a")
dev.off()

png(file="histogram of prem rel 1 minus a perc.png",bg="transparent")
hist(result$premium_minus_a_perc,main="histogram of percentage of prem rel 1 minus a",xlab="percentage of prem rel 1 minus a")
dev.off()

png(file="boxplot- delta_n_minus_a~bundle_index.png",bg="transparent")
boxplot(result$bundle_index~result$diff_minus_a,main="Delta n minus avs Bundle Index",xlab="delta_n_minus_a",ylab="bundle_index")
dev.off()

png(file="boxplot- delta_n_minus_a ~premrel1_minus_a.png",bg="transparent")
boxplot(result$premium_minus_a ~result$diff_minus_a,xlab="delta_n_minus_a ",ylab="premrel1_minus_a ",main="Premium rel B1_minus_a vs. delta n_minus_a ")
dev.off()

png(file="boxplot- bundle_index~premium_minus_a.png",bg="transparent")
boxplot(result$premium_minus_a~result$bundle_index,xlab="bundle_index",ylab="premium_minus_a",main="premium_minus_a vs. bundle_index")
dev.off()

model1=lm(result$premium_minus_a ~result$diff_minus_a + factor(result$item_id) -1 )
summary(model1)

model2=lm(result$premium_minus_a ~result$bundle_index+ factor(result$item_id) -1 )
summary(model2)
