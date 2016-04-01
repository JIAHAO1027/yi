library(dplyr)
#setwd("C:/Users/xin_chen/Dropbox/STIMULATING_Research_ Ideas and Inspirations_/Bundle_Pricing_joint_Zach/iPad Air 2 data/Bundles_lstg")
setwd("F:/Dropbox/urap_programming/yi/gen_premium")

setwd("./input")
#FIXME

#py equivalent
# def access_srp_process(infilepath, outfilepath):
#def Rmerge_lstg_w_price(infilepath, outfilepath)


#filename_lst="Juexiao_sample_700D.csv"#"700d_2016.csv"
#filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/Combinelist1_Jan192016_jue+sa+eve+xin.csv"
#filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/700D_Eve&Yu list- Editted.csv"
# filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/Juexiao_Wang_31-59_2016Jan19.csv"
# filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/Juexiao_Wang_31-59_2016Jan20_addingrow260 - Sheet1.csv"
#filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/Jan262016_Combinelist1checked.csv"
#filename_lst="C:/Users/xin_chen/Dropbox/URAP/working/Juexiao_Wang_31-59_2016Jan20_addingrow260 - Sheet1.csv"
filename_srp=""
#filename_dict ="C:/Users/xin_chen/Dropbox/STIMULATING_Research_ Ideas and Inspirations_/Bundle_Pricing_joint_Zach/iPad Air 2 data/Bundles_lstg/dict_700d_20160126.csv"
#filename_dict ="dict_700d_750d_Jan102016.csv"

filename_lst="lst_700D_Yi_Feb142016_giftindex_corrected.csv"
filename_dict="dict_700d_750d_Feb082016.csv"

lst = read.csv(filename_lst)
dict =read.csv(filename_dict)



##lst
lst$gift_or_not <- as.numeric(lst$gift_or_not)

#only keep bundle price
#compute the price of bundle_i - bundle_0
df_bundle_price <- lst %>%
  select(Author, ItemID.LIsting_id., bundle_index, bundle_price, dict_index) %>%
    group_by(Author, ItemID.LIsting_id., bundle_index) %>%
      slice(1:1) %>%
        group_by(Author, ItemID.LIsting_id.) %>%
          mutate(bundle_add_on = bundle_price - first(bundle_price))

##verify all bundle_add_ons are 0 for bundle 0
##output should be a zero vector
#df_bundle_price_b0 <- subset(df_bundle_price, df_bundle_price$bundle_index == 0)
#df_bundle_price_b0$bundle_add_on


##examine bundle_add_on
##side output
summary(df_bundle_price$bundle_add_on)
max_add_on_bundle <- subset(df_bundle_price, df_bundle_price$bundle_add_on == max(df_bundle_price$bundle_add_on))



##dict
##keeping top row per index

df_dict <- dict %>%
  group_by(index_final) %>%
    slice(1:1) #%>%



##check dim
##dim(df_dict)

#alalalalalal
df_dict$index_final<-as.character(df_dict$index_final)
lst$dict_index<-as.character(lst$dict_index)


#Q: warnings
#df_lst_join_dict <- left_join(lst, dict, by = c("dict_index"="index_final"))


df_lst_join_dict <- merge(lst, df_dict, by.x = "dict_index", by.y = "index_final", all.x = T)

#This line is dangerous! (Trying to directly convert factor to numeric)
df_lst_join_dict$Avg..Price.Esitimate <- as.numeric(df_lst_join_dict$Avg..Price.Esitimate)

##
df_price <- df_lst_join_dict %>%
  select(Author, ItemID.LIsting_id., bundle_index, bundle_price, dict_index, gift_or_not, Avg..Price.Esitimate) %>%
    
    group_by(Author, ItemID.LIsting_id., bundle_index) %>%
      summarise(p_add_on_est = sum(Avg..Price.Esitimate * (1 - gift_or_not), na.rm = T)
                ,num_of_gifts = sum(gift_or_not, na.rm = T)
                ,num_of_access_b0raw = n()) #%>%

                
df_price$ind_bundle0gd <- ifelse(df_price$num_of_access_b0raw ==1, 1, 0)
df_price$num_of_access <- ifelse(df_price$bundle_index == 0 & df_price$num_of_access_b0raw ==1, 0, df_price$num_of_access)
df_price$num_of_access[df_price$bundle_index == 0]

##Verify            
#dim(df_price) #$p_add_on_est 
#dim(df_bundle_price)#$bundle_add_on)

#df_price_b0 <- subset(df_price, df_price$bundle_index == 0)
#df_price_b0$p_add_on_est



df_premium <- merge(df_bundle_price,df_price)  
df_premium$premium = df_premium$bundle_add_on - df_premium$p_add_on_est  

#Verify output is Null
#Fixme e.g. 45219159638
# can drop all such listings by 
item_id_b0fix = subset(df_premium$ItemID.LIsting_id., df_premium$bundle_index == 0 & df_premium$premium != 0)
df_premium_b0fix = subset(df_premium, df_premium$ItemID.LIsting_id. %in% item_id_b0fix)
df_premium_b0good = subset(df_premium, !(df_premium$ItemID.LIsting_id. %in% df_premium_b0fix$ItemID.LIsting_id.))


##premium = df_bundle_price$bundle_add_on - df_price$p_add_on_est
summary(df_premium_b0good$premium)
#hist(df_premium_b0good$premium)

##Fixme automate w function
###for i = 1 to 8
premium_b1 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 1)
summary(premium_b1)

premium_b2 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 2)
summary(premium_b2)

premium_b3 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 3)
summary(premium_b3)

premium_b4 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 4)
summary(premium_b4)

premium_b5 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 5)
summary(premium_b5)

premium_b6 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 6)
summary(premium_b6)

premium_b7 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 7)
summary(premium_b7)

premium_b8 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 8)
summary(premium_b8)

#premium_b9 = subset(df_premium_b0good$premium, df_premium_b0good$bundle_index == 9)
#summary(premium_b9)


lm_add_on_acess = lm(df_premium_b0good$bundle_add_on ~ df_premium_b0good$num_of_access)
summary(lm_add_on_acess)

lm_premium_acess = lm(df_premium_b0good$premium ~ df_premium_b0good$num_of_access)
summary(lm_premium_acess)

lm_add_on_index = lm(df_premium_b0good$bundle_add_on ~ df_premium_b0good$bundle_index)
summary(lm_add_on_index)

lm1 = lm(df_premium_b0good$premium ~ df_premium_b0good$bundle_index)
summary(lm1)

lm2 = lm(df_premium_b0good$premium ~ df_premium_b0good$bundle_add_on)
summary(lm2)

lm22 = lm(df_premium_b0good$premium ~ df_premium_b0good$bundle_index +df_premium_b0good$bundle_add_on)
summary(lm22)

lm22fe = lm(df_premium_b0good$premium ~ df_premium_b0good$bundle_index + 
            df_premium_b0good$bundle_add_on + factor(df_premium_b0good$item_id)-1)
summary(lm22fe)



names(df_premium_b0good)[names(df_premium_b0good)== "ItemID.LIsting_id."] = "item_id"

setwd("../output")
write.csv(df_premium_b0good, file = "result4.csv")
#"C:/Users/xin_chen/Dropbox/STIMULATING_Research_ Ideas and Inspirations_/Bundle_Pricing_joint_Zach/iPad Air 2 data/tmp/bundle_premium.csv")






