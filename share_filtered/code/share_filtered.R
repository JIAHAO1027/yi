
install.packages("dplyr")
install.packages("plyr")
library(dplyr) 
library(stringi)
library(DataComputing)
library(dplyr)


setwd( "/Users/suyanglu/Dropbox/urap_programming/suyang/share_filtered/input")
filename="transec_700d_cleaned_20160215.csv"


#read the excel file
df_input = read.csv(filename,head=TRUE)
df_input<-df_input%>%select(item_id, amount, color_category_ind_18_55, bundle_index)


#group the cells
grouped_sales <- df_input %>% group_by(item_id)%>%summarize(item_sales=sum(amount))

grouped_bundle <- df_input%>% group_by(item_id, bundle_index)%>%summarize(bundle_sales=sum(amount))



grouped_f_sales <- df_input%>%group_by(item_id, color_category_ind_18_55)%>%filter(color_category_ind_18_55==1)%>%summarize(filterted_item_sales=sum(amount))

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

s1
s1[is.na(s1)]<-"."
#output the file
setwd( "/Users/suyanglu/Dropbox/urap_programming/suyang/share_filtered/output")


write.csv(s1,file="share_filtered_700d_0229.csv")
n#s1.to_csv(filePath_new, encoding = 'utf-8', n)

