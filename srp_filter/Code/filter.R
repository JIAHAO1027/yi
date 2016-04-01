#Install xlsx package first

library(dplyr)
library(xlsx)

setwd("F:/Dropbox/urap_programming/yi/srp_filter/CC")
#setwd("C:/Users/xin_chen/Dropbox/urap_programming/yi/srp_filter/CC")

#Only applicable for "CC" series (SD cards)
#Create a new folder inside "../CC" named "filtered" for outputs

temp = list.files(pattern="*.xlsx")
for (i in 1:length(temp))
{  
  df=read.xlsx(temp[i], sheetIndex = 1,encoding = "UTF-8")
  if (nrow(df)!=0)
  {
    df$ind_filter=""
    df$ind_brand=""
    df$ind_memory=""
    df$ind_speed=""
    df1 <- df%>%
      filter(num_srp == 1)%>%
      arrange(as.numeric(as.character(item_price)))%>%
      select(num_srp,item_title,item_url,ind_filter,ind_brand,ind_memory,ind_speed,item_price,everything())
 
  write.xlsx(df1,file=paste("filtered/",temp[i],sep=""),row.names = FALSE,showNA=FALSE)
  }
}