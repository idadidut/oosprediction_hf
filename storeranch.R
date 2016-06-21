library(dplyr)
datasup <- read.csv(file='~/Documents/Rstudio/filename.csv', header = TRUE)
result <- distinct(select(datasup, store))
result$items <- aggregate(tot_order ~ store, data=datasup, FUN=sum)[2]$tot_order
result$sku <- aggregate(sku ~ store, data=datasup, FUN=length)[2]$sku
result$totdelivered <- aggregate(totdelivered ~ store, data=datasup, FUN=sum)[2]$totdelivered
result$totprice <- aggregate(totprice ~ store, data=datasup, FUN=sum)[2]$totprice
result <- arrange(result, desc(totprice))
result
write.csv(result, File='~/fname.csv', row.names = FALSE)