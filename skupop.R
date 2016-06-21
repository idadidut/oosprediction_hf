library(dplyr)
result <- distinct(select(datasup, product, supermarket))
result$totorder <- aggregate(totorder ~ product + supermarket, data=datasup, FUN=sum)[3]$totorder
result$totdelivered <- aggregate(totdelivered ~ product + supermarket, data=datasup, FUN=sum)[3]$totdelivered
result$totprice <- aggregate(totprice ~ product + supermarket, data=datasup, FUN=sum)[3]$totprice
result <- arrange(result, desc(totprice))
tbl_df(result)
