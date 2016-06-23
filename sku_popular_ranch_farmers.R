library(dplyr)
library(RPostgreSQL)

#-----------------------------------------
setwd("/Users/idad/Documents/RStudio")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", user= "ghilman", password="ghilmanpass", dbname="ebdb", port= 5000)
fileName <-paste(getwd(),"/sql/oos_purchased_only_ranch.sql",sep="")
query = readChar(fileName, file.info(fileName)$size)
dat = dbGetQuery(con,query)

skupop <- alldat %>% group_by(sku,supermarket, month = month(deliver_date)) %>% filter(sum(units_sold) > 0) %>% 
  summarise(units_sold = sum(units_sold), total_sales = sum(total_sales), varq = var(Qty.Stock)) %>% 
  mutate(price = total_sales/units_sold) %>% ungroup() %>% arrange(total_sales,varq)

skupopdata <- melt(skupop, id.vars = 1:3)
skupopdata <- dcast(skupopdata, sku + supermarket ~ month + variable)

#-- nambah komen
#-- tambah satu lagi