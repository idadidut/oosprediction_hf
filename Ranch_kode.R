library(RPostgreSQL)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(data.table)
library(openxlsx)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", user= "ghilman", password="ghilmanpass", dbname="ebdb", port= 5000)
fileName <- "~/Documents/Rstudio/sql/oos_purchased_only_ranch.sql"
query = readChar(fileName, file.info(fileName)$size)
dat = dbGetQuery(con,query)

dranch <- filter(dat, deliver_date >= as.Date('2016-03-07'), deliver_date <= as.Date('2016-06-12'))
dranch <- dranch %>% group_by(store, cw = isoweek(deliver_date)) %>% summarise(total_order = sum(tot_order), total_demand = sum(total_demand), total_sold = sum(units_sold), total_oos = sum(oos), total_sales = sum(total_sales), total_lost = sum(total_lost)) %>% 
          mutate(percent_total_lost = total_lost/(total_lost + total_sales))

ranchall <- dat %>% group_by(store) %>% summarise(dis_sku = n_distinct(sku),total_order = sum(tot_order),oos_sku = uniqueN(sku[oos > 0]), total_demand = sum(total_demand), total_sold = sum(units_sold), total_oos = sum(oos), total_sales = sum(total_sales), total_lost = sum(total_lost)) %>% 
  mutate(percent_total_lost = total_lost/(total_lost + total_sales), total_oos_sku = oos_sku/dis_sku)

vardat <- dranch %>% group_by(store) %>% summarise(variance = var(percent_total_lost))

dranch_x <- dcast(dranch, store~cw, value.var = 'percent_total_lost')
list_of_data = list("ranch_alltime"= ranchall, 'ranch_per_week' = dranch, 'ranch_variant' = vardat, 'ranch_lost_per_week' = dranch_x)
write.xlsx(list_of_data, file= '~/Documents/Rstudio')
ggplot(data=dranch, aes(cw, percent_total_lost, color=as.factor(store)))+geom_line() +
    scale_x_continuous(breaks = round(seq(min(dranch$cw), max(dranch$cw), by = 1),1)) + 
    scale_y_continuous(breaks = seq(-0.5,max(dranch$percent_total_lost), by=0.05))

#nyoba git