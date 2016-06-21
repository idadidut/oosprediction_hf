library(dplyr)
library(RPostgreSQL)
library(reshape2)
library(ggplot2)
library(lubridate)
library(zoo)

##-------------------------------excel preprocessing----------------------------
setwd("/Users/idad/Dropbox (HappyFresh)/HappyFresh Team Folder/Contents/backward/ranch")
temp = list.files(pattern = "042016.TXT")
setwd("/Users/idad/Documents/RStudio")

myfiles = sapply(temp, USE.NAMES = T, simplify = F, FUN = function(x){
  read.table(x, sep = ";", fill = NA, quote = "", header = T, colClasses = "character")
})

names(myfiles)<-gsub(".TXT", "", names(myfiles))
for(i in 1:length(myfiles)){
  myfiles[[i]]<- myfiles[[i]][,c(1,2,5,9,10,13)]
  myfiles[[i]]$tgl = as.Date(dmy(substring(names(myfiles[i]), first = nchar(names(myfiles[i]))-7, 
                                           last =nchar(names(myfiles[i])))))
}

finalexcel <- bind_rows(myfiles)
finalexcel$Normal.Selling..VKP0. <- as.numeric(finalexcel$Normal.Selling..VKP0.)
finalexcel$Promo.Selling..VKA0. <- as.numeric(finalexcel$Promo.Selling..VKA0.)
finalexcel$Qty.Stock <- as.numeric(sub(pattern = "\\..*", "",finalexcel$Qty.Stock))


finalexcel <- finalexcel %>% arrange(Product.Code, tgl)
finalexcel$discount_xls <- ifelse(!is.na(finalexcel$Promo.Selling..VKA0.), 
                                  round((finalexcel$Normal.Selling..VKP0.- finalexcel$Promo.Selling..VKA0.)/finalexcel$Promo.Selling..VKA0.,3), NA)

finalexcel$Product.Code<- paste(finalexcel$Product.Code, "-ID", sep = "")
finalexcel <- finalexcel[!duplicated(finalexcel[,-(4:6)]),]
finalexcel$Qty.Stock[finalexcel$Qty.Stock<0] <-0
finalexcel$Store <- as.factor(finalexcel$Store)
##------------------------------database preprocessing--------------------------

filename <- paste(getwd(),"/dbconnect.txt",sep="")
dbinfo <- read.csv(filename, header = T, stringsAsFactors = F, quote = "\\")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host=dbinfo[1,2], user=dbinfo[2,2], password=dbinfo[3,2], dbname=dbinfo[4,2], port=dbinfo[5,2])
fileName <- paste(getwd(),"/sql/oos_purchased_only_ranch.sql",sep="")
query = readChar(fileName, file.info(fileName)$size)
dat = dbGetQuery(con,query)

dat$supermarket <- as.factor(dat$store)
dat$store <- NULL

alldat <- left_join(dat, finalexcel[,-(3:5)], by = c("supermarket" = "Store", "sku"="Product.Code", "deliver_date"="tgl"))
alldat <- alldat[!is.na(alldat$Qty.Stock),]
plot1 <- function(x, title){
  ggplot(x, aes(Qty.Stock))+
    theme_bw(base_size = 20)+stat_ecdf(geom = "step", show.legend = T, aes(colour = supermarket) )+
    labs(title=title, x="Excel Qty", colour = "supermarket")
}
alldatranch <- alldat %>% filter(substr(supermarket,1,1)=='1')
alldatfarmers <- alldat %>% filter(substr(supermarket,1,1)=='2')
#--
alldat <- alldatranch
#--

oossku <- as.data.frame(filter(alldat, oos > 0))
print(plot1(oossku,"ECDF of Excel Stock Quantity to OOS"))
oossku$Qty.Stock <- ifelse(oossku$Qty.Stock > 100, 100, oossku$Qty.Stock)

nonoossku <- as.data.frame(filter(alldat, oos == 0))
print(plot1(nonoossku, "ECDF of Excel Stock Quantity to non-OOS"))
nonoossku$Qty.Stock <- ifelse(nonoossku$Qty.Stock > 100, 100, nonoossku$Qty.Stock)

p<- plot1(oossku,"ECDF of Excel Stock Quantity to OOS")
p2<- plot1(nonoossku, "ECDF of Excel Stock Quantity to non-OOS")
print(p)
print(p2)

funecdf = function(x){
  fx = ecdf(x)
  fx(seq(0,100,1))
}

calecdf = function(x){
 a <- unique(x$supermarket)
  z = list()
  for(i in 1:length(a)){
    b <- x %>% filter(supermarket == a[i])
    z[[i]] <- funecdf(b$Qty.Stock)
  }
  dataecdf <- as.data.frame(z)
  names(dataecdf) <- a
  dataecdf
}

listdatoos <- calecdf(oossku)
listdatnonoos <- calecdf(nonoossku)

datoos <- melt(listdatoos, value.name = "oos_sku", variable.name= "store")
datoos$qty <- 0:100
datoos<- datoos[,c(1,3,2)]
datnonoos <- melt(listdatnonoos, value.name = "nonoos_sku", variable.name= "store")
datnonoos$qty <- 0:100

datcovered <- left_join(datoos, datnonoos)
datcovered$diff <- abs(datcovered$oos_sku-datcovered$nonoos_sku)

totorderDist <- oossku %>% group_by(supermarket, Qty.Stock) %>% summarise(tot_order = sum(tot_order)) %>% 
  mutate(oosorder = cumsum(tot_order), oosorderprop = cumsum(tot_order)/sum(tot_order))
totorderNonDist <- nonoossku %>% group_by(supermarket, Qty.Stock) %>% summarise(tot_orderNon = sum(tot_order)) %>% 
  mutate(nonoosorder = cumsum(tot_orderNon), nonoosorderprop = cumsum(tot_orderNon)/sum(tot_orderNon))
totorderMon <- oossku %>% group_by(supermarket, Qty.Stock) %>% summarise(tot_money_oos = sum(total_lost)) %>% 
  mutate(tot_money_lost = cumsum(tot_money_oos), lost_money_prop = cumsum(tot_money_oos)/sum(tot_money_oos))
totorderMonSales <- alldat %>% group_by(supermarket, Qty.Stock = ifelse(Qty.Stock>100,100,Qty.Stock)) %>% summarise(tot_money_nonoos = sum(total_sales)) %>% 
  mutate(tot_money = cumsum(tot_money_nonoos), money_prop = cumsum(tot_money_nonoos)/sum(tot_money_nonoos))


datcovered <- left_join(datcovered, totorderDist, by = c("store" = "supermarket", "qty" = "Qty.Stock"))
datcovered <- left_join(datcovered, totorderNonDist, by = c("store" = "supermarket", "qty" = "Qty.Stock"))
datcovered <- left_join(datcovered, totorderMon, by = c("store" = "supermarket", "qty" = "Qty.Stock"))
datcovered <- left_join(datcovered, totorderMonSales, by = c("store" = "supermarket", "qty" = "Qty.Stock"))

datcovered$tot_order[is.na(datcovered$tot_order)] <- 0
datcovered$tot_orderNon[is.na(datcovered$tot_orderNon)] <- 0
datcovered$tot_money_oos[is.na(datcovered$tot_money_oos)] <- 0
datcovered$tot_money_nonoos[is.na(datcovered$tot_money_nonoos)] <- 0

datcovered <- datcovered %>% group_by(store) %>% mutate(oosorder1 = na.locf(oosorder)) %>% select(-oosorder)
datcovered <- datcovered %>% group_by(store) %>% mutate(oosorderprop1 = na.locf(oosorderprop)) %>% select(-oosorderprop)
datcovered <- datcovered %>% group_by(store) %>% mutate(nonoosorder1 = na.locf(nonoosorder)) %>% select(-nonoosorder)
datcovered <- datcovered %>% group_by(store) %>% mutate(nonoosorderprop1 = na.locf(nonoosorderprop)) %>% select(-nonoosorderprop)
datcovered <- datcovered %>% group_by(store) %>% mutate(tot_money_lost1 = na.locf(tot_money_lost)) %>% select(-tot_money_lost)
datcovered <- datcovered %>% group_by(store) %>% mutate(lost_money_prop1 = na.locf(lost_money_prop)) %>% select(-lost_money_prop)
datcovered <- datcovered %>% group_by(store) %>% mutate(tot_money1 = na.locf(tot_money)) %>% select(-tot_money)
datcovered <- datcovered %>% group_by(store) %>% mutate(money_prop1 = na.locf(money_prop)) %>% select(-money_prop)

datcovered <- datcovered[c(1,2,3,4,5,6,10,11,7,12,13,8,14,15,9,16,17)]

datcovered$avg_TP = (datcovered$oos_sku+ datcovered$oosorderprop1+ datcovered$lost_money_prop1)/3
datcovered$avg_FP = (datcovered$nonoos_sku+ datcovered$nonoosorderprop1+ datcovered$money_prop1)/3
datcovered$opt <- sqrt(datcovered$avg_FP^2 + (1-datcovered$avg_TP)^2)

optimumroc = datcovered %>% group_by(store) %>% 
  summarise(qty = qty[which.min(opt)], xminopt = avg_FP[which.min(opt)], yminopt = avg_TP[which.min(opt)])

ggplot(datcovered, aes(avg_FP, avg_TP))+geom_line(colour = "red")+geom_abline(linetype = "dashed")+
  geom_point(data=optimumroc, aes(xminopt, yminopt), color = "blue")+
  facet_wrap(~store)+theme_bw()+geom_text(data=optimumroc, aes(x=xminopt-0.05, y = yminopt+.1, label = qty))

