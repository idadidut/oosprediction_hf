library(openxlsx)
library(dplyr)
library(RPostgreSQL)
library(reshape2)
library(ggplot2)
library(lubridate)

##-------------------------------excel preprocessing----------------------------
setwd("D:\\HappyFresh\\projects\\Dropbox\\14. OOS Mining\\lottemart")
temp = list.files(pattern=".xlsx")
myfiles = sapply(temp, USE.NAMES = T, simplify = F, FUN = function(x){
  read.xlsx(x, cols = c(1,2,4,6,7,10))
})

names(myfiles)<-gsub(".xlsx", "", names(myfiles))
# nambahin variable tanggal ke data
for(i in 1:length(myfiles)){
  myfiles[[i]]$Store = as.character(myfiles[[i]]$Store)
  myfiles[[i]]$tgl = as.Date(ymd(substring(names(myfiles[i]), first = nchar(names(myfiles[i]))-7, 
                      last =nchar(names(myfiles[i])))))
}

# merge myfile jadi 1 dataframe
finalexcel <- bind_rows(myfiles)

finalexcel <- finalexcel %>% arrange(Barcode, tgl)

# nambahin variable diskon
finalexcel$discount_xls <- round((finalexcel$`Normal.Selling.(VKP0)`- finalexcel$`Promo.Selling.(VKA0)`)/finalexcel$`Normal.Selling.(VKP0)`,3)

finalexcel$Store <- as.factor(finalexcel$Store)


finalexcel$Barcode<- paste(finalexcel$Barcode, "-ID", sep = "")
finalexcel <- finalexcel[!duplicated(finalexcel[,-(4:6)]),]
finalexcel$Qty.Stock[finalexcel$Qty.Stock<0] <-0
finalexcel$Store <- as.factor(finalexcel$Store)
names(finalexcel)[1] <- "supermarket"
##------------------------------database preprocessing--------------------------
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", user= "ghilman", password="ghilmanpass", dbname="ebdb", port= 5000)
fileName <-"D:\\HappyFresh\\projects\\Dropbox\\14. OOS Mining/oos_purchased_only.sql"
query = readChar(fileName, file.info(fileName)$size)
dat = dbGetQuery(con,query)
dat$supermarket <- as.factor(dat$supermarket)

alldat <- left_join(dat, finalexcel[,-(3:5)], by = c("supermarket" = "supermarket", "sku"="Barcode", "tgl"="tgl"))

#a=alldat %>% group_by(sku) %>% mutate(totrow=n())

alldat <- filter(alldat, !is.na(Qty.Stock))
alldat <- alldat %>% group_by(isoos, supermarket) %>% mutate(spl = ifelse(row_number()<=n()*.75, "train", "test")) %>% ungroup()

train <- alldat %>% filter(spl=="train") %>% select(-spl)
test <- alldat %>% filter(spl=="test") %>% select(-spl)

plot1 <- function(x, title){
  ggplot(x, aes(Qty.Stock))+
    theme_bw(base_size = 20)+stat_ecdf(geom = "step", show.legend = T, aes(colour = supermarket) )+
    labs(title=title, x="Excel Qty", colour = "supermarket")
}

ecdfdat<- function(x, status){
  a = ggplot_build(x)$data[[1]][,c("x","y","group")]
  a = a %>% group_by(group) %>% arrange(y) %>% ungroup() %>% round(2) %>% data.frame()
  a$status <- status
  a
}

traintest <- function(x, supermarket){
  filtered <- as.data.frame(filter(x, isoos == TRUE))
  print(plot1(filtered,"ECDF of Excel Stock Quantity to OOS"))
  filtered$Qty.Stock <- ifelse(filtered$Qty.Stock > 100, 100, filtered$Qty.Stock)
  filtered2 <- as.data.frame(filter(x, isoos == FALSE, !is.na(totdelivered)))
  print(plot1(filtered2, "ECDF of Excel Stock Quantity to non-OOS"))
  filtered2$Qty.Stock <- ifelse(filtered2$Qty.Stock > 100, 100, filtered2$Qty.Stock)
  p<- plot1(filtered,"ECDF of Excel Stock Quantity to OOS")
  p2<- plot1(filtered2, "ECDF of Excel Stock Quantity to non-OOS")
  print(p)
  print(p2)
  
  g <- ggplot_build(p)
  colLabel = data.frame(colours = unique(g$data[[1]]["colour"]), supermarket = levels(g$plot$data[, g$plot$labels$colour]),
                        group = unique(g$data[[1]]$group))
  filtered_ecdf <- ecdfdat(p, "oos")
  filtered2_ecdf<- ecdfdat(p2, "non-oos")
  
  # order level
  totorderDist <- filtered %>% group_by(supermarket, Qty.Stock) %>% summarise(totorder = sum(totorder)) %>% 
    mutate(ooscumsum = cumsum(totorder), oosprop = cumsum(totorder)/sum(totorder))
  totorderNonDist <- filtered2 %>% group_by(supermarket, Qty.Stock) %>% summarise(totorderNon = sum(totorder)) %>% 
    mutate(nonooscumsum = cumsum(totorderNon), nonoosprop = cumsum(totorderNon)/sum(totorderNon))
  
  final <- rbind(filtered_ecdf, filtered2_ecdf)
  rawfinal<-left_join(x = filtered_ecdf, y = filtered2_ecdf, by = c("x" = "x", "group" = "group"))
  rawfinal <- left_join(rawfinal, colLabel[,-1])
  
  rawfinal$dif <- abs(rawfinal$y.x - rawfinal$y.y)
  rawfinal <- filter(rawfinal, x!=Inf, x!=-Inf)
  rawfinal <- left_join(rawfinal, totorderDist, by = c("supermarket" = "supermarket", "x" = "Qty.Stock"))
  rawfinal <- left_join(rawfinal, totorderNonDist, by = c("supermarket" = "supermarket", "x" = "Qty.Stock"))
  
  a = rawfinal %>% group_by(group) %>% summarise(x= x[which.max(dif)], y1=y.x[which.max(dif)])
  
  print(ggplot(final, aes(x,y))+geom_line(aes(color=status), size=1)+
    geom_vline(data = a, aes(xintercept = x), linetype = "dashed")+
    geom_text(data = a, aes(x=x+5, y = .1, label = x))+
    facet_wrap(~group)+theme_bw(base_size = 20)+
    labs(title=paste("ECDF of ", supermarket,"'s Stock Qty", sep = ""), y="% of cumulative sku covered", x="Qty"))
  
  rawfinal<- rawfinal[, c(7,1,2,5,8,10,11,13,14)]
  write.table(rawfinal, "clipboard-16384", sep="\t", row.names = F)
  rawfinal
}

trainmod= traintest(train, "Lotte")
testmod = traintest(test, "Lotte")

tampung = left_join(trainmod, testmod, by = c("supermarket"="supermarket", "x"="x"))
MAE= tampung %>% group_by(supermarket) %>% summarise(mae = mean(abs(y.x.x - y.x.y), na.rm = T))
write.table(tampung, "clipboard-16384", sep="\t", row.names = F)
write.table(MAE, "clipboard-16384", sep="\t", row.names = F)

##---------------------------------Supplier Level-------------------------------

p3<- ggplot(filtered, aes(Qty.Stock))+theme_bw(base_size = 20)+stat_ecdf(geom = "step")+
  labs(title="ECDF of Excel Stock Quantity to OOS - Supplier Level", x="Excel Qty")
p3
filteredSupplier_ecdf <- ggplot_build(p3)$data[[1]][,c("x","y")]
filteredSupplier_ecdf <- filteredSupplier_ecdf %>% arrange(y) %>% round(3) %>% data.frame()

p4<- ggplot(filtered2, aes(Qty.Stock))+theme_bw(base_size = 20)+stat_ecdf(geom = "step")+
  labs(title="ECDF of Excel Stock Quantity to non-OOS - Supplier Level", x="Excel Qty")
p4
filtered2Supplier_ecdf <- ggplot_build(p4)$data[[1]][,c("x","y")]
filtered2Supplier_ecdf <- filtered2Supplier_ecdf %>% arrange(y) %>% round(3) %>% data.frame()

b = left_join(x = filteredSupplier_ecdf, y = filtered2Supplier_ecdf, by = c("x" = "x"))
b = arrange(b, x)
b$dif = abs(b$y.x-b$y.y)


ggplot(b, aes(x,y.x))+geom_line(colour= "red", size=1)+ geom_line(aes(x,y.y), size=1)+
  geom_point(data = b[which.max(b$dif),], aes(x, dif), colour = "#990000")+
  geom_vline(data = b[which.max(b$dif),], aes(xintercept = x), linetype = "dashed")+
  geom_hline(data = b[which.max(b$dif),], aes(yintercept = dif), linetype = "dashed")+
  theme_bw(base_size = 20)+scale_colour_hue(h=c(90, 360))+
  labs(title="ECDF of Qty", y="% of cumulative sku covered", x="Qty")
