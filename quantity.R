


b = alldat %>% filter(oos == 0, Qty.Stock >= units_sold)
corqty <- b %>% group_by(supermarket) %>% 
  summarise(sku_nonunique = n(), sku_unique = n_distinct(sku), sales = sum(total_sales))


indexb = which(alldat$oos == 0 & alldat$Qty.Stock >= alldat$units_sold)
a = alldat[-indexb,]
missqty <- a %>% group_by(supermarket) %>%
  summarise(sku_nonunique = n(), sku_unique = n_distinct(sku), 
            sales = sum(total_sales))

names(corqty)[-1] <- paste(names(corqty[-1]), "_cor", sep = "")
names(missqty)[-1] <- paste(names(missqty[-1]), "_miss", sep = "")
qtyqty <- left_join(corqty, missqty)
write.csv(qtyqty, 'hasilmei.csv' , row.names = F)
