library(dplyr)
library(RPostgreSQL)
library(ggplot2)
#-------------------

setwd("/Users/idad/Documents/RStudio")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", user= "ghilman", password="ghilmanpass", dbname="ebdb", port= 5000)
fileName <-paste(getwd(),"/sql/raw data.sql",sep="")
query = readChar(fileName, file.info(fileName)$size)
dat = dbGetQuery(con,query)

datdat <- dat %>% filter(ordernr == 1) %>% mutate(days = ifelse(completed_at - sign_up >30,30,completed_at - sign_up)) %>%
  group_by(days,client_version,client_type) %>% summarise(totuser = length(user_id)) %>% ungroup() %>% 
  group_by(client_version,client_type) %>% mutate(usrprop = totuser/sum(totuser))

ggplot(datdat,aes(x = as.factor(days),y = usrprop)) + geom_bar(stat = 'identity', position = 'dodge',aes(fill = client_version)) + facet_grid(client_type ~ .) +
  labs(x = 'days', title = 'First Order Day Convertion') + theme_light()
