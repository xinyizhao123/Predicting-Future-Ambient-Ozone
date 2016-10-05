##########################################################################################
# This program is to check, clean and subset data of each parameter by year (2000-2011)
# Programmer: Xinyi Zhao
# Date: 09/21/2015
##########################################################################################

setwd("H:/AQproject/data/data files/level 1 - raw data/main parameters")

################################# 
#           VOC Data            #
################################# 

z1 <- read.table("RD_501_PAMSVOC_2011-0.txt", sep = "|", stringsAsFactors = F, fill = T)
z1$V3 <- as.numeric(z1$V3)
z1 <- z1[-c(1,2, 14:28)]

names(z1) <- c("state", "county", "site", "parameter", "POC", 
               "duration", "unit", "method", "date", "time", "value")

z1$location <- paste(z1$state, z1$county, sep = ', ')
z1$siteid <- paste(z1$state, z1$county, z1$site, sep = '-')
z1 <- data.frame(z1, year=rep(2011, nrow(z1)))

z11 <- z1[z1$parameter == 43102 & z1$duration == 7,]
z11 <- subset(z11, z11$value!="NA")
z111 <- subset(z11, z11$location =="18, 89" | 
                   z11$location =="22, 33" |
                   z11$location =="51, 33" |
                   z11$location =="25, 9" |
                   z11$location =="44, 7" |
                   z11$location =="17, 31" |
                   z11$location =="25, 13" |
                   z11$location =="44, 3" |
                   z11$location =="22, 47" |
                   z11$location =="6, 37" |
                   z11$location =="13, 223" |
                   z11$location =="13, 247" |
                   z11$location =="13, 89" |
                   z11$location =="48, 141" |
                   z11$location =="48, 167" |
                   z11$location =="48, 201" |
                   z11$location =="18, 127" |
                   z11$location =="24, 5" |
                   z11$location =="25, 25" |
                   z11$location =="6, 65" |
                   z11$location =="18, 163" |
                   z11$location =="18, 97" |
                   z11$location =="19, 113" |
                   z11$location =="19, 153" |
                   z11$location =="19, 163" |
                   z11$location =="48, 113" |
                   z11$location =="48, 121" |
                   z11$location =="46, 99")

#head(z111)
#table(z111$unit)
#table(z111$location)
#sum(is.na(z111$value))
#tapply(z111$date, z111$location, table) 

write.csv(z111, "VOC_2011.csv")

#install.packages("xtable")
#library(xtable)
#tb <- xtable(table(z$location))

################################# 
#           NOx Data            #
################################# 

######################################################################################
# double check the daily data to see if it is adjusted daily mean or not

### hourly data (old version)

y1 <- read.table("RD_501_42603_2000-0.txt", sep = "|", stringsAsFactors = F)
y1$V3 <- as.numeric(y1$V3)
y1 <- y1[-c(1,2, 14:28)]

names(y1) <- c("state", "county", "site", "parameter", "POC", 
               "duration", "unit", "method", "date", "time", "value")

y1$location <- paste(y1$state, y1$county, sep = ', ')
y1$siteid <- paste(y1$state, y1$county, y1$site, sep = '-')
y1 <- data.frame(y1, year=rep(2000, nrow(y1)))

y1 <- subset(y1, y1$value!="NA")
y11 <- subset(y1, y1$location =="18, 89" | 
                 y1$location =="22, 33" |
                 y1$location =="51, 33" |
                 y1$location =="25, 9" |
                 y1$location =="44, 7" |
                 y1$location =="17, 31" |
                 y1$location =="25, 13" |
                 y1$location =="44, 3" |
                 y1$location =="22, 47" |
                 y1$location =="6, 37" |
                 y1$location =="13, 223" |
                 y1$location =="13, 247" |
                 y1$location =="13, 89" |
                 y1$location =="48, 141" |
                 y1$location =="48, 167" |
                 y1$location =="48, 201" |
                 y1$location =="18, 127" |
                 y1$location =="24, 5" |
                 y1$location =="25, 25" |
                 y1$location =="6, 65" |
                 y1$location =="18, 163" |
                 y1$location =="18, 97" |
                 y1$location =="19, 113" |
                 y1$location =="19, 153" |
                 y1$location =="19, 163" |
                 y1$location =="48, 113" |
                 y1$location =="48, 121" |
                 y1$location =="46, 99")

y111 <- y11
y111$ave = rep(0,dim(y111)[1])
y111$nobs = rep(0,dim(y111)[1])
tmp.id=unique(y11$siteid)
for (iid in tmp.id){
  tmp.date=unique(y11[which(y11$siteid==iid),"date"])
  tmp=tapply(y11[which(y11$siteid==iid),"value"],y11[which(y11$siteid==iid),"date"],mean)
  tmp.len=tapply(y11[which(y11$siteid==iid),"date"],y11[which(y11$siteid==iid),"date"],length)
  y111$ave[which(y11$siteid==iid)]=rep(tmp,times=tmp.len)
  y111$nobs[which(y11$siteid==iid)]=rep(tmp.len,times=tmp.len)
}

y111 <- subset(y111, y111$nobs>=23)
y111 <- subset(y111, y111$time=="00:00")
y111$complete <- as.numeric(y111$nobs == 24)
y111 <- y111[-c(11)]

#head(y11)
#table(y11$unit)
#sum(is.na(y11$value))
#tapply(y11$date, y11$location, table)
#tb <- xtable(table(y1$location))

# The daily mean is not adjusted, and the new version data are consistent with old version data
# For convenience, use new version data (daily)
#################################################################################################

### daily data (new version)

x1 <- read.csv("daily_NONOxNOy_2011.csv", stringsAsFactors = FALSE)

x1$State.Code <- as.numeric(x1$State.Code)
x1 <- x1[-c(11,16,20,29)]
x1$location <- paste(x1$State.Code, x1$County.Code, sep = ', ')
x1$siteID <- paste(x1$State.Code, x1$County.Code, x1$Site.Num, sep = '-')
x1 <- data.frame(x1, year=rep(2011, nrow(x1)))

x11 <- x1[x1$Parameter.Code == 42603 & x1$Sample.Duration == "1 HOUR",]
x11 <- subset(x11, x11$Arithmetic.Mean!="NA")
x111 <- subset(x11, x11$location =="18, 89" | 
                 x11$location =="22, 33" |
                 x11$location =="51, 33" |
                 x11$location =="25, 9" |
                 x11$location =="44, 7" |
                 x11$location =="17, 31" |
                 x11$location =="25, 13" |
                 x11$location =="44, 3" |
                 x11$location =="22, 47" |
                 x11$location =="6, 37" |
                 x11$location =="13, 223" |
                 x11$location =="13, 247" |
                 x11$location =="13, 89" |
                 x11$location =="48, 141" |
                 x11$location =="48, 167" |
                 x11$location =="48, 201" |
                 x11$location =="18, 127" |
                 x11$location =="24, 5" |
                 x11$location =="25, 25" |
                 x11$location =="6, 65" |
                 x11$location =="18, 163" |
                 x11$location =="18, 97" |
                 x11$location =="19, 113" |
                 x11$location =="19, 153" |
                 x11$location =="19, 163" |
                 x11$location =="48, 113" |
                 x11$location =="48, 121" |
                 x11$location =="46, 99")

x111 <- subset(x111, x111$Observation.Count>=23)
x111$quality <- as.numeric(x111$Observation.Count == 24)

write.csv(x111, "NOx_2011.csv")

################################# 
#           OZone Data          #
################################# 

x1 <- read.csv("daily_44201_2011.csv", stringsAsFactors = FALSE)

x1$State.Code <- as.numeric(x1$State.Code)
x1 <- x1[-c(11,16,20,29)]
x1$location <- paste(x1$State.Code, x1$County.Code, sep = ', ')
x1$siteID <- paste(x1$State.Code, x1$County.Code, x1$Site.Num, sep = '-')
x1 <- data.frame(x1, year=rep(2011, nrow(x1)))

x11 <- x1[x1$Parameter.Code == 44201 & x1$Sample.Duration == "8-HR RUN AVG BEGIN HOUR",]
x11 <- subset(x11, x11$Arithmetic.Mean!="NA")
x111 <- subset(x11, x11$location =="18, 89" | 
                 x11$location =="22, 33" |
                 x11$location =="51, 33" |
                 x11$location =="25, 9" |
                 x11$location =="44, 7" |
                 x11$location =="17, 31" |
                 x11$location =="25, 13" |
                 x11$location =="44, 3" |
                 x11$location =="22, 47" |
                 x11$location =="6, 37" |
                 x11$location =="13, 223" |
                 x11$location =="13, 247" |
                 x11$location =="13, 89" |
                 x11$location =="48, 141" |
                 x11$location =="48, 167" |
                 x11$location =="48, 201" |
                 x11$location =="18, 127" |
                 x11$location =="24, 5" |
                 x11$location =="25, 25" |
                 x11$location =="6, 65" |
                 x11$location =="18, 163" |
                 x11$location =="18, 97" |
                 x11$location =="19, 113" |
                 x11$location =="19, 153" |
                 x11$location =="19, 163" |
                 x11$location =="48, 113" |
                 x11$location =="48, 121" |
                 x11$location =="46, 99")

x111 <- subset(x111, x111$Observation.Count>=23)
x111$quality <- as.numeric(x111$Observation.Count == 24)

write.csv(x111, "ozone_2011.csv")