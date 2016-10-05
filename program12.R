
setwd("C:/Users/Hitomi/Dropbox/Ozone project/data")

st <- read.csv("fulldata.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]

st$date <- as.Date(as.character(st$date), format("%Y-%m-%d"))
simu$date <- as.Date(as.character(simu$date), format("%m/%d/%Y"))
str(st)
str(simu)

st <- st[order(st$siteID, st$date),]
simu <- simu[order(simu$siteID, simu$date),]

pro <- merge(st, simu, by=c("siteID", "date"), all=TRUE) 

pro$year <- format(pro$date, "%Y")
pro$month <- format(pro$date, "%m")
table(pro$year, pro$month, pro$siteID)

write.csv(pro, "projectdata_12months.csv")

st <- pro
st <- subset(st, st$month != "01")
st <- subset(st, st$month != "02")
st <- subset(st, st$month != "11")
st <- subset(st, st$month != "12")
unique(st$month)

write.csv(st, "projectdata_mar2oct.csv")
table(st$year, st$month, st$siteID)


#### correlation between observed data and simulated data ###

abc <- st
ro1 <- 0
ro2 <- 0
tmp.id=unique(st$siteID)
for (iid in tmp.id){
  ro1[iid] <- cor(abc[which(st$siteID==iid),"VOC"],st[which(st$siteID==iid),"VOCS_s"], use="pairwise.complete.obs")
  ro2[iid] <- cor(abc[which(st$siteID==iid),"NOx"],st[which(st$siteID==iid),"NOx_s"], use="pairwise.complete.obs")
}

# VOC
ro1
# NOx
ro2

library(ggplot2)

st <- subset(st, st$VOC != "NA")
st <- subset(st, st$NOx != "NA")
st <- subset(st, st$year < 2006)

st0 <- subset(st, st$VOC<500)
ggplot(st0, aes(x=date, y=VOC)) + geom_point(aes(colour = factor(siteID))) + stat_smooth(aes(colour = factor(siteID))) + 
  ggtitle("Observed VOC with time (remove outliers)") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=date, y=VOCS_s)) + geom_point(aes(colour = factor(siteID))) + stat_smooth(aes(colour = factor(siteID))) + 
  ggtitle("Simulated VOC with time") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

st00 <- subset(st, st$NOx<120)
ggplot(st00, aes(x=date, y=NOx)) + geom_point(aes(colour = factor(siteID))) + stat_smooth(aes(colour = factor(siteID))) + 
  ggtitle("Observed NOx with time (remove outliers)") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=date, y=NOx_s)) + geom_point(aes(colour = factor(siteID))) + stat_smooth(aes(colour = factor(siteID))) + 
  ggtitle("Simulated NOx with time") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))





st <- read.csv("projectdata_mar2oct.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]
st <- subset(st, st$siteID =="6-71-1004")
#st <- subset(st, st$year < 2004)
st1 <- st
st1 <- subset(st1, st1$VOC != "NA")
st1 <- subset(st1, st1$NOx != "NA")

mean(st$ozone, na.rm = TRUE)
mean(st1$ozone, na.rm = TRUE)

hist(st$ozone)
hist(st1$ozone)






LL <- subset(LatLon, LatLon$LAT < 42 & LatLon$LON < -76)
m1 <- subset(LL, LL$LAT < 34.2 & LL$LAT > 33.9
             & LL$LON < -117.3 & LL$LON > -117.9)