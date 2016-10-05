setwd("C:/Users/Hitomi/Dropbox/Ozone project/data")

st <- read.csv("projectdata_mar2oct.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]

hist <- read.csv("historical.csv", stringsAsFactors = FALSE)

st$date <- as.Date(as.character(st$date), format("%Y-%m-%d"))
hist$date <- as.Date(as.character(hist$date), format("%m/%d/%Y"))
hist$month <- format(hist$date, "%m")
str(st)
str(hist)

hist <- subset(hist, hist$month != "01")
hist <- subset(hist, hist$month != "02")
hist <- subset(hist, hist$month != "11")
hist <- subset(hist, hist$month != "12")
unique(hist$month)

names(hist) <- c("siteID","solar.s.GSW","solar.s.RGRND","temp.max.s","humid.s","prcp.s","date","month")
hist <- hist[-c(8)]
st <- st[order(st$siteID, st$date),]
hist <- hist[order(hist$siteID, hist$date),]

pro <- merge(st, hist, by=c("siteID", "date"), all=TRUE) 
colnames(pro)[22] <- "NOx.s"
colnames(pro)[23] <- "VOC.s"

pro <- pro[c(1:14,22,23,26:30,24,25,15:21)]

write.csv(pro,"projectdata_final.csv")

future <- read.csv("future.csv", stringsAsFactors = FALSE)

future$date <- as.Date(as.character(future$date), format("%m/%d/%Y"))
future$month <- format(future$date, "%m")
str(future)

future <- subset(future, future$month != "01")
future <- subset(future, future$month != "02")
future <- subset(future, future$month != "11")
future <- subset(future, future$month != "12")
unique(future$month)
future <- future[-c(13)]
names(future) <- c("siteID","solar.GSW45","solar.RGRND45","temp.max45","humid45","prcp45",
                   "solar.GSW85","solar.RGRND85","temp.max85","humid85","prcp85","date")
future <- future[c(1,12,2:11)]

