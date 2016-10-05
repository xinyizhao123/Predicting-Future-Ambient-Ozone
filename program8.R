##### Add 8-hr maximum ozone

oz <- read.csv("H:/AQproject/Data Collection/Data Files/Level 2 - Subset Data/Main Parameters/ozone_all.csv", stringsAsFactors = FALSE)
head(oz)
unique(oz$siteid)
table(oz$Units.of.Measure)
str(oz)

oz <- subset(oz, oz$siteid =="13-89-2"|
               oz$siteid =="17-31-4201"|
               oz$siteid =="18-89-22"|
               oz$siteid =="22-33-13"|
               oz$siteid =="22-33-9"|
               oz$siteid =="22-47-9"|
               oz$siteid =="24-5-3001"|
               oz$siteid =="25-13-8"|
               oz$siteid =="25-25-42"|
               oz$siteid =="25-9-2006"|
               oz$siteid =="44-3-2"|
               oz$siteid =="44-7-1010"|
               oz$siteid =="48-113-69"|
               oz$siteid =="48-121-34"|
               oz$siteid =="48-201-1039"|
               oz$siteid =="48-201-24"|
               oz$siteid =="48-201-29"|
               oz$siteid =="6-37-2"|
               oz$siteid == "13-247-1"|
               oz$siteid =="48-113-69"|
               oz$siteid =="48-201-29"|
               oz$siteid == "6-37-1002"|
               oz$siteid == "6-37-6012"|
               oz$siteid == "6-71-1004"|
               oz$siteid == "48-141-44")

oz <- subset(oz, oz$Observation.Count>=23)
oz$ozone.quality <- as.numeric(oz$Observation.Count == 24)

oz1 <- oz[c(31,13,19,34)]
head(oz1)
names(oz1) <- c("siteID","date","ozone_max", "ozone.q")
table(oz1$ozone.q)
oz1$date <- as.Date(as.character(oz1$date), format("%Y-%m-%d"))
str(oz1)
oz1 <- oz1[order(oz1$siteID, oz1$date),]

test <- unique(oz1[c(1,2)]) # multiple daily values of ozone in a same day

oz2 <- oz1
oz2$ozone_m = rep(0,dim(oz2)[1])
oz2$nobs2 = rep(0,dim(oz2)[1])
tmp.id=unique(oz1$siteID)
for (iid in tmp.id){
  tmp.date=unique(oz1[which(oz1$siteID==iid),"date"])
  tmp=tapply(oz1[which(oz1$siteID==iid),"ozone_max"],oz1[which(oz1$siteID==iid),"date"],mean)
  tmp.len=tapply(oz1[which(oz1$siteID==iid),"date"],oz1[which(oz1$siteID==iid),"date"],length)
  oz2$ozone_m[which(oz1$siteID==iid)]=rep(tmp,times=tmp.len)
  oz2$nobs2[which(oz1$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(oz2$nobs2)
head(oz2)
test2 <- subset(oz2, oz2$nobs2==2)

final <- oz2[c(1,2,5,4)]
final <- final[order(final$siteID, final$date),]
final <- unique(final)

### still 3 duplicated row --find and delete

abc <- final
abc$num = rep(0,dim(abc)[1])
tmp.id=unique(final$siteID)
for (iid in tmp.id){
  tmp.date=unique(final[which(final$siteID==iid),"date"])
  tmp.len=tapply(final[which(final$siteID==iid),"date"],final[which(final$siteID==iid),"date"],length)
  abc$num[which(final$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(abc$num)
abc2 <- abc[abc$num == 2,]

final1 <-final[!(final$siteID =="17-31-4201" & final$date == "2001-07-31" & final$ozone.q == 0),]
final11 <-final1[!(final1$siteID =="17-31-4201" & final1$date == "2001-09-10" & final1$ozone.q == 0),]
final111 <-final11[!(final11$siteID =="17-31-4201" & final11$date == "2009-03-18" & final11$ozone.q == 0),]

test3 <- unique(final111[c(1,2)])

head(final111)
head(oz)

st <- read.csv("H:/AQproject/Data Analysis/study.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]
st$date <- as.Date(as.character(st$date), format("%Y-%m-%d"))
head(st)
str(st)

ozf <- final111
ozf <- ozf[order(ozf$siteID, ozf$date),]
st <- st[order(st$siteID, st$date),]

x <- merge(ozf, st, by=c("siteID", "date"))
head(x)

x <- x[-c(8,9)]

colnames(x)[3] <- "ozone"
colnames(x)[4] <- "ozone.q"

unique(x$siteID)
t1 <- subset(st, st$siteID =="13-223-3")

write.csv(x,"H:/AQproject/Data Analysis/st_oz.csv")



##### Add 24-hr maximum temperature

setwd("H:/AQproject/Data Collection/Data Files/Level 1 - Raw Data/Meteorological Data")

temp2001 <- read.csv("daily_TEMP_2001.csv", stringsAsFactors = FALSE)
temp2002 <- read.csv("daily_TEMP_2002.csv", stringsAsFactors = FALSE)
temp2003 <- read.csv("daily_TEMP_2003.csv", stringsAsFactors = FALSE)
temp2004 <- read.csv("daily_TEMP_2004.csv", stringsAsFactors = FALSE)
temp2005 <- read.csv("daily_TEMP_2005.csv", stringsAsFactors = FALSE)
temp2006 <- read.csv("daily_TEMP_2006.csv", stringsAsFactors = FALSE)
temp2007 <- read.csv("daily_TEMP_2007.csv", stringsAsFactors = FALSE)
temp2008 <- read.csv("daily_TEMP_2008.csv", stringsAsFactors = FALSE)
temp2009 <- read.csv("daily_TEMP_2009.csv", stringsAsFactors = FALSE)
temp2010 <- read.csv("daily_TEMP_2010.csv", stringsAsFactors = FALSE)

temp <- rbind(temp2001, temp2002, temp2003, temp2004, temp2005, 
              temp2006, temp2007, temp2008, temp2009, temp2010)

head(temp)

temp$siteID <- paste(temp$State.Code, temp$County.Code, temp$Site.Num, sep = '-')

temp1 <- subset(temp, temp$siteID =="13-89-2"|
                 temp$siteID =="17-31-72"|
                 temp$siteID =="18-89-22"|
                 temp$siteID =="22-33-13"|
                 temp$siteID =="22-33-9"|
                 temp$siteID =="22-47-9"|
                 temp$siteID =="24-5-3001"|
                 temp$siteID =="25-13-8"|
                 temp$siteID =="25-25-42"|
                 temp$siteID =="25-9-2006"|
                 temp$siteID =="44-3-2"|
                 temp$siteID =="44-7-1010"|
                 temp$siteID =="48-113-69"|
                 temp$siteID =="48-121-34"|
                 temp$siteID =="48-201-1039"|
                 temp$siteID =="48-201-24"|
                 temp$siteID =="48-201-29"|
                 temp$siteID =="6-37-2"|
                 temp$siteID == "13-247-1"|
                 temp$siteID =="48-113-69"|
                 temp$siteID =="48-201-29"|
                 temp$siteID == "6-37-1002"|
                 temp$siteID == "6-37-6012"|
                 temp$siteID == "6-71-1004"|
                 temp$siteID == "48-141-44")

table(temp1$siteID)

temp1$siteID[temp1$siteID == "17-31-72"] <- "17-31-4201"

temp1 <- subset(temp1, temp1$Arithmetic.Mean!="NA")
temp1 <- subset(temp1, temp1$Observation.Count>=23)
temp1$temp.q <- as.numeric(temp1$Observation.Count == 24)

head(temp1)   

temp1$Date.Local <- as.Date(as.character(temp1$Date.Local), format("%Y-%m-%d"))
temp1$year <- format(temp1$Date.Local, "%Y")

tapply(temp1$year, temp1$siteID, table)
unique(temp1$siteID)

str(temp1)

temp1 <- temp1[c(12,17,18,30,31)]
names(temp1) <- c("date","temp2","temp.max","siteID", "temp.q2")

test <- temp1
t1 <- unique(test[c(1,4)]) # one value for each day

x <- read.csv("st_oz.csv", stringsAsFactors = FALSE)
x <- x[-c(1)]
x$date <- as.Date(as.character(x$date), format("%Y-%m-%d"))
head(x)

temp1 <- temp1[order(temp1$siteID, temp1$date),]
x <- x[order(x$siteID, x$date),]

y <- merge(temp1, x, by=c("siteID", "date"))
head(y)

y <- y[-c(3,5)]
y <- y[c(1,2,4:9,3,10:21)]
colnames(y)[8] <- "temp.avg"

write.csv(y,"H:/AQproject/Data Analysis/st2.csv")

y$year <- format(y$date, "%Y")
y$month <- format(y$date, "%m")

# Select only Mar to Oct

y1 <- y
y1 <- subset(y1, y1$month != "01")
y1 <- subset(y1, y1$month != "02")
y1 <- subset(y1, y1$month != "11")
y1 <- subset(y1, y1$month != "12")
unique(y1$month)

tapply(y1$year, y1$siteID, table)

write.csv(y1,"H:/AQproject/Data Analysis/project.csv")

head(y1)