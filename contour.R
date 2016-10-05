#install.packages("akima")

library(lme4)
library(Metrics)
library(merTools)
library(lattice)
library(akima)

st <- read.csv("C:/Users/zhaohexu/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]

st <- subset(st, st$state != "Rhode Island") # drop Providence

# standardize variables

st$sNOx <- st$NOx/sd(st$NOx)
st$sVOC <- st$VOC/sd(st$VOC)
st$stemp.max <- st$temp.max/sd(st$temp.max)
st$shumid <- st$humid/sd(st$humid)
st$sprcp <- st$prcp/sd(st$prcp)
st$ssolar <- st$solar/sd(st$solar)

st$y <- sqrt(st$ozone)
st$N2 <- st$sNOx^2
st$Vl <- log(st$sVOC+1)

st <- subset(st, st$siteID == "13-247-1"|st$siteID == "13-89-2"|st$siteID == "24-5-3001"|st$siteID == "18-89-22"|
               st$siteID == "48-121-34"|st$siteID == "6-37-2"|st$siteID == "6-71-1004"|st$siteID == "48-113-69"|
               st$siteID == "6-37-1002"|st$siteID == "6-37-6012")

st$m.stemp <- rep(0,length(st$date))
tmp.id <- unique(st$siteID)

st1=st2=st3=st4=st

#Covariates using median#
bind<-NULL
for (iid in tmp.id){
  a<-t(as.matrix( apply(st[which(st$siteID==iid),][,c("stemp.max","shumid","sprcp","ssolar")],2,function(x)median(x,na.rm=T))))
  a.expand<-a[rep(1,nrow(st[which(st$siteID==iid),])),]
  bind<-rbind(bind,a.expand)
}
st[,c("m.stemp.max","m.shumid","m.sprcp","m.ssolar")]=bind

num<-unique(st$siteID)


predi.12=NULL
for (i in 1:length(num)){
  
  test <- subset(st,siteID==num[i]&st$VOC<1000) 
  
  fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl), data=test)
  
  n.data=test[,c("m.stemp.max","m.shumid","m.sprcp","m.ssolar","NOx","VOC")] 
  grid.data=expand.grid(list(x=seq(min(n.data$NOx),max(n.data$NOx),0.2),y=seq(min(n.data$VOC),max(n.data$VOC),5))) 
  median.data=n.data[rep(1,nrow(grid.data)),c("m.stemp.max","m.shumid","m.sprcp","m.ssolar")] 
  grid.data=cbind(grid.data,median.data)
  colnames(grid.data)=c("NOx","VOC","stemp.max","shumid","sprcp","ssolar")
  
  grid.data$sVOC=grid.data$VOC/sd(st$VOC)
  grid.data$sNOx=grid.data$NOx/sd(st$NOx)
  grid.data$Vl=log(grid.data$sVOC+1)
  grid.data$N2=(grid.data$sNOx)^2
  
  predi.12<- predict(fit.12, newdata=grid.data, allow.new.levels=T)
  print(levelplot(predi.12 ~ VOC*NOx, data = grid.data,
                  main=paste("8-hr Max Ozone (ppm), site ID = ", num[i]),
                  xlab="VOC (ppbC)", ylab="NOx (ppb)",
                  col.regions = terrain.colors(100))
  )    
}


#Covariates using 75% stemp.max quantile#
st=st1
bind<-NULL
for (iid in tmp.id){
  a<-as.matrix( apply(st[which(st$siteID==iid),][,c("stemp.max","shumid","sprcp","ssolar")],2,function(x)quantile(x,c(0.5,0.75),na.rm=T)))
  a.new<-t(as.matrix(c(a[2,1],a[1,2:4])))
  a.expand<-a.new[rep(1,nrow(st[which(st$siteID==iid),])),]
  bind<-rbind(bind,a.expand)
}
st[,c("TQ.stemp.max","m.shumid","m.sprcp","m.ssolar")]=bind

num<-unique(st$siteID)


predi.12=NULL
for (i in 1:length(num)){
  
  test <- subset(st,siteID==num[i]&st$VOC<1000) 
  
  fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl), data=test)
  
  n.data=test[,c("TQ.stemp.max","m.shumid","m.sprcp","m.ssolar","NOx","VOC")] 
  grid.data=expand.grid(list(x=seq(min(n.data$NOx),max(n.data$NOx),0.2),y=seq(min(n.data$VOC),max(n.data$VOC),5))) 
  median.data=n.data[rep(1,nrow(grid.data)),c("TQ.stemp.max","m.shumid","m.sprcp","m.ssolar")] 
  grid.data=cbind(grid.data,median.data)
  colnames(grid.data)=c("NOx","VOC","stemp.max","shumid","sprcp","ssolar")
  
  grid.data$sVOC=grid.data$VOC/sd(st$VOC)
  grid.data$sNOx=grid.data$NOx/sd(st$NOx)
  grid.data$Vl=log(grid.data$sVOC+1)
  grid.data$N2=(grid.data$sNOx)^2
  
  predi.12<- predict(fit.12, newdata=grid.data, allow.new.levels=T)
  print(levelplot(predi.12 ~ VOC*NOx, data = grid.data,
                  main=paste("8-hr Max Ozone (ppm), site ID = ", num[i]),
                  xlab="VOC (ppbC)", ylab="NOx (ppb)",
                  col.regions = terrain.colors(100),sub="75% stemp.max quantile")
  )    
}


#Covariates using 75% shumid quantile#
st=st2
bind<-NULL
for (iid in tmp.id){
  a<-as.matrix( apply(st[which(st$siteID==iid),][,c("stemp.max","shumid","sprcp","ssolar")],2,function(x)quantile(x,c(0.5,0.75),na.rm=T)))
  a.new<-t(as.matrix(c(a[1,1],a[2,2],a[1,3:4])))
  a.expand<-a.new[rep(1,nrow(st[which(st$siteID==iid),])),]
  bind<-rbind(bind,a.expand)
}
st[,c("m.stemp.max","TQ.shumid","m.sprcp","m.ssolar")]=bind

num<-unique(st$siteID)


predi.12=NULL
for (i in 1:length(num)){
  
  test <- subset(st,siteID==num[i]&st$VOC<1000) 
  
  fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl), data=test)
  
  n.data=test[,c("m.stemp.max","TQ.shumid","m.sprcp","m.ssolar","NOx","VOC")] 
  grid.data=expand.grid(list(x=seq(min(n.data$NOx),max(n.data$NOx),0.2),y=seq(min(n.data$VOC),max(n.data$VOC),5))) 
  median.data=n.data[rep(1,nrow(grid.data)),c("m.stemp.max","TQ.shumid","m.sprcp","m.ssolar")] 
  grid.data=cbind(grid.data,median.data)
  colnames(grid.data)=c("NOx","VOC","stemp.max","shumid","sprcp","ssolar")
  
  grid.data$sVOC=grid.data$VOC/sd(st$VOC)
  grid.data$sNOx=grid.data$NOx/sd(st$NOx)
  grid.data$Vl=log(grid.data$sVOC+1)
  grid.data$N2=(grid.data$sNOx)^2
  
  predi.12<- predict(fit.12, newdata=grid.data, allow.new.levels=T)
  print(levelplot(predi.12 ~ VOC*NOx, data = grid.data,
                  main=paste("8-hr Max Ozone (ppm), site ID = ", num[i]),
                  xlab="VOC (ppbC)", ylab="NOx (ppb)",
                  col.regions = terrain.colors(100),sub="75% shumid quantile")
  )    
}


#Covariates using 75% sprcp quantile#
st=st2
bind<-NULL
for (iid in tmp.id){
  a<-as.matrix( apply(st[which(st$siteID==iid),][,c("stemp.max","shumid","sprcp","ssolar")],2,function(x)quantile(x,c(0.5,0.75),na.rm=T)))
  a.new<-t(as.matrix(c(a[1,1:2],a[2,3],a[1,4])))
  a.expand<-a.new[rep(1,nrow(st[which(st$siteID==iid),])),]
  bind<-rbind(bind,a.expand)
}
st[,c("m.stemp.max","m.shumid","TQ.sprcp","m.ssolar")]=bind

num<-unique(st$siteID)


predi.12=NULL
for (i in 1:length(num)){
  
  test <- subset(st,siteID==num[i]&st$VOC<1000) 
  
  fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl), data=test)
  
  n.data=test[,c("m.stemp.max","m.shumid","TQ.sprcp","m.ssolar","NOx","VOC")] 
grid.data=expand.grid(list(x=seq(min(n.data$NOx),max(n.data$NOx),0.2),y=seq(min(n.data$VOC),max(n.data$VOC),5))) 
median.data=n.data[rep(1,nrow(grid.data)),c("m.stemp.max","m.shumid","TQ.sprcp","m.ssolar")] 
grid.data=cbind(grid.data,median.data)
colnames(grid.data)=c("NOx","VOC","stemp.max","shumid","sprcp","ssolar")

grid.data$sVOC=grid.data$VOC/sd(st$VOC)
grid.data$sNOx=grid.data$NOx/sd(st$NOx)
grid.data$Vl=log(grid.data$sVOC+1)
grid.data$N2=(grid.data$sNOx)^2

predi.12<- predict(fit.12, newdata=grid.data, allow.new.levels=T)
print(levelplot(predi.12 ~ VOC*NOx, data = grid.data,
                main=paste("8-hr Max Ozone (ppm), site ID = ", num[i]),
                xlab="VOC (ppbC)", ylab="NOx (ppb)",
                col.regions = terrain.colors(100),sub="75% sprcp quantile")
)    
}


#Covariates using 75% solar quantile#
st=st2
bind<-NULL
for (iid in tmp.id){
  a<-as.matrix( apply(st[which(st$siteID==iid),][,c("stemp.max","shumid","sprcp","ssolar")],2,function(x)quantile(x,c(0.5,0.75),na.rm=T)))
  a.new<-t(as.matrix(c(a[1,1:3],a[2,4])))
  a.expand<-a.new[rep(1,nrow(st[which(st$siteID==iid),])),]
  bind<-rbind(bind,a.expand)
}
st[,c("m.stemp.max","m.shumid","m.sprcp","TQ.ssolar")]=bind

num<-unique(st$siteID)


predi.12=NULL
for (i in 1:length(num)){
  
  test <- subset(st,siteID==num[i]&st$VOC<1000) 
  
  fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl), data=test)
  
  n.data=test[,c("m.stemp.max","m.shumid","m.sprcp","TQ.ssolar","NOx","VOC")] 
  grid.data=expand.grid(list(x=seq(min(n.data$NOx),max(n.data$NOx),0.2),y=seq(min(n.data$VOC),max(n.data$VOC),5))) 
  median.data=n.data[rep(1,nrow(grid.data)),c("m.stemp.max","m.shumid","m.sprcp","TQ.ssolar")] 
  grid.data=cbind(grid.data,median.data)
  colnames(grid.data)=c("NOx","VOC","stemp.max","shumid","sprcp","ssolar")
  
  grid.data$sVOC=grid.data$VOC/sd(st$VOC)
  grid.data$sNOx=grid.data$NOx/sd(st$NOx)
  grid.data$Vl=log(grid.data$sVOC+1)
  grid.data$N2=(grid.data$sNOx)^2
  
  predi.12<- predict(fit.12, newdata=grid.data, allow.new.levels=T)
  print(levelplot(predi.12 ~ VOC*NOx, data = grid.data,
                  main=paste("8-hr Max Ozone (ppm), site ID = ", num[i]),
                  xlab="VOC (ppbC)", ylab="NOx (ppb)",
                  col.regions = terrain.colors(100),sub="75% ssolar quantile")
  )    
}
