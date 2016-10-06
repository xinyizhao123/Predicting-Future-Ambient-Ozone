############ Projection Program ############
load("C:/Users/zhaohexu/Dropbox/Ozone project/data/project_data.RData")
library(merTools)
st <- study

##### Historical projection

# standardize variables

st$sNOx <- st$NOx/sd(st$NOx, na.rm = TRUE)
st$sVOC <- st$VOC/sd(st$VOC, na.rm = TRUE)
st$stemp.max <- st$temp.max/sd(st$temp.max, na.rm = TRUE)
st$shumid <- st$hmdt/sd(st$hmdt, na.rm = TRUE)
st$sprcp <- st$prcp/sd(st$prcp, na.rm = TRUE)
st$ssolar <- st$solar/sd(st$solar, na.rm = TRUE)

st$stemp.max.s <- st$temp.max.s/sd(st$temp.max.s, na.rm = TRUE)
st$shumid.s <- st$humid.s/sd(st$humid.s, na.rm = TRUE)
st$sprcp.s <- st$prcp.s/sd(st$prcp.s, na.rm = TRUE)
st$ssolar.s.GSW <- st$solar.s.GSW/sd(st$solar.s.GSW, na.rm = TRUE)
st$ssolar.s.RGRND <- st$solar.s.RGRND/sd(st$solar.s.RGRND, na.rm = TRUE)

# transform variables

st$y <- sqrt(st$ozone)
st$N2 <- st$sNOx^2
st$Vl <- log(st$sVOC+1)

# projection by monitor

tmp.id <- unique(st$siteID)
ozone.obs.mean <- 0
ozone.obs.sd <- 0
ozone.pred.mean <- 0
ozone.pred.sd <- 0
ozone.pred.uct <- 0
ozone.q95.mean <- 0
ozone.q95.sd <- 0
ozone.q95.quantile <- 0

for (iid in tmp.id){
  st.sub <- st[st$siteID == iid, ]
  fit0 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
               (ssolar+stemp.max+shumid)*N2+
               (ssolar+stemp.max+shumid)*(sNOx:Vl), data=st.sub)
  
  beta <- fit0$coef
  vcov <- vcov(fit0)
  resid.sd <- summary (fit0)$sigma
  beta.sim <- t(mvrnorm(10000, beta, vcov))
  
  # design matrix
  fit1 <- lm(y~(sprcp.s+stemp.max.s+shumid.s)*sNOx+
               (ssolar.s.GSW+stemp.max.s+shumid.s)*N2+
               (ssolar.s.GSW+stemp.max.s+shumid.s)*(sNOx:Vl), data=st.sub)
  x1 <- model.matrix(fit1)
  rownames(x1) <- NULL
  
  # predicted ozone -- historical
  ozone.mean <- (x1 %*% beta.sim)
  ozone.resid <- matrix(rnorm(nrow(ozone.sim1)*ncol(ozone.sim1), 0, resid.sd), ncol = ncol(ozone.sim1))
  ozone.sim1 <- (ozone.mean + ozone.resid)^2 
                          
  ozone.mean1 <- colMeans(ozone.sim1, na.rm = TRUE)
  ozone.pred.mean[iid] <- mean(ozone.mean1, na.rm = TRUE)   #This is the point estimate
  ozone.pred.sd[iid] <- sd(ozone.mean1, na.rm = TRUE)  #This is the standard error
  ozone.pred.uct[iid] <- quantile(ozone.mean1, c(0.025, .975), na.rm = TRUE) #This is the 95% uncertainty interval
  
  # quantile
  ozone.q95 <- apply (ozone.sim1, 1, quantile, 0.95, na.rm = TRUE)
  ozone.q95.mean[iid] <- mean (ozone.q95, na.rm = T)
  ozone.q95.sd[iid] <- sd (ozone.q95, na.rm=T)
  ozone.q95.quantile[iid] <- quantile(ozone.q95, c(0.025, .975), na.rm = TRUE)
  
  # observed ozone
  st.sub2 <- subset(st.sub, st.sub$year<2005)
  ozone.obs.mean[iid] <- mean(st.sub2$ozone, na.rm = TRUE)
  ozone.obs.sd[iid] <- sd(st.sub2$ozone, na.rm = TRUE)
}

#### results
# Historical observed ozone
ozone.obs.mean
ozone.obs.sd
# Historical predicted ozone
ozone.pred.mean # mean
ozone.pred.sd # SD
ozone.pred.uct  # uncertainty interval
ozone.q95.mean 
ozone.q95.sd 
ozone.q95.quantile 


##### Future projection

# merge historical VOC and NOx to future dataset

ft <- future
ft$hist.date <- paste((as.numeric(format(ft$date, "%Y"))-50), format(ft$date,"%m-%d"), sep="-") 
ft$hist.date <- as.Date(as.character(ft$hist.date), format("%Y-%m-%d"))

st0 <- st[c(1,2,31:36,42:44)]
colnames(st0)[2] <- "hist.date"
st0 <- st0[order(st0$siteID, st0$hist.date),]
ft <- ft[order(ft$siteID, ft$hist.date),]
ftr <- merge(st0, ft, by=c("siteID", "hist.date")) 

ftr$stemp.max45 <- ftr$temp.max45/sd(ftr$temp.max45, na.rm = TRUE)
ftr$shumid45 <- ftr$humid45/sd(ftr$humid45, na.rm = TRUE)
ftr$sprcp45 <- ftr$prcp45/sd(ftr$prcp45, na.rm = TRUE)
ftr$ssolar.GSW45 <- ftr$solar.GSW45/sd(ftr$solar.GSW45, na.rm = TRUE)
ftr$ssolar.RGRND45 <- ftr$solar.RGRND45/sd(ftr$solar.RGRND45, na.rm = TRUE)

ftr$stemp.max85 <- ftr$temp.max85/sd(ftr$temp.max85, na.rm = TRUE)
ftr$shumid85 <- ftr$humid85/sd(ftr$humid85, na.rm = TRUE)
ftr$sprcp85 <- ftr$prcp85/sd(ftr$prcp85, na.rm = TRUE)
ftr$ssolar.GSW85 <- ftr$solar.GSW85/sd(ftr$solar.GSW85, na.rm = TRUE)
ftr$ssolar.RGRND85 <- ftr$solar.RGRND85/sd(ftr$solar.RGRND85, na.rm = TRUE)

# projection by monitor

tmp.id <- unique(ftr$siteID)

ozone.pred.mean.GSW45 <- 0
ozone.pred.sd.GSW45 <- 0
ozone.pred.IQR.GSW45 <- 0

ozone.pred.mean.GSW85 <- 0
ozone.pred.sd.GSW85 <- 0
ozone.pred.IQR.GSW85 <- 0

for (iid in tmp.id){
  ftr.sub <- ftr[ftr$siteID == iid, ]
  fit0 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
               (ssolar+stemp.max+shumid)*N2+
               (ssolar+stemp.max+shumid)*(sNOx:Vl), data=ftr.sub)
  
  beta <- fit0$coef
  vcov <- vcov(fit0)
  beta.sim <- t(mvrnorm(10000, beta, vcov))
  
  # design matrix
  # (1) solar radiation GSW (RCP 4.5)
  fit1 <- lm(y~(sprcp45+stemp.max45+shumid45)*sNOx+
               (ssolar.GSW45+stemp.max45+shumid45)*N2+
               (ssolar.GSW45+stemp.max45+shumid45)*(sNOx:Vl), data=ftr.sub)
  x1 <- model.matrix(fit1)
  rownames(x1) <- NULL
  
  # (3) solar radiation GSW (RCP 8.5)
  fit3 <- lm(y~(sprcp85+stemp.max85+shumid85)*sNOx+
               (ssolar.GSW85+stemp.max85+shumid85)*N2+
               (ssolar.GSW85+stemp.max85+shumid85)*(sNOx:Vl), data=ftr.sub)
  x3 <- model.matrix(fit3)
  rownames(x3) <- NULL
  
  # predicted ozone -- future
  # (1) solar radiation GSW (RCP 4.5)
  ozone.sim1 <- (x1 %*% beta.sim)^2
  ozone.mean1 <- colMeans(ozone.sim1, na.rm = TRUE)
  
  ozone.pred.mean.GSW45[iid] <- mean(ozone.mean1, na.rm = TRUE)
  ozone.pred.sd.GSW45[iid] <- sd(ozone.mean1, na.rm = TRUE)
  ozone.pred.IQR.GSW45[iid] <- IQR(ozone.mean1, na.rm = TRUE)
  
  # (3) solar radiation GSW (RCP 8.5)
  ozone.sim3 <- (x3 %*% beta.sim)^2
  ozone.mean3 <- colMeans(ozone.sim3, na.rm = TRUE)
  
  ozone.pred.mean.GSW85[iid] <- mean(ozone.mean3, na.rm = TRUE)
  ozone.pred.sd.GSW85[iid] <- sd(ozone.mean3, na.rm = TRUE)
  ozone.pred.IQR.GSW85[iid] <- IQR(ozone.mean3, na.rm = TRUE)
  
}

#### results
# Future predicted ozone -- solar radiation GSW (RCP 4.5)
ozone.pred.mean.GSW45
ozone.pred.sd.GSW45
ozone.pred.IQR.GSW45

# Future predicted ozone -- solar radiation GSW (RCP 8.5)
ozone.pred.mean.GSW85
ozone.pred.sd.GSW85
ozone.pred.IQR.GSW85


