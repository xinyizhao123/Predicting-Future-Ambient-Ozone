####### look at distributions of each variables

library(lattice)
install.packages("pdflatex")

########## Historical Data

## solar radiation -- GSW
tapply(study$solar.s.GSW, study$siteID, quantile, seq(0, 1, by= 0.01), na.rm=TRUE)
histogram(~ solar.s.GSW | siteID, study)

## solar radiation -- RGRND
tapply(study$solar.s.RGRND, study$siteID, quantile, seq(0, 1, by= 0.01), na.rm=TRUE)
histogram(~ solar.s.RGRND | siteID, study)

## temperature
tapply(study$temp.max.s, study$siteID, quantile, seq(0, 1, by= 0.01), na.rm=TRUE)
histogram(~ temp.max.s | siteID, study)

## humidity
tapply(study$humid.s, study$siteID, quantile, seq(0, 1, by= 0.01), na.rm=TRUE)
histogram(~ humid.s | siteID, study)

## precipitation
tapply(study$prcp.s, study$siteID, quantile, seq(0, 1, by= 0.01), na.rm=TRUE)
histogram(~ prcp.s | siteID, study)

