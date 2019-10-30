#creates residual variograms
#test for spatial autocorrelation, Fig.6

library(mgcv)
library(splines)
library(rgdal)
library(raster)
library(PerformanceAnalytics)
library(spatstat)
library(maptools)
library (rgdal)
library(lattice)
library(sqldf)
require(rgeos)
library(gstat)


## combine rsd with coordinate for GAM and RF
#residuals of GAM
rsd_gam <- residuals.gam(gam1)
#residuals fo RF
rsd_rf <- as.numeric(residuals(mod.nfk_sqrt.rf))

rsd_AufID <- data_proxy$AufID
rsd_pawc_df <- as.data.frame(cbind(rsd_AufID, rsd_gam, rsd_rf))

#extract coordates to pkt_coords

#combine with coordinates
pkt_rsd_pawc <- sqldf("SELECT rsd_pawc_df.*, pkt_coords.* 
                     FROM rsd_pawc_df INNER JOIN pkt_coords 
                     ON (rsd_pawc_df.rsd_gam_AufID = pkt_coords.AufID);")


#GAM residuals variogram
xy_rsd <- as.data.frame(cbind(pkt_rsd_pawc$coords.x1, pkt_rsd_pawc$coords.x2))
pkt_rsd_pawc_sp <- SpatialPointsDataFrame(coords=xy_rsd, data=pkt_rsd_pawc ,proj4string=CRS("+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +units=m +no_defs"))

x.temp<- matrix(nrow=20, ncol=100)

for (i in 1:100) {
  pkt_rsd_pawc_sp$rsd_gam_ran <- sample(pkt_rsd_pawc$rsd_gam)
  v2 = variogram(rsd_gam_ran ~1 , pkt_rsd_pawc_sp, cutoff=20000, width=1000)
  x.temp[,i] <- v2$gamma
}

v.crs.rsd_gam <- as.data.frame(apply(x.temp, 1, max))
names(v.crs.rsd_gam) <- "max"
v.crs.rsd_gam$min <- apply(x.temp, 1, min)
v.crs.rsd_gam$q95 <- apply(x.temp, 1, quantile, probs=c(0.95))
v.crs.rsd_gam$q05 <- apply(x.temp, 1, quantile, probs=c(0.05))
v <- variogram(rsd_gam ~1 , pkt_rsd_gam_sp,cutoff=20000, width=1000)
v.crs.rsd_gam$dist <- v$dist 
v.crs.rsd_gam$gamma <- v$gamma
v.crs.rsd_gam$model <- "GAM residuals"


#RF residuals variogram
x.temp<- matrix(nrow=20, ncol=100)

for (i in 1:100) {
  pkt_rsd_pawc_sp$rsd_rf_ran <- sample(pkt_rsd_pawc$rsd_rf)
  v2 = variogram(rsd_rf_ran ~1 , pkt_rsd_pawc_sp, cutoff=20000, width=1000)
  x.temp[,i] <- v2$gamma
}

v.crs.rsd_rf <- as.data.frame(apply(x.temp, 1, max))
names(v.crs.rsd_rf) <- "max"
v.crs.rsd_rf$min <- apply(x.temp, 1, min)
v.crs.rsd_rf$q95 <- apply(x.temp, 1, quantile, probs=c(0.95))
v.crs.rsd_rf$q05 <- apply(x.temp, 1, quantile, probs=c(0.05))
v <- variogram(rsd_rf ~1 , pkt_rsd_gam_sp,cutoff=20000, width=1000)
v.crs.rsd_rf$dist <- v$dist 
v.crs.rsd_rf$gamma <- v$gamma
v.crs.rsd_rf$model <- "RF residuals"


#PAWC observed variogram
x.temp<- matrix(nrow=20, ncol=100)

for (i in 1:100) {
  pkt_rsd_pawc_sp$nfk_gel_ran <- sample(pkt_rsd_pawc$nfk_gel)
  v2 = variogram(nfk_gel_ran ~1 , pkt_rsd_pawc_sp, cutoff=20000, width=1000)
  x.temp[,i] <- v2$gamma
}

v.crs.nfk_gel <- as.data.frame(apply(x.temp, 1, max))
names(v.crs.nfk_gel) <- "max"
v.crs.nfk_gel$min <- apply(x.temp, 1, min)
v.crs.nfk_gel$q95 <- apply(x.temp, 1, quantile, probs=c(0.95))
v.crs.nfk_gel$q05 <- apply(x.temp, 1, quantile, probs=c(0.05))
v <- variogram(nfk_gel ~1 , pkt_rsd_gam_sp, cutoff=20000, width=1000)
v.crs.nfk_gel$dist <- v$dist 
v.crs.nfk_gel$gamma <- v$gamma
v.crs.nfk_gel$model <- "PAWC observed"
v.crs.nfk_gel$gamma <- v.crs.nfk_gel$gamma/600
v.crs.nfk_gel$min <- v.crs.nfk_gel$min/600
v.crs.nfk_gel$max <- v.crs.nfk_gel$max/600
v.crs.nfk_gel$q95 <- v.crs.nfk_gel$q95/600
v.crs.nfk_gel$q05 <- v.crs.nfk_gel$q05/600


#combine datasets
var.crs.ges <- rbind(v.crs.rsd_gam, v.crs.rsd_rf, v.crs.nfk_gel)

#plot Fig6
lw <- list(left.padding = list(x = 0.1, units = "inches"))
lw$right.padding <- list(x = 0.3, units = "inches")
lh <- list(bottom.padding = list(x = 0.1, units = "inches"))
lh$top.padding <- list(x = -0.2, units = "inches")

setwd("XXXX")
jpeg(file="Fig_6.jpg", width=1000, height=800, pointsize=15, res=100)
lattice.options(layout.widths = lw, layout.heights = lh)

xyplot(gamma ~ dist | model, data=var.crs.ges, type="l", layout=c(1,3),
       ylab=list(label=expression(paste("semivariance [")*gamma*"]"), cex=2.3),  
       xlim = c(0, 20000),ylim=c(2.75,4.25), index.cond=list(c(3,1,2)),
       xlab=list(label="distance [m]", cex=2.3),
       scales=list(x=list(cex=1.8, labels=c("0", "5*10³", "10*10³", "15*10³", "20*10³")), 
                   y=list(cex=1.8),alternating=c(1),tick.number=3), 
       strip=strip.custom(bg="white"), par.strip.text=list(cex=2),
       panel=function(x,y, subscripts){
         lpolygon(c(var.crs.ges$dist[subscripts], rev(var.crs.ges$dist[subscripts])),c(var.crs.ges$q05[subscripts], rev(var.crs.ges$q95[subscripts])), border="gray90", col="gray90", lwd=0.1)
         panel.xyplot(x,y, type="l", lwd=4, col="black") 
       }
)
dev.off()

