#estaimation of prediction intervals (PI) for PAWC to build accuracy plots Fig.7
 
#for GAM ####
library(mgcv)
library(lattice)
#gam1=final model
pred_gam<- predict(gam1,se=TRUE)
se <- sqrt(pred_gam$se.fit^2 + gam1$sig2)

#calcluate PI for p=0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99 
gam.pi10 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi10)<-c("p45","p55")
gam.pi10[[1]] <- pred_gam$fit - qnorm(.55)*se
gam.pi10[[2]] <- pred_gam$fit + qnorm(.55)*se
gam.pi10[1:3,]
gam.pi20 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi20)<-c("p40","p60")
gam.pi20[[1]] <- pred_gam$fit - qnorm(.60)*se
gam.pi20[[2]] <- pred_gam$fit + qnorm(.60)*se
gam.pi20[1:3,]
gam.pi30 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi30)<-c("p35","p65")
gam.pi30[[1]] <- pred_gam$fit - qnorm(.65)*se
gam.pi30[[2]] <- pred_gam$fit + qnorm(.65)*se
gam.pi30[1:3,]
gam.pi40 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi40)<-c("p30","p70")
gam.pi40[[1]] <- pred_gam$fit - qnorm(.70)*se
gam.pi40[[2]] <- pred_gam$fit + qnorm(.70)*se
gam.pi50 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi50)<-c("p25","p75")
gam.pi50[[1]] <- pred_gam$fit - qnorm(.75)*se
gam.pi50[[2]] <- pred_gam$fit + qnorm(.75)*se
gam.pi50[1:3,]
gam.pi60 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi60)<-c("p20","p80")
gam.pi60[[1]] <- pred_gam$fit - qnorm(.80)*se
gam.pi60[[2]] <- pred_gam$fit + qnorm(.80)*se
gam.pi60[1:3,]
gam.pi70 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi70)<-c("p15","p85")
gam.pi70[[1]] <- pred_gam$fit - qnorm(.85)*se
gam.pi70[[2]] <- pred_gam$fit + qnorm(.85)*se
gam.pi70[1:3,]
gam.pi80 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi80)<-c("p10","p90")
gam.pi80[[1]] <- pred_gam$fit - qnorm(.90)*se
gam.pi80[[2]] <- pred_gam$fit + qnorm(.90)*se
gam.pi80[1:3,]
gam.pi90 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi90)<-c("p05","p95")
gam.pi90[[1]] <- pred_gam$fit - qnorm(.95)*se
gam.pi90[[2]] <- pred_gam$fit + qnorm(.95)*se
gam.pi95 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi95)<-c("p025","p975")
gam.pi95[[1]] <- pred_gam$fit - qnorm(.975)*se
gam.pi95[[2]] <- pred_gam$fit + qnorm(.975)*se
gam.pi99 <- as.data.frame(matrix(nrow=1625, ncol=2)); colnames(gam.pi99)<-c("p005","p995")
gam.pi99[[1]] <- pred_gam$fit - qnorm(.995)*se
gam.pi99[[2]] <- pred_gam$fit + qnorm(.995)*se
gam.pi99[1:3,]

gam.pi.df <- as.data.frame(cbind(gam1$fitted.values, data_proxy$nfk_sqrt, 
                                 gam.pi10$p45, gam.pi10$p55,
                                 gam.pi20$p40, gam.pi20$p60,
                                 gam.pi30$p35, gam.pi30$p65,
                                 gam.pi40$p30, gam.pi40$p70,
                                 gam.pi50$p25, gam.pi50$p75,
                                 gam.pi60$p20, gam.pi60$p80,
                                 gam.pi70$p15, gam.pi70$p85,
                                 gam.pi80$p10, gam.pi80$p90,
                                 gam.pi90$p05, gam.pi90$p95,
                                 gam.pi95$p025, gam.pi95$p975,
                                 gam.pi99$p005, gam.pi99$p995))
names(gam.pi.df) <- c("fit","obs","pi10.lwr","pi10.upr","pi20.lwr","pi20.upr","pi30.lwr","pi30.upr","pi40.lwr","pi40.upr",
                      "pi50.lwr","pi50.upr","pi60.lwr","pi60.upr","pi70.lwr","pi70.upr","pi80.lwr","pi80.upr",
                      "pi90.lwr","pi90.upr","pi95.lwr","pi95.upr","pi99.lwr","pi99.upr")
#View(gam.pi.df)

gam.pi.df$pi10.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi10.lwr & gam.pi.df$obs<=gam.pi.df$pi10.upr,1,0) 
gam.pi.df$pi20.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi20.lwr & gam.pi.df$obs<=gam.pi.df$pi20.upr,1,0) 
gam.pi.df$pi30.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi30.lwr & gam.pi.df$obs<=gam.pi.df$pi30.upr,1,0) 
gam.pi.df$pi40.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi40.lwr & gam.pi.df$obs<=gam.pi.df$pi40.upr,1,0) 
gam.pi.df$pi50.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi50.lwr & gam.pi.df$obs<=gam.pi.df$pi50.upr,1,0) 
gam.pi.df$pi60.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi60.lwr & gam.pi.df$obs<=gam.pi.df$pi60.upr,1,0) 
gam.pi.df$pi70.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi70.lwr & gam.pi.df$obs<=gam.pi.df$pi70.upr,1,0) 
gam.pi.df$pi80.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi80.lwr & gam.pi.df$obs<=gam.pi.df$pi80.upr,1,0) 
gam.pi.df$pi90.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi90.lwr & gam.pi.df$obs<=gam.pi.df$pi90.upr,1,0) 
gam.pi.df$pi95.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi95.lwr & gam.pi.df$obs<=gam.pi.df$pi95.upr,1,0) 
gam.pi.df$pi99.t <- ifelse(gam.pi.df$obs>=gam.pi.df$pi99.lwr & gam.pi.df$obs<=gam.pi.df$pi99.upr,1,0) 

gam.pi.plot <- data.frame("frac_true"=c(
  sum(gam.pi.df$pi10.t)/length(gam.pi.df$pi10.t),
  sum(gam.pi.df$pi20.t)/length(gam.pi.df$pi20.t),
  sum(gam.pi.df$pi30.t)/length(gam.pi.df$pi30.t),
  sum(gam.pi.df$pi40.t)/length(gam.pi.df$pi40.t),
  sum(gam.pi.df$pi50.t)/length(gam.pi.df$pi50.t),
  sum(gam.pi.df$pi60.t)/length(gam.pi.df$pi60.t),
  sum(gam.pi.df$pi70.t)/length(gam.pi.df$pi70.t),
  sum(gam.pi.df$pi80.t)/length(gam.pi.df$pi80.t),
  sum(gam.pi.df$pi90.t)/length(gam.pi.df$pi90.t),
  sum(gam.pi.df$pi95.t)/length(gam.pi.df$pi95.t),
  sum(gam.pi.df$pi99.t)/length(gam.pi.df$pi99.t)),
  "quant"=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
gam.pi.plot$type <- "GAM"
View(gam.pi.plot) 


#for RF ####
library(quantregForest)

predictors <- c("R_SubstGenValueMix", "R_Karbonat_Mix","curv_plan", "slope_deg",
                "sqrt_swi", "sqrt_valde", "raster_5_x", "raster_5_y", "temp", "prec", "glo_rad")

rf_qrF <- quantregForest::quantregForest(x=data_proxy[,predictors], y=data_proxy[,26], mtry=6, ntree=500, keep.inbag=TRUE)
plot(rf_qrF)
print(rf_qrF)

#calcluate PI for p=0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99 
rf.pi10 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.45,0.55)))
rf.pi20 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.40,0.60)))
rf.pi30 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.35,0.65)))
rf.pi40 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.30,0.70)))
rf.pi50 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.25,0.75)))
rf.pi60 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.20,0.80)))
rf.pi70 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.15,0.85)))
rf.pi80 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.10,0.90)))
rf.pi90 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.05,0.95)))
rf.pi95 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.025,0.975)))
rf.pi99 <- as.data.frame(predict(rf_qrF, newdata=NULL, what=c(0.005,0.995)))

rf.pi.df <- as.data.frame(cbind(mod.nfk_sqrt.rf$finalModel$predicted, data_proxy$nfk_sqrt, 
                                rf.pi10$`quantile= 0.45`, rf.pi10$`quantile= 0.55`,
                                rf.pi20$`quantile= 0.4`, rf.pi20$`quantile= 0.6`,
                                rf.pi30$`quantile= 0.35`, rf.pi30$`quantile= 0.65`,
                                rf.pi40$`quantile= 0.3`, rf.pi40$`quantile= 0.7`,
                                rf.pi50$`quantile= 0.25`, rf.pi50$`quantile= 0.75`,
                                rf.pi60$`quantile= 0.2`, rf.pi60$`quantile= 0.8`,
                                rf.pi70$`quantile= 0.15`, rf.pi70$`quantile= 0.85`,
                                rf.pi80$`quantile= 0.1`, rf.pi80$`quantile= 0.9`,
                                rf.pi90$`quantile= 0.05`, rf.pi90$`quantile= 0.95`,
                                rf.pi95$`quantile= 0.025`, rf.pi95$`quantile= 0.975`,
                                rf.pi99$`quantile= 0.005`, rf.pi99$`quantile= 0.995`))
names(rf.pi.df) <- c("fit","obs","pi10.lwr","pi10.upr","pi20.lwr","pi20.upr","pi30.lwr","pi30.upr","pi40.lwr","pi40.upr",
                     "pi50.lwr","pi50.upr","pi60.lwr","pi60.upr","pi70.lwr","pi70.upr","pi80.lwr","pi80.upr",
                     "pi90.lwr","pi90.upr","pi95.lwr","pi95.upr","pi99.lwr","pi99.upr")

rf.pi.df$pi10.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi10.lwr & rf.pi.df$obs<=rf.pi.df$pi10.upr,1,0) 
rf.pi.df$pi20.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi20.lwr & rf.pi.df$obs<=rf.pi.df$pi20.upr,1,0) 
rf.pi.df$pi30.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi30.lwr & rf.pi.df$obs<=rf.pi.df$pi30.upr,1,0) 
rf.pi.df$pi40.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi40.lwr & rf.pi.df$obs<=rf.pi.df$pi40.upr,1,0) 
rf.pi.df$pi50.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi50.lwr & rf.pi.df$obs<=rf.pi.df$pi50.upr,1,0) 
rf.pi.df$pi60.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi60.lwr & rf.pi.df$obs<=rf.pi.df$pi60.upr,1,0) 
rf.pi.df$pi70.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi70.lwr & rf.pi.df$obs<=rf.pi.df$pi70.upr,1,0) 
rf.pi.df$pi80.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi80.lwr & rf.pi.df$obs<=rf.pi.df$pi80.upr,1,0) 
rf.pi.df$pi90.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi90.lwr & rf.pi.df$obs<=rf.pi.df$pi90.upr,1,0) 
rf.pi.df$pi95.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi95.lwr & rf.pi.df$obs<=rf.pi.df$pi95.upr,1,0) 
rf.pi.df$pi99.t <- ifelse(rf.pi.df$obs>=rf.pi.df$pi99.lwr & rf.pi.df$obs<=rf.pi.df$pi99.upr,1,0) 

rf.pi.plot <- data.frame("frac_true"=c(
  sum(rf.pi.df$pi10.t)/length(rf.pi.df$pi10.t),
  sum(rf.pi.df$pi20.t)/length(rf.pi.df$pi20.t),
  sum(rf.pi.df$pi30.t)/length(rf.pi.df$pi30.t),
  sum(rf.pi.df$pi40.t)/length(rf.pi.df$pi40.t),
  sum(rf.pi.df$pi50.t)/length(rf.pi.df$pi50.t),
  sum(rf.pi.df$pi60.t)/length(rf.pi.df$pi60.t),
  sum(rf.pi.df$pi70.t)/length(rf.pi.df$pi70.t),
  sum(rf.pi.df$pi80.t)/length(rf.pi.df$pi80.t),
  sum(rf.pi.df$pi90.t)/length(rf.pi.df$pi90.t),
  sum(rf.pi.df$pi95.t)/length(rf.pi.df$pi95.t),
  sum(rf.pi.df$pi99.t)/length(rf.pi.df$pi99.t)),
  "quant"=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
rf.pi.plot$type <- "RF"

# union gam & rf ####
pi.plot <- rbind(gam.pi.plot, rf.pi.plot)
#plot
setwd("xxxx")
jpeg(file="PAWC_uncertainty_fractions.jpg", width=1000, height=800, pointsize=15, res=100)
lattice.options(layout.widths = lw, layout.heights = lh)
xyplot(frac_true ~ quant, groups=type, pi.plot, type="p",
       ylab=list(label=expression("coverage probability"), cex=2.3),  
       xlim = c(0,1.01),ylim=c(0,1.01),
       xlab=list(label="prediction interval", cex=2.3),
       scales=list(cex=2),
       auto.key=list(corner=c(0.05,0.95), cex=2.3),
       par.settings= list(superpose.symbol=list(col=c("black"), pch=c(19,1), cex=1.5)),
       panel=function(x,y, ...){
         panel.xyplot(x,y, ...) 
         panel.abline (a=0, b=1, lty=1, lwd=2, col="black")
       }
)
dev.off()