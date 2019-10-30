#plottet CV results of GAM & RF 

#create dataset
msss_nfk.gam.train <- as.data.frame(msssTrain[,1])
colnames(msss_nfk.gam.train)[1] <- "msss"
msss_nfk.gam.train$model <- c("GAM rCV") 
msss_nfk.gam.train$dataset <- c("training: 90%")
msss_nfk.gam.test <- as.data.frame(msssTest[,1])
colnames(msss_nfk.gam.test)[1] <- "msss"
msss_nfk.gam.test$model <- c("GAM rCV") 
msss_nfk.gam.test$dataset <- c("prediction: 10%")
msss_nfk.gam <- rbind(msss_nfk.gam.train, msss_nfk.gam.test)

msss_nfk.gam.train.qmap <- as.data.frame(msssTrainQMAP[,1])
colnames(msss_nfk.gam.train.qmap)[1] <- "msss"
msss_nfk.gam.train.qmap$model <- c("GAM rCV QMAP") 
msss_nfk.gam.train.qmap$dataset <- c("training: 90%")
msss_nfk.gam.test.qmap <- as.data.frame(msssTestQMAP[,1])
colnames(msss_nfk.gam.test.qmap)[1] <- "msss"
msss_nfk.gam.test.qmap$model <- c("GAM rCV QMAP") 
msss_nfk.gam.test.qmap$dataset <- c("prediction: 10%")
msss_nfk.gamQMAP<- rbind(msss_nfk.gam.train.qmap, msss_nfk.gam.test.qmap)

msss_nfk.gamSb.train <- as.data.frame(msssTrainSB[,1])
colnames(msss_nfk.gamSb.train)[1] <- "msss"
msss_nfk.gamSb.train$model <- c("GAM sbCV") 
msss_nfk.gamSb.train$dataset <- c("training: 90%")
msss_nfk.gamSb.test <- as.data.frame(msssTestSB[,1])
colnames(msss_nfk.gamSb.test)[1] <- "msss"
msss_nfk.gamSb.test$model <- c("GAM sbCV") 
msss_nfk.gamSb.test$dataset <- c("prediction: 10%")
msss_nfk.gamSb <- rbind(msss_nfk.gamSb.train, msss_nfk.gamSb.test)

msss_nfk.rf.train <- as.data.frame(msssTrain.rf[,1])
colnames(msss_nfk.rf.train)[1] <- "msss"
msss_nfk.rf.train$model <- c("RF rCV") 
msss_nfk.rf.train$dataset <- c("training: 90%")
msss_nfk.rf.test <- as.data.frame(msssTest.rf[,1])
colnames(msss_nfk.rf.test)[1] <- "msss"
msss_nfk.rf.test$model <- c("RF rCV") 
msss_nfk.rf.test$dataset <- c("prediction: 10%")
msss_nfk.rf <- rbind(msss_nfk.rf.train, msss_nfk.rf.test)

#combinde datasets
msss_nfk <- rbind(msss_nfk.gam, msss_nfk.gamSb, msss_nfk.rf)
#line break in labels
msss_nfk$model  <- sub(' ', '\n', msss_nfk$model)



#set lattice 
lw <- list(left.padding = list(x = 0.1, units = "inches"))
lw$right.padding <- list(x = -0.1, units = "inches")
lh <- list(bottom.padding = list(x = -0.1, units = "inches"))
lh$top.padding <- list(x = -0.2, units = "inches")

#plot
setwd("xxxx")
jpeg(file="fif_5.jpg", width=800, height=600, pointsize=15,res=100)
lattice.options(layout.widths = lw, layout.heights = lh)
bwplot(msss~model|dataset, data=msss_nfk, 
       scales=list(x=list(cex=1.5), y=list(cex=1.5)),
       ylim=c(0.3,0.7),
       ylab= list(label=expression(paste("SS"["mse"])), cex=1.5),
       par.settings=list(superpose.line=list(col=c('black'), lwd=3, lty=c(1), cex=1.5), layout.heights=list(strip=2), 
                         plot.symbol=list(col=rep(c("black"),5)),
                         box.rectangle= list(fill=rep(c("white"),5),col=rep(c("black"),5)),
                         box.umbrella= list(col=rep(c("black"),5)),
                         box.dot= list(col=rep(c("black"),5))),
       strip=strip.custom(bg="white",par.strip.text=list(cex=1.8))
)
dev.off()
