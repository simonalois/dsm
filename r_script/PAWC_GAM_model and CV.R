#model for PAWC GAM

#function for calculation error assessment
rmse <- function(pred, obs){
  round(sqrt(mean((pred - obs)^2)),2)
}
msss <- function(pred, obs){
  round(1-(sum((obs - pred)^2) / sum((obs - (1/length(obs) * sum(obs)))^2)),2)
}
mae <- function(pred, obs){
  round(mean(abs(obs - pred), na.rm=TRUE),2)
}

R2Pears <- function(pred, obs){
  round(cor(pred, obs, use="pairwise.complete.obs", method="pearson")^2,3)
}

#load library
library(mgcv)
library(splines)
library(rgdal)
library(raster)
library(PerformanceAnalytics)
library(MASS)
library(Metrics)

###gam1 model with significant proxydata & adapted knotes (k) 
gam1 <- gam(nfk_sqrt ~ s(R_SubstGenValueMix, k=8) +s(R_SS_Mix, k=5) +s(R_Karbonat_Mix, k=8)
            +s(dgm5, k=8)+s(slope_deg, k=4) +s(curv_prof, k=3)
            +s(sqrt_valde, k=3)
            +s(sqrt_swi, k=3)
            +s(raster_5_x, raster_5_y, k=100),
            data=data_proxy, family=gaussian, gamma=1)

gam.check(gam1)
summary(gam1)
plot(gam1)

rmse(gam1$fitted.values, data_proxy$nfk_sqrt)
msss(gam1$fitted.values, data_proxy$nfk_sqrt)
mae(gam1$fitted.values, data_proxy$nfk_sqrt)
R2Pears(gam1$fitted.values, data_proxy$nfk_sqrt)

#backtransformation of predicted values
pred_gam <- as.data.frame(predict.gam(gam1, data_proxy, se.fit=TRUE, type="response"))
pred_gam$obs <- data_proxy$nfk
pred_gam$pred <- (gam1$fitted.values^2)-(var(residuals.gam(gam1))^2)+var(gam1$fitted.values)


### model validation #############################################################################################################
# validation with random data splitting n=1:10  

library(qmap)
data_proxy$Weight <- c(1)
  
msssTrain <- matrix(nrow=10, ncol=1)
msssTest <- matrix(nrow=10, ncol=1)
#msssTrain.50 <- matrix(nrow=10, ncol=1)
#msssTest.50 <- matrix(nrow=10, ncol=1)
  
#msssTrainQMAP <- matrix(nrow=10, ncol=1)
#msssTestQMAP <- matrix(nrow=10, ncol=1)
#msssTrainQMAP.50 <- matrix(nrow=10, ncol=1)
#msssTestQMAP.50 <- matrix(nrow=10, ncol=1)
  
RsquTrain <- matrix(nrow=10, ncol=1)
RsquTest <- matrix(nrow=10, ncol=1)

for(i in 1:10){
    w <- as.logical(rbinom(nrow(data_proxy), size = 1, prob = 9/10))
    Weights <- data_proxy$Weight
    Weights[!w] <- 0
    
    gam.cro.va <- gam(nfk_sqrt ~ s(R_SubstGenValueMix, k=8) +s(R_SS_Mix, k=5) +s(R_Karbonat_Mix, k=8)
                      +s(dgm5, k=8)+s(slope_deg, k=4) +s(curv_prof, k=3)
                      +s(sqrt_valde, k=3)
                      +s(sqrt_swi, k=3)
                      +s(raster_5_x, raster_5_y, k=100),
                      data=data_proxy[w==1,], family=gaussian, gamma=1, control=ctrl)
    
    # fill vector
    msssTrain[i,1] <-  msss(predict(gam.cro.va, data_proxy[w==1,],cluster=cl),data_proxy$nfk_sqrt[w==1]) 
    msssTest[i,1]  <-  msss(predict(gam.cro.va, data_proxy[w==0,], cluster=cl), data_proxy$nfk_sqrt[w==0])  
    
    RsquTrain[i,1] <- R2Pears(predict(gam.cro.va, data_proxy[w==1,],cluster=cl), data_proxy$nfk_sqrt[w==1]) 
    RsquTest[i,1]  <- R2Pears(predict(gam.cro.va, data_proxy[w==0,], cluster=cl), data_proxy$nfk_sqrt[w==0])
    
    #gam.cro.va.qm.fit <- fitQmap(obs=data_proxy$nfk_sqrt[w==1], mod=predict(gam.cro.va, data_proxy[w==1,], cluster=cl)) 
    #gam.cro.va.qm.pred_Train<- doQmap(predict(gam.cro.va, data_proxy[w==1,], cluster=cl), gam.cro.va.qm.fit) 
    #gam.cro.va.qm.pred_Test<- doQmap(predict(gam.cro.va, data_proxy[w==0,], cluster=cl), gam.cro.va.qm.fit) 
    
    #msssTrainQMAP[i,1] <-  msss(gam.cro.va.qm.pred_Train, data_proxy$nfk_sqrt[w==1]) 
    #msssTestQMAP[i,1]  <-  msss(gam.cro.va.qm.pred_Test, data_proxy$nfk_sqrt[w==0])  
    
    #data_proxy.w1.50 <- subset(data_proxy[w==1,], nfk < 50)
    #data_proxy.w0.50 <- subset(data_proxy[w==0,], nfk < 50)
    
    #msssTrain.50[i,1] <-  msss(predict(gam.cro.va, data_proxy.w1.50,cluster=cl), data_proxy.w1.50$nfk_sqrt) 
    #msssTest.50[i,1]  <-  msss(predict(gam.cro.va, data_proxy.w0.50, cluster=cl), data_proxy.w0.50$nfk_sqrt)
    
    #gam.cro.va.qm.pred_Train.50<- doQmap(predict(gam.cro.va, data_proxy.w1.50, cluster=cl), gam.cro.va.qm.fit) 
    #gam.cro.va.qm.pred_Test.50<- doQmap(predict(gam.cro.va, data_proxy.w0.50, cluster=cl), gam.cro.va.qm.fit) 
    #msssTrainQMAP.50[i,1] <-  msss(gam.cro.va.qm.pred_Train.50, data_proxy.w1.50$nfk_sqrt) 
    #msssTestQMAP.50[i,1]  <-  msss(gam.cro.va.qm.pred_Test.50, data_proxy.w0.50$nfk_sqrt)  
}  
  
# return vector
  mean(msssTest)
  mean(msssTrain)
  
# validation with random data splitting n=1:10  
#subset pkt.shp by data.proxy.sub
pkt.shp <- pkt.shp[(pkt.shp$AufID %in% data_proxy$AufID),]
#create spatial blocks into 10 folds
library(blockCV)
sb <- spatialBlock(speciesData = pkt.shp, species = NULL, 
                       theRange = 10000, #size of the blocks in m
                       selection="random",
                       k=100
)
#extract folds
folds <- sb$folds
  
#create vectors to be fild by model runs
RsquTrainSB <- matrix(nrow=10, ncol=1)
RsquTestSB <- matrix(nrow=10, ncol=1)
msssTrainSB <- matrix(nrow=10, ncol=1)
msssTestSB <- matrix(nrow=10, ncol=1)
    
#model runs
for(i in 1:100){
      trainSet <- data.frame(nr=unlist(folds[[i]][1]), w=1)
      testSet <-  data.frame(nr=unlist(folds[[i]][2]), w=0)
      spBlock <- rbind(trainSet, testSet)
      spBlock <- spBlock[order(spBlock$nr),]
      w <- spBlock$w
      
      gam.cro.va <- gam(nfk_sqrt ~ s(R_SubstGenValueMix, k=8) +s(R_SS_Mix, k=5) +s(R_Karbonat_Mix, k=8)
                        +s(dgm5, k=8)+s(slope_deg, k=4) +s(curv_prof, k=3)
                        +s(sqrt_valde, k=3)
                        +s(sqrt_swi, k=3)
                        +s(raster_5_x, raster_5_y, k=100),
                        data=data_proxy[w==1,], family=gaussian, gamma=1, control=ctrl)
      
    # fill vector
      RsquTrainSB[i,1] <- R2Pears(predict(gam.cro.va, data_proxy[w==1,],cluster=cl), data_proxy$nfk_sqrt[w==1]) 
      RsquTestSB[i,1]  <- R2Pears(predict(gam.cro.va, data_proxy[w==0,], cluster=cl), data_proxy$nfk_sqrt[w==0])
      msssTrainSB[i,1] <-  msss(predict(gam.cro.va, data_proxy[w==1,],cluster=cl),data_proxy$nfk_sqrt[w==1]) 
      msssTestSB[i,1]  <-  msss(predict(gam.cro.va, data_proxy[w==0,], cluster=cl), data_proxy$nfk_sqrt[w==0])                                                                
}

