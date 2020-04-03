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
library(raster)
library(caret)
library(CAST)
library(doParallel)
library(pdp)
library(quantreg)


#define predictors
#all
predictors <- c("R_SubstGenValueMix", "R_TM_Mix", "R_BS_Mix","R_SS_Mix","R_Karbonat_Mix","dgm5","aspect_con","slope_deg","curv_plan","curv_prof","norm_hight", 
                "slope_leng","sqrt_valde","vert_dist","convergence","sqrt_tpi","sqrt_swi","ndvi","raster_5_x","raster_5_y","temp","prec","lh_nh","glo_rad")
#final
predictors <- c("R_SubstGenValueMix", "R_Karbonat_Mix","curv_plan", "slope_deg",
                "sqrt_swi", "sqrt_valde", "raster_5_x", "raster_5_y", "temp", "prec", "glo_rad")
#define response variable
response <- "nfk_sqrt"

cl <- makeCluster(4)
registerDoParallel(cl)

mod.nfk_sqrt.rf <- train(data_proxy[,predictors], data_proxy$nfk_sqrt, method="rf", metric="Rsquared", keep.forest=TRUE, 
                         trControl=trainControl(method="oob"), importance=TRUE, verbose=TRUE)


ffs_mod.nfk <-ffs(data_proxy[,predictors], data_proxy$nfk_sqrt, method="rf", metric="Rsquared",
                  trControl=trainControl(method="oob"), importance=TRUE, tuneLength = 2, verbose = FALSE)

#backward variable selection with caret rfe
control <- caret::rfeControl( functions =  rfFuncs, method = "cv", verbose=FALSE)
bfs <- caret::rfe(data_proxy[,predictors],data_proxy$nfk_sqrt, rfeControl=control)

stopCluster(cl)
closeAllConnections()

rmse(mod.nfk_sqrt.rf$finalModel$predicted, data_proxy$nfk_sqrt)
msss(mod.nfk_sqrt.rf$finalModel$predicted, data_proxy$nfk_sqrt)
mae(mod.nfk_sqrt.rf$finalModel$predicted, data_proxy$nfk_sqrt)
R2Pears(mod.nfk_sqrt.rf$finalModel$predicted, data_proxy$nfk_sqrt)

### model validation #############################################################################################################
# validation with random data splitting n=1:10  

msssTrain.rf <- matrix(nrow=10, ncol=1)
msssTest.rf <- matrix(nrow=10, ncol=1)
msssTrain.rf.50 <- matrix(nrow=10, ncol=1)
msssTest.rf.50 <- matrix(nrow=10, ncol=1)
  
#msssTrain.rf.QMAP <- matrix(nrow=10, ncol=1)
#msssTest.rf.QMAP <- matrix(nrow=10, ncol=1)
#msssTrain.rf.QMAP.50 <- matrix(nrow=10, ncol=1)
#msssTest.rf.QMAP.50 <- matrix(nrow=10, ncol=1)
  
RsquTrain.rf <- matrix(nrow=10, ncol=1)
RsquTest.rf <- matrix(nrow=10, ncol=1)

cl <- makeCluster(4)
registerDoParallel(cl)
  
for(i in 1:10){
    w <- as.logical(rbinom(nrow(data_proxy), size = 1, prob = 9/10))
    w <- as.numeric(w)
    
    data_proxy.w1 <- data_proxy[w==1,]
    data_proxy.w0 <- data_proxy[w==0,]
    
    rf.cro.va <- train(data_proxy.w1[,predictors], data_proxy.w1$nfk_sqrt, method="rf", metric="Rsquared",
                       trControl=trainControl(method="oob"), importance=TRUE, verbose=TRUE)
    
    # fill vector
    msssTrain.rf[i,1] <-  msss(rf.cro.va$finalModel$predicted, data_proxy.w1$nfk_sqrt) 
    msssTest.rf[i,1]  <-  msss(caret::predict.train(rf.cro.va, data_proxy.w0, cluster=cl), data_proxy.w0$nfk_sqrt)  
    
    RsquTrain.rf[i,1] <-  R2Pears(rf.cro.va$finalModel$predicted, data_proxy.w1$nfk_sqrt) 
    RsquTest.rf[i,1]  <-  R2Pers(caret::predict.train(rf.cro.va, data_proxy.w0, cluster=cl), data_proxy.w0$nfk_sqrt)  
    
    
    # rf.cro.va.qm.fit <- fitQmap(obs=data_proxy.w1$nfk_sqrt, mod=rf.cro.va$finalModel$predicted) 
    # rf.cro.va.qm.pred_Train<- doQmap(rf.cro.va$finalModel$predicted, rf.cro.va.qm.fit) 
    # rf.cro.va.qm.pred_Test<- doQmap(caret::predict.train(rf.cro.va, data_proxy.w0, cluster=cl), rf.cro.va.qm.fit) 
    # 
    # msssTrain.rf.QMAP[i,1] <-  msss(rf.cro.va.qm.pred_Train, data_proxy.w1$nfk_sqrt) 
    # msssTest.rf.QMAP[i,1]  <-  msss(rf.cro.va.qm.pred_Test, data_proxy.w0$nfk_sqrt)  
    # 
    # data_proxy.w1.50 <- subset(data_proxy.w1, nfk < 50)
    # data_proxy.w0.50 <- subset(data_proxy.w0, nfk < 50)
    # 
    # msssuTrain.rf.50[i,1] <-  msss(caret::predict.train(rf.cro.va, data_proxy.w1.50, cluster=cl), data_proxy.w1.50$nfk_sqrt) 
    # msssTest.rf.50[i,1]  <-  msss(caret::predict.train(rf.cro.va, data_proxy.w0.50, cluster=cl), data_proxy.w0.50$nfk_sqrt)
    # 
    # rf.cro.va.qm.pred_Train.50<- doQmap(caret::predict.train(rf.cro.va, data_proxy.w1.50, cluster=cl), rf.cro.va.qm.fit) 
    # rf.cro.va.qm.pred_Test.50<- doQmap(caret::predict.train(rf.cro.va, data_proxy.w0.50, cluster=cl), rf.cro.va.qm.fit) 
    # msssTrain.rf.QMAP.50[i,1] <-  msss(rf.cro.va.qm.pred_Train.50, data_proxy.w1.50$nfk_sqrt) 
    # msssTest.rf.QMAP.50[i,1]  <-  msss(rf.cro.va.qm.pred_Test.50, data_proxy.w0.50$nfk_sqrt) 
}

stopCluster(cl)
closeAllConnections()




