## Limpar Memória
rm(list=ls(all=TRUE))

## Set working directory
setwd("~/Doc/Wind")

## Libraries
library(extrafont)
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)
library(Metrics)
library(mlbench)
library(caret)
library(caretEnsemble)
library(scmamp)
library(PMCMR)

load("original.RData")

# Example of Stacking algorithms
# create submodels
seed <- 7

control <- trainControl(method="cv", 
                        number=5, 
                        savePredictions='final',
                        verboseIter = FALSE)

algorithmList <- c('xgbLinear', 'mlpML', 'knn', 'svmLinear2')
                   # 'svmLinear2', 
                   # 'cubist')
                   # , 
                   # 'bridge')

set.seed(seed)

models <- caretList(Power~., data=treino, trControl=control,
                    preProcess = c("BoxCox"), 
                    methodList=algorithmList,
                    tuneLength = 5)
# results <- resamples(models)
# summary(results)
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# dotplot(results, scales=scales)

# Stack 
# stackControl <- trainControl(method = "cv", number = 5, 
#                              savePredictions = 'final')

stack <- caretStack(models, method = "svmRadialSigma", trControl = control)

# PREDICTIONS
  # TREINO E TESTE
pred_xgbLinear         <- predict.train(models$xgbLinear, newdata = x_teste)
pred_xgbLinear_treino  <- predict.train(models$xgbLinear, newdata = x_treino)

pred_mlpML        <- predict.train(models$mlpML, newdata = x_teste)
pred_mlpML_treino <- predict.train(models$mlpML, newdata = x_treino)

pred_knn        <- predict.train(models$knn, newdata = x_teste)
pred_knn_treino <- predict.train(models$knn, newdata = x_treino)

# pred_svmRadialSigma        <- predict.train(models$svmRadialSigma, newdata = x_teste)
# pred_svmRadialSigma_treino <- predict.train(models$svmRadialSigma, newdata = x_treino)

pred_svmLinear2        <- predict.train(models$svmLinear2, newdata = x_teste)
pred_svmLinear2_treino <- predict.train(models$svmLinear2, newdata = x_treino)

# pred_cubist        <- predict.train(models$cubist, newdata = x_teste)
# pred_cubist_treino <- predict.train(models$cubist, newdata = x_treino)

# pred_bridge        <- predict.train(models$bridge, newdata = x_teste)
# pred_bridge_treino <- predict.train(models$bridge, newdata = x_treino)

pred_stack      <- predict(stack, newdata = x_teste)
pred_stack_treino <- predict(stack, newdata = x_treino)

# METRICAS
# TESTE
pred_RMSE <- data.frame(stack = RMSE(pred_stack,y_teste),
                        xgbLinear = RMSE(pred_xgbLinear,y_teste),
                        mlpML = RMSE(pred_mlpML,y_teste),
                        knn = RMSE(pred_knn,y_teste),
                        svmLinear2 = RMSE(pred_svmLinear2,y_teste))

pred_RRMSE <- data.frame(stack = RMSE(pred_stack,y_teste)/mean(y_teste),
                        xgbLinear = RMSE(pred_xgbLinear,y_teste)/mean(y_teste),
                        mlpML = RMSE(pred_mlpML,y_teste)/mean(y_teste),
                        knn = RMSE(pred_knn,y_teste)/mean(y_teste),
                        svmLinear2 = RMSE(pred_svmLinear2,y_teste)/mean(y_teste))

pred_MAPE <- data.frame(stack = mape(pred_stack,y_teste),
                        xgbLinear = mape(pred_xgbLinear,y_teste),
                        mlpML = mape(pred_mlpML,y_teste),
                        knn = mape(pred_knn,y_teste),
                        svmLinear2 = mape(pred_svmLinear2,y_teste))

pred_Rsquared <- data.frame(stack = cor(pred_stack,y_teste)^2,
                            xgbLinear = cor(pred_xgbLinear,y_teste)^2,
                            mlpML = cor(pred_mlpML,y_teste)^2,
                            knn = cor(pred_knn,y_teste)^2,
                            svmLinear2 = cor(pred_svmLinear2,y_teste)^2)


# TREINO
pred_RMSE_treino <- data.frame(stack = RMSE(pred_stack_treino,y_treino),
                        xgbLinear = RMSE(pred_xgbLinear_treino,y_treino),
                        mlpML = RMSE(pred_mlpML_treino,y_treino),
                        knn = RMSE(pred_knn_treino,y_treino),
                        svmLinear2 = RMSE(pred_svmLinear2_treino,y_treino))


pred_RRMSE_treino <- data.frame(stack = RMSE(pred_stack_treino,y_treino)/mean(y_treino),
                         xgbLinear = RMSE(pred_xgbLinear_treino,y_treino)/mean(y_treino),
                         mlpML = RMSE(pred_mlpML_treino,y_treino)/mean(y_treino),
                         knn = RMSE(pred_knn_treino,y_treino)/mean(y_treino),
                         svmLinear2 = RMSE(pred_svmLinear2_treino,y_treino)/mean(y_treino))


pred_MAPE_treino <- data.frame(stack = mape(pred_stack_treino,y_treino),
                        xgbLinear = mape(pred_xgbLinear_treino,y_treino),
                        mlpML = mape(pred_mlpML_treino,y_treino),
                        knn = mape(pred_knn_treino,y_treino),
                        svmLinear2 = mape(pred_svmLinear2_treino,y_treino))

pred_Rsquared_treino <- data.frame(stack = cor(pred_stack_treino,y_treino)^2,
                            xgbLinear = cor(pred_xgbLinear_treino,y_treino)^2,
                            mlpML = cor(pred_mlpML_treino,y_treino)^2,
                            knn = cor(pred_knn_treino,y_treino)^2,
                            svmLinear2 = cor(pred_svmLinear2_treino,y_treino)^2)

Metricas <- rbind(pred_RRMSE_treino,
                  pred_MAPE_treino,
                  pred_Rsquared_treino,
                  pred_RRMSE,
                  pred_MAPE,
                  pred_Rsquared)
row.names(Metricas) <- c("RRMSE_treino","MAPE_treino","RSquared_treino",
                         "RRMSE","MAPE","RSquared")


t(Metricas)


save.image("Resultados2.RData")


################## plot #################

PO<-function(Obs,Pred)
{
  data<-data.frame(as.vector(unlist(data.frame(Obs,Pred))),
                   rep(c("y(t) - Estimated","y(t)-Predicted"),each=dim(Obs)[1]),
                   rep(seq(1,dim(Obs)[1]),times=2))
                   # rep(c(4200:4300),times=2))
  colnames(data)<-c("Predictions","Legend","Frequency")

  g2 <- ggplot(data, aes(Frequency, Predictions,  colour=Legend))+ylab("Power (KW)")+xlab("Samples (10 minutes)")+ggtitle("")
  g2 <- g2 + geom_line(size=0.6)+ theme_bw(base_size = 18) 
  # +
  #   theme(legend.position = "none", 
  #         axis.text=element_text(size=18),
  #         legend.text=element_text(size=rel(1)),
  #         plot.title = element_text(hjust=0.5),
  #         text=element_text(family="Times New Roman"),
  #         axis.title=element_text(size=20))
  # g2 <- g2 + geom_point()
  g2 <- g2 + geom_label(x=dim(Obs)[1]*0.1, y=min(Obs),label="Training set",show.legend = FALSE, size = 6)
  g2 <- g2 + geom_label(x=dim(Obs)[1]*0.8, y=min(Obs),label="Test set",show.legend = FALSE, size = 6)
  g2 <- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 1, color ="blue")
  g2 <- g2 + scale_color_manual(values=c("#000000","#999999"),labels = c("Predicted","Observed"))
  g2 <- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))
  print(g2)
  
  # ggsave(
  #   file.path("Plot", paste("predicao.png")),
  #   g2,
  #   width = 10,
  #   height = 5,
  #   dpi = 1200)
  
}

Obs             <- data.frame(c(y_treino,y_teste))
Pred_stack      <- data.frame(c(pred_stack_treino,pred_stack))
Pred_xgbLinear  <- data.frame(c(pred_xgbLinear_treino,pred_xgbLinear))
Pred_mlpML      <- data.frame(c(pred_mlpML_treino,pred_mlpML))
Pred_knn        <- data.frame(c(pred_knn_treino,pred_knn))
Pred_svmLinear2 <- data.frame(c(pred_svmLinear2_treino,pred_svmLinear2))
# Pred_cubist     <- data.frame(c(pred_cubist_treino,pred_cubist))
# Pred_bridge     <- data.frame(c(pred_bridge_treino,pred_bridge))

{x11()
  PO1(Obs[4200:4300,],
      Pred_xgbLinear[4200:4300,],Pred_mlpML[4200:4300,],
      Pred_knn[4200:4300,],Pred_svmLinear2[4200:4300,],
      'xgBoost', 'MLP', 'K-NN', 'SVR-Linear')
}

PO(Obs, Pred_mlpML)



 
# violin<-data.frame(c(se[,1],se[,2],se[,3],se[,4],se[,5],se[,6],se[,7])
#                    ,Legenda=rep(c("stack","rf","gbm","knn","svmLinear2","brnn","bridge"), each=(dim(se)[1])))
# colnames(violin)<-c("PAE","Model")
# 
# x11()
# ggplot(violin, aes(x=Model, y=PAE))+
#   # geom_violin(trim=FALSE)+
#   scale_y_log10()+
#   geom_boxplot(width = 0.2)+  theme_bw(base_size = 20)+
#   stat_summary(fun.y=mean, geom="point", size=2, color="black")+
#   xlab("Models") + ylab("Absolute Percetual Error (APE)")+
#   scale_x_discrete(labels=c("XGB","STACK","GBM","MLP","RF","SVR","KNN"))+
#   #ggtitle("Quadratic error of the models used in forecasting soybean price") +
#   theme_bw(base_size = 18)+
#   theme(legend.position = "bottom", legend.direction = "horizontal") + 
#   theme(axis.text=element_text(size=18),
#         legend.text=element_text(size=rel(1)),
#         plot.title = element_text(hjust=0.5),
#         text=element_text(family="Times New Roman"),
#         axis.title=element_text(size=20))


################# Estatística #####################

se <- c((pred_stack - (y_teste))^2,
                 (pred_svmLinear2 - (y_teste))^2,
                 (pred_knn - (y_teste))^2,
                 (pred_mlpML - (y_teste))^2,
                 (pred_xgbLinear - (y_teste))^2)

erro <- data.frame((pred_stack - (y_teste)),
        (pred_svmLinear2 - (y_teste)),
        (pred_knn - (y_teste)),
        (pred_mlpML - (y_teste)),
        (pred_xgbLinear - (y_teste)))

se.matrix <- matrix(se, byrow = FALSE, ncol = 5)

colnames(se.matrix) <- c("STACK","SVR-Linear","K-NN","MLP","xgBoost")

friedman <- friedman.test(se.matrix)

CDposthoc <- posthoc.friedman.nemenyi.test(se.matrix)

plotCD(se.matrix, alpha = 0.05, cex = 1.25, decreasing = FALSE)

se.plot <- sort(apply(se.matrix,2,sum))

se.plot <- data.frame(se.plot)

se.plot <- data.frame(se.plot, 
                      c("M1","M2","M3","M4","M5"))

colnames(se.plot) <- c("SSE","Models")

{
  x11()
  ggplot(se.plot, aes(Models, SSE, fill = Models)) + 
    geom_bar(stat = "identity") +
    scale_x_discrete(label=(c("STACK","SVR-Linear",
                              "xgBoost","K-NN","MLP"))) +
    scale_fill_brewer(palette = "Blues") +
    theme_bw(base_size = 22) +
    theme(legend.position = "none")
}                    
