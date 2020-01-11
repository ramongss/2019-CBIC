## Set working directory
setwd("~/Doc/Wind")

## Libraries
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)
library(caretEnsemble)

########################Machine Learning#######################

load("data.RData")

lag <- 1

x <- data_wind_final[1:(dim(data_wind_final)[1]-lag),-1]
y <- data_wind_final[(lag+1):dim(data_wind_final)[1],1]

data <- cbind(y,x)


n <- dim(data)[1]
cut <- 0.7 * n

treino <- data[1:cut,]
teste <- tail(data,n-cut)

x_treino <- treino[,-1]
y_treino <- treino[,1]

x_teste <- teste[,-1]
y_teste <- teste[,1]


fitControl<- trainControl(method = "timeslice",
                          initialWindow = 362, #70% do conjunto das observações
                          horizon = 51,       #10% do conjunto das observações
                          fixedWindow = TRUE,
                          allowParallel = TRUE,
                          savePredictions="final")

mod <- train(x_treino, 
             y_treino, 
             method = "svmLinear2",
             preProcess = c("corr","center","scale"),
             trControl = fitControl,
             tuneLength = 5)

pred_treino <- predict(mod,x_treino)
pred_teste  <- predict(mod,x_teste)

predicoes <- c(pred_treino,pred_teste)

RMSE_treino <- RMSE(pred_treino,y_treino)
R2_treino   <- cor(pred_treino,y_treino)^2
MAE_treino  <- MAE(pred_treino,y_treino) 
MAPE_treino <- mape(pred_treino,y_treino)

RMSE_teste <- RMSE(pred_teste,y_teste)
R2_teste   <- cor(pred_teste,y_teste)^2
MAE_teste  <- MAE(pred_teste,y_teste)
MAPE_teste <- mape(pred_teste,y_teste)

Metricas <- cbind(RMSE_treino,R2_treino,MAE_treino,MAPE_treino,
                  RMSE_teste,R2_teste,MAE_teste,MAPE_teste)

########################################plot#################

PO<-function(Obs,Pred,Title)
{
  data<-data.frame(as.vector(unlist(data.frame(Obs,Pred))),
                   rep(c("y(t) - Estimated","y(t)-Predicted"),each=dim(Obs)[1]),
                   rep(seq(1,dim(Obs)[1]),times=2))
  colnames(data)<-c("Predictions","Legend","Frequency")
  
  g2<- ggplot(data, aes(Frequency, Predictions, colour=Legend))+ylab("Value")+xlab("Hour")+ggtitle(paste("Individual", Title)) 
  g2<- g2 + geom_line(size=0.8)+theme_bw(base_size = 18)
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.1, y=500,label="Training set",show.legend = FALSE)
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.8, y=500,label="Test set",show.legend = FALSE)
  g2<- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 0.8)
  g2<- g2 + scale_color_manual(values=c("#000000","#999999"),labels = expression(italic(hat(y)(t)),italic(y(t))))
  g2<- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))  
  print(g2)
}

Obs <-data.frame(c(y_treino,y_teste))
Pred<-data.frame(c(pred_treino,pred_teste))
{x11()
PO(Obs,Pred,"One hour power forecasting")
}


