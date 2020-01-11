## Biblioteca
library(readxl)
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)
library(caretEnsemble)

## Carregar os dados

dataset <- read_excel("Dados petróleo e GN.xlsx")
names(dataset) <- c("Month","Oil","Gas")
dataset$Month <- NULL
dataset$Oil <- NULL

## Tratamento dos dados (treino e teste)

lag <- 5

x5 <- dataset[1:(dim(dataset)[1]-lag),1]
x4 <- dataset[(lag-3):(dim(dataset)[1]-lag+1),1]
x3 <- dataset[(lag-2):(dim(dataset)[1]-lag+2),1]
x2 <- dataset[(lag-1):(dim(dataset)[1]-lag+3),1]
x1 <- dataset[(lag):(dim(dataset)[1]-lag+4),1]
y  <- dataset[(lag+1):(dim(dataset)[1]),1]


data <- cbind(y,x1,x2,x3,x4,x5)
names(data) <- c("y","x1","x2","x3","x4","x5")

newdata <- FE(as.data.frame(data$x1))

data <- cbind(data,(newdata[[1]]))

n <- dim(data)[1]
cut <- 0.7 * n

treino <- data[1:cut,]
teste <- tail(data,n-cut)

x_treino <- treino[,-1]
y_treino <- treino[,1]

x_teste <- teste[,-1]
y_teste <- teste[,1]

## Treino

seed <- 7

control <- trainControl(method = "cv", 
                        number = 5,
                        savePredictions = 'final')

algorithmList <- c('knn','svmLinear2','rf','xgbLinear','mlp')

set.seed(seed)

models <- caretList(y~., data=treino, trControl=control,
                    preProcess = c("center","scale"), 
                    methodList = algorithmList,
                    tuneLength = 5)

# results <- resamples(models)
# 
# summary(results)

# scales <- list(x=list(relation="free"), y=list(relation="free"))
# 
# dotplot(results, scales=scales)

## Predição

pred_knn        <- predict(models$knn, teste)
pred_knn_treino <- predict(models$knn, treino)

pred_svmLinear2        <- predict(models$svmLinear2, teste)
pred_svmLinear2_treino <- predict(models$svmLinear2, treino)

pred_rf         <- predict(models$rf, teste)
pred_rf_treino  <- predict(models$rf, treino)

pred_xgbLinear        <- predict(models$xgbLinear, teste)
pred_xgbLinear_treino <- predict(models$xgbLinear, treino)

pred_mlp         <- predict(models$mlp, teste)
pred_mlp_treino  <- predict(models$mlp, treino)


pred_RMSE <- data.frame(knn = RMSE(pred_knn,y_teste),
                        svmLinear2 = RMSE(pred_svmLinear2,y_teste),
                        rf = RMSE(pred_rf,y_teste),
                        xgbLinear = RMSE(pred_xgbLinear,y_teste),
                        mlp = RMSE(pred_mlp,y_teste))

pred_MAPE <- data.frame(knn = mape(pred_knn,y_teste),
                        svmLinear2 = mape(pred_svmLinear2,y_teste),
                        rf = mape(pred_rf,y_teste),
                        xgbLinear = mape(pred_xgbLinear,y_teste),
                        mlp = mape(pred_mlp,y_teste))

pred_Rsquared <- data.frame(knn = cor(pred_knn,y_teste)^2,
                            svmLinear2 = cor(pred_svmLinear2,y_teste)^2,
                            rf = cor(pred_rf,y_teste)^2,
                            xgbLinear = cor(pred_xgbLinear,y_teste)^2,
                            mlp = cor(pred_mlp,y_teste)^2)

pred_RMSE
pred_MAPE
pred_Rsquared


##################plot#################

PO<-function(Obs,Pred,Title)
{
  data<-data.frame(as.vector(unlist(data.frame(Obs,Pred))),
                   rep(c("y(t) - Estimated","y(t) - Predicted"),each=dim(Obs)[1]),
                   rep(seq(1,dim(Obs)[1]),times=2))
  colnames(data)<-c("Predictions","Legend","Frequency")
  
  g2<- ggplot(data, aes(Frequency, Predictions, colour=Legend))+ylab("Value")+xlab("Month")+ggtitle(Title)
  g2<- g2 + geom_line(size=0.75)+theme_bw(base_size = 18)
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.1, y=max(Obs),label="Training set",show.legend = FALSE, color = "#000000")
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.8, y=max(Obs),label="Test set",show.legend = FALSE, color = "#000000")
  g2<- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 0.8)
  g2<- g2 + scale_color_manual(values=c("#999999","#000000"),labels = expression(italic(y(t)),italic(hat(y)(t))))
  g2<- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))
  print(g2)
}


Obs <- data.frame(y)
Pred1 <- data.frame(c(pred_svmLinear2_treino,pred_svmLinear2))
Pred2 <- data.frame(c(pred_knn_treino,pred_knn))
Pred3 <- data.frame(c(pred_rf_treino,pred_rf))
Pred4 <- data.frame(c(pred_xgbLinear_treino,pred_xgbLinear))
Pred5 <- data.frame(c(pred_mlp_treino,pred_mlp)) 

{x11()
  PO(Obs,Pred1,"Gas Production")
}

{x11()
  PO1(Obs,Pred1,Pred2,Pred3,Pred4,Pred5,"Oil Production")
}

