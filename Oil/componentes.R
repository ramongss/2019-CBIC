# models <- c('knn','svmLinear2','rf','xgbLinear','mlp')
# 
# combs <- as.matrix(expand.grid(models,models,models)) 
# 
# comp <- data.frame(c1,c2,c3)
# 
# for (i in 1:3) {
#   for (j in 1:dim(combs)[1]) {
#   treinamento <- train(comp[,i],y_treino,
#                        models = combs[j,i])
#   preds.comp
#   preds <- matrix()
#   }
# }
# # for vai de 1 ate numero de colunas
# # separar cada comp em treino e teste
# # 

## Set working directory
setwd("~/Doc/Oil")

## Libraries
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)
library(caretEnsemble)
library(e1071)
library(readxl)

rm(list=ls(all=TRUE))

## Carregar dados

dataset <- read_excel("Dataset.xlsx", 
                      col_types = c("date", "numeric", "numeric"))

dataset <- ts(dataset[,2],frequency = 12, start = c(2010,1))
dataset <- dataset[,1]

## Separar componentes

dataset.div <- stl(dataset, "per")

# c1 = seasonal
# c2 = trend
# c3 = remainder

c1 <- data.frame(dataset.div$time.series[,1])
  colnames(c1) <- "seasonal"
c2 <- data.frame(dataset.div$time.series[,2])
  colnames(c2) <- "trend"
c3 <- data.frame(dataset.div$time.series[,3])
  colnames(c3) <- "remainder"

## Lags das componentes

lag <- 5

c1.x5 <- c1[1:(dim(c1)[1]-lag),1]
c1.x4 <- c1[(lag-3):(dim(c1)[1]-lag+1),1]
c1.x3 <- c1[(lag-2):(dim(c1)[1]-lag+2),1]
c1.x2 <- c1[(lag-1):(dim(c1)[1]-lag+3),1]
c1.x1 <- c1[(lag):(dim(c1)[1]-lag+4),1]
c1.y  <- c1[(lag+1):(dim(c1)[1]),1]

c1.lag <- cbind(c1.y,c1.x1,c1.x2,c1.x3,c1.x4,c1.x5)
colnames(c1.lag) <- c("y","x1","x2","x3","x4","x5")

c2.x5 <- c2[1:(dim(c2)[1]-lag),1]
c2.x4 <- c2[(lag-3):(dim(c2)[1]-lag+1),1]
c2.x3 <- c2[(lag-2):(dim(c2)[1]-lag+2),1]
c2.x2 <- c2[(lag-1):(dim(c2)[1]-lag+3),1]
c2.x1 <- c2[(lag):(dim(c2)[1]-lag+4),1]
c2.y  <- c2[(lag+1):(dim(c2)[1]),1]

c2.lag <- cbind(c2.y,c2.x1,c2.x2,c2.x3,c2.x4,c2.x5)
colnames(c2.lag) <- c("y","x1","x2","x3","x4","x5")

c3.x5 <- c3[1:(dim(c3)[1]-lag),1]
c3.x4 <- c3[(lag-3):(dim(c3)[1]-lag+1),1]
c3.x3 <- c3[(lag-2):(dim(c3)[1]-lag+2),1]
c3.x2 <- c3[(lag-1):(dim(c3)[1]-lag+3),1]
c3.x1 <- c3[(lag):(dim(c3)[1]-lag+4),1]
c3.y  <- c3[(lag+1):(dim(c3)[1]),1]

c3.lag <- cbind(c3.y,c3.x1,c3.x2,c3.x3,c3.x4,c3.x5)
colnames(c3.lag) <- c("y","x1","x2","x3","x4","x5")

## Separar em treino e teste

n <- dim(c1.lag)[1]
cut <- 0.7 * n

# c1
c1.treino <- c1.lag[1:cut,]
c1.teste <- tail(c1.lag,n-cut)

c1.xtreino <- c1.treino[,-1]
c1.ytreino <- c1.treino[,1]

c1.xteste <- c1.teste[,-1]
c1.yteste <- c1.teste[,1]

# c2
c2.treino <- c2.lag[1:cut,]
c2.teste <- tail(c2.lag,n-cut)

c2.xtreino <- c2.treino[,-1]
c2.ytreino <- c2.treino[,1]

c2.xteste <- c2.teste[,-1]
c2.yteste <- c2.teste[,1]

# c3
c3.treino <- c3.lag[1:cut,]
c3.teste <- tail(c3.lag,n-cut)

c3.xtreino <- c3.treino[,-1]
c3.ytreino <- c3.treino[,1]

c3.xteste <- c3.teste[,-1]
c3.yteste <- c3.teste[,1]

treino <- list(c1.treino,c2.treino,c3.treino)
teste  <- list(c1.teste,c2.teste,c3.teste)

## Treinamento e Predição

set.seed(7)

control <- trainControl(method = "cv", 
                        number = 5,
                        savePredictions = 'final')

model.list <- c('knn','svmLinear2','rf','xgbLinear','mlp')

combs <- as.matrix(expand.grid(model.list,model.list,model.list))

c1.model <- NA
c2.model <- NA
c3.model <- NA
pred.c1.treino <- NA
pred.c1.teste  <- NA
pred.c1        <- NA

pred.c2.treino <- NA
pred.c2.teste  <- NA
pred.c2        <- NA

pred.c3.treino <- NA
pred.c3.teste  <- NA
pred.c3        <- NA

Predicao <- NA

  # Cria barra de progresso
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

for (i in 1:dim(combs)[1]) {
  
  # Treino
  
  c1.model <- train(y~., data = treino[[1]],
                       method = combs[i,1],
                       trControl = control,
                       preProcess = c("center","scale"),
                       tuneLength = 4)
  
  c2.model <- train(y~.,data = treino[[2]],
                       trControl = control,
                       preProcess = c("center","scale"),
                       method = combs[i,2],
                       tuneLength = 4)
  
  c3.model <- train(y~.,data = treino[[3]],
                       trControl = control,
                       preProcess = c("center","scale"),
                       method = combs[i,3],
                       tuneLength = 4)
  
  # Predição
  
  pred.c1.treino <- predict(c1.model,treino[[1]])
  pred.c1.teste  <- predict(c1.model,teste[[1]])
  pred.c1        <- data.frame(c(pred.c1.treino,pred.c1.teste)) 
  
  pred.c2.treino <- predict(c2.model,treino[[2]])
  pred.c2.teste  <- predict(c2.model,teste[[2]])
  pred.c2        <- data.frame(c(pred.c2.treino,pred.c2.teste)) 
    
  pred.c3.treino <- predict(c3.model,treino[[3]])
  pred.c3.teste  <- predict(c3.model,teste[[3]])
  pred.c3        <- data.frame(c(pred.c3.treino,pred.c3.teste))
  
  Predicao[i] <- pred.c1 + pred.c2 + pred.c3
  
  # Atualiza barra de progresso
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i, title=paste( round(i/dim(combs)[1]*100, 0),
                                          "% done"))
}

close(pb)

save.image("Predicoes.RData")

Obs.treino <- treino[[1]] + treino[[2]] + treino[[3]]
Obs.teste  <- teste[[1]] + teste[[2]] + teste[[3]]

# Cria barra de progresso
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

Metricas <- matrix(nrow = dim(combs)[1],ncol = 5)

for (i in 1:dim(combs)[1]){
  
  Predicao.treino <- Predicao[[i]][1:cut]
  Predicao.teste  <- tail(Predicao[[i]],n-cut)
  
  #Metricas
  pred.RMSE  <- RMSE(Predicao.teste, Obs.teste)
  pred.RRMSE <- RMSE(Predicao.teste, Obs.teste)/mean(Predicao.teste)
  pred.MAPE  <- mape(Predicao.teste, Obs.teste)
  pred.R2    <- cor(Predicao.teste, Obs.teste[,1])^2
  
  Metricas[i,] <- c(i,pred.RMSE,pred.RRMSE,pred.MAPE,pred.R2)
  
  # Atualiza barra de progresso
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste( round(i/dim(combs)[1]*100, 0),
                                        "% done"))
}
colnames(Metricas) <- c("i","RMSE","RRMSE","MAPE","R2")


Metricas[which.min(Metricas[,2]),]
Metricas[which.min(Metricas[,3]),]
Metricas[which.min(Metricas[,4]),]
Metricas[which.max(Metricas[,5]),]

save.image("Resultados.RData")

## MODELO 6 É O MELHOR