## Limpar memória
rm(list=ls(all=TRUE))

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
library(forecast)

## Carregar dados

dataset <- read_excel("Dataset.xlsx", 
                      col_types = c("date", "numeric", "numeric"))

dataset <- dataset[,2]

## Adicionar lags

lag <- 5

x5 <- dataset[1:(dim(dataset)[1]-lag),1]
x4 <- dataset[(lag-3):(dim(dataset)[1]-lag+1),1]
x3 <- dataset[(lag-2):(dim(dataset)[1]-lag+2),1]
x2 <- dataset[(lag-1):(dim(dataset)[1]-lag+3),1]
x1 <- dataset[(lag):(dim(dataset)[1]-lag+4),1]
y  <- dataset[(lag+1):(dim(dataset)[1]),1]

data <- cbind(y,x1,x2,x3,x4,x5)
names(data) <- c("y","x1","x2","x3","x4","x5")

## Dividir treino e teste

n <- dim(data)[1]
cut <- 0.7 * n

treino <- data[1:cut,]
teste <- tail(data,n-cut)

x_treino <- treino[,-1]
y_treino <- treino[,1]

x_teste <- teste[,-1]
y_teste <- teste[,1]

## Treinamento

set.seed(7)

control <- trainControl(method = "timeslice",
                        initialWindow = 0.7*dim(treino)[1],
                        horizon = 1,
                        fixedWindow = FALSE,
                        allowParallel = TRUE,
                        savePredictions = 'final')

model <- train(y~., data = treino,
               method = "earth",
               trControl = control,
               preProcess = c("center","scale"),
               tuneLength = 4)

## Predicao

pred.treino <- predict(model,treino)
pred.teste  <- predict(model,teste)
predicao    <- data.frame(c(pred.treino,pred.teste))

## Metricas

pred.RMSE  <- RMSE(pred.teste, teste[,1])
pred.RRMSE <- RMSE(pred.teste, teste[,1])/mean(pred.teste)
pred.MAPE  <- mape(pred.teste, teste[,1])
pred.R2    <- cor(pred.teste, teste[,1])^2

pred.RMSE.treino  <- RMSE(pred.treino, treino[,1])
pred.RRMSE.treino <- RMSE(pred.treino, treino[,1])/mean(pred.treino)
pred.MAPE.treino  <- mape(pred.treino, treino[,1])
pred.R2.treino    <- cor(pred.treino, treino[,1])^2

Metricas   <- data.frame(cbind(pred.RMSE,pred.RRMSE,pred.MAPE,pred.R2))
Metricas.treino   <- data.frame(cbind(pred.RMSE.treino ,
                                      pred.RRMSE.treino ,
                                      pred.MAPE.treino ,
                                      pred.R2.treino ))


colnames(Metricas) <- c("RMSE","RRMSE","MAPE","R2")
colnames(Metricas.treino) <- c("RMSE","RRMSE","MAPE","R2")

se <- data.frame((pred.teste-y_teste)^2)
se.treino <- data.frame((pred.treino-y_treino)^2)

sse <- apply(se,2,sum)
sse.treino <- apply(se.treino,2,sum)
