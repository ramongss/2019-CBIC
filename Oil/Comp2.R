## Limpar memória
rm(list=ls(all=TRUE))

## Set working directory
setwd("~/Doc/Oil")

## Libraries
library(ggplot2)
library(dplyr)
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


dataset <- ts(dataset[,2],frequency = 12, start = c(2010,1)) # Oil data
# dataset <- ts(dataset[,3],frequency = 12, start = c(2010,1)) # Gas data
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

save.image("oil-datalag.RData")
# save.image("gas-datalag.RData")

## Treinamento e Predição

set.seed(7)

control <- trainControl(method = "timeslice",
                        initialWindow = 0.7*dim(treino[[1]])[1],
                        horizon = 1,
                        fixedWindow = FALSE,
                        allowParallel = TRUE,
                        savePredictions = 'final')

model.list <- c("knn", "svmLinear2", "rf", "xgbLinear", "mlp", 
                "earth", "gbm", "extraTrees", "treebag", "brnn")

combs <- as.matrix(expand.grid(model.list))

c1.model <- list()
c2.model <- list()
c3.model <- list()

pred.c1.treino <- NA
pred.c1.teste  <- NA
pred.c1        <- NA

pred.c2.treino <- NA
pred.c2.teste  <- NA
pred.c2        <- NA

pred.c3.treino <- NA
pred.c3.teste  <- NA
pred.c3        <- NA

# Cria barra de progresso
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

for (i in 1:dim(combs)[1]) {
  
  c1.model[[i]] <- train(y~., data = treino[[1]],
                       method = combs[i,1],
                       trControl = control,
                       preProcess = c("center","scale"),
                       tuneLength = 4)
  
  c2.model[[i]] <- train(y~.,data = treino[[2]],
                       trControl = control,
                       preProcess = c("center","scale"),
                       method = combs[i,1],
                       tuneLength = 4)
  
  c3.model[[i]] <- train(y~.,data = treino[[3]],
                       trControl = control,
                       preProcess = c("center","scale"),
                       method = combs[i,1],
                       tuneLength = 4)
  # Predição
  
  pred.c1.treino <- predict(c1.model[[i]],treino[[1]])
  pred.c1.teste  <- predict(c1.model[[i]],teste[[1]])
  pred.c1[i]     <- data.frame(c(pred.c1.treino,pred.c1.teste))

  pred.c2.treino <- predict(c2.model[[i]],treino[[2]])
  pred.c2.teste  <- predict(c2.model[[i]],teste[[2]])
  pred.c2[i]     <- data.frame(c(pred.c2.treino,pred.c2.teste))

  pred.c3.treino <- predict(c3.model[[i]],treino[[3]])
  pred.c3.teste  <- predict(c3.model[[i]],teste[[3]])
  pred.c3[i]     <- data.frame(c(pred.c3.treino,pred.c3.teste))
  
  # Atualiza barra de progresso
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste( round(i/dim(combs)[1]*100, 0),
                                        "% done"))
}

close(pb)

count <- c(1:length(model.list))

combpred <- expand.grid(count,count,count)

Obs.treino <- treino[[1]] + treino[[2]] + treino[[3]]
Obs.teste  <- teste[[1]] + teste[[2]] + teste[[3]]

Metricas <- matrix(nrow = dim(combpred)[1],ncol = 5)
Metricas.treino <- matrix(nrow = dim(combpred)[1],ncol = 5)

Predicao <- matrix(nrow = n, ncol = dim(combpred)[1])

# Cria barra de progresso
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combpred)[1], width = 30, style = 3)

for (i in 1:dim(combpred)[1]) {
  Predicao[,i] <- pred.c1[[combpred[i,1]]] + 
                  pred.c2[[combpred[i,2]]] + 
                  pred.c3[[combpred[i,3]]]
  
  Predicao.treino <- Predicao[,i][1:cut]
  Predicao.teste  <- tail(Predicao[,i],n-cut)
  
  # #Metricas
  pred.RMSE  <- RMSE(Predicao.teste, Obs.teste)
  pred.RRMSE <- RMSE(Predicao.teste, Obs.teste)/mean(Predicao.teste)
  pred.MAPE  <- mape(Predicao.teste, Obs.teste)
  pred.R2    <- cor(Predicao.teste, Obs.teste[,1])^2
  
  pred.RMSE.treino  <- RMSE(Predicao.treino, Obs.treino)
  pred.RRMSE.treino <- RMSE(Predicao.treino, Obs.treino)/mean(Predicao.treino)
  pred.MAPE.treino  <- mape(Predicao.treino, Obs.treino)
  pred.R2.treino    <- cor(Predicao.treino, Obs.treino[,1])^2
  
  Metricas.treino[i,] <- c(i, pred.RMSE.treino,pred.RRMSE.treino,
                       pred.MAPE.treino,pred.R2.treino)
  Metricas[i,] <- c(i,pred.RMSE,pred.RRMSE,pred.MAPE,pred.R2)
  

  # Atualiza barra de progresso
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste(round(i/dim(combpred)[1]*100, 0),
                                        "% done"))
}

colnames(Metricas) <- c("i","RMSE","RRMSE","MAPE","R2")
colnames(Metricas.treino) <- c("i","RMSE","RRMSE","MAPE","R2")


Metricas[which.min(Metricas[,2]),]
Metricas[which.min(Metricas[,3]),]
Metricas[which.min(Metricas[,4]),]
Metricas[which.max(Metricas[,5]),]

Metricas.treino[which.min(Metricas[,2]),]
Metricas.treino[which.min(Metricas[,3]),]
Metricas.treino[which.min(Metricas[,4]),]
Metricas.treino[which.max(Metricas[,5]),]

save.image("oil-10-models.RData")
# save.image("gas-10-models.RData")

se <- data.frame((Predicao.teste-Obs.teste[,1])^2)
se.treino <- data.frame((Predicao.treino-Obs.treino[,1])^2)

sse <- apply(se,2,sum)
sse.treino <- apply(se.treino,2,sum)

data <- data.frame(dataset,values)

ggplot(data, aes(values, as.vector(dataset))) + 
  geom_line() + theme_bw(base_size = 18) +
  ylab("Produção de Petróleo (Mboe/d)") + xlab("Ano")

OBS <- as.data.frame(c(Obs.treino[,1],Obs.teste[,1]))

PO3(OBS,Predicao[,51],predicaoknn,predicaomars)
