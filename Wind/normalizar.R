## Passo 1 - Separar treino e teste
## Passo 2 - Calcular média e desvio padrão de x_treino e y_treino
## Passo 3 - Normalizar (x_treino - x_med)/x_sd 
##                      (x_teste  - x_med)/x_sd
##                      (y_treino - y_med)/y_sd
##                      (y_teste  - y_med)/y_sd
## Passo 4 - Treinar e prever
## Passo 5 - Desnormalizar y_treino y_teste (y_treino*y_sd)+y_med
##                                          (y_teste*y_sd)+y_med

## Passo 1
load("dataset.RData")

## Passo 2

x_med <- lapply(x_treino,mean)
y_med <- mean(y_treino)

x_sd <- lapply(x_treino, sd)
y_sd <- sd(y_treino)

## Passo 3
xtreino_norm <- (x_treino - x_med)/x_sd
xteste_norm  <- (x_teste  - x_med)/x_sd

ytreino_norm <- (y_treino - y_med)/y_sd
yteste_norm  <- (y_teste  - y_med)/y_sd

## Passo 4
seed <- 7

set.seed(seed)

actfun       <- c("relu","sig","sin","radbas","hardlim",
                  "hardlims","satlins","tansig","tribas","purelin")
init_weights <- c("normal_gaussian","uniform_positive",
                    "uniform_negative")
nhid         <- c(10:100)

grid <- as.matrix(expand.grid(actfun,init_weights,nhid))

Metricas <- matrix(nrow = dim(grid)[1],ncol = 4)

pb <- tkProgressBar(title = "Please wait...", min = 0, 
                    max = dim(grid)[1], width = 300)

for(i in 1:dim(grid)[1]){

  model_elm <- elm_train(as.matrix(xtreino_norm),
                         as.matrix(ytreino_norm),
                         actfun = grid[i,1],
                         init_weights = grid[i,2],
                         nhid = as.numeric(grid[i,3]))
  
  pred_elm_treino <- c(elm_predict(model_elm,as.matrix(xtreino_norm)))
  pred_treino     <- pred_elm_treino*y_sd+y_med
  pred_elm_teste  <- c(elm_predict(model_elm,as.matrix(xteste_norm)))
  pred_teste      <- pred_elm_teste * y_sd+ y_med
  
  
  RMSE_elm     <- RMSE(pred_teste,y_teste)
  MAPE_elm     <- mape(pred_teste,y_teste)
  Rsquared_elm <- cor(pred_teste,y_teste)^2
  
  Metricas[i,] <- c(i,RMSE_elm,MAPE_elm,Rsquared_elm)
  
  percentage <- (i/dim(grid)[1])
  
  if(percentage %% 0.01 == 0) {
    setTkProgressBar(pb, i, label=paste(round(percentage*100, 0),"% done"))
  }
  # cat("comb",i/dim(grid)[1]*100,"\n")
}

Metricas[which.min(Metricas[,2]),]
Metricas[which.min(Metricas[,3]),]
Metricas[which.max(Metricas[,4]),]

##################plot#################

PO<-function(Obs,Pred,Title)
{
  data<-data.frame(as.vector(unlist(data.frame(Obs,Pred))),
                   rep(c("y(t) - Estimated","y(t)-Predicted"),each=dim(Obs)[1]),
                   rep(seq(1,dim(Obs)[1]),times=2))
  colnames(data)<-c("Predictions","Legend","Frequency")
  
  g2<- ggplot(data, aes(Frequency, Predictions, colour=Legend))+ylab("Value")+xlab("Hour")+ggtitle(paste("Individual", Title))
  g2<- g2 + geom_line(size=0.75)+theme_bw(base_size = 18)
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.1, y=400,label="Training set",show.legend = FALSE, color = "#000000")
  g2<- g2 + geom_text(x=dim(Obs)[1]*0.8, y=400,label="Test set",show.legend = FALSE, color = "#000000")
  g2<- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 0.8)
  g2<- g2 + scale_color_manual(values=c("#000000","#999999"),labels = expression(italic(hat(y)(t)),italic(y(t))))
  g2<- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))
  print(g2)
}

Obs <- data.frame(y)
Pred<- data.frame(c(pred_treino,pred_teste))
{x11()
  PO(Obs,Pred,"One hour power forecasting")
}

