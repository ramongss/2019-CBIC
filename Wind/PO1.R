PO1<-function(Obs,Pred1,Pred2,Pred3,Pred4,nameM1,nameM2,nameM3,nameM4)
{
data1<-data.frame(as.vector(unlist(data.frame(Obs,Pred1,Pred2,Pred3,Pred4))),
                 rep(c("Observed","Predicted 1","Predicted 2",
                       "Predicted 3","Predicted 4"),each=dim(Obs)[1]),
                 # rep(seq(1,dim(Obs)[1]),times=5))
                 rep(c(4200:4300),times=5))
colnames(data1)<-c("Predictions","Legend","Frequency")

g2<- ggplot(data1, aes(Frequency, Predictions, colour=Legend))+ylab("Power (KW)")+xlab("Samples (10 minutes)")
g2<- g2 + geom_line(size=0.6)+theme_bw(base_size = 18)
# g2 <- g2 + geom_label(x=dim(Obs)[1]*0.1, y=min(Obs),label="Training set",show.legend = FALSE, size = 6, color = "black")
# g2 <- g2 + geom_label(x=dim(Obs)[1]*0.8, y=min(Obs),label="Test set",show.legend = FALSE, size = 6, color = "black")
# g2<- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 1, color = 'blue')
g2<- g2 + scale_color_brewer(palette = "Greens",label = c("Observed",nameM1,nameM2,nameM3,nameM4))
# g2<- g2 + scale_color_manual(values=c("#000000","#999999"))
g2<- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))
print(g2)
}
