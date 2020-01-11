PO1<-function(Obs,Pred1,Pred2,Pred3,Pred4,Pred5,Title)
{
data1<-data.frame(as.vector(unlist(data.frame(Obs,Pred1,Pred2,Pred3,Pred4,Pred5))),
                 rep(c("Observed","Predicted 1","Predicted 2",
                       "Predicted 3","Predicted 4","Predicted 5"),each=dim(Obs)[1]),
                 rep(seq(1,dim(Obs)[1]),times=6))
colnames(data1)<-c("Predictions","Legend","Frequency")

g2<- ggplot(data1, aes(Frequency, Predictions, colour=Legend))+ylab("Value")+xlab("Month")+ggtitle(Title)
g2<- g2 + geom_line(size=0.75)+theme_bw(base_size = 18)
g2<- g2 + geom_text(x=dim(Obs)[1]*0.1, y=max(Obs),label="Training set",show.legend = FALSE, color = "#000000")
g2<- g2 + geom_text(x=dim(Obs)[1]*0.8, y=max(Obs),label="Test set",show.legend = FALSE, color = "#000000")
g2<- g2 + geom_vline(xintercept = dim(Obs)[1]*0.7, size = 0.8)
g2<- g2 + scale_color_brewer(palette="Dark2",label = c("Observed","svmLinear2","knn","rf","xgbLinear","mlp"))
# g2<- g2 + scale_color_manual(values=c("#000000","#999999"))
g2<- g2 + theme(legend.position = "bottom", legend.direction = "horizontal",plot.title = element_text(hjust = 0.5))
print(g2)
}
