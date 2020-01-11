{
  x11()
  g2 <- ggplot(data, aes(x = PCTimeStamp, y= Power)) + 
    geom_line(size = 0.6, color = "#696969") + 
    xlab("Date") + ylab("Power (KW)") +
    theme_bw(base_size = 15)
  
  print(g2)
  
  acf(data[,2])
  
  ggsave(
    file.path("Plot", paste("acf.eps")),
    acf(data[,2]),
    width = 10,
    height = 5,
    dpi = 1200)         
}

xtable(Metricas)



ggplot(data, aes(x = Month, y = Oil)) +
  geom_line() + theme_bw() +
  xlab("Ano") + ylab("Produção de Petróleo")

adf.test(dataset, lags = 1, type = "ct")

adf.test(dataset, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
         description = NULL)

kpss.test(dataset, null="Trend")

fisher.g.test(dataset) #Tem uma componente periodica deterministca

# > ur.df(dataset,type="trend")
# 
# ############################################################### 
# # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
# ############################################################### 
# 
# The value of the test statistic is: -2.8733 3.0269 4.1319 

# > ur.df(dataset,type="drift")
# 
# ############################################################### 
# # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
# ############################################################### 
# 
# The value of the test statistic is: -1.3412 1.2887 


ur.df(dataset,type="trend")

kruskal.test(data=data, Oil~Month)

## Teste de Friedmann
## Plot CD

## t(Metricas)