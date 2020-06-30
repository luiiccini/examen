if(!require(quantmod)) install.packages('quantmod')
library(quantmod)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(arules)

setwd()
data_env<- new.env()
symbols<- c("AMXL.MX")

getSymbols(Symbols=symbols, env=data_env, from="2015-01-01", 
	to="2019-12-31")

Datos<- do.call(merge, eapply(data_env, Cl))
Datos_rendimientos<- apply(Datos, 2, ROC, type="discrete")
Datos_rendimientos %<>% as.data.frame()
Datos_rendimientos <- Datos_rendimientos[-1,]
Datos_rendimientos %<>% as.data.frame()
names(Datos_rendimientos) <- 'Rendimiento'
Datos %<>% as.data.frame() 
Datos["Fecha"] <- Datos %>% row.names()
Datos["Año"] <- str_sub(Datos["Fecha"]$Fecha,start=1,end = 4)
fechas <- Datos$Fecha[c(2:nrow(Datos))]
Datos_rendimientos['Fecha'] <- fechas
Datos_rendimientos["Año"] <- str_sub(Datos_rendimientos["Fecha"]$Fecha,start=1,end = 4)

entropia <- function(df){
  test <- discretize(df$Rendimiento,breaks = 30)
  test <- as.vector(test)
  dist <- test %>% table()/length(test)
  suma <- 0
  for (i in dist){
    h <- i*log2(i)
    suma <- suma + h
  }
  return(-suma)
}

calculo_var <- function(df=Datos_rendimientos,precios = Datos,acciones=100,year){
  df %<>% filter(`Año`==year) 
  precios %<>% filter(`Año`==year)
  graf <- df %>% select(-c(`Año`,Fecha)) %>% ggplot(aes(x=Rendimiento)) + geom_histogram() +
    ggtitle(year)
  S <- precios[1,1]*acciones
  media <- df %>% select(-c(`Año`,Fecha)) %>% colMeans()
  test <- df %>% select(-c(`Año`,Fecha)) %>% var()
  w <- rep(1,ncol(test))/ncol(test) %>% matrix(nrow = 1,ncol = ncol(test))
  wt <- w %>% t
  media_portafolio <- (w%*%media)[1,1]
  desv <- (w%*%test%*%wt %>% sqrt())[1,1]
  t <- nrow(df)
  VaR <- qnorm(0.95,media,desv)*S*desv*sqrt(t)
  ent <- df %>% entropia()
  return(list('var'=VaR,'hist'=graf,'entropia'=ent))
}

#Hacer for para calcular var cada año y los histogramas de cada 
vars <- c()
hist <- c()
entropias <- c()
for (i in c('2015','2016','2017','2018','2019')){
  output <- calculo_var(year=i)
  vars <- c(vars,output$var)
  hist <- c(hist,output$hist)
  entropias <- c(entropias,output$entropia)
}


out <- data.frame(Año=c('2015','2016','2017','2018','2019'))
out['VaR'] <- vars
out['Entropia'] <- entropias

write.csv(out, file = "Output/var_entropia.csv",row.names = F)
