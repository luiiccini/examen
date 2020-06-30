
#######TAREA 1.4

getwd()
if(!require(quantmod)) install.packages('quantmod')
library(quantmod)


data_env<- new.env()
symbols<- c("AMXL.MX", "GMEXICOB.MX","ALFAA.MX","GFNORTEO.MX","FEMSAUBD.MX")

getSymbols(Symbols=symbols, env=data_env, from="2019-06-05", 
	to="2020-06-05")

Datos<- do.call(merge, eapply(data_env, Cl))
View(Datos)
names(Datos)
##############################################################################
Datos_rendimientos<- apply(Datos, 2, ROC, type="discrete")
Datos_rendimientos<-as.data.frame(Datos_rendimientos[-1,])
names(Datos_rendimientos)


Datos_rendimientos<- Datos_rendimientos[complete.cases(Datos_rendimientos),]

View(Datos_rendimientos)

##############################################################################

	matriz<-data.frame(Variable=double(),x=double(),y0=double(),y=double(),
	y1=double(),Nx1=double(),xa=double(),Y02=double(),y2=double(),
	y12=double(),Nx2=double())


	particiones<-100
	series<-10
	x<-0.01
	xa<-0.05



for(i in 1:series){

	bins<-seq(min(Datos_rendimientos[,i]),max(Datos_rendimientos[,i]),
along.with=(1:particiones))

	histograma<-hist(Datos_rendimientos[,i],main=sprintf("Valor en riesgo de %s", symbols[i]),
breaks=bins,xlab="rendimientos",ylab="frecuencia",col="blue",border="blue")

dev.off()

histograma$count


no_datos<-length(Datos_rendimientos[,i])

prob_acumulada<-cumsum(histograma$count)/no_datos

posicion0<-max(which(prob_acumulada<x))

x0<-prob_acumulada[posicion0]
x1<-prob_acumulada[posicion0+1]
y0<-histograma$breaks[posicion0]
y1<-histograma$breaks[posicion0+1]

y<-y0+((y1-y0)/(x1-x0))*(x-x0)

posicion1<-max(which(prob_acumulada<xa))

x0<-prob_acumulada[posicion1]
x1<-prob_acumulada[posicion1+1]
y02<-histograma$breaks[posicion1]
y12<-histograma$breaks[posicion1+1]

y2<-y02+((y12-y02)/(x1-x0))*(xa-x0)

Nx1<-sum(histograma$count[prob_acumulada<x])
Nx2<-sum(histograma$count[prob_acumulada<xa])
datos<-(c(symbols[i],x,y0,y,y1,Nx1,xa,y02,y2,y12,Nx2))
matriz<-rbind(tabla,datos)

}



names(matriz)<-(c("Variable","x","y0","VAR","y1","CVAR","x","y02","VAR","y12","CVAR"))

View(matriz)

write.csv(tabla, file = "C:/Users/bluem/OneDrive/Documentos/An?lisis N?merico/TABLA_TAREA1.csv")

