#Sesión 29 Junio de 2020

#1.Genere un algorítmo que cambie la desviación
#  estándar para el cálculo de una opción europea.
#	sigma, límite inferior de .01 y un límite
#	superior de 0.3, teniendo saltos de 0.01.
#  Genere un archivo de salida en .csv con la sigma
#	y el pago de opción correspondiente.

getwd()
setwd("C:/Users/Alex/OneDrive/Documentos/4to Semestre/Análisis numérico")

S<-100
r<-.049
T<-12/12
dt<-1/(250*T)
sigma<-0.01
K<-110
trayectorias<-1000
suma<-0
n<-100
tabla<-matrix(, nrow=30, ncol=0)
Sigma<-c()
Call<-c()

while(a<n & abs(sigma)<=0.31){
  suma<-0
  for(j in 1:trayectorias){
    S<-100
    for(i in 1:(T*250)){
    
      z0<-rnorm(1,0,1)
      
      dW<-z0*(dt^.5)
      dS<-(r*dt+sigma*dW)*S
      S<-S+dS
    }
    funcion_pago<-max(S-K,0)
    suma<-suma+funcion_pago
  } 
  Esperanza<-suma/trayectorias 
  C<-Esperanza*exp(-r*T)
  print(sigma)
  Sigma<-append(Sigma, sigma)
  Call<-append(Call, C)
  sigma<-sigma+0.01
  a<-a+1
  print(C)
}

tabla<-cbind(tabla, Sigma, Call)
View(tabla)
write.csv(tabla,file="Sensibilidad_cambiando_sigma.csv")

#2. Para el cálculo de una opción genere un algorítmo
#	que contemple cambios en el tiempo. 
#	T= 5, 4, 3, 2, 1, 0.5, 0.25, 0.125

S<-100
r<-.049
sigma<-0.15
K<-110
trayectorias<-1000
suma<-0
tabla<-matrix(, nrow=8, ncol=0)
Tiempo<-c()
Call<-c()

for(a in 1:8){
  
  suma<-0
  T<-switch(a,5,4,3,2,1,0.5,0.25,0.125)
  dt<-1/(250*T)
  
  for(j in 1:trayectorias){
  
    S<-100
  
  for(i in 1:(T*250)){
    
    z0<-rnorm(1,0,1)
    dW<-z0*(dt^0.5)
    dS<-(r*dt + sigma*dW)*S
    S<-S+dS
  }
  funcion_pago<-max(S-K,0)
  suma<-suma+funcion_pago
}

esperanza<-suma/trayectorias
C<-esperanza * exp(-r*T)
Tiempo<-append(Tiempo, T)
Call<-append(Call, C)
print(T)
print(C)
}

tabla<-cbind(Tiempo, Call)
View(tabla)

#3. Para el cálculo de una opción arcoiris, en el 
#	que los parámetros son tomados de la serie 
#	histórica del último año. Compare el precio 
#	de la opción considerando 4 periodos en el año
#	de referencia y por lo tanto 4 pagos. 
#	Periodo 1 de enero 2019 - 31 de diciembre 2019
#trimestre 		sigma		S
#1			0.13		último precio de cada
#2			0.10		trimestre
#3			0.15
#4			0.14

install.packages("lubridate",dependencies = TRUE)
library(lubridate)
today()

if(!require(quantmod)) install.packages('quantmod')
library(quantmod)


data_env<-new.env()
symbols<-c("AMXL.MX", "BIMBOA.MX" )
inicio1<-as.Date("2019-01-01")
fin<-as.Date("2019-12-31")
#trimestre<-as.Date()

getSymbols(Symbols=symbols, env=data_env, from=inicio1, 
           to=fin)

Datos<-do.call(merge, eapply(data_env, Cl ))
Datos<-Datos[complete.cases(Datos),]
View(Datos)

Datos_rendimientos<-apply(Datos, 2, ROC, type="discrete")
Datos_rendimientos<-as.data.frame(Datos_rendimientos[-1,])
names(Datos_rendimientos)
View(Datos_rendimientos)

###############################


r <- 0.049
T <- 12/12		
dt <- 1/(250*T)
sigma <- 0.15
acciones <- 2
trayectorias <- 1000
suma <- 0

posicion0 <- 1
inicio <- inicio1
fechas <-  as.Date(index(Datos),tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
#Sigma1 <- c()
#Sigma2 <- c()
Call <- c()


for(k in 1:4){
  suma<-0
  intervalo<-(inicio+months(3))
  posicion1<-max(which(fechas<=intervalo))-1
  
  sigma1 <- sd(Datos_rendimientos[posicion0:posicion1,1])
  sigma2 <- sd(Datos_rendimientos[posicion0:posicion1,2])
  
  
  for(j in 1:trayectorias){
    
    S1<- Datos[posicion1,1]
    S2<- Datos[posicion1,2]
    K<-(S1+S2)/2
    
    for(i in 1:(T*250)){
      z0<-rnorm(1,0,1)
      dW=z0*(dt^.5)
      dS<- (r*dt + sigma*dW)*S1
      S1<-S1+dS
      
      z0<-rnorm(1,0,1)
      dW=z0*(dt^.5)
      dS<- (r*dt + sigma*dW)*S2
      S2<-S2+dS
      
    }
    funcion_pago<- max(S1-K, S2-K,0)	
    suma<-suma+funcion_pago
  }
  esperanza<-suma/trayectorias
  C<-esperanza*exp(-r*T)
  Call<-append(Call, C)
  Sigma1<-append(Sigma1, sigma1)
  Sigma2<-append(Sigma2, sigma2)
  
  
  
  posicion0<-posicion1
  inicio<-intervalo
  
}

tabla<-cbind(Sigma1, Sigma2, Call)
View(tabla)
