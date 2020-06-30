#Sesión: 19 de junio de 2020 

#Objetivos:

#a)	Determine una opción asiática, en donde la función de pago
#	está determinada por 
#	C=E[Max(S_prom-K,0)]
#	en donde S_prom para este caso, se toma de las últimas 
#	cotizaciones de un mes.

#b)	Determine una opción barrera, en donde la función de pago
#	está determinada por 
#	C=E[Max(S-K,0)] 
#	en donde S para este caso, se activar la función de pago
#	si el subyacente pasa una barrera inferior del 10% 
#	del activo.

#Para la acción AMXL.MX, determine la opción barrera y asiática
#respecto a un comportamiento histórico de un año.

#Algorítmo:
#	1. Descargar los datos.
#	2. Obtener la información
#	   Variables que necesitamos para poder iniciar programa
#		Respecto de la acción
#			S, K, sigma		#de la serie histórica
#			S  	última cotización de la acción	
#			K  	expectativas respecto al comportamiento del activo
#			sigma desviación estándar de los rendimientos
#		Respecto del mercado
#			r 	cetes
#		Respecto del contrato
#			dt, T			T<-0.5
#	3. Damos de alta las variables
#	4. Inicia la simulación de la opción europea

#	5. Para la opción asiática
#	C=E[Max(S_prom-K,0)]


##########################################################
#	Descargar la serie

if(!require(quantmod)) install.packages('quantmod')
library(quantmod)

getSymbols("AMXL.MX", src="yahoo", from="2019-06-18", 
	to="2020-06-18", periodicity="daily")
View(AMXL.MX)
names(AMXL.MX)

Datos_rendimientos<-apply(AMXL.MX[,4], 2, ROC, type="discrete")
Datos_rendimientos<-as.data.frame(Datos_rendimientos[-1,])
names(Datos_rendimientos)
View(Datos_rendimientos)


##########################################################
#	Extraer la información		S, K, sigma

numero<- length(AMXL.MX[,4])
S0<- as.double(AMXL.MX$AMXL.MX.Close[numero,])
sigma<-sd(Datos_rendimientos[,1])
K<-  S0
r<-  0.049
T<- 0.5
dt<-1/(250*T)

##########################################################
#	Iniciar el ciclo			#Opción asiática

trayectorias<-10000
suma_funcion <- 0
dias <- 20

for(j in 1:trayectorias){

	S <- S0
	suma <- 0

	for(i in 1:(250*T)){
	
		z0 <- rnorm(1,0,1)
		
		dW <- z0*(dt^0.5)
		dS <- (r*dt + sigma*dW)*S
		S <- S+dS
	if(i>(250*T-dias)){suma <- suma+S}	
	}

	promedioS<-suma/dias
	funcion_pago<-max(promedioS-K,0)
	suma_funcion<-suma_funcion+funcion_pago
}
esperanza_fnc_pago<-suma_funcion/trayectorias
C<-esperanza_fnc_pago * exp(-r*T)
C


##########################################################
#	Iniciar el ciclo			#Opción barrera

trayectorias<-5000
suma_funcion <- 0
dias <- 20
pro <- 0

for(j in 1:trayectorias){
  
  S <- S0
  suma <- 0
  
  for(i in 1:(250*T)){
    
    z0 <- rnorm(1,0,1)
    
    dW <- z0*(dt^0.5)
    dS <- (r*dt + sigma*dW)*S
    S <- S+dS
  }
  H <- S-K
  if(H/K>0.1) {
    H <- H
    pro <- pro + 1
  } else {H <- 0}
  
  funcion_pago<-max(H,0)
  suma_funcion<-suma_funcion+funcion_pago
}
esperanza_fnc_pago<-suma_funcion / pro
C <- esperanza_fnc_pago * exp(-r*T)
C

##########################################################
#	Ejercicio de tarea

#1. Determinar la sensibilidad de la opción respecto al número 
	#de trayectorias
	#trayectorias		C
	#1
	#10
	#100
	#1000
	#10000
#	Exportar en .csv

#2. Determinar la sensibilidad de la opción respecto al precio
#	de ejercicio	 K
#	expectativas de aumento de valor del S	10%
#			   baja del valor del S
#
#3. Determinar la opción con una barrera inferior.
#	Hacer el planteamiento de un problema
	
#	barrera_inferior
#	activar














































