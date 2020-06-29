#Sesi?n: 24 de Junio 2020

#Derivada n?merica

x<-(0:6)
funcion<-sin(x)
plot(funcion,type="l")

x0<-2
Delta<-0.1
funcion<-sin(x0)
funcionD<-sin(x0+Delta)
derivadaN<-(funcionD-funcion)/Delta

epsilon<-10^-5

n<-400
i<-1

while(i<n & abs(derivadaN)>epsilon){

	derivadaA<-derivadaN

	funcion<-sin(x0)
	funcionD<-sin(x0+Delta)
	derivadaN<-(funcionD-funcion)/Delta

	if(derivadaA*derivadaN<0){
	x0<-x0-Delta
	Delta<-Delta/10
	print("cambio")}

	x0<-x0+Delta
	i<-i+1
}
i
derivadaN

#################################################

#DETERMINE LA DERIVADA NUM?RICA DE LA FUNCI?N 
#DE DEMANDA D=100 -2P, PARA UN MONOPOLISTA
#CON UNA FUNCI?N DE INGRESOS Y=D*P

#a)	GRAFIQUE LA FUNCI?N DE INGRESOS
#b) DETERMINE EL INGRESO 
#c) DETERMINE EL PUNTO EN QUE SE MAXIMIZA EL INGRESO

########################################
#a)	GRAFIQUE LA FUNCI?N

P<-0:50
D<-100-2*P
Y<-D*P
plot(Y, type="l")

########################################
#b) DETERMINE EL INGRESO 

Y=(100-2*P)*P
Y= 100P - 2P^2

########################################
#c) DETERMINE EL PUNTO EN QUE SE MAXIMIZA EL INGRESO

p0<-20
Delta<-0.1

Y=(100*p0)-2*p0^2 
YD=(100*(p0+Delta))-(2*(p0+Delta)^2)
derivadaN<-(YD-Y)/Delta

n<-200
i<-1
epsilon<-10^-2

while(i<n & abs(derivadaN)>epsilon){

	derivadaA<-derivadaN

	Y<-(100*p0)-2*p0^2 
	YD<-(100*(p0+Delta))-(2*(p0+Delta)^2)
	derivadaN<-(YD-Y)/Delta

	if(derivadaA*derivadaN<0){
	p0<-p0-Delta
	Delta<-Delta/10
	print("cambio")
	}


	p0<-p0+Delta
	i<-i+1
}
i
p0


#El precio que maximiza el ingreso es 25
Y<-(100*p0)-2*p0^2 

#comprobando el resultado con la derivada

Y=(100*p0)-2*p0^2 

Derivando Y con respecto al precio

100-4p=0
     p=100/4
	 p=25














