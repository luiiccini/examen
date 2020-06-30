# Esta función recibe una función y encuentra la derivada numérica en un punto x0

der_num <- function(x,f,h=0.01,type=1){
  if (type==1){
  derivada <- (f(x+h) - f(x-h))/(2*h)
  return(derivada)
  }else{
  derivada <- (f(x+h) - f(x))/(h)
  return(derivada)
  }
}

max_min_f <- function(x,f,h=0.01,type=1,x0=0,delta = 0.001){
  derivada <- 1
  x <- x0
  while (abs(derivada)>0.001) {
    derivada <- der_num(x,f,h,type)
    x <- x + delta 
  }
  return(x)
}

#der_num(x=1,function(x){x^2},type=1) #Aplicación de la derivada
#max_min_f(x, sin) #Enontrar puntos críticos
