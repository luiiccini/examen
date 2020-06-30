#Ra??ces por diferentes m??todos

#Newton

library(Deriv)
test <- Deriv(~x^2,'x')

newton <- function(f,x0,tol){
  f_prima <- Deriv(f)
    x <-  x0
    while (abs(f(x)) > tol){
      x <-  x - f(x)/f_prima(x)
    }
    return(x)
}

#Bisecci??n

bisection <- function(f, a, b, n = 1000, tol = 1e-7) {
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('Signos de f(a) y f(b) difieren')
  } else if ((f(a) > 0) && (f(b) < 0)) {
    stop('Signos de f(a) y f(b) difieren')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calcular punto medio
    
    # Si la funci??n se anula en el punto medio, parar
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    
    # Si se necesita otra iteraci??n, 
    # verificar los signos de la funci??n en los puntos c y a y reasignar
    # a o b ser??n utilizados en la siguiente iteraci??n.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  # Si se llega al n??mero m??ximo de iteraciones, mostrar que se han hecho muchas iteraciones, 
  print('Demasiadas iteraciones')
}




#Falsa posici??n

regula_falsi <- function(a,b,f,MAX_ITER){
  if (func(a) * func(b) >= 0){ 
  print("Par??metros a,b no apropiados") 
  return(NULL)
  }
c <- a # Inicializar

  for (i in 1:MAX_ITER){ 
    
    # Encontrar punto que toca al eje x 
    c <-  (a * f(b) - b * f(a))/ (f(b) - f(a)) 
  
  # Verificar si es una ra??z 
    if (f(c) == 0){
      break
    }else if(f(c) * f(a) < 0){
      b <- c 
    }else{
      a <- c
    }
  }
  return(c)
}

#regula_falsi(-200,300,function(x) x^3 - x^2 + 2 ,1000)
