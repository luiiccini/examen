#Regla del trapecio

integral <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f debe ser una función de parámetro variable')
  }
  
  h <- (b - a) / n
  
  j <- 1:n - 1
  
  xj <- a + j * h
  
  approx <- (h / 2) * (f(a) + 2 * sum(f(xj)) + f(b))
  
  return(approx)
}
