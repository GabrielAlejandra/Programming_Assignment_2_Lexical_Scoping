#PRIMERA FORMA
# 1. Factorial_loop: una versión que calcula el factorial de un entero utilizando
# looping (como un bucle for)

Factorial_loop<-function(x){
  if(x<0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  if(x == 0){
    return(1)
  } else{
    y <- 1
    for(i in 1:x){
      y <- y * ((1:x)[i])
    }
    return(y)
  }
}
#SEGUNDA FORMA
# 2. Factorial_reduce: una versión que computa el factorial usando la reducción ()
# función en el paquete purrr. Alternativamente, puede usar la función Reducir ()
# en el paquete base.
Factorial_reduce <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  # ensure purrr package is installed
  if (!require('purrr', quietly = TRUE)) {
    stop('Please install the purrr package')
  }
  
  if(x == 0){
    return(1)
  } else{
    reduce(as.numeric(1:x), `*`) %>% return()
  }
}

#TERCERA FORMA
# 3. Factorial_func: una versión que usa recursividad para calcular el factorial.

Factorial_func <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  if (x == 0){
    return (1)
  } else{
    return (x * Factorial_func(x-1))
  }          
}

#CUARTA FORMA
# 4. Factorial_mem: una versión que usa memoria para calcular el factorial.

memoization <- function(){
  
  values <- 1
  
  Factorial_mem <- function(x){
    
    if(x < 0){
      stop("Factorials can only be computed when x is equal to, or greater than, zero")
    }
    
    if (x == 0 | x == 1){
      return(1)
    } 
    
    if (length(values) < x){
      values <<- `length<-`(values, x)
    }
    
    if (!is.na(values[x])){
      return(values[x])
    }
    #calculate new values
    values[x] <<- x * factorial(x-1)
    values[x]
  }
  Factorial_mem
}

Factorial_mem <- memoization()

# benchmarking these four functions
library(microbenchmark)
microbenchmark(
  Factorial_loop(1),
  Factorial_reduce(1),
  Factorial_func(1),
  Factorial_mem(1)
)

microbenchmark(
  Factorial_loop(10),
  Factorial_reduce(10),
  Factorial_func(10),
  Factorial_mem(10)
)

microbenchmark(
  Factorial_loop(100),
  Factorial_reduce(100),
  Factorial_func(100),
  Factorial_mem(100)
)

