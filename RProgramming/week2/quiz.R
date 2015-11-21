cube <- function(x, n) {
  x^3
}


#Q2/The following code will produce a warning in R. Why?
x <- 1:10
if(x > 5) {
  x <- 0
}

#'x' is a vector of length 10 and 'if' can only test a single logical statement

#Q3/

f <- function(x) {
  g <- function(y) {
    y + z
  }
  
  z <- 4
  x + g(x)
}


#Q4. Consider the following expression: 
#What is the value of 'y' after evaluating this expression?
x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


#Q5:

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}