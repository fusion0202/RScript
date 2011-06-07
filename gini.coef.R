gini.coef <- function(x)
{
  stopifnot(x >= 0)
  n <- length(x)
  x <- sort(x)
  x <- cumsum(x)
  x <- c(0, x/x[n])
  y <- seq(0, 1, length=n+1)
  return(2*sum(y-x)/n)
}