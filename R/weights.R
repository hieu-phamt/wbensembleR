weights <- function(n = NA, p = 1, alpha = 1) {
  harmonic1 <- sapply(1:n, FUN = function(x) 1/x^p)
  weight1 <- sum(harmonic1[1:n], na.rm = T)
  weightt <- numeric()
  for (i in 2:n) {
    harmonic <- sapply(i:n, FUN = function(x) 1/x^p)
    regularizer <- sapply(1:(i - 1), FUN = function(x) alpha/x^p)
    weightt[i - 1] <- sum(harmonic) + sum(regularizer)
  }
  return(c(weight1, weightt))
}
