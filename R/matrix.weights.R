matrix.weights <- function(n = NA, p = 1, alpha = 1){
  parameters <- expand.grid(p,alpha)
  weights.matrix <- apply(parameters,1, function(x) weights(n,x[1],x[2]))
  weights.matrix <- rbind(t(parameters),weights.matrix)
  rownames(weights.matrix) <- c("p","alpha",paste0("weight",1:n))

  return(weights.matrix)
}
