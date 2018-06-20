predict.ensemble <- function(object = NULL, data = NULL, p = NA, alpha = NA){

  votes <- matrix(NA,
                  nrow = length(object[[2]]),
                  ncol = nrow(data),
                  dimnames = list(paste0("model",1:length(object[[2]])), rownames(data)))

  for(i in 1:length(object[[2]])){
    votes[i,] <- caret::predict.train(object[[2]][[i]], data)
  }

  weighted.votes <- matrix.weights(n = length(object[[2]]), p = p, alpha = alpha)

  vector.results <- numeric()
  for(j in 1:(length(p)*length(alpha))){

    weighted.votess <- ifelse(apply(votes,2,FUN = function(x)
      sum(weighted.votes[3:(length(object[[2]]) + 2),j]*x)/
        sum(weighted.votes[3:(length(object[[2]]) + 2),j])) <= 1.5, 1, 2)

    results <- sum(weighted.votess == as.numeric(data[,which(colnames(data) == as.character(object[[3]])[2])]))/nrow(data)

    vector.results[j] <- round(results,4)
  }

  combined <- rbind(weighted.votes,vector.results)
  output <- data.frame(combined[c(1:2,length(object[[2]]) + 3), ])

  rownames(output)[3] <- "accuracy"
  colnames(output) <- 1:(length(p)*length(alpha))


  return(list(votes,output))
}
