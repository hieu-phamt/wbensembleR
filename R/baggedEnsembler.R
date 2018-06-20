#' Bagged Ensembler Function
#'
#' This function allows you to express your love of cats.
#' @param formula A formula describing the model to befitted
#' @param data An optional data frame containing the variables in the model
#' @param size Number of models to construct in ensemble
#' @param model A string specifying which classification to use. See caret.
#' @param sample.fraction The fraction of amount of data to sample
#' @return List of out-of-bag errors and models
#' @export

baggedEnsembler <- function(formula = NA, data = NA, size = 100, model = NA, sample.fraction = 0.632, ...){

  results.mat <- matrix(NA, nrow = size, ncol =  2,
                        dimnames = list(c(1:size), c("OOB Error", "Model")))
  model.list <- list()
  for(i in 1:size) {
    bag.sample <- sample(nrow(data), sample.fraction * nrow(data), replace = T) # Bagging sample numbers for training data
    train.data.bag <- data[bag.sample,] # Bagged training data
    test.data.bag <- data[-bag.sample,] # Bagged testing data

    train.model <- caret::train(formula, data = train.data.bag, method = model, ...) # model on training data

    ## Out of bag error for model
    oob.model <- c(1 - sum(caret::predict.train(train.model, test.data.bag) ==
                             test.data.bag[,which(colnames(test.data.bag) == as.character(formula)[2])])/
                     length(caret::predict.train(train.model, test.data.bag)))

    results.mat[i,1] <- oob.model
    results.mat[i,2] <- i
    model.list[[i]] <- train.model
  }

  model.list <- model.list[order(results.mat[,2])]
  results.mat <- results.mat[order(results.mat[,1]),]
  return(list(results.mat,model.list,formula))
}
