vote.weight <-  function(){

  weighted.vote <- apply(df,2,function(x) sum(weights[,j]*x)/sum(weights[,j]))
  weighted.vote <- ifelse(weighted.vote <= 1.5, 1, 2)
  results <- sum(weighted.vote == actual)/ncol(df)
  m.results <- round(results,4)

  return(m.results)
}
