# Adapted from: https://github.com/benhamner/Metrics/blob/master/R/R/metrics.r#L181

# Compute the mean average precision at 3

map3 <- function(actual, predicted, returnScores=FALSE)
{
  scores <- rep(0, length(actual))
  scores[predicted[,1]==actual] <- 1
  scores[predicted[,2]==actual] <- 1/2
  scores[predicted[,3]==actual] <- 1/3
  
  if(!returnScores){
    out <- mean(scores)
  } else{
    out <- scores
  }
  out
}

#' Compute the average precision at k
#'
#' This function computes the average precision at k
#' between two sequences
#'
#' @param k max length of predicted sequence
#' @param actual ground truth set (vector)
#' @param predicted predicted sequence (vector)
#' @export
apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(i>1 && predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
mapk <- function (k, actual, predicted, returnScores=FALSE)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[i], predicted[i,])
  }
  
  if(!returnScores){
    out <- mean(scores)
  } else{
    out <- scores
  }
  out
}