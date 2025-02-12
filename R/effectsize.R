
#' Table of effect size
#'
#' @param formula y~x. y numeric, x categorical, 2 group only
#' @param data Dataframe
#' @return Table of effect size
#' @export
effectsize <- function(formula, data) {
  outcome <- all.vars(formula)[1]
  explanatory <- all.vars(formula)[2]
  y = data[[outcome]]

  groups <- split(data[[outcome]], data[[explanatory]]) # split by category, [[]] return vector

  mean_1 <- mean(groups[[1]], na.rm=TRUE)
  mean_2 <- mean(groups[[2]], na.rm=TRUE)
  mean_diff <- abs(mean_1-mean_2)

  # PRE calculation
  sst <- sum((y - mean(y))^2)
  sse <-  sum((groups[[1]] - mean_1)^2) + sum((groups[[2]] - mean_2)^2)
  PRE <- (sst - sse) / sst

  # Cohen's d calculation
  sd_1 <- sd(groups[[1]], na.rm=TRUE)
  sd_2 <- sd(groups[[2]], na.rm=TRUE)
  df_1 <- length(na.omit(groups[[1]]))-1
  df_2 <- length(na.omit(groups[[2]]))-1

  pooled_sd <- sqrt((df_1*sd_1^2 + df_2*sd_2^2)/(df_1+df_2))
  d <- mean_diff/pooled_sd

  val <- data.frame(
    b1 = mean_diff,
    cohensD = d,
    PRE = PRE,
    df_1 = df_1,
    df_2 = df_2
  )
  rownames(val) <- ""
  return(val)
}
