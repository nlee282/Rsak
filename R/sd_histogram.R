
#' Sampling distribution histogram
#'
#' @importFrom supernova b1
#' @param formula y~x. y numeric, x numeric
#' @param data dataframe
#' @param n sample size
#' @param alpha alpha value (0.05)
#' @param statistic b1, pre, f, etc.
#' @param region middle, lower, upper
#' @export
sd_histogram <- function(formula, data, n, alpha=0.05, statistic=b1, region=middle) {

  data <- na.omit(data)
  # get outcome and explanatory variable names
  outcome <- all.vars(formula)[1]
  explanatory <- all.vars(formula)[2]

  sample <- statistic(data[[outcome]] ~ data[[explanatory]], data = data) # get sample statistic

  sdos <- do(n) * statistic(shuffle(data[[outcome]]) ~ data[[explanatory]], data = data) # sampling distribution of statistic

  gf_histogram(~statistic, data=sdos, fill = ~region(statistic, 1 - alpha)) %>% # histogram
    gf_labs(x=colnames(sdos), fill=paste("Within", 1-alpha)) %>%
    gf_point(x=sample, y=0) %>%
    gf_lims(x = c(min(sdos$statistic, sample), max(sdos$statistic, sample))) # scale x=axis to fit sample point
}
