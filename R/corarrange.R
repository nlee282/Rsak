
#' Arrange numeric variables by correlation.
#'
#' @importFrom dplyr arrange desc filter select
#' @param matrix Dataframe, numeric values
#' @param var (Optional) filter by specific variable
#' @return Table of correlations, arranged by correlation coefficient
#' @export
corarrange <- function(dataframe, var) {

  dataframe <- select(dataframe, where(is.numeric))

  correlation <- as.data.frame(as.table(abs(cor(dataframe)))) # abs(cor matrix) -> data frame
  correlation <- arrange(correlation, desc(Freq)) # arrange by frequency

  if (!missing(var)) {
    return(arrange(filter(correlation, Var2==var, Freq<1), desc(Freq))) # return correlations against specified variable
  }

  clean_correlation <- filter(correlation, Freq<1) # filter out variable against variable (PRE of 1)
  clean_correlation$Index <- c(1:nrow(clean_correlation)) # index rows
  clean_correlation <- filter(clean_correlation, Index %% 2 == 1) # removes duplicates

  return(select(clean_correlation, Var1, Var2, Freq))

}
