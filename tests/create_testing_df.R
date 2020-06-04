#' @title Data frame for tests
#'
#' @description Produces data frame to test stuff
#'
#' @param k rows
#' @param distmean findable thing
#' @param distsd lalala
#'
#' @return df for testing
#'
#' @examples
#'
#' @export create.testing.df


create.testing.df <- function(k = 100, distmean = 150, distsd = 25) {
  test_df <- as.data.frame(matrix(nrow = k, ncol = 4))
  colnames(test_df) <- c("id", "variable", "min", "max")
  vars <- c("A", "B", "C")
  test_df_two <- test_df
  test_df$variable <- rep(vars, times = ceiling(k/length(vars)))[1:k]
  test_df$min <- round(runif(100, min = -699, max = 699), digits = 0)
  test_df$max <- test_df$min + (round(runif(100, min = 1, max = 299), digits = 0))


  test_df_two$min <- round(rnorm(k, mean = distmean, sd = distsd), digits = 0)
  test_df_two$max <- test_df_two$min + round(rnorm(k, mean = 35, sd = 9), digits = 0)
  test_df_two$variable <- "D"

  test_df <- rbind(test_df, test_df_two)
  rm(test_df_two)
  test_df <- test_df[sample(nrow(test_df)),]

  test_df$id <- paste("ID_", 1:nrow(test_df), sep ="")
  return(test_df)
}
