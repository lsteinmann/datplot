#' @title Scaling Factor for Combined Histogramm Plots
#'
#' @description Requires a dataframe as produced by datsteps() or a number as DAT_df_steps. Calculated the value with which the
#' y-axis of a density graph should be multplied in order to be visible in the corresponding histogram.
#'
#' @param DAT_df_steps a dataframe as returned by datsteps (works also with a single number and a vector)
#' @param binwidth the bandwidth to use for the density function and histogram. Should be stepsize used to
#' create the dataframe. If a df as returned by datsteps() is given, stepsize can be automatically assigned
#' using the corresponding attribute (binwidth = "stepsize")
#'
#' @return the value with which to scale the density curve to a histogram plot so that both will be visible
#'
#' @examples
#' DAT_df_steps <- datsteps(DAT_df[1:100,], stepsize = 25)
#' get.histogramscale(DAT_df_steps)
#'
#' get.histogramscale(DAT_df_steps$DAT_step, binwidth = 20)
#' get.histogramscale(500, binwidth = 20)
#'
#' @export get.histogramscale
get.histogramscale <- function(DAT_df_steps, binwidth = "stepsize") {
  if (check.number(DAT_df_steps) & length(DAT_df_steps) == 1) {
    nrow <- DAT_df_steps
    if (binwidth == "stepsize") {
      stop("'binwidth == stepsize' cannot be used with a number, supply either a dataframe as returned by datsteps or a numerical binwidth")
    }
  } else {
    if (class(DAT_df_steps) == "data.frame") {
      nrow <- nrow(DAT_df_steps)
    }
    if (is.atomic(DAT_df_steps)) {
      nrow <- length(DAT_df_steps)
    }
    if (binwidth == "stepsize") {
      binwidth <- attributes(DAT_df_steps)$stepsize
      if (is.null(binwidth)) {
        stop("Supply numerical binwidth or dataframe as returned by datsteps")
      }
    } else if (!check.number(binwidth)) {
      stop('Supply numerical binwidth or use binwidth = "stepsize" with a dataframe as returned by datsteps.')
    }
  }
  histogramscale <- nrow * binwidth
  return(histogramscale)
}
