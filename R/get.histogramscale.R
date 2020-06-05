#' @title Scaling Factor for Combined Histogramm Plots
#'
#' @description Requires a dataframe as produced by datsteps().
#' Meaning 6 columns in the following order:
#' * ID,
#' * group,
#' * minimum/earliest date,
#' * maximum/latest date,
#' * weight,
#' * 'DAT_Steps'
#'
#' @param DAT_df a dataframe as returned by datsteps
#' @param bw the bandwidth to use for the density function and histogram. Should be stepsize used to
#' create the dataframe. If a df as returned by datsteps() is given, stepsize is automatically assigned
#' using the corresponding attribute.
#'
#' @return the value with which to scale the density curve to a histogram plot so that both will be visible
#'
#' @examples
#' DAT_df_steps <- datsteps(DAT_df[1:100,], stepsize = 25)
#' get.histogramscale(DAT_df_steps)
#'
#' @export get.histogramscale

get.histogramscale <- function(DAT_df, bw = "auto") {
  if (bw == "auto") {
    if (!is.null(attributes(DAT_df)$stepsize)) {
      bw <- attributes(DAT_df)$stepsize
    } else {
      stop("Either specify stepsize/bandwidth (bw = ) or use a data.frame as returned by datsteps()")
    }
  } else if (!is.numeric(bw)) {
    stop("Either specify stepsize/bandwidth (bw = ) or use a data.frame as returned by datsteps()")
  }
  density <- density(DAT_df$DAT_step, weights = DAT_df$weight, bw = bw)
  breaks <- range(DAT_df$DAT_step)
  breaks <- breaks[2] - breaks [1]
  breaks <- round(breaks / bw, digits = 0)
  hist <- hist(DAT_df$DAT_step, breaks = breaks)
  histogramscale <- max(hist$counts) / max(density$y)
  return(histogramscale)
}
