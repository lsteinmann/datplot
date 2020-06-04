#' @title Scaling Factor for Combined Histogramm Plots
#'
#' @description Requires a dataframe as produced by datsteps(). (Meaning 6 columns in the following order: ID, group, minimum/earliest date, maximum/latest date, weight, 'DAT_Steps')
#'
#' @param DAT_df a dataframe as returned by datsteps
#' @param bw the bandwidtg to use. shoudl be stepsize used to compute the dataframe, and if a df as returned by datsteps() is automatically assigned
#'
#' @return the factor with which to scale the density curve to a histogram plot
#'
#' @export get.histogramscale

get.histogramscale <- function(DAT_df, bw = "auto") {
  if (bw == "auto") {
    if (!is.null(attributes(DAT_df)$stepsize)) {
      bw <- attributes(DAT_df)$stepsize
    } else {
      stop("Either specify stepsize or use a data.frame as returned by datsteps()")
    }
  } else if (!is.numeric(bw)) {
    stop("Either specify stepsize or use a data.frame as returned by datsteps()")
  }
  density <- density(DAT_df$DAT_step, weights = DAT_df$weight, bw = bw)
  breaks <- range(DAT_df$DAT_step)
  breaks <- breaks[2] - breaks [1]
  breaks <- round(breaks / bw, digits = 0)
  hist <- hist(DAT_df$DAT_step, breaks = breaks)
  histogramscale <- max(hist$counts) / max(density$y)
  return(histogramscale)
}
