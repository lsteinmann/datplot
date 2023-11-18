#' @title Scaling Factor for Combined Histogram Plots
#'
#' @description Requires a data.frame as produced by [datsteps()] or a number as
#' DAT_df_steps. Calculates the value with which the y-axis of a density graph
#' should be multiplied by in order to be visible in the corresponding histogram.
#'
#' @param DAT_df_steps a data.frame as returned by [datsteps()]. (Will also
#' work with a single number and a vector.)
#' @param binwidth the bandwidth to use for the density function and histogram.
#' Should equal the stepsize used to create the data.frame. If a data.frame as
#' returned by [datsteps()] is given, stepsize can be automatically assigned
#' using the corresponding attribute (`binwidth = "stepsize"`)
#'
#' @return the value with which to scale the density curve to a histogram
#' plot so that both will be visible
#'
#' @export get.histogramscale
#'
#' @examples
#' data("Inscr_Bithynia")
#' DAT_df <- Inscr_Bithynia[, c("ID", "Location", "DAT_min", "DAT_max")]
#' DAT_df_steps <- datsteps(DAT_df, stepsize = 25)
#' get.histogramscale(DAT_df_steps)
#'
#' get.histogramscale(DAT_df_steps$DAT_step, binwidth = 20)
#' get.histogramscale(500, binwidth = 20)
get.histogramscale <- function(DAT_df_steps, binwidth = "stepsize") {
  msg_sts <- paste("'binwidth = 'stepsize'' can only be used when",
                   "a data.frame as returned by `datsteps()` is supplied.",
                   "Otherwise, binwidth needs to be numeric.")
  if (is.numeric(DAT_df_steps) & length(DAT_df_steps) == 1) {
    nrow <- DAT_df_steps
    if (binwidth == "stepsize") {
      stop(msg_sts)
    }
  } else {
    if (inherits(DAT_df_steps, "data.frame")) {
      nrow <- nrow(DAT_df_steps)
    }
    if (is.atomic(DAT_df_steps)) {
      nrow <- length(DAT_df_steps)
    }
    if (binwidth == "stepsize") {
      binwidth <- attributes(DAT_df_steps)$stepsize
      if (is.null(binwidth)) {
        stop(msg_sts)
      }
    } else if (!is.numeric(binwidth)) {
      stop("Argument 'binwidth' has to be either 'stepsize' or numeric.")
    }
  }
  histogramscale <- nrow * binwidth
  return(histogramscale)
}
