#' @title Create 'steps' of dates for each object in a dataframe
#'
#' @description Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param DAT_df a dataframe with 4 variables: ID, group, minimum date (int/num) maximum date (int/num), _must_ be in this order, colnames are irrelevant; each object _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be used as an interval for creating dating steps.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @examples
#' DAT_df_steps <- datsteps(DAT_df[1:100,], stepsize = 25)
#' plot(density(DAT_df_steps$DAT_step))
#'
#'
#' @export datsteps

datsteps <- function(DAT_df, stepsize = 25) {
  DAT_mat <- as.matrix(DAT_df)
  result <- as.data.frame(NULL)
  if (stepsize == "auto") {
    stepsize <- generate.stepsize(DAT_df)
  } else if (!is.numeric(stepsize)) {
    stop(print("stepsize has to be either 'auto' or numeric."))
  }

  if (any(DAT_df[,3] > DAT_df[,4])) {
    warning(paste("Warning: Dating seems to be in wrong order at ID ", paste(DAT_df[which(DAT_df[,3] > DAT_df[,4]),1], collapse = ", "), " (Index: ",
                  paste(which(DAT_df[,3] > DAT_df[,4]), collapse = ", "), ")",
                  ". Dates have been switched, but be sure to check your original data for possible mistakes.", sep = ""))
    DAT_err <- which(DAT_df[,3] > DAT_df[,4])
    DAT_df <- switch.dating(DAT_df, DAT_err)
  }

  weights <- get.weights(DAT_df[,3], DAT_df[,4])

  DAT_df$weight <- weights[,1]
  result <- create.sub.objects(DAT_df, stepsize)
  return(result)
}

