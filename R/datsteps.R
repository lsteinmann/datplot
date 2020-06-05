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

  # Checking the overall structure
  check.structure(DAT_df)

  # Check for the two Dainng columns to be in the correct order:
  if (any(DAT_df[,3] > DAT_df[,4])) {
    # Strore Index of Rows to switch later and issue warning, so people can check the Data!
    dat_wrong_order <- which(DAT_df[,3] > DAT_df[,4])
    warning(paste("Warning: Dating seems to be in wrong order at ID ",
                  paste(DAT_df[dat_wrong_order,1], collapse = ", "),
                  " (Index: ",
                  paste(dat_wrong_order, collapse = ", "),
                  ")",
                  ". Dates have been switched, but be sure to check your original data for possible mistakes.",
                  sep = ""))
    # Switch the Dating of Rows assumed to be in wrong order:
    DAT_df <- switch.dating(DAT_df, dat_wrong_order)
  }

  # Prepare the Matrix to be used instead of the df for faster processing
  DAT_mat <- matrix(ncol = 5, nrow = nrow(DAT_df))
  DAT_mat[,1] <- 1:nrow(DAT_df)
  DAT_mat[,2] <- DAT_df[,3]
  DAT_mat[,3] <- DAT_df[,4]

  colnames(DAT_mat) <- c("index", "datmin", "datmax", "weight", "step")

  # If not already set, set stepsize
  if (stepsize == "auto") {
    stepsize <- generate.stepsize(DAT_mat)
  } else if (!is.numeric(stepsize)) {
    stop(print("stepsize has to be either 'auto' or numeric."))
  }

  # calculate the weights
  weights <- get.weights(DAT_mat[,"datmin"], DAT_mat[,"datmax"])
  DAT_mat[,"weight"] <- weights[,1]

  # Process the dating to create the steps
  DAT_res <- create.sub.objects(DAT_mat, stepsize)

  # convert to data.frame again and store the variable and ID in the correct order, using the matrix index as reference
  result <- as.data.frame(DAT_res)
  result[,2] <- DAT_df[result[,1],2]
  result[,1] <- DAT_df[result[,1],1]

  # names and attributes
  colnames(result) <- c("ID", "variable", "DAT_min", "DAT_max", "weight", "DAT_step")
  attr(result$DAT_step, "descr") <- "step"
  attr(result$weight, "descr") <- "weight"
  attr(result, "stepsize") <- stepsize

  return(result)
}
