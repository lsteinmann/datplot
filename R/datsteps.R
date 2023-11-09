#' @title Create 'steps' of dates for each object in a dataframe
#'
#' @description Requires a dataframe with 4 variables: ID (ideally factor),
#' group (ideally factor), minimum date (int/numeric) and maximum date
#' (int/numeric). It's expected that dates BCE are displayed as negative
#' values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param DAT_df a dataframe with 4 variables: ID, group, minimum date (int/num)
#' maximum date (int/num), _must_ be in this order, colnames are irrelevant;
#' each object _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be used as an
#' interval for creating dating steps.
#' @param calc "weight" (default): use the
#' [published original calculation](https://doi.org/10.1017/aap.2021.8)
#' for weights,
#' "probability": calculate year-wise probability instead (only useful
#' for a stepsize of 1)
#' @param cumulative TRUE: add a column for cumulative probability ("cumul_prob")
#' (only useful for a stepsize of 1, as the summed probability in larger
#' stepsizes has no meaning)
#'
#' @return a data.frame with a number of steps for each object as well
#' as a 'weight' value, that is a quantification of how well the object is
#' dated (lesser value means object is dated to larger timespans,
#' i.e. with less confidence)
#'
#' @examples
#' \dontrun{
#' DAT_df_steps <- datsteps(DAT_df[1:100, ], stepsize = 25)
#' plot(density(DAT_df_steps$DAT_step))
#' }
#'
#'
#' @export datsteps
datsteps <- function(DAT_df,
                     stepsize = 25,
                     calc = "weight",
                     cumulative = FALSE) {

  calc <- ifelse(grepl("weight", calc),
                 "weight",
                 calc)
  calc <- ifelse(grepl("prob", calc),
                 "probability",
                 calc)

  # redundand
  if (cumulative & calc != "probability") {
    warning("Switching to probability calculation to provide cumulative probability.")
    calc <- "probability"
  }
  if (stepsize != 1 && calc == "probability") {
    warning("Probability calculation is only meaningful for stepsize = 1.")
  }

  calc <- match.arg(calc, c("weight", "probability"))

  switch(calc,
         weight = message("Using 'weight'-calculation (see https://doi.org/10.1017/aap.2021.8)."),
         probability = message("Using step-wise probability calculation."))


  DAT_df <- as.data.frame(DAT_df)
  # Checking the overall structure
  check.structure(DAT_df)

  colnames <- c("index", "datmin", "datmax", calc, "step")


  # Check for the two Dainng columns to be in the correct order:
  if (any(DAT_df[, 3] > DAT_df[, 4])) {
    # Strore Index of Rows to switch later and issue warning, so people
    # can check the Data!
    dat_wrong_order <- which(DAT_df[, 3] > DAT_df[, 4])
    warning(paste("Warning: Dating seems to be in wrong order at ID ",
                  paste(DAT_df[dat_wrong_order, 1], collapse = ", "),
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
  DAT_mat[, 1] <- 1:nrow(DAT_df)
  DAT_mat[, 2] <- DAT_df[, 3]
  DAT_mat[, 3] <- DAT_df[, 4]

  colnames(DAT_mat) <- colnames

  # If not already set, set stepsize
  if (stepsize == "auto") {
    stepsize <- generate.stepsize(DAT_mat)
  } else if (!is.numeric(stepsize)) {
    stop(print("stepsize has to be either 'auto' or numeric."))
  }

  # calculate the weights or probabilities
  if (calc == "weight") {
    res <- get.weights(DAT_mat[, "datmin"],
                       DAT_mat[, "datmax"])
  } else if (calc == "probability") {
    res <- get.probability(DAT_mat[, "datmin"],
                           DAT_mat[, "datmax"])
  }
  DAT_mat[, calc] <- res

  DAT_list <- as.data.frame(DAT_mat)
  rownames(DAT_list) <- DAT_list[,1]

  DAT_list <- unlist(apply(DAT_list, 1, list), recursive = FALSE)


  # Process the dating to create the steps
  DAT_res <- create.sub.objects(DAT_list, stepsize, calc, cumulative)

  # convert to data.frame again and store the variable and ID in the correct
  # order, using the matrix index as reference
  result <- as.data.frame(DAT_res)

  # names and attributes
  colnames <- c("ID", "variable", "DAT_min", "DAT_max",
                calc, "DAT_step")
  if(cumulative) {
    colnames <- c(colnames, "cumul_prob")
  }
  result <- as.data.frame(matrix(nrow = nrow(DAT_res), ncol = length(colnames)))
  colnames(result) <- colnames

  result$ID <- DAT_df[DAT_res[, 1], 1]
  result$variable <- DAT_df[DAT_res[, 1], 2]
  result$DAT_min <- DAT_res[, "datmin"]
  result$DAT_max <- DAT_res[, "datmax"]
  result[, calc] <- DAT_res[, calc]
  result$DAT_step <- DAT_res[, "step"]
  if(cumulative) {
    result$cumul_prob <- DAT_res[, "cumul_prob"]
  }

  attr(result$DAT_step, "descr") <- "step"
  switch(calc,
         weight = attr(result$weight, "descr") <- "Calculated weight of each object according to doi.org/10.1017/aap.2021.8",
         probability = attr(result$probability, "descr") <- "Dating-Probability of each object")
  attr(result, "stepsize") <- stepsize

  return(result)
}
