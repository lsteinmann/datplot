#' @title Create 'steps' of dates for each object in a data.frame
#'
#' @description
#' This function transforms a data.frame of dated objects with associated data
#' to a new data.frame which contains a row for each dating 'step' for each
#' objects. Dating 'steps' can be single years (with `stepsize = 1`) or an
#' arbitrary number that will be used as a guideline for the interval.
#' It's expected that dates BCE are displayed as negative
#' values while dates CE are positive values. Ignoring this will cause
#' problems. If dates are provided in the wrong order in any number of
#' rows they will automatically be switched.
#'
#' The function along with a guide on how to use it and a case study is published
#' in [Steinmann -- Weissova 2021](https://doi.org/10.1017/aap.2021.8).
#'
#'
#' @param DAT_df a data.frame with 4 variables:
#'   * `ID` : An identifier for each row, e.g. an Inventory number (ideally character).
#'   * `group` : A grouping variable, such as type or context (ideally factor).
#'   * `DAT_min` : minimum dating (int/num), the minimum dating boundary for a
#'   single object, i.e. the earliest year the object may be dated to.
#'   * `DAT_min` : maximum dating (int/num), the maximum dating boundary for a
#'   single object, i.e. the latest year the object may be dated to.
#' The columns _must_ be in this order, column names are irrelevant; each row
#' _must_ correspond to one datable entity / object.
#' @param stepsize numeric, default is 1. Number of years that should be used
#' as an interval for creating dating steps.
#' @param calc method of calculation to use;
#' can be either one of "weight" (default) or "probability":
#'  * "weight": use the
#'     [published original calculation](https://doi.org/10.1017/aap.2021.8)
#'     for weights,
#'  * "probability": calculate year-wise probability instead (only reasonable
#'     when `stepsize = 1`)
#' @param cumulative FALSE (default), TRUE: add a column containing the
#' cumulative probability for each object (only reasonable when `stepsize = 1`,
#' and will automatically use probability calculation)
#'
#' @return an expanded data.frame in with each row represents a dating 'step'.
#' Added columns contain the value of each step, the 'weight' or 'probability'-
#' value for each step, and (if chosen) the cumulative probability.
#'
#' @examples
#' \dontrun{
#' data(DAT_df)
#' DAT_df_steps <- datsteps(DAT_df, stepsize = 25)
#' plot(density(DAT_df_steps$DAT_step))
#' }
#'
#'
#' @export datsteps
datsteps <- function(DAT_df,
                     stepsize = 1,
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

  # check for Dating in wrong order and switch accordingly
  DAT_df <- switch.dating(DAT_df)

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
