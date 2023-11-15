#' @title Determine stepsize (internal)
#'
#' @description Determines stepsize by selecting the absolute minimum value
#' between the upper and lower end of all dating ranges.
#'
#' @param DAT_mat a matrix as prepared by [datsteps()], resp. a matrix witch
#' columns names `datmin` and `datmax` containing numeric/integer value of the
#' dating ranges.
#'
#' @return A single numeric value that can be used as minimal stepsize.
#'
#' @seealso [datsteps()]
#'
#' @keywords internal
generate.stepsize <- function(DAT_mat) {
  timespans <- (abs(DAT_mat[, "datmin"] - DAT_mat[, "datmax"]) + 1)
  stepsize <- min(timespans)

  print(paste("Using stepsize = ", stepsize, " (auto).", sep = ""))
  return(stepsize)
}

#' @title Switch values where dating is in wrong order (internal)
#'
#' @description Requires a data.frame with 2 numeric variables in the
#' 3rd and 4th column: minimum date (int/numeric) and
#' maximum date (int/numeric) as used in [datsteps()].
#'
#' @param DAT_df a data.frame with 4 variables in this order: ID, group,
#' minimum date (int/num), maximum date (int/num)
#'
#' @return The same data.frame with the dating values which were in wrong order
#' switched.
#'
#' @seealso [datsteps()]
#'
#' @keywords internal
switch.dating <- function(DAT_df) {
  dat_wrong_order <- which(DAT_df[, 3] > DAT_df[, 4])

  if (length(dat_wrong_order) > 0) {
    warning(paste0("Warning: Dating seems to be in wrong order at ID ",
                  paste(DAT_df[dat_wrong_order, 1], collapse = ", "),
                  " (Index: ", paste(dat_wrong_order, collapse = ", "),
                  "). Dates have been switched, but be sure to check ",
                  "your original data for possible mistakes."))
    # Switch the Dating of Rows assumed to be in wrong order:
    DAT_df[dat_wrong_order, 3:4] <- DAT_df[dat_wrong_order, 4:3]
  }

  return(DAT_df)
}


#' @title Calculate the weights for each dated object
#'
#' @description Calculates the weights from two vectors of minimum and maximum
#' dating for each object. Returns a dataframe with the weight in the first
#' column and FALSE in the second if two rows have the same value in both
#' min and max dating. See [publication](https://doi.org/10.1017/aap.2021.8)
#' for information about how this is calculated.
#'
#' @param DAT_min a numeric vector containing the minimum date of each object
#' @param DAT_max a numeric vector containing the maximum date of each object
#'
#' @seealso [datsteps()], [get.probability()]
#'
#' @return a vector of 'weight'-values for the datsteps-data.frame, that is a
#' quantification of how well the object is dated (lesser value means object
#' is dated to larger timespans, i.e. with less confidence)
#'
#' @export get.weights
get.weights <- function(DAT_min, DAT_max) {
  stopifnot(is.numeric(DAT_min))
  stopifnot(is.numeric(DAT_max))

  weights <- abs(DAT_min - DAT_max)

  if (any(weights == 0)) {
    warning(paste0("Warning: DAT_min and DAT_max at Index: ",
                   paste(which(weights == 1), collapse = ", "),
                   " have the same value! Is this correct? ",
                   "Please check the table for possible errors."))
    # set weight to 1 to treat objects with same min and max
    # dating (dated to one year precisely) as very influential
    # will have the same weight as objects dated to two years,
    # (which may also equal a span of 1 year)
    weights[which(weights == 0)] <- 1
  }
  # weights have to be below 1
  weights <- 1 / weights

  return(weights)
}


#' @title Calculate the probability for each year and each dated object
#'
#' @description Calculates the probability of each object being dated into
#' each year / timeslot from two vectors of minimum and maximum
#' dating. Returns a vector of probabilities.
#'
#' @inheritParams get.weights
#'
#' @return a vector of probabilities for each object being dated to any
#' single year within the timespan (lesser value means object is dated to
#' larger timespans, i.e. with less confidence).
#'
#' @seealso [datsteps()], [get.weights()]
#'
#' @export get.probability
get.probability <- function(DAT_min, DAT_max) {
  stopifnot(is.numeric(DAT_min))
  stopifnot(is.numeric(DAT_max))

  # calculate the dating probability
  # (thanks to Christian Gugl for requesting this)
  prob <- abs(DAT_min - DAT_max)
  prob <- prob + 1
  prob <- 1 / prob

  return(prob)
}


#' @title Calculate the sequence of dating steps
#'
#' @description
#' Produces an appropriate sequence of years between the minimum
#' and maximum dating.
#'
#' If they cannot be properly divided by the stepsize set
#' beforehand, either three values are generated for objects that are dated to
#' a range of more then 60% of the stepsize (min, mean, max), or two values for
#' objects dated to a timespan of less or equal to 60% of the stepsize.
#' If they can be divided without residual, the normal sequence is returned.
#' If there is a residual, the stepsize is modified depending on how large the
#' residual is.
#'
#' @param datmin numeric value of the minimum dating of one object
#' @param datmax numeric value of the maximum dating of one object
#' @param stepsize the stepsize to be used
#'
#' @return sequence of steps to be created by [create.sub.objects()]
#'
#' @seealso [datsteps()], [create.sub.objects()]
#'
#' @export get.step.sequence
#'
#' @examples
#' min_year <- -494
#' max_year <- -334
#' sequence <- get.step.sequence(datmin = min_year, datmax = max_year, stepsize = 25)
#' print(sequence)
#'
#' min_year <- 1
#' max_year <- 100
#' sequence <- get.step.sequence(datmin = min_year, datmax = max_year, stepsize = 25)
#' print(sequence)
get.step.sequence <- function(datmin = 0, datmax = 100, stepsize = 25) {

  stopifnot(is.numeric(datmin))
  stopifnot(is.numeric(datmax))
  stopifnot(is.numeric(stepsize))

  # Get the difference of the two dating values
  timespan <- datmax - datmin

  # First: If the stepsize is larger than the timespan, two different
  # strategies can be employed
  if (timespan %/% stepsize == 0) {
    if (timespan > (stepsize * 0.6)) {
      # If the timespan exceeds 60% of the stepsize, three steps will be
      # created corresponding to minimum, mean and maximum dating
      sequence <- c(datmin, round(((datmin + datmax) / 2), digits = 0), datmax)
    } else if (timespan == 0) {
      # for objects dated to one year, only use one year!
      sequence <- datmin
    } else {
      # if the timespan is less than 60% of the stepsize, just two values
      # corresponding to minimum and maximum dating will be returned
      sequence <- c(datmin, datmax)
    }
  } else {
    # If the timespan can be devided at least once, first generate the sequence
    sequence <- seq(from = datmin, to = datmax, by = stepsize)
    # then check how many years the maximum dating would be off
    resid <- datmax - sequence[length(sequence)]
    if (resid >= (stepsize / 2)) {
      # if the residual is larger or equals half the stepsize, the stepsize is
      # temporarily modified to fit the as many values
      # as it would with the length of the sequence generated
      stepsize_mod <- (datmax - datmin) / (length(sequence) + 1)
      sequence <- seq(datmin, datmax, stepsize_mod)
      # then rounds all values except first and last, which need to stay as
      # minumum and maximum date
      sequence[-c(1, length(sequence))] <-
        round(sequence[-c(1, length(sequence))],
              digits = 0)
    } else if (resid != 0) {
      # if the residual is smaller but also not 0, the sequence values at moved
      # by an appropriate fraction
      move <- round(resid / (length(sequence) - 1), digits = 0)
      sequence[2:length(sequence)] <- sequence[2:length(sequence)] + move
      # and the end of the sequence is reset as the maximum dating
      sequence[length(sequence)] <- datmax
      # TODO: these two things do essentially the same? I need to fix the first
      # one to use the largest possible division, maybe
    } else {
      # this implies that there was no residual, so the original
      # sequence can be used
    }
  }
  # returns the sequence
  return(sequence)
}


#' @title Create sub-objects for each object in a dataframe (internal)
#'
#' @description Requires a list with named vectors as [datsteps()] will
#' hand to the function.
#'
#' @param DAT_list a list as prepared by [datsteps()]
#' @inheritParams datsteps
#'
#' @return an expanded list of the same structure to be further processed by
#' [datsteps()] each object duplicated according to the number of steps required
#'
#' @keywords internal

create.sub.objects <- function(DAT_list,
                               stepsize,
                               calc = "weight",
                               cumulative = FALSE) {

  diffs <- unlist(lapply(DAT_list, function(x) x["datmax"] - x["datmin"]))

  switch (calc,
          weight = diffs[diffs == 0] <- 1,
          probability = diffs <- diffs + 1
  )


  if (any(diffs < stepsize)) {
    warning(paste0("stepsize is larger than the range of the ",
                   "closest dated object at Index = ",
                   paste(which(diffs < stepsize), collapse = ", "), "). ",
                   "For information see documentation of get.step.sequence()."))
  }

  DAT_list <- lapply(DAT_list, function(object) {
    sequence <- get.step.sequence(object["datmin"], object["datmax"],
                                  stepsize)
    new_object <- lapply(sequence, function(step) {
      new_object <- object
      new_object["step"] <- step
      return(new_object)
    })
    names(new_object) <- NULL
    new_object <- do.call(rbind, new_object)
    if (cumulative) {
      cumul_prob <- cumsum(new_object[,calc])
      new_object <- cbind(new_object, cumul_prob)
    }
    return(new_object)
  })



  result <- do.call(rbind, DAT_list)

  switch(calc,
         weight = attr <- "Calculated weight of each object according to doi.org/10.1017/aap.2021.8",
         probability = attr <- "Dating-Probability of each object")

  attributes(result)$calc <- c(calc, attr)

  return(result)
}


#' @title Check for numbers (internal)
#'
#' @description Checks if value is either numeric, integer or double
#' and and returns TRUE.
#'
#' @param value A value to check
#'
#' @return TRUE if value is any kind of number, FALSE if value is not
#'
#' @keywords internal


check.number <- function(value) {
  # This is so useless. :)
  result <- c(is.integer(value), is.numeric(value), is.double(value))
  result <- any(result)
  return(result)
}

#' @title Check if the structure is compatible with [datsteps()]  (internal)
#'
#' @description Checks if the object passed to [datsteps()] can be used for
#' processing.
#'
#' @param DAT_df An object to check
#'
#' @return TRUE if object can be processed by [datsteps()], error / FALSE if not
#'
#' @keywords internal

check.structure <- function(DAT_df) {
  dat_df_structure <- c(NA, NA, NA, NA, NA)
  names(dat_df_structure) <- c("is.df", "is.id", "is.var",
                               "is.minDAT", "is.maxDAT")
  # Todo
  dat_df_structure["is.df"] <- is.data.frame(DAT_df)
  dat_df_structure["is.id"] <- is.character(DAT_df[, 1, drop = TRUE])
  dat_df_structure["is.var"] <- is.factor(DAT_df[, 2, drop = TRUE])
  dat_df_structure["is.minDAT"] <- check.number(DAT_df[, 3, drop = TRUE])
  dat_df_structure["is.maxDAT"] <- check.number(DAT_df[, 4, drop = TRUE])


  if (dat_df_structure[1] == FALSE) {
    result <- FALSE
    stop("datsteps requires an object of class data.frame")
  } else {
    result <- TRUE
    }
  if (any(dat_df_structure[c("is.minDAT", "is.maxDAT")] == FALSE)) {
    result <- FALSE
    stop("The 3rd and 4th columns of your data.frame have to be numeric.")
  } else {
    result <- TRUE
    }
  if (any(dat_df_structure[2:3] == FALSE)) {
    message("It is recommended to use character vector for the ID column and factor for the variable column.")
  }
  return(result)
}
