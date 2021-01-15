#' @title Determine stepsize (internal)
#'
#' @description Determines stepsize by selecting the absolute minimum value between the upper and lower end of all dating ranges.
#'
#' @param DAT_mat a matrix as prepared by datsteps(), resp. a matrix witch columns "datmin" and "datmax" containing numeric/integer value of the dating ranges.
#'
#' @return stepsize
#'
#' @export generate.stepsize

generate.stepsize <- function(DAT_mat) {
  timespans <- abs(DAT_mat[,"datmin"] - DAT_mat[,"datmax"])
  stepsize <- min(timespans)
  if(stepsize < 1) {
    stepsize <- 1
  }
  print(paste("Using stepsize = ", stepsize, " (auto).", sep = ""))
  return(stepsize)
}

#' @title Switch values where dating is in wrong order (internal)
#'
#' @description Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric) and DAT_err as a vector of indizes where
#' dating is in wrong order.
#'
#' @param DAT_df a dataframe with 4 variable: ID, group, minimum date (int/num) maximum date (int/num)
#' @param DAT_err a vector containing the indizes of the dates which are in wrong order
#'
#' @return corrected DAT_mat
#'
#' @export switch.dating

switch.dating <- function(DAT_df, DAT_err) {
  DAT_df[DAT_err,3:4] <- DAT_df[DAT_err,4:3]
  return(DAT_df)
}


#' @title Calculate the weights for each dated object (internal)
#'
#' @description Calculates the weights from two vectors of minimum and maximum dating for each object.
#' Returns a dataframe with the weight in the first column and FALSE in the second if two rows have the
#' same value in both min and max dating.
#'
#' @param DAT_min a vector containing the minimum date (a number) of each object
#' @param DAT_max a vector containing the maximum date (a number) of each object
#'
#' @return the 'weight' value for the datsteps-dataframe, that is a quantification of
#' how well the object is dated (lesser value means object is dated to larger
#' timespans, i.e. with less confidence)
#'
#' @export get.weights


get.weights <- function(DAT_min, DAT_max) {
  weights <- as.data.frame(matrix(ncol = 2, nrow = length(DAT_min)))
  weights[,1] <- abs(DAT_min - DAT_max)
  weights[,2] <- TRUE
  if (any(weights[,1] == 0)) {
    # store FALSE if any weights are zero to display the warning below
    weights[which(weights[,1] == 0),2] <- FALSE
    # then store a weight of 1 as to treat objects with same min and max dating (dated to one year precisely) as very influential
    weights[which(weights[,1] == 0),1] <- 1
  }
  # weights have to be below 1
  weights[,1] <- 1/weights[,1]
  if (any(weights[,2] == FALSE)) {
    warning(paste("Warning: DAT_min and DAT_max at Index: ", paste(which(weights[,2] == FALSE), collapse = ", "), ")",
                  " have the same value! Is this correct? Please check the table for possible errors.", sep = ""))
  }
  return(weights)
}



#' @title Calculate output rows (internal)
#'
#' @description an approximation(!) of the rows that will be needed to fit all the steps of the dating
#'
#' @param DAT_mat a matrix as transformed by datsteps()
#' @param stepsize the stepsize given to or by datsteps()
#'
#' @return the number of rows create.sub.objects should at least produce in order to fit all steps
#'
#' @export calculate.outputrows


# TODO: as stated, this is still an approximation and overestimates the number. in testing, there were cases of
# underestimating, but it hasnt happened since the last fix

calculate.outputrows <- function(DAT_mat, stepsize) {
  mean_year_index <- which(DAT_mat[,"datmax"] - DAT_mat[,"datmin"] < stepsize)

  if (length(mean_year_index) == 0) {
    outputrows <- ceiling(sum(((abs(DAT_mat[,"datmax"] - DAT_mat[,"datmin"])) / stepsize) + 3))
  } else {
    outputrows <- ceiling(sum(((abs(DAT_mat[-mean_year_index,"datmax"] - DAT_mat[-mean_year_index,"datmin"]))/stepsize)+3))
    outputrows <- outputrows + length(mean_year_index)
  }
  return(outputrows)
}




#' @title Calculate the sequence of dating steps (internal)
#'
#' @description Produces an appropriate sequence of years between the minimum and maximum dating.
#' If they cannot be properly devided by the stepsize set beforehand, either three values are generated for
#' objects that are dated to a range of more then 60% of the stepsize (min, mean, max), or two values for
#' objects dated to a timespan of less or equal to 60% of the stepsize.
#' If they can be devided without residual, the normal sequence is returned. If there is a residual, the stepsize
#' is modified depending on how large the residual is. (TODO: There is still a problem here that needs fixing.)
#'
#' @param datmin value of the minimum dating of one object
#' @param datmax value of the maximum dating of one object
#' @param stepsize the stepsize to be used
#'
#' @return sequence of steps to be created by create.sub.objects()
#'
#' @export get.step.sequence

get.step.sequence <- function(datmin = 0, datmax = 100, stepsize = 25) {
  # Get the difference of the two dating values
  timespan <- datmax - datmin

  # First: If the stepsize is larger than the timespan, two different strategies can be employed
  if (timespan %/% stepsize == 0) {
    if (timespan > (stepsize*0.6)) {
      # If the timespan exceeds 60% of the stepsize, three steps will be created corresponding to minimum, mean and maximum dating
      sequence <- c(datmin, round(((datmin+datmax)/2), digits = 0), datmax)
    } else if (timespan == 0) {
      # for objects dated to one year, only use one year!
      sequence <- datmin
    } else {
      # if the timespan is less than 60% of the stepsize, just two values corresponding to minimum and maximum dating will be returned
      sequence <- c(datmin, datmax)
    }
  } else {
    # If the timespan can be devided at least once, first generate the sequence
    sequence <- seq(from = datmin, to = datmax, by = stepsize)
    # then check how many years the maximum dating would be off
    resid <- datmax-sequence[length(sequence)]
    if (resid >= (stepsize/2)) {
      # if the residual is larger or equals half the stepsize, the stepsize is temporarily modified to fit the as many values
      # as it would with the length of the sequence generated
      stepsize_mod <- (datmax-datmin)/(length(sequence)+1) ## length(sequence + 1 might be better!)
      sequence <- seq(datmin, datmax, stepsize_mod)
      # then rounds all values except first and last, which need to stay as minumum and maximum date
      sequence[-c(1,length(sequence))] <- round(sequence[-c(1,length(sequence))], digits = 0)
    } else if (resid != 0) {
      # if the residual is smaller but also not 0, the sequence values at moved by an appropriate fraction
      move <- round(resid/(length(sequence)-1), digits = 0)
      sequence[2:length(sequence)] <- sequence[2:length(sequence)] + move
      # and the end of the sequence is reset as the maximum dating
      sequence[length(sequence)] <- datmax
      # TODO: these two things do essentially the same? I need to fix the first one to use the largest possible division, maybe
    } else {
      # this implies that there was no residual, so the original sequence can be used
    }
  }
  # returns the sequence
  return(sequence)
}









#' @title Create sub-objects for each object in a dataframe (internal)
#'
#' @description Requires a matrix with 4 named columns as datsteps will hand to the function:
#' * "index" (identifier so the values can later be reassigned to their ID and variable),
#' * "datmin" (minimum dating as any kind of number),
#' * "datmax" (maximum dating as any kind of number),
#' * "weight" (as created by get.weights),
#' * "step" (empty).
#' It's expected that dates BCE are displayed as negative values while dates CE are positive values.
#' Ignoring this will cause problems in any case, that would be fixed automatically by switch.dating().
#'
#' @param DAT_mat a matrix with 3 variables as prepared by datsteps()
#' @param stepsize Number of years that should be used as an interval for creating dating steps.
#'
#' @return a longer matrix of the same structure to be further processed by datsteps() with a number of steps for each object
#'
#' @export create.sub.objects

create.sub.objects <- function(DAT_mat, stepsize) {

  outputrows <- calculate.outputrows(DAT_mat, stepsize)
  result <- as.data.frame(matrix(ncol = 6, nrow = outputrows+100))

  diffs <- DAT_mat[,"datmax"]-DAT_mat[,"datmin"]

  if (any(diffs < stepsize)) {
    diffs <- diffs[diffs < stepsize]
    warning(paste("stepsize is larger than the range of the closest dated object at Index = ",
                paste(which(diffs < stepsize), collapse = ", "), "). For information see documentation of get.step.sequence().", sep = ""))
  }

  for (i in 1:nrow(DAT_mat)) {
    sequence <- NULL
    sequence <- get.step.sequence(DAT_mat[i,"datmin"], DAT_mat[i,"datmax"], stepsize)

    first_na <- match(NA, result[,1])
    last_row <- first_na + (length(sequence)-1)
    result[first_na:last_row,1] <- DAT_mat[i,1]
    result[first_na:last_row,3] <- DAT_mat[i,2]
    result[first_na:last_row,4] <- DAT_mat[i,3]
    result[first_na:last_row,5] <- DAT_mat[i,4]
    result[first_na:last_row,6] <- sequence
  }
  result <- result[-c(match(NA, result[,1]):nrow(result)), ]
  return(result)
}

#' @title Check for numbers (internal)
#'
#' @description Checks if value is either numeric, integer or double and and returns TRUE.
#'
#' @param value A value to check
#'
#' @return TRUE if value is any kind of number, FALSE if value is not
#'
#' @export check.number


check.number <- function(value) {
  result <- c(is.integer(value), is.numeric(value), is.double(value))
  result <- any(result)
  return(result)
}

#' @title Check the Structure to be compatible with datsteps()  (internal)
#'
#' @description Checks if the object passed to datsteps() will work
#'
#' @param DAT_df An object to check
#'
#' @return TRUE if object can be processed by datsteps(), error / FALSE if not
#'
#' @export check.structure

check.structure <- function(DAT_df) {
  dat_df_structure <- c(NA, NA, NA, NA, NA)
  names(dat_df_structure) <- c("is.df", "is.id", "is.var", "is.minDAT", "is.maxDAT")

  dat_df_structure["is.df"] <- is.data.frame(DAT_df)
  dat_df_structure["is.id"] <- is.character(DAT_df[,1])
  dat_df_structure["is.var"] <- is.factor(DAT_df[,2])
  dat_df_structure[c("is.minDAT", "is.maxDAT")] <- c(check.number(DAT_df[,3]), check.number(DAT_df[,4]))

  if (dat_df_structure[1] == FALSE) {
    result <- FALSE
    stop("datsteps requires an object of class data.frame")
  } else { result <- TRUE }
  if (any(dat_df_structure[4:5] == FALSE)) {
    result <- FALSE
    stop("The 3rd or 4th columns of your data.frame are not numbers.")
  } else { result <- TRUE }
  if (any(dat_df_structure[2:3] == FALSE)) {
    warning("It is recommended to use character vector for the ID column and factor for the variable column.")
  }
  return(result)
}


