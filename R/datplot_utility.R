#' @title Determine stepsize
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

#' @title Switch values where dating is in wrong order
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


#' @title Calculate the weights for each dated object
#'
#' @description Calculates the weights from two vectors of minimum and maximum dating for each object.
#' Returns a dataframe with the weight in the first column and FALSE in the second if two rows have the
#' same value in both min and max dating.
#'
#' @param DAT_min a vector containing the minimum date (int/num) of each object
#' @param DAT_max a vector containing the maximum date (int/num) of each object
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
    weights[which(weights[,1] == 0),2] <- FALSE
    weights[which(weights[,1] == 0),1] <- 1
  }
  weights[,1] <- 1/weights[,1]
  if (any(weights[,2] == FALSE)) {
    warning(paste("Warning: DAT_min and DAT_max at Index: ", paste(which(weights[,2] == FALSE), collapse = ", "), ")",
                  " have the same value! Is this correct? Please check the table for possible errors.", sep = ""))
  }
  return(weights)
}



#' @title Calculate output rows
#'
#' @description approximation :(
#'
#' @param DAT_mat todo
#' @param stepsize todo
#'
#' @return outputrows
#'
#' @export calculate.outputrows



calculate.outputrows <- function(DAT_mat, stepsize) {
  mean_year_index <- which(DAT_mat[,"datmax"] - DAT_mat[,"datmin"] < stepsize)

  if (length(mean_year_index) == 0) {
    outputrows <- ceiling(sum(((abs(DAT_mat[,"datmax"] - DAT_mat[,"datmin"])) / stepsize) + 3))
  } else {
    outputrows <- ceiling(sum(((abs(DAT_mat[-mean_year_index,"datmax"] - DAT_mat[-mean_year_index,"datmin"]))/stepsize)+3))
    outputrows <- outputrows+length(mean_year_index)
  }
  return(outputrows)
}




#' @title Calculate the sequence of dating steps
#'
#' @description TO DO also i dont want to write documentation
#'
#' @param datmin todo
#' @param datmax todo
#' @param stepsize todo
#'
#' @return sequence
#'
#' @export get.step.sequence





### THIS IS NOT DONE
get.step.sequence <- function(datmin = 0, datmax = 100, stepsize = 25) {
  timespan <- datmax - datmin
  if (timespan %/% stepsize == 0) {
    if (timespan > (stepsize*0.6)) {
      sequence <- c(datmin, round(((datmin+datmax)/2), digits = 0), datmax)
    } else {
      sequence <- c(datmin, datmax)
    }
  } else {
    sequence <- seq(from = datmin, to = datmax, by = stepsize)
    resid <- datmax-sequence[length(sequence)]
    if (resid >= (stepsize/2)) {
      stepsize_mod <- (datmax-datmin)/length(sequence)
      sequence <- seq(datmin, datmax, stepsize_mod)
      sequence[-c(1,length(sequence))] <- round(sequence[-c(1,length(sequence))], digits = 0)
    } else if (resid != 0) {
      move <- round(resid/(length(sequence)-1), digits = 0)
      sequence[2:length(sequence)] <- sequence[2:length(sequence)] + move
      sequence[length(sequence)] <- datmax
    } else {
    }
  }
  return(sequence)
}









#' @title Create sub-objects for each object in a dataframe
#'
#' @description Requires a dataframe with 5 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric), maximum date (int/numeric) and weight (as created by get.weights). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param DAT_mat a matrix with 3 variables: ID, group, minimum date (int/num) maximum date (int/num), _must_ be in this order, colnames are irrelevant; each object _must_ be one row.
#' @param stepsize Number of years that should be used as an interval for creating dating steps.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @export create.sub.objects

create.sub.objects <- function(DAT_mat, stepsize) {

  outputrows <- calculate.outputrows(DAT_mat, stepsize)
  result <- as.data.frame(matrix(ncol = 6, nrow = outputrows+100))

  diffs <- DAT_mat[,"datmax"]-DAT_mat[,"datmin"]

  if (any(diffs < stepsize)) {
    diffs <- diffs[diffs < stepsize]
    warning(paste("stepsize is larger than the range of the closest dated object at Index = ",
                paste(which(diffs < stepsize), collapse = ", "), "). Using mean as year.", sep = ""))
  }

  for (i in 1:nrow(DAT_mat)) {
    sequence <- NULL
    sequence <- get.step.sequence(DAT_mat[i,"datmin"], DAT_mat[i,"datmax"], stepsize)
    for (step in sequence) {
      wip <- as.vector(DAT_mat[i,])
      wip[5] <- step
      wip[4] <- wip[4] #/ length(sequence)
      first_na <- match(NA, result[,1])
      result[first_na,1] <- wip[1]
      result[first_na,3] <- wip[2]
      result[first_na,4] <- wip[3]
      result[first_na,5] <- wip[4]
      result[first_na,6] <- wip[5]
    }
  }
  result <- result[-c(match(NA, result[,1]):nrow(result)), ]
  return(result)
}

#' @title Check for numbers
#'
#' @description Checks if value is either numeric, integer or double and and returns TRUE.
#'
#' @param value A value to check
#'
#' @return TRUE if value is any kind of number, FALSE if value is not
#'
#' @export check.number


check.number <- function(value) {
  result <- is.integer(value)
  if (result == FALSE) {
    result <- is.numeric(value)
    if  (result == FALSE) {
      result <- is.double(value)
    }
  }
  return(result)
}

#' @title Check the Structure to be compatible with datsteps()
#'
#' @description Checks if the object passed to datsteps() will work
#'
#' @param DAT_df A value to check
#'
#' @return TRUE if df can be processen, FALSE if not
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


