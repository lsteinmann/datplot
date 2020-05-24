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
  mean_year_index <- which(DAT_mat[,"datmax"]-DAT_mat[,"datmin"] < stepsize)

  if (length(mean_year_index) == 0) {
    outputrows <- ceiling(sum(((abs(DAT_mat[,"datmin"]-DAT_mat[,"datmax"]))/stepsize)+1))
  } else {
    outputrows <- ceiling(sum(((abs(DAT_mat[-mean_year_index,"datmin"]-DAT_mat[-mean_year_index,"datmax"]))/stepsize)+1))
    outputrows <- outputrows+length(mean_year_index)
  }
  return(outputrows)
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
    if ((DAT_mat[i,"datmax"]-DAT_mat[i,"datmin"]) < stepsize) {
      sequence <- (DAT_mat[i,"datmin"]+DAT_mat[i,"datmax"])/2
    } else {
      sequence <- seq(DAT_mat[i,"datmin"], DAT_mat[i,"datmax"], by = stepsize)
    }
    length <- length(sequence)
    for (step in sequence) {
      wip <- as.vector(DAT_mat[i,])
      wip[5] <- step
      wip[4] <- wip[4] / length(sequence)
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


#' @title Scales the content of the weight columns according to group membership
#'
#' @description Requires a dataframe as produced by datsteps(). (Meaning 6 columns in the following order: ID, group, minimum/earliest date, maximum/latest date, weight, 'DAT_Steps')
#'
#' @param DAT_mat a dataframe as returned by datsteps
#' @param var the index of the column of said dataframe that should be used as the group variable, OR "all" (note: all non-numeric values will result in the weight being scaled accross all objects)
#'
#' @return the same dataframe, with scaled 'weight'-values
#'
#' @export scaleweight

scaleweight <- function(DAT_mat, var = c("all", 2) ) {
  res_DAT_mat <- data.frame(NULL)
  if (is.numeric(var)) {
    uvar <- unique(DAT_mat[,var])
    for (row in 1:length(uvar)) {
      wip <- DAT_mat[which(DAT_mat[,var] == uvar[row]),]
      wip$weight <- wip$weight / sum(wip$weight)
      res_DAT_mat <- rbind(res_DAT_mat, wip)
    }
  } else {
    DAT_mat$weight <- DAT_mat$weight / sum(DAT_mat$weight)
    res_DAT_mat <- DAT_mat
  }
  return(res_DAT_mat)
}
