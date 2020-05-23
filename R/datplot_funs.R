#' Create 'steps' of dates for each object in a dataframe
#'
#' Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param df a dataframe with 4 variables: ID, group, minimum date (int/num) maximum date (int/num), _must_ be in this order, colnames are irrelevant; each object _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be used as an interval for creating dating steps.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @export datsteps


datsteps <- function(df, stepsize = 25) {
  result <- as.data.frame(NULL)
  if (stepsize == "auto") {
    timespans <- abs(df[,3] - df[,4])
    stepsize <- generate.stepsize(timespans)
  } else if (!is.numeric(stepsize)) {
    stop(error("stepsize has to be either 'auto' or numeric."))
  }

  if (any(df[,3] > df[,4])) {
    warning(paste("Warning: Dating seems to be in wrong order at ID ", paste(df[which(df[,3] > df[,4]),1], collapse = ", "), " (Index: ",
                  paste(which(df[,3] > df[,4]), collapse = ", "), ")",
                  ". Dates have been switched, but be sure to check your original data for possible mistakes.", sep = ""))
    DAT_err <- which(df[,3] > df[,4])
    df <- switch.dating(df, DAT_err)
  }

  weights <- get.weights(df[,3], df[,4])

  df$weight <- weights[,1]
  result <- create.sub.objects(df, stepsize)
  return(result)
}


#' Generate stepsize
#'
#' Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric).
#'
#' @param df a dataframe with 4 variable: ID, group, minimum date (int/num) maximum date (int/num)
#'
#' @return stepsize
#'
#' @export switch.dating

generate.stepsize <- function(timespans) {
  stepsize <- min(abs(df[,4] - df[,3]))
  if(stepsize < 1) {
    stepsize <- 1
  }
  print(paste("Using stepsize = ", stepsize, " (auto).", sep = ""))
  return(stepsize)
}

#' Switch values where dating is in wrong order
#'
#' Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric).
#'
#' @param df a dataframe with 4 variable: ID, group, minimum date (int/num) maximum date (int/num)
#' @param DAT_err a vector containing the dates in wrong order
#'
#' @return corrected df
#'
#' @export switch.dating

switch.dating <- function(df, DAT_err) {
  df[DAT_err,3:4] <- df[DAT_err,4:3]
  return(df)
}


#' Calculate the weights for each dated object
#'
#' Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param DAT_min a vector containing the minimum date (int/num) of each object
#' @param DAT_max a vector containing the maximum date (int/num) of each object
#'
#' @return the 'weight' value for the datsteps-dataframe, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
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
    warning(paste("Warning: DAT_min and DAT_max in ID ",
                  paste(df[which(weights[,2] == FALSE),1], collapse = ", "),
                  " (Index: ", paste(which(weights[,2] == FALSE), collapse = ", "), ")",
                  " have the same value! Is this correct? Please check the table for possible errors.", sep = ""))
  }
  return(weights)
}




#' Create sub-objects for each object in a dataframe
#'
#' Requires a dataframe with 5 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric), maximum date (int/numeric) and weight (as created by get.weights). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param df a dataframe with 4 variable: ID, group, minimum date (int/num) maximum date (int/num), _must_ be in this order, colnames are irrelevant; each object _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be used as an interval for creating dating steps.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @export create.sub.objects

create.sub.objects <- function(df, stepsize) {

  mean_year_index <- which(df[,4]-df[,3] < stepsize)

  if (length(mean_year_index) == 0) {
    outputnr <- ceiling(sum(((abs(df[,3]-df[,4]))/stepsize)+1))
  } else {
    outputnr <- ceiling(sum(((abs(df[-mean_year_index,3]-df[-mean_year_index,4]))/stepsize)+1))
    outputnr <- outputnr+length(mean_year_index)
  }

  result <- as.data.frame(matrix(ncol = ncol(df)+1, nrow = outputnr+100))

  colnames(result) <- c(colnames(df), "DAT_step")
  diffs <- df[,4]-df[,3]

  if (any(diffs < stepsize)) {
    diffs <- diffs[diffs < stepsize]
    warning(paste("stepsize is larger than the range of the closest dated object: ",
                paste(df[which(diffs < stepsize),1], collapse = ", "), " (Index = ",
                paste(which(diffs < stepsize), collapse = ", "), "). Using mean as year.", sep = ""))
  }

  for (i in 1:nrow(df)) {
    sequence <- NULL
    if ((df[i,4]-df[i,3]) < stepsize) {
      sequence <- (df[i,3]+df[i,4])/2
    } else {
      sequence <- seq(df[i,3], df[i,4], by = stepsize)
    }
    length <- length(sequence)
    for (step in sequence) {
      wip <- df[i,]
      wip$DAT_Step <- step
      wip$weight <- wip$weight / length(sequence)
      first_na <- match(NA, result$ID)
      result[first_na,] <- wip[,]
    }
  }
  result <- result[-c(match(NA, result$ID):nrow(result)), ]
  return(result)
}


#' Scales the content of the weight columns according to group membership
#'
#' Requires a dataframe as produced by datsteps(). (Meaning 6 columns in the following order: ID, group, minimum/earliest date, maximum/latest date, weight, 'DAT_Steps')
#'
#' @param df a dataframe as returned by datsteps
#' @param var the index of the column of said dataframe that should be used as the group variable, OR "all" (note: all non-numeric values will result in the weight being scaled accross all objects)
#'
#' @return the same dataframe, with scaled 'weight'-values
#'
#' @export scaleweight

scaleweight <- function(df, var = c("all", 2) ) {
  res_df <- data.frame(NULL)
  if (is.numeric(var)) {
    uvar <- unique(df[,var])
    for (row in 1:length(uvar)) {
      wip <- df[which(df[,var] == uvar[row]),]
      wip$weight <- wip$weight / sum(wip$weight)
      res_df <- rbind(res_df, wip)
    }
  } else {
    df$weight <- df$weight / sum(df$weight)
    res_df <- df
  }
  return(res_df)
}
