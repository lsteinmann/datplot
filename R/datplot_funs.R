#' Create 'steps' of dates for each object in a dataframe
#'
#' Requires a dataframe with 4 variables: ID (ideally factor), group (ideally factor),
#' minimum date (int/numeric) and maximum date (int/numeric). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive values. Ignoring this will cause problems
#' in any case.
#'
#' @param df a dataframe with 4 variable: ID, group, minimum date (int/num) maximum date (int/num), _must_ be in this order, colnames are irrelevant; each object _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be used as an interval for creating dating steps.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @export datsteps

datsteps <- function(df, stepsize = 5) {
  result <- as.data.frame(NULL)
  if (any(df[,3] > df[,4])) {
    print(paste("Error: Dating seems to be in wrong order at ",
                df[which(df[,3] > df[,4]),1],
                " (Index: ", which(df[,3] > df[,4]), ")",
                ". Please supply minimum date in 3rd Column, maximum date in 4th.", sep = ""))
  } else {
    df$weight <- abs(df[,3] - df[,4])
    if (any(df$weight == 0)) {
      print(paste("Warning: DAT_min and DAT_max in ",
                  df[which(df$weight == 0),1],
                  " (Index: ", which(df$weight == 0), ")",
                  " have the same value! Is this correct? Please check the table for possible errors.", sep = ""))
      df$weight[which(df$weight == 0)] <- 1
    }
    df$weight <- 1/df$weight
    for (i in 1:nrow(df)) {
      sequence <- NULL
      sequence <- seq(df[i,3], df[i,4], by = stepsize)
      length <- length(sequence)
      for (zahl in sequence) {
        wip <- df[i,]
        wip$DAT_Step <- zahl
        wip$weight <- wip$weight / length(sequence)
        result <- rbind(result, wip)
      }
    }
  }
  return(result)
}



#' Scales the content of the weight columns according to group membership
#'
#' Requires a dataframe as produced by datsteps(). (Meaning 6 columns in the following order: ID, group, minimum/earliest date, maximum/latest date, weight, 'DAT_Steps')
#'
#' @param df a dataframe as returned by datsteps
#' @param var the column of said dataframe that should be used as the group variable
#'
#' @return the same dataframe, with the 'weight'-values scaled along group membership
#'
#' @export scaleweight

scaleweight <- function(df, var) {
  res_df <- data.frame(NULL)
  uvar <- unique(var)
  for (row in 1:length(uvar)) {
    wip <- df[which(var == uvar[row]),]
    wip$weight <- wip$weight / sum(wip$weight)
    res_df <- rbind(res_df, wip)
  }
  return(res_df)
}
