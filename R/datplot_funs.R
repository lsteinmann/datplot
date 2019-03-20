#' Create 'steps' of dates for each object in a dataframe
#'
#' Requires a dataframe with 4 variable: one ID (ideally factor), one group (ideally factor),
#' a minimum date (int/numeric) and a maximum date (int/numeric). It's expected that dates BCE are
#' displayed as negative values while dates CE are positive. Ignoring this will cause problems when
#' crossing 0.
#'
#' @param df a dataframe with 4 variable: ID, group, minimum date (num) maximum date (num), _must_ be in this order, colnames are irrelevant; each objects _must_ be one row.
#' @param stepsize defaults to 5. Number of years that should be considered a timestap/datestep.
#'
#' @return a larger dataframe with a number of steps for each object as well as a 'weight' value, that is a quantification of how well the object is dated (lesser value means object is dated to larger timespans, i.e. with less confidence)
#'
#' @export datsteps

datsteps <- function(df, stepsize = 5) {
  result <- as.data.frame(NULL)
  wip_data <- df
  wip_data$weight <- abs(df[,3] - df[,4])
  wip_data$weight <- 1/wip_data$weight
  for (i in 1:nrow(wip_data)) {
    sequence <- NULL
    sequence <- seq(wip_data[i,3], wip_data[i,4], by = stepsize)
    length <- length(sequence)
    for (zahl in sequence) {
      wip_sec <- wip_data[i,]
      wip_sec$DAT_Step <- zahl
      wip_sec$weight <- wip_sec$weight / length(sequence)
      result <- rbind(result, wip_sec)
    }
  }
  return(result)
}



#' Scales the content of the weight columns according to group membership
#'
#' Requires a dataframe as produces by datsteps(). (Meaning 6 columns in the following order: ID, group, minimum/earliest date, maximum/latest date, weight, 'DAT_Steps')
#'
#' @param df a dataframe as returned by datsteps
#' @param var the columns of said dataframe that should be used as the group variable
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
