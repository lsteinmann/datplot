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
