#' @title Scales the content of a column according to group membership
#'
#' @description Requires a dataframe with one variable and one value column.
#'
#' @param DAT_df a dataframe
#' @param var the index of the column that should be used
#' as the group variable, OR "all" (note: all non-numeric values will
#' result in the weight being scaled across all objects)
#' @param val the column that should be scaled (value / sum(values))
#'
#' @return the same dataframe, with scaled values in the specifies column
#'
#' @export scaleweight

scaleweight <- function(DAT_df, var = c("all", 2), val = 5 ) {
  if (check.number(var)) {
    if (check.number(val)) {
      uvar <- unique(DAT_df[,var])
      for (row in 1:length(uvar)) {
        index <- which(DAT_df[,var] == uvar[row])
        DAT_df[index,val] <-  DAT_df[index,val] / sum(DAT_df[index,val])
      }
      attr(DAT_df[,val], "descr") <- "weight (scaled to sum of objects grouped by variable)"
      } else { stop("val needs to be of a number (the index of the column that should be scaled)")}
    } else {
    DAT_df[,val] <- DAT_df[,val] / sum(DAT_df[,val])
    attr(DAT_df[,val], "descr") <- "weight (scaled to sum of all objects)"
  }
return(DAT_df)
}
