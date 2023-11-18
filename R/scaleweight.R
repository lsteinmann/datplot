#' @title Scales the content of a column
#'
#' @description Requires a data.frame with one variable and one value column.
#'
#' @param DAT_df a data.frame
#' @param var index or name of the column that should be used
#' as the group variable, OR "all"
#' @param val index or name of the column that should be
#' scaled (has to be numeric)
#'
#' @return the same data.frame, with the scaled values in the specified column
#'
#' @export scaleweight
#'
#' @examples
#' data("Inscr_Bithynia")
#' DAT_df <- Inscr_Bithynia[, c("ID", "Location", "DAT_min", "DAT_max")]
#' DAT_df_steps <- datsteps(DAT_df, stepsize = 25)
#' DAT_df_scaled <- scaleweight(DAT_df_steps, var = 2, val = 5)

scaleweight <- function(DAT_df, var = "all", val = 5) {

  if (!is.numeric(val)) {
    val <- which(colnames(DAT_df) == val)
    if (length(val) == 0) {
      stop(paste("'val' needs to be a number",
                 "(the index of the column that should be scaled)"))
    }
  }
  if (val > ncol(DAT_df)) {
    stop(paste("No column at index", val))
  }
  if (!is.numeric(DAT_df[, val])) {
    stop(paste("Column", val, "is not numeric."))
  }

  if (is.character(var) && var != "all") {
    var <- which(colnames(DAT_df) == var)
    if (length(var) == 0) {
      stop(paste("var needs to be either 'all' or the index of the",
                 "column containing the variable",
                 "that is to be used for scaling"))
    }
  } else if (is.numeric(var) && var > ncol(DAT_df)) {
    stop(paste("No column at index", var))
  }


  if (var == "all") {
    DAT_df[, val] <- DAT_df[, val] / sum(DAT_df[, val])
    new_desc <- paste(attr(DAT_df[, val], "descr"),
                      "(scaled to sum of all objects)")
  } else {
    uvar <- unique(DAT_df[, var])
    for (row in seq_len(length(uvar))) {
      index <- which(DAT_df[, var] == uvar[row])
      DAT_df[index, val] <-  DAT_df[index, val] / sum(DAT_df[index, val])
    }
    new_desc <- paste0(attr(DAT_df[, val], "descr"),
                       " (scaled to sum of objects grouped by column '",
                       colnames(DAT_df)[var], "')")
  }
  attr(DAT_df[, val], "descr") <- new_desc

  return(DAT_df)
}
