#' datplot Testing data
#'
#' A test dataset containing a data.frame how it should ideally be arranged to work with datplot.
#' Data at not real and illustrate some common problems such as lower and upper dating in the wrong columns.
#'
#' \itemize{
#'   \item ID. Identifier of the Objects (has to be unique)
#'   \item carat. Grouping variable, such as a Type or a Findspot
#'   \item DAT_min. Integer: lower range of the dating, BCE in negative numbers
#'   \item DAT_max. Integer: uppder range of the dating, BCE in negative numbers
#' }
#'
#' @docType data
#' @keywords datasets
#' @name DAT_df
#' @usage data(DAT_df)
#' @format A data frame with 10000 rows and 4 variables

"DAT_df"
NULL
