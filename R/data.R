#' datplot Testing data
#'
#' A test dataset containing a data.frame how it should ideally be arranged to work with datplot.
#' Data are not real and illustrate some common problems such as lower and upper dating in the wrong columns.
#'
#' \itemize{
#'   \item ID. Identifier of the Objects (has to be unique)
#'   \item var. Grouping variable, such as a Type or a Findspot
#'   \item DAT_min. Integer: lower range of the dating, BCE in negative numbers
#'   \item DAT_max. Integer: uppder range of the dating, BCE in negative numbers
#' }
#'
#' @docType data
#' @keywords datasets
#' @name DAT_df
#' @usage data(DAT_df)
#' @format A data frame with 5000 rows and 4 variables

"DAT_df"
NULL


#' Beazley (sample of 1000)
#'
#' A test dataset containing a data.frame how it should ideally be arranged to work with datplot.
#' Data are gathered from the Beazley Archive Pottery Database (BAPD) -- https://www.beazley.ox.ac.uk/pottery/default.htm
#' and transformed to work with datplot
#'
#' \itemize{
#'   \item Identifier (Vase.Number in BAPD)
#'   \item Technique: Sample contains only red- or blackfigured objects
#'   \item DAT_min. Integer: lower range of the dating, BCE in negative numbers
#'   \item DAT_max. Integer: uppder range of the dating, BCE in negative numbers
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Beazley
#' @usage data(Beazley)
#' @format A data frame with 1000 rows and 4 variables
#' @source https://www.beazley.ox.ac.uk/pottery/default.htm

"Beazley"
NULL
