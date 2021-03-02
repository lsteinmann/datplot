#' datplot Testing data
#'
#' A test dataset containing a data.frame how it should ideally be arranged
#' to work with datplot. Data are not real and illustrate some common problems
#' such as lower and upper dating in the wrong columns.
#'
#' \itemize{
#'   \item ID. Identifier of the Objects (has to be unique)
#'   \item var. Grouping variable, such as a Type or a Findspot
#'   \item DAT_min. Integer: lower range of the dating, BCE in negative numbers
#'   \item DAT_max. Integer: upper range of the dating, BCE in negative numbers
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
#' A test dataset containing a data.frame how it should ideally be arranged
#' to work with datplot.Data are gathered from the Beazley Archive Pottery
#' Database (BAPD) -- https://www.beazley.ox.ac.uk/pottery/default.htm and
#' transformed to work with datplot
#'
#' \itemize{
#'   \item Identifier (Vase.Number in BAPD)
#'   \item Technique: Sample contains only red- or blackfigured objects
#'   \item DAT_min. Integer: lower range of the dating, BCE in negative numbers
#'   \item DAT_max. Integer: upper range of the dating, BCE in negative numbers
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


#' @title Inscr_Bithynia
#' @description The data set was gathered by Barbora Weissova and published
#' as part of her dissertation “Regional Economy, Settlement Patterns and the
#' Road System in Bithynia (4th Century BC - 6th Century AD). Spatial and
#' Quantitative Analysis.”.
#'
#'
#' @format A data frame with 2878 rows and 9 variables:
#' \describe{
#'   \item{\code{ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{ikey}}{character ID at \url{https://inscriptions.packhum.org/}
#'   / \url{https://edh-www.adw.uni-heidelberg.de/home}, if available}
#'   \item{\code{Location}}{factor Findspot of the Inscription (City)}
#'   \item{\code{Source}}{character Corpus/Citation of the Inscription}
#'   \item{\code{Dating}}{character Original Chronological Assessment,
#'   may contain inconsistencies}
#'   \item{\code{Language}}{factor Language of the Inscription,
#'   can either be Latin, Greek, or both}
#'   \item{\code{uncertain_dating}}{logical TRUE if Dating is not certain,
#'   FALSE if dating is certain}
#'   \item{\code{DAT_min}}{integer lower border of the dating timespan,
#'   negative values for BCE, positive values for CE}
#'   \item{\code{DAT_max}}{integer upper border of the dating timespan,
#'   negative values for BCE, positive values for CE}
#'   \item{\code{URL}}{Link to the inscription (if available) at
#'   \url{https://inscriptions.packhum.org/} or
#'   \url{https://edh-www.adw.uni-heidelberg.de/home}}
#'}
#' @source Weissova, Barbora. 2019. “Regional Economy, Settlement Patterns and
#' the Road System in Bithynia (4th Century BC - 6th Century AD). Spatial and
#' Quantitative Analysis.” Dissertation, Berlin: Freie Universität Berlin.
#' \url{https://refubium.fu-berlin.de/handle/fub188/23730},
#' partially after \url{https://inscriptions.packhum.org/}
"Inscr_Bithynia"
