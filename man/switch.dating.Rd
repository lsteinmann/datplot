% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datplot_utility.R
\name{switch.dating}
\alias{switch.dating}
\title{Switch values where dating is in wrong order (internal)}
\usage{
switch.dating(DAT_df, DAT_err)
}
\arguments{
\item{DAT_df}{a dataframe with 4 variable: ID, group, minimum date (int/num)
maximum date (int/num)}

\item{DAT_err}{a vector containing the indices of the dates which are
in wrong order}
}
\value{
corrected DAT_mat
}
\description{
Requires a dataframe with 4 variables: ID (ideally factor),
group (ideally factor), minimum date (int/numeric) and
maximum date (int/numeric) and DAT_err as a vector of indices where
dating is in wrong order.
}
