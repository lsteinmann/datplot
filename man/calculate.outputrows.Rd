% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datplot_utility.R
\name{calculate.outputrows}
\alias{calculate.outputrows}
\title{Calculate output rows (internal)}
\usage{
calculate.outputrows(DAT_mat, stepsize)
}
\arguments{
\item{DAT_mat}{a matrix as transformed by datsteps()}

\item{stepsize}{the stepsize given to or by datsteps()}
}
\value{
the number of rows create.sub.objects should at least produce in
order to fit all steps
}
\description{
an approximation(!) of the rows that will be needed to fit
all the steps of the dating
}
