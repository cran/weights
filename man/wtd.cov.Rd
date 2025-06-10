\name{wtd.cov}
\alias{wtd.cov}
\title{
Produces weighted covariances with standard errors and significance.
}
\description{
\code{wtd.cov} produces a covariance matrix comparing two variables or matrices, using a set of weights. Standard errors, t-values, and p-values are estimated via a regression-based approach. If no weights are provided, unweighted covariance is returned.
}
\usage{
wtd.cov(x, y=NULL, weight=NULL, collapse=TRUE)
}
\arguments{
  \item{x}{
A matrix or vector of values to be compared. If \code{y} is \code{NULL}, \code{x} will be used for both variables.
}
  \item{y}{
A vector or matrix to be compared with \code{x}. Defaults to \code{NULL}.
}
  \item{weight}{
Optional weights used to compute the weighted covariance. If \code{NULL}, equal weighting is assumed.
}
  \item{collapse}{
Logical indicator for whether the results should be simplified when the output is a vector.
}
}
\value{
A list containing:
\itemize{
  \item{\code{covariance} — Weighted covariance matrix}
  \item{\code{std.err} — Standard error of the covariance estimate}
  \item{\code{t.value} — T-statistic associated with the covariance}
  \item{\code{p.value} — P-value for the t-statistic}
}
If the results are scalar or one-dimensional, a simplified matrix will be returned.
}
\author{
Josh Pasek, Professor of Communication & Media and Political Science at the University of Michigan (\url{https://www.joshpasek.com})
}
\seealso{
\code{\link{wtd.cor}}, \code{\link{wtd.partial.cov}}, \code{\link{onecor.wtd}}, \code{\link[Hmisc]{wtd.var}}, \code{\link{stdz}}
}
\examples{
x <- c(1, 2, 3, 4)
y <- c(2, 4, 6, 8)
w <- c(1, 2, 1, 1)

wtd.cov(x, y)
wtd.cov(x, y, weight = w)
}
\keyword{ ~covariance }
\keyword{ ~weights }