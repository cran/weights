\name{wtd.quantile}
\alias{wtd.quantile}
\title{Weighted quantiles}
\description{
\code{wtd.quantile} computes quantiles from a numeric vector using observation weights.
}
\usage{
wtd.quantile(x, weight = NULL, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
}
\arguments{
  \item{x}{Numeric vector of values.}
  \item{weight}{Optional numeric vector of weights.}
  \item{probs}{Numeric vector of quantile probabilities (between 0 and 1).}
  \item{na.rm}{Logical. If \code{TRUE}, removes missing values.}
}
\value{
A named numeric vector of quantiles.
}
\seealso{
\code{\link{quantile}}, \code{\link{wtd.median}}
}
\examples{
x <- c(1, 2, 3, 4, 5)
w <- c(1, 1, 5, 1, 1)
wtd.quantile(x, weight = w)
}
\author{Josh Pasek}
\keyword{weights}
\keyword{quantiles}