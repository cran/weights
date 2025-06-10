\name{wtd.median}
\alias{wtd.median}
\title{Weighted median}
\description{
\code{wtd.median} computes the median of a numeric vector using weights.
}
\usage{
wtd.median(x, weight = NULL, na.rm = TRUE)
}
\arguments{
  \item{x}{Numeric vector of values.}
  \item{weight}{Optional numeric vector of weights.}
  \item{na.rm}{Logical. If \code{TRUE}, removes missing values.}
}
\value{
A single numeric value representing the weighted median.
}
\seealso{
\code{\link{wtd.quantile}}, \code{\link{median}}
}
\examples{
x <- c(1, 2, 3, 4, 5)
w <- c(1, 1, 5, 1, 1)
wtd.median(x, weight = w)
}
\author{Josh Pasek}
\keyword{weights}
\keyword{median}