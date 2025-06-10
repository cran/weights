\name{wtd.partial.cov}
\alias{wtd.partial.cov}
\title{
Computes weighted partial covariances, controlling for covariates
}
\description{
\code{wtd.partial.cov} estimates the weighted partial covariance between two variables or sets of variables, controlling for additional covariates. The function uses weighted linear regression to residualize both dependent and independent variables before computing weighted covariances among the residuals.
}
\usage{
wtd.partial.cov(x, y = NULL, preds = NULL, weight = NULL, collapse = TRUE)
}
\arguments{
  \item{x}{
A numeric vector or matrix. Each column will be residualized on \code{preds} and used in the partial covariance calculation.
}
  \item{y}{
An optional numeric vector or matrix. If \code{NULL}, \code{x} will be used in both dimensions.
}
  \item{preds}{
A vector, matrix, or data frame of covariates to control for via linear regression.
}
  \item{weight}{
An optional numeric vector of weights. If \code{NULL}, equal weights are assumed.
}
  \item{collapse}{
Logical. If \code{TRUE}, the output will be simplified to a matrix if possible.
}
}
\value{
A list with the following components:
\itemize{
  \item{\code{covariance}} — Weighted partial covariance estimates
  \item{\code{std.err}} — Standard errors of the covariance estimates
  \item{\code{t.value}} — T-statistics
  \item{\code{p.value}} — P-values
}
If the covariance matrix is a vector or scalar, the result is simplified when \code{collapse = TRUE}.
}
\author{
Josh Pasek (\url{https://www.joshpasek.com})
}
\seealso{
\code{\link{wtd.partial.cor}}, \code{\link{wtd.cov}}
}
\examples{
set.seed(123)
x <- rnorm(100)
y <- 0.5 * x + rnorm(100)
z <- rnorm(100)
w <- runif(100, 0.5, 1.5)

wtd.partial.cov(x, y, preds = z, weight = w)
}
\keyword{ ~partial }
\keyword{ ~covariance }
\keyword{ ~weights }