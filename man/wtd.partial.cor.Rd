\name{wtd.partial.cor}
\alias{wtd.partial.cor}
\title{
Computes weighted partial correlations, controlling for covariates
}
\description{
\code{wtd.partial.cor} estimates the weighted partial correlation between two variables or sets of variables, controlling for additional covariates. This function uses weighted regression to residualize the inputs and computes the correlation of the residuals, providing standard errors and significance tests.
}
\usage{
wtd.partial.cor(x, y = NULL, preds = NULL, weight = NULL, collapse = TRUE)
}
\arguments{
  \item{x}{
A numeric vector or matrix. Each column will be residualized on \code{preds}.
}
  \item{y}{
An optional numeric vector or matrix. If \code{NULL}, \code{x} is used as both inputs.
}
  \item{preds}{
Covariates to control for via weighted linear regression.
}
  \item{weight}{
Optional weights to be applied in the regression and correlation steps.
}
  \item{collapse}{
Logical. If \code{TRUE}, simplifies the output to a matrix when possible.
}
}
\value{
A list with:
\itemize{
  \item{\code{correlation}} — Estimated partial correlations
  \item{\code{std.err}} — Standard errors
  \item{\code{t.value}} — T-statistics
  \item{\code{p.value}} — P-values
}
When \code{collapse = TRUE}, the result is simplified when possible.
}
\author{
Josh Pasek (\url{https://www.joshpasek.com})
}
\seealso{
\code{\link{wtd.partial.cov}}, \code{\link{wtd.cor}}, \code{\link{onecor.wtd}}
}
\examples{
set.seed(456)
x <- rnorm(100)
y <- 0.4 * x + rnorm(100)
z <- rnorm(100)
w <- runif(100, 1, 2)

wtd.partial.cor(x, y, preds = z, weight = w)
}
\keyword{ ~partial }
\keyword{ ~correlation }
\keyword{ ~weights }