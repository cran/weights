\name{wtd.anova}
\alias{wtd.anova}
\title{Weighted one-way ANOVA}
\description{
\code{wtd.anova} performs a weighted analysis of variance across groups using a continuous response variable and a grouping factor.
}
\usage{
wtd.anova(response, group, weight = NULL)
}
\arguments{
  \item{response}{Numeric vector of outcome values.}
  \item{group}{Factor indicating group membership.}
  \item{weight}{Optional numeric vector of weights. If \code{NULL}, equal weights are used.}
}
\value{
A data frame with rows for "Between" and "Within" group variance and columns for SS, df, MS, F statistic, and p-value.
}
\seealso{
\code{\link{aov}}, \code{\link{lm}}
}
\examples{
set.seed(1)
group <- rep(c("A", "B", "C"), each = 10)
x <- c(rnorm(10), rnorm(10, mean = 1), rnorm(10, mean = 2))
w <- runif(30, 0.5, 2)
wtd.anova(x, group, weight = w)
}
\author{Josh Pasek}
\keyword{weights}
\keyword{anova}