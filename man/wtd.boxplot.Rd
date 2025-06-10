\name{wtd.boxplot}
\alias{wtd.boxplot}
\title{Weighted boxplot}
\description{
\code{wtd.boxplot} produces boxplots for weighted data by group, accounting for weights when computing medians and quartiles.
}
\usage{
wtd.boxplot(x, group = NULL, weight = NULL, show.outliers = TRUE,
            whisker.mult = 1.5, box.col = "lightgray", border = "black", ...)
}
\arguments{
  \item{x}{Numeric vector of values.}
  \item{group}{Optional grouping factor.}
  \item{weight}{Optional numeric vector of weights. If \code{NULL}, equal weights are assumed.}
  \item{show.outliers}{Logical. If \code{TRUE}, show weighted outliers based on interquartile range.}
  \item{whisker.mult}{Numeric multiplier for the IQR to define whiskers (default is 1.5, as in standard boxplots).}
  \item{box.col}{Color for the box portion of the plot.}
  \item{border}{Color for the boxplot borders.}
  \item{...}{Additional graphical parameters passed to \code{\link{plot}} or \code{\link{rect}}.}
}
\value{
A base R graphic is produced showing weighted boxplots by group. No value is returned.
}
\seealso{
\code{\link{boxplot}}, \code{\link{wtd.quantile}}, \code{\link{wtd.median}}
}
\examples{
set.seed(123)
x <- rnorm(100)
group <- rep(letters[1:2], each = 50)
w <- runif(100, 0.5, 2)
wtd.boxplot(x, group, weight = w)
}
\author{Josh Pasek}
\keyword{graphics}
\keyword{weights}