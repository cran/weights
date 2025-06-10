\name{wtd.violinplot}
\alias{wtd.violinplot}
\title{Draw weighted violin plots by group}
\description{
\code{wtd.violinplot} produces violin plots for weighted data by group using kernel density estimation.
}
\usage{
wtd.violinplot(x, group = NULL, weight = NULL, 
               bw = "nrd0", adjust = 1,
               col = "gray", border = "black", 
               names = NULL, width = 0.4,
               na.rm = TRUE, ...)
}
\arguments{
  \item{x}{Numeric vector of values.}
  \item{group}{Optional grouping factor indicating which group each value belongs to. If \code{NULL}, all values are treated as a single group.}
  \item{weight}{Optional numeric vector of weights, the same length as \code{x}. If \code{NULL}, equal weights are used.}
  \item{bw}{The smoothing bandwidth to be used, passed to \code{\link[stats]{density}}. Default is \code{"nrd0"}.}
  \item{adjust}{A multiplicative bandwidth adjustment. The bandwidth used is actually \code{adjust * bw}. See \code{\link[stats]{density}}.}
  \item{col}{Color(s) for the filled violin shapes. Passed to \code{\link[vioplot]{vioplot}}.}
  \item{border}{Color(s) for the outline of the violins.}
  \item{names}{Optional vector of group names to be displayed on the x-axis. If \code{NULL}, levels of the \code{group} factor are used.}
  \item{width}{Width of the violin plots. Passed to \code{\link[vioplot]{vioplot}}.}
  \item{na.rm}{Logical. Should missing values be removed? Default is \code{TRUE}.}
  \item{...}{Additional graphical parameters passed to \code{\link[vioplot]{vioplot}}.}
}
\details{
This function uses kernel density estimates with weights to generate violin plots for each level of the grouping variable. Internally, it calls \code{\link[stats]{density}} with the \code{weights} argument, and constructs violin plots using \code{\link[vioplot]{vioplot}}.
}
\value{
A base R plot is produced showing weighted violin plots by group. No value is returned.
}
\seealso{
\code{\link[vioplot]{vioplot}}, \code{\link[stats]{density}}, \code{\link{wtd.hist}}, \code{\link{wtd.boxplot}}
}
\examples{
set.seed(123)
x <- c(rnorm(100), rnorm(100, mean = 2))
group <- rep(c("A", "B"), each = 100)
wts <- c(rep(1, 100), runif(100, 0.5, 2))

wtd.violinplot(x, group = group, weight = wts,
               col = c("lightblue", "lightgreen"))

x2 <- c(seq(0,2,length.out=100), seq(0,6,length.out=100))
wts2 <- rep(1, 200)

wtd.violinplot(x2, group = group, weight = wts2,
               col = c("lightblue", "lightgreen"))

wtd.violinplot(x2, group = group, weight = (wts2+.1)/(x2+.1),
               col = c("lightblue", "lightgreen"))
}
\author{Josh Pasek}
\keyword{graphics}
\keyword{weights}