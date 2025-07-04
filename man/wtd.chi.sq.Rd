\name{wtd.chi.sq}
\alias{wtd.chi.sq}
\title{
Produces weighted chi-squared tests.
}
\description{
\code{wtd.chi.sq} produces weighted chi-squared tests for two- and three-variable contingency tables.  Decomposes parts of three-variable contingency tables as well. Note that weights run with the default parameters here treat the weights as an estimate of the precision of the information. A prior version of this software was set to default to \code{mean1=FALSE}.}
\usage{
wtd.chi.sq(var1, var2, var3=NULL, weight=NULL, na.rm=TRUE,
drop.missing.levels=TRUE, mean1=TRUE)
}
\arguments{
  \item{var1}{
\code{var1} is a vector of values which the researcher would like to use to divide any data set.
}
  \item{var2}{
\code{var2} is a vector of values which the researcher would like to use to divide any data set.
}
  \item{var3}{
\code{var3} is an optional additional vector of values which the researcher would like to use to divide any data set.
}
  \item{weight}{
\code{weight} is an optional vector of weights to be used to determine the weighted chi-squared for all analyses.
}
  \item{na.rm}{
\code{na.rm} removes missing data from analyses.
}
  \item{drop.missing.levels}{
\code{drop.missing.levels} drops missing levels from variables.
}
  \item{mean1}{
\code{mean1} is an optional parameter for determining whether the weights should be forced to have an average value of 1. If this is set as false, the weighted correlations will be produced with the assumption that the true N of the data is equivalent to the sum of the weights.
}
}
\value{
A two-way chi-squared produces a vector including a single chi-squared value, degrees of freedom measure, and p-value for each analysis.

A three-way chi-squared produces a matrix with a single chi-squared value, degrees of freedom measure, and p-value for each of seven analyses.  These include: (1) the values using a three-way contingency table, (2) the values for a two-way contingency table with each pair of variables, and (3) assessments for whether the relations between each pair of variables are significantly different across levels of the third variable.
}
\author{
Josh Pasek, Professor of Communication & Media and Political Science at the University of Michigan (www.joshpasek.com).
}
\seealso{
\code{\link{wtd.cor}}
\code{\link{wtd.t.test}}
}
\examples{
var1 <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
var2 <- c(1,1,2,2,3,3,1,1,2,2,3,3,1,1,2)
var3 <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,1,2,2,2,2,2)

wtd.chi.sq(var1, var2)
wtd.chi.sq(var1, var2, weight=weight)

wtd.chi.sq(var1, var2, var3)
wtd.chi.sq(var1, var2, var3, weight=weight)
}
\keyword{ ~contingency tables }
\keyword{ ~chisquared }
\keyword{ ~decompose}
