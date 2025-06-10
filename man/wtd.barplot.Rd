\name{wtd.barplot}
\alias{wtd.barplot}
\title{Weighted barplot}
\description{
\code{wtd.barplot} is a wrapper around barplot that creates barplots of
counts or proportions using weights. Note that for now this only works
in the special case of a single weighted variable. Formulas will be
added later.
}
\usage{
wtd.barplot(x, weight = NULL, percent = FALSE, horiz = FALSE, ...)
}
\arguments{
  \item{x}{Categorical variable (factor or character).}
  \item{weight}{Optional numeric vector of weights.}
  \item{percent}{If \code{TRUE}, display percentages instead of raw counts.}
  \item{horiz}{If \code{TRUE}, draw bars horizontally.}
  \item{...}{Additional arguments passed to \code{\link{barplot}}.}
}
\value{
A barplot is drawn. No value is returned.
}
\seealso{
\code{\link{barplot}}, \code{\link{table}}
}
\examples{
x <- sample(c("Yes", "No"), 100, replace = TRUE)
w <- runif(100, 0.5, 2)
wtd.barplot(x, weight = w)
}
\author{Josh Pasek}
\keyword{graphics}
\keyword{weights}
\keyword{barplot}