\name{dummify}
\alias{dummify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separate a factor into separate dummy variables for each level.
}
\description{
\code{dummify} creates a matrix with columns signifying separate dummy variables for each level of a factor.  The column names are the former levels of the factor.
}
\usage{
dummify(x, show.na=FALSE, keep.na=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{x} is a factor the researcher desires to split into separate dummy variables.
}
\item{show.na}{
If \code{show.na} is 'TRUE', output will include a column idicating the cases that are missing.
}
\item{keep.na}{
If \code{keep.na} is 'TRUE', output vectors will have "NA"s for cases that were originally missing.
}
}
\value{
\code{dummify} returns a matrix with a number of rows equal to the length of \code{x} and a number of columns equal to the number of levels of \code{x}.
}
\author{
Josh Pasek, Professor of Communication & Media and Political Science at the University of Michigan (www.joshpasek.com).
}
\examples{
data("anes04")

anes04$agecats <- cut(anes04$age, c(17, 25,35,45,55,65, 99))
levels(anes04$agecats) <- c("age1824", "age2534", "age3544",
          "age4554", "age5564", "age6599")

agedums <- dummify(anes04$agecats)
table(anes04$agecats)
summary(agedums)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dummy }
\keyword{ ~split }
