\name{wtd.xtab}
\alias{wtd.xtab}
\title{
Weighted cross-tabulations using up to three categorical variables
}
\description{
\code{wtd.xtab} creates 2-way or 3-way weighted cross-tabulations. It uses weighted counts and optionally returns row, column, or total percentages. The function outputs either a matrix or a list of matrices for easier interpretation than base R's default array structure.
}
\usage{
wtd.xtab(var1, var2, var3 = NULL,
         weight = NULL,
         percent = c("none", "row", "column", "total"),
         na.rm = TRUE,
         drop.missing.levels = TRUE,
         mean1 = TRUE,
         digits = 1)
}
\arguments{
  \item{var1}{
A categorical variable to appear as rows in the table.
}
  \item{var2}{
A categorical variable to appear as columns in the table.
}
  \item{var3}{
An optional third categorical variable used to split the table (i.e., one table per level of \code{var3}).
}
  \item{weight}{
A numeric vector of weights. If \code{NULL}, equal weights are assumed.
}
  \item{percent}{
How percentages should be computed: \code{"none"} (default), \code{"row"}, \code{"column"}, or \code{"total"}.
}
  \item{na.rm}{
Logical. If \code{TRUE}, removes observations with missing values on any input variable.
}
  \item{drop.missing.levels}{
Logical. If \code{TRUE}, drops unused factor levels in \code{var1}, \code{var2}, and \code{var3}.
}
  \item{mean1}{
Logical. If \code{TRUE}, normalizes the weights to have a mean of 1.
}
  \item{digits}{
Number of digits to which percentages should be rounded (only used if \code{percent != "none"}).
}
}
\value{
If \code{var3} is \code{NULL}, returns a list with:
\itemize{
  \item{\code{counts} — A matrix of weighted counts}
  \item{\code{percent} — A matrix of percentages (or \code{NULL} if \code{percent = "none"})}
}

If \code{var3} is specified, returns a named list where each element corresponds to a level of \code{var3} and contains:
\itemize{
  \item{\code{counts} — A matrix of weighted counts}
  \item{\code{percent} — A matrix of percentages (or \code{NULL})}
}
}
\details{
This function provides a cleaner and more interpretable alternative to \code{\link{xtabs}} when working with weights and categorical variables. It simplifies 2-way and 3-way tabulations and avoids confusing multi-dimensional array output.
}
\seealso{
\code{\link{wtd.chi.sq}}, \code{\link{xtabs}}, \code{\link[Hmisc]{wtd.table}}
}
\examples{
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$wt_cat <- cut(mtcars$wt, 3)

# Two-way table
wtd.xtab(mtcars$cyl, mtcars$am)

# With row percentages
wtd.xtab(mtcars$cyl, mtcars$am, weight = mtcars$wt, percent = "row")

# Three-way table, split by gear
wtd.xtab(mtcars$cyl, mtcars$am, mtcars$gear, weight = mtcars$wt)

# Column percentages by weight class
wtd.xtab(mtcars$cyl, mtcars$am, mtcars$wt_cat, weight = mtcars$wt, percent = "column")
}
\author{
Josh Pasek
}
\keyword{weights}
\keyword{crosstab}
\keyword{survey}
\keyword{contingency table}