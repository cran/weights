\name{onetable}
\alias{onetable}
\title{
Create a clean regression summary table from one or more models
}
\description{
\code{onetable} extracts and formats coefficients, standard errors, p-values, significance stars, and model fit statistics from one or more model objects. It returns a matrix-style table suitable for printing or export. Models may include linear models, generalized linear models, ordinal regressions, mixed-effects models, generalized additive models, penalized regressions, and multinomial regressions.
}
\usage{
onetable(..., digits = 2, p.digits = 3,
         fitstats = c("r.squared", "adj.r.squared", "mcfadden", "aic", "n"),
         model.names = NULL, collapse = FALSE, formatted = TRUE,
         show.cutpoints = TRUE, approx.p = FALSE)
}
\arguments{
  \item{...}{
One or more fitted model objects. Supported classes include \code{lm}, \code{glm}, \code{\link[MASS]{polr}}, \code{lmerMod}, \code{\link[mgcv]{gam}}, \code{\link[glmnet]{glmnet}}, and \code{\link[nnet]{multinom}}.
}
  \item{digits}{
Number of digits to display for estimates and standard errors (default is 2).
}
  \item{p.digits}{
Number of digits to display for p-values (default is 3).
}
  \item{fitstats}{
A character vector of model fit statistics to display. Options include \code{"r.squared"}, \code{"adj.r.squared"}, \code{"mcfadden"}, \code{"aic"}, and \code{"n"}.
}
  \item{model.names}{
An optional character vector to name the models in the output.
}
  \item{collapse}{
If \code{TRUE}, each model is displayed in a single column with format \code{estimate (se)***}.
}
  \item{formatted}{
If \code{TRUE}, values are formatted using \code{\link{rd}} for readability. If \code{FALSE}, raw numeric values are returned.
}
  \item{show.cutpoints}{
If \code{TRUE}, includes cutpoints for ordinal models (e.g., from \code{polr}).
}
  \item{approx.p}{
If \code{TRUE}, attempts to compute approximate p-values (e.g., from t-statistics) for models that do not provide them natively (e.g., \code{lmerMod}).
}
}
\value{
A character matrix with one row per coefficient (and optionally model fit statistics), and one or more columns depending on whether \code{collapse = TRUE}. The object is suitable for display using functions like \code{\link[knitr]{kable}} or export to LaTeX or HTML tables.
}
\seealso{
\code{\link{coeffer}}, \code{\link{findn}}, \code{\link{rd}}, \code{\link[pscl]{pR2}}, \code{\link[MASS]{polr}}, \code{\link[nnet]{multinom}}, \code{\link[lme4]{lmer}}, \code{\link[mgcv]{gam}}, \code{\link[glmnet]{glmnet}}, \code{\link[knitr]{kable}}
}
\examples{
data(mtcars)
mod1 <- lm(mpg ~ wt + hp, data = mtcars)
mod2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)

onetable(mod1, mod2)

# Collapsed form
onetable(mod1, mod2, collapse = TRUE)

# Use formatted = FALSE for raw numeric output
onetable(mod1, mod2, formatted = FALSE)

# Add approximate p-values for mixed models
library(lme4)
mod3 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
onetable(mod3, approx.p = TRUE)
}
\keyword{regression}
\keyword{summary}
\keyword{table}