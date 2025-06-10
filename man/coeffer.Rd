\name{coeffer}
\alias{coeffer}
\alias{coeffer.default}
\alias{coeffer.polr}
\alias{coeffer.lmerMod}
\alias{coeffer.glmerMod}
\alias{coeffer.gam}
\alias{coeffer.glmnet}
\alias{coeffer.multinom}
\title{
Extract model coefficients with standard errors and significance stars
}
\description{
\code{coeffer} is a generic function to extract estimates, standard errors, p-values, and significance stars from a fitted model object. It supports a variety of common model types including linear, generalized linear, ordinal, mixed-effects, additive, penalized, and multinomial regression models.
}
\usage{
coeffer(x, digits = 2, vertical = TRUE, approx.p = FALSE, s = "lambda.1se", ...)
}
\arguments{
  \item{x}{
A fitted model object. Supported classes include \code{lm}, \code{glm}, \code{\link[MASS]{polr}}, \code{lmerMod}, \code{\link[mgcv]{gam}}, \code{\link[glmnet]{glmnet}}, and \code{\link[nnet]{multinom}}.
}
  \item{digits}{
Number of digits to retain in internal rounding (used for formatting).
}
  \item{vertical}{
Logical; included for compatibility, but not used by most methods.
}
  \item{approx.p}{
Logical; if \code{TRUE}, attempts to compute approximate p-values for models that do not provide them (e.g., \code{lmerMod}). Defaults to \code{FALSE}.
}
  \item{s}{
Sets \code{s = "lambda.1se"} or sets \code{s} to other value for \code{\link[glmnet]{glmnet}} models.
}
  \item{...}{
Additional arguments passed to methods.
}
}
\value{
A list with the following components (or a list of such lists for \code{\link[nnet]{multinom}} models):
\itemize{
  \item{\code{rn} — Coefficient names}
  \item{\code{est} — Point estimates}
  \item{\code{ses} — Standard errors}
  \item{\code{pval} — P-values, where available (otherwise \code{NA})}
  \item{\code{star} — Significance stars based on p-values}
  \item{\code{cps} — Cutpoint names for ordinal models (otherwise \code{NULL})}
}
}
\details{
For models that do not provide p-values (e.g., \code{\link[lme4]{lmer}}, \code{\link[glmnet]{glmnet}}), \code{approx.p = TRUE} will attempt to calculate approximate p-values using standard normal approximations based on the ratio of estimate to standard error. This should be used with caution.

\code{\link[nnet]{multinom}} models return a list of coefficient sets, one for each outcome level.
}
\author{
Josh Pasek
}
\seealso{
\code{\link{summary}}, \code{\link{onetable}}, \code{\link[pscl]{pR2}}, \code{\link[MASS]{polr}}, \code{\link[nnet]{multinom}}, \code{\link[lme4]{lmer}}, \code{\link[mgcv]{gam}}, \code{\link[glmnet]{glmnet}}
}
\examples{
data(mtcars)
mod1 <- lm(mpg ~ wt + hp, data = mtcars)
coeffer(mod1)

mod2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
coeffer(mod2)

library(MASS)
mod3 <- polr(Sat ~ Infl + Type + Cont, data = housing)
coeffer(mod3)

library(lme4)
mod4 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
coeffer(mod4)
coeffer(mod4, approx.p = TRUE)

library(mgcv)
mod5 <- gam(mpg ~ s(wt) + hp, data = mtcars)
coeffer(mod5)

library(glmnet)
x <- model.matrix(mpg ~ wt + hp, data = mtcars)[, -1]
y <- mtcars$mpg
mod6 <- glmnet(x, y)
coeffer(mod6, s = mod6$lambda.min)

library(nnet)
mod7 <- multinom(vs ~ wt + hp, data = mtcars, trace = FALSE)
coeffer(mod7)
}
\keyword{model}
\keyword{coefficients}
\keyword{summary}