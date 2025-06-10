\name{findn}
\alias{findn}
\alias{findn.default}
\alias{findn.lm}
\alias{findn.glm}
\alias{findn.polr}
\alias{findn.lmerMod}
\alias{findn.glmerMod}
\alias{findn.gam}
\alias{findn.multinom}
\alias{print.findn}
\title{
Summarize key model information including sample size and fit statistics
}
\description{
\code{findn} is a generic function that extracts useful summary information from a model object. It supports linear models (\code{lm}), generalized linear models (\code{glm}), ordinal regression models from \code{\link[MASS]{polr}}, mixed-effects models from \code{\link[lme4]{lmer}}, generalized additive models from \code{\link[mgcv]{gam}}, and multinomial models from \code{\link[nnet]{multinom}}.
}
\usage{
findn(x, ...)
}
\arguments{
  \item{x}{
A fitted model object of class \code{lm}, \code{glm}, \code{polr}, \code{lmerMod}, \code{glmerMod}, \code{gam}, or \code{multinom}.
}
  \item{...}{
Additional arguments passed to methods (currently unused).
}
}
\value{
A named list with the following components, where available:
\itemize{
  \item{\code{type} — A character string describing the model type}
  \item{\code{n} — The number of observations used in the model}
  \item{\code{r.squared} — R-squared (for OLS and GAM models)}
  \item{\code{adj.r.squared} — Adjusted R-squared (for OLS models)}
  \item{\code{mcfadden} — McFadden's pseudo-R² (for GLMs and \code{polr}, if \code{pscl} is installed)}
  \item{\code{aic} — AIC value for the model}
}
The object is assigned class \code{"findn"} with a custom \code{print} method for display.
}
\author{
Josh Pasek
}
\seealso{
\code{\link{summary}}, \code{\link{AIC}}, \code{\link[pscl]{pR2}}, \code{\link[MASS]{polr}}, \code{\link[nnet]{multinom}}, \code{\link[lme4]{lmer}}, \code{\link[mgcv]{gam}}
}
\examples{
data(mtcars)
mod1 <- lm(mpg ~ wt + hp, data = mtcars)
findn(mod1)

mod2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
findn(mod2)

library(MASS)
mod3 <- polr(Sat ~ Infl + Type + Cont, data = housing)
findn(mod3)

library(lme4)
mod4 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
findn(mod4)

library(mgcv)
mod5 <- gam(mpg ~ s(wt) + hp, data = mtcars)
findn(mod5)

library(nnet)
mod6 <- multinom(vs ~ wt + hp, data = mtcars, trace = FALSE)
findn(mod6)
}
\keyword{model}
\keyword{summary}
\keyword{regression}