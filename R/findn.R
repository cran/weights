#' Extract summary model statistics (N, R-squared, AIC, etc.)
#'
#' @param x A fitted model object (e.g., lm, glm, polr, lmerMod)
#' @param ... Additional arguments (not used)
#' @return A list with model type, N, R-squared, adjusted R-squared, McFadden's R², and AIC
#' @export
findn <- function(x, ...) {
  UseMethod("findn")
}

#' @export
findn.default <- function(x, ...) {
  findn.glm(x, ...)
}

#' @export
findn.lm <- function(x, ...) {
  out <- list(
    type = "(OLS)",
    n = length(x$residuals),
    r.squared = summary(x)$r.squared,
    adj.r.squared = summary(x)$adj.r.squared,
    mcfadden = NA,
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}

#' @export
findn.glm <- function(x, ...) {
  fam <- x$family$family
  link <- x$family$link

  type <- switch(fam,
    "gaussian" = "(OLS)",
    "binomial" = if (link == "logit") "(Logistic)" else "(Binomial)",
    "poisson"  = "(Poisson)",
    paste0("(", fam, ")")
  )

  n <- length(residuals(x))
  r2 <- NA
  adjr2 <- NA
  mcf <- tryCatch({
    if (requireNamespace("pscl", quietly = TRUE)) {
      pscl::pR2(x)["McFadden"]
    } else NA
  }, error = function(e) NA)

  out <- list(
    type = type,
    n = n,
    r.squared = r2,
    adj.r.squared = adjr2,
    mcfadden = as.numeric(mcf),
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}

#' @export
findn.polr <- function(x, ...) {
  fam <- summary(x)$method
  type <- switch(fam,
    "logistic" = "(Ordered Logit)",
    "probit"   = "(Ordered Probit)",
    paste0("(Ordered ", fam, ")")
  )

  mcf <- tryCatch({
    if (requireNamespace("pscl", quietly = TRUE)) {
      pscl::pR2(x)["McFadden"]
    } else NA
  }, error = function(e) NA)

  out <- list(
    type = type,
    n = summary(x)$n,
    r.squared = NA,
    adj.r.squared = NA,
    mcfadden = as.numeric(mcf),
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}

#' @export
findn.lmerMod <- function(x, ...) {
  out <- list(
    type = "(Mixed Effects)",
    n = nobs(x),
    r.squared = NA,
    adj.r.squared = NA,
    mcfadden = NA,
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}

#' @export
findn.glmerMod <- findn.lmerMod

#' @export
findn.gam <- function(x, ...) {
  fam <- x$family$family
  type <- paste0("(GAM: ", fam, ")")
  out <- list(
    type = type,
    n = length(x$residuals),
    r.squared = summary(x)$r.sq,
    adj.r.squared = summary(x)$r.sq,
    mcfadden = NA,
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}

#' @export
findn.multinom <- function(x, ...) {
  n <- if (!is.null(x$weights)) sum(x$weights) else nrow(x$fitted.values)
  mcf <- tryCatch({
    if (requireNamespace("pscl", quietly = TRUE)) {
      pscl::pR2(x)["McFadden"]
    } else NA
  }, error = function(e) NA)

  out <- list(
    type = "(Multinomial)",
    n = n,
    r.squared = NA,
    adj.r.squared = NA,
    mcfadden = as.numeric(mcf),
    aic = AIC(x)
  )
  class(out) <- "findn"
  out
}
