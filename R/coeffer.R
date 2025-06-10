#' Extract model coefficients with standard errors and significance
#'
#' \code{coeffer} is a generic function to extract estimates, standard errors,
#' p-values, and significance stars from a fitted model object. It supports
#' a variety of common model types including linear, generalized linear,
#' ordinal logistic, mixed-effects, additive, penalized, and multinomial models.
#'
#' @param x A fitted model object (e.g., from \code{lm}, \code{glm}, \code{polr}, \code{lmerMod}, \code{gam}, etc.)
#' @param digits Number of digits to round when formatting internally
#' @param vertical Logical, not currently used but preserved for compatibility
#' @param approx.p If TRUE, attempts to calculate approximate p-values when not provided
#' @param ... Additional arguments passed to methods (e.g., \code{s} for \code{glmnet})
#'
#' @return A list with components:
#'   \item{rn}{Coefficient names}
#'   \item{est}{Point estimates}
#'   \item{ses}{Standard errors}
#'   \item{pval}{P-values (NA if not calculable)}
#'   \item{star}{Significance stars}
#'   \item{cps}{Cutpoints (for ordered models only)}
#' @export
coeffer <- function(x, digits = 2, vertical = TRUE, approx.p = FALSE, s = "lambda.1se", ...) {
  UseMethod("coeffer")
}

#' @export
coeffer.default <- function(x, digits = 2, vertical = TRUE, ...) {
  coefs <- coef(summary(x))
  rn <- rownames(coefs)
  est <- coefs[, 1]
  ses <- coefs[, 2]
  pval <- coefs[, 4]
  star <- starmaker(pval)

  list(
    rn = rn,
    est = est,
    ses = ses,
    pval = pval,
    star = star,
    cps = NULL
  )
}

coeffer.lm <- function(x, ...)
    coeffer.default(x, ...)

#' @export
coeffer.polr <- function(x, digits = 2, vertical = TRUE, ...) {
  coefs <- coef(summary(x))
  zvals <- coefs[, 1] / coefs[, 2]
  pval <- 2 * pnorm(-abs(zvals))
  star <- starmaker(pval)

  cps <- paste("Cutpoint", seq_len(nrow(coefs) - length(coef(x))))
  rn <- rownames(coefs)

  list(
    rn = rn,
    est = coefs[, 1],
    ses = coefs[, 2],
    pval = pval,
    star = star,
    cps = cps
  )
}

#' @export
coeffer.lmerMod <- function(x, digits = 2, vertical = TRUE, approx.p = FALSE, ...) {
  coefs <- summary(x)$coefficients
  rn <- rownames(coefs)
  est <- coefs[, 1]
  ses <- coefs[, 2]

  if (approx.p) {
    tvals <- est / ses
    pval <- 2 * pnorm(-abs(tvals))
    star <- starmaker(pval)
  } else {
    pval <- rep(NA, length(est))
    star <- rep("", length(est))
  }

  list(
    rn = rn,
    est = est,
    ses = ses,
    pval = pval,
    star = star,
    cps = NULL
  )
}
#' @export
coeffer.glmerMod <- coeffer.lmerMod

#' @export
coeffer.gam <- function(x, digits = 2, vertical = TRUE, ...) {
  coefs <- summary(x)$p.table
  rn <- rownames(coefs)
  est <- coefs[, 1]
  ses <- coefs[, 2]
  pval <- coefs[, 4]
  star <- starmaker(pval)

  list(
    rn = rn,
    est = est,
    ses = ses,
    pval = pval,
    star = star,
    cps = NULL
  )
}

#' @export
coeffer.glmnet <- function(x, digits = 2, vertical = TRUE, approx.p = FALSE, s = "lambda.1se", ...) {
  coef_mat <- as.matrix(coef(x, s = s))
  keep <- coef_mat[, 1] != 0
  coef_mat <- coef_mat[keep, , drop = FALSE]

  rn <- rownames(coef_mat)
  est <- coef_mat[, 1]
  ses <- rep(NA, length(est))

  pval <- rep(NA, length(est))
  star <- rep("", length(est))

  list(
    rn = rn,
    est = est,
    ses = ses,
    pval = pval,
    star = star,
    cps = NULL
  )
}

#' @export
coeffer.multinom <- function(x, digits = 2, vertical = TRUE, ...) {
  coefs <- summary(x)$coefficients
  ses   <- summary(x)$standard.errors

  # Ensure coefs and ses are always matrices
  if (is.null(dim(coefs))) {
    coefs <- matrix(coefs, ncol = 1, dimnames = list(names(coefs), NULL))
    ses   <- matrix(ses,   ncol = 1, dimnames = list(names(ses), NULL))
    colnames(coefs) <- colnames(ses) <- levels(x)[-1]  # fallback outcome name
  }

  pval <- 2 * pnorm(-abs(coefs / ses))  # Approx. p-values

  # Stars or blank matrix
  if (!is.null(pval) && length(dim(pval)) == 2) {
    star <- apply(pval, 2, starmaker)
  } else {
    star <- matrix("", nrow = nrow(coefs), ncol = ncol(coefs),
                   dimnames = dimnames(coefs))
  }

  rn <- rownames(coefs)
  outcomes <- colnames(coefs)

  out <- lapply(seq_along(outcomes), function(j) {
    list(
      rn = rn,
      est = coefs[, j],
      ses = ses[, j],
      pval = pval[, j],
      star = star[, j],
      cps = NULL,
      outcome = outcomes[j]
    )
  })

  class(out) <- "coeffer.multinom"
  return(out)
}
