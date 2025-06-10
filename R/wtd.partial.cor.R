wtd.partial.cor <- function(x, y = NULL, preds = NULL, weight = NULL, collapse = TRUE) {
  x <- as.matrix(x)
  if (is.null(weight)) {
    weight <- rep(1, nrow(x))
  }
  if (is.null(y)) {
    y <- x
  }
  y <- as.matrix(y)

  # Residualize x and y with respect to preds using weighted linear models
  res_x <- apply(x, 2, function(col) residuals(lm(col ~ preds, weights = weight)))
  res_y <- apply(y, 2, function(col) residuals(lm(col ~ preds, weights = weight)))

  # Compute weighted correlations of residuals
  materset <- lapply(as.data.frame(res_x), function(xcol)
    lapply(as.data.frame(res_y), function(ycol) {
      g <- try(onecor.wtd(xcol, ycol, weight), silent = TRUE)
      if (inherits(g, "try-error")) g <- c(NA, NA, NA, NA)
      g
    })
  )

  est <- sapply(materset, function(q) sapply(q, function(g) g[1]))
  se <- sapply(materset, function(q) sapply(q, function(g) g[2]))
  tval <- sapply(materset, function(q) sapply(q, function(g) g[3]))
  pval <- sapply(materset, function(q) sapply(q, function(g) g[4]))

  out <- list(correlation = est, std.err = se, t.value = tval, p.value = pval)

  if (is.vector(est) & collapse == TRUE || (1 %in% dim(est)) & collapse == TRUE) {
    out <- matrix(unlist(out), ncol = 4, byrow = FALSE)
    rownames(out) <- names(est)
    colnames(out) <- c("correlation", "std.err", "t.value", "p.value")
  }

  out
}
