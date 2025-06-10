wtd.quantile <- function(x, weight = NULL, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(weight)
    x <- x[keep]
    weight <- weight[keep]
  }
  if (is.null(weight)) weight <- rep(1, length(x))
  ord <- order(x)
  x <- x[ord]
  weight <- weight[ord]
  cum_wt <- cumsum(weight) / sum(weight)
  sapply(probs, function(p) approx(cum_wt, x, xout = p, ties = "ordered")$y)
}
