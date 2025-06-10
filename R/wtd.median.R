wtd.median <- function(x, weight = NULL, na.rm = TRUE) {
  wtd.quantile(x, weight, probs = 0.5, na.rm = na.rm)
}
