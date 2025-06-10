#' @export
print.findn <- function(x, digits = 3, ...) {
  cat("Model Summary:\n")
  out <- cbind(
    Type = x$type,
    N = formatC(x$n, digits = 0, format = "d"),
    R2 = ifelse(is.na(x$r.squared), "", formatC(x$r.squared, digits = digits, format = "f")),
    Adj.R2 = ifelse(is.na(x$adj.r.squared), "", formatC(x$adj.r.squared, digits = digits, format = "f")),
    McFadden.R2 = ifelse(is.na(x$mcfadden), "", formatC(x$mcfadden, digits = digits, format = "f")),
    AIC = formatC(x$aic, digits = digits, format = "f")
  )
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}
