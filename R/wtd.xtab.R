wtd.xtab <- function(var1, var2, var3 = NULL,
                     weight = NULL,
                     percent = c("none", "row", "column", "total"),
                     na.rm = TRUE,
                     drop.missing.levels = TRUE,
                     mean1 = TRUE,
                     digits = 1) {

  percent <- match.arg(percent)

  if (is.null(weight)) {
    weight <- rep(1, length(var1))
  }

  if (mean1) {
    weight <- weight / mean(weight, na.rm = TRUE)
  }

  # Handle NA removal
  if (na.rm) {
    filt <- !is.na(var1) & !is.na(var2)
    if (!is.null(var3)) filt <- filt & !is.na(var3)
    var1 <- var1[filt]
    var2 <- var2[filt]
    if (!is.null(var3)) var3 <- var3[filt]
    weight <- weight[filt]
  }

  # Drop unused levels if requested
  if (drop.missing.levels) {
    if (is.factor(var1)) var1 <- droplevels(var1)
    if (is.factor(var2)) var2 <- droplevels(var2)
    if (!is.null(var3) && is.factor(var3)) var3 <- droplevels(var3)
  }

  # Helper for calculating percentages
get_percents <- function(mat) {
  switch(percent,
    row    = t(apply(mat, 1, wpct)),
    column = apply(mat, 2, wpct),
    total  = wpct(as.vector(mat)),
    none   = NULL
  )
}

format_percent <- function(x) {
  if (is.null(x) || !is.numeric(x)) return(NULL)
  round(100 * x)
}

  # 2-way case
  if (is.null(var3)) {
    tab <- xtabs(weight ~ var1 + var2)
    result <- list(
      counts = as.matrix(tab),
      percent = format_percent(get_percents(tab))
    )
    return(result)
  }

  # 3-way case
  slices <- levels(factor(var3))
  result <- list()

  for (lvl in slices) {
    i <- var3 == lvl
    subt <- xtabs(weight[i] ~ var1[i] + var2[i])
    mat <- as.matrix(subt)
    result[[lvl]] <- list(
      counts = mat,
      percent = format_percent(get_percents(mat))
    )
  }

  return(result)
}
