wtd.boxplot <- function(x, group = NULL, weight = NULL,
                        show.outliers = TRUE, whisker.mult = 1.5,
                        box.col = "lightgray", border = "black", ...) {
  if (is.null(group)) group <- rep("All", length(x))
  group <- as.factor(group)

  if (is.null(weight)) weight <- rep(1, length(x))

  # Prepare data
  df <- data.frame(x = x, group = group, weight = weight)
  df <- df[complete.cases(df), ]

  # Summary stats
  box.stats <- lapply(levels(df$group), function(g) {
    groupset <- df[df$group == g, ]
    q0 <- Hmisc::wtd.quantile(groupset$x, weight=groupset$weight, probs = 0)
    q1 <- Hmisc::wtd.quantile(groupset$x, groupset$weight, probs = 0.25)
    med <- Hmisc::wtd.quantile(groupset$x, groupset$weight, probs = 0.5)
    q3 <- Hmisc::wtd.quantile(groupset$x, groupset$weight, probs = 0.75)
    q4 <- Hmisc::wtd.quantile(groupset$x, groupset$weight, probs = 1)
    iqr <- q3 - q1
    lower <- max(c(q1 - whisker.mult * iqr, q0), na.rm=TRUE)
    upper <- min(c(q3 + whisker.mult * iqr, q4), na.rm=TRUE)
    outliers <- if (show.outliers) groupset$x[groupset$x < lower | groupset$x > upper] else numeric(0)
    list(q1 = q1, median = med, q3 = q3, lower = lower, upper = upper, outliers = outliers)
  })
  names(box.stats) <- levels(df$group)

  # Set up plot
  n <- length(box.stats)
  y.range <- range(x, na.rm=TRUE)
  plot(1, type = "n", xlim = c(0.5, n + 0.5), ylim = y.range,
       xaxt = "n", xlab = "Group", ylab = "Value", ...)

  axis(1, at = 1:n, labels = names(box.stats))

  # Draw boxes
  for (i in seq_along(box.stats)) {
    b <- box.stats[[i]]
    rect(i - 0.2, b$q1, i + 0.2, b$q3, col = box.col, border = border)
    segments(i - 0.2, b$median, i + 0.2, b$median, lwd = 2, col = border)
    segments(i, b$lower, i, b$q1, lty = 1, col = border)
    segments(i, b$q3, i, b$upper, lty = 1, col = border)
    segments(i - 0.1, b$lower, i + 0.1, b$lower, col = border)
    segments(i - 0.1, b$upper, i + 0.1, b$upper, col = border)
    if (length(b$outliers) > 0) {
      points(rep(i, length(b$outliers)), b$outliers, pch = 1, col = border)
    }
  }
}
