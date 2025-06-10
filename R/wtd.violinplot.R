wtd.violinplot <- function(x, group = NULL, weight = NULL,
                                bw = "nrd0", adjust = 1,
                                col = "gray", border = "black",
                                names = NULL, width = 0.4,
                                na.rm = TRUE, ...) {
  if (is.null(group)) {
    group <- factor(rep("All", length(x)))
  } else {
    group <- factor(group)
  }

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  }

  df <- data.frame(x = x, group = group, weight = weight)
  if (na.rm) {
    df <- df[complete.cases(df), ]
  }

  groups <- levels(df$group)
  n_groups <- length(groups)

  globaldensity <- density(x, weights=(weight / sum(weight)), bw = bw, adjust = adjust, na.rm = TRUE)

  plot(1, type = "n", xlim = c(0.5, n_groups + 0.5), ylim = range(globaldensity$x, na.rm = TRUE),
       xaxt = "n", xlab = "", ylab = "", ...)
  axis(1, at = seq_along(groups), labels = if (is.null(names)) groups else names)

  for (i in seq_along(groups)) {
    g <- groups[i]
    groupset <- df[df$group == g, ]
    if (nrow(groupset) < 2 || length(unique(groupset$x)) < 2) next

    d <- density(groupset$x,
                 weights = groupset$weight / sum(groupset$weight),
                 bw = bw, adjust = adjust, na.rm = TRUE)
    y <- d$x
    v <- d$y / max(d$y) * width

    # Draw mirrored polygon
    polygon(c(i - v, rev(i + v)), c(y, rev(y)),
            col = col[i %% length(col) + 1],
            border = border)
  }
}
