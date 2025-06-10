wtd.anova <- function(response, group, weight = NULL) {
  if (is.null(weight)) weight <- rep(1, length(response))
  if (length(weight) != length(response)) stop("Length mismatch.")
  group <- factor(group)

  group_means <- tapply(response * weight, group, sum) / tapply(weight, group, sum)
  overall_mean <- sum(response * weight) / sum(weight)

  ss_between <- sum(tapply(weight, group, sum) * (group_means - overall_mean)^2)
  ss_within <- sum(weight * (response - group_means[group])^2)

  df_between <- length(group_means) - 1
  df_within <- length(response) - length(group_means)

  ms_between <- ss_between / df_between
  ms_within <- ss_within / df_within

  fstat <- ms_between / ms_within
  pval <- 1 - pf(fstat, df_between, df_within)

  data.frame(
    Source = c("Between", "Within"),
    SS = c(ss_between, ss_within),
    df = c(df_between, df_within),
    MS = c(ms_between, ms_within),
    F = c(fstat, NA),
    p.value = c(pval, NA)
  )
}
