wtd.barplot <- function(x, weight = NULL, percent = FALSE, horiz = FALSE, ...) {
    if (is.null(weight)) weight <- rep(1, nrow(x))
    tab <- tapply(weight, x, sum)
    if (percent) tab <- 100 * tab / sum(tab)
    graphics::barplot(tab, horiz = horiz, las = 1, col = "gray", ...)
}
