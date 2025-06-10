#' Create a regression table from one or more models
#'
#' \code{onetable} takes fitted model objects and organizes their coefficients,
#' standard errors, p-values, and fit statistics into a clean, matrix-style table.
#'
#' @param ... One or more fitted model objects
#' @param digits Number of digits to round estimates and standard errors
#' @param p.digits Number of digits to round p-values (default is 3)
#' @param fitstats A vector of model stats to include, such as \code{"r.squared"}, \code{"aic"}
#' @param model.names Optional names for the models
#' @param collapse If \code{TRUE}, outputs a single column per model in format \code{b (se)***}
#' @param formatted If \code{TRUE}, uses formatted rounding (via \code{rd}); otherwise shows raw numbers
#' @param show.cutpoints If \code{TRUE}, includes \code{polr} cutpoints in output
#' @param approx.p If \code{TRUE}, estimates p-values for models that do not provide them (e.g., \code{lmerMod})
#'
#' @return A matrix with coefficients and statistics organized by model
#' @export
onetable <- function(..., digits = 2, p.digits = 3,
                     fitstats = c("r.squared", "adj.r.squared", "mcfadden", "aic", "n"),
                     model.names = NULL,
                     collapse = FALSE,
                     formatted = TRUE,
                     show.cutpoints = TRUE,
                     approx.p = FALSE) {

  models <- list(...)
  n_models <- length(models)
  ot_list <- lapply(models, function(m) onetable.single(m, digits = digits,
                                                        fitstats = fitstats,
                                                        show.cutpoints = show.cutpoints,
                                                        approx.p = approx.p))

  all_coefs <- unique(unlist(lapply(ot_list, function(x) rownames(x$coefficients))))
  all_stats <- unique(unlist(lapply(ot_list, function(x) names(x$fit.stats))))

  if (is.null(model.names)) {
    model.names <- paste("Model", seq_len(n_models))
  }

  full_table <- NULL
  header_row1 <- NULL
  header_row2 <- NULL

  for (i in seq_along(ot_list)) {
    ot <- ot_list[[i]]
    coefs <- ot$coefficients

    ests  <- coefs$Estimate
    ses   <- coefs$`Std. Err`
    pval  <- coefs$p.value
    stars <- coefs$Sig

    est_row  <- setNames(rep("", length(all_coefs)), all_coefs)
    se_row   <- est_row
    p_row    <- est_row
    star_row <- est_row

    est_row[names(ests)]  <- if (formatted) rd(ests, digits = digits) else ests
    se_row[names(ses)]    <- if (formatted) rd(ses, digits = digits) else ses
    p_row[names(pval)]    <- if (formatted) rd(pval, digits = p.digits) else pval
    star_row[names(stars)] <- stars

    if (collapse) {
      collapsed <- paste0(est_row, " (", se_row, ")", star_row)
      col_block <- matrix(collapsed, ncol = 1, dimnames = list(all_coefs, model.names[i]))
      block <- col_block
      header_row1 <- c(header_row1, model.names[i])
      header_row2 <- c(header_row2, "Estimate (SE)")
    } else {
      block <- rbind(est_row, se_row, p_row, star_row)
      rownames(block) <- c("Estimate", "Std. Err", "p", "*")
      block <- block[, all_coefs, drop = FALSE]
      block <- t(block)
      colnames(block) <- paste(model.names[i], c("Est.", "SE", "p", "*"))
      header_row1 <- c(header_row1, rep(model.names[i], 4))
      header_row2 <- c(header_row2, "Est.", "SE", "p", "*")
    }

    full_table <- if (is.null(full_table)) block else cbind(full_table, block)
  }

  for (stat in all_stats) {
    stat_row <- rep("", ncol(full_table))
    stat_used <- FALSE
    for (i in seq_along(ot_list)) {
        if (stat %in% names(ot_list[[i]]$fit.stats)) {
            val <- ot_list[[i]]$fit.stats[[stat]]
            col_pos <- (i - 1) * if (collapse) 1 else 4 + 1
            if (!is.null(val)) {
                stat_row[col_pos] <- if (formatted && is.numeric(val)) rd(val, digits = digits) else val
                stat_used <- TRUE
            }
        }
        col_pos <- (i - 1) * if (collapse) 1 else 4 + 1
      if (!is.null(val)) {
        stat_row[col_pos] <- if (formatted && is.numeric(val) && !is.na(val)) rd(val, digits = digits) else val
        stat_used <- TRUE
      }
    }
    if (stat_used) {
      full_table <- rbind(full_table, stat_row)
      rownames(full_table)[nrow(full_table)] <- paste0("[", stat, "]")
    }
  }

  out <- rbind(header_row1, header_row2, full_table)
  rownames(out)[1:2] <- c("", "")
  return(out)
}

#' Internal helper for onetable()
#' @noRd
onetable.single <- function(x, digits = 2,
                            fitstats = c("r.squared", "adj.r.squared", "mcfadden", "aic", "n"),
                            show.cutpoints = TRUE,
                            approx.p = FALSE) {

  cf <- coeffer(x, digits = digits, approx.p = approx.p)
  nset <- findn(x)

  # If multinom returns a list of sub-models
  if (inherits(cf, "coeffer.multinom")) {
    combined <- do.call(cbind, lapply(cf, function(submodel) {
      rn <- submodel$rn
      coefs <- data.frame(
        Estimate = submodel$est,
        `Std. Err` = submodel$ses,
        p.value = submodel$pval,
        Sig = submodel$star,
        row.names = rn,
        stringsAsFactors = FALSE
      )
      colnames(coefs) <- paste(colnames(coefs), submodel$outcome)
      coefs
    }))
    return(list(coefficients = combined, fit.stats = nset))
  }

  rn <- cf$rn
  if (!show.cutpoints && !is.null(cf$cps)) {
    rn <- setdiff(rn, cf$cps)
  }

  coefs <- data.frame(
    Estimate = as.numeric(cf$est),
    `Std. Err` = as.numeric(cf$ses),
    p.value = as.numeric(cf$pval),
    Sig = cf$star,
    row.names = rn,
    stringsAsFactors = FALSE
  )

  stats <- unlist(nset)[fitstats]
  stats <- stats[!is.na(stats)]

  list(coefficients = coefs, fit.stats = stats)
}
