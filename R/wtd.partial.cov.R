wtd.partial.cov <- function(x, y=NULL, preds=NULL, weight=NULL, collapse=TRUE){
  x <- as.matrix(x)
  if(is.null(weight)){
    weight <- rep(1, dim(x)[1])
  }
  if(is.null(y)){
    y <- x
  }
  y <- as.matrix(y)
  res_x <- apply(x, 2, function(col) residuals(lm(col ~ preds, weights = weight)))
  res_y <- apply(y, 2, function(col) residuals(lm(col ~ preds, weights = weight)))
  vsx <- apply(res_x, 2, function(col) wtd.var(col, weight))
  vsy <- apply(res_y, 2, function(col) wtd.var(col, weight))
#  vsx <- apply(x, 2, function(x) anova(lm(x~preds, weight=weight))$'Mean Sq'[2]) #Is This The Correct Variance To Use?
#  vsy <- apply(y, 2, function(y) anova(lm(y~preds, weight=weight))$'Mean Sq'[2]) #Is This The Correct Variance To Use?
  varsmult <- vsx %*% t(vsy)
  materset <- lapply(as.data.frame(x), function(x) lapply(as.data.frame(y), function(y) onecor.partial.wtd(x, y, preds=preds, weight)))
  est <- varsmult*sapply(materset, function(q) sapply(q, function(g) g[1]))
  se <- varsmult*sapply(materset, function(q) sapply(q, function(g) g[2]))
  tval <- sapply(materset, function(q) sapply(q, function(g) g[3]))
  pval <- sapply(materset, function(q) sapply(q, function(g) g[4]))
  out <- list(covariance=est, std.err=se, t.value=tval, p.value=pval)
  if(is.vector(est) & collapse==TRUE || (1 %in% dim(est)) & collapse==TRUE){
    out <- matrix(unlist(out), ncol=4, byrow=FALSE)
    rownames(out) <- names(est)
    colnames(out) <- c("covariance", "std.err", "t.value", "p.value")
  }
  out
}

