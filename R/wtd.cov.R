wtd.cov <- function(x, y=NULL, weight=NULL, collapse=TRUE){
  x <- as.matrix(x)
  if(is.null(weight)){
    weight <- rep(1, dim(x)[1])
  }
  if(is.null(y)){
    y <- x
  }
  y <- as.matrix(y)
  vsx <- apply(x, 2, function(x) sqrt(Hmisc::wtd.var(x, weight=weight, na.rm=TRUE)))
  vsy <- apply(y, 2, function(y) sqrt(Hmisc::wtd.var(y, weight=weight, na.rm=TRUE)))
  varsmult <- vsx %*% t(vsy)
  materset <- lapply(as.data.frame(x), function(x) lapply(as.data.frame(y), function(y) try(onecor.wtd(x, y, weight))))
  est <- varsmult*as.numeric(sapply(materset, function(q) sapply(q, function(g) g[1])))
  se <- varsmult*as.numeric(sapply(materset, function(q) sapply(q, function(g) g[2])))
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

