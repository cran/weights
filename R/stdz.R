stdz <- function(x, weight=NULL){
  if(is.null(weight)){
    weight <- rep(1, length(x))
  }
  x <- x-Hmisc::wtd.mean(x, weight, na.rm=TRUE)
  x <- x/sqrt(Hmisc::wtd.var(x, weight, na.rm=TRUE))
  x
}
