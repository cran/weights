plotinteractpreds <- function(out, seplot=TRUE, ylim=NULL, main=NULL, xlab=NULL, ylab=NULL, legend=TRUE, placement="bottomright", lwd=3, add=FALSE, addby=TRUE, addat=FALSE, mfrow=NULL, linecol=NULL, secol=NULL, showbynamelegend=FALSE, showatnamelegend=FALSE, showoutnamelegend=FALSE, lty=NULL, density=30, startangle=45, ...){
    UseMethod("plotinteractpreds")
}
