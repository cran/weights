plotwtdinteraction <- function(x, across, by=NULL, at=NULL, acrosslevs=NULL, bylevs=NULL, atlevs=NULL, weight=NULL, dvname=NULL, acclevnames=NULL, bylevnames=NULL, atlevnames=NULL, stdzacross=FALSE, stdzby=FALSE, stdzat=FALSE, limitlevs=20, type="response", seplot=TRUE, ylim=NULL, main=NULL, xlab=NULL, ylab=NULL, legend=TRUE, placement="bottomright", lwd=3, add=FALSE, addat=FALSE, addby=TRUE, mfrow=NULL, linecol=NULL, secol=NULL, showbynamelegend=FALSE, showatnamelegend=FALSE, lty=NULL, density=30, startangle=45, approach="prototypical", data=NULL, ...){
    out <- findwtdinteraction(x, across, by, at, acrosslevs, bylevs, atlevs, weight, dvname, acclevnames, bylevnames, atlevnames, stdzacross, stdzby, stdzat, limitlevs, type=type, ...)
    plotinteractpreds(out, seplot, ylim, main, xlab, ylab, legend, placement, lwd, add, addat, mfrow, linecol, secol, showbynamelegend, showatnamelegend, lty, density, startangle, ...)
}
        
