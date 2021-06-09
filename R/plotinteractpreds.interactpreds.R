plotinteractpreds.interactpreds <- function(out, seplot=TRUE, ylim=NULL, main=NULL, xlab=NULL, ylab=NULL, legend=TRUE, placement="bottomright", lwd=3, add=FALSE, addby=TRUE, addat=FALSE, mfrow=NULL, linecol=NULL, secol=NULL, showbynamelegend=FALSE, showatnamelegend=FALSE, showoutnamelegend=FALSE, lty=NULL, density=30, startangle=45, ...){
    #oldmfrow <- par()$mfrow
    if(class(out)!="interactpreds")
        warning("This function may not work with data that is not generated using the findwtdinteraction function")
    #if(!is.null(mfrow))
    #    par(mfrow=mfrow)
    dvname <- out$Meta$dvname
    across <- out$Meta$across
    by <- out$Meta$by
    at <- out$Meta$at
    mins <- lapply(1:length(out$Means), function(x) out$Means[[x]]-1.96*out$SEs[[x]])
    maxs <- lapply(1:length(out$Means), function(x) out$Means[[x]]+1.96*out$SEs[[x]])
    if(is.null(ylim) | length(ylim)!=2)
        ylim <- c(min(unlist(mins), na.rm=TRUE), max(unlist(maxs), na.rm=TRUE))
    atlevs <- try(names(out$Means))
    bylevs <- try(rownames(out$Means[[1]]))
    acrosslevs <- try(names(out$Means[[1]]))
    if(!is.null(bylevs)){
        acrosslevs <- try(colnames(out$Means[[1]]))
        hasby=TRUE
        bylevnames <- bylevs
    }
    if(is.null(bylevs)){
        acrosslevs <- try(colnames(out$Means[[1]]))
        hasby <- FALSE
        bylevs <- "All"
    }
    accnumeric <- suppressWarnings(sum(as.character(as.numeric(as.character(acrosslevs)))==as.character(acrosslevs), na.rm=TRUE)==length(acrosslevs))
    if(isTRUE(accnumeric)){
        acrosslevs <- as.numeric(as.character(acrosslevs))
        acrossvals <- as.numeric(acrosslevs)
    }
    if(!isTRUE(accnumeric))
        acrossvals <- 1:length(acrosslevs)
    if(is.null(xlim) | length(xlim)!=2)
        xlim <- range(acrossvals, na.rm=TRUE)
    hasat <- !(is.null(atlevs) || length(atlevs)==1)
    if(is.null(linecol) & isTRUE(addat))
        linecol <- gray(seq(0, .5, length.out=length(atlevs)))
    if(is.null(linecol))
        linecol <- "black"
    if(is.null(secol) & isTRUE(addat))
        secol <- gray(seq(.3, .8, length.out=length(atlevs)))
    if(is.null(secol))
        secol <- "light gray"
    atlegend <- atlevs
    bylegend <- bylevs
    if(showatnamelegend==TRUE)
        atlegend <- paste(atlevs, at)
    if(showbynamelegend==TRUE)
        bylegend <- paste(bylevs, by)
    premain <- main
    for(a in 1:length(atlevs)){
        if(is.null(premain)){
            mainp <- paste("Interaction Plot of", dvname, "Across Levels of\n", across)
            if(!is.null(bylevs) & length(bylevs)>1)
                mainp <- paste(mainp, "By", by)
            if(isTRUE(hasat) & !isTRUE(addat))
                main <- paste(mainp, "At", at, "=", atlevs[a])
            if(isTRUE(hasat) & isTRUE(addat))
                main <- paste(mainp, "At Each Level Of", at)
            if(!isTRUE(addat))
                main <- mainp
        }
        if(is.null(ylab))
            ylab <- dvname
        if(is.null(xlab))
            xlab <- across
        if(add==FALSE){
            plot(acrosslevs~acrossvals, type="n", ylim=ylim, main=main, ylab=ylab, xlab=xlab, axes=FALSE, ...)
            axis(1, at=c(-999,999))
            axis(1, at=acrossvals, labels=acrosslevs)
            axis(2, at=c(-999,999))
            axis(2)
            axis(3, at=c(-999,999))
            axis(4, at=c(-999,999))
        }
        if(length(density)==1)
            dense <- rep(density, length(bylevs))
        if(length(linecol)==1 & isTRUE(hasat) & isTRUE(addat))
            linecol <- rep(linecol, length(atlevs))
        if(length(linecol)==1)   
            linecol <- rep(linecol, length(bylevs))
        if(length(secol)==1 & isTRUE(hasat) & isTRUE(addat))
            secol <- rep(secol, length(atlevs))
        if(length(secol)==1)   
            secol <- rep(secol, length(bylevs))
        if(seplot==TRUE){
            if(hasby==TRUE){
                for(i in 1:length(bylevs)){
                    if(seplot==TRUE & !isTRUE(hasat))
                        polygon(c(acrossvals, rev(acrossvals)), c(maxs[[a]][i,], rev(mins[[a]][i,])), density=dense[i], angle=startangle+10*(i-1), col=secol[i])# just added the [i] here, check that it works
                    if(isTRUE(seplot) & isTRUE(hasat) & !isTRUE(addat))
                        polygon(c(acrossvals, rev(acrossvals)), c(maxs[[a]][i,], rev(mins[[a]][i,])), density=dense[i], angle=startangle+10*(i-1), col=secol[i])# just added the [i] here, check that it works
                    if(isTRUE(seplot) & isTRUE(hasat) & isTRUE(addat))
                        polygon(c(acrossvals, rev(acrossvals)), c(maxs[[a]][i,], rev(mins[[a]][i,])), density=dense[i], angle=startangle*a+10*(i-1), col=secol[a])
                }
            }
            if(hasby==FALSE)
                polygon(c(acrossvals, rev(acrossvals)), c(maxs[[a]], rev(mins[[a]])), density=density, angle=startangle*a, col=secol[a])
        }
        if(isTRUE(addat))
            add <- TRUE
        if(!is.null(lty))
            ltyset <- lty
        if(is.null(lty))
            ltyset <- 1:length(bylevs)
        if(length(ltyset)!=length(bylevs)){
            ltyset <- 1:length(bylevs)
            warning("lty must have a length equal to the number of levels for the by variable (or 1 if there is no by variable), it has been reset to defaults")
        }
        if(isTRUE(hasby)){
            for(i in 1:length(bylevs)){
                if(!isTRUE(hasat) || (isTRUE(hasat) & !isTRUE(addat)))
                    lines(acrossvals, out$Means[[a]][i,], lty=ltyset[i], lwd=lwd, col=linecol[i])
                if(isTRUE(hasat) & isTRUE(addat))
                    lines(acrossvals, out$Means[[a]][i,], lty=ltyset[i], lwd=lwd, col=linecol[a])            
                if(legend==TRUE){
                    if(!isTRUE(hasat))
                        legend(x=placement, legend=bylegend, lty=ltyset, lwd=lwd, col=linecol)
                    if(isTRUE(hasat) & !isTRUE(addat))
                        legend(x=placement, legend=bylegend, lty=ltyset, lwd=lwd, col=linecol)
                }
            }
        }
        if(!isTRUE(hasby)){
            if(!isTRUE(hasat) || (isTRUE(hasat) & !isTRUE(addat)))
                lines(acrossvals, out$Means[[a]], lty=ltyset, lwd=lwd, col=linecol)
            if(isTRUE(hasat) & isTRUE(addat))
                lines(acrossvals, out$Means[[a]], lty=ltyset, lwd=lwd, col=linecol[a])
        }
    }
    if(isTRUE(legend) & isTRUE(hasat) & isTRUE(addat) & isTRUE(hasby))
        legend(x=placement, legend=c(bylegend, atlegend), lty=c(1:length(bylevs), rep(1, length(atlevs))), lwd=lwd, col=c(rep("black", length(bylevnames)), linecol))
    if(isTRUE(legend) & isTRUE(hasat) & isTRUE(addat) & !isTRUE(hasby))
        legend(x=placement, legend=c(atlegend), lty=1:length(atlevs), lwd=lwd, col=linecol)
    #if(!is.null(mfrow))
    #    par(mfrow=oldmfrow)
}
