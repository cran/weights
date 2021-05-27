plotinteractpreds.interactpredsmnl <- function(out, seplot=FALSE, ylim=NULL, main=NULL, xlab=NULL, ylab=NULL, legend=TRUE, placement="bottomright", lwd=3, add=FALSE, addby=TRUE, addat=FALSE, mfrow=NULL, linecol=NULL, secol=NULL, showbynamelegend=FALSE, showatnamelegend=FALSE, showoutnamelegend=FALSE, lty=NULL, density=30, startangle=45, ...){
    if(addat==TRUE){
        warning("addat is not available for multinomial logit")
    }
    if(seplot==TRUE){
        warning("standard errors are not yet available for multinomial logit, they will hopefully be added soon")
    }
    seplot <- FALSE
    oldmfrow <- par()$mfrow
    if(!is.null(mfrow))
        par(mfrow=mfrow)
    dvname <- out$Meta$dvname
    across <- out$Meta$across
    by <- out$Meta$by
    at <- out$Meta$at
    mins <- lapply(1:length(out$Means), function(x) out$Means[[x]])#-1.96*out$SEs[[x]])
    maxs <- lapply(1:length(out$Means), function(x) out$Means[[x]])#+1.96*out$SEs[[x]])
    if(is.null(ylim))
        ylim <- c(min(unlist(mins), na.rm=TRUE), max(unlist(maxs), na.rm=TRUE))
    atlevs <- try(names(out$Means))
    bylevs <- try(names(out$Means[[1]]))
    acrosslevs <- try(rownames(out$Means[[1]]))
    outcats <- try(colnames(out$Means[[1]]))
    if(!is.null(bylevs)){
        acrosslevs <- try(rownames(out$Means[[1]][[1]]))
        outcats <- try(colnames(out$Means[[1]][[1]]))
        hasby=TRUE
        bylevnames <- bylevs
    }
    if(is.null(bylevs)){
        hasby <- FALSE
        bylevs <- "All"
    }
    accnumeric <- suppressWarnings(sum(as.character(as.numeric(acrosslevs))==acrosslevs, na.rm=TRUE)==length(acrosslevs))
    if(accnumeric==TRUE){
        acrosslevs <- as.numeric(acrosslevs)
        acrossvals <- as.numeric(acrosslevs)
    }
    if(accnumeric==FALSE)
        acrossvals <- 1:length(acrosslevs)
    if(is.null(xlim))
        xlim <- c(min(acrossvals, na.rm=TRUE), max(acrossvals, na.rm=TRUE))
    hasat <- !(is.null(atlevs) || length(atlevs)==1)
    if(is.null(linecol) & addat==TRUE)
        linecol <- gray(seq(0, .5, length.out=length(atlevs)))
    if(is.null(linecol))
        linecol <- "black"
    atlegend <- atlevs
    bylegend <- bylevs
    outlegend <- outcats
    if(showatnamelegend==TRUE)
        atlegend <- paste(atlevs, at)
    if(showbynamelegend==TRUE)
        bylegend <- paste(bylevs, by)
    if(showoutnamelegend==TRUE)
        outlegend <- paste(outcats, dvname)
    premain <- main
    for(a in 1:length(atlevs)){
        for(b in 1:length(bylevs)){
            if(is.null(premain)){
                mainp <- paste("Interaction Plot of", dvname, "Across Levels of\n", across)
                if(!is.null(bylevs) & length(bylevs)>1 & addby==TRUE)
                    mainp <- paste(mainp, "By", by)
                if(!is.null(bylevs) & length(bylevs)>1 & addby==FALSE)
                    mainp <- paste(mainp, "At", by, "=", bylevs[b])
                if(hasat==TRUE & addby==FALSE)
                    mainp <- paste(mainp, "and")
                if(hasat==TRUE & addat==FALSE)
                    main <- paste(mainp, "At", at, "=", atlevs[a])
                if(hasat==TRUE & addat==TRUE)
                    main <- paste(mainp, "At Each Level Of", at)
                if(hasat==FALSE)
                    main <- mainp
            }
            if(is.null(ylab))
                ylab <- dvname
            if(is.null(xlab))
                xlab <- across
            if(add==FALSE){
                plot(acrossvals, acrossvals, type="n", ylim=ylim, main=main, ylab=ylab, xlab=xlab, axes=FALSE, ...)
                axis(1, at=c(-999,999))
                axis(1, at=acrossvals, labels=acrosslevs)
                axis(2, at=c(-999,999))
                axis(2)
                axis(3, at=c(-999,999))
                axis(4, at=c(-999,999))
            }
            if(length(density)==1)
                dense <- rep(density, length(outcats))
            if(length(linecol)==1 & hasby==TRUE & addby==TRUE)
                linecol <- rep(linecol, length(bylevs))
            if(length(linecol)==1)   
                linecol <- rep(linecol, length(outcats))
            if(addby==TRUE)
                add <- TRUE
            if(!is.null(lty))
                ltyset <- lty
            if(is.null(lty))
                ltyset <- 1:length(outcats)
            if(length(ltyset)!=length(outcats)){
                ltyset <- 1:length(outcats)
                warning("lty must have a length equal to the number of outcome categories, it has been reset to defaults")
            }
            for(i in 1:length(outcats)){
                if(hasby==FALSE || (hasby==TRUE & addby==FALSE))
                    lines(acrossvals, out$Means[[a]][[b]][,i], lty=ltyset[i], lwd=lwd, col=linecol[i])
                if(hasby==TRUE & addby==TRUE)
                    lines(acrossvals, out$Means[[a]][[b]][,i], lty=ltyset[i], lwd=lwd, col=linecol[b])            
                if(legend==TRUE){
                    if(hasby==FALSE)
                        legend(x=placement, legend=outlegend, lty=ltyset, lwd=lwd, col=linecol)
                    if(hasby==TRUE & addby==FALSE)
                        legend(x=placement, legend=outlegend, lty=ltyset, lwd=lwd, col=linecol)
                }
            }
            #if(hasby==FALSE || (hasby==TRUE & addby==FALSE))
            #    lines(acrossvals, out$Means[[a]][[b]], lty=ltyset, lwd=lwd, col=linecol)
            #if(hasby==TRUE & addby==TRUE)
            #    lines(acrossvals, out$Means[[a]][[b]], lty=ltyset, lwd=lwd, col=linecol[b])
        }
        if(legend==TRUE & hasby==TRUE & addby==TRUE)
            legend(x=placement, legend=c(outlegend, bylegend), lty=c(1:length(outcats), rep(1, length(bylevs))), lwd=lwd, col=c(rep("black", length(outcats)), linecol))
    }
    if(!is.null(mfrow))
        par(mfrow=oldmfrow)
}
