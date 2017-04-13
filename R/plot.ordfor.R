#' Visualization of optimized partition
#'
#' Visualization of the optimized partition of [0,1] interpretable as the 
#' relative widths of the classes of the ordinal target variable.
#'
#' @param x object of class \code{ordfor}. See function \code{\link{ordfor}}.
#' @param useggplot logical. Should R package \code{ggplot2} be used to produce a more neatly looking plot?
#' @param ... other parameters to be passed through to plotting functions if \code{useggplot=FALSE}.
#'
#' @return
#' If \code{useggplot = TRUE} (default) a \code{ggplot} x is returned where the ggplot2 plot is also shown on the current device. See \code{?ggplot} for saving ggplot2 plots.
#' If \code{useggplot = FALSE} \code{NULL} is returned.
#'
#' @examples
#' data(hearth)
#'
#' set.seed(123)
#' hearthsubset <- hearth[sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(1/2)))),]
#' ordforres <- ordfor(depvar="Class", data=hearthsubset, ndiv=80, nbest=5)
#' # NOTE: ndiv=80 is not enough!! In practice, ndiv=1000 (default value) or a higher
#' # number should be used.
#' 
#' plot(ordforres)
#'
#' plot(ordforres, useggplot=FALSE)
#'
#' @import ggplot2
#' @importFrom graphics plot par rect lines text
#'
#' @export
plot.ordfor <- 
  function(x, useggplot=TRUE, ...) {
        
    if(useggplot) {
      
      ## require("ggplot2")
     
      borders <- x$bordersbest
      
      datatemp <- data.frame(x=1, y=diff(borders), y2=(borders[-1]+borders[-length(borders)])/2, 
                             cols=1:(length(borders)-1), classes=x$classes)
      
      datatemp2 <- data.frame(xtext=rep(0.83, length(borders)),
                              ytext=c(0.03, borders[-c(1, length(borders))]+0.03, 1 -0.03),
                              textlabels=round(borders, 2))
      
      p <- ggplot(data=datatemp) +
        geom_bar(stat = "identity", aes_string(x='x', y = 'y', fill = 'cols'), width = 0.4) + 
        geom_label(aes_string(x='x', y='y2', label='classes'), fill="white") +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank()) +
        geom_text(data=datatemp2, aes_string(x='xtext', y='ytext', label='textlabels'), col="white") + ggtitle("Optimized partition") +
        coord_flip()
      p
      
    }
    else {
      
      borders <- x$bordersbest
      
      marginsafe <- par()$mar
      par(mar=c(0.1, 0.1, 1.5, 0.1))
      plot(0, 0, xlim=c(0,1), ylim=c(0,1), col="white", frame.plot=FALSE, xaxt="n", yaxt="n", xlab=NA, ylab=NA, main="Optimized partition", ...)
      rect(xleft=0, ybottom=0, xright=1, ytop=1)
      for(i in 2:(length(borders)-1))
        lines(rep(borders[i],2), c(0,1))
      for(i in 1:(length(borders)-1)) {
        text((borders[-1]+borders[-length(borders)])/2, 0.5, x$classes)
      }
      text(c(0.02, borders[-c(1, length(borders))]+0.04, 1 -0.02), 0.12, round(borders, 2))
      par(mar=marginsafe)  
      
    }
    
  }