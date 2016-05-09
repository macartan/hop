
#' Generate transition matrix for Stochastic stability demonstration
#'
#' Takes error e as argument
#' @param e Error
#' @keywords Stochastic Stability
#' @export
#' @examples
#' gt_sss_mat()

gt_sss_mat <- function(e=.1) matrix(c(1 - e, (1 - e) / 3, 0, 0,
                                   e, e, 2 * e / 3, 0,
                                   0, 2 * (1 - e) / 3, (2 - e) / 3, e,
                                   0, 0, (1 - e) / 3, 1 - e), ncol = 4)

#' Stochastic stabiity
#'
#' Plot matrices for different values of e and t. Thanks to Bernd Beber for generating Much of this code.
#' @param e Error
#' @param t time
#' @keywords Stochastic Stability
#' @export
#' @examples
#' gt_sss()

gt_sss <- function(mat = gt_sss_mat, 
                   e=1, 
                   t=1, 
                   cex.pt = 8, 
                   ann = TRUE, 
                   mar = 1.5, 
                   oma = 5, 
                   bgcol="gray90", 
                   pointcol="gray50", 
                   labcex=.5, 
                   indent=4, 
                   arrows=TRUE, 
                   etlabs = TRUE, 
                   outax=TRUE) {
  
  
  # compute and plot transition matrix for specific error rate and time periods
  .gt_sss <- function(mat, e, t, cex.pt = 8, ann = FALSE, mar = 1.5, bgcol="gray90", pointcol="gray50", labcex=.5, indent = 4) {
    mat <- match.fun(mat)
    p.t <- p <- mat(e)
    if(t > 1) for(i in 2:t) p.t <- p.t %*% p
    ifelse(ann, par(mar = c(indent * mar, indent * mar, mar, mar), mgp = c(2, .4, 0)), par(mar = rep(mar, 4)))
    plot(NA, xlim = c(.5, 4.5), ylim = c(.5, 4.5), axes = FALSE, ann = FALSE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bgcol, border = NA)
    points(rep(1:4, each = 4), rep(4:1, times = 4), cex = cex.pt * c(p.t), pch = 19, col = pointcol)
    if(ann) {
      for(m in 1:2) axis(m, if(m==1) 1:4 else 4:1, c("None", "One", "Two", "All"), tick = FALSE, cex=labcex)
      title(xlab = "How many  will play L in t periods?", ylab = "How many play L now?")
    }
    return(p.t)
  }
  
  
    par(mfrow = c(length(e), length(t)), oma = c(oma, oma, 0, 0))
    k <- 1
    for(i in rev(e)) {
        for(j in t) {
            ifelse(k == 1, 
                   .gt_sss(mat, i, j, cex.pt = cex.pt, ann = ann, mar = mar, bgcol=bgcol, pointcol=pointcol, labcex=labcex, indent=indent), 
                   .gt_sss(mat, i, j, cex.pt = cex.pt, mar = mar, bgcol=bgcol, pointcol=pointcol))
            k <- k + 1 } }
    par(new = TRUE)
    par(mfrow = c(1, 1), mai = par("omi") + par("mai"), oma = rep(0, 4), mgp = c(3, 1, .5))
	if(outax==TRUE){plot(NA, xlim = c(0,1), ylim = c(0,1), xaxs = "i", yaxs = "i", axes = FALSE, ylab = "Error rate", xlab = "Time")}
	if(outax==FALSE){plot(NA, xlim = c(0,1), ylim = c(0,1), xaxs = "i", yaxs = "i", axes = FALSE, xlab="How many  will cheat tomorrow?", ylab = "How many cheating now?")}
		
    if(etlabs){axis(1, (2 * (1:length(t)) - 1) / (2 * length(t)), t, tick = FALSE)
		axis(2, (2 * (1:length(e)) - 1) / (2 * length(e)), e, tick = FALSE)}
    if(arrows){	arrows(0, -.02, 1, -.02, length = .1, xpd=NA)
				arrows(-.02, 1, -.02, 0, length = .1, xpd=NA)}
}

