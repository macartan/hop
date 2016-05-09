#' Condorcet jury theorem.
#'
#' Illustrates basic logic from law of large numbers.
#' @param n number of voters
#' @param base  probability each is right, defaults to 0.51
#' @keywords Condorcet, Jury
#' @export
#' @examples
#'  gt_jury(20)
#'
gt_jury = function(n,
                   fill=TRUE,
                   bgcol="grey",
                   fillcol="red",
                   fillcol2="white",
                   base = .51,
                   lwd=1,
                   xlim = c(-.3,n+.3),
                   ...){
	k=0:(n+1)
	m = ceiling(n/2)
	k2 = 0:m
	k3 = (m):(n+1)
	n_label <- as.character(n)
	plot(k-.5,dbinom(k,n,base),
	     "s",
	     ylab=("Likelihood"),
	     main=bquote(bold(Likelihood~Function~`for`)~bolditalic(n) == bold(.(n_label))),
	     xlim=xlim,
	     ylim = c(0, 1.1*(dbinom(floor(n*base),n,base))),
	     xlab="Number correct",...)
	if(fill){
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bgcol)
			y = as.vector(sapply(k2, function(i) c(dbinom(i-1, n, base),dbinom(i, n, base))))[-1]
			x= sort(rep(k2,2))[-1] - .5
			polygon(c(-5,x,x[2*m]), c(0,y,0), col=fillcol)
			y = as.vector(sapply(k3, function(i) c(dbinom(i-1, n, base),dbinom(i, n, base))))[-1]
			x= sort(rep(k3,2))[-1] - .5
			polygon(c(m-.5,x,x[2*m]), c(0,y,0), col=fillcol2)
			}
	abline(v=m-.5,lty=2)
	}
