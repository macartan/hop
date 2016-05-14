
#' Folk Theorem Illustration
#'
#' Graphs the individually rational and feasible outcomes that can be sustained as equilibria of a two person game
#'
#' @param X Payoff matrix for player 1. Defaults to coordination.
#' @param Y Payoff matrix for player 2. Defaults to coordination.
#' @keywords Payoff matrix, Nash
#' @export
#' @examples
#' X = matrix(c(2,3,0,1),2)
#' gt_folk(X)
#' X = matrix(  c(2,0,0,0,1,0,0,0,5),3)
#' Y = t(matrix(c(1,2,1,2,1,1,1,3,1),3))
#' gt_folk(X,Y)
#'
gt_folk = function(X,
                     Y=t(X),
                     fine=100,
                     feasible=TRUE,
                     rational=TRUE,
                     pointsize=1,
                     colf= "grey",
                     colr=	rgb(1, .1, 0, .5),
                     main="",
                     mainsize=1,...){
	Z=matrix(c(X, Y), ncol=2)
	plot(Z, xlab=expression(italic(u)[1]), ylab=expression(italic(u)[2]), main=main, cex.main=mainsize,...)

	# Feasible Set
	if(feasible==TRUE){
		hpts <- chull(Z)
		hpts <- c(hpts, hpts[1])
		polygon(Z[hpts, ], col=colf)
		}
	# Individually rational set
	if(rational==TRUE){
		mm1 = gt_minimax(X, fine=fine)[[1]]
		mm2 = gt_minimax(t(Y), fine=fine)[[1]]
		polygon(c(mm1,mm1,max(X),max(X)), c(mm2,max(Y),max(Y),mm2), col=colr)
		abline(v=mm1)
		abline(mm2,0)
		}
	points(X, Y, pch=19, cex=pointsize)
}

