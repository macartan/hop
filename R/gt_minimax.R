


#' Minimax
#'
#' Calculates minimax strategies and payoffs on a probability grid with increments of 1/fine.  Matrix A is the matrix for a single player with own actions in the row position
#' @param A Payoff matrix
#' @keywords Minimax
#' @export
#' @examples
#' A <- matrix(c(2,0,0,0,1,0,0,0,5),3)
#' gt_minimax(A, 200)
#'

gt_minimax = function(A, fine=200){
	P = gt_permv(rep(fine+1, ncol(A)-1))/fine
	P = cbind(P, 1-rowSums(P))
	P = P[P[,ncol(P)]>=0,]
	uR =  A%*%t(P) 					# Expected utility of action R under each possibility
	u.M=sapply(1:nrow(P), function(i) max(uR[,i]))		# Max reponse payoff
	u.minimax=min(u.M)		# Minimax payoff
	list("Minimax payoff"=u.minimax, "Other's strategy"=P[u.M==u.minimax,])
	}



