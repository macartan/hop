#' Nash bargaining solution
#'
#' Calculates and plots the Nash bargaining solution for a two player game. Provide either a payoff matrix for underlying game or a function that defines a frontier.
#' @param u1 a function, increasing in x
#' @param u2 a function, decreasing in x
#' @param matrix if TRUE payof matrix expected rather than function, SQ may be set manullay or minimax assumed
#' @param X a payoff matrix with utilities for player 1
#' @param Y a payoff matrix with utilities for player 2
#' @param SQ 2 a dimensional vector with status quo utilities
#' @param solution_only Just show solution
#' @param origin Mark the origin if TRUE
#' @keywords Nash bargaining
#' @export
#' @examples
#'  gt_nbs(u1 = function(x) x^.5, u2 = function (x) 1 - x)
#'  gt_nbs(matrix  = TRUE, solution_only = TRUE)

gt_nbs <- function(u1 = function(x) x^.5,
                 u2 = function(x) (1-x) ,
                 matrix = FALSE,
                 X = matrix(c(3,4,2,1),2,2), # Chicken
                 Y = t(X),
                 SQ = c(0,0),
                 solution_only = FALSE,
                  setcol="red",
                  flatcol="grey",
                  hullcolor = grey(.5),
                  pointsize=1,
                  length=1000,
                  xtop = 1,
                  xlim = NULL,
                  ylim = NULL,
                  alt=NULL,   # may be be set to some value to indicate a second comparison point on the indifference curve
                  altlab=FALSE,
                  paretoset_border = NA,
                  NBScol = "black",
                  diagonal = TRUE,
                  at = c(0,1),
                  origin = TRUE # Mark the origin
                  ){

# Say:
# g = function(x){(x^.8)*.5}
# f = function(x){(1-x^2)^.5}
# Then:
# NBS = max((x^.8)*.5*(1-x^2)^.5 = max .8ln(x)+.5ln(1-x^2);  .8/x -x/(1-x^2) = 0; x=(.8/1.8)^.5 = 2/3
# SLOPE: b=-f(nsh)/g(nsh)
# Y intercept: f(nsh)=a+b*g(nsh) = a-f(nsh) --> a = 2*f(nsh)
# X intercept:  0 = a - bz; z = a/b =  2*g(nsh)

if(!matrix){
  f <- u2
  g <- u1
 }

# In case a payoff matrix is provided, the frontier implied by the maximum payoff is used
# with minimax utility normalized to 0 (unless this is provided by user)
if(matrix) {
  if(is.null(SQ)) SQ <- c(gt_minimax(X)[[1]], gt_minimax(t(Y))[[1]])
  Z <- matrix(c(X, Y), ncol=2)
  Z <- t(t(Z) - SQ)
  opt <- max(rowSums(Z))
  xtop <- opt
  f <- function(x) x
  g <- function(x) opt - x

  if(is.null(xlim)) xlim <-  c(min(Z),xtop)
  if(is.null(ylim)) ylim <- c(min(Z),xtop)
  }

  if(is.null(xlim)) xlim <-  c(SQ[1],xtop)
  if(is.null(ylim)) ylim <- c(SQ[2],xtop)

x 	<-  seq(0,xtop, length = length)
x2 	<-  seq(0,max(xlim)*2, length = length)
nsh	<-  x[sapply(x, function(i) f(i)*g(i))==max(sapply(x, function(i) f(i)*g(i)))]  # NASH Bargaining Solution
if(length(nsh)>1) nsh <- sample(nsh,1)
N	  <-  g(nsh)*f(nsh)

if(!solution_only) par(mfrow=c(1,3))

plot(g(x),f(x),
     type="n",
     xlab=expression(italic(u)[1]),
     ylab=expression(italic(u)[2]),
     xlim=xlim,
     ylim=ylim,
     axes =FALSE)
	if(is.null(at)) {axis(1); axis(2)}
  if(!is.null(at)) {axis(1, at=at); axis(2, at=at)}
  polygon(c(0,g(x)),c(0,f(x)), col=setcol, border = paretoset_border)
  if(matrix) {
    hpts <- chull(Z)
    hpts <- c(hpts, hpts[1])
    polygon(Z[hpts, ], col=hullcolor)
    points(Z,  col = "black", pch=19, cex=pointsize)
  }
  if(!solution_only) title("Original Problem")
  if(solution_only) title(paste("Nash bargaining solution: (",
                                round(g(nsh),2),
                                ",",
                                round(f(nsh),2),
                                ")", sep = ""
                                 ))

	if(diagonal) abline(0,1, col = "grey", lty = 2)
	points(g(nsh), f(nsh), pch=19, col=NBScol)
  if(!is.null(alt)){points(g(alt), f(alt), pch=19, col="red")}
	lines(x2,N/x2, col=NBScol)
	lines(x2,1.2*N/x2, col=grey(.8))
	lines(x2,1.4*N/x2, col=grey(.8))
	text(g(nsh)+.075, f(nsh), "NBS", cex=1.1, col=NBScol)
	if(altlab){text(g(alt)-.05, f(alt), "Alt", cex=1.1, col="red")}
	if(origin) abline(a=0,b=0); abline(v = 0)
	box()

if(!solution_only){

plot(g(x),f(x), type="n",
     xlab=expression(italic(u)[1]),
     ylab=expression(italic(u)[2]),
     main="Flat Superset",
     xlim=xlim, ylim=ylim,
     axes =FALSE)
if(is.null(at)) {axis(1); axis(2)}
if(!is.null(at)) {axis(1, at=at); axis(2, at=at)}
	polygon(c(0, 0, 2*g(nsh)), c(0, 2*f(nsh), 0), col=flatcol, border = paretoset_border)
	polygon(c(0,g(x)),c(0,f(x)), col=setcol, border = paretoset_border)
	if(diagonal) abline(0,1, col = "grey", lty = 2)
	points(g(nsh), f(nsh), pch=19, col=NBScol)
	if(!is.null(alt)){points(g(alt), f(alt), pch=19, col="red")}
	lines(x2,N/x2, col=NBScol)
	text(g(nsh)+.075, f(nsh), "NBS", cex=1.1, col=NBScol)
	if(altlab) {text(g(alt)-.05, f(alt), "Alt", cex=1.1, col="red")}
	box()


plot(g(x),f(x), type="n", xlab=expression(paste(italic(u)[1], " (rescaled)")), ylab=expression(paste(italic(u)[2], " (rescaled)")), main="Transformed Superset",
     xlim=xlim, ylim=ylim, col="grey",
     axes =FALSE)
  if(is.null(at)) {axis(1); axis(2)}
  if(!is.null(at)) {axis(1, at=at); axis(2, at=at)}
	polygon(c(0, 0, 1), c(0, 1, 0), col=flatcol, border = paretoset_border)
	polygon(c(0,g(x))/(2*g(nsh)),c(0,f(x))/(2*f(nsh)), col=setcol, border = paretoset_border)
	if(diagonal) abline(0,1, col = "grey", lty = 2)
	points(g(nsh), f(nsh), pch=19, col=grey(.8))
	if(!is.null(alt)) points(g(alt)/(2*g(nsh)), f(alt)/(2*f(nsh)), pch=19, col="red")
	lines(x2/(2*g(nsh)),(N/x2)/(2*f(nsh)), col=NBScol)
	lines(x2,N/x2, col=grey(.8))
	lines(g(x),f(x), type="l",  col=grey(.8))
	points(.5, .5, cex=1.5, col= NBScol, pch = 19)
	text(g(nsh)+.075, f(nsh), "NBS", col=grey(.8))
	text(.575, .5, "NBS", cex=1.1, col=NBScol)
	if(altlab){text(g(alt)/(2*g(nsh))-.05, f(alt)/(2*f(nsh)), "Alt", cex=1.1, col="red")}
	box()

  }
	}

