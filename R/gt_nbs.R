#' Nash bargaining solution
#'
#' @param u1 a function, increasing in x
#' @param u2 a function, decreasing in x
#' @param solution_only Just show solution
#' @keywords Nash bargaining
#' @export
#' @examples
#'  gt_nbs(u1 = function(x) x^.5, u2 = function (x) 1 - x)

gt_nbs= function(u1 = function(x) x^.5,
                 u2 = function(x) (1-x) ,
                 solution_only = FALSE,
                  setcol="red",
                  flatcol="grey",
                  length=500,
                  xtop = 1,
                  xlim = c(0,xtop),
                  ylim = xlim,
                  alt=NULL,   # may be be set to some value to indicate a second comparison point on the indifference curve
                  altlab=FALSE,
                  paretoset_border = NA,
                  NBScol = "black",
                  diagonal = TRUE,
                  at = c(0,1)
                  ){

# Say:
# g = function(x){(x^.8)*.5}
# f = function(x){(1-x^2)^.5}
# Then:
# NBS = max((x^.8)*.5*(1-x^2)^.5 = max .8ln(x)+.5ln(1-x^2);  .8/x -x/(1-x^2) = 0; x=(.8/1.8)^.5 = 2/3
# SLOPE: b=-f(nsh)/g(nsh)
# Y intercept: f(nsh)=a+b*g(nsh) = a-f(nsh) --> a = 2*f(nsh)
# X intercept:  0 = a - bz; z = a/b =  2*g(nsh)
f <- u2
g <- u1
x 	<-  seq(0,xtop, length = length)
x2 	<-  seq(0,max(xlim)*2, length = length)
nsh	<-  x[sapply(x, function(i) f(i)*g(i))==max(sapply(x, function(i) f(i)*g(i)))]  # NASH Bargaining Solution
if(length(nsh)>1) nsh <- sample(nsh,1)
N	  <-  g(nsh)*f(nsh)

if(!solution_only) par(mfrow=c(1,3))

plot(g(x),f(x),
     type="n",
     xlab=expression(u[1]),
     ylab=expression(u[2]),
     main="Original Problem",
     xlim=xlim,
     ylim=ylim,
     axes =FALSE)
	if(is.null(at)) {axis(1); axis(2)}
  if(!is.null(at)) {axis(1, at=at); axis(2, at=at)}
  polygon(c(0,g(x)),c(0,f(x)), col=setcol, border = paretoset_border)
	if(diagonal) abline(0,1, col = "grey", lty = 2)
	points(g(nsh), f(nsh), pch=19, col=NBScol)
  if(!is.null(alt)){points(g(alt), f(alt), pch=19, col="red")}
	lines(x2,N/x2, col=NBScol)
	lines(x2,1.2*N/x2, col=grey(.8))
	lines(x2,1.4*N/x2, col=grey(.8))
	text(g(nsh)+.075, f(nsh), "NBS", cex=1.1, col=NBScol)
	if(altlab){text(g(alt)-.05, f(alt), "Alt", cex=1.1, col="red")}
	box()

if(!solution_only){

plot(g(x),f(x), type="n", xlab=expression(u[1]), ylab=expression(u[2]), main="Flat Superset",
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

plot(g(x),f(x), type="n", xlab=expression(paste(u[1], " (rescaled)")), ylab=expression(paste(u[2], " (rescaled)")), main="Transformed Superset",
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

