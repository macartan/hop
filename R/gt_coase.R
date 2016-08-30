#' Plot Coase Theorem Illustrations
#'
#' gt_coase plots the utility possibility set either from a bimatrix game or from a utlity possibility set paramaterized by pareto frontier f
#' @param f Function that mpas a possibility frontier
#' @param X Payoff matrix
#' @param SQ status quo outcome: defaults to minimax when game matrix provided
#' @param feasible Include efasible set
#' @param bargain Include bargaining set
#' @keywords Coase Theorem
#' @export
#' @examples
#' #Illustration of bargaining outcomes from possibility set
#' gt.coase(f=function(x) 1-x^2, matrix=FALSE, feasible=TRUE,  bargain = TRUE,  SQ=c(0,1))
#' #Illustration of bargaining outcomes from matrix game
#' gt.coase(X=matrix(c(2,3,0,1),2), matrix=TRUE, feasible=TRUE,  bargain = TRUE)


gt_coase = function(X=NULL, Y=t(X),
                    f=function(x) {1-x},
                    pointsize = 1,
			SQ=c(0,0),
			lab_SQ=TRUE,
			SQ_lab = "SQ",
			feasible=TRUE,
			bargain = FALSE,
			xlim=c(ifelse(matrix, min(X,Y), 0),ifelse(matrix, max(X,Y), 1.65)), ylim=xlim,
			xlab=expression(italic(u)[1]),
			ylab=expression(italic(u)[2]),
			main="",
			mainsize=1,
			addarrows=TRUE,
			figtitle="",
			col1 = "red",
			col2="pink",
			col1b=col1,
			col2b=col2,
			arrcol="black",
			offs=.15,
			textsz = 1,
			length=.1,
			axes=FALSE,
			mar=2*c(1,1,1,1),
			mgpY=c(1,1,0),
			mgpX=c(1,1,0)){

if(!is.null(X)) {matrix <- TRUE} else {matrix <- FALSE}
if((is.null(X)) & (length(SQ)== 1)) stop("Two dimensional SQ needed for non-matrix games")
if(!is.null(X) & (SQ == "minimax")[1]) SQ <- c(gt_minimax(X)[[1]], gt_minimax(t(Y))[[1]])

par(mar=mar)

if(matrix){Z=matrix(c(X, Y), ncol=2)
			opt = Z[Z[,1]+Z[,2] == max(Z[,1]+Z[,2]),]
      if(!is.null(nrow(opt))) opt <- opt[1,]
			z = max(Z[,1]+Z[,2])
			plot(Z, xlab=xlab, ylab=ylab, main=main, cex.main=mainsize, xlim=xlim, ylim=ylim)
			if(feasible){
					# Feasible Set -- from correlated strategies
					hpts <- chull(Z)
					hpts <- c(hpts, hpts[1])
					polygon(Z[hpts, ], col=col1, border = col1b)
			}
			points(Z, pch=19, cex=pointsize)
			}

if(!matrix){
			x = seq(0,1,.01)
			opt = c(x[x+f(x) == max(x+f(x))], f(x[x+f(x) == max(x+f(x))]))
			z = sum(opt)
			plot(x,f(x), type="n", xlab="", ylab="",  cex=2, xlim=xlim, ylim=ylim, axes=axes); box()
			title(ylab=ylab,mgp=mgpY)
			title(xlab=xlab,mgp=mgpX)
			}
if(bargain) {polygon(c(SQ[1],SQ[1], z-SQ[2]),c(SQ[2], z-SQ[1], SQ[2]), col = col2, border = NA)}

if(matrix & feasible){polygon(Z[hpts, ], col=col1, border = NA); points(Z, pch=16)}

if(!matrix) {polygon(c(0,x),c(0,f(x)), col = col1, border = NA)}

if(lab_SQ) {points(SQ[1],SQ[2], cex=2, pch=19); text(SQ[1],SQ[2]+offs, SQ_lab, pos=4, cex=textsz )}

if(bargain) {
	arr = function(b,a,z,p) {arrows(b,a,p*b + (1-p)*(z-a), p*(z-b)+(1-p)*a, length=length, lwd=1.5, col=arrcol)}
	points(SQ[1],SQ[2], cex=2, pch=19)
	points(opt[1], opt[2], col = col2b, pch=19)
	text((z+SQ[1]-SQ[2])/2 + offs, (z+SQ[2]-SQ[1])/2+ offs, "Possible deals", srt=-45, cex=textsz )
	segments(SQ[1], z-SQ[1], z-SQ[2], SQ[2], lwd=4, col = col2b)
	if(addarrows) for(p in c(.1, .5, .9)) arr(SQ[1],SQ[2],z,p)
box()
}}


