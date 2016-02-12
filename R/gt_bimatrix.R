
#' Payoff Matrix
#'
#' This function helps you plot a payoff matrix and identify pure strategy Nash equilibria
#' @param X Payoff matrix for player 1. Defaults to coordination.
#' @param Y Payoff matrix for player 2. Defaults to coordination.
#' @keywords
#' @export
#' @examples
#' gt_bimatrix()
#'


# plots bimatrix and shades pure strat NE whereX and Y are 2x2 payoff matrices, and c is scale parameter for how big the text should be,
# plotstyle can be "s"or "m" where "s" uses square plotting and "m" uses maximal area plotting
# srt1,2 Turns the writing on the player labels
# u scale parameter for utility labels
# matrixfill=gray(.9), numcol="black", gameborder=T, bordercol="white", borderdim = NULL,
# main : plot title
# tscale : scaling parameter to control all labels
# offset1=c(.8,.8), offset2=c(.2,.2), arrowdist=.15, radius=.2, space=max(arrowdist, radius+.075),
# arrow1=TRUE, arrow2=TRUE,
# arrow1col=gray(.4), arrow2col=gray(.4),
# width1=3, width2=3,
# nash=T, nashcol=gray(.6), nashborder="black",
# labels1=NULL, labels2=NULL, labelsize=NULL, p1="Player 1", p2="Player 2", playersize=NULL, labelsdim = NULL, pdim =NULL){

gt_bimatrix <- function(
  X = matrix(c(1,0,0,1),2),              # Payoff matrix
  Y=t(X),
  P1="Player 1",
  P2="Player 2",
  labels1 = NULL,
  labels2 = NULL,
  labelsize=NULL,
  u=NULL,
  pty="s",
  srt1=0,
  srt2=0,
  mar=c(.7,.7,.7,.7),
  matrixfill=gray(.7),
  pcol1="black",
  pcol2=pcol1,     # player colors
  numcol1=pcol1,
  numcol2=pcol2,   # payoff colors
  scol1=pcol1,
  scol2=pcol2,     #strategy colors
  gameborder=TRUE,
  bordercol="white",
  borderdim = NULL,
  lwd=3,
  offset1=c(.8,.8),
  offset2=c(.2,.2),
  arrow1=TRUE,
  arrow2=arrow1,
  arrow1col=gray(.4),
  arrow2col=arrow1col,
  width1=3,
  width2=width1,
  arrowdist=.2,
  space=max(arrowdist, radius+.075),
  nash=TRUE,
  radius=.2,
  starfill="orange",
  tips=8,
  nashborder="black",
  playersize=NULL,
  labelsdim = NULL,
  pdim =NULL,
  tscale=1,
  main="",
  tfont=NULL,
  maincex=1,
  col.main="red",
  mainline= -.5  # Arguments for plot title
){

  if(!is.null(labels1)) labels2sub <- labels1
  if(is.null(labels1))  labels2sub <-mapply(paste, "Y",1:ncol(Y), sep="")
  if(is.null(labels2))  labels2 <- labels2sub
  if(is.null(labels1))  labels1 <- mapply(paste, "X",1:nrow(X), sep="")

  # Checks
  if ((nrow(X)!=nrow(Y))|(ncol(X)!=ncol(Y))) stop("Dimensions of X and Y are not equal")
  if (length(labels1)!=nrow(X)) stop ("Row labels do not match options")
  if (length(labels2)!=ncol(Y)) stop ("Col labels do not match options")
  if (is.character(labels1)==FALSE | is.character(labels2)==FALSE) stop ("Labels are not character strings")

  # Prepare Scaling (May be modified wth kscale)
  k <- tscale*2.4*(.85)^(max(length(labels1), length(labels2))-2)
  if (is.null(u)) u <- tscale*(3 - ((max(nchar(as.character(c(X,Y)))))- 1)*.5 - .25*(max(length(labels1), length(labels2)) - 1))
  if (is.null(labelsize)) labelsize <- k; if (is.null(playersize)) playersize<- k
  if (is.null(borderdim)) borderdim <- c(-.3*max(length(labels1), length(labels2)) - .1*labelsize, .25*max(length(labels1), length(labels2)) + .1*labelsize)

  # Start Graph
  par(pty=pty, mar=mar)
  plot(1,1, xlim=c(borderdim[1],ncol(X)+.1), ylim=c(-0.1,nrow(X)+borderdim[2]), ann=F, axes=F, asp=par("pin")[1]/par("pin")[2], type="n")
  title(main=main, cex.main = maincex, col.main=col.main, line = mainline)
  if(gameborder) polygon(c(borderdim[1],borderdim[1], ncol(X)+.1, ncol(X)+.1, borderdim[1]), c(-.1, nrow(X)+ borderdim[2], nrow(X)+borderdim[2], -.1, -.1), col=bordercol, border=NA)
  polygon(c(0,ncol(X),ncol(X),0), c(0,0,nrow(X),nrow(X)), lwd=lwd, col=matrixfill)
  segments(1:(ncol(X)-1), rep(0,(ncol(X)-1)), 1:(ncol(X)-1), rep(nrow(X),(ncol(X)-1)), lwd=lwd)
  segments(rep(0,(ncol(X)-1)), 1:(nrow(X)-1), rep(ncol(X),(ncol(X)-1)), 1:(nrow(X)-1), lwd=lwd)

  a1 <- rep((1-offset1[1]),nrow(X)); for(i in 2:ncol(X)) a1<-c(a1,rep((i-offset1[1]),nrow(X)))
  b1 <- rep(seq((nrow(X)-offset1[2]), (1-offset1[2]),-1), ncol(X))
  a2 <- rep((1-offset2[1]),nrow(X)); for(i in 2:ncol(X)) a2<-c(a2,rep((i-offset2[1]),nrow(X)))
  b2 <- rep(seq((nrow(X)-offset2[2]),(1-offset2[2]),-1), ncol(X))

  text(a1,b1, as.character(as.vector(X)), cex=u, col=numcol1)
  text(a2,b2, as.character(as.vector(Y)), cex=u, col=numcol2)

  if (is.null(labelsdim)) labelsdim <- c(-.10*max(length(labels1), length(labels2)), .08*max(length(labels1), length(labels2)))
  #	if (is.null(pdim)) pdim <-c(-.3*max(length(labels1), length(labels2)), .25*max(length(labels1), length(labels2)))
  if (is.null(pdim)) pdim <-c(-.2*max(length(labels1), length(labels2)), .15*max(length(labels1), length(labels2)))
  # Action Labels
  text(rep(labelsdim[1],nrow(X)), (rep(nrow(X)+.5,nrow(X)) - 1:nrow(X)), labels1, cex=labelsize, family=tfont, font=2, srt=srt1, col=scol1)
  text(1:ncol(X)-.5, rep(nrow(X)+ labelsdim[2],ncol(X)), labels2, cex=labelsize, family=tfont, font=2, srt=srt2, col=scol2)

  # Player Labels
  text(pdim[1], nrow(X)/2, P1, cex=playersize, srt=90, family=tfont, font=2, col=pcol1)
  text(ncol(X)/2, nrow(X)+ pdim[2], P2, cex=playersize, family=tfont, font=2, col=pcol2)

  if (arrow1) gt_BRarrow(X,Y,space=space, nash=nash, color=arrow1col, width=width1, arrowdist=arrowdist)
  if (arrow2) gt_BRarrow(X,Y,space=space, nash=nash, color=arrow2col, width=width2,  arrowdist=arrowdist, vert=TRUE)

  pureNEx <- array(NA, c(nrow(X), ncol(X)))
  pureNEy = pureNEx
  for (i in 1:nrow(X)){
    for (j in 1:ncol(X)){

      pureNEx[i,j] <- ifelse((X[i,j]==max(X[,j]) & (Y[i,j]==max(Y[i,]))),j-.5, NA)
      pureNEy[i,j] <- ifelse((X[i,j]==max(X[,j]) & (Y[i,j]==max(Y[i,]))),nrow(X)-i+.5, NA)
    }}
  if (nash) gt_star(as.vector(pureNEx), as.vector(pureNEy), rad=radius, phi=0, starfill=starfill, tips=tips)
}


# Example of asymmetric matrix
#M1 = matrix(1:12, 3, 4)
#gt_bimatrix(M1, M1, labels1 =paste(1:3), labels2 = paste(1:4), main = "Asymmetric", mainline = -1
#            )
