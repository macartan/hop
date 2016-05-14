#' Payoff Matrix
#'
#' This function helps you plot a payoff matrix and identify pure strategy Nash equilibria.
#' Used for two player normal form games and finite, though possibly different, strategy sets.
#' Credit for base code: Neelanjan Sircar
#' @param X Payoff matrix for player 1. Defaults to coordination.
#' @param Y Payoff matrix for player 2. Defaults to coordination.
#' @param P1 name of player 1 (row)
#' @param P2 Name of player 2 (column)
#' @param labels1 Names of strategies for player 1
#' @param labels2 Names of strategies for player 2
#' @param labelsize Size of labels
#' @param arrow1 draw response arrows for player 1
#' @param arrow2 draw response arrows for player 2
#' @param arrow1col color of best response arrows; same for arrow2col
#' @param width1 Width of arrows; same for width2
#' @param nash mark the Nash equilibrium, if there are any
#' @param alength length of arrow head, defaults to .25
#' @keywords Payoff matrix, Nash
#' @export
#' @examples
#' M1 <- matrix(1:12, 3, 4)
#' gt_bimatrix(M1, M1, labels1 =paste(1:3), labels2 = paste(1:4), main = "Asymmetric", mainline = -1)
#' # Here is a more conservative style:
#' gt_bimatrix(matrix(c(1,0,0,0), 2, 2),
#'             labels1 = c("U","D"), labels2 = c("L","R"),
#'             pty = "m", matrixfill=NULL, nash = FALSE, arrow1= FALSE,
#'             asp = .45, tfont = "serif", tscale = .8)

gt_bimatrix <- function(
  # Payoff matrix
    X = matrix(c(1,0,0,1),2),
    Y=t(X),
    P1="Player 1",
    P2="Player 2",
    labels1 = NULL,
    labels2 = NULL,
    labelsize=NULL,
  # Arrows
    arrow1=TRUE,
    arrow2=arrow1,
    arrow1col=gray(.4),
    arrow2col=arrow1col,
    width1=3,
    width2=width1,
    arrowdist=.2,
    alength = .25,
    space=max(arrowdist, radius+.075),
  # Nash
    nash=TRUE,
    radius=.2,
    starfill="red",
    starborderlwd = 1.5, # Thickness of border of Nash star
    tips=8,
    nashborder="black",
  # Formating
    tfont=NULL,
    pty="s",
    asp = NULL,
    srt1=0,
    srt2=0,
    mar=c(.7,.7,.7,.7),
  # Colors
    matrixfill=gray(.7),
    pcol1="black",
    pcol2=pcol1,     # player colors
    numcol1=pcol1,
    numcol2=pcol2,   # payoff colors
    scol1=pcol1,
    scol2=pcol2,     #strategy colors
    col.main="red",
    bordercol="white",
  # Lines
    gameborder=TRUE,
    borderdim = NULL,
    lwd=1.5,
    offset1=c(.8,.8),
    offset2=c(.2,.2),
  # Scales
    u=NULL,
    playersize=NULL,
    labelsdim = NULL,
    pdim =NULL,
    tscale=1,  # scale text
    maincex=1,
  # Arguments for plot title
     main="",
     mainline= -.5
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
  if(is.null(asp)) asp <- par("pin")[1]/par("pin")[2]
  plot(1,1, xlim=c(borderdim[1],ncol(X)+.1), ylim=c(-0.1,nrow(X)+borderdim[2]), ann=F, axes=F, asp=asp, type="n")
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

  if (arrow1) gt_BRarrow(X,Y,space=space, nash=nash, color=arrow1col, width=width1,  arrowdist=arrowdist, alength = alength)
  if (arrow2) gt_BRarrow(X,Y,space=space, nash=nash, color=arrow2col, width=width2,  arrowdist=arrowdist, vert=TRUE, alength = alength)

  pureNEx <- array(NA, c(nrow(X), ncol(X)))
  pureNEy = pureNEx
  for (i in 1:nrow(X)){
    for (j in 1:ncol(X)){

      pureNEx[i,j] <- ifelse((X[i,j]==max(X[,j]) & (Y[i,j]==max(Y[i,]))),j-.5, NA)
      pureNEy[i,j] <- ifelse((X[i,j]==max(X[,j]) & (Y[i,j]==max(Y[i,]))),nrow(X)-i+.5, NA)
    }}
  if (nash) gt_star(as.vector(pureNEx), as.vector(pureNEy),
                    rad=radius, phi=0, starfill=starfill, tips=tips,
                    starborderlwd=starborderlwd)
}
