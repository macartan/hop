#' Helper to shift coordinates
#'
#' Generate a sequence centered on s, with length l, and gaps of d. Used to align multiple arrows.
#' @param s a number -- center of shift
#' @param l a number -- length of shift
#' @param d a number -- size of shift
#' @keywords shift
#' @examples
#' gt_shift()
gt_shift <- function(s, # center
                      l, # length
                      d  # size of step
                      ) {
              sort(s + d * rep(0:l, each = 2)[2:(l + 1)] * (-1)^(1:l), decreasing = TRUE)
            }


#' Helper to make best response arrows for bimatrix
#' @param X x coordinated for arrow
#' @param Y y coordinated for arrow
#' @keywords arrow
#' @examples
#' gt_BRarrow()
gt_BRarrow <- function(
  X,
  Y,
  vert=FALSE,
  arrowdist=.2,
  space=.15,
  color=gray(.3),
  nash=TRUE,
  width=3,
  alength = .25
  ){
  d <- array(arrowdist, c(nrow(X),ncol(X))); numarrow1 <- rep(NA, nrow(Y)); numarrow2 <- rep(NA, ncol(Y)) ##summarizing arrow info
  for (i in 1:nrow(Y)){
    for (j in 1:ncol(Y)){
      if (nash) d[i,j] <- ifelse((X[i,j]==max(X[,j])) & (Y[i,j]==max(Y[i,])),space, arrowdist)
      numarrow1[i] <- sum(Y[i,j]==max(Y[i,]))*(ncol(Y)-sum(Y[i,j]==max(Y[i,])))
      numarrow2[j] <- sum(X[i,j]==max(X[,j]))*(nrow(Y)-sum(X[i,j]==max(X[,j])))
    }}
  num <- max(c(numarrow1, numarrow2,1)) #(1 included to avoid 0 denominator)
  if(vert) {Y <- t(X)}
  for (i in 1:nrow(Y)){
    A <- array(NA, c(ncol(Y), ncol(Y))); B <- array(NA, c(ncol(Y), ncol(Y)))
    for (j in 1:ncol(Y)){
      for (k in 1:ncol(Y)){
        if((Y[i,j]==max(Y[i,])) & (Y[i,j]>Y[i,k])){
          if (!vert)	 A[j,k] <- k-.5-d[i,k]*(k>j)+d[i,k]*(k<j);  if(vert) A[j,k] <- ncol(Y)-k + .5 +d[k,i]*(k>j)-d[k,i]*(k<j)
          if (!vert)	 B[j,k] <- j-.5-d[i,j]*(j>k)+d[i,j]*(j<k); if(vert) B[j,k] <- ncol(Y)-j + .5 -d[j,i]*(k>j)+d[j,i]*(k<j)
        }}}
    r <- sum(!is.na(A))  ## number of arrows
    if (r!=0) {
      if (!vert) arrows((na.omit(as.vector(A))),gt_shift((nrow(X)-i+.5),r, .3/r) - .15/r*(r%%2==0), na.omit(as.vector(B)),gt_shift((nrow(X)-i+.5),r, .3/r) - .15/r*(r%%2==0), lwd=width, col=color, angle=35/num, length = alength)
      if (vert) arrows(as.vector(gt_shift((i-.5),r, .3/r))- .15/r*(r%%2==0), na.omit(as.vector(A)), as.vector(gt_shift((i-.5),r, .3/r)) - .15/r*(r%%2==0), na.omit(as.vector(B)), lwd=width, col=color, angle=35/num, length = alength)}
  }
 }






#' Drawing a curve
#'
#' Curve segment starting; let circle be indxed by 0,1, where 0 is the beginning of the circle (center right) moving clockwise to 1 back to center right
#' run is the length of the curve, so run = 1 is a full circle, run = .5 is a semi circle etc; run -x, goes anticlockwise
#' Then semi circle might start at .5 and end at 1, or start even at .75 and end at 2.
#'
#' @param cx a number x center of curve
#' @param cy a number y center of curve
#' @keywords curve arrows
#' @examples
#' gt_curve()
gt_curve = function(cx=0,
                    cy=0,
                    radx=1,
                    rady=1,
                    col="red",
                    fine=100,
                    type="l",
                    from=0,
                    run=1,
                    new=FALSE,
                    main="",
                    lwd=1,
                    arrow=FALSE,
                    arlength = (radx+rady)/4,
                    tilt=0,
                    lty=1,
                    vectors = FALSE){
  z = seq(0,-run*2*pi, length=fine)+(2*pi)*from
  if(vectors) {cbind(cos(z)*radx+cx, sin(z)*rady+cy + tilt*cos(z)*radx)
  } else {
    if(new) plot(cos(z)*radx+cx, sin(z)*rady+cy + tilt*cos(z)*radx, col=col, type=type, lty=lty, main=main, lwd=lwd)
    if(!new) points(cos(z)*radx+cx, sin(z)*rady+cy + tilt*cos(z)*radx, col=col, type=type, lty=lty,lwd=lwd)
    if(arrow) arrows(cos(z[fine-1])*radx+cx, sin(z[fine-1])*rady+cy + tilt*cos(z[fine-1])*radx,
                     cos(z[fine])*radx+cx,   sin(z[fine])*rady+cy + tilt*cos(z[fine])*radx,
                     col=col,
                     length=arlength, lwd=lwd, lty=lty)
  }
}


#' Generates set of permutations. Credit: Bernd Beber
#'
#' @param v a vector giving number of elements in each variable
#' @keywords permutations
#' @export
#' @examples
#' gt_permv(c(2,3))
gt_permv <- function(v) {
  sapply(1:length(v), function(x) {
    rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
         length.out=prod(v))
  } ) - 1
}

