# Helper files


#' Helper to shift coordinates
#'
#' Generate a sequence centered on s, with length l, and gaps of d. Used to align multiple arrows.
#' @param
#' @keywords
#' @export
#' @examples
#' gt_shift()

gt_shift <- function(s, # center
                      l, # length
                      d  # size of step
                      ) {
              sort(s + d * rep(0:l, each = 2)[2:(l + 1)] * (-1)^(1:l), decreasing = TRUE)
            }


#' Helper to make best response arrows for bimatrix
#'
#' A function to create best response arrows for the matrix
#' @param
#' @keywords
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
  width=3
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
      if (!vert) arrows((na.omit(as.vector(A))),gt_shift((nrow(X)-i+.5),r, .3/r) - .15/r*(r%%2==0), na.omit(as.vector(B)),gt_shift((nrow(X)-i+.5),r, .3/r) - .15/r*(r%%2==0), lwd=width, col=color, angle=35/num)
      if (vert) arrows(as.vector(gt_shift((i-.5),r, .3/r))- .15/r*(r%%2==0), na.omit(as.vector(A)), as.vector(gt_shift((i-.5),r, .3/r)) - .15/r*(r%%2==0), na.omit(as.vector(B)), lwd=width, col=color, angle=35/num)}
  }
 }


#' Helper to draw stars
#'
#' A function to draw stars
#' @param
#' @keywords
#' @export
#' @examples
#' gt_star()

# A function to draw a star
gt_star <- function(
  x,                     # x coordinate
  y,                     # y coordinate
  rad=1,                 # radius
  phi=0,                 # rotation of stars (counter-clockwise, radians),
  starfill="turquoise",  # color inside star
  outer="black",         # color of border
  tips=8                 # tips
  ){
  if (length(x)!=length(y)) stop("Vector lengths of x and y are different")
  for (i in 1:length(x)){
      polygon(rad*c(rep(c(1,.5),tips),1)*cos(seq(0,2,1/tips)*pi + pi/2 + phi)+x[i],
              rad*c(rep(c(1,.5),tips),1)*sin(seq(0,2,1/tips)*pi + pi/2 +phi) + y[i],
              col=starfill, border=outer, lwd=3)
    }
  }
