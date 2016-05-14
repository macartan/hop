
#' Helper to draw stars
#'
#' A function to draw stars
#' @param tips number of points
#' @keywords stars
#' @export
#' @examples
#' gt_star()
#'
gt_star <- function(
  x,                     # x coordinate
  y,                     # y coordinate
  rad=1,                 # radius
  phi=0,                 # rotation of stars (counter-clockwise, radians),
  starfill="turquoise",  # color inside star
  outer="black",         # color of border
  tips=8,                # tips
  starborderlwd = 3      # star border
){
  if (length(x)!=length(y)) stop("Vector lengths of x and y are different")
  for (i in 1:length(x)){
    polygon(rad*c(rep(c(1,.5),tips),1)*cos(seq(0,2,1/tips)*pi + pi/2 + phi)+x[i],
            rad*c(rep(c(1,.5),tips),1)*sin(seq(0,2,1/tips)*pi + pi/2 +phi) + y[i],
            col=starfill,
            border=outer,
            lwd=starborderlwd)
  }
}

