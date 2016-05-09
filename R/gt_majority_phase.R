#' Phase plot for majority rule. Requires majority preference relation function.
#'
#' @param P Matrix with location of ideal points
#' @param n number of arrow per region
#' @param raylengths vector with lengths of distances to examine
#' @param grid number of grid points
#' @keywords Plott
#' @export
#' @examples
#'  gt_majority_phase(matrix(c(0, 0, .5, 1, 1, 0), 2), 31, raylengths=.5)

gt_majority_phase = function(
  P,
  n = 11,
  grid = 4,
  pointsize=2,
  box = TRUE,
  raylengths=c(.5),
  raycols=c("red", "pink"),
	rayarrs=c(.15,.1),
  q=.5,
  pointcol="grey",
  lwd=1,
  main = ""
  ){
  plot(P[1,], P[2,], type="n", pch=19,  asp=1, axes=F, ann=F)
  title(main)
  if(box){box()}
	for(ro in 0:grid){
		for(co in 0:grid){
			x = c(ro/grid, co/grid)
			for(i in 1:n){
				for(j in length(raylengths):1){
					y = x+raylengths[j]*matrix(c(cos(2*i*pi/n)/grid, sin(2*i*pi/n)/grid), 2)
					if(gt_maj_pref(P, y, x, q=q)){
					  arrows(x[1], x[2], y[1], y[2], col = raycols[j], length = rayarrs[j],lwd=lwd)
					  }
				}}
			points(x[1], x[2], col= raycols[j], pch=19, cex=pointsize/2)
			}}
	points(P[1,], P[2,], col=pointcol, pch=19, cex=pointsize)
	}
