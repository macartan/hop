#' Helper to slope text
#'
#' Note angle is a bit messy because of aspect ratios; fixer can be used to adjust
#' @param labels text
#' @keywords text
#' @export
#' @examples
#' frame()
#' gt_slope_text("quadratic", f = function(x) x^2)
gt_slope_text = function(labels, f, xl=0, xh=1, vshift=.05, col="black", fixer=1, pos = NULL, adj = c(.5,0),
                         cex = 1,
                         shadow = TRUE,
                         shade = "white" # for shadow
){
  ft <- text
  if(shadow) ft <- function( ... ) gt_shadowtext(bg = shade, ...)
  TX = c(" ", strsplit(labels, "")[[1]]," ")
  k = length(TX)
  z = xl + ((0:(k-1))/k)*(xh-xl)
  angles = 	c(
    0,
    (
      atan2(
        f(z)[3:k] - f(z)[1:(k-2)],
        fixer * 2 * (xh - xl) / (k)
      ) * 180 / pi
    ),
    0
  )

  for(i in 1:k){ft(
    x=z[i],
    y=f(z[i])+vshift,
    labels=TX[i],
    col = col,
    srt=angles[i],
    pos = pos,
    adj = adj,
    cex = cex)}
}
