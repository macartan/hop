#' Helper for shadow text
#'
#' Place a shadow behind text (white default)
#' Helper to make text more visible
#' Credit: Greg Snow http://blog.revolutionanalytics.com/2009/05/make-text-stand-out-with-outlines.html
#'
#' @param x X location
#' @param y Y location
#' @param labels text
#' @param col Text color
#' @param bg Background (shadow) color
#' @param n_shadows Number of shadows to cast: deault is 12
#' @keywords text
#' @export
#' @examples
#' frame()
#' gt_shadowtext(x = .5, y = .5, labels = "text", col = "white", bg = "red")
gt_shadowtext <- function(x,
                          y=NULL,
                          labels,
                          col='black',
                          bg='white',
                          n_shadows = 12,
                          theta= seq(pi/4, 2*pi,
                                     length.out=n_shadows),
                          r=0.1, ... ) {
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo,
          labels, col=bg, ... )
  }
  text(xy$x, xy$y, labels, col=col, ... )
}
