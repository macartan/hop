#' Melzer Richard Illustration
#'
#' gt_plot_mr plots returns to actors in a Melzer RIchard economy given an endowment e
#' @param e Endowment
#' @param t tax range
#' @param gt_mr_distribution function showing distribution given taxes
#' @param gt_mr_production function giving production given taxes
#' @param gt_mr_maxu function giving max utility given endowment
#' @param gt_mr_utility function giving utility given taxes
#' @keywords Melzer Richard, Tax
#' @export
#' @examples
#' #Illustration of bargaining outcomes from possibility set
#' gt_plot_mr(e=.5, main = "Endowment = .5")


gt_plot_mr <- function(endowment = .5,
                       e = endowment,
                       tax = seq(0,1,.01),
                       ymax = gt_mr_utility(gt_mr_maxu(e), e),
                       main = paste("Endowment =",e),
                       xlab = "",
                       scale = 1,
                       ylab="",
                       color = "grey",
                       gt_mr_distribution =  function(tax, e)  tax*(1-tax)/2,
                       gt_mr_production   =  function(tax, e)  (1-tax)^2*e/4 ,
                       gt_mr_utility      =  function(tax, e)  (gt_mr_distribution(tax, e) + gt_mr_production(tax, e)),
                       gt_mr_maxu         =  function(e)     (1-e)/(2-e)
                       ){
  plot(tax, scale*gt_mr_distribution(tax, e), type = "l",
       ylim = c(0, ymax),
       xlab = xlab,
       ylab = ylab,
       axes = FALSE,
       main = main
  )
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
         color)
  lines(tax, scale*gt_mr_distribution(tax, e))
  axis(2, at = c(0, 1))
  axis(1, at = c(0, .5, 1))
  lines(tax, scale*gt_mr_production(tax, e), lty = 2)
  lines(tax, scale*gt_mr_utility(tax, e), col = "red")
  points(gt_mr_maxu(e), scale*gt_mr_utility(gt_mr_maxu(e), e), col = "red", bg = "red", pch = 16)
  box()
}




