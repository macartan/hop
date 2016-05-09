



#' Banzhaf index
#'
#' We generate a function for calculating the Banzhaf index and apply it to two different ?q-rules? for Nassau county data. The Banzhaf function can be defined in this way for any rule using this approach. +1 for decisive coalitions, -1 otherwise
#' @param w Vector of voting weights
#' @param q Threshold for voting success
#' @keywords Banzhaf, Decision Rules
#' @export
#' @examples
#'  gt_banzhaf(w = 1:5, q=.5)

gt_banzhaf <-  function(
                        w,         # Votes
                        q = .5     # q rule
                        ){
        if(q >= 1) stop("Threshold q should be less than 1")
        if(q <= 0) stop("Threshold q should be greater than 0")
        x  <- gt_permv(rep(2,length(w)))
        BH <- colSums(-1 + 2 * x[gt_qrule(x, w, q),])
        BH/sum(BH)
         }

#' Plot Banzhaf index against normalized raw weights
#'
#' @param w Vector of voting weights
#' @param q Threshold for voting success
#' @keywords Banzhaf, Decision Rules
#' @export
#' @examples
#'  gt_plot_banzhaf(w = 1:5, q=.5)
#'  gt_plot_banzhaf(w=c(1, 1, 3,7,9,9)/30, q = .5)

gt_plot_banzhaf <- function(
  w,
  q,
  col1="red",
  col2="white"
  ){	                             # Define plotting function
    w <- w/sum(w)
    k <- length(w)
    b <- gt_banzhaf(w, q)
    top <- max(c(w,b))*1.1

    plot(1, 1, type="n", ylim=c(0,top), xlim=c(0,3), axes=F,
    xlab="", ylab=""); box()
    polygon(c(1,1,2,2),c(0,top,top,0), col=col1)
    segments(rep(1, k-1), w, rep(2, k-1), b, col=col2)
    text(c(.7, 2.3, .7, 2.3), c(0,0, top, top), c(0,0, "Votes", "Power"))
    text(c(rep(.7,k), rep(2.3,k)), c(w,b), round(c(w,b),2), pch=.4)
    }
