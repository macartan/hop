

#' Define q Rule (strict inequality)
#'
#' Assesses whether a coalition is sufficient to support an action
#'
#' @param x Vector of indicators of coalition membership
#' @param w Vector of voting weights
#' @param q Threshold for voting success
#' @keywords Condorcet, Decision RUles
#' @export
#' @examples
#'  gt_qrule(x=c(1,1,0,0,0), q=.5)
#'  gt_qrule(x=c(1,1,1,0,0), q=.5)

gt_qrule = function(x,                                # Coalition membership
                    w = rep(1/length(x), length(x)),  # Voting weights
                    q = .5                            # Threshold for sucess
                  ) {
                    x%*%w > (q*sum(w))
                  }
