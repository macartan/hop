
#' Legislative bargaining
#'
#' Legislative bargaining with three equally weighted voters. For any recognition probabilities, return the equilibrium allocations. Note: when lowest unit has 0 probability then a random solution taken
#'
#' @param p A vector of recognition probabilities
#' @param seed A seed
#' @keywords MIW
#' @export
#' @examples
#'  gt_leg_barg(c(.7,.3,0))
gt_leg_barg <- function(p, seed = NULL) {
          if(!is.null(seed)) set.seed(seed)
          if(min(p) <0 ) stop("positive weights only please")
          p <- p/sum(p)
          mp <- max(p)
          np <- min(p)
          op <- 1-mp -np
          v  <- p

          if(np != 0){
            v[p==mp]	= mp/(2-mp)
            v[p!=mp] = (1-mp/(2-mp))/2}

          if(mp<.5 & np>0) v <- rep(1/3,3)

          if(np == 0)  {
            print("Anything goes: one of multiple equilibria selected")
            VMAX     <- runif(1, min = mp/(mp+2*op), max = mp/(mp+op))
            VMID     <- VMAX / (mp/op)
            v[p==mp] <- VMAX
            v[round(p,4) == round(op,4)] <- VMID  # deals with awkward precision issue
            v[p==np] <- 1-VMAX - VMID
          }

         return(paste("allocations:", paste(round(v,3), collapse = ", ")))}
