
## Cascade

# A  cascade  can  also  form,  for  example,  if  the  first  two  decisions  differ  and  the  next  two  match.
# In  all  cases,  it  takes  an  imbalance  of  two  de-  cisions  in  one  direction  to  overpower  the  informational  content  of  subsequent  individual  signals.
# PosteriorFunction to figure if you should say Yes or no given a string with n relevant Yeses and m relevant no's

#' # Legislative bargaining with three equally weighted voters. For any recognition probabilities, return the equilibrium allocations. Note: when lowest unit has 0 probability then a random solution taken
#'
#' @param x Vector with signals received
#' @keywords Cascade
#' @export
#' @examples
#'  gt_declarations(c(1,0,1,1,0))
#'  gt_declarations(c(0,0,1,1,0))

gt_declarations = function(x){
  SayYes      <-  function(x) { t = 2^(sum(x)) / ( 2^(sum(x)) + 2^(sum(1-x)))
  ifelse(t==.5, x[length(x)]==1, t>.5)  # assume opt for private signal in case of indifference
  }

  # Would you conclude differently with a different private signal?
  Informative <- function(x) {z<-x
  z[length(x)] <- (1- x[length(x)])
  SayYes(x)    != SayYes(z)
  }

  Declarations = matrix(NA, length(x))
  Declarations[1:2] <- x[1:2]
  Informatives = matrix(FALSE, length(x))
  Informatives[1:2] <- c(TRUE, TRUE)
  for(i in 3:length(x)) {
    data <- c(Declarations[Informatives], x[i])
    Declarations[i]<-SayYes(data)
    Informatives[i]<-Informative(data)
  }
  return(data.frame(Signals = x, Declarations=Declarations, Informatives=Informatives))
}

