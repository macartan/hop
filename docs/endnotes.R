## ----include=FALSE, cache=FALSE-----------------------------------------------
rm(list=ls(all=TRUE))
library(knitr)


## ----eval=FALSE---------------------------------------------------------------
## devtools::install_github("macartan/hop")


## -----------------------------------------------------------------------------
library(hop)


## -----------------------------------------------------------------------------
payoffs <-  matrix(c(2,3,0,1),2)
gt_bimatrix(payoffs, labels1=c("C","D"))
gt_bimatrix(payoffs, labels1=c("C","D"), nash=F, arrow1=F)


## -----------------------------------------------------------------------------
payoffs <- matrix(c(2,3,1,0),2)
gt_bimatrix(payoffs, labels1=c("C","D"))


## -----------------------------------------------------------------------------
payoffs <- matrix(c(3,2,0,1),2)
gt_bimatrix(payoffs, labels1=c("C","D"))


## -----------------------------------------------------------------------------
payoffs <- matrix(c(2,3,0,1),2)
gt_folk(payoffs)


## -----------------------------------------------------------------------------
payoffs <- matrix(c(3,2,0,1),2)
gt_folk(payoffs)


## -----------------------------------------------------------------------------
gt_jury(n_voters=7, probability_correct=.6)


## -----------------------------------------------------------------------------
gt_plot_banzhaf(weights = c(1, 1, 3,7,9,9), q_rule = .5)
gt_plot_banzhaf(weights = c(1, 1, 3,7,9,10), q_rule = .6)


## -----------------------------------------------------------------------------
ideals <- matrix(c(0, 0, .5, 1, 1, 0), 2)
gt_majority_phase(ideals)


## -----------------------------------------------------------------------------
ideals <- matrix(c(.2, .2, .5, .9, 1, 0), 2)
gt_majority_phase(ideals, raylengths=c(.5, .2))


## -----------------------------------------------------------------------------
gt_cycles(n_voters = 3, n_motions = 25)


## -----------------------------------------------------------------------------
probabilities <- c(.8,.2,0)
gt_leg_barg(probabilities)


## -----------------------------------------------------------------------------
signals <- c(1,1,1,1,1,1,0,0,0)
D <- replicate(50000, gt_cascade(sample(signals))$Declarations)
mean(D[9,])


## ---- fig.width=9, fig.height=3.5---------------------------------------------
gt_nbs(u1 = function(x) x^.5, u2 = function (x) 1 - x)


## -----------------------------------------------------------------------------
gt_nbs(solution_only = T)


## -----------------------------------------------------------------------------
payoffs <- matrix(c(2,3,0,1),2)
gt_coase(payoffs, bargain = T, SQ = "minimax")
gt_coase(f=function(x) 1-x^2, bargain = T, SQ=c(0,1))


## ---- fig.width=7, fig.height=8-----------------------------------------------
gt_cgm (theta1 = .2, theta2 = .4)


## -----------------------------------------------------------------------------
gt_plot_mr(endowment = .5)


## ---- fig.width=9, fig.height=9-----------------------------------------------
gt_sss(error = 0, periods = 2)


## ---- fig.width=7, fig.height=7-----------------------------------------------
goalie_payoff <- -matrix(c(0,1,.75,.25),2)
gt_bimatrix(goalie_payoff, -goalie_payoff,
            labels1=c("Left","Right"), P1="Goalie", P2="Shooter")


## ---- fig.width=6.5, fig.height=6.8-------------------------------------------
gt_brgraph(goalie_payoff, -goalie_payoff,
           labels1=c("Left", "Right"), P1="Goalie", P2="Shooter")


## ---- message=FALSE, warning = FALSE, results ='hide', include = FALSE--------
purl("endnotes.Rmd")

