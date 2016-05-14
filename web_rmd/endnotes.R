## ----include=FALSE, cache=FALSE------------------------------------------
rm(list=ls(all=TRUE))
library(knitr)

## ----eval=FALSE----------------------------------------------------------
## devtools::install_github("macartan/hop")

## ------------------------------------------------------------------------
library(hop)

## ------------------------------------------------------------------------
payoffs <-  matrix(c(2,3,0,1),2)
gt_bimatrix(payoffs, labels1=c("C","D"))
gt_bimatrix(payoffs, labels1=c("C","D"), nash=FALSE, arrow1=FALSE)

## ------------------------------------------------------------------------
payoffs <- matrix(c(2,3,1,0),2)
gt_bimatrix(payoffs, labels1=c("C","D"))

## ------------------------------------------------------------------------
payoffs <- matrix(c(3,2,0,1),2)
gt_bimatrix(payoffs, labels1=c("C","D"))

## ------------------------------------------------------------------------
payoffs <- matrix(c(2,3,0,1),2)
gt_folk(payoffs)

## ------------------------------------------------------------------------
payoffs <- matrix(c(3,2,0,1),2)
gt_folk(payoffs)

## ------------------------------------------------------------------------
gt_jury(n=7, base=.6)

## ------------------------------------------------------------------------
gt_plot_banzhaf(weights = c(1, 1, 3,7,9,9), q_rule = .5)
gt_plot_banzhaf(weights = c(1, 1, 3,7,9,10), q_rule = .6)

## ------------------------------------------------------------------------
ideals <- matrix(c(0, 0, .5, 1, 1, 0), 2)
gt_majority_phase(ideals)

## ------------------------------------------------------------------------
ideals <- matrix(c(.2, .2, .5, .9, 1, 0), 2)
gt_majority_phase(ideals, raylengths=c(.5, .2))

## ------------------------------------------------------------------------
gt_cycles(n_voters = 3, n_motions = 25)

## ------------------------------------------------------------------------
probabilities <- c(.8,.2,0)
gt_leg_barg(probabilities)

## ------------------------------------------------------------------------
x <- c(1,1,1,1,1,1,0,0,0)
D <- replicate(50000, gt_declarations(sample(x))$Declarations)
mean(D[9,])

## ---- fig.width=9, fig.height=3.5----------------------------------------
gt_nbs(u1 = function(x) x^.5, u2 = function (x) 1 - x)

## ------------------------------------------------------------------------
gt_nbs(solution_only = TRUE)

## ------------------------------------------------------------------------
payoffs <- matrix(c(2,3,0,1),2)
gt_coase(X=payoffs, bargain = TRUE, SQ = "minimax")
gt_coase(f=function(x) 1-x^2, bargain = TRUE, SQ=c(0,1))

## ---- fig.width=7, fig.height=8------------------------------------------
gt_cgm (theta1 = .2, theta2 = .4)

## ------------------------------------------------------------------------
gt_plot_mr(e = .5, main = "Endowment = 0.5")

## ---- fig.width=7, fig.height=7------------------------------------------
gt_sss(error = 0, periods = 2)

## ---- fig.width=7, fig.height=7------------------------------------------
XC <- matrix(c(0,1,.75,.25),2)
gt_bimatrix(-XC, XC, srt1=90,
            labels1=c("Left","Right"), P1="Goalie", P2="Shooter")

## ---- fig.width=6.5, fig.height=6.8--------------------------------------
gt_brgraph(-XC, XC,
           labelsR=c("Left", "Right"), P1="Goalie", P2="Shooter")

## ---- message=FALSE, warning = FALSE, results ='hide'--------------------
purl("endnotes.Rmd")

