
#' Plot best reactions for two person normal form games
#'
#' This function plots best reactions. Intersections are Nash equilibria. Thanks to Neelan Sircar for developing much of this code.
#' To show utilities for players use \code{util = TRUE}. To show best response functions use \code{BR = TRUE}. Both can be done also
#' with output either aligned horizontally or vertically, using \code{vert}.
#' @param R Row payoffs
#' @param C Column payoffs
#' @param PR Name of row player
#' @param PC Name of column player
#' @param P1 alternative argument for Row Name
#' @param P2 alternative argument for Col Name
#' @param overrow Switch to have utilty plots plotted over row player (player 1)
#' @keywords Payoff matrix, Nash, Best responses
#' @export
#' @examples
#' C <- matrix(c(1,0,0,1),2,2)
#' R <- matrix(c(1,0,0,1),2,2)
#' gt_brgraph(R, C, PR="Country R", PC="Country C", leglocBR=c(.8,.2))
#' # A chicken game:
#'
#' gt_brgraph(Chicken, BR = FALSE, util = TRUE,
#'   cthighlight = TRUE, rthighlight = TRUE,
#'   labelsC = c("Jill A", "Jill B"),
#'   labelsC = c("Jack A", "Jack B"),
#'   ur = TRUE,
#'   PC = "Jill", PR = "Jack")
gt_brgraph <- function(
        X,
        Y=t(X),
        players = c("Row", "Col"),
        PR=players[1],
        P1=PR,
        PC=players[2],
        P2 = PC,
        labelsR = c("C", "D"),
        labels1 = labelsR,
        labelsC=labels1,
        labels2 = labelsC,
				legBR=TRUE, # Legend for the best response graph
				legr=TRUE,
				legc=TRUE,
				leglocBR=c(.8,.2),
				legxyr=c(c(.8, 3*(min(C)+max(C))/4)),
				legxyc=c(c(.8, (min(C)+max(C))/4)),
				ulegend = TRUE, # legend for the utilies graph
				BR=TRUE,
				br = BR,
				draw1=TRUE,
				draw2=TRUE,
				vert=TRUE,
				util=FALSE,
				overrow  = TRUE,
				Rloc=NULL,
				Cloc=NULL,
				ur=TRUE,  # Graph ROWS's utility given range of COLUMN actions   (for each  of row's two actions)
				u1 = ur,
				uc=TRUE,  # Graph COLUMN's utility given range of column actions (for each  of row's two actions),
				u2 = uc,
				cthighlight=FALSE, rthighlight=FALSE,   # Highlight minimax / maximin 1
				cbhighlight=FALSE, rbhighlight=FALSE,   # Highlight minimax / maximin 2

				colr = "red",     # P1 / Row color
				colc = gray(.4), # P2 / Col color

				col1 = colr,     # Column color
				col2 = colc,     # Row color

				mmcol_r="pink",
				mmcol_c=grey(.8),
				mmcol_1=mmcol_r,
				mmcol_2=mmcol_c,

				xlabu=paste("Probability on", labelsR[1], "for", PR),
				ylabu= NULL,
				legtextr=c(paste(PR, "'s utility | ", PC, " plays ", labelsC[1], ")", sep=""),
				           paste(PR, "'s utility | ", PC, " plays ", labelsC[2], ")", sep="")),
				legtextc=c(paste(PC, "'s utility | ", PC, " plays ", labelsC[1], ")", sep=""),
				           paste(PC, "'s utility | ", PC, " plays ", labelsC[2], ")", sep="")),
				mainbr = "", mainu = "",
				hl_lwd = 8,
				lwd = 2) {

# Allow naming by row or by column
  R = X
  C = Y
  PR = P1
  PC = P2
  ur = u1
  uc = u2
  colr <- col1
  colc <- col2
  mmcol_r <- mmcol_1
  mmcol_c <- mmcol_2
  labelsR <- labels1
  labelsC <- labels2
  BR = br
if (BR & util){ if(vert) par(mfrow=c(2,1), mar=c(2, 4, 2, 2) + 0.1)#, pty="s")
				else par(mfrow=c(1,2), pty="s") }

probC <- round(((R[2,2] - R[1,2])/(R[1,1]+R[2,2]-R[2,1]-R[1,2])),2);
probR <- round(((C[2,2] - C[2,1])/(C[1,1]+C[2,2]-C[2,1]-C[1,2])),2)

pR <- ifelse(probR<0|probR>1,as.numeric(probR<0),probR)
pC <- ifelse(probC<0|probC>1,as.numeric(probC<0),probC)

if(is.null(Rloc)) Rloc <-  (rep(pR+.075,2))*(pR<=.5)+(rep(pR-.075,2))*(pR>.5)
if(is.null(Cloc)) Cloc <- (c(pC-.075,pC-.125- .035*(BR & util & vert)))*(pC>=.5)+(c(pC+.125, pC+.075))*(pC<.5)

# Best responses to low and high probabilities on strategy 1
R_pl <-  as.numeric(R[2,1]>R[1,1]); C_pl <- as.numeric(C[1,2]> C[1,1])
R_ph <-  as.numeric(R[2,2]>R[1,2]); C_ph <- as.numeric(C[2,2]> C[2,1])

# Graph best responses
if(BR){ plot(1,1, xlim=c(0,1), ylim=c(0,1),
             xlab=paste("Probability on", labelsC[1], "for", PC),
             ylab= paste("Probability on", labelsR[1], "for", PR),
             cex.lab=1-.3*(BR & util), cex.axis=1-.3*(BR & util), type="n", main=mainbr)
	if(draw1){points(c(0,pC,pC,1), c(R_pl,R_pl,R_ph,R_ph), type="l", lwd=5, col=gray(.3))}
	if(draw2){points(c(C_pl,C_pl,C_ph,C_ph),c(0,pR,pR,1), type="l", lwd=5, col=gray(.3), lty=3)}
	if (draw1 & draw2) {gt_shadowtext(Rloc[1], Cloc[1],
	                         paste("Prob ", PR, " plays ", labelsR[1], "=", pR,sep=""),
	                         cex=1-.3*(BR & util & vert));
	  gt_shadowtext(Rloc[2], Cloc[2], paste("Prob ", PC, " plays ", labelsC[1], "=", pC,sep=""), cex=1-.3*(BR & util & vert))}
	if(legBR) legend(leglocBR[1], leglocBR[2], c(PR ,PC),
	                 lty=c(1,3), lwd=5, col=gray(.3), xjust=.5, yjust=.5, cex=1-.3*(BR & util))}

# Graph utilities

	if(util & overrow){
	  # Default is to graph with row player strategies on X.
	  # overrow redefines everything to switch around

	  if(is.null(ylabu) & !uc) ylabu <- (paste("Utility for", PR))
	  if(is.null(ylabu) & !ur) ylabu <- (paste("Utility for", PC))
	  if(is.null(ylabu) & ur & uc) ylabu <- paste("Utility for", PC, "and ", PR)

	  plot(0,0, xlim=c(0,1), ylim=c(min(C,R),max(C,R)), xlab=xlabu, ylab=ylabu,
	               cex.lab=1-.3*(BR & util), cex.axis=1-.3*(BR & util), type="n", main=mainu)

	if(ur) {
			if(rthighlight) {x = (0:100)/100; points(x, sapply(x, function(i) max(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
			                                         type ="l", lwd=8, col=mmcol_r)}
			if(rbhighlight) {x = (0:100)/100; points(x, sapply(x, function(i) min(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
			                                         type ="l", lwd=8, col=mmcol_r)}
			points(c(0,1), c(R[2,1],R[1,1]), type ="l",
			       lwd=lwd, col=colr)        # row's utility from COLUMN taking first action given range of row actions on horizontal
			points(c(0,1), c(R[2,2],R[1,2]), type ="l",
			       lwd=lwd, col=colr, lty=3) # row's utility from COLUMN taking second action given range of row actions on horizontal
			if(ulegend) if(legr & !uc) { legend(legxyr[1], legxyr[2], legtextr, lty=c(1,3),
			                                    lwd=5, col=c(colr, colr), xjust=.5, yjust=.5,
			                                    cex=1-.3*(BR & util))	}
			}
	if(uc) {if(cthighlight) {
          	    x = (0:100)/100;
          	    points(x, sapply(x, function(i) max(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))),
        	           type ="l", lwd=8, col=mmcol_c)}
    			if(cbhighlight) {x <- (0:100)/100
			                points(x, sapply(x, function(i) min(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))),
			                       type ="l", lwd=hl_lwd, col=mmcol_c)
			                }
			points(c(0,1), c(C[2,1],C[1,1]), type ="l",
			       lwd=lwd, col=colc)         # column's utility from COLUMN taking first  action given range of row actions on horizontal
			points(c(0,1), c(C[2,2],C[1,2]), type ="l",
			       lwd=lwd, col=colc, lty=3)  # column's utility from COLUMN taking second action given range of row actions on horizontal
			if(ulegend) if(legc & !ur) { legend(
			                              legxyc[1],
			                              legxyc[2],
			                              legtextc,
			                              lty=c(1,3),
			                              lwd=5,
			                              col=c(colc, colc),
			                              xjust=.5,
			                              yjust=.5,
			                              cex=1-.3*(BR & util))	}
			}
	  if(ulegend) if(ur&uc&legr&legc) {
	    legend(legxyr[1], legxyr[2], c(legtextr, legtextc), lty=c(1,3), lwd=5,
	           col=c(colr, colr, colc,colc ), xjust=.5, yjust=.5, cex=1-.3*(BR & util))
	    }

	}



if(util & !overrow){
  # Default is to graph with row player strategies on X.
  # overrow redefines everything to switch around
  # Switch payoff matrix and labels
  R <- t(Y)
  C <- t(X)
  PR <- P2
  PC <- P1
  colr <- col2
  colc <- col1
  mmcol_r <- mmcol_2
  mmcol_c <- mmcol_1

  labelsR <- labels2
  labelsC <- labels1


  if(is.null(ylabu) & !uc) ylabu <- (paste("Utility for", PR))
  if(is.null(ylabu) & !ur) ylabu <- (paste("Utility for", PC))
  if(is.null(ylabu) & ur & uc) ylabu <- paste("Utility for", PC, "and ", PR)

  plot(0,0, xlim=c(0,1), ylim=c(min(C,R),max(C,R)), xlab=xlabu, ylab=ylabu,
       cex.lab=1-.3*(BR & util), cex.axis=1-.3*(BR & util), type="n", main=mainu)

  if(ur) {
    if(rthighlight) {x = (0:100)/100; points(x, sapply(x, function(i) max(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
                                             type ="l", lwd=8, col=mmcol_r)}
    if(rbhighlight) {x = (0:100)/100; points(x, sapply(x, function(i) min(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
                                             type ="l", lwd=8, col=mmcol_r)}
    points(c(0,1), c(R[2,1],R[1,1]), type ="l",
           lwd=lwd, col=colr)        # row's utility from COLUMN taking first action given range of row actions on horizontal
    points(c(0,1), c(R[2,2],R[1,2]), type ="l",
           lwd=lwd, col=colr, lty=3) # row's utility from COLUMN taking second action given range of row actions on horizontal
    if(ulegend) if(legr & !uc) { legend(legxyr[1], legxyr[2], legtextr, lty=c(1,3),
                                        lwd=5, col=c(colr, colr), xjust=.5, yjust=.5,
                                        cex=1-.3*(BR & util))	}
  }
  if(uc) {if(cthighlight) {
    x = (0:100)/100;
    points(x, sapply(x, function(i) max(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))),
           type ="l", lwd=8, col=mmcol_c)}
    if(cbhighlight) {x <- (0:100)/100
    points(x, sapply(x, function(i) min(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))),
           type ="l", lwd=hl_lwd, col=mmcol_c)
    }
    points(c(0,1), c(C[2,1],C[1,1]), type ="l",
           lwd=lwd, col=colc)         # column's utility from COLUMN taking first  action given range of row actions on horizontal
    points(c(0,1), c(C[2,2],C[1,2]), type ="l",
           lwd=lwd, col=colc, lty=3)  # column's utility from COLUMN taking second action given range of row actions on horizontal
    if(ulegend) if(legc & !ur) { legend(
      legxyc[1],
      legxyc[2],
      legtextc,
      lty=c(1,3),
      lwd=5,
      col=c(colc, colc),
      xjust=.5,
      yjust=.5,
      cex=1-.3*(BR & util))	}
  }
  if(ulegend) if(ur&uc&legr&legc) {
    legend(legxyr[1], legxyr[2], c(legtextr, legtextc), lty=c(1,3), lwd=5,
           col=c(colr, colr, colc,colc ), xjust=.5, yjust=.5, cex=1-.3*(BR & util))
  }

}



}

