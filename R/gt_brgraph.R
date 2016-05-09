######################################################
#	Best response graph for 2 person game	         #




#' Plot best reactions for normal form games
#'
#' This function plots best reactions. Intersections are Nash equilibria. Thanks to Neelan Sircar for developing this code.
#' @param R Row payoffs
#' @param C Column payoffs
#' @keywords Payoff matrix, Nash, Best responses
#' @export
#' @examples
#' C <- matrix(c(1,0,0,1),2,2)
#' R <- matrix(c(1,0,0,1),2,2)
#' brgraph(R, C, P1="Country R", P2="Country C", leglocBR=c(.8,.2))


gt_brgraph <- function(
        R, C=t(R),
        PR="Row player",
        PC="Column Player",
        labelsR = c("C", "D"),
        labelsC=labelsR,
				legBR=TRUE, # Legend for the best response graph
				legr=TRUE, legc=TRUE, leglocBR=c(.8,.2),
				legxyr=c(c(.8, 3*(min(C)+max(C))/4)),
				legxyc=c(c(.8, (min(C)+max(C))/4)),
				ulegend = TRUE, # legend for the utilies graph
				BR=TRUE,
				draw1=TRUE,
				draw2=TRUE,
				vert=TRUE,
				util=FALSE,
				Rloc=NULL,
				Cloc=NULL,
				ur=TRUE,  # Graph ROWS's utility given range of COLUMN actions (for each  of row's two actions)
				uc=TRUE, # Graph COLUMN's utility given range of column actions (for each  of row's two actions)
				cthighlight=FALSE, rthighlight=FALSE,  # Highlight minimax / maximin 1
				cbhighlight=FALSE, rbhighlight=FALSE,   # Highlight minimax / maximin 2
				mmcol="turquoise",
				xlabu=paste("Probability on", labelsC[1], "for", PC),
				ylabu=paste("Utility for", PR),
				legtextr=c(paste("u(",labelsR[1], ")", sep=""),paste("u(",labelsR[2], ")", sep="")),
				legtextc=c(paste("u(",labelsC[2], ")", sep=""),paste("u(",labelsC[1], ")", sep="")),
				mainbr = "", mainu = "",
				colown="red", coloth= gray(.4),
				hl_lwd = 8,
				lwd = 2) {

if (BR & util){ if(vert) par(mfrow=c(2,1), mar=c(2, 4, 2, 2) + 0.1, pty="s")
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
if(BR){ plot(1,1, xlim=c(0,1), ylim=c(0,1), xlab=paste("Probability on", labelsC[1], "for", PC), ylab= paste("Probability on", labelsR[1], "for", PR), cex.lab=1-.3*(BR & util), cex.axis=1-.3*(BR & util), type="n", main=mainbr)
	if(draw1){points(c(0,pC,pC,1), c(R_pl,R_pl,R_ph,R_ph), type="l", lwd=5, col=gray(.3))}
	if(draw2){points(c(C_pl,C_pl,C_ph,C_ph),c(0,pR,pR,1), type="l", lwd=5, col=gray(.3), lty=3)}
	if (draw1 & draw2) {text(Rloc[1], Cloc[1], paste("Prob ", PR, " plays ", labelsR[1], "=", pR,sep=""), cex=1-.3*(BR & util & vert));
						text(Rloc[2], Cloc[2], paste("Prob ", PC, " plays ", labelsC[1], "=", pC,sep=""), cex=1-.3*(BR & util & vert))}
	if(legBR) legend(leglocBR[1], leglocBR[2], c(PR ,PC),
	                 lty=c(1,3), lwd=5, col=gray(.3), xjust=.5, yjust=.5, cex=1-.3*(BR & util))}

# Graph utilities

	if(util){ plot(0,0, xlim=c(0,1), ylim=c(min(C,R),max(C,R)), xlab=xlabu, ylab=ylabu,
	               cex.lab=1-.3*(BR & util), cex.axis=1-.3*(BR & util), type="n", main=mainu)

	if(ur) {
			if(rthighlight) {x = (0:100)/100; points(x, sapply(x, function(i) max(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
			                                         type ="l", lwd=8, col=mmcol)}
			if(rbhighlight) {x = (0:100)/100; points(x, sapply(x, function(i) min(R[2,1]+i*(R[1,1]-R[2,1]), R[2,2]+i*(R[1,2]-R[2,2]))),
			                                         type ="l", lwd=8, col=mmcol)}
			points(c(0,1), c(R[2,1],R[1,1]), type ="l",
			       lwd=lwd, col=colown)        # row's utility from COLUMN taking first action given range of row actions on horizontal
			points(c(0,1), c(R[2,2],R[1,2]), type ="l",
			       lwd=lwd, col=colown, lty=3) # row's utility from COLUMN taking second action given range of row actions on horizontal
			if(ulegend) if(legr & !uc) { legend(legxyr[1], legxyr[2], legtextr, lty=c(1,3),
			                                    lwd=5, col=c(colown, colown), xjust=.5, yjust=.5,
			                                    cex=1-.3*(BR & util))	}
			}
	if(uc) {if(cthighlight) {
  	    x = (0:100)/100;
  	    points(x, sapply(x, function(i) max(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))), type ="l", lwd=8, col=mmcol)}
			if(cbhighlight) {x <- (0:100)/100
			                points(x, sapply(x, function(i) min(C[2,1]+i*(C[1,1]-C[2,1]), C[2,2]+i*(C[1,2]-C[2,2]))),
			                       type ="l", lwd=hl_lwd, col=mmcol)
			                }
			points(c(0,1), c(C[2,1],C[1,1]), type ="l",
			       lwd=lwd, col=coloth)         # column's utility from COLUMN taking first  action given range of row actions on horizontal
			points(c(0,1), c(C[2,2],C[1,2]), type ="l",
			       lwd=lwd, col=coloth, lty=3)  # column's utility from COLUMN taking second action given range of row actions on horizontal
			if(ulegend) if(legc & !ur) { legend(legxyc[1], legxyc[2], legtextc, lty=c(1,3), lwd=5, col=c(coloth, coloth), xjust=.5, yjust=.5, cex=1-.3*(BR & util))	}
			}
	  if(ulegend) if(ur&uc&legr&legc) {
	    legend(legxyr[1], legxyr[2], c(legtextr, legtextc), lty=c(1,3), lwd=5,
	           col=c(colown, colown, coloth,coloth ), xjust=.5, yjust=.5, cex=1-.3*(BR & util))
	    }

	}
}

