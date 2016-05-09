#' # Function for plotting utilities for  games in which two players can each choose  a strategy from a continuous action space.  Best responses function or correspondence are plotted and Nash equilibria identified.  sequence from s1 to s2
#'
#' @param u1 Player 1's utility
#' @param u2 Player 2's utility
#' @keywords Best response plot
#' @export
#' @examples
#'  gt_brcplot()
gt_brcplot = function(n.grid = 100,   # n.grid determines how accurate the graph is;
                     s1=0.01, s2=0.99,
                     a1=seq(s1,s2,length=n.grid),
                     a2=a1,                    # strategy range default is (0,1) (bounded away from 0 and 1)
                     u1=function(a1,a2){(a1-a2)^2},                              # payoff function for each profile of strategies
                     u2=function(a1, a2) {-u1(a1,a2)},
                     nash=TRUE, starfill="pink", tips=8, nashborder="black",	# Plot Nash
                     markbr = FALSE,
                     starfill2 = "red", # dominant strategy intersection color
                     radius=dist(range(a1))/30, border=NA,
                     tol=.001, # How close does close have to be to be called Nash?
                     type="surface",                                             # choose type of "surface" or "br" (for best response)
                     player= 1,    # 1 for 1, 2 for 2, 3 for both
                     cont=TRUE,
                     screen = list(z = -245, x = -75),                                 # choose the angle of 3d plot
                     clevels = 10,                                                   #choose number of contour levels desired
                     brtype = "l", brcex1=.5, brcex2=.4,
                     brpch = 21,                               #choose type of plot; text and symbol size, line width
                     brlwd = 1,
                     BRI = FALSE,   # Mark the best response equilibrium
                     pch1 = 21,
                     pch2 = 21,
                     col1="turquoise", col1br="blue", col1v=rgb(0,1,0,1),col2="grey",
                     col2br="red",
                     col2v=rgb(0.5,0,.5,1),
                     colbg1br = "white", colbg2br = "white",
                     canvascolor = "red",
                     xlab = expression(a[1]), ylab=expression(a[2]), zlab="U",
                    toptitle = paste("best response plot"),
                    surface_br_pointtype = "l",
                    at_axis = c(0, .5, 1)
                    ) {
  step= (s2-s1)/(n.grid-1)
  U1<-outer (a1,a2,u1)
  U2<-outer (a1,a2,u2)
  br.int <- FALSE

  # Player 1 Best responses
  brmax1=a1[max.col(t(U1), ties.method = "last")] # max value of best response correspondence given a2
  brmin1=a1[max.col(t(U1), ties.method = "first")] # min value of best response correspondence given a2
  br1=cbind(brmax1,brmin1)  # max min value matrix

  brm1=matrix(NA,length(a1),length(a1)) # best response matrix
  for (i in 1: length(a1)){
    brm1[i,]=c( a1[a1 <= br1[i,1] & a1>=br1[i,2]] , rep("NA",length(a1)-((br1[i,1]-br1[i,2])/step+1)))

  }
  brv1       <- as.vector(t(brm1))     # reshape the matrix to vector
  bri2       <- rep(a2,each=length(a2))
  best.resp1 <- cbind(brv1,bri2) # best response correspondence for player 1
  # Player 1 dominant strategy
  if (max(brmin1)==min(brmax1)) v1 <- max(brmin1) # weakly dominate strategy

  # Player 2 Best responses
  brmax2=a2[max.col(U2, ties.method = "last")] # max value of best response correspondence given a2
  brmin2=a2[max.col(U2, ties.method = "first")] # min value of best response correspondence given a2
  br2=cbind(brmax2,brmin2)  # max min value matrix
  brm2=matrix(NA,length(a2),length(a2)) # best response matrix
  for (i in 1: length(a2)){
    brm2[i,]=c(a2[a2<=br2[i,1]&a2>=br2[i,2]],rep("NA",length(a2)-((br2[i,1]-br2[i,2])/step+1)))
  }
  brv2=as.vector(t(brm2))     # reshape the matrix to vector
  bri1=rep(a1,each=length(a1))
  best.resp2<-cbind(bri1,brv2) # best response correspondence for player 2
  # Player 2 dominant strategy
  if (max(brmin2)==min(brmax2)) v2=max(brmin2) # weakly dominate strategy


  if (sum(abs(brmax1-brmin1)) == 0 & sum(abs(brmax2-brmin2)) == 0)
                {best.resp1 <- cbind(a1[max.col(t(U1), ties.method = "first")],a2)
                 best.resp2 <- cbind(a1,a2[max.col(U2, ties.method = "first")])
                 br.int     <- rowSums((best.resp1 - best.resp2)^2)<tol	}

  # intersection of best response functions
  U = c(as.vector(U1), as.vector(U2))
  i = c(rep(0, n.grid^2), rep(1, n.grid^2))


  if(type=="br" ){

  if(cont){
    if(player ==1){
       contour(a1,a2,U1, xlab=xlab, ylab=ylab,col=col1, nlevels=clevels,main=toptitle, axes = FALSE)
       axis(1, at = at_axis)
       axis(2, at = at_axis)
       lines(best.resp1,col=col1br, type=surface_br_pointtype,  cex=brcex1, lwd=brlwd)
       if (max(brmin1)==min(brmax1)) abline(v = v1, lwd=brlwd, col=col1v)
       box()
      }

    if(player ==2){
      contour(a1,a2,U2, xlab=xlab, ylab=ylab,col=col2, nlevels=clevels,main=toptitle, axes = FALSE)
      axis(1, at = at_axis)
      axis(2, at = at_axis)
      lines(best.resp2,col=col2br, type=surface_br_pointtype,  cex=brcex2, lwd=brlwd)
      if (max(brmin2)==min(brmax2))   abline(v2, 0, lwd=brlwd, col=col2v)
      box()
    }

    if(player ==3){
      contour(a1,a2,U1, xlab=xlab, ylab=ylab,col=col1, nlevels=clevels,main=toptitle, axes = FALSE)
      contour(a1,a2,U2, nlevels=clevels, add = TRUE, col = col2)
      axis(1, at = at_axis)
      axis(2, at = at_axis)
      lines(best.resp1,col=col1br, type=surface_br_pointtype,  cex=brcex1, lwd=brlwd)
      lines(best.resp2,col=col2br, type=surface_br_pointtype, cex=brcex2, lwd=brlwd)
      if (max(brmin1)==min(brmax1)) abline(v = v1,lwd=brlwd, col=col1v)
      if (max(brmin2)==min(brmax2))   abline(v2, 0, lwd=brlwd, col=col2v)
      if(max(brmin1)==min(brmax1)&max(brmin2)==min(brmax2) & BRI)
        {symbols(v1, v2, circles=s2/40, add=TRUE, inches=FALSE, bg="gold")}
     else{ if(nash) {if(sum(br.int)>0){
        gt_star(best.resp1[br.int],
                best.resp2[br.int],
                rad=radius,
                phi=0, starfill=starfill, tips=tips, outer=border)
       }}
       }
      box()
    }
  }

  else{
    plot(a1, a2, type="n", ylim=c(0,s2), xlim=c(0,s2), xlab=xlab, ylab=ylab,main=toptitle)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = canvascolor)

    if(player ==1){

      lines(best.resp1,col=col1br, bg = colbg1br, type="p", cex=brcex1, lwd=brlwd, pch=pch1)
    if (max(brmin1)==min(brmax1)) abline(v=v1,  lwd=2, col=col1v)
  }

       if(player ==2){
         lines(best.resp2,col=col2br, bg = colbg2br, type="p", cex=brcex2, lwd=brlwd, pch=pch2)
      if (max(brmin2)==min(brmax2))   abline(v2, 0, lwd=2, col=col2v)
       }


       if(player ==3){
         lines(best.resp1,col=col1br, bg = colbg1br, type="p", cex=brcex1, lwd=brlwd, pch=pch1)
         lines(best.resp2,col=col2br, bg = colbg2br, type="p", cex=brcex2, lwd=brlwd, pch=pch2)

         if (max(brmin1)==min(brmax1)) abline(v = v1,lwd=2, col=col1v)
         if (max(brmin2)==min(brmax2))   abline(v2,0,  lwd=2, col=col2v)
         if(markbr & max(brmin1)==min(brmax1)&max(brmin2)==min(brmax2)) {
           #symbols(v2, v1, circles=s2/40, add=TRUE, inches=FALSE, bg="gold")
         gt_star(v2,v1, rad=radius, phi=0, starfill=starfill2, tips=tips, outer=border)
           } else{ if (nash & sum(br.int)>0)
           gt_star(best.resp1[br.int], best.resp2[br.int], rad=radius, phi=0, starfill=starfill, tips=tips, outer=border)
         }
       }
  }

  }



  if(type=="surface"){
    u <- function(a1,a2,i){(i==1)*u1(a1,a2) + (i==2)*u2(a1,a2)}
    D=    expand.grid(a1=a1, a2=a2, i=c(1,2))
    U <- u(D$a1,D$a2,D$i)
    trellis.par.set("axis.line",list(col="transparent"))
    wireframe(U~D$a1*D$a2, groups=D$i, screen = screen,
              scales = list(arrows=FALSE, cex= .45, col = "black", font = 3),
              xlab=xlab, ylab=ylab, zlab=zlab)
  }
}


# #########Second price auction######
# pdf(file="pages/_SPSB.pdf", width=6, height=10)

# u1=function(a1,a2){(200-a2)*(a1>a2)+(1/2)*(200-a2)*(a1==a2)+0*(a1<a2)}
# u2=function(a1,a2){(400-a1)*(a2>a1)+(1/2)*(400-a1)*(a1==a2)+0*(a2<a1)}
# par(mfrow=c(3,2))
# L= matrix(c(1,2,3,3, 3,3),byrow=TRUE, ncol=2)
# layout(L)

# gt.brcplot(n.grid = 51, u1=u1, u2=u2, s1=0,s2=1000, type="br", cont=FALSE,
           # col1br=col5t,col2br=col0t,col1v=col5, col2v=col0,
           # xlab=paste("Dee's bid"), ylab=paste("Dum's response"),
           # toptitle=paste("Dum's responses to Dee's bids"),player=1, brcex=.5)
# gt.brcplot(n.grid = 51, u1=u1, u2=u2, s1=0,s2=1000, type="br", cont=FALSE,
           # col1br=col5t,col2br=col0t,col1v=col5, col2v=col0,
           # xlab=paste("Dee's response"), ylab=paste("Dum's bid"),
           # toptitle=paste("Dee's responses to Dum's bids"),player=2, brcex=.5)
# gt.brcplot(n.grid = 101, u1=u1, u2=u2, s1=0,s2=1000, type="br", cont=FALSE,
           # col1br=col5t,col2br=col0t,col1v=col5, col2v=col0,
           # xlab=paste("Dee's bid"), ylab=paste("Dum's bid"),
           # toptitle=paste("Equilibriums"),player=3, brcex=.8)
# dev.off()


####### redistributive politics######


# z=0
# u1=function(a1,a2){.5+.3*a1/(z+a1+a2)+.15*(1-a1)/(z+2-a1-a2)}
# u2=function(a1,a2){.5+.3*a2/(z+a1+a2)+.15*(1-a2)/(z+2-a1-a2)}

# pdf(file="DL1.pdf", width=7, height=7.5)
# gt.brcplot(n.grid = 40, u1=u1, u2=u2, s1=.1,s2=.9, type="surface", screen=list(z = 36, x = -60))
# dev.off()

# pdf(file="DL2.pdf", width=7, height=7.5)
# gt.brcplot(n.grid = 400, u1=u1, u2=u2, type="br", player=3,brtype="l", clevels=20, brlwd=2,
          # tol=.0000005, col1br = col0, col2br = col5, col1=col5t, starfill=grey(.7))

# dev.off()
