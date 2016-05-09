
#' Clarke-Groves Mechanism. Utility for public good is of the form u(x | theta_i) = theta_i*x^.5 - t_i. Default cost equals amount produced, say.  Consider two games. In one each is charged x/2. In the other VCG transfers are used.  Games solved as normal form games (as if in fact players know the other's preferences, even if mechanism does not)
#'
#' @param theta1 Player 1's valuation
#' @param theta2 Player 2's valuation
#' @keywords Clarke Groves Mechanism, Mechanism Design
#' @export
#' @examples
#'  gt_cgm()

gt_cgm = function(
  select  = function(i,j) (i+j)^2/4,       # Social optimum of production given two valuations
  theta1  = .6, # Jill x axis ; RED
  theta2  = .3, # Jack y axis ; BLACK
  normalize = TRUE,   # Normalize utilities so optimal CG utility is normalizevalue
  normalizevalue = 100,  # Normalize utilities so optimal CG utility is normalizevalue
  u       = function(x, theta) theta*(x^.5),
  cost    = function(x) x,
  declarations1 = c(0,.3,.6,1),
  declarations2 = declarations1,
  ucex    = 1.5,
  h       = function(j) -(j^2/2),
  scale = 1000,
  maincex = 2,
  col.main = "red",
  mainline = -1.7,
  n.grid = 500,
  clevels = 4,
  at_axis = c(0, .3, .6, 1),
  addpoint = FALSE,  #  add a marker for the ideals,
  gcnash = TRUE,
  hack = FALSE  # This is a hack for the specific CG illustration; should not be needed
){

  U = function(i,j, theta) u( select(i, j), theta)*scale
  V = function(i,j) u( select(i, j), 1)*scale              ## Total value of good produced
  C = function(i,j) cost(select(i,j))*scale
  transfer = function(i,j) -(
    cost(select(i,j)) -       # Full cost of optimal production
      u(select(i,j), j) -     # Less gains to others at optimum
      h(j)                    # also tax function not depending on i that splits costs and optimizes others welfare
  )*scale                       # this maximizes: j x^.5 - x/2 to select x = j^2


  par(mfrow = c(2,2))

  ## PRODUCTION AND COSTS
  mar = rep(4,4)
  gt_brcplot(n.grid=n.grid,
             u1 = V, type="br",
             toptitle = "Optimal Public Goods Production",
             surface_br_pointtype = "n",
             clevels = clevels,
             col1 = "black",
             col1v = NULL, col2v = NULL,
             ylab = "If this was Jack's true valuation",
             xlab = "If this was Jill's true valuation"
  )  # what gets selected

  if(addpoint) points(theta1,theta2, col = "red", pch = 21)



  ## INDIVIDUAL  COSTS
  gt_brcplot(n.grid=n.grid,                 #  split cost
             u1 = function(a1,a2) C(a1,a2)/2,
             u2 = function(a1,a2) -transfer(a1,a2),
             type="br", toptitle = "Costs",
             surface_br_pointtype = "n",
             clevels = clevels,
             col1 = "black", col2 = "red",
             col1v = NULL, col2v = NULL,
             at_axis = at_axis,
             ylab = "If this was Jack's declared valuation",
             xlab = "If this was Jill's declared valuation",
             player = 3, nash = FALSE
  )
  if(addpoint) points(theta1,theta2, col = "red", pch = 21)

  if(hack){
    text(.67, .66, "Divide costs in half", col = "black", srt = -45)
    gt_slope_text("Clarke-Groves transfers", function(x) (.85-(x*(1.05))^2)^.5, xl=0.3, xh=.8, col = "red", vshift = -.08)
  }


  #  Game
  gt_brcplot(n.grid=n.grid,                 # utility from split cost
             u1 = function(a1,a2) U(a1,a2, theta1)-C(a1,a2)/2 - normalize*(-normalizevalue+U(theta1,theta2, theta1)+transfer(theta1,theta2)),
             u2 = function(a1,a2) U(a1,a2, theta2)-C(a1,a2)/2 - normalize*(-normalizevalue+U(theta1,theta2, theta2)+transfer(theta1,theta2)),
             type="br",
             player =3 , toptitle = "Game Induced by Dividing Costs in Half", clevels = clevels,
             at_axis = at_axis,
             ylab = "If truth was 0.3 but Jack declared this",
             xlab = "If truth was 0.6 but Jill declared this",
             col1 = "pink",
             col1br = "red",
             col1v = "red",
             col2 = "grey",
             col2br = "black",
             col2v = "black",
             nash = TRUE

  )

  if(addpoint) points(theta1,theta2, col = "red", pch = 21)
  if(hack  & gcnash) gt_star(1, 0, rad=.08, phi=0, starfill="red", tips=8, outer=TRUE, starborderlwd=1)
  if(hack){
    text(.62, .62, "Jill's best response", col = "red", srt = -45)
    text(.32, .33, "Jack's best response", col = "black", srt = -45)}

  gt_brcplot(n.grid=n.grid,                 # utility from transfer
             u1 = function(a1,a2) U(a1,a2, theta1)+transfer(a1,a2) - normalize*(-normalizevalue+U(theta1,theta2, theta1)+transfer(theta1,theta2)),
             u2 = function(a1,a2) U(a1,a2, theta2)+transfer(a2,a1) - normalize*(-normalizevalue+U(theta1,theta2, theta2)+transfer(theta2,theta1)),
             type="br",
             player =3 , toptitle = "Game Induced by Clarke-Groves Transfers", clevels = clevels,
             at_axis = at_axis,
             ylab = "If truth was 0.3 but Jack declared this",
             xlab = "If truth was 0.6 but Jill declared this",
             col1 = "pink",
             col1br = "red",
             col1v = "red",
             col2 = "grey",
             col2br = "black",
             col2v = "black",
             nash = TRUE
  )

  if(addpoint) points(theta1,theta2, col = "red", pch = 21)
  if(gcnash) gt_star(theta1, theta2, rad=.08, phi=0, starfill="red", tips=8, outer=TRUE, starborderlwd =1)
}

