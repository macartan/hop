
#' Calculate and plot chaotic cycles
#'
#' Plots chaotic majority rule cycles. We randomly select n voters and construct a sequence of k motions, each of which is majority preferred to the previous one in the sequence. Parameter z is used to scale the graph and a seed ("seed") is used for replication
#'
#' @param n Number of voters
#' @param k Number of motions
#' @param seed optionally set a seed
#' @param z size of plane for plotting xlim(-z,z)
#' @param a Alternative policy
#' @keywords Plott, Spatial, Chaos
#' @export
#' @examples
#' gt_cycles(3,5)
#'
#'



gt_cycles = function(
  n_voters = 3,
  n = n_voters,
  n_motions = 3,
  k = n_motions,
  seed = 1,
  z = 3,
	col1="red",  # color or path
  coli="black",   # color of ideal points
  colp = "red",   # color of policy proposals
  pointcex = .5,
	xlab = "Issue 1",
	ylab = "Issue 2",
	ann = TRUE,
	axes = TRUE,
	a2z = FALSE,
	a2zcol = "red",
  a2zw=2,
  a2zclose=.025,
	alab  = "",
	zlab  = "",
	inner = FALSE, colrange = rep(col1, k), col2=colrange[k],
	mar   = rep(2,4)
	){
  par(mar=mar)
  set.seed(seed)
	ideals = matrix(rnorm(2*n),2,n)		# Choose ideal points
	X      = m = c(mean(ideals[1,]), mean(ideals[2,]))	# Choose a centrist status quo
	V      = matrix(0,1,2)			# Choose a motion
	d      = function(a,b){diag(t((a-b))%*%(a-b))}	# Define distance
	aPb    = function(a,P,b){sum(d(a,P)<d(b,P))>n/2}	# Define Majority Preferences
	plot(t(ideals), xlim=c(-z, z),  ylim=c(-z, z), ann=ann, axes=axes, xlab=xlab, ylab=ylab)

	i=1				# Loop through motions
	while (i<=k){
	  points(X[1],X[2], col=colp, pch=19, cex = pointcex)	# Graph status quos
	  D = rnorm(2)*(1+d(X, m))			# Select random motions
	  if(aPb(X+D, ideals, X)*(d(X, m)<d(X+D, m)) | aPb(X+D, ideals, X)*inner)	# Focus on ex-centric motions if inner = FALSE
	    {V = D
	    if(i<k) segments(X[1],X[2],X[1]+D[1],X[2]+D[2], col=colrange[i])
	    X=X+D				# Define new status quo
	    i = i+1}}
	arrows(X[1]-V[1], X[2]-V[2], X[1]-a2zclose*V[1], X[2]-a2zclose*V[2], .12, col=col2)

	if(a2z) arrows(m[1],m[2],X[1]-a2zclose*(X[1]-m[1]),X[2]-a2zclose*(X[2]-m[2]), .12,col=a2zcol, lwd=a2zw)

		points(t(ideals), col=coli, pch=19) # Graph ideal points
  points(m[1], m[2], col = "red", pch=19)
  points(X[1], X[2], col = "red", pch=19)
  text(m[1]-0.1, m[2]-0.1, alab)
  text(X[1]-0.1, X[2]-0.1, zlab)

	}
