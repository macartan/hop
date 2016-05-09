
#' Majority Preference Relation. Let P be a d*n vecotr of  ideal points for n individuals in d dimensions a and b are d*1 policy vectors gt_maj_pref(P,a,b) reports whether a strict majority of individuals strictly prefer a to b
#'
#' @param method Distance metric, deafulats to Euclidean
#' @param P matrix of ideal points
#' @param b Status quo policy
#' @param a Alternative policy
#' @keywords Plott, Spatial, Chaos
#' @export
#' @examples
#' gt_maj_pref(matrix(c(0, 0, .5, 1, 1, 0), 2), 0:1, c(.5,.5))
#'
gt_maj_pref = function(P, a, b, method="euclidean", q=.5){
	n = ncol(P)
	m = nrow(P)
	d1 = as.matrix(dist(t(cbind(P, a)), method = method))[n+1,-(n+1)]
	d2 = as.matrix(dist(t(cbind(P, b)), method = method))[n+1,-(n+1)]
	sum(d1 < d2)>(n*q)
	}


#' Assesses how much  majorities gain from a policy change in spatial model
#'
#' @param method Distance metric, deafulats to Euclidean
#' @param P matrix of ideal points
#' @param b Status quo policy
#' @param a Alternative policy
#' @keywords Plott, Spatial, Chaos
#' @export
#' @examples
#' gt_maj_gain(matrix(c(0, 0, .5, 1, 1, 0), 2), 0:1, c(.5,.5))
#'
#'
gt_maj_gain = function(P, a, b, method="euclidean"){
	n = ncol(P)
	m = nrow(P)
	d1 = as.matrix(dist(t(cbind(P, a)), method = method))[n+1,-(n+1)]
	d2 = as.matrix(dist(t(cbind(P, b)), method = method))[n+1,-(n+1)]
	sum((d2 - d1)[d1<d2])
	}
