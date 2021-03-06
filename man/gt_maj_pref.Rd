% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_spatial_preferences.R
\name{gt_maj_pref}
\alias{gt_maj_pref}
\title{Majority Preference Relation.}
\usage{
gt_maj_pref(P, a, b, method = "euclidean", q = 0.5)
}
\arguments{
\item{P}{matrix of ideal points}

\item{a}{Alternative policy}

\item{b}{Status quo policy}

\item{method}{Distance metric, deafulats to Euclidean}
}
\description{
Let P be a \code{d*n} vecotr of  ideal points for n individuals in d dimensions a and b are d*1 policy vectors \code{gt_maj_pref(P,a,b)} reports whether a strict majority of individuals strictly prefer a to b
}
\examples{
gt_maj_pref(matrix(c(0, 0, .5, 1, 1, 0), 2), 0:1, c(.5,.5))

}
\keyword{Chaos}
\keyword{Plott,}
\keyword{Spatial,}
