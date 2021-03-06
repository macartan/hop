% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_slope_text.R
\name{gt_slope_text}
\alias{gt_slope_text}
\title{Helper to slope text}
\usage{
gt_slope_text(labels, f, xl = 0, xh = 1, vshift = 0.05, col = "black",
  fixer = 1, pos = NULL, adj = c(0.5, 0), cex = 1, shadow = TRUE,
  shade = "white")
}
\arguments{
\item{labels}{text}
}
\description{
Note angle is a bit messy because of aspect ratios; fixer can be used to adjust
}
\examples{
frame()
gt_slope_text("quadratic", f = function(x) x^2)
}
\keyword{text}
