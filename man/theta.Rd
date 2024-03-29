% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance_matrix.R
\name{theta}
\alias{theta}
\title{Function theta}
\usage{
theta(p, alpha, sigma, g, h)
}
\arguments{
\item{p}{power, real number from (-1,1)}

\item{alpha}{self-similarity parameter of alpha stable random motion.}

\item{sigma}{Scale parameter of lfsm}

\item{g, h}{functions \eqn{g,h: \R \to \R} with finite alpha-norm (see \code{\link{Norm_alpha}}).}
}
\description{
Function of the form
\deqn{\theta(g,h)_{p} = a_p^{-2} \int_{\R^2}  |xy|^{-1-p}U_{g,h}(x,y) dxdy}
}
\references{
\insertRef{MOP18}{rlfsm}
}
