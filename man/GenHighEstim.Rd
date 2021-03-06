% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limit_theorems.R
\name{GenHighEstim}
\alias{GenHighEstim}
\title{High frequency estimation procedure for lfsm.}
\usage{
GenHighEstim(p, p_prime, path, freq, low_bound = 0.01, up_bound = 4)
}
\arguments{
\item{p}{power}

\item{p_prime}{power}

\item{path}{sample path of lfsm on which the inference is to be performed}

\item{freq}{Frequency of the motion. It can take two values: "H" for high frequency and "L" for the low frequency setting.}

\item{low_bound}{positive real number}

\item{up_bound}{positive real number}
}
\description{
General estimation procedure for high frequency case when 1/alpha is not a natural number.
"Unnecessary" parameter freq is preserved to allow for investigation of the inference procedure in low frequency case
}
\details{
In this algorithm the preliminary estimate of alpha is found via using \code{\link{uniroot}} function. The latter is
given the lower and the upper bounds for alpha via low_bound and up_bound parameters. It is not possible to
pass 0 as the lower bound because there are numerical limitations on the alpha estimate, caused by the
length of the sample path and by numerical errors. p and p_prime must belong to the interval (0,1/2) (in the notation kept in rlfsm package)
The two powers cannot be equal.
}
\examples{
m<-45; M<-60; N<-2^10-M
sigma<-0.3
p<-0.2; p_prime<-0.4

#### Continuous case
lfsm<-path(N=N,m=m,M=M,alpha=1.8,H=0.8,
           sigma=sigma,freq='L',disable_X=FALSE,seed=3)$lfsm

GenHighEstim(p=p,p_prime=p_prime,path=lfsm,freq="H")

#### H-1/alpha<0 case
lfsm<-path(N=N,m=m,M=M,alpha=0.8,H=0.8,
           sigma=sigma,freq='H',disable_X=FALSE,seed=3)$lfsm

GenHighEstim(p=p,p_prime=p_prime,path=lfsm,freq="H")

}
\references{
\insertRef{MOP18}{rlfsm}
}
