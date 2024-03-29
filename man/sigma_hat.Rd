% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_for_estimates.r
\name{sigma_hat}
\alias{sigma_hat}
\title{Statistical estimator for sigma}
\usage{
sigma_hat(t1, k, path, alpha, H, freq)
}
\arguments{
\item{t1}{real number such that t1 > 0}

\item{k}{increment order}

\item{path}{sample path of lfsm on which the inference is to be performed}

\item{alpha}{self-similarity parameter of alpha stable random motion.}

\item{H}{Hurst parameter}

\item{freq}{Frequency of the motion. It can take two values: "H" for high frequency and "L" for the low frequency setting.}
}
\description{
Statistical estimator for sigma
}
\examples{
m<-45; M<-60; N<-2^14-M
alpha<-1.8; H<-0.8; sigma<-0.3
freq='H'
r=1; k=2; p=0.4; t1=1; t2=2

# Reproducing the work of ContinEstim
# in high frequency case
lfsm<-path(N=N,m=m,M=M,alpha=alpha,H=H,
           sigma=sigma,freq='L',disable_X=FALSE,seed=1)$lfsm

H_est<-H_hat(p=p,k=k,path=lfsm)
H_est
alpha_est<-alpha_hat(t1=t1,t2=t2,k=k,path=lfsm,H=H_est,freq=freq)
alpha_est

sigma_est<-tryCatch(
                    sigma_hat(t1=t1,k=k,path=lfsm,
                    alpha=alpha_est,H=H_est,freq=freq),
                    error=function(c) 'Impossible to compute sigma_est')
sigma_est
}
