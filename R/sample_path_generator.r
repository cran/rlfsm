# Paper by Stoev and Taqqu "Simulation methods for linear fractional stable motion and
# FARIMA using the Fast Fourier Transform"



#' Creates the corresponding value from the paper by Stoev and Taqqu (2004).
#' @references \insertRef{StoevTaqqu04}{rlfsm}
#' @export
#' @inheritParams path
a_tilda<-function(N, m, M, alpha, H){

    a<-vector(mode="numeric", length=m*(M+N)) # it creates 0 in mM to m(M+N) anyway

    # Function (t-s)_+^H-1/a
    PLUS<-function(x,beta) if(x>0) x^beta else  0

    # Computation of a's
    for(ind_j in 1:(m*M)){

        a[ind_j]<-((ind_j/m)^(H-1/alpha) - PLUS(ind_j/m-1, H-1/alpha))*m^(-1/alpha)

    }
    a
}

#' Generator of linear fractional stable motion
#'
#' The function creates a lfsm sample path using the numerical algorithm from the paper by Stoev and Taqqu. Linear fractional stable motion is defined as
#' \deqn{X_t = \int_{\R} \left\{(t-s)_+^{H-1/\alpha} - (-s)_+^{H-1/\alpha} \right\} dL_s}
#' @return It returns a list containing the motion, the underlying Levy motion, the point number of the motions from 0 to N and the corresponding coordinate (which depends on the frequency), the parameters that were used to generate the lfsm, and the predefined frequency.
#' @param N a number of points of the lfsm.
#' @param m discretization. A number of points between two nearby motion points
#' @param M truncation parameter. A number of points at which the integral representing the definition of lfsm is calculated. So, after M points back we consider the rest of the integral to be 0.
#' @param alpha self-similarity parameter of alpha stable random motion.
#' @param H Hurst parameter
#' @param sigma Scale parameter of lfsm
#' @param freq Frequency of the motion. It can take two values: "H" for high frequency and "L" for the low frequency setting.
#' @param disable_X is needed to disable computation of X. The default value is FALSE. When it is TRUE, only a levy motion is returned, which in turn reduces the computation time. The feature is particularly useful for reproducibility when combined with seeding.
#' @param seed this parameter performs seeding of path generator
#' @param levy_increments increments of Levy motion underlying the lfsm.
#'
#' @seealso \code{\link{paths}} simulates a number of lfsm sample paths.
#' @references \insertRef{StoevTaqqu04}{rlfsm}
#' @examples
#' # Path generation
#'
#' m<-256; M<-600; N<-2^10-M
#' alpha<-1.8; H<-0.8; sigma<-1
#' seed=2
#'
#' List<-path(N=N,m=m,M=M,alpha=alpha,H=H,
#'            sigma=sigma,freq='L',disable_X=FALSE,seed=3)
#'
#' # Normalized paths
#' Norm_lfsm<-List[['lfsm']]/max(abs(List[['lfsm']]))
#' Norm_oLm<-List[['levy_motion']]/max(abs(List[['levy_motion']]))
#'
#' # Visualization of the paths
#' plot(Norm_lfsm, col=2, type="l", ylab="coordinate")
#' lines(Norm_oLm, col=3)
#' leg.txt <- c("lfsm", "oLm")
#' legend("topright",legend = leg.txt, col =c(2,3), pch=1)
#'
#'
#' # Creating Levy motion
#' levyIncrems<-path(N=N, m=m, M=M, alpha, H, sigma, freq='L',
#'                   disable_X=TRUE, levy_increments=NULL, seed=seed)
#'
#' # Creating lfsm based on the levy motion
#'   lfsm_full<-path(m=m, M=M, alpha=alpha,
#'                   H=H, sigma=sigma, freq='L',
#'                   disable_X=FALSE,
#'                   levy_increments=levyIncrems$levy_increments,
#'                   seed=seed)
#'
#' sum(levyIncrems$levy_increments==
#'     lfsm_full$levy_increments)==length(lfsm_full$levy_increments)
#'
#'
#'
#'
#' @export
#'
path<-function(N=NULL,m,M,alpha,H,sigma,freq,disable_X=FALSE,levy_increments=NULL,seed=NA){

    if(is.null(N) & is.null(levy_increments)) stop('Levy motion is not specified in any way')
    if(!is.null(N) & !is.null(levy_increments)) stop('Both levy_increments and N are specified')

    #### Computing increments ####
    if(is.null(N)) {

            # !!!!!!! the logic must be checked
            # m(M+N) points of LM are taken; snapshot
            # We could specify increments of levy motion instead of levy motion. lm implies unnecessary summations
            # add full (M+N)m levy motion as an output ? -> generate LM then substitute it to generate lfsm
            lenLevyInc<-length(levy_increments)

            # Integerness of N is checked numerically, so there may be false positives.
            N<-lenLevyInc/m-M; if(!(N%%1==0)) stop('N is not integer')
            Z<-c(levy_increments[(m*M+1):(m*(N+M))],levy_increments[1:(m*M)]) # increments of the levy motion

        } else {

            ### Part 1. Generating levy motion.

            #### clause 3

            if(!is.na(seed)) set.seed(seed)
            Z<-rstable(m*(N+M),alpha,beta=0) # standart SaS variables
            levy_increments<-c(Z[(m*N+1):(m*(M+N))],Z[1:(m*N)])

        }


    ### Setting of the frequency

    # freq may take only "H" and "L" values(high- and low frequency settings)
    if(freq=="H") {multiplier<-1/(N^H); coords<-seq(from = 0, to = 1, length.out = N+1)
    } else if (freq=="L") {multiplier<-1;  coords<-seq(from = 0, to =N)
    } else stop("the freq takes only two parameters- H and L ")


    #### Extraction of levy motion
    #L_noise<-Z[1:(m*N)]
    LM<-vector(mode="numeric",length=N)
    LM_N<-0

    for(i in 1:N){

        LM[i]<-LM_N+sum(Z[(m*(i-1)+1):(m*i)])
        LM_N<-LM[i]

    }
    LM<-c(0,LM) # LM[0] is not 0, but it can be for the time being

    ### Part 2. Generating linear fractional stable motion.

    if(disable_X){

	    X<-NULL

    } else {

	    ##### clause 2
	    a_t<-a_tilda(N,m,M,alpha,H)*sigma

	    a_hat<-fft(a_t)

    	##### clause 3
    	Z_hat<-fft(Z)

    	#### clause 4
	    # In R the definition of inverse fft lacks the
	    # normalization constant, so here we divide by it explicitly.
	    W_raw<-fft(a_hat*Z_hat, inverse=TRUE)/length(a_hat)
	    W<-W_raw[1:(m*N)]

	    #### clause 5
	    index_m<-m*(1:(length(W)/m))
	    Y_m_M<-Re(W[index_m]) # although W is real, 0i-part isn't needed

	    X<-vector(mode="numeric",length=length(Y_m_M))
	    X<-cumsum(Y_m_M)
	    X<-c(0,X) # lfsm starts from zero (t=0 => kernel = 0)

    }

    list(point_num=0:N, coordinates=coords, lfsm=multiplier*X, levy_motion=LM,
         levy_increments=levy_increments, pars=c(alpha=alpha,H=H,sigma=sigma), frequency=freq)

}