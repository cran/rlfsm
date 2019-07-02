
# Technical function. Unavailable for users
param_characteris_builder <- function(param_name, MeanSdData, data, data_nor, indexD, indexF, env_run){

  cross_ind <- indexD==param_name & indexF=='Mean'
  data_cross_ind <- which(cross_ind)[1]

  if(is.na(data_cross_ind)) {

      warning("Probably there is no parameter to estimate")
      NULL

  } else {

      par_name_Mean <- paste(param_name,'Mean',sep='_')
      par_name_b <- paste(param_name,'b',sep='_')
      par_name_Sd <- paste(param_name,'Sd',sep='_')

      bias <- MeanSdData[par_name_Mean]-get(param_name, envir=env_run)
      attr(bias, "names")<-par_name_b

      MeanSdData<-c(MeanSdData, bias)
      data_n=(data[param_name]-MeanSdData[par_name_Mean])/MeanSdData[par_name_Sd]

      list(
          MeanSdData,
          data_nor<-rbind(data_nor, t(data_n)))
  }

}





#### CLT function ####
#' The function explores numerical properties of statistical estimators operating on random processes.
#'
#' The function is useful, for instance, when one needs to compute standard deviation of \eqn{\widehat \alpha_{high}}
#' estimator given a fixed set of parameters.
#'
#' CLT  performs Monte-Carlo experiments to compute parameters according to procedure Inference.
#' More specifically, for each element of s it generates Nmc lfsm sample paths with length equal to s[i], performs the statistical
#' inference on each, obtaining the estimates, and then returns their different statistics. It is vital that the estimator
#' returns a list of named parameters (one or several of 'sigma', 'alpha' and 'H'). CLT uses the names to lookup the true
#' parameter value and compute its bias.
#'
#' For sample path generation CLT uses a light-weight version of path, path_fast. In order to be applied,
#' function Inference must accept argument 'path' as a sample path.
#' @param Nmc Number of Monte Carlo repetitions
#' @param Inference statistical function to apply to sample paths
#' @param s sequence of path lengths
#' @inheritParams path
#' @param fr frequency. Either "H" or "L"
#' @param ... parameters to pass to Inference
#' @return It returns a list containing the following components:
#'     \item{CLT_dataset}{a data frame, standardized values of the estimates depending on path length s}
#'     \item{BSdM}{a data frame, means, biases and standard deviations depending on s}
#'     \item{Inference}{a closure used to obtain estimates}
#'     \item{alpha, H, sigma}{the parameters for which CLT performs path generation}
#'     \item{freq}{frequency, either 'L'  for low- or 'H' for high frequency}
#' @export
#' @examples
#' #### Set of global parameters ####
#' m<-25; M<-60
#' p<-.4; p_prime<-.2; k<-2
#' t1<-1; t2<-2
#' NmonteC<-5e1
#' S<-c(1e2,3e2)
#' alpha<-1.8; H<-0.8; sigma<-0.3
#'
#'
#' # How to plot empirical density
#' \donttest{
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,ContinEstim,
#'                      t1=t1,t2=t2,p=p,k=k)
#' l_plot<-Plot_dens(par_vec=c('sigma','alpha','H'),
#'                   CLT_data=theor_3_1_H_clt, Nnorm=1e7)
#'
#' }
#'
#' # For CLT() it is vital that the estimator returns a list of named parameters
#'
#' H_hat_f <- function(p,k,path) {hh<-H_hat(p,k,path); list(H=hh)}
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,H_hat_f,
#'                      p=p,k=k)
#'
#'
#' # The estimator can return one, two or three of the parameters.
#'
#' est_1 <- function(path) list(H=1)
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,est_1)
#'
#' est_2 <- function(path) list(H=0.8, alpha=1.5)
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,est_2)
#'
#' est_3 <- function(path) list(sigma=5, H=0.8, alpha=1.5)
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,est_3)
CLT<-function(Nmc,s,m,M,alpha,H,sigma,fr,Inference,...){

    i<-integer(0) # avoids NOTEs when being builded
    ind<-integer(0) # avoids NOTEs when being builded
    index<-NULL; indexD<-NULL; indexF<-NULL

    CLT_dataset<-data.frame()
    BSdM_data<-data.frame()
    BSdMData<-data.frame()

    for(i in 1:length(s)) {

        data<-data.frame()

        data<-foreach (ind = 1:Nmc, .combine = rbind, .packages='stabledist', .export = LofF, .inorder=FALSE) %dopar% {

            path <- path_fast(N=s[i],m=m,M=M,alpha=alpha,H=H,sigma=sigma,freq=fr)

            if(is.null(unlist(formals(Inference)['freq']))){
                LL<-Inference(path=path,...)
            } else  {
                LL<-Inference(path=path,freq=fr,...)
            }


            # check if the inference gives no errors (alpha>0 case).
            # Cases with errors are not included.
            if(!is.character(LL)) as.data.frame(LL)

        }

        #browser()
        nms<-names(data)
        data <- data.matrix(data) # convert all values to numeric
        data <- as.data.frame(data) # convert back to data frame

        if(!is.null(data) & !is.null(nms)){
            # WHAT IF THERE ARE NO NAMES?
            indexD<-rep(nms, each = 2)
            indexF<-rep(c("Sd","Mean"), times=length(nms))

            Sd<-function(x) sqrt(var(x, na.rm = TRUE))
            Mean<- function(x) sum(x, na.rm = TRUE)/length(x[!is.na(x)])
            ParFs<-c('Mean'=Mean,'Sd'=Sd)

            MeanSdData<-vector()
            VofF<-c("Sd","Mean")
            #browser()
            MeanSdData<-foreach (index = 1:length(indexD), .combine = c, .export = VofF) %dopar% {

                ParFs[[indexF[index]]](data[,indexD[index]])

            }
            names(MeanSdData)<-stringi::stri_join(indexD, indexF, sep="_")


            data_nor <- data.frame()
            env_CLT <- environment()

            alpha_ress <- param_characteris_builder(param_name='alpha', MeanSdData=MeanSdData,
                                                    data=data, data_nor=data_nor, indexD=indexD, indexF=indexF, env_run = env_CLT)
            if(!is.null(alpha_ress)) {
                MeanSdData <- alpha_ress[[1]]
                data_nor<-alpha_ress[[2]]
            }

            H_ress <- param_characteris_builder(param_name='H', MeanSdData=MeanSdData,
                                                data=data, data_nor=data_nor, indexD=indexD, indexF=indexF, env_run = env_CLT)
            if(!is.null(H_ress)) {
              MeanSdData <- H_ress[[1]]
              data_nor<-H_ress[[2]]
            }

            sigma_ress <- param_characteris_builder(param_name='sigma', MeanSdData=MeanSdData,
                                                    data=data, data_nor=data_nor, indexD=indexD, indexF=indexF, env_run = env_CLT)

            if(!is.null(sigma_ress)) {
                MeanSdData <- sigma_ress[[1]]
                data_nor<-sigma_ress[[2]]
            }



            if(is.null(data_nor)) {
              CLT_dataset<-rbind(CLT_dataset,s=s[i])
            } else {
              CLT_dataset<-rbind(CLT_dataset,cbind(s=s[i], t(data_nor)))
            }
            BSdMData<-rbind(BSdMData,c(s[i],MeanSdData))
            colnames(BSdMData)<-c("s",names(MeanSdData))

        } else {
            stop("The inference function either hasn't produced any data, or the estimates have no names")
        }

    }

    list(CLT_dataset=CLT_dataset,BSdM=BSdMData,Inference=Inference,alpha=alpha,H=H,sigma=sigma,freq=fr)
}
