
#### CLT function ####
#' The function explores numerical properties of statistical estimators operating on random processes.
#'
#' The function is useful, for instance, when one needs to compute standard deviation of \eqn{\widehat \alpha_{high}}
#' estimator given a fixed set of parameters.
#'
#' CLT  performs Monte-Carlo experiments to compute parameters according to procedure Inference.
#' More specifically, for each element of s it generates Nmc lfsm sample paths with length equal to s[i], performs the statistical
#' inference on each, obtaining the estimates, and then returns their different statistics.
#'
#' For sample path generation CLT uses a light-weight version of path, path_fast.
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
#' theor_3_1_H_clt<-CLT(s=S,fr='H',Nmc=NmonteC,
#'                      m=m,M=M,alpha=alpha,H=H,
#'                      sigma=sigma,ContinEstim,
#'                      t1=t1,t2=t2,p=p,k=k)
#' l_plot<-Plot_dens(par_vec=c('sigma','alpha','H'),
#'                   CLT_data=theor_3_1_H_clt, Nnorm=1e7)
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
            LL<-Inference(path=path,freq=fr,...)
            # check if the inference gives no errors (alpha>0 case).
            # Cases with errors are not included.
            if(!is.character(LL)) as.data.frame(LL)

        }
        if(!is.null(data)){

            indexF<-c("Sd","Mean","Sd","Mean","Sd","Mean")
            indexD<-c('alpha','alpha','H','H','sigma','sigma')

            Sd<-function(x) sqrt(var(x))
            Mean<- function(x) sum(x)/length(x)
            ParFs<-c('Mean'=Mean,'Sd'=Sd)

            MeanSdData<-vector()
            VofF<-c("Sd","Mean")
            MeanSdData<-foreach (index = 1:6, .combine = c, .export = VofF) %dopar% {

                ParFs[[indexF[index]]](data[,indexD[index]])

            }
            names(MeanSdData)<-stringi::stri_join(indexD, indexF, sep="_")


            MeanSdData['alpha_b']<-MeanSdData['alpha_Mean']-alpha
            MeanSdData['H_b']<-MeanSdData['H_Mean']-H
            MeanSdData['sigma_b']<-MeanSdData['sigma_Mean']-sigma

            # Normalized values
            data_nor<-cbind((data$H-MeanSdData['H_Mean'])/MeanSdData['H_Sd'],
                            (data$alpha-MeanSdData['alpha_Mean'])/MeanSdData['alpha_Sd'],
                            (data$sigma-MeanSdData['sigma_Mean'])/MeanSdData['sigma_Sd'])

            colnames(data_nor)<-c("H","alpha","sigma")

            r<-cbind(s=s[i], data_nor)
            CLT_dataset<-rbind(CLT_dataset,r)
            BSdMData<-rbind(BSdMData,c(s[i],MeanSdData))
            colnames(BSdMData)<-c("s",names(MeanSdData))
            BSdM_data<-cbind(BSdMData$s, BSdMData$alpha_Mean, BSdMData$alpha_Sd, BSdMData$alpha_b,
                                         BSdMData$H_Mean,     BSdMData$H_Sd,     BSdMData$H_b,
                                         BSdMData$sigma_Mean, BSdMData$sigma_Sd, BSdMData$sigma_b)
            colnames(BSdM_data)<-c("s","alpha_Mean","alpha_Sd","alpha_b",
                                       "H_Mean","H_Sd","H_b",
                                       "sigma_Mean","sigma_Sd","sigma_b")
        }

    }

    list(CLT_dataset=CLT_dataset,BSdM=BSdM_data,Inference=Inference,alpha=alpha,H=H,sigma=sigma,freq=fr)
}
