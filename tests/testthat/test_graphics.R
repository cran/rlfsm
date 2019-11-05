
context("Graphical functions checks")

#### Parameter setup
m<-45; M<-60
p<-.4; p_prime<-.2
t1<-1; t2<-2; k<-2

NmonteC<-1e2
S<-1e3*(1:5)
alpha<-.8; H<-0.8; sigma<-0.3
###################

H_hat_f <- function(p,k,path) {hh<-H_hat(p,k,path); list(H=hh)}
theor_3_1_H_clt<-MCestimLFSM(s=S,fr='H',Nmc=NmonteC,
                             m=m,M=M,alpha=alpha,H=H,
                             sigma=sigma,H_hat_f,
                             p=p,k=k)
###################


###################
###################

###################
l_plot_vb <- Plot_vb(theor_3_1_H_clt)
l_plot_dens<- Plot_dens(par_vec=c('alpha','H'), MC_data=theor_3_1_H_clt, Nnorm=1e7)
###################
test_that("Graphics works with H-estimatior only", {
    expect_gt(length(l_plot_vb),expected=1)
    expect_equal(length(l_plot_dens), 2)
})
