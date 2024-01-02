# Simdata,echo=FALSE, fig.width=7,fig.height=5,results='asis'>>=
n<-100
YR<-c(-3:3)
b0<-.5
b1<-.2  #22% increase
  
phi <- 2
mu <- exp(b0+b1*YR)/(1+exp(b0+b1*YR))
  
alpha <- phi*mu
beta <- phi*(1-mu)
  
rho <- 1/(1 + alpha + beta)
  
#mu<-exp(2)/(1+exp(2))
  
lim.0 <- c(0, 0.001, 0.05, 0.25, 0.5, 0.75, 0.95, 1)  #cut-points included for zero class
  
y.beta <- numeric()
  
for(i in 1:length(alpha)){
    y.beta<-c(y.beta,rbeta(n,alpha[i],beta[i]))
  }
  
Y.bb <- rbinom(length(y.beta), 30, prob = y.beta)

Y.ord <- cut(y.beta, lim.0)

levels(Y.ord) <- c(0,1,2,3,4,5,6)
  
dat.betatrend <- data.frame(Y = y.beta, 
                            Year = rep(YR,each = n),
                            Y.betabin = Y.bb, 
                            Y.ord = Y.ord)
  
Fcn.logit<-function(y.in){
    if(y.in==0) y.in<- y.in+.001 else
      if(y.in==1) y.in<- y.in-.001 else y.in<-y.in
      return(log(y.in/(1-y.in)))}
  
dat.betatrend$logit.bin <- aaply(dat.betatrend$Y.betabin/30,1,Fcn.logit)
  
par(mfrow=c(1,2),pty='s')
  
  # hist(y.beta,breaks=c(lim.0))
  # with(dat.betatrend,scatter.smooth(Year,Y,xlab="Year",ylab="Beta Random Variable"))
  
print(xtable(data.frame("delta"=rho[1],"beta_0"=b0,"beta_1"=b1,"phi"=phi),
               caption="True Beta Parameter Values Used to Simulate Data"),
        include.rownames=FALSE,
        include.colnames=TRUE, 
        caption.placement="bottom")
  
#@
    
#<<betabinom,eval=FALSE,results='asis',echo=TRUE,message=FALSE>>=
    
#This is fitting a beta-binomial model using the VGAM package and the vglm function.
    
mod.betabinom <- vglm(cbind(Y.betabin, c(rep(30,700)-Y.betabin)) ~ Year, betabinomial(zero = "rho"),
                          data = dat.betatrend, trace = TRUE)
#@
    
#<<vgam-results,eval=FALSE,echo=FALSE,results='asis'>>=
    
#this just creates a pretty output table from vglm.
    
CIs <- confintvglm(mod.betabinom, method = "wald")
  
coefs<-c(coef(mod.betabinom, matrix = TRUE)[1,2], coef(mod.betabinom, matrix = TRUE)[,1])
  
tab<-cbind(coefs,rbind(CIs[2,],CIs[1,],CIs[3,]))
  
  
colnames(tab)<-c("Coefficients","Lower 95% CI", "Upper 95% CI")
  
row.names(tab)<-c("logit(delta)","beta_0","beta_1")
  
  
print(xtable(tab,caption="Estimated Values and Wald Confidence Intervals for Beta-Binomial with VGAM pkg"),include.rownames=TRUE, include.colnames=TRUE)
  
  
  
  @
    
    % latex table generated in R 3.5.1 by xtable 1.8-3 package
  % Tue Jan 29 13:06:42 2019
  \begin{table}[ht]
  \centering
  \begin{tabular}{rrrr}
  \hline
  & Coefficients & Lower 95\% CI & Upper 95\% CI \\ 
  \hline
  logit(delta) & -0.66 & -0.77 & -0.55 \\ 
  beta\_0 & 0.51 & 0.42 & 0.60 \\ 
  beta\_1 & 0.17 & 0.12 & 0.21 \\ 
  \hline
  \end{tabular}
  \caption{Estimated Values and Wald Confidence Intervals for Beta-Binomial with VGAM pkg} 
  \end{table}
  
  <<logit.emp,results='asis',echo=TRUE>>=
    # for comparison for pin-point data, a logistic regression for binomial counts is fit using
    # glm function in Base R.
    
    mod.glm<-glm(cbind(Y.betabin, c(rep(30,700)-Y.betabin)) ~ Year,
                 data=dat.betatrend,family=binomial(link=logit))
  
  # Another approach is to use logit-transformation and then apply a standard linear regression model.
  
  mod.lm<-lm(logit.bin~Year,data=dat.betatrend)
  @
    
    
    <<diag.logitbinom, results='asis',fig.cap='Diagnostic plot for logistic regression for binomial counts',echo=TRUE>>=
    
    #built-in diagnostic plots for glm models
    par(mfrow=c(2,2),pty='m')
  
  plot(mod.glm)
  
  @
    
    <<diag.lmblogit, results='asis',fig.cap='Diagnostic plot for logit-transformation of empirical proportions and applying linear regression',echo=TRUE>>=
    
    #built-in diagnostic plots for lm models
    
    par(mfrow=c(2,2),pty='m')
  
  plot(mod.lm)
  
  @
    
    <<glm-lm-results,echo=FALSE,results='asis'>>=
    
    print(xtable(summary(mod.glm),caption="Results for fitting binomial logistic regression model for trend "))
  
  print(xtable(summary(mod.lm),caption="Results for fitting linear model for trend with logit-transformed proportions"))
  
  @