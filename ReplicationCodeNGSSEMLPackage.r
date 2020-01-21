################################################################################
### R code, Section 4 ###
################################################################################
rm(list=ls(all=TRUE))  

## Select server
options(repos="http://cran.r-project.org")  

# load packages
if(!require("dlm")) {install.packages("dlm"); library("dlm")}
if(!require("mvtnorm")) {install.packages("mvtnorm"); library("mvtnorm")}
if(!require("fields")) {install.packages("fields"); library("fields")}
if(!require("interp")) {install.packages("interp"); library("interp")}
if(!require("compiler")) {install.packages("compiler"); library("compiler")}
if(!require("car")) {install.packages("car");library("car")}
if(!require("NGSSEML")) {install.packages("NGSSEML"); library("NGSSEML")}
#Note: Please, you must check if the packages above have installed correctly. 
#Otherwise, NGSSEML package may return some error message. 

################################################################################
## Section 4.1, Count data ##
################################################################################
library("NGSSEML")
# example data
# MLE estimation:
data(Yt)
Xtm=Yt[,3:7]    # Xt as matrix always!
#Xt[,1]=Xt[,1]*1000
Xtm[,1]=(1:168-73)/168
Ytm=Yt$y
Ztm=NULL
model="Poisson"
#Trend,CosAnnual,SinAnnual,CosSemiAnnual,SinSemiAnnual
#LabelParTheta=c("w","Beta1","Beta2","Beta3","Beta4","Beta5")
StaPar=c(0.8,-0.8,0.01,0.01,0.01,0.01)
a0=0.01
b0=0.01
ci=0.95
data1=data.frame(Ytm,Xtm)
#Fit:
fit=ngssm.mle(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data1,model=model,pz=NULL,StaPar=StaPar,a0=a0,b0=b0,ci=ci)
#fit
# Point estimation and confidence intervals #
###############################################
# Bayesian estimation:
library("NGSSEML")
#### Inputs: 
data(Yt)
Xtm=Yt[,3:7]   
Ytm=Yt$y
Ztm=NULL
model="Poisson"
#CosAnnual,SinAnnual,CosSemiAnnual,SinSemiAnnual
#LabelParTheta=c("w","Beta1","Beta2","Beta3","Beta4","Beta5")
StaPar=c(0.79,-0.002,-0.11,-0.49,0.18,-0.38)
a0=0.01
b0=0.01
pointss=8     ### points
nsamplex=4000  ##sample
ci=0.95       ## Cred. level
data1=data.frame(Ytm,Xtm)
#Fit:
#Bayesian:
set.seed(24666)
fitbayes=ngssm.bayes(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data1,model=model,pz=NULL,StaPar=StaPar,
a0=a0,b0=b0,prw=c(1,1),prbetamu=rep(0,5),prbetasigma=diag(10, 5, 5),
ci=ci,pointss=pointss,nsamplex=nsamplex,postplot=TRUE,contourplot=TRUE)
###############################################
# Smoothing #
posts=fitbayes[[2]]
#PlotF function:
dev.new()
x = seq(as.Date("1970/01/01"),as.Date("1983/12/31"),"months") 
PlotF(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data1,model=model,pz=NULL,axisxdate=x,Proc="Smooth",Type="Marg",
a0=0.01,b0=0.01,ci=0.95,posts=posts,startdate="1970/01/01",enddate="1983/12/31",Freq="months",
cols=c("black","blue","lightgrey"),xxlab="t",yylab="Yt",yylim=c(0,15),Lty=c(1,2,1),Lwd=c(2,2,2),Cex=0.68)

################################################################################
## Section 4.2, Volatility data ##
################################################################################
library("NGSSEML")
# example data
###############################################
# MLE estimation:
#GED:
data(Rt)
Ytm=Rt$Rt
Xt=NULL
Zt=NULL 
model="GED"
#LabelParTheta=c("w","nu")
StaPar=c(0.9,1)
StaPar1=c(exp(StaPar[1])/(1+exp(StaPar[1])),log(StaPar[2]))
a0=0.01
b0=0.01
ci=0.95
fit=ngssm.mle(Ytm~1,data=data.frame(Ytm),model=model,pz=NULL,a0=a0,b0=b0,ci=ci)

# Point estimation and confidence intervals #
###############################################
# Bayesian estimation:
#library("NGSSEML")
data(Rt)
Ytm=Rt$Rt
Date=Rt$Date
Xtm=NULL
Ztm=NULL
model="GED"
#LabelParTheta=c("W","nu")
StaPar=c(0.9,1)
p=length(StaPar)
nn=length(Ytm)
a0=0.01
b0=0.01
pointss=15    ### points
nsamplex=1000 #sample
ci=0.95        # Cred. level 
#Bayesian:
fitbayes=ngssm.bayes(Ytm~1,data=data.frame(Ytm),model=model,pz=NULL,
StaPar=StaPar,a0=a0,b0=b0,prw=c(1,1),prnu=c(0.01,0.01),ci=ci,
pointss=pointss,nsamplex=nsamplex,postplot=TRUE,contourplot=TRUE)
  
###############################################
# Smoothing #
#### Inputs: 
#Smoothing:
set.seed(1000)
posts=fitbayes[[2]]
############
#PlotF function:
dev.new()
#Axis-x date:
PlotF(Ytm~1,data=data.frame(Ytm),model=model,pz=NULL,
axisxdate=Rt$Date,plotYt=FALSE,transf=-0.5,Proc="Smooth",
Type="Marg",distl="PRED",a0=a0,b0=b0,
ci=ci,posts=posts,startdate='2000-06-01',enddate='2008-01-29',Freq="days",Typeline='l',
cols=c("black","blue","lightgrey"),xxlab="t",yylab=expression(paste(hat(sigma)[t])),
yylim=c(0.02,0.10),Lty=c(1,2,1),Lwd=c(2,2,2),Cex=0.68)
dev.new()

#Axis-x order of observations:
PlotF(Ytm~1,data=data.frame(Ytm),model=model,pz=NULL,axisxdate=NULL,
plotYt=FALSE,transf=-0.5,Proc="Smooth",
Type="Marg",distl="PRED",a0=a0,b0=b0,
ci=ci,posts=posts,startdate=NULL,enddate=NULL,Freq=NULL,Typeline='l',
cols=c("black","blue","lightgrey"),xxlab="t",yylab=expression(paste(hat(sigma)[t])),
yylim=c(0.02,0.10),Lty=c(1,2,1),Lwd=c(2,2,2),Cex=0.68)

################################################################################
## Section 4.3, Piecewise exponential data ##
################################################################################
library("NGSSEML")
# example data
###############################################
# MLE estimation:
data(gte_data)
Ytm=gte_data$V1
Xtm=NULL
Ztm=NULL
model="PEM"
amp=FALSE
Eventm=gte_data$V2        # Event: failure, 1.
Break=GridP(Ytm, Eventm, nT = NULL)
#LabelParTheta=c("w")
StaPar=c(0.73)
a0=0.01
b0=0.01
ci=0.95
fit=ngssm.mle(Ytm~1,data=data.frame(Ytm,Eventm),model=model,nBreaks=NULL,
amp=amp,a0=a0,b0=b0,ci=ci) 

# Point estimation and confidence intervals #
###############################################
# Bayesian estimation:
#library("NGSSEML")
#### Inputs: 
data(gte_data)
Ytm=gte_data$V1
Eventm=gte_data$V2   # Event: failure, 1.
Breakm=GridP(Ytm, Eventm, nT = NULL)
Xtm=NULL
Ztm=NULL
model="PEM"
amp=FALSE
#LabelParTheta=c("w")
StaPar=c(0.5)
lower=c(0.01)
upper=c(0.99)
p=length(StaPar)
nn=length(Ytm)
a0=0.01
b0=0.01
pointss=50    ### points
nsamplex=3000 ## Sampling posterior
ci=0.95
alpha=1-ci
#Fit:
fitbayes=ngssm.bayes(Ytm~1,data=data.frame(Ytm,Eventm),model=model,pz=NULL,StaPar=StaPar,
amp=amp,a0=a0,b0=b0,prw=c(1,1),prnu=NULL,prchi=NULL,prmu=NULL,prbetamu=NULL,
prbetasigma=NULL,ci=ci,pointss=pointss,nsamplex=nsamplex, 
postplot=TRUE,contourplot=FALSE)

###############################################
# Smoothing #
#### Inputs: 
posts=fitbayes[[2]]
#Smoothing:
set.seed(1000)
dev.new()
#PlotF function:
PlotF(Ytm~1,data=data.frame(Ytm,Eventm),pz=NULL,plotYt=FALSE,
axisxdate=Break[1:17],model=model,Proc="Smooth",Type="Marg",distl="PRED",a0=a0,b0=b0,
ci=ci,posts=posts,Typeline='s',
cols=c("black","blue","lightgrey"),xxlab="Time to Failure (Days)",yylab="Failure rate",
yylim=c(0,0.008),xxlim=c(0,139),Lty=c(1,2,1),Lwd=c(2,2,2),Cex=0.68)

################################################################################
## Section 4.4, Software reliability data ##
################################################################################
#library("NGSSEML")
# example data
###############################################
# MLE estimation:
data(sys1_data)
Ytm=sys1_data[,1]+0.00001
Xtm=sys1_data[,2]   # Xt as matrix always!
Zt=NULL
model="SRWeibull"
#LabelParTheta=c("w","alpha","Beta1")
StaPar=c(0.9,0.7,0.01)
fit=ngssm.mle(Ytm~Xtm,data=data.frame(Ytm,Xtm),
model=model,pz=NULL,StaPar=StaPar,a0=0.01,b0=0.01,ci=0.95)

# Point estimation and confidence intervals #
###############################################
# Bayesian estimation:
#library("NGSSEML")
#### Defaults values (NULL): 
#### Inputs: 
data(sys1_data)
Ytm=sys1_data[,1]+0.00001
Xtm=sys1_data[,2]
model="SRWeibull"  
#model="SRGamma"  
#LabelParTheta=c("w","nu","Beta")
pointss=20    ### points
#Fit:
StaPar=c(0.98,0.75,0.02)
fitbayes=ngssm.bayes(Ytm~Xtm,data=data.frame(Ytm,Xtm),
model=model,pz=NULL,StaPar=StaPar,
prw=c(1,1),prnu=c(0.1,0.1),prbetamu=rep(0,1),prbetasigma=diag(100,1,1),
pointss=pointss,nsamplex=3000,postplot=TRUE,contourplot=TRUE)

###############################################
#Smoothing #
#### Inputs: 
posts=fitbayes[[2]]
#Smoothing:
set.seed(1000)
dev.new()
#PlotF function:
PlotF(Ytm~Xtm,data=data.frame(Ytm,Xtm),pz=NULL,plotYt=TRUE,
transf=1/4,axisxdate=NULL,model=model,
Proc="Smooth",Type="Marg",distl="PRED",a0=0.01,b0=0.01,
ci=0.95,posts=posts,Typeline='l',cols=c("black","blue","lightgrey"),
xxlab="Number of Failures",yylab=expression(paste(y[i]^(1/4))),
yylim=c(-2,10),xxlim=c(0,139),Lty=c(1,2,1),Lwd=c(1,2,1),Cex=0.68)
################################################################################
################################################################################
 
