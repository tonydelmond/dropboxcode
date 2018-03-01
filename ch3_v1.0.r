#Clear the console, clear the workspace, and detach all dataframes----
cat("\014")
rm(list = ls())

#Set working directory
#Tony's Location (desktop)
#setwd("C:/Users/tony/Dropbox/Research_and_Reading/Research/Dissertation/Chapters/Chapter_3/Code")
#Tony's Location (laptop)
setwd("C:/Users/tonyd/Dropbox/Research_and_Reading/Research/Dissertation/Chapters/Chapter_3/Code")

#Double Bounded Discrete Choice Contingent Valuation Method
if(TRUE){

###################################################
###################################################
##########     IMPORT AND FORMAT DATA    ##########
###################################################
###################################################

  #B1 = x[,2]
  #B^u = x[,3]
  #B^d = x[,4]
  #d^yy = x[,5]
  #d^yn = x[,6]
  #d^ny = x[,7]
  #d^nn = x[,8]
  
  #Import raw data
  #data = read.csv("data1_dbdc_raw.csv")
  data = read.csv("data1_dbdc_cleaned.csv")
  
  #Include response variables with (0,1) formatting, conditional bid values, renamed variables
  dataV2 <- data.frame(cbind("R1" = data$first #1-yes, 2-no
                             ,"R2" = data$second #1-yes, 2-no
                             ,"BD1" = 9.99 #initial bid
                             ,"BD2" = data$finalamt #second bid (up if R1=1, else down)
                             ,"dem.age" = data$screener_age
                             ,"dem.female" = data$screener_gender
                             ,"dem.education" = data$ed_screener
                             ,"dem.income" = data$income_screener
                             ,"dem.employmentcode" = data$employment_status
                             ,"dem.employmentdummy" = data$employment_status
                             ,"dem.ethnicity" = data$ethnicity_screener
                             ,"Aroma" = data$Aroma
                             ,"Appearance" = data$Appearance
                             ,"Taste" = data$Taste.Flavor
                             ,"Overall" = data$Overall
                             ,"PrimaryShopper" = data$X1.Primary_Shopper
                             ,"AvgConsumption" = data$X2.Avg_consumption
  ))

  #Reformat specific variables
  #Gender
    dataV2$dem.female[dataV2$dem.female==1] <- NA #Recode unspecified gender (initially 1) to NA
    dataV2$dem.female[dataV2$dem.female==2] <- 1  #Recode gender female (initially 2) to 1
    dataV2$dem.female[dataV2$dem.female==3] <- 0  #Recode gender male (initially 3) to 0
  #Education
    dataV2$dem.education[dataV2$dem.education==1] <- NA #Recode unspecified education (initially 1) to NA
    dataV2$dem.education[dataV2$dem.education==2] <- 3  #Recode "2-year/Vocational" (initially 2) to 3
    dataV2$dem.education[dataV2$dem.education==3] <- 4  #Recode "4-year degree/Bachelor's" (initially 3) to 4
    dataV2$dem.education[dataV2$dem.education==4] <- 6  #Recode "Doctorate" (initially 1) to 6
    dataV2$dem.education[dataV2$dem.education==5] <- 1  #Recode "High School" education (initially 5) to 1
    dataV2$dem.education[dataV2$dem.education==6] <- 5  #Recode "Master's" (initially 6) to 5
    dataV2$dem.education[dataV2$dem.education==7] <- NA #Recode "Other" education (initially 7) to NA
    dataV2$dem.education[dataV2$dem.education==8] <- 2  #Recode "Some College" (initially 8) to 2
  #Income
    dataV2$dem.income[dataV2$dem.income==1] <- NA #Recode unspecified income (initially 1) to NA
    dataV2$dem.income[dataV2$dem.income==2] <- 2 #Recode 10K-20K (initially 2) to 2
    dataV2$dem.income[dataV2$dem.income==3] <- 11 #Recode 100K-120K (initially 1) to 11
    dataV2$dem.income[dataV2$dem.income==4] <- 3 #Recode 20K-30K (initially 4) to 3
    dataV2$dem.income[dataV2$dem.income==5] <- 4 #Recode 30K-40K (initially 5) to 4
    dataV2$dem.income[dataV2$dem.income==6] <- 5 #Recode 40K-50K (initially 6) to 5
    dataV2$dem.income[dataV2$dem.income==7] <- 6 #Recode 50K-60K (initially 7) to 6
    dataV2$dem.income[dataV2$dem.income==8] <- 7 #Recode 60K-70K (initially 8) to 7
    dataV2$dem.income[dataV2$dem.income==9] <- 8 #Recode 70K-80K (initially 1) to NA
    dataV2$dem.income[dataV2$dem.income==10] <- 9 #Recode 80K-90K (initially 10) to 9
    dataV2$dem.income[dataV2$dem.income==11] <- 10 #Recode 90K-100K (initially 11) to 10
    dataV2$dem.income[dataV2$dem.income==12] <- 1 #Recode <10K (initially 12) to 1
    dataV2$dem.income[dataV2$dem.income==13] <- 12 #Recode >120K (initially 13) to 12
  #Employment Code (treated as level of employment, least to greatest)
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==1] <- NA #Unspecified employment (initially 1) to NA
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==2] <- 7 #Recode "Full Time" (initially 2) to 7
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==3] <- 6 #Recode "Part Time" (initially 3) to 6
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==4] <- 4 #Recode "Homemaker" (initially 4) to NA
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==5] <- 5 #Recode "Other" employment (initially 5) to NA
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==6] <- 2 #Recode "Retired" (initially 6) to NA
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==7] <- 3 #Recode "Student" (initially 7) to NA
    dataV2$dem.employmentcode[dataV2$dem.employmentcode==8] <- 1 #Recode "Unemployed" (initially 8) to NA
  #New Employment Dummy (Treated as a dummy of certainty of income from employment)
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==1] <- NA #Unspecified employment (initially 1) to NA
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==2] <- 1 #Recode "Full Time" (initially 2) to 1
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==3] <- 1 #Recode "Part Time" (initially 3) to 1
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==4] <- 0 #Recode "Homemaker" (initially 4) to 0
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==5] <- NA #"Other" employment (initially 5) to NA
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==6] <- 0 #Recode "Retired" (initially 6) to 0
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==7] <- 0 #Recode "Student" (initially 7) to 0
    dataV2$dem.employmentdummy[dataV2$dem.employmentdummy==8] <- 0 #Recode "Unemployed" (initially 8) to 0

    
dataV3 <- data.frame(cbind("Constant"=rep(1,nrow(dataV2))
                           ,"B1"=dataV2$BD1
                           ,"B2u"=dataV2$BD2
                           ,"B2d"=dataV2$BD2))

#Create dummy variables for responses
dataV3$dyy[dataV2$R1==1 & dataV2$R2==1] <- 1
dataV3$dyy[is.na(dataV3$dyy)] <- 0
dataV3$dyn[dataV2$R1==1 & dataV2$R2==2] <- 1
dataV3$dyn[is.na(dataV3$dyn)] <- 0
dataV3$dny[dataV2$R1==2 & dataV2$R2==1] <- 1
dataV3$dny[is.na(dataV3$dny)] <- 0
dataV3$dnn[dataV2$R1==2 & dataV2$R2==2] <- 1
dataV3$dnn[is.na(dataV3$dnn)] <- 0





#Bring in the remaining data
dataV3 <- data.frame(cbind(dataV3, dataV2[,5:ncol(dataV2)]))

  
  

####################################################
####################################################
##     DEFINE ALL FUNCTIONS USED IN ESTIMATION    ##
####################################################
####################################################

if(FALSE){
#1a: Maximize log-likelihood function for logistic CDF (minimize neg.)
#Specify the likelihood -- non-nested-function version
#LogLikeLogistic = function(theta,t1,t2,ys,nys,nns){
#  logL = -sum(ys*log(1 - (1/(1 + exp(t1%*%theta))))+nys*log(1/(1 + exp(t1%*%theta)))-(1/(1 + exp(t2%#*%theta)))+nns*log(1/(1 + exp(t2%*%theta))))
#  return(logL)
#}
#End Log Likelihood function

#B1 = x[,2]
#B^u = x[,3]
#B^d = x[,4]
#d^yy = x[,5]
#d^yn = x[,6]
#d^ny = x[,7]
#d^nn = x[,8]

#0a: Specify log likelihood function using logistic distribution -- non-nested-function version
LogLikeLogistic2 = function(theta,x){
  logL = -sum(x[,5]*log(1 - (1/(1 + exp(x[,1]*theta[1]-x[,2]*theta[2]+t1[,3:ncol(t1)]%*%theta[3:ncol(t1)]))))+nys*log(1/(1 + exp(t1[,1]*theta[1]-t1[,2]*theta[2]+t1[,3:ncol(t1)]%*%theta[3:ncol(t1)])))-(1/(1 + exp(t2[,1]*theta[1]-t2[,2]*theta[2]+t2[,3:ncol(t2)]%*%theta[3:ncol(t2)])))+nns*log(1/(1 + exp(t2[,1]*theta[1]-t2[,2]*theta[2]+t2[,3:ncol(t2)]%*%theta[3:ncol(t2)]))))
  return(logL)
}

#Programmatically create WTP vector (all X mean, no X mean, median)
#1: WTP vector creation function (optim w/ $par)
WTPfunc = function(theta,xk){
  #Define parameters
  ahat = theta$par[1] #recall that 'ahat' is negative 
  rhohat = theta$par[2]
  bhat = theta$par[3:(ncol(xk)+2)]
  abhat = theta$par[-2]
  #Initialize WTP vector
  WTP = matrix(0,nrow=3,ncol=2)
  WTP[,1] = rbind("mean WTP (with expl. var.)", 
                  "mean WTP (no expl. var.)", 
                  "median WTP (no expl. var.)")
  #Calculations:
  #Mean WTP using means of explanatory variables
  WTP[1,2] = -(1/(rhohat))*(ahat+(colMeans(xk)%*%bhat))
  #Mean WTP using only intercept and bid coefficient (no expl. vars)
  WTP[2,2] = -(log(1+exp(ahat)))/rhohat
  #Median WTP using only intercept and bid coefficient (no expl. vars)
  WTP[3,2] = -ahat/rhohat
  return(WTP)
}
#End WTP function creation

#2: WTP vector creation function for atomic vectors (optim w/out $par)
WTPfunc2 = function(theta,xk){
  #Define parameters
  ahat = theta[1] #recall that 'ahat' is negative 
  rhohat = theta[2]
  bhat = theta[3:(ncol(xk)+2)]
  abhat = theta[-2]
  #Initialize WTP vector
  WTP = matrix(0,nrow=3,ncol=2)
  WTP[,1] = rbind("mean WTP (with expl. var.)", 
                  "mean WTP (no expl. var.)", 
                  "median WTP (no expl. var.)")
  #Calculations:
  #Mean WTP using means of explanatory variables
  WTP[1,2] = -(1/(rhohat))*(ahat+(colMeans(xk)%*%bhat))
  #Mean WTP using only intercept and bid coefficient (no expl. vars)
  WTP[2,2] = -(log(1+exp(ahat)))/rhohat
  #Median WTP using only intercept and bid coefficient (no expl. vars)
  WTP[3,2] = -ahat/rhohat
  return(WTP)
}
#End WTP function creation for atomic vectors

#3: Create a distribution of individual WTP estimates
distr_WTP_est = function(theta,xk){
  nObs = nrow(xk)
  #Define parameters
  ahat = theta[1] #recall that 'ahat' is negative 
  rhohat = theta[2]
  bhat = theta[3:(ncol(xk)+2)]
  abhat = theta[-2]
  #Initialize WTP vector
  WTP = matrix(0,nrow=3,ncol=1)
  #Calculations:
  #Mean WTP using means of explanatory variables
  distr = matrix(0,nObs,1)
  for (i in 1:nObs){
    distr[i] = (1/(rhohat))*(ahat+(xk[i,]%*%bhat))
  }
  return(distr)
}
#End function to create estimated distribution

#4: Create function to return plot of kernel density of estimated bids
kdensfunc = function(theta, xk){
  
  #Create variable: individual WTP estimates for OOHB2
  ind_WTP_est = distr_WTP_est(theta, xk)
  
  #Plot estimated empirical distribution of WTP using assigned ranges
  #(round first, then create histogram)
  #round_ind_WTP_est_OOHB2 = round(ind_WTP_est_OOHB2,digits=1)
  #hist(round_ind_WTP_est_OOHB2, breaks=40, col="red")
  
  #Kernel density plot for estimated WTP
  kd_ind_WTP_est <- density(ind_WTP_est)
  #plot(kd_ind_WTP_est, main = "Kernel Density of estimated WTP")
  plot(kd_ind_WTP_est, main = "")
  return(summary(ind_WTP_est))
}
#End function

#5: Calculate McFadden (1974) pseudo-R-squared
McFaddenR = function(theta,x1,x2,responses){
  L1 = LogLikeLogistic(theta,x1,x2,responses[,1],responses[,3],responses[,4])
  #  L0 = LogLikeLogistic(rbind(theta[1],theta[2]),cbind(x1[,1],x1[,2]),cbind(x2[,1],x2[,2]),responses[,1],responses[,3],responses[,4]) #constant and bid
  L0 = LogLikeLogistic(rbind(theta[1]),cbind(x1[,1]),cbind(x2[,1]),responses[,1],responses[,3],responses[,4]) #only constant
  pseudoR2= 1 - L1/L0
  return(pseudoR2)
}
#End McFadden pseudo-R-squared fxn

#6: Calculate marginal effects (following Lin et al., 2006)
#Create function for marginal effects
MEfunc = function(theta,xk){
  
  #Define parameters
  ahat = theta[1]
  rhohat = theta[2]
  bhat = theta[3:length(theta)]
  
  #Initialize a matrix (of size k by 1)
  ME_temp = matrix(0,ncol(xk)+2,4)
  
  #Intercept
  ME_temp[1,1] = ahat
  
  #Bid
  ME_temp[2,1] = rhohat
  
  #Systematically (programatically) fill in marginal effects
  for(i in 1:length(bhat)){
    ME_temp[i+2,1] = -bhat[i]/rhohat
  }
  
  varcovar <- var(xk)
  for(i in 1:length(bhat)){
    ME_temp[i+2,2] = sqrt(ME_temp[i+2,1]*varcovar[i,i]*ME_temp[i+2,1]) #std err
    ME_temp[i+2,3] = ME_temp[i+2,1]/ME_temp[i+2,2] #zscore
    ME_temp[i+2,4] = 2*pnorm(-abs(ME_temp[i+2,3])) #pvalue
  }
  
  #multiply grad(k)*var(k)*grad(k) and take square root
  return(ME_temp)
}
#End of ME function

#7: Calculate average partial effects (following Greene, 7e IE, p. 730)
#Create function for APE calculation
APEfunc = function(theta,xk){
  
  #Define parameters
  ahat = theta[1]
  rhohat = theta[2]
  bhat = theta[3:length(theta)]
  abhat = theta[-2]
  localnObs = nrow(xk)
  PE_temp = matrix(0,nrow(xk),ncol(xk))
  APE_temp = matrix(0,ncol(xk)+2,1)
  APE_temp[1] = ahat
  
  #Create loop over i for parameters    
  for(i in 1:length(bhat)){
    i1 = i+1
    i2 = i+2
    thetamod = abhat[-i1]
    xkall = cbind(matrix(1,localnObs,1),xk)
    xk_no_i = xkall[,-i1]
    
    #Nested loop for j over observations
    for(j in 1:localnObs){
      p1 = (abhat%*%xkall[j,]/rhohat)
      p2 = sum(((thetamod%*%colMeans(xk_no_i))/rhohat)+(abhat[i1]%*%xkall[j,i1])/rhohat)/localnObs
      PE_temp[j,i] = p2-p1
    }
    
  }
  
  #Define values to be returned and stored
  return(PE_temp)
  
}
#END APEfunction2

#8: Program to extract Hessian from optimization process, convert to information matrix, calculate standard errors, and assign z scores
#Create coefficient stats extraction function
coeff_func = function(theta,xk){
  #Naming convention based on "routine" from optTheta_[routine] --              all matrices can be extracted using a different return request at         the end of the function
  #Extract the routine name from theta name
  setname <- deparse(substitute(theta))
  setname2 <- sub("optTheta_","",setname)
  
  #Extract the (numerical) Hessian (already negative of hessian since we minimized log likelihood, so info matrix is just inverse of hessian) and coefficient estimates from numerical optimization routine
  thetahess = theta$hessian
  thetapar = theta$par
  
  #Extract and name Hessian
  assign(paste0("hessian_", setname2),thetahess)
  hessian = eval(as.symbol(paste0("hessian_",setname2)))
  
  #Create the information matrix from the extracted Hessian (already negative)
  assign(paste0("infomat_", setname2),solve(hessian))
  infomat = eval(as.symbol(paste0("infomat_",setname2)))
  
  #Calculate standard errors from the square root of the diagonal of the information matrix
  assign(paste0("se_", setname2),sqrt(diag(infomat)))
  se = eval(as.symbol(paste0("se_",setname2)))
  
  #Calculate z scores from coefficient estimates (theta) and std err.
  assign(paste0("zscore_",setname2),thetapar/se)
  zscore = eval(as.symbol(paste0("zscore_",setname2)))
  
  #Convert z scores to p values (two-tailed p value s.t. abs(z)>1)
  assign(paste0("pvalue_",setname2),2*pnorm(-abs(zscore)))
  pvalue = eval(as.symbol(paste0("pvalue_",setname2)))
  
  #Calculate marginal effects
  assign(paste0("ME_",setname2),MEfunc(thetapar,xk))
  ME = eval(as.symbol(paste0("ME_",setname2)))
  
  #Get variable names
  VarName1 = colnames(xk)
  VarName2 = append(c("Intercept","Bid"),VarName1)
  
  #Create extraction table of results (for return) and clean up names
  table = cbind("Parameter"=VarName2,"Coefficient Estimate"=thetapar,
                "Std Err"=se,"Z score"=zscore,"P value"=pvalue,
                "Marginal Effect"=append(rep(0,2),ME[3:nrow(ME),1]),
                "M.E. Std Err"=append(rep(0,2),ME[3:nrow(ME),2]),
                "M.E. Z score"=append(rep(0,2),ME[3:nrow(ME),3]),
                "M.E. P value"=append(rep(0,2),ME[3:nrow(ME),4]))
  
  #Remove temporary matrices
  #rm(se,infomat,hessian,zscore,pvalue,ME)
  
  #Specify what gets returned
  return(table)
}
#End function

#Kanninen & Khawaja (1995) FCCC/ICCC Measure for Goodness of Fit
#Function
KanninenMeasure = function(x1,x2,theta,responses){
  #1. Estimate yes-no probabilities for initial bid using OOHB parameter estimates
  Py <- matrix(0,nrow(x1),1)
  for (i in 1:nrow(x1)){
    Py[i,] <- 1/(1+exp(-(theta%*%x1[i,])))
  }
  Pn <- 1-Py
  
  #2. Allocate to predicted yes-no
  AllocatePy <- ifelse(Py>0.5,1,0)
  AllocatePn <- ifelse(Pn>0.5,1,0)
  
  #3. Keep initially correctly classified cases only (ICCC)
  ICCC_mat <- matrix(0,nrow(x1),2)
  for (i in 1:nrow(x1)){
    ICCC_mat[i,1] <- ifelse((responses[i,1]+AllocatePy[i])==2,1,0)
    ICCC_mat[i,2] <- ifelse((responses[i,2]+AllocatePn[i])==2,1,0)
  }
  ICCC = sum(ICCC_mat)/nrow(x1)
  
  #4. Estimate joint (OOHB) probabilities of each respondent
  Pnn <- matrix(0,nrow(x2),1)
  Pny <- matrix(0,nrow(x2),1)
  for (i in 1:nrow(x2)){
    Pnn[i,] <- 1 - 1/(1+exp(-(theta%*%x2[i,])))
  }
  for (i in 1:nrow(x2)){
    Pny[i,] <- 1/(1+exp(-(theta%*%x1[i,]))) - 1/(1+exp(-(theta%*%x2[i,])))
  }
  
  #5. Skip because we have no bid premia
  
  #6. Allocate to predicted yes-no, then estimate conditional probabilities
  AllocatePnn <- ifelse(responses[,1]==0 & Pnn>0.5,1,0)
  AllocatePny <- ifelse(responses[,1]==0 & Pny>0.5,1,0)
  cond_Py_given_n <- ifelse(AllocatePny==1,Pny/Pn,0)
  cond_Pn_given_n <- ifelse(AllocatePnn==1,Pnn/Pn,0)
  
  #7. Add the number of correctly classified cases and estimate the percentage
  FCCC_vector <- AllocatePny + AllocatePnn
  FCCC = sum(FCCC_vector)/sum(responses[,2])
  GOF = cbind(ICCC, FCCC)
  return(GOF)
}
#End function

#Function to run full procedure and compile results in single table
fullproc = function(theta,x1,x2,xk,responses){
  temp.table <- coeff_func(theta,xk)
  temp.fulltheta = theta$par
  temp.table <- rbind(temp.table,
                      cbind("pseudo-Rsqrd", 
                            McFaddenR(temp.fulltheta,x1,x2,responses),
                            NA,NA,NA,NA,NA,NA,NA),
                      cbind(c("ICCC", "FCCC"),
                            t(KanninenMeasure(x1,x2,temp.fulltheta,responses)),
                            NA,NA,NA,NA,NA,NA,NA),
                      cbind(WTPfunc(theta,xk),NA,NA,NA,NA,NA,NA,NA))
  #  OOHB3_pseudoR2 = McFaddenR(fullthetahat_OOHB3,x1_OOHB3,x2_OOHB3,responsemat)
  #  kdensfunc(fullthetahat_OOHB3,xk_OOHB3)
  #  GOF_OOHB3 = KanninenMeasure(x1_OOHB3,x2_OOHB3,fullthetahat_OOHB3,responsemat)
}
#End function

#Confidence intervals
#nboot = 1000
bootstrap = function(nboot,t1,t2,responses){
  bootmean <- matrix(0,nrow=nboot,ncol=1)
  fxn.obs = nrow(t1)
  xfoo <- cbind(t1[,2],t2[,2:ncol(t2)],responses) #combine t1, t2, responses
  for(i in 1:nboot){
    bootsample <- matrix(0,nrow=0,ncol=ncol(xfoo))
    for(j in 1:fxn.obs){
      randObs <- round(runif(1,min=0.5,max=fxn.obs+0.5)) #random w/ equal likelihood
      bootsample <- rbind(bootsample,xfoo[randObs,])
    }
    #Re-extract the bootstrap sample x1, x2, and response matrices
    x1_foo = cbind(1, bootsample[,1],bootsample[,3:(ncol(bootsample)-4)])
    x2_foo = cbind(1, bootsample[,2:(ncol(bootsample)-4)])
    xk_foo = cbind(bootsample[,3:(ncol(bootsample)-4)])
    response_foo = cbind(bootsample[,(ncol(bootsample)-3):ncol(bootsample)])
    thetaStart_foo = rep(0.1, ncol(x2_foo)) #create programmatic initial theta
    
    #Numerical optimization routine to estimate theta
    optTheta_foo = optim(thetaStart_foo, LogLikeLogistic, gr = NULL, 
                         method = "Nelder-Mead", control = list(maxit = 1000),
                         hessian = TRUE, t1 = x1_foo, t2 = x2_foo,
                         ys = response_foo[,1], nys = response_foo[,3], 
                         nns = response_foo[,4])
    #Extract estimated coefficients
    ahat_foo = optTheta_foo$par[1] #recall that 'ahat' is negative 
    rhohat_foo = optTheta_foo$par[2]
    bhat_foo = optTheta_foo$par[3:ncol(x1_foo)]
    #Mean WTP using means of explanatory variables
    WTP_foo = (1/(rhohat_foo))*(ahat_foo+(colMeans(xk_foo)%*%bhat_foo))
    bootmean[i,]=WTP_foo
  }
  boot_stdev = sd(bootmean)
  boot_CI_lower = mean(bootmean)-1.96*boot_stdev
  boot_CI_upper = mean(bootmean)+1.96*boot_stdev
  table = rbind("Boot Samples" = nboot, 
                "Observations" = fxn.obs, 
                "Min WTP (boot)" = min(bootmean),
                "Max WTP (boot)" = max(bootmean),
                "Mean WTP (boot)" = mean(bootmean), 
                "St. Dev. WTP (boot)" = boot_stdev,
                "Conf. Int. Lower (boot)" = boot_CI_lower,
                "Conf. Int. Upper (boot)" = boot_CI_upper)
  return(table)
  #Join t1 and t2, sample with replacement, run regression, create WTP estimate, save WTP estimate in bootstrap table, get standard deviation of bootstrap WTP estimates, confidence interval = mean(WTP_boot) +- 1.96*SE_boot
}
} #old code from Ch. 1


#1: Specify the CDF for the maximum likelihood estimation below
G = function(theta,Bid,x){
  x = as.matrix(x) #Convert data frame to matrix
  G_logistic = 1/(1+exp(x[,1]*theta[1]-Bid*theta[2]+x[,9:ncol(x)]%*%theta[3:(ncol(x)-6)]))
}

#Treat x matrix as x=[Cons, B1, B2, d^yy, d^yn, d^ny, d^nn, X-other]
LogLikelihood = function(theta,x){
  xyy <- subset(x,x$dyy==1)
  xyn <- subset(x,x$dyn==1)
  xny <- subset(x,x$dny==1)
  xnn <- subset(x,x$dnn==1)
  LogL = -sum(xyy[,5]*log(1-G(theta,xyy[,3],xyy)),
              xyn[,6]*log(G(theta,xyn[,3],xyn)-G(theta,xyn[,2],xyn)),
              xny[,7]*log(G(theta,xny[,2],xny)-G(theta,xny[,4],xny)),
              xnn[,8]*G(theta,xnn[,4],xnn))
  return(LogL)
}


#Variables from which to choose in data
#[1] "Prev_Pan_Id"                                            
#[2] "Pan_Id"                                                 
#[3] "Info"                                                   
#[4] "Blinding_Code"                                          
#[5] "Q1__Gender"                                             
#[6] "Q2__Age"                                                
#[7] "Q3__1__White.Caucasian__European_American__Non.Hispanic"
#[8] "Q3__2__Hispanic_or_Latino_American"                     
#[9] "Q3__3__American_Indian_or_Alaskan_Native"               
#[10] "Q3__4__Asian__Asian_American"                           
#[11] "Q3__5__Black__African_American__Non.Hispanic_"          
#[12] "Q3__6__Middle_Eastern__Middle_Eastern_American"         
#[13] "Q3__7__Pacific_Islander"                                
#[14] "Q4__Level_of_education"                                 
#[15] "Q5__Married"                                            
#[16] "Q6__Income"                                             
#[17] "Q7__1__Full_time_employed"                              
#[18] "Q7__2__Part_time_employed"                              
#[19] "Q7__3__Unemployed"                                      
#[20] "Q7__4__Self.employed"                                   
#[21] "Q7__5__Retired"                                         
#[22] "Q7__6__Homemaker.caregiver"                             
#[23] "Q7__7__Student"                                         
#[24] "Q8__Household"                                          
#[25] "Q9__Children"                                           
#[26] "Q9__Children_COMMENTS"                                  
#[27] "Aroma"                                                  
#[28] "Appearance"                                             
#[29] "Taste.Flavor"                                           
#[30] "Overall"                                                
#[31] "Comment"                                                
#[32] "first"                                                  
#[33] "intamt"                                                 
#[34] "second"                                                 
#[35] "finalamt"                                               
#[36] "X1.Primary_Shopper"                                     
#[37] "X2.Avg_consumption"                                     
#[38] "X3.Category_of_Beer"                                    
#[39] "X4.Pilsner"                                             
#[40] "X4.Lager"                                               
#[41] "X4.Pale_Ale"                                            
#[42] "X4.IPA"                                                 
#[43] "X4.Amber"                                               
#[44] "X4.Porter"                                              
#[45] "X4.Stout"                                               
#[46] "X4.Other.s."                                            
#[47] "X4.Other.s._COMMENTS"                                   
#[48] "X5.Pay_for_beer"                                        
#[49] "X6.Price"                                               
#[50] "X6.Style"                                               
#[51] "X6..Brand"                                              
#[52] "X7..Conventional"                                       
#[53] "X7.Walmart"                                             
#[54] "X7.Co.op"                                               
#[55] "X7.Warehouse"                                           
#[56] "X7.Convenience"                                         
##[57] "X7.CSA"                                                 
#[58] "X7.Farmers_Mkt"                                         
#[59] "X8.Format"                                              
#[60] "X9"                                                     
#[61] "X10"                                                    
#[62] "X11"                                                    
#[63] "X12"                                                    
#[64] "X13"                                                    
#[65] "X14"                                                    
#[66] "X15"                                                    
#[67] "X16"                                                    
#[68] "X17"                                                    
#[69] "X18"                                                    
#[70] "X19"                                                    
#[71] "X20"                                                    
#[72] "X21"                                                    
#[73] "X22"                                                    
#[74] "X23"                                                    
#[75] "X24"                                                    
#[76] "X25"                                                    
#[77] "X26"                                                    
#[78] "X27"                                                    
#[79] "X28"                                                    
#[80] "X29..age"                                               
#[81] "X30.Environ"                                            
#[82] "X30.Healthier"                                          
#[83] "X30.Peers"                                              
#[84] "X30.Flavorful"                                          
#[85] "X30.Safer"                                              
#[86] "X30.Other"                                              
#[87] "X30.Other_COMMENTS"                                     
#[88] "X30.Don.t"                                              
#[89] "X31.Organic_beer"                                       
#[90] "X31a.If.yes..USDA" 

#Choose explanatory variables for analysis
variables_of_interest = c(10,11) #Model 2
#x_of_interest = c(10,11,13,14) #Model 3
#x_of_interest = c(9,10,11,13,14) #Model 3 non-categorical
#x_of_interest = c(6,9,10,11,13,14) #Model 4 non-categorical
#x_of_interest = c(9,10,11,12,13,14) #Model 6 non-categorical
#x_of_interest = c(9,10,11,12,14,15)

#Create new x1 using intercept, initial bid, and explanatory variables
#x1_OOHB3 = cbind(x_all[,1:2],x_all[,x_of_interest])
#Format data for use
dataV2 <- data.frame(cbind("R1" = data$first #1-yes, 2-no
                           ,"R2" = data$second #1-yes, 2-no
                           ,"BD1" = 9.99 #initial bid
                           ,"BD2" = data$finalamt #second bid (up if R1=1, else down)
                           ,data[,variables_of_interest]
))


for(i in 9:ncol(dataV3)){
dataV3 <- dataV3[!is.na(dataV3[,i]), ]
}

#Test function with a fake theta
#theta1 = rep(1.1, ncol(dataV3)-6)
#theta2 <- data.frame(rep(0.1, ncol(dataV3)-6))
#theta3 <- data.frame(t(theta2))
theta4 <- rep(0.1, ncol(dataV3)-6)
#k <- G(theta2,dataV3[,3],dataV3)
#k2 <- LogLikelihood(theta4,dataV3)

#install.packages("optimization")
#library(optimization)
#install.packages("optimx")
#library(optimx)

#dataV3[49,2]<- 1.00
#dataV3[48,2]<- 1.00

#Run numerical optimization routine for estimated theta (minimize negative of log-likelihood function)
optTheta = optim(theta4, #parameters with initial values
                 LogLikelihood, #function
                 gr = NULL,
                 method = "BFGS",
                 control = list(maxit = 1000),
                 hessian = TRUE,
                 x = dataV3)

#optTheta = optimx(theta4, #parameters with initial values
#                 fn = LogLikelihood, #function
#                 gr = NULL,
#                 hess = NULL,
#                 lower=-1000,
#                 upper = Inf,
#                 #method = "Rvmmin",
#                 method = c("Nelder-Mead", "BFGS"),
#                 #method = "L-BFGS-B",
#                 #itnmax - NULL,
#                 hessian = FALSE,
#                 control = list(),#maxit = 1000),
#                 x = dataV3)


if(FALSE){
#Convert raw {1,2} responses to binary {0,1}
data2$R1[data2$R1 == 2] <- 0
data2$R2[data2$R2 == 2] <- 0
data2$PrimaryShopper[data2$PrimaryShopper == 2] <- 0
} #Data build for package DBDC (doesn't run)



if(FALSE){
#2: Maximize log-likelihood function for logistic CDF (minimize neg.)
#Specify the likelihood -- nested-function version
LogLikeLogistic = function(theta,t1,t2,ys,nys,nns){
  logL = -sum(ys*log(1 - (1/(1 + exp(t1%*%theta))))+nys*log(1/(1 + exp(t1%*%theta)))-(1/(1 + exp(t2%*%theta)))+nns*log(1/(1 + exp(t2%*%theta))))
  return(logL)
}
#End Log Likelihood function

#Specify alternate likelihood -- non-nested-function version
LogLikeLogistic2 = function(theta,t1,t2,ys,nys,nns){
  logL = -sum(ys*log(1 - (1/(1 + exp(t1[,1]*theta[1]-t1[,2]*theta[2]+t1[,3:ncol(t1)]%*%theta[3:ncol(t1)]))))+nys*log(1/(1 + exp(t1[,1]*theta[1]-t1[,2]*theta[2]+t1[,3:ncol(t1)]%*%theta[3:ncol(t1)])))-(1/(1 + exp(t2[,1]*theta[1]-t2[,2]*theta[2]+t2[,3:ncol(t2)]%*%theta[3:ncol(t2)])))+nns*log(1/(1 + exp(t2[,1]*theta[1]-t2[,2]*theta[2]+t2[,3:ncol(t2)]%*%theta[3:ncol(t2)]))))
  return(logL)
}

}#end of if(false)


if(FALSE){
#Double-Bounded Dichotomous Choice Contingent Valuation Method----
#Import the data, raw_data.csv, with variable names in the first row. Attach dataset.
data = read.csv("data1_dbdc.csv")
#attach(data)

#Open additional libraries
install.packages("Icens")
install.packages("interval")
install.packages("DCchoice")
install.packages("tidyr")
library(DCchoice) #for DBDC
library(tidyr) #for data cleaning

#Format data for use
data2 <- data.frame(cbind("R1" = data$first
               ,"R2" = data$second
               ,"BD1" = 9.99
               ,"BD2" = data$finalamt
               ,"Aroma" = data$Aroma
#               ,"Appearance" = data$Appearance
#               ,"Taste" = data$Taste.Flavor
#               ,"Overall" = data$Overall)
#               ,"PrimaryShopper" = data$X1.Primary_Shopper
#               ,"AverageConsumption" = data$X2.Avg_consumption
                ))

#Convert raw {1,2} responses to binary {0,1}
data2$R1[data2$R1 == 2] <- 0
data2$R2[data2$R2 == 2] <- 0
#data2$PrimaryShopper[data2$PrimaryShopper == 2] <- 0

#Remove rows without all bid data
#data2 <- data2 %>% drop_na(R2)
#data2[!is.na(data2$R2),]
#data3 <- subset(data2,!is.na(R2))

#Remove rows without all data
data2 <- data2[complete.cases(data2),]

#data2$BD1[12]<-10.25
data2$BD1[49]<- 5.00
#data2$BD1[44]<- 32.00

#Create formula
formula1 <- R1 + R2 ~ Aroma + Appearance + Taste + Overall + PrimaryShopper + AverageConsumption | BD1 + BD2 

#Run regression
zed <- dbchoice(formula = formula1,
                data = data2,
                #na.action = na.omit,
                dist = "logistic",
                par = NULL,
                log=)


if(FALSE){
dbchoice(formula = formula1, data = data2, na.action = na.omit, dist = "log-logistic", par = NULL){
  # argument "na.action" was added in June 2016
  
  if (!inherits(formula, "Formula"))
    formula <- Formula(formula)
  
  # evaluating the formula and stops if the formula is not defined correctly
  if (!inherits(formula, "formula")) stop("invalid formula")
  # stop if the LHS does not contain two variables
  if(length(formula[[2]]) != 3) stop("LHS variable in the formula must be like y1 + y2 ")
  
  # checking the distribution
  if(dist != "logistic" & dist != "log-logistic" & dist != "normal" & dist != "log-normal" & dist != "weibull"){
    stop("'dist' is incorrect.")
  }
  # extracting explanatory variables (including the intercept) from the specified data frame
  cl <- match.call()            # a call to the function
  if(missing(data)) stop("the name of the data frame object must be supplied in the 'data' argument")
  
  mf <- match.call(expand.dots = TRUE)
  
  # Revised in June 2016
  #  m <- match(c("formula", "data", "subset"), names(mf), 0L)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  original.data <- data
  data <- mf
  
  #  # removing observations with missing values
  #  na.num <- max(sum(as.numeric(is.na(data))))
  #  if(na.num != 0){ 
  #    d1 <- nrow(data)
  #    data <- na.omit(data)
  #    d2 <- nrow(data)
  #    warning(paste("Missing values detected.", d1 - d2, "rows are removed.", sep = " "))
  #  }
  
  # defining the dependent variable
  y1 <- model.part(formula, data = data, lhs = 1)[[1]]  # yes/no to the first bid
  y2 <- model.part(formula, data = data, lhs = 1)[[2]]  # yes/no to the second bid
  
  nobs <- length(y1)
  
  # making dummy variables for the first and second bids
  if(is.factor(y1)){   # when the yes/no variables are defined as factor
    yy <- ifelse(y1 == "yes" & y2 == "yes", 1, 0)
    yn <- ifelse(y1 == "yes" & y2 == "no", 1, 0)
    ny <- ifelse(y1 == "no" & y2 == "yes", 1, 0)
    nn <- ifelse(y1 == "no" & y2 == "no", 1, 0)
  } else {
    yy <- ifelse(y1 == 1 & y2 == 1, 1, 0)
    yn <- ifelse(y1 == 1 & y2 == 0, 1, 0)
    ny <- ifelse(y1 == 0 & y2 == 1, 1, 0)
    nn <- ifelse(y1 == 0 & y2 == 0, 1, 0)
  }
}
}

if(TRUE){

dbchoice <- function (formula, data, subset, na.action = na.omit, dist = "log-logistic", par = NULL, ...){
  # argument "na.action" was added in June 2016
  
  if (!inherits(formula, "Formula"))
    formula <- Formula(formula)
  
  # evaluating the formula and stops if the formula is not defined correctly
  if (!inherits(formula, "formula")) stop("invalid formula")
  # stop if the LHS does not contain two variables
  if(length(formula[[2]]) != 3) stop("LHS variable in the formula must be like y1 + y2 ")
  
  # checking the distribution
  if(dist != "logistic" & dist != "log-logistic" & dist != "normal" & dist != "log-normal" & dist != "weibull"){
    stop("'dist' is incorrect.")
  }
  
  # extracting explanatory variables (including the intercept) from the specified data frame
  cl <- match.call()            # a call to the function
  if(missing(data)) stop("the name of the data frame object must be supplied in the 'data' argument")
  
  mf <- match.call(expand.dots = TRUE)
  
  # Revised in June 2016
  #  m <- match(c("formula", "data", "subset"), names(mf), 0L)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  original.data <- data
  data <- mf
  
  #  # removing observations with missing values
  #  na.num <- max(sum(as.numeric(is.na(data))))
  #  if(na.num != 0){ 
  #    d1 <- nrow(data)
  #    data <- na.omit(data)
  #    d2 <- nrow(data)
  #    warning(paste("Missing values detected.", d1 - d2, "rows are removed.", sep = " "))
  #  }
  
  # defining the dependent variable
  y1 <- model.part(formula, data = data, lhs = 1)[[1]]  # yes/no to the first bid
  y2 <- model.part(formula, data = data, lhs = 1)[[2]]  # yes/no to the second bid
  
  nobs <- length(y1)
  
  # making dummy variables for the first and second bids
  if(is.factor(y1)){   # when the yes/no variables are defined as factor
    yy <- ifelse(y1 == "yes" & y2 == "yes", 1, 0)
    yn <- ifelse(y1 == "yes" & y2 == "no", 1, 0)
    ny <- ifelse(y1 == "no" & y2 == "yes", 1, 0)
    nn <- ifelse(y1 == "no" & y2 == "no", 1, 0)
  } else {
    yy <- ifelse(y1 == 1 & y2 == 1, 1, 0)
    yn <- ifelse(y1 == 1 & y2 == 0, 1, 0)
    ny <- ifelse(y1 == 0 & y2 == 1, 1, 0)
    nn <- ifelse(y1 == 0 & y2 == 0, 1, 0)
  }
  
  
  # Creating a design matrix
  # Revised in June 2016
  #    bidf <- formula(formula, lhs = 0, rhs = 2)
  #    bid <- model.frame(bidf, data)  # the first and the second stage bids
  bid <- model.part(formula, data, lhs = 0, rhs = 2)
  
  BID <- ifelse(bid[, 1] > bid[, 2], bid[, 2], bid[, 1])
  
  yvar <- cbind(yy, yn, ny, nn)   # yes/no to "bid"
  
  # Revised in June 2016
  #    ff2 <- formula(formula, lhs = 0, rhs = 1)
  #    X <- model.frame(ff2, data)
  #    mmX <- model.matrix(ff2, X)
  X <- model.part(formula, data, lhs = 0, rhs = 1)
  mmX <- model.matrix(formula, data, lhs = 0, rhs = 1)
  
  tmp.data <- data.frame(y1, mmX, BID)
  
  # obtaining initial parameter values by logit model
  if(is.null(par)){
    f.stage <- glm(y1~. -1, family = binomial(link = "logit"), data = tmp.data)
    ini <- f.stage$coefficients # saving initial values for ML estimation
    npar <- length(ini)
    ini[npar] <- ifelse(ini[npar] > 0, -ini[npar], ini[npar])     # gives a negative initial value for the bid coefficient
    if (substr(dist, 1, 4) == "log-" | dist == "weibull") names(ini)[npar] <- "log(bid)"
    names(ini)[1] <- "(Intercept)"
  } else { # initial parameter values are supplied by the user
    if(length(par) != ncol(tmp.data)-1) stop("the length of 'par' does not coincide with the number of explanatory variables.")
    ini <- par
    f.stage <- ini
  }
  
  
  if(dist == "logistic" | dist == "log-logistic"){
    # likelihood function
    dbLL <- function(param, dvar, ivar, bid){
      yy <- dvar[, 1]
      yn <- dvar[, 2]
      ny <- dvar[, 3]
      nn <- dvar[, 4]
      
      X1 <- cbind(ivar, bid[, 1])
      X2 <- cbind(ivar, bid[, 2])
      
      ll <- 
        sum(plogis(-X2[yy == 1, ]%*%param, lower.tail = FALSE, log.p = TRUE))  + 
        sum(plogis(-X2[nn == 1, ]%*%param, lower.tail = TRUE, log.p = TRUE))   + 
        sum(log(plogis(-X2[yn == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE) - 
                  plogis(-X1[yn == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE))) +   
        sum(log(plogis(-X1[ny == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE) - 
                  plogis(-X2[ny == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE)))  
      ifelse(is.finite(ll), return(-ll), NaN) 
    }
  } else if(dist == "normal" | dist == "log-normal") {
    # likelihood function
    dbLL <- function(param, dvar, ivar, bid){
      yy <- dvar[, 1]
      yn <- dvar[, 2]
      ny <- dvar[, 3]
      nn <- dvar[, 4]
      
      X1 <- cbind(ivar, bid[, 1])
      X2 <- cbind(ivar, bid[, 2])
      
      ll <- 
        sum(pnorm(-X2[yy == 1, ]%*%param, lower.tail = FALSE, log.p = TRUE))  + 
        sum(pnorm(-X2[nn == 1, ]%*%param, lower.tail = TRUE, log.p = TRUE))   + 
        sum(log(pnorm(-X2[yn == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE) - 
                  pnorm(-X1[yn == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE))) +   
        sum(log(pnorm(-X1[ny == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE) - 
                  pnorm(-X2[ny == 1, ]%*%param, lower.tail = TRUE, log.p = FALSE)))  
      ifelse(is.finite(ll), return(-ll), NaN) 
    }
  } else if(dist == "weibull"){
    dbLL <- function(param, dvar, ivar, bid){
      yy <- dvar[, 1]
      yn <- dvar[, 2]
      ny <- dvar[, 3]
      nn <- dvar[, 4]
      
      X1 <- cbind(ivar, bid[, 1])
      X2 <- cbind(ivar, bid[, 2])
      
      ll <- 
        sum(pweibull(exp(-X2[yy == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = FALSE, log.p = TRUE))  + 
        sum(pweibull(exp(-X2[nn == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = TRUE, log.p = TRUE))   + 
        sum(log(pweibull(exp(-X2[yn == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = TRUE, log.p = FALSE) - 
                  pweibull(exp(-X1[yn == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = TRUE, log.p = FALSE))) +   
        sum(log(pweibull(exp(-X1[ny == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = TRUE, log.p = FALSE) - 
                  pweibull(exp(-X2[ny == 1, , drop = FALSE]%*%param), shape = 1, lower.tail = TRUE, log.p = FALSE)))  
      ifelse(is.finite(ll), return(-ll), NaN) 
    }
  }
  
  # ML estimation of double-bounded dichotomous choice
  suppressWarnings(
    dbchoice <- optim(ini, fn = dbLL, method="BFGS", hessian = TRUE, dvar = yvar, ivar = mmX, bid = bid, control = list(abstol = 10^(-30)))
  )
  npar <- length(dbchoice$par)
  
  terms <- terms(formula)
  
  # Revised in June 2016
  #  fac <- which(attr(attr(X, "terms"), "dataClasses") == "factor")
  fac <- which(sapply(X, is.factor) == TRUE)
  
  xlevels <- as.list(fac)
  j <- 0
  for (i in fac) {
    j <- j + 1
    xlevels[[j]] <- levels(X[[i]])
  }
  contrasts <- attr(mmX, "contrasts")
  
  # arranging outcomes into a single list variable
  output <- list(
    f.stage = f.stage,            # the outcome of the initial estimation
    dbchoice  = dbchoice,         # the outcome from the optimization
    coefficients = dbchoice$par,  # the coefficient estimates
    call = cl,                    # the function call
    formula = formula,            # the defined model formula
    Hessian = dbchoice$hessian,   # the numerical Hessian at the estimates
    distribution = dist,          # the specified error distribution
    loglik = -dbchoice$value,     # log-likelihood at the estimates
    convergence = ifelse(dbchoice$convergence == 0, TRUE, FALSE),   # convergence status
    niter = dbchoice$counts,      # the number of iterations
    nobs = nobs,                  # the number of observations
    covariates = mmX,             # a matrix of the covariates
    bid = bid,                    # the suggested bid
    yn = cbind(y1, y2),           # the acceptance/rejection variable
    data.name = data,             # the data matrix
    terms = terms,
    contrasts = contrasts,
    # Revised in June 2016
    data = original.data,
    xlevels = xlevels)
  
  class(output) <- "dbchoice"       # setting the object class
  return(output)
}

# a function for summarizing the outputs
summary.dbchoice <- function(object, ...){
  # obtaining necessary components from the object
  coef <- object$coefficients
  npar <- length(coef)
  se <- sqrt(diag(solve(object$Hessian)))   # standard errors of the estimates
  # object$nobs <- nrow(object$covariates)
  bid <- object$bid
  X <- object$covariates
  dist = object$distribution
  
  # estimating the null model
  #    formula_null <- object$formula
  formula_null <- formula(object$formula)
  formula_null[[3]][[2]] <- 1
  
  # Revised in June 2016
  #    db_null <- dbchoice(formula_null, data = eval(object$data.name), dist = dist, par = coef[c(1, npar)])
  if (inherits(object, "oohbchoice")) {
    db_null <- oohbchoice(formula_null, data = eval(object$data), dist = dist, par = coef[c(1, npar)])
  } else {
    db_null <-   dbchoice(formula_null, data = eval(object$data), dist = dist, par = coef[c(1, npar)])
  }
  
  # function for obrtaining AIC and BIC
  akaike <- function(loglik, npar, k ){
    -2*loglik + k*npar
  }
  
  # computing various mean estimates for different error distributions
  WTPs <- wtp(object = X, b = coef, bid = bid, dist = dist)
  object$medianWTP <- WTPs$medianWTP
  object$meanWTP <- WTPs$meanWTP
  object$trunc.meanWTP <- WTPs$trunc.meanWTP
  object$adj.trunc.meanWTP <- WTPs$adj.trunc.meanWTP
  
  # computing pseudo-R^2
  #   object$psdR2 <- 1 - object$loglik/db_null$loglik
  #   names(object$psdR2) <- "pseudo-R^2 measure"
  #   object$adjpsdR2 <- 1 - (object$loglik - npar)/db_null$loglik
  #   names(object$adjpsdR2) <- "adjusted pseudo-R^2 measure"
  
  # Likelihood Ratio Statistic
  LR <- -2*(db_null$loglik - object$loglik)
  d.f <- length(object$coefficients) - length(db_null$coefficients)
  pvalLR <- pchisq(LR, df = d.f, lower.tail = FALSE)
  object$LR.test <- c(LR, d.f, pvalLR)
  
  # creating a table for coefficients, se, etc. 
  zstat <- coef/se  # z statistics
  pval <- round(2*pnorm(-abs(zstat)), 6)  # p-value
  coef <- cbind(coef, se, zstat, pval)
  colnames(coef) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  
  object$coef <- coef
  
  # computing AIC and BIC by the function AKAIKE
  object$AIC <- akaike(object$loglik, npar, k = c(2, log(object$nobs)))
  names(object$AIC) <- c("AIC", "BIC")  
  
  class(object) <- "summary.dbchoice"
  return(object)
  
}

print.dbchoice <- function(x, digits = max(3, getOption("digits") - 1), ...){
  if(!x$convergence)  cat("The optimization did not converge\n")
  cat("\nDistribution:", x$distribution, "\n", sep = " ")
  print.default(format(x$coef, digits = digits), print.gap = 1, quote = FALSE)
  
  invisible(x)
}

print.summary.dbchoice <- function(x, digits = max(3, getOption("digits") - 1), ...){
  
  # what shall we do for Pearson residuals?
  
  cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
  cat("Formula:", deparse(x$formula, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
  
  if(!x$convergence)  cat("The optimization did not converge\n")
  cat("Coefficients:", "\n")
  #print.default(format(x$coef, digits = digits), print.gap = 2, quote = FALSE, right = TRUE)
  printCoefmat(x$coef, digits = 4, dig.tst = 4)    # printCoefmat() is defined in stats package
  
  cat("\nDistribution:", x$distribution, "", sep = " ")
  cat("\nNumber of Obs.:", formatC(x$nobs, digits = 0), "\n")
  cat("Log-likelihood:", formatC(x$loglik, format="f", digits = digits), "\n")
  #   cat("pseudo-R^2:", formatC(x$psdR2, format="f", digits = 4), 
  #       ", adjusted pseudo-R^2:", formatC(x$adjpsdR2, format="f", digits = 4), "")
  cat("\nLR statistic:", formatC(x$LR.test[1], format="f", digits = 3), "on", formatC(x$LR.test[2], digits = 0), 
      "DF, p-value:", formatC(x$LR.test[3], format="f", digits = 3), "\n")
  cat("AIC:", formatC(x$AIC[1], format="f", digits = digits), ", BIC:", formatC(x$AIC[2], format="f", digits = digits), "\n")
  cat("\nIterations:", formatC(x$niter, digits = 0), "")
  cat("\nConvergence:", x$convergence, "\n")
  
  cat("\nWTP estimates:")
  if(is.finite(x$meanWTP)){
    cat("\n Mean :", x$meanWTP, "", sep = " ")
  } else {
    cat("\n Mean :", x$meanWTP, "(because of |beta_Lbid| < 1)", sep = " ")
  }
  cat("\n Mean :", x$trunc.meanWTP, "(truncated at the maximum bid)", sep = " ")
  cat("\n Mean :", x$adj.trunc.meanWTP, "(truncated at the maximum bid with adjustment)", sep = " ")
  cat("\n Median:", x$medianWTP, "\n", sep = " ")
  
}
} #Creates DBDC technique without DBChoice package

} #Package version


} 

#Ordinal Analysis----


#Fractional Analysis----


