# Last update 

rm(list=ls())

# This piece of code is divided into 2 main sections:

# 1 # run a first example of a statistical model 
    # specified via a JAGS model synthax
    # contained in a separate ASCII (.txt) file 
    # 2022-rjags-first-example.txt
    # to be stored in your current working directory
    # this is a simple non-conjugate model for 
    # univariate normal data (both parameters unknown)

# 2 # run a second more complex example of a non linear model 
    # again with a single covariate 
    # where you want to 
       # a) explore the posterior distribution 
       # b) make a (posterior) predictive analysis for a new observation
       # c) fit alternative models and choose the "best" one
       # d) explore the mcmc output with suitable diagnostics for convergence monitoring

# install.packages("R2jags")
library(R2jags)

# check that you have installed the latest version of JAGS in your coputer/laptop

# https://sourceforge.net/projects/mcmc-jags/files/

# ALWAYS START WITH A SIMULATED DATA EXAMPLE WHERE YOU KNOW WHAT THE *TRUE* PARAMETER VALUE IS!!

# look at the ASCII file named
# "2022-rjags-first-example.txt"

# to be passes as "model.file" argument to the function jags(...)

set.seed(123)
N <- 100
x <- rnorm(N,mean=3,sd=2)

mu_0 <- 0
x
hist(x)

# 1) model data input preparation should be stored into a list 
#    to be passed as "data" argument to the jags(...) function
dd <- list("x" = x,"N" = N, "mu_0" = mu_0)
dd
# 2) specification of model parameters to be monitored 
#    as a vector of mode "character"
#    to be passed as "parameters.to.save" argument to the jags(...) function

params <- c("mu","tau", "variance")

# 3) specification parameter to be used as initial values for the 
#    model parameters  
#    specified as a function retutning a list (of lists) 
#    to be passed as "inits" argument to the jags(...) function

# you can initialize with a function object returning a named list

inits <- list(inits1=list("mu"=0,"sigma"=1),
              inits2=list("mu"=100,"sigma"=0.001),
              inits3=list("mu"=-10,"sigma"=0.1)
)

# or as a function

#inits <- function(){
#  list("mu"=0,"sigma"=1)
#}

?jags

myfirstjags <- jags(data=dd,inits=inits,parameters.to.save=params,model.file="2022-rjags-first-example.txt",n.chains=3,n.iter=2000, n.burnin=1, n.thin = 1)

# for understandig the jags synthax please refer to:

#    - the JAGS user manual downloadable from the souceforge repository at
#      https://sourceforge.net/projects/mcmc-jags/files/Manuals/4.x/jags_user_manual.pdf/download

# for understanding the R output in the myfirstjags objects resulting from the jags(...) function

myfirstjags
class(myfirstjags)
names(myfirstjags)

print(myfirstjags)
plot(myfirstjags)


# let's have a look at the component named "BUGSoutput"
# which in turn has a list structure with a lot of components
str(myfirstjags$BUGSoutput)
names(myfirstjags$BUGSoutput)

myfirstjags$BUGSoutput$sims.array[,1,"deviance"]

plot(density(myfirstjags$BUGSoutput$sims.array[,1,"mu"]))

#MC error IF they were i.i.d. 
var(myfirstjags$BUGSoutput$sims.array[,1,"mu"])/length(myfirstjags$BUGSoutput$sims.array[,1,"mu"])

effectivesamplesize <- LaplacesDemon::ESS(myfirstjags$BUGSoutput$sims.array[,1,"mu"])

# MCMC error (taking into account autocorrelation)
var(myfirstjags$BUGSoutput$sims.array[,1,"mu"])/effectivesamplesize 

head(myfirstjags$BUGSoutput$sims.array[,1,])
plot(myfirstjags$BUGSoutput$sims.array[,1,"mu"])

plot(myfirstjags$BUGSoutput$sims.array[,2,"deviance"])
head(myfirstjags$BUGSoutput$sims.array[,2,])

# this is the place where you will find the diffenent simulations 
# of the possibly multiple Markov chains (here n.chains=3) started from different starting points 
# and is arranged in an "array" class object 
# with 3 dimensions (iteration, which_markov_chain_index, parameter compoonents [+ deviance])

str(myfirstjags$BUGSoutput$sims.array)

# here is an example on how we can extract from the simulations 
# those related with the first (out of 3) Markov chain
# and considering only the "mu" component of the parameter vector ("mu","tau","variance")

myfirstjags$BUGSoutput$sims.array[,1,"mu"]

# and plot a smnoothed version of the empirical histogram

plot(density(myfirstjags$BUGSoutput$sims.array[,1,"mu"]))


# you can also initialize the jasg(...) function with a list object

inits2 <-  list(list("mu"=0,"sigma"=1),list("mu"=10,"sigma"=10))

mysecondjags <- jags(data=dd,inits=inits2,parameters.to.save=params,model.file="2022-rjags-first-example.txt",n.chains=2,n.iter=2000, n.burnin = 1)

class(mysecondjags)

print(mysecondjags)
plot(mysecondjags)

str(mysecondjags)

str(mysecondjags$BUGSoutput)

str(mysecondjags$BUGSoutput$sims.array)

# here is another example on how we can extract from the simulations 
# those related with the first (out of 3) markov chain
# and considering only the "variance" component of the parameter vector ("mu","tau","variance")

# we look here at the head of the matrix of joint simulations of parametrs (+deviance derived quantity)

head(mysecondjags$BUGSoutput$sims.array[,1,])

mysecondjags$BUGSoutput$sims.array[,1,"variance"]
# let us produce the traceplot of this simulation
plot(mysecondjags$BUGSoutput$sims.array[,1,"variance"],type="l")

# replot the traceplot without the first 999 simulations
plot(mysecondjags$BUGSoutput$sims.array[1000:1999,1,"variance"],type="l")
# replot the smoothed histogram based on simulations of the variance without the first 999
plot(density(mysecondjags$BUGSoutput$sims.array[1000:1999,1,"variance"]),type="l")


# 2 # run a second more complex example of a non linear model 
    # again with a single covariate 
    # where you want to 

      # a) explore the posterior distribution 
      # b) make a (posterior) predictive analysis for a new observation
      # c) fit alternative models and choose the "best" one
      # d) explore the mcmc output with suitable diagnostics for convergence monitoring



###### Now, let us play with the dugongs example with JAGS 

parameters <- c("alpha","beta","gamma","tau")

mydata <- list(   x = c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,  7.0,
                      8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0, 13.0,
                      13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
               Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                     2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                     2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), N = 27)

plot(mydata$x,mydata$Y)

inits1 <- list(alpha = 1.2, beta = 1.2, tau = 1, gamma = 0.5)
inits2 <- list(alpha = 10.2, beta = 10.2, tau = 1, gamma = 0.5)

initial.values <- list(inits1,inits2)

dugongjags <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-model-dugong.txt",n.chains=2,n.iter=20000)

print(dugongjags)
?jags

print(dugongjags)

# plot(dugongjags)

# traceplot(dugongjags)
str(dugongjags$BUGSoutput)
str(dugongjags$BUGSoutput$sims.matrix)
str(dugongjags$BUGSoutput$sims.array)

head(dugongjags$BUGSoutput$sims.matrix)
head(dugongjags$BUGSoutput$sims.array[,1,])
dim(dugongjags$BUGSoutput$sims.array[,1,])
plot(dugongjags$BUGSoutput$sims.matrix[,"gamma"],type="l")
acf(dugongjags$BUGSoutput$sims.matrix[,"gamma"])

plot(dugongjags$BUGSoutput$sims.array[,1,"gamma"])
acf(dugongjags$BUGSoutput$sims.array[,1,"gamma"])





parameters <- c("alpha","beta","sigma")

inits1 <- list(alpha = 1.2, beta = 1.2, tau = 1)
inits2 <- list(alpha = 1.2, beta = 1.2, tau = 1)

initial.values <- list(inits1,inits2)

dugongjags_alt_linear <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-LINEAR-model-dugong.txt",n.chains=2,n.iter=20000)



print(dugongjags_alt_linear)
plot(dugongjags_alt_linear)

traceplot(dugongjags_alt_linear)

dugongjags_alt_linear$BUGSoutput$DIC


plot(mydata$x,mydata$Y)
abline(dugongjags_alt_linear$BUGSoutput$summary[,"mean"]["alpha"],dugongjags_alt_linear$BUGSoutpu$summary[,"mean"]["beta"])

dugongjags_alt_linear$DIC



#########

parameters <- c("alpha","beta","gamma","sigma")

inits1 <- list(alpha = 1.2, beta = 1.2,  gamma = 0.2, tau = 1)
inits2 <- list(alpha = 1.2, beta = 1.2,  gamma = 0.2, tau = 1)

initial.values <- list(inits1,inits2)

dugongjags_alt_quadratic <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-QUADRATIC-model-dugong.txt",n.chains=2,n.iter=20000)

print(dugongjags_alt_quadratic)

dugongjags_alt_quadratic$BUGSoutput$DIC


## comparison among the 3 different alternative models 
## will be driven by the DIC index comparison ...
## the lewer ... the better

dugongjags$BUGSoutput$DIC
dugongjags_alt_linear$BUGSoutput$DIC
dugongjags_alt_quadratic$BUGSoutput$DIC


??densityplot

# install.packages("mcmc")
library(mcmc)
library(coda)

class(dugongjags)
?as.mcmc

library(help=coda)

mcmc.dugong <- as.mcmc(dugongjags)
class(mcmc.dugong)

str(mcmc.dugong)

colnames(mcmc.dugong[[1]])

for(i in 1:ncol(mcmc.dugong[[1]])){
  densplot(mcmc.dugong[[1]][,i],main=colnames(mcmc.dugong[[1]])[i])
  locator(1)
}

autocorr.plot(mcmc.dugong,auto.layout=FALSE)

effectiveSize(mcmc.dugong)

# another useful function for effective sample size
# can be found in the "LaplacesDemon" package
# LaplacesDemon::ESS

LaplacesDemon::ESS(dugongjags$BUGSoutput$sims.array[,1,"gamma"])

# this function works as long as you have the package/library "LaplacesDemon" installed in your computer

# another useful function for evaluating the MCMC error 
# of the approcimations

?MCSE
?LaplacesDemon::MCSE
LaplacesDemon::MCSE(dugongjags$BUGSoutput$sims.array[,1,"gamma"])

gelman.plot(mcmc.dugong)
geweke.diag(mcmc.dugong)
geweke.plot(mcmc.dugong)
raftery.diag(mcmc.dugong)
heidel.diag(mcmc.dugong)

?jags


dugongjags2 <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-model-dugong.txt",n.chains=2,n.iter=20000, n.thin=1)

str(dugongjags2$BUGSoutput$sims.array)


parameters <- c("alpha", "beta",  "gamma", "tau", "cond_expect_pred_0", "pred_0")
dugongjags3 <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-model-dugong.txt",n.chains=2,n.iter=20000, n.thin=1)

dugongjags3$BUGSoutput$sims.array[,1,"cond_expect_pred_0"]

plot(density(dugongjags3$BUGSoutput$sims.array[,1,"cond_expect_pred_0"]))

lines(density(dugongjags3$BUGSoutput$sims.array[,1,"pred_0"]),col="red")


####### library(ggmcmc)
# install.packages("ggmcmc")

library(ggmcmc)

class(dugongjags2)
class(as.mcmc(dugongjags2))

S <- ggs(as.mcmc(dugongjags2))
# look at a collection of diagnostics graphics in a single PDF file 

ggmcmc(S)

LaplacesDemon::MCSE(dugongjags2$BUGSoutput$sims.array[,1,"beta"])

var(dugongjags2$BUGSoutput$sims.array[,1,"beta"])
length(dugongjags2$BUGSoutput$sims.array[,1,"beta"])

sqrt(var(dugongjags2$BUGSoutput$sims.array[,1,"beta"])/length(dugongjags2$BUGSoutput$sims.array[,1,"beta"]))

(sqrt(var(dugongjags2$BUGSoutput$sims.array[,1,"beta"])/length(dugongjags2$BUGSoutput$sims.array[,1,"beta"]))
)<LaplacesDemon::MCSE(dugongjags2$BUGSoutput$sims.array[,1,"beta"])

acf(dugongjags2$BUGSoutput$sims.array[,1,"beta"])


#######
# it is worth mentioning another package which provides and interface to JAGS

# library(rjags)

set.seed(123)
N <- 1000
x <- rnorm(N,mean=3,sd=2)

dd <- list("x" = x,"N" = N, mu_0=0)
params <- c("mu","tau")


inits <- function(){
  list("mu"=0,"sigma"=1)
}

inits <- function(){
  list("mu"=0,"sigma"=1)
}


jags <- jags.model("2022-rjags-first-example.txt",
                   data = dd,
                   n.chains = 4,
                   n.adapt = 100)
update(jags, 1000)
prova <- jags.samples(jags,
                   c("mu", "tau"),
                   1000)


# NOW let us come back to the function jags(...) of the R2jags package/library

# Let us see an example of prediction of a new observable 
# for which we have the covariate(s) available 

# the main point is how to code it in the ASCII model file 
# (see "2022-jags-model-dugong-with-prediction.txt")


dugongjagspred <- jags(data=mydata,inits=initial.values,parameters.to.save=c("alpha","beta","gamma","tau","Ypred30","condexp30"),model.file="2022-jags-model-dugong-with-prediction.txt",n.chains=2,n.iter=2000)


print(dugongjagspred)

mode(dugongjagspred)
names(dugongjagspred)
mode(dugongjagspred$BUGSoutput)
names(dugongjagspred$BUGSoutput)

head(dugongjagspred$BUGSoutput$sims.matrix)

head(dugongjagspred$BUGSoutput$sims.matrix[,"alpha"]-dugongjagspred$BUGSoutput$sims.matrix[,"beta"]*dugongjagspred$BUGSoutput$sims.matrix[,"gamma"]^30)

library(ggmcmc)
S <- ggs(as.mcmc(dugongjagspred))
# look at a collection of diagnostics graphics in a single PDF file 
ggmcmc(S)

oo <- as.mcmc(dugongjagspred)

plot(density(oo[[1]][,"Ypred30"]),main="Prediction and conditional expectation of dugong's length \n given age x_i=30",ylim=c(0,7))

lines(density(oo[[1]][,"condexp30"]),col="red")



colnames(dugongjags$BUGSoutput$sims.matrix)

dugongjags <- jags(data=mydata,inits=initial.values,parameters.to.save=parameters,model.file="2022-jags-model-dugong.txt",n.chains=2,n.iter=20000)

acf(dugongjags$BUGSoutput$sims.matrix[,"beta"])


library(ggmcmc)
ggs(as.mcmc(myfirstjags))
S <- ggs(as.mcmc(myfirstjags))
# look at a collection of diagnostics graphics in a single PDF file 
ggmcmc(S)
