
############### Using dredge with defined number of predictors and custom function to assess MC #################
library(MASS)
library(MuMIn)

## Create dataset with correlated variables
mu <- rep(0,2)
Sigma <- matrix(.7, nrow=2, ncol=2) + diag(2)*.3
rawvars <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)

rdata <- data.frame(A=rawvars[,1], B=rawvars[, 2], C=rnorm(10000), D=rep(c("A","B","C", "D"), each=10, 10000))
cor(rdata[, 1:3])

#################################### nlme package
library(nlme)

### Full models
gls.model <- gls(A ~ B + C, data = rdata, method="ML")
lme.model <- lme(A ~ B + C, random = ~1| D, data = rdata, method="ML")

### Function to calculate maximum correlation coefficient between predictor variables, retrieved from each model
max.r <- function(x){
  if(class(x)=="gls"){
  corm <- summary(x)$corBeta} 
  else if(class(x)=="lme"){
    corm <- summary(x)$corFixed}
  else { print("Error: invalid model class")}
  corm <- as.matrix(corm)
  if (length(corm)==1){
    corm <- 0
    max(abs(corm))
  } else if (length(corm)==4){
    cormf <- corm[2:nrow(corm),2:ncol(corm)]
    cormf <- 0
    max(abs(cormf))
  } else {
    cormf <- corm[2:nrow(corm),2:ncol(corm)]
    diag(cormf) <- 0
    max(abs(cormf))
  }
}

max.r(gls.model) ##Test function
max.r(lme.model) ##Test function

###Run dredge specifying the number of predictor variables and including the max.r function
options(na.action = na.fail)
Allmodels <- dredge(Fullmodel, rank = "AIC", m.lim=c(0, 3), extra= c(max.r)) 
Allmodels[Allmodels$max.r<=0.6, ] ##Subset models with max.r <=0.6 (not collinear)
NCM <- get.models(Allmodels, subset = max.r<=0.6) ##Retrieve models with max.r <=0.6 (not collinear)
model.sel(NCM) ##Final model selection table

##################################### lme4 package
library(lme4)

###Full model
lmer.model <- lmer(A ~ B + C + (1|D), data = rdata, REML=F)

### Function to calculate maximum correlation coefficient between predictor variables, retrieved from each model
max.r2 <- function(x){
  corm <- cov2cor(vcov(x))
  corm <- as.matrix(corm)
  if (length(corm)==1){
    corm <- 0
    max(abs(corm))
  } else if (length(corm)==4){
  cormf <- corm[2:nrow(corm),2:ncol(corm)]
  cormf <- 0
  max(abs(cormf))
  } else {
    cormf <- corm[2:nrow(corm),2:ncol(corm)]
    diag(cormf) <- 0
    max(abs(cormf))
  }
}

max.r2(lmer.model) ##Test function
###Run dredge specifying the number of predictor variables and including the max.r function
options(na.action = na.fail)
Allmodels <- dredge(lmer.model, rank = "AIC", m.lim=c(0, 3), extra= c(max.r2)) 
Allmodels[Allmodels$max.r<=0.6, ] ##Subset models with max.r <=0.6 (not collinear)
NCM <- get.models(Allmodels, subset = max.r<=0.6) ##Retrieve models with max.r <=0.6 (not collinear)
model.sel(NCM) ##Final model selection table



