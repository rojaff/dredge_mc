
############### Using dredge with defined number of predictors and custom function to assess MC #################

#################################### nlme package
library(nlme)
library(MuMIn)

##Random dataset
rdata <- data.frame(A=rnorm(100), B=(rnorm(100)*2), C=(rnorm(100)/2), D=(rnorm(100)+5), E=(rnorm(100)-5))
head(rdata)

###Full model
Fullmodel <- gls(A ~ B + C + D + E, data = rdata, method="ML")

### Function to calculate maximum correlation coefficient between predictor variables, retrieved from each model
max.r <- function(x){
  corm <- summary(x)$corBeta ## use corFixed for lme models
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

max.r(Fullmodel) ##Test function
###Run dredge specifying the number of predictor variables and including the max.r function
options(na.action = na.fail)
Allmodels <- dredge(Fullmodel, rank = "AIC", m.lim=c(0, 3), extra= c(max.r)) 
Allmodels[Allmodels$max.r<=0.6, ] ##Subset models with max.r <=0.6 (not collinear)
NCM <- get.models(Allmodels, subset = max.r<=0.6) ##Retrieve models with max.r <=0.6 (not collinear)
model.sel(NCM) ##Final model selection table

##################################### lme4 package
library(lme4)

##Random dataset
rdata <- data.frame(A=rnorm(100), B=(rnorm(100)*2), C=(rnorm(100)/2), D=(rnorm(100)+5), E=rep(1:10, 5))
rdata$E <- factor(rdata$E)

###Full model
Fullmodel <- lmer(A ~ B + C + D + (1|E), data = rdata, REML=F)

### Function to calculate maximum correlation coefficient between predictor variables, retrieved from each model
max.r <- function(x){
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

max.r(Fullmodel) ##Test function
###Run dredge specifying the number of predictor variables and including the max.r function
options(na.action = na.fail)
Allmodels <- dredge(Fullmodel, rank = "AIC", m.lim=c(0, 3), extra= c(max.r)) 
Allmodels[Allmodels$max.r<=0.6, ] ##Subset models with max.r <=0.6 (not collinear)
NCM <- get.models(Allmodels, subset = max.r<=0.6) ##Retrieve models with max.r <=0.6 (not collinear)
model.sel(NCM) ##Final model selection table



