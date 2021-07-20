
library(flexsurv)

mixcensoredInt_SHPN_gamma <- function (y1, y2, d=rep(1, length(y1)), wt=rep(1, length(y1)),
                                 dist="gamma", n, cluster=NULL, classify="EM",
                                 maxiter=100, tol=1e-6) {
  #y1 is the right/left censored value, the exact lifetime observation, or for
  # interval censoring the lower value of the censoring interval
  #y2 is the upper value of the censoring interval
  #d is the censoring indicator (0=right censored, 1=event at time,
  # 2=left censored, 3=interval censored)
  #wt are the weights for the observations
  #dist: either the "weibull", "lognormal", or "gaussian" distribution
  #n is the number of components
  #cluster: start with random initialization of posterior probabilities (=NULL), or
  # a matrix with n columns of initial posterior probabilities for the observations
  #classify: "EM", "CEM", or "SEM" strategy
  #maxiter is the maximum number of iterations
  #tol is the convergence criterion 
  nobs <- sum(wt) #number of observations
  
  parms <- matrix(NA, ncol=3, nrow=n)
  colnames(parms) <- c("mu", "sigma", "logLik")
  posteriorP <- matrix(NA, ncol=n, nrow=length(y1))#
  stdErr <- matrix(NA, ncol=2, nrow=n)
  colnames(stdErr) <- c("mu", "log(sigma)")
  P <- matrix(NA, ncol=n, nrow=length(y1))
  
  
  
  iteration <- 0 #initialize iteration counter
  loglikInit <- logLikC <- 0 #initialize log-likelihood estimates
  
  #initialize posterior probabilities
  if (is.null(cluster)) {
    alpha <- rep(.1, n)
    posteriorP <- rdirichlet(length(y1), alpha)
  }
  else {posteriorP <- cluster}
  
  while (iteration < maxiter) {
    #estimate prior probabilities
    priorP <- apply(wt*posteriorP, 2, sum)/nobs
    
    #estimate distribution parameters for each component
    if (classify=="EM"){
      for (i in 1:n) {
        wtp <- ifelse(wt*posteriorP[,i]<=0, 1e-15, wt*posteriorP[,i])
        cp <- flexsurvreg(Surv(y1,y2, d, type="interval")~1, dist= dist,weights=wtp)#,
       
        
        ## these are when using gamma
        parms[i,] <- c(exp(cp$res.t[2]), exp(cp$res.t[1]), cp$loglik)
        stdErr[i,] <- sqrt(diag(vcov(cp))) ## temporarily hashing out whilst running weibull
      }
      
    }
    
    else if (classify=="CEM"){
      compPost <- apply(posteriorP, 1, which.max)
      for (i in 1:n) {
        cp <- survreg(Surv(y1,y2,d, type="interval")~1, weights=wt, dist=dist,
                      subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }
    
    else if (classify=="SEM"){
      compPost <- apply(posteriorP, 1, function (p) sample(x=1:n, size=1, prob=p))
      for (i in 1:n) {
        cp <- survreg(Surv(y1,y2,d, type="interval")~1, weights=wt, dist=dist,
                      subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }
    
    #compute the (complete) log-likelihood value
    logLikC <- sum(parms[,3]) + sum(wt*(posteriorP%*%log(priorP)))
    logLikC <- ifelse(is.na(logLikC), 0, logLikC)
    
    #estimate posterior probabilities
    if (dist=="weibull") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        
        
        cp <- ifelse(d==0, 1-pweibull(y1, scale = mu, shape = sigma), #right censoring
                     ifelse(d==1, dweibull(y1, scale = mu, shape = sigma), #exact observations
                            ifelse(d==2, pweibull(y1, scale = mu, shape = sigma), #left censoring
                                   pweibull(y2, scale = mu, shape =  sigma)-pweibull(y1, scale = mu, shape = sigma) ))) ## interval censoring
        
        P[,j] <- priorP[j]*cp
      }
    }
    
    else if (dist=="lognormal") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        
        ## adapted so don't have to standardise with z
        cp <- ifelse(d==0, 1-plnorm(y1, meanlog = mu, sdlog = sigma), #right censoring
                     ifelse(d==1, dlnorm(y1, meanlog = mu, sdlog = sigma), #exact observations
                            ifelse(d==2, plnorm(y1, meanlog = mu, sdlog = sigma), #left censoring
                                   plnorm(y2, meanlog = mu, sdlog = sigma)-plnorm(y1, meanlog = mu, sdlog = sigma) ))) ## interval censoring
        
        P[,j] <- priorP[j]*cp}
    }
    
    else if (dist=="gamma") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        
        cp <- ifelse(d==0, 1-pgamma(y1, shape = sigma,  rate = mu ), #right censoring
                     ifelse(d==1, dgamma(y1, shape = sigma, rate = mu ), #exact observations
                            ifelse(d==2, pgamma(y1,shape = sigma, rate = mu ), #left censoring
                                   pgamma(y2, shape = sigma, rate = mu )-pgamma(y1,shape = sigma, rate = mu ) ))) ## interval censoring
        
        P[,j] <- priorP[j]*cp
      }
    }
    
    posteriorP <- P/rowSums(P)
    
    #check convergence criterion
    if (( abs(logLikC-loglikInit) / (abs(loglikInit)+.001*tol) ) < tol) break
    loglikInit <- logLikC #update log-likelihood
    iteration <- iteration + 1 #increment counter
  }
  
  if (iteration == maxiter) warning("Maximum number of iterations exceeded")
  list(components=parms[,1:2], prior=priorP, loglik=logLikC,
       AIC=-2*logLikC + 2*(n-1+2*n), BIC=-2*logLikC + (n-1+2*n)*log(nobs),
       strategy=classify, distribution=dist, iterations=iteration,
       standardError=stdErr, 
       posterior=posteriorP)
}

