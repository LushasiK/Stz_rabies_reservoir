
# Tree construction:
guess=function(case,
               possdates,
               possIDs,
               uncert,
               species,
               owner_status,
               distmatrix,
               knownsource=rep(0, length(case)),
               exclude=TRUE){
  
  # find progenitors for cases - each case corresponds to a row in the rabid (case) table
  # default:    no known source - can assign known progenitor where possible
  #             exclude livestock as progenitors - can decide specific spp to exclude
  
  # Arguments:  the case (row in the rabid table),
  #             dates of the cases,
  #             IDs of possible progenitors
  #             distance matrix between all cases (no NAs to ensure assignments....)
  #             species of possible progenitors
  #             default settings for known sources and species exclusion criteria
  #             Work out which were traced (or not) afterwards
  
  if (sum(unlist(knownsource[case]))==0){  # if no known source, look for progenitor
    if(exclude==TRUE){  # ONLY look for possible progenitors that CANNOT be livestock:
      
    sourcei=which(possdates <= possdates[case] & possIDs != possIDs[case] & 
                      species != "livestock")
    }}
    
 
  # Probabilities of potential sources
  
  ## If incorporating the uncertainty then use these lines:
  
  SI_short = (possdates[case]-possdates[sourcei])-uncert[case] - uncert[sourcei]
  SI_long = (possdates[case]-possdates[sourcei])+uncert[case] + uncert[sourcei]
  
  SI_run<- c()
  SIprobs <- c()
  for(j in 1:length(SI_short)){
    if(SI_short[j] >= 0){ SI_short[j] = SI_short[j]
    }else{ 
      SI_short[j] = 0
    }
    
    if(SI_long[j] >= 0){ SI_long[j] = SI_long[j]
    }else{ 
      SI_long[j] = 0
      
    }
    
    SI_run[j] = runif(1, min = SI_short[j], max = SI_long[j])
    SIprobs[j] = dlnorm(SI_run[j], meanlog = SImean, sdlog = SIsd)
  }
  # Serial interval probabilities of possible sources
  sourceIDs = possIDs[sourcei]		# Identities of possible sources
  
  ### 
  if(!is.na(distmatrix[case,1])){  # only examine for cases where distances have been calculated
    sourcedists = distmatrix[case, sourcei] # determine distances of possible progenitors
    for(k in 1: length(sourcedists)){
      if(sourcedists[k] ==0) {
        sourcedists[k] <- sourcedists[k] + 0.5
      } else {
        NULL
      }
    }
    
    ## with the 50m cut off this is: 
    #distprobs = dgamma(sourcedists, shape = distshape, scale = distscale)
    if(owner_status[case] == "Known") {
      distprobs = dgamma(sourcedists, shape = distshape, scale = distscale)
    } else {
      distprobs = dgamma(sourcedists, shape = 2*distshape, scale = distscale)
      }
    
    ## Using one distribution for cases which are known and two convolutions
    ## of the distance distribution for those that are unknown or willdife
    
    sourceprobs = distprobs*SIprobs 		# overall probability of progenitors (product of spatial & temporal distance)
    
    # Assign progenitors:
    unitprobs = sourceprobs/sum(sourceprobs, na.rm=T)  # Scale probablities to one
    unitprobs[is.na(unitprobs)] = 0		# BUT GET RID OF NAs!
    cumunitprobs = cumsum(unitprobs)		# and make them cumulative
    ravr = runif(1,0,1)					# pick a random variate (RV) for assigning the progenitor
    sourceID = which(cumunitprobs>ravr)[1]  # which probability matches the RV (the [1] tells the
    ## code to select the first value in the vector that is above the ravr threshold.)
    SOURCE = sourceIDs[sourceID]			# Designate that as the source
    sourceprob = unitprobs[sourceID]		# keep track of the OVERALL SCALED probability
    SIprob = SIprobs[sourceID]  	# keep track of the SI probability
    distprob = distprobs[sourceID]    # keep track of the DISTANCE probability
    sourceLL = log(sourceprobs[sourceID]) # assign log likelihood of progenitor
    
  } else { SOURCE = sourceprob = sourceLL=NA} #assign NAs if distance not calculate-able
  c(SOURCE, sourceprob, sourceLL, SIprob, distprob)
}

