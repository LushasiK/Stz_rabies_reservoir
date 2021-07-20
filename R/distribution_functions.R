# Functions for reporting the mean and variance of various distributions
# e.g. Gamma, Weibull, Negative binomial, Log normal, Rayleigh
gamma_mean <- function(g_shape, g_scale){
  mean_gamma <- g_shape*g_scale 
  var_gamma <- g_shape*(g_scale^2)
  output <- c(mean_gamma,var_gamma)
  names(output) <- c("mean", "var")
  return(output)
}

weibull_mean <- function(w_shape, w_scale){
  mean_weib <- w_scale*(gamma(1+1/w_shape))
  var_weib <- (w_scale^2)*(gamma(1+2/w_shape) - (gamma(1+1/w_shape))^2)
  output <- c(mean_weib, var_weib)
  names(output) <- c("mean", "var")
  return(output)
}


negbin_mean <- function(nb_size, nb_prob){
  mean_nb <- (nb_prob*nb_size)/(1-nb_prob)
  var_nb <- (nb_prob*nb_size)/(1-nb_prob)^2
  output <- c(mean_nb, var_nb)
  names(output) <- c("mean", "var")
  return(output)
}


lognorm_mean <- function(m_log, sd_log){
  mean_ln <- exp(m_log + (sd_log^2)/2)
  var_ln <- (exp(sd_log^2)-1)*exp(2*m_log+sd_log^2)
  output <- c(mean_ln, var_ln)
  names(output) <- c("mean", "var")
  return(output)
}

ray_mean <- function(r_sigma){
  mean_ray <- r_sigma*(sqrt(pi/2))
  var_ray <- ((4-pi)/2)*(r_sigma^2)
  output <- c(mean_ray, var_ray)
  names(output) <- c("mean", "var")
  return(output)
}

