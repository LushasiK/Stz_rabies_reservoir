gamma_mean <- function(g_shape, g_scale){
  mean_gamma <- g_shape*g_scale 
  var_gamma <- g_shape*(g_scale^2)
  sd_gamma <- sqrt(var_gamma)
  output <- c(mean_gamma,var_gamma, sd_gamma)
  names(output) <- c("mean", "var", "sd")
  return(output)
}



weibull_mean <- function(w_shape, w_scale){
  mean_weib <- w_scale*(gamma(1+1/w_shape))
  var_weib <- (w_scale^2)*(gamma(1+2/w_shape) - (gamma(1+1/w_shape))^2)
  sd_weib <- sqrt(var_weib)
  output <- c(mean_weib, var_weib, sd_weib)
  names(output) <- c("mean", "var")
  return(output)
}


lognorm_mean <- function(m_log, sd_log){
  mean_ln <- exp(m_log + (sd_log^2)/2)
  var_ln <- (exp(sd_log^2)-1)*exp(2*m_log+sd_log^2)
  sd_ln <- sqrt(var_ln)
  output <- c(mean_ln, var_ln, sd_ln)
  names(output) <- c("mean", "var")
  return(output)
}