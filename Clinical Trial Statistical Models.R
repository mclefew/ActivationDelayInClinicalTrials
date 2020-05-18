predict_recruitments_treatment <- function(recruited_total,
                                           M,
                                           Ms,
                                           alpha,
                                           tau,
                                           forward_interval,
                                           stockout_risk=.05,
                                           precision=.001){
  
  n_Is_a = 0
  cum_prob = 0
  
  while(cum_prob < (1-stockout_risk)){
    cum_prob = cum_prob + prob_n_assigned_to_treatment_in_region(n_Is_a=n_Is_a,
                                                                 M=M,
                                                                 Ms=Ms,
                                                                 alpha=alpha,
                                                                 recruited_total=recruited_total,
                                                                 tau=tau,
                                                                 forward_interval=forward_interval,
                                                                 precision=precision)
    
    n_Is_a = n_Is_a + 1
  }
  
  return(n_Is_a-1)
}





estimate_alpha <- function(recruited_by_center_total){
  
  v.realized <- table( factor(recruited_by_center_total, levels = 0:max(recruited_by_center_total)))
  
  out <- alpha.MLE(n=sum(recruited_by_center_total),
                   N=length(recruited_by_center_total),
                   v.realized)
  
  return(out)
}



prob_n_assigned_to_treatment_in_region <- function(n_Is_a,
                                                   M,
                                                   Ms,
                                                   alpha,
                                                   recruited_total,
                                                   tau,
                                                   forward_interval,
                                                   precision = .0001
){
  prob_out <- 0
  recruitment_exhausted <- FALSE
  
  #prediction parameters for negative binomial prediction model - see predict_recruitments()
  alpha_p <- tau/forward_interval
  r <- recruited_total
  p <- alpha_p/(alpha_p+1)
  
  n_recruited = n_Is_a/K*block_length
  while(! recruitment_exhausted){
    if (I > 1){
      
      added_probability = (prob_n_arrive_to_system(n=n_recruited,
                                                   recruited_total=recruited_total,
                                                   tau=tau,forward_interval = forward_interval) *
                             anisimov_P(S=n_recruited/block_length*K,
                                                 M=M,Ms=Ms,alpha=alpha,n=n_Is_a)
      )
      
    } else{
      
      added_probability = prob_n_arrive_to_system(n=n_recruited,
                                                  recruited_total=recruited_total,
                                                  tau=tau,forward_interval = forward_interval)
      
    }
    prob_out = prob_out + added_probability
    
    if (pnbinom(n_recruited,r,p) > (1-precision) ){
      recruitment_exhausted = TRUE
    }
    n_recruited = n_recruited + 1
  }
  
  return(prob_out)
}




prob_n_arrive_to_system <- function(n,
                                    recruited_total,
                                    tau,
                                    forward_interval){
  
  #prediction parameters for negative binomial prediction model - see predict_recruitments()
  alpha_p <- tau/forward_interval
  r <- recruited_total
  p <- alpha_p/(alpha_p+1)
  
  out <- dnbinom(n,r,p)
  
  return(out)
  
}




anisimov_P <- function(S,M,Ms,alpha,n){
  P = choose(S,n)*beta(alpha*Ms+n,
                       alpha*(M-Ms)+S-n)/
    beta(alpha*Ms,alpha*(M-Ms))
  return(P)
}







