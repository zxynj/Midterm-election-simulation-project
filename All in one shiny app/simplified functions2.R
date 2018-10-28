execute_all_fun3 = function(multinom_p, contamination=FALSE, contem_multinom_r, q, N, nrep, alpha=0.05) {
  if(contamination) {
    N_contam=rbinom(nrep,N,q)
    data_sim=sapply(N_contam,function(x) rmultinom(1,x,contem_multinom_r)) + sapply(N-N_contam,function(x) rmultinom(1,x,multinom_p))
    }
  else {
    data_sim=rmultinom(nrep,N,multinom_p)
  }
  
  p_hat=data_sim/N
  delta_p_hat=rbind(p_hat[1,]-p_hat[2,],p_hat[1,]-p_hat[3,],p_hat[2,]-p_hat[3,])
  sum_p_hat=rbind(p_hat[1,]+p_hat[2,],p_hat[1,]+p_hat[3,],p_hat[2,]+p_hat[3,])
  A=qchisq(1-alpha/3,2)
  d=sum_p_hat-delta_p_hat^2
  lower_m1=delta_p_hat-sqrt(A*d/N)
  upper_m1=delta_p_hat+sqrt(A*d/N)
  
  a=uniroot(function(x) alpha-6+2*pnorm(x)+4*pnorm(x*sqrt(2)),lower=-3,upper=3)$root
  lower_m2=delta_p_hat-a/sqrt(N)
  upper_m2=delta_p_hat+a/sqrt(N)
  
  delta_p=c(multinom_p[1]-multinom_p[2],multinom_p[1]-multinom_p[3],multinom_p[2]-multinom_p[3])
  
  return(list(
    simult_coverage_prob_m1=sum(colSums(upper_m1 >= delta_p & lower_m1 <= delta_p)==3)/nrep,
    simult_coverage_prob_m2=sum(colSums(upper_m2 >= delta_p & lower_m2 <= delta_p)==3)/nrep,
    avg_ci_width_m1=sum(colSums(upper_m1-lower_m1)/3)/nrep,
    avg_ci_width_m2=sum(colSums(upper_m2-lower_m2)/3)/nrep,
    max_ci_width_m1=sum(colMaxs(upper_m1-lower_m1))/nrep,
    max_ci_width_m2=sum(colMaxs(upper_m2-lower_m2))/nrep)
  )
}